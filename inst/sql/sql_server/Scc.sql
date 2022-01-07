/************************************************************************
Copyright 2022 Observational Health Data Sciences and Informatics

This file is part of SelfControlledCohort

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
************************************************************************/

{@results_table == #results} ? {
IF OBJECT_ID('tempdb..#results', 'U') IS NOT NULL
	DROP TABLE #results;
} : {
IF OBJECT_ID('@results_table', 'U') IS NOT NULL
    DROP TABLE @results_table;
}

IF OBJECT_ID('tempdb..#scc_exposure_summary', 'U') IS NOT NULL
	DROP TABLE #scc_exposure_summary;

IF OBJECT_ID('tempdb..#scc_outcome_summary', 'U') IS NOT NULL
	DROP TABLE #scc_outcome_summary;

-- Summarize risk windows
SELECT exposure_id,
	COUNT(DISTINCT person_id) AS num_persons,
	COUNT(*) AS num_exposures,
	SUM(time_at_risk_exposed) / 365.25 AS time_at_risk_exposed,
	SUM(time_at_risk_unexposed) / 365.25 AS time_at_risk_unexposed
INTO #scc_exposure_summary
FROM (
	SELECT person_id,
		exposure_id,
		DATEDIFF(DAY, risk_window_start_exposed, risk_window_end_exposed) + 1 AS time_at_risk_exposed,
		DATEDIFF(DAY, risk_window_start_unexposed, risk_window_end_unexposed) + 1 AS time_at_risk_unexposed
	FROM @risk_windows_table
) t1
GROUP BY exposure_id;

-- Summarize outcomes in risk windows
SELECT exposure_id,
	outcome_id,
	SUM(
		CASE WHEN
				outcome_date >= risk_window_start_exposed
			AND
				outcome_date <= risk_window_end_exposed
		THEN
			1
		ELSE
			0
		END
	) AS num_outcomes_exposed,
	SUM(
		CASE WHEN
				outcome_date >= risk_window_start_unexposed
			AND
				outcome_date <= risk_window_end_unexposed
		THEN
			1
		ELSE
			0
		END	) AS num_outcomes_unexposed
INTO #scc_outcome_summary
FROM @risk_windows_table risk_windows
INNER JOIN (
	SELECT person_id,
		outcome_id,
		outcome_date
	FROM (
		SELECT ot.@outcome_person_id AS person_id,
			ot.@outcome_id AS outcome_id,
			ot.@outcome_start_date AS outcome_date
			{@first_outcome_only} ? {,ROW_NUMBER() OVER (PARTITION BY ot.@outcome_person_id, ot.@outcome_id ORDER BY ot.@outcome_start_date) AS rn1}
		FROM
			@outcome_database_schema.@outcome_table ot
{@outcome_ids != ''} ? {		INNER JOIN #scc_outcome_ids soi ON soi.outcome_id = ot.@outcome_id}
	) raw_outcomes
{@first_outcome_only} ? {	WHERE rn1 = 1}
) outcomes
ON risk_windows.person_id = outcomes.person_id
GROUP BY exposure_id,
	outcome_id;

--Create final summary table
SELECT full_grid.exposure_id,
	full_grid.outcome_id,
	num_persons,
	num_exposures,
	CASE WHEN outcome_summary.num_outcomes_exposed IS NULL THEN 0 ELSE outcome_summary.num_outcomes_exposed END AS num_outcomes_exposed,
	CASE WHEN outcome_summary.num_outcomes_unexposed IS NULL THEN 0 ELSE outcome_summary.num_outcomes_unexposed END AS num_outcomes_unexposed,
	time_at_risk_exposed,
	time_at_risk_unexposed
INTO @results_table
FROM (
	SELECT exposure_id,
		num_persons,
		num_exposures,
		time_at_risk_exposed,
		time_at_risk_unexposed,
		outcome_id
	FROM #scc_exposure_summary,
		(
			SELECT DISTINCT outcome_id
			FROM #scc_outcome_summary
		) o1
) full_grid
LEFT JOIN #scc_outcome_summary outcome_summary
	ON full_grid.exposure_id = outcome_summary.exposure_id
		AND full_grid.outcome_id = outcome_summary.outcome_id;
