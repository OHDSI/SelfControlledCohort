/************************************************************************
Copyright 2018 Observational Health Data Sciences and Informatics

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
{DEFAULT @cdm_database_schema = CDM_Truven_MDCD_v569.dbo}
{DEFAULT @exposure_database_schema = CDM_Truven_MDCD_v569.dbo}
{DEFAULT @exposure_table = drug_era}
{DEFAULT @exposure_start_date = drug_era_start_date}
{DEFAULT @exposure_end_date = drug_era_end_date}
{DEFAULT @exposure_id = drug_concept_id}
{DEFAULT @exposure_person_id = person_id}
{DEFAULT @outcome_database_schema = CDM_Truven_MDCD_v569.dbo}
{DEFAULT @outcome_table = condition_era}
{DEFAULT @outcome_start_date = condition_era_start_date}
{DEFAULT @outcome_id = condition_concept_id}
{DEFAULT @outcome_person_id = person_id}
{DEFAULT @first_exposure_only = TRUE}
{DEFAULT @first_outcome_only = TRUE}
{DEFAULT @exposure_ids = 1125315, 1713332, 19011773, 1174888}
{DEFAULT @outcome_ids = 444382}
{DEFAULT @min_age = ''}
{DEFAULT @max_age = ''}
{DEFAULT @study_start_date = ''}
{DEFAULT @study_end_date = ''}
{DEFAULT @add_length_of_exposure_exposed = TRUE}
{DEFAULT @risk_window_start_exposed = 1}
{DEFAULT @risk_window_end_exposed = 30}
{DEFAULT @add_length_of_exposure_unexposed = TRUE}
{DEFAULT @risk_window_end_unexposed = -1}
{DEFAULT @risk_window_start_unexposed = -30}
{DEFAULT @has_full_time_at_risk = TRUE}
{DEFAULT @washout_window = 183}
{DEFAULT @followup_window = 183}
{DEFAULT @compute_tar_distribution = FALSE}

IF OBJECT_ID('tempdb..#results', 'U') IS NOT NULL
	DROP TABLE #results;

IF OBJECT_ID('tempdb..#risk_windows', 'U') IS NOT NULL
	DROP TABLE #risk_windows;

IF OBJECT_ID('tempdb..#scc_exposure_summary', 'U') IS NOT NULL
	DROP TABLE #scc_exposure_summary;

IF OBJECT_ID('tempdb..#scc_outcome_summary', 'U') IS NOT NULL
	DROP TABLE #scc_outcome_summary;

-- Create risk windows
--HINT DISTRIBUTE_ON_KEY(person_id)
SELECT person_id,
  exposure_id,
  risk_window_start_exposed,
  risk_window_end_exposed,
  risk_window_start_unexposed,
  risk_window_end_unexposed
INTO #risk_windows
FROM (
	SELECT t1.person_id,
		exposure_id,
		CASE WHEN
			DATEADD(DAY, @risk_window_start_exposed, exposure_start_date) <= op1.observation_period_end_date
		THEN
			DATEADD(DAY, @risk_window_start_exposed, exposure_start_date)
	    ELSE
			op1.observation_period_end_date
		END AS risk_window_start_exposed,
		CASE WHEN
			DATEADD(DAY, {@add_length_of_exposure_exposed} ? {DATEDIFF(DAY, exposure_start_date, exposure_end_date)} : {0} + @risk_window_end_exposed, exposure_start_date) <= op1.observation_period_end_date
		THEN
			DATEADD(DAY, {@add_length_of_exposure_exposed} ? {DATEDIFF(DAY, exposure_start_date, exposure_end_date)} : {0} + @risk_window_end_exposed, exposure_start_date)
	    ELSE
			op1.observation_period_end_date
		END AS risk_window_end_exposed,
		CASE WHEN
			DATEADD(DAY, {@add_length_of_exposure_unexposed} ? {-DATEDIFF(DAY, exposure_start_date, exposure_end_date)} : {0} + @risk_window_start_unexposed, exposure_start_date) >= op1.observation_period_start_date
		THEN
			DATEADD(DAY, {@add_length_of_exposure_unexposed} ? {-DATEDIFF(DAY, exposure_start_date, exposure_end_date)} : {0} + @risk_window_start_unexposed, exposure_start_date)
	    ELSE
			op1.observation_period_start_date
		END AS risk_window_start_unexposed,
		CASE WHEN
			DATEADD(DAY, @risk_window_end_unexposed, exposure_start_date) >= op1.observation_period_start_date
		THEN
			DATEADD(DAY, @risk_window_end_unexposed, exposure_start_date)
	    ELSE
			op1.observation_period_start_date
		END AS risk_window_end_unexposed
	FROM (
		SELECT person_id,
			exposure_id,
			exposure_start_date,
			exposure_end_date
		FROM (
			SELECT et.@exposure_person_id AS person_id,
				et.@exposure_id AS exposure_id,
				et.@exposure_start_date AS exposure_start_date,
				et.@exposure_end_date AS exposure_end_date
				{@first_exposure_only} ? {,ROW_NUMBER() OVER (PARTITION BY et.@exposure_person_id, et.@exposure_id ORDER BY et.@exposure_start_date) AS rn1}
			FROM
				@exposure_database_schema.@exposure_table et
{@exposure_ids != ''} ? {			INNER JOIN #scc_exposure_ids sei ON sei.exposure_id = et.@exposure_id }
		) raw_exposures
{@first_exposure_only} ? {		WHERE rn1 = 1}
	) t1
	INNER JOIN @cdm_database_schema.person p1
		ON t1.person_id = p1.person_id
	INNER JOIN @cdm_database_schema.observation_period op1
		ON t1.person_id = op1.person_id
	WHERE t1.exposure_start_date >= op1.observation_period_start_date
		AND t1.exposure_start_date <= op1.observation_period_end_date
{@study_start_date != ''} ? {		AND exposure_start_date >= CONVERT(DATE, '@study_start_date')}
{@study_end_date != ''} ? {		AND exposure_start_date <= CONVERT(DATE, '@study_end_date')}
{@min_age != ''} ? {		AND YEAR(exposure_start_date) - p1.year_of_birth  >= @min_age}
{@max_age != ''} ? {		AND YEAR(exposure_start_date) - p1.year_of_birth  <= @max_age}
{@has_full_time_at_risk}  ? {
		AND op1.observation_period_end_date >= DATEADD(DAY, {@add_length_of_exposure_exposed} ? {DATEDIFF(DAY, exposure_start_date, exposure_end_date)} : {0} + @risk_window_end_exposed, exposure_start_date)
		AND op1.observation_period_start_date <= DATEADD(DAY, -{@add_length_of_exposure_exposed} ? {DATEDIFF(DAY, exposure_start_date, exposure_end_date)} : {0} + @risk_window_start_unexposed, exposure_start_date)
}
{@washout_window != ''} ? {		AND DATEDIFF(DAY, op1.observation_period_start_date, exposure_start_date) >= @washout_window}
{@followup_window != ''} ? {		AND DATEDIFF(DAY, exposure_start_date, op1.observation_period_end_date) >= @followup_window}
) t2
WHERE risk_window_end_exposed >= risk_window_start_exposed
	AND risk_window_end_unexposed >= risk_window_start_unexposed;

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
	FROM #risk_windows
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
FROM #risk_windows risk_windows
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
INTO #results
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


-- Start of distribution of treatment duration and time to outcome
{@compute_tar_distribution} ? {
WITH treatment_times AS (
    SELECT
        exposure_id,
        outcome_id,
        CASE WHEN outcome_date >= risk_window_start_exposed AND outcome_date <= risk_window_end_exposed
            THEN 1
            ELSE 0
        END AS is_exposed_outcome,

        CASE WHEN outcome_date >= risk_window_start_unexposed AND outcome_date <= risk_window_end_unexposed
            THEN 1
            ELSE 0
        END AS is_unexposed_outcome,

  	    DATEDIFF(DAY, risk_window_start_exposed, risk_window_end_exposed) + 1 AS time_at_risk_exposed,
  	    abs(DATEDIFF(DAY, risk_window_start_exposed, outcome_date) + 1) AS time_to_outcome

    FROM #risk_windows risk_windows
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
),

-- time at risk distribution
tx_distribution AS (
       SELECT
              o.exposure_id,
              o.outcome_id,
              o.mean_tx_time,
              coalesce(o.sd_tx_time, 0) AS sd_tx_time,
              o.min_tx_time,
              MIN(CASE WHEN s.accumulated >= .10 * o.total THEN time_at_risk_exposed ELSE o.max_tx_time END) AS p10_tx_time,
              MIN(CASE WHEN s.accumulated >= .25 * o.total THEN time_at_risk_exposed ELSE o.max_tx_time END) AS p25_tx_time,
              MIN(CASE WHEN s.accumulated >= .50 * o.total THEN time_at_risk_exposed ELSE o.max_tx_time END) AS median_tx_time,
              MIN(CASE WHEN s.accumulated >= .75 * o.total THEN time_at_risk_exposed ELSE o.max_tx_time END) AS p75_tx_time,
              MIN(CASE WHEN s.accumulated >= .90 * o.total THEN time_at_risk_exposed ELSE o.max_tx_time END) AS p90_tx_time,
              o.max_tx_time
       FROM (
              SELECT
                     exposure_id,
                     outcome_id,
                     avg(1.0 * time_at_risk_exposed) AS mean_tx_time,
                     stdev(time_at_risk_exposed) AS sd_tx_time,
                     min(time_at_risk_exposed) AS min_tx_time,
                     max(time_at_risk_exposed) AS max_tx_time,
                     count_big(*) AS total
              FROM treatment_times q
              WHERE q.is_exposed_outcome = 1 OR q.is_unexposed_outcome = 1
              GROUP BY exposure_id, outcome_id
       ) o
       JOIN (
              SELECT exposure_id, outcome_id, time_at_risk_exposed, count_big(*) AS total,
                     sum(count_big(*)) OVER (PARTITION BY exposure_id, outcome_id ORDER BY time_at_risk_exposed) AS accumulated
              FROM treatment_times q
              WHERE q.is_exposed_outcome = 1 OR q.is_unexposed_outcome = 1
              GROUP BY exposure_id, outcome_id, time_at_risk_exposed
       ) s on (o.exposure_id = s.exposure_id and o.outcome_id = s.outcome_id)
       GROUP BY o.exposure_id, o.outcome_id, o.total, o.min_tx_time, o.max_tx_time, o.mean_tx_time, o.sd_tx_time
),
-- Average (absolute) time between exposure and outcome
time_to_distribution AS (
       SELECT
              o.exposure_id,
              o.outcome_id,
              o.mean_time_to_outcome,
              coalesce(o.sd_time_to_outcome, 0) AS sd_time_to_outcome,
              o.min_time_to_outcome,
              MIN(CASE WHEN s.accumulated >= .10 * o.total THEN time_to_outcome ELSE o.max_time_to_outcome END) AS p10_time_to_outcome,
              MIN(CASE WHEN s.accumulated >= .25 * o.total THEN time_to_outcome ELSE o.max_time_to_outcome END) AS p25_time_to_outcome,
              MIN(CASE WHEN s.accumulated >= .50 * o.total THEN time_to_outcome ELSE o.max_time_to_outcome END) AS median_time_to_outcome,
              MIN(CASE WHEN s.accumulated >= .75 * o.total THEN time_to_outcome ELSE o.max_time_to_outcome END) AS p75_time_to_outcome,
              MIN(CASE WHEN s.accumulated >= .90 * o.total THEN time_to_outcome ELSE o.max_time_to_outcome END) AS p90_time_to_outcome,
              o.max_time_to_outcome
       FROM (
              SELECT
                     exposure_id,
                     outcome_id,
                     avg(1.0 * time_to_outcome) AS mean_time_to_outcome,
                     stdev(time_to_outcome) AS sd_time_to_outcome,
                     min(time_to_outcome) AS min_time_to_outcome,
                     max(time_to_outcome) AS max_time_to_outcome,
                     count_big(*) AS total
              FROM treatment_times q
              WHERE q.is_exposed_outcome = 1 OR q.is_unexposed_outcome = 1
              GROUP BY exposure_id, outcome_id
       ) o
       JOIN (
              SELECT exposure_id, outcome_id, time_to_outcome, count_big(*) AS total,
                     sum(count_big(*)) OVER (PARTITION BY exposure_id, outcome_id ORDER BY time_to_outcome) AS accumulated
              FROM treatment_times q
              WHERE q.is_exposed_outcome = 1 OR q.is_unexposed_outcome = 1
              GROUP BY exposure_id, outcome_id, time_to_outcome
       ) s on (o.exposure_id = s.exposure_id and o.outcome_id = s.outcome_id)
       GROUP BY o.exposure_id, o.outcome_id, o.total, o.min_time_to_outcome, o.max_time_to_outcome, o.mean_time_to_outcome, o.sd_time_to_outcome
)

SELECT tx.*,
    tt.mean_time_to_outcome,
    tt.sd_time_to_outcome,
    tt.min_time_to_outcome,
    tt.p10_time_to_outcome,
    tt.p25_time_to_outcome,
    tt.median_time_to_outcome,
    tt.p75_time_to_outcome,
    tt.p90_time_to_outcome,
    tt.max_time_to_outcome
    INTO #tar_stats
    FROM tx_distribution tx
    INNER JOIN time_to_distribution tt ON (tt.exposure_id = tx.exposure_id AND tt.outcome_id = tx.outcome_id);
}
-- End of treatment time distribution calculations

TRUNCATE TABLE #scc_exposure_summary;
TRUNCATE TABLE #scc_outcome_summary;
TRUNCATE TABLE #risk_windows;
TRUNCATE TABLE #scc_outcome_ids;
TRUNCATE TABLE #scc_exposure_ids;

DROP TABLE #scc_exposure_summary;
DROP TABLE #scc_outcome_summary;
DROP TABLE #risk_windows;
DROP TABLE #scc_outcome_ids;
DROP TABLE #scc_exposure_ids;