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

{@risk_windows_table == #risk_windows} ? {
IF OBJECT_ID('tempdb..#risk_windows', 'U') IS NOT NULL
	DROP TABLE #risk_windows;
} :{
IF OBJECT_ID('@risk_windows_table', 'U') IS NOT NULL
	DROP TABLE @risk_windows_table;
}
-- Create risk windows
--HINT DISTRIBUTE_ON_KEY(person_id)
SELECT person_id,
  exposure_id,
  exposure_start_date,
  risk_window_start_exposed,
  risk_window_end_exposed,
  risk_window_start_unexposed,
  risk_window_end_unexposed
INTO @risk_windows_table
FROM (
	SELECT t1.person_id,
		exposure_id,
		exposure_start_date,
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
