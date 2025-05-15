{DEFAULT @analysis_id = 1}
{@drop_results_table} ? {DROP TABLE IF EXISTS @risk_windows_table;}

CREATE TABLE IF NOT EXISTS @risk_windows_table (
  person_id BIGINT,
  exposure_id BIGINT,
  analysis_id BIGINT,
  exposure_start_date DATE,
  risk_window_start_exposed DATE,
  risk_window_end_exposed DATE,
  risk_window_start_unexposed DATE,
  risk_window_end_unexposed DATE
);

DELETE FROM @risk_windows_table
WHERE EXISTS (
    SELECT 1
    FROM @exposure_database_schema.@exposure_table
    WHERE @exposure_database_schema.@exposure_table.@exposure_id = @risk_windows_table.exposure_id
    AND analysis_id = @analysis_id
);

-- Create risk windows
INSERT INTO @risk_windows_table
(
    person_id,
    exposure_id,
    analysis_id,
    exposure_start_date,
    risk_window_start_exposed,
    risk_window_end_exposed,
    risk_window_start_unexposed,
    risk_window_end_unexposed
)
SELECT
    person_id,
    exposure_id,
    @analysis_id as analysis_id,
    exposure_start_date,
    risk_window_start_exposed,
    risk_window_end_exposed,
    risk_window_start_unexposed,
    risk_window_end_unexposed
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
