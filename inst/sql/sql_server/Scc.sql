{DEFAULT @drop_results_table = FALSE}
{@drop_results_table} ? {DROP TABLE IF EXISTS @results_table;}
DROP TABLE IF EXISTS #scc_exposure_summary;
DROP TABLE IF EXISTS #scc_outcome_summary;

-- Summarize risk windows
SELECT exposure_id,
    @analysis_id as analysis_id,
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
SELECT @analysis_id as analysis_id,
    exposure_id,
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


DROP TABLE IF EXISTS #full_grid;
SELECT exposure_id,
		num_persons,
		num_exposures,
		time_at_risk_exposed,
		time_at_risk_unexposed,
		outcome_id
INTO #full_grid
FROM #scc_exposure_summary,
(
    SELECT DISTINCT outcome_id
    FROM #scc_outcome_summary
) o1;


-- Probably not supported
CREATE TABLE IF NOT EXISTS @results_table (
    target_cohort_id BIGINT,
    outcome_cohort_id BIGINT,
    analysis_id BIGINT,
    num_persons BIGINT,
    num_exposures BIGINT,
    num_outcomes_exposed BIGINT,
    num_outcomes_unexposed BIGINT,
    time_at_risk_exposed BIGINT,
    time_at_risk_unexposed
);


DELETE FROM @results_table
WHERE EXISTS (
    SELECT 1
    FROM #full_grid
    WHERE  @results_table.target_cohort_id = #full_grid.exposure_id
      AND  @results_table.outcome_cohort_id = #full_grid.outcome_id
      AND  @results_table.analysis_id = @analysis_id
);

-- INSERT INTO RESULTS TABLE
INSERT INTO @results_table (target_cohort_id, outcome_cohort_id, analysis_id, num_persons, num_exposures,
                            num_outcomes_exposed, num_outcomes_unexposed, time_at_risk_exposed, time_at_risk_unexposed)
SELECT
    fg.exposure_id as target_cohort_id,
	fg.outcome_id as outcome_cohort_id,
	@analysis_id as analysis_id,
	num_persons,
	num_exposures,
	CASE WHEN outcome_summary.num_outcomes_exposed IS NULL THEN 0 ELSE outcome_summary.num_outcomes_exposed END AS num_outcomes_exposed,
	CASE WHEN outcome_summary.num_outcomes_unexposed IS NULL THEN 0 ELSE outcome_summary.num_outcomes_unexposed END AS num_outcomes_unexposed,
	time_at_risk_exposed,
	time_at_risk_unexposed
FROM #full_grid fg
LEFT JOIN #scc_outcome_summary outcome_summary
	ON fg.exposure_id = outcome_summary.exposure_id
		AND fg.outcome_id = outcome_summary.outcome_id;
