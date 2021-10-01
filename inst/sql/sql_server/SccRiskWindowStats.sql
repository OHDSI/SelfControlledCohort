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