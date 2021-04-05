{DEFAULT @compute_tar_distribution = FALSE}
SELECT r.* {@compute_tar_distribution} ? {
,ts.mean_tx_time,
ts.sd_tx_time,
ts.min_tx_time,
ts.p10_tx_time,
ts.p25_tx_time,
ts.median_tx_time,
ts.p75_tx_time,
ts.p90_tx_time,
ts.max_tx_time,
ts.mean_time_to_outcome,
ts.sd_time_to_outcome,
ts.min_time_to_outcome,
ts.p10_time_to_outcome,
ts.p25_time_to_outcome,
ts.median_time_to_outcome,
ts.p75_time_to_outcome,
ts.p90_time_to_outcome,
ts.max_time_to_outcome}
FROM #results r
{@compute_tar_distribution} ? {LEFT JOIN #tar_stats ts ON (ts.exposure_id = r.exposure_id AND ts.outcome_id = r.outcome_id)}
