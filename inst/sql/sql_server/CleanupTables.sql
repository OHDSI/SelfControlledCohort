{@outcome_ids != ''} ? {
TRUNCATE TABLE #scc_outcome_ids;
DROP TABLE #scc_outcome_ids;
}

{@exposure_ids != ''} ? {
TRUNCATE TABLE #scc_exposure_ids;
DROP TABLE #scc_exposure_ids;
}

TRUNCATE TABLE #scc_exposure_summary;
DROP TABLE #scc_exposure_summary;

TRUNCATE TABLE #scc_outcome_summary;
DROP TABLE #scc_outcome_summary;

{@risk_windows_table == #risk_windows} ? {
    TRUNCATE TABLE #risk_windows;
    DROP TABLE #risk_windows;
}

{@results_table == #results} ? {
    TRUNCATE TABLE #results;
    DROP TABLE #results;
}
{@compute_tar_distribution} ? {
    TRUNCATE TABLE #tar_stats;
    DROP TABLE #tar_stats;
}