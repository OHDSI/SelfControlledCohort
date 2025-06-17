create table @database_schema.scc_analysis_setting (
   analysis_id INT PRIMARY KEY,
   description VARCHAR,
   settings VARCHAR
);

create table @database_schema.scc_result (
    database_id VARCHAR NOT NULL,
    analysis_id INT NOT NULL,
    outcome_cohort_id BIGINT NOT NULL,
    target_cohort_id BIGINT NOT NULL,
    rr NUMERIC,
    se_log_rr NUMERIC,
    log_rr NUMERIC,
    lb_95 NUMERIC,
    ub_95 NUMERIC,
    p_value NUMERIC,
    -- calibrated results
    calibrated_rr NUMERIC,
    calibrated_se_log_rr NUMERIC,
    calibrated_log_rr NUMERIC,
    calibrated_lb_95 NUMERIC,
    calibrated_ub_95 NUMERIC,
    calibrated_p_value NUMERIC,
    exposure_calibrated INT,
    c_pt NUMERIC,
    t_pt NUMERIC,
    t_at_risk NUMERIC,
    c_at_risk NUMERIC,
    t_cases NUMERIC,
    c_cases NUMERIC,
    num_exposures NUMERIC,
    i2 NUMERIC,
    PRIMARY KEY (database_id, analysis_id, outcome_cohort_id, target_cohort_id)
);

create TABLE @database_schema.scc_stat (
    database_id VARCHAR NOT NULL,
    analysis_id INT NOT NULL,
    outcome_cohort_id BIGINT NOT NULL,
    target_cohort_id BIGINT NOT NULL,
    stat_type varchar(100),
    mean NUMERIC,
    sd NUMERIC,
    minimum NUMERIC,
    p10 NUMERIC,
    p25 NUMERIC,
    median NUMERIC,
    p75 NUMERIC,
    p90 NUMERIC,
    maximum NUMERIC,
    total NUMERIC,
    PRIMARY KEY (database_id, analysis_id, outcome_cohort_id, target_cohort_id, stat_type)
);

create TABLE @database_schema.scc_diagnostics_summary (
    database_id VARCHAR NOT NULL,
    analysis_id INT NOT NULL,
    outcome_cohort_id BIGINT NOT NULL,
    target_cohort_id BIGINT NOT NULL,
    diagnostic_value NUMERIC,
    pass INT,
    diagnostic_name TEXT NOT NULL,
    PRIMARY KEY (database_id, analysis_id, outcome_cohort_id, target_cohort_id)
);

create TABLE @database_schema.scc_outcome_exposure (
    outcome_cohort_id BIGINT NOT NULL,
    target_cohort_id BIGINT NOT NULL,
    true_effect_size NUMERIC,
    PRIMARY KEY (outcome_cohort_id, target_cohort_id)
);
