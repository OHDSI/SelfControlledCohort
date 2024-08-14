{DEFAULT @cohort_definition = 'cohort_definition'}
{DEFAULT @exposure_cohort = 'exposure_cohort'}
{DEFAULT @outcome_cohort = 'outcome_cohort'}
{DEFAULT @concept_set_definition = 'concept_set_definition'}
{DEFAULT @atlas_cohort_reference = 'atlas_cohort_reference'}
{DEFAULT @cohort_concept_set = 'cohort_concept_set'}
{DEFAULT @analysis_setting = 'analysis_setting'}

create table @schema.scc_analysis_setting (
   analysis_id INT PRIMARY KEY,
   description VARCHAR,
   settings VARCHAR
);

create table @schema.scc_result (
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
    i_2 NUMERIC,
    PRIMARY KEY (source_id, analysis_id, outcome_cohort_id, target_cohort_id),
);

drop table IF EXISTS @schema.scc_stat;
create TABLE @schema.scc_stat (
    source_id INT NOT NULL,
    analysis_id INT NOT NULL,
    outcome_cohort_id BIGINT NOT NULL,
    target_cohort_id BIGINT NOT NULL,
    stat_type varchar(100),
    mean NUMERIC,
    sd NUMERIC,
    minimum NUMERIC,
    p_10 NUMERIC,
    p_25 NUMERIC,
    median NUMERIC,
    p_75 NUMERIC,
    p_90 NUMERIC,
    maximum NUMERIC,
    total NUMERIC,
    PRIMARY KEY (source_id, analysis_id, outcome_cohort_id, target_cohort_id, stat_type)
);
