# Creates a small synthetic mock results database for exploring
# extras/ResultsExploration.qmd.
#
# Usage (from the repository root):
#   Rscript extras/mockResults/createMockResultsDb.R
# or with an explicit output path:
#   Rscript extras/mockResults/createMockResultsDb.R /path/to/mockResults.sqlite

suppressMessages(library(DatabaseConnector))
suppressMessages(library(dplyr))

args <- commandArgs(trailingOnly = TRUE)
outPath <- if (length(args) >= 1) {
  args[1]
} else {
  file.path("extras", "mockResults", "mockResults.sqlite")
}
outPath <- normalizePath(outPath, mustWork = FALSE)
if (file.exists(outPath)) {
  file.remove(outPath)
}
dir.create(dirname(outPath), showWarnings = FALSE, recursive = TRUE)

connectionDetails <- createConnectionDetails(dbms = "sqlite", server = outPath)
connection <- connect(connectionDetails)
on.exit(disconnect(connection), add = TRUE)

insert <- function(table, data) {
  DatabaseConnector::insertTable(
    connection = connection,
    databaseSchema = NULL,
    tableName = table,
    data = data,
    dropTableIfExists = TRUE,
    createTable = TRUE,
    tempTable = FALSE
  )
}

# Cohort references (CohortGenerator data model) --------------------------
cg_cohort_definition <- data.frame(
  cohort_definition_id = c(1L, 2L, 101L, 102L, 103L, 104L, 105L, 106L),
  cohort_name = c(
    "ACE inhibitors",
    "Beta blockers",
    "Angioedema",
    "Cough",
    "Hyperkalemia",
    "Hypoglycemia",
    "Fracture (negative control)",
    "Appendicitis (negative control)"
  ),
  description = "",
  json = "{}",
  sql_command = "SELECT 1;",
  stringsAsFactors = FALSE
)

# Analysis settings --------------------------------------------------------
scc_analysis_setting <- data.frame(
  analysis_id = 1L,
  description = "SCC analysis 1: 30-day risk window",
  settings = '{"firstExposureOnly":true,"riskWindowStartExposed":1,"riskWindowEndExposed":30}',
  stringsAsFactors = FALSE
)

# Exposure-outcome pairs; negative controls have true_effect_size = 1 ------
scc_outcome_exposure <- expand.grid(
  target_cohort_id = c(1L, 2L),
  outcome_cohort_id = c(101L, 102L, 103L, 104L, 105L, 106L),
  stringsAsFactors = FALSE
) |>
  dplyr::mutate(true_effect_size = ifelse(outcome_cohort_id %in% c(105L, 106L), 1, NA_real_))

# Site-level results -------------------------------------------------------
sites <- c("Site A", "Site B", "Site C")
siteScale <- c(1.0, 1.4, 1.1)

# Pair-level meta estimates (evidence synthesis analysis 1)
pairSpecs <- data.frame(
  target_cohort_id = c(1L, 1L, 1L, 2L, 2L, 2L, 1L, 2L, 1L, 2L),
  outcome_cohort_id = c(101L, 102L, 103L, 101L, 103L, 104L, 105L, 105L, 106L, 106L),
  meta_rr = c(0.32, 0.40, 0.60, 0.40, 0.45, 2.50, 1.00, 1.05, 0.98, 1.02),
  meta_lb = c(0.22, 0.30, 0.50, 0.30, 0.35, 1.80, 0.80, 0.85, 0.78, 0.82),
  meta_ub = c(0.45, 0.55, 1.20, 0.55, 0.60, 3.50, 1.30, 1.30, 1.25, 1.27),
  meta_p = c(0.001, 0.002, 0.06, 0.002, 0.005, 0.001, 0.90, 0.80, 0.85, 0.90),
  meta_pi_lb = c(0.18, 0.25, 0.40, 0.26, 0.30, 1.40, 0.70, 0.75, 0.70, 0.72),
  meta_pi_ub = c(0.55, 0.65, 1.50, 0.62, 0.68, 4.50, 1.50, 1.50, 1.40, 1.45),
  stringsAsFactors = FALSE
)

siteRows <- do.call(rbind, lapply(seq_len(nrow(pairSpecs)), function(i) {
  spec <- pairSpecs[i, ]
  do.call(rbind, lapply(seq_along(sites), function(s) {
    sc <- siteScale[s]
    rr <- spec$meta_rr * c(0.9, 1.05, 0.95)[s]
    lb <- rr * exp(-0.16)
    ub <- rr * exp(0.16)
    p <- min(1, spec$meta_p * c(1.2, 0.8, 1.0)[s])
    nExposed <- pmax(2, round(10 * rr / (1 + rr) * sc))
    nUnexposed <- pmax(2, round(10 / (1 + rr) * sc))
    data.frame(
      database_id = sites[s],
      analysis_id = 1L,
      outcome_cohort_id = spec$outcome_cohort_id,
      target_cohort_id = spec$target_cohort_id,
      rr = rr,
      se_log_rr = 0.16,
      lb_95 = lb,
      ub_95 = ub,
      p_value = p,
      calibrated_rr = rr,
      calibrated_se_log_rr = 0.16,
      calibrated_lb_95 = lb,
      calibrated_ub_95 = ub,
      calibrated_p_value = p,
      num_persons = round(600 * sc),
      time_at_risk_exposed = round(25000 * sc),
      time_at_risk_unexposed = round(60000 * sc),
      num_outcomes_exposed = nExposed,
      num_outcomes_unexposed = nUnexposed,
      num_exposures = round(520 * sc),
      i2 = NA_real_,
      stringsAsFactors = FALSE
    )
  }))
}))

# Site-level diagnostics ---------------------------------------------------
diagRows <- do.call(rbind, lapply(seq_len(nrow(siteRows)), function(i) {
  r <- siteRows[i, ]
  data.frame(
    database_id = r$database_id,
    analysis_id = r$analysis_id,
    outcome_cohort_id = r$outcome_cohort_id,
    target_cohort_id = r$target_cohort_id,
    diagnostic_name = c("MDRR", "PRE_EXPOSURE_RATE_RATIO", "PRE_EXPOSURE_P_VALUE", "EVENT_DEPENDENT_OBSERVATION"),
    diagnostic_value = c(6.0, 1.1, 0.3, 0.05),
    pass = 1L,
    stringsAsFactors = FALSE
  )
}))

# EASE is computed per target (outcome_cohort_id = 0)
easeRows <- data.frame(
  database_id = rep(sites, each = 2),
  analysis_id = 1L,
  outcome_cohort_id = 0L,
  target_cohort_id = rep(c(1L, 2L), times = length(sites)),
  diagnostic_name = "EASE",
  diagnostic_value = 0.16,
  pass = 1L,
  stringsAsFactors = FALSE
)

scc_diagnostics_summary <- rbind(diagRows, easeRows)

# Deliberate site-level FAIL on a pair that is still shown, so the details
# panel demonstrates a FAIL badge (the meta-analysis gate is es unblind).
idx <- which(
  scc_diagnostics_summary$database_id == "Site C" &
    scc_diagnostics_summary$target_cohort_id == 2L &
    scc_diagnostics_summary$outcome_cohort_id == 103L &
    scc_diagnostics_summary$diagnostic_name == "MDRR"
)
scc_diagnostics_summary$diagnostic_value[idx] <- 11.0
scc_diagnostics_summary$pass[idx] <- 0L

# Evidence synthesis analysis settings ------------------------------------
es_analysis <- data.frame(
  evidence_synthesis_analysis_id = c(1L, 2L),
  evidence_synthesis_description = c(
    "Random-effects SCC meta-analysis",
    "Fixed-effects SCC meta-analysis"
  ),
  source_method = "SelfControlledCohort",
  definition = "{}",
  stringsAsFactors = FALSE
)

# Meta-analysis results (evidence synthesis analysis 1) --------------------
siteCounts <- siteRows |>
  dplyr::group_by(target_cohort_id, outcome_cohort_id) |>
  dplyr::summarise(
    num_persons = sum(num_persons),
    time_at_risk_exposed = sum(time_at_risk_exposed),
    time_at_risk_unexposed = sum(time_at_risk_unexposed),
    num_outcomes_exposed = sum(num_outcomes_exposed),
    num_outcomes_unexposed = sum(num_outcomes_unexposed),
    num_exposures = sum(num_exposures),
    .groups = "drop"
  )

es_scc_result <- pairSpecs |>
  dplyr::left_join(siteCounts, by = c("target_cohort_id", "outcome_cohort_id")) |>
  dplyr::mutate(
    evidence_synthesis_analysis_id = 1L,
    analysis_id = 1L,
    rr = meta_rr,
    ci_95_lb = meta_lb,
    ci_95_ub = meta_ub,
    p = meta_p,
    one_sided_p = pmin(1, meta_p / 2),
    log_rr = log(meta_rr),
    se_log_rr = (log(meta_ub) - log(meta_lb)) / (2 * qnorm(0.975)),
    calibrated_rr = meta_rr,
    calibrated_ci_95_lb = meta_lb,
    calibrated_ci_95_ub = meta_ub,
    calibrated_p = meta_p,
    calibrated_one_sided_p = pmin(1, meta_p / 2),
    calibrated_log_rr = log(meta_rr),
    calibrated_se_log_rr = se_log_rr,
    pi_95_lb = meta_pi_lb,
    pi_95_ub = meta_pi_ub,
    calibrated_pi_95_lb = meta_pi_lb,
    calibrated_pi_95_ub = meta_pi_ub,
    n_databases = 3L
  ) |>
  dplyr::select(
    target_cohort_id, outcome_cohort_id, analysis_id, evidence_synthesis_analysis_id,
    rr, ci_95_lb, ci_95_ub, p, one_sided_p, log_rr, se_log_rr,
    num_persons, time_at_risk_exposed, time_at_risk_unexposed,
    num_outcomes_exposed, num_outcomes_unexposed, num_exposures, n_databases,
    calibrated_rr, calibrated_ci_95_lb, calibrated_ci_95_ub, calibrated_p,
    calibrated_one_sided_p, calibrated_log_rr, calibrated_se_log_rr,
    pi_95_lb, pi_95_ub, calibrated_pi_95_lb, calibrated_pi_95_ub
  )

# Meta-analysis diagnostics (evidence synthesis analysis 1) ----------------
es_scc_diagnostics_summary <- data.frame(
  target_cohort_id = c(1L, 1L, 1L, 2L, 2L, 2L, 1L, 2L, 1L, 2L),
  outcome_cohort_id = c(101L, 102L, 103L, 101L, 103L, 104L, 105L, 105L, 106L, 106L),
  analysis_id = 1L,
  evidence_synthesis_analysis_id = 1L,
  mdrr = c(3.2, 3.5, 4.0, 4.2, 3.0, 5.0, 6.0, 6.5, 5.8, 6.2),
  i_2 = c(0.15, 0.20, 0.35, 0.70, 0.10, 0.30, 0.05, 0.08, 0.04, 0.06),
  tau = c(0.08, 0.10, 0.18, 0.40, 0.06, 0.15, 0.02, 0.03, 0.02, 0.03),
  ease = c(0.12, 0.14, 0.20, 0.30, 0.10, 0.18, 0.08, 0.09, 0.07, 0.08),
  mdrr_diagnostic = "PASS",
  i_2_diagnostic = c("PASS", "PASS", "PASS", "FAIL", "PASS", "PASS", "PASS", "PASS", "PASS", "PASS"),
  tau_diagnostic = "PASS",
  ease_diagnostic = c("PASS", "PASS", "PASS", "FAIL", "PASS", "PASS", "PASS", "PASS", "PASS", "PASS"),
  unblind = c(1L, 1L, 1L, 0L, 1L, 1L, 1L, 1L, 1L, 1L),
  stringsAsFactors = FALSE
)

# Write everything ----------------------------------------------------------
insert("cg_cohort_definition", cg_cohort_definition)
insert("scc_analysis_setting", scc_analysis_setting)
insert("scc_outcome_exposure", scc_outcome_exposure)
insert("scc_result", siteRows)
insert("scc_diagnostics_summary", scc_diagnostics_summary)
insert("es_analysis", es_analysis)
insert("es_scc_result", es_scc_result)
insert("es_scc_diagnostics_summary", es_scc_diagnostics_summary)

message(sprintf("Mock results database written to %s", outPath))
message(sprintf("- %d site result rows across %d data sources", nrow(siteRows), length(sites)))
message(sprintf("- %d meta-analysis results, %d evidence synthesis analyses", nrow(es_scc_result), nrow(es_analysis)))
message(sprintf("- %d exposure-outcome pairs (%d negative controls)",
                nrow(scc_outcome_exposure), sum(scc_outcome_exposure$true_effect_size == 1, na.rm = TRUE)))
