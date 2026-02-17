# Copyright 2025 Observational Health Data Sciences and Informatics
#
# This file is part of SelfControlledCohort
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

#' Get default diagnostic thresholds
#'
#' @description
#' Returns default thresholds for diagnostic tests following SCCS standards.
#' Version 2.1.0+ uses revised diagnostics that align with SelfControlledCaseSeries package.
#'
#' @return A list of diagnostic thresholds
#'
#' @details
#' Thresholds:
#' \itemize{
#'   \item mdrrMaxAcceptable - Maximum acceptable MDRR (default: 2.0). Higher values indicate low power.
#'   \item maxPreExposureProportion - Maximum proportion of persons with pre-exposure outcomes (default: 0.05)
#'   \item preExposurePThreshold - Significance threshold for pre-exposure gain test (default: 0.05)
#'   \item maxEventDependentCensoring - Maximum proportion censored within 30 days of outcome (default: 0.10)
#'   \item timeTrendPThreshold - Significance threshold for time trend test (default: 0.05)
#'   \item minEventsPerWindow - Minimum events required in each window (default: 3)
#' }
#'
#' @export
getDefaultDiagnosticThresholds <- function() {
  list(
    mdrrMaxAcceptable = 2.0, # Max MDRR for adequate power
    maxPreExposureProportion = 0.05, # Max 5% with pre-exposure outcomes
    preExposurePThreshold = 0.05, # Significance level for pre-exposure test
    maxEventDependentCensoring = 0.10, # Max 10% censored within 30 days of outcome
    timeTrendPThreshold = 0.05, # Significance level for time trend
    minEventsPerWindow = 3 # Min 3 events in each window
  )
}

#' Run Self-Controlled Cohort Diagnostics
#'
#' @description
#' Runs a suite of diagnostic tests to assess the validity of SCC analysis assumptions.
#' Version 1.6.0+ implements diagnostics aligned with SelfControlledCaseSeries package standards.
#'
#' @param connection                  DatabaseConnector connection instance
#' @param cdmDatabaseSchema           Name of database schema that contains OMOP CDM
#' @param tempEmulationSchema         Schema for temp table emulation (Oracle, Impala)
#' @param resultsTable                Name of the results table (can be temporary or permanent)
#' @param riskWindowsTable            Name of the risk windows table
#' @param outcomeTable                Name of outcome table (e.g., "condition_era", "cohort")
#' @param outcomeDatabaseSchema       Schema containing outcome table
#' @param exposureTable               Name of exposure table (e.g., "drug_era", "cohort")
#' @param exposureDatabaseSchema      Schema containing exposure table
#' @param analysisId                  Analysis identifier
#' @param databaseId                  Database identifier for results export
#' @param diagnostics                 Character vector of diagnostics to run. Options:
#'                                    "all", "mdrr", "pre_exposure_gain", "event_dependent",
#'                                    "time_trend", "sparse_data"
#' @param thresholds                  Named list of diagnostic thresholds (see getDefaultDiagnosticThresholds)
#' @param resultExportManager         ResultModelManager::ResultExportManager instance
#'
#' @details
#' Available diagnostics:
#' \itemize{
#'   \item mdrr - Minimum Detectable Relative Risk (power analysis)
#'   \item pre_exposure_gain - Tests for outcomes before exposure start
#'   \item event_dependent - Tests for outcome-induced censoring
#'   \item time_trend - Tests for temporal trends in outcome risk
#'   \item sparse_data - Checks for sufficient events in both windows
#' }
#'
#' @return
#' Invisible data frame of diagnostic results
#'
#' @export
runSccDiagnostics <- function(connection,
                              cdmDatabaseSchema,
                              tempEmulationSchema = getOption("sqlRenderTempEmulationSchema"),
                              resultsTable,
                              riskWindowsTable,
                              outcomeTable = "condition_era",
                              outcomeDatabaseSchema = cdmDatabaseSchema,
                              analysisId,
                              databaseId,
                              diagnostics = c("all"),
                              thresholds = getDefaultDiagnosticThresholds(),
                              resultExportManager) {
  if (!DatabaseConnector::dbIsValid(connection)) {
    stop("Invalid connection object")
  }

  checkmate::assertR6(resultExportManager, "ResultExportManager")

  # Expand "all" to specific diagnostics
  allDiagnostics <- c(
    "mdrr", "pre_exposure_gain", "event_dependent",
    "time_trend", "sparse_data"
  )

  if ("all" %in% diagnostics) {
    diagnostics <- allDiagnostics
  }

  ParallelLogger::logInfo("Running SCC diagnostics")

  diagnosticResults <- data.frame()

  # Run MDRR diagnostic (power analysis)
  if ("mdrr" %in% diagnostics) {
    ParallelLogger::logInfo("- Running MDRR (power) diagnostic")
    mdrrResult <- .computeMdrrDiagnostic(
      connection = connection,
      resultsTable = resultsTable,
      analysisId = analysisId,
      thresholds = thresholds,
      tempEmulationSchema = tempEmulationSchema
    )
    diagnosticResults <- rbind(diagnosticResults, mdrrResult)
  }

  # Run pre-exposure gain diagnostic
  if ("pre_exposure_gain" %in% diagnostics) {
    ParallelLogger::logInfo("- Running pre-exposure gain diagnostic")
    preExpGainResult <- .computePreExposureGainDiagnostic(
      connection = connection,
      cdmDatabaseSchema = cdmDatabaseSchema,
      riskWindowsTable = riskWindowsTable,
      outcomeTable = outcomeTable,
      outcomeDatabaseSchema = outcomeDatabaseSchema,
      analysisId = analysisId,
      thresholds = thresholds,
      tempEmulationSchema = tempEmulationSchema
    )
    diagnosticResults <- rbind(diagnosticResults, preExpGainResult)
  }

  # Run event-dependent observation diagnostic
  if ("event_dependent" %in% diagnostics) {
    ParallelLogger::logInfo("- Running event-dependent observation diagnostic")
    eventDepResult <- .computeEventDependentDiagnostic(
      connection = connection,
      cdmDatabaseSchema = cdmDatabaseSchema,
      riskWindowsTable = riskWindowsTable,
      outcomeTable = outcomeTable,
      outcomeDatabaseSchema = outcomeDatabaseSchema,
      analysisId = analysisId,
      thresholds = thresholds,
      tempEmulationSchema = tempEmulationSchema
    )
    diagnosticResults <- rbind(diagnosticResults, eventDepResult)
  }

  # Run time trend diagnostic
  if ("time_trend" %in% diagnostics) {
    ParallelLogger::logInfo("- Running time trend diagnostic")
    timeTrendResult <- .computeTimeTrendDiagnostic(
      connection = connection,
      riskWindowsTable = riskWindowsTable,
      outcomeTable = outcomeTable,
      outcomeDatabaseSchema = outcomeDatabaseSchema,
      analysisId = analysisId,
      thresholds = thresholds,
      tempEmulationSchema = tempEmulationSchema
    )
    diagnosticResults <- rbind(diagnosticResults, timeTrendResult)
  }

  # Run sparse data diagnostic
  if ("sparse_data" %in% diagnostics) {
    ParallelLogger::logInfo("- Running sparse data diagnostic")
    sparseResult <- .computeSparseDataDiagnostic(
      connection = connection,
      resultsTable = resultsTable,
      analysisId = analysisId,
      thresholds = thresholds,
      tempEmulationSchema = tempEmulationSchema
    )
    diagnosticResults <- rbind(diagnosticResults, sparseResult)
  }

  # Add database_id to results
  if (nrow(diagnosticResults) > 0) {
    diagnosticResults$database_id <- databaseId

    # Reorder columns to match schema
    diagnosticResults <- diagnosticResults |>
      dplyr::select(
        "database_id", "analysis_id", "target_cohort_id", "outcome_cohort_id",
        "diagnostic_name", "diagnostic_value", "pass"
      )

    # Export results
    resultExportManager$exportDataFrame(diagnosticResults,
      "scc_diagnostics_summary",
      append = FALSE
    )

    ParallelLogger::logInfo(sprintf("Completed %d diagnostic tests", nrow(diagnosticResults)))

    # Summary of failures
    failures <- diagnosticResults |>
      dplyr::filter(.data$pass == 0)

    if (nrow(failures) > 0) {
      ParallelLogger::logWarn(sprintf("%d diagnostic test(s) failed:", nrow(failures)))
      for (i in seq_len(nrow(failures))) {
        ParallelLogger::logWarn(sprintf(
          "  - %s (Target: %s, Outcome: %s, Value: %.3f)",
          failures$diagnostic_name[i],
          failures$target_cohort_id[i],
          failures$outcome_cohort_id[i],
          failures$diagnostic_value[i]
        ))
      }
    } else {
      ParallelLogger::logInfo("All diagnostic tests passed")
    }
  } else {
    ParallelLogger::logWarn("No diagnostic results generated")
  }

  return(invisible(diagnosticResults))
}

#' Compute MDRR (power) diagnostic
#' @noRd
.computeMdrrDiagnostic <- function(connection,
                                   resultsTable,
                                   analysisId,
                                   thresholds,
                                   tempEmulationSchema) {
  sql <- "
  SELECT
    target_cohort_id,
    outcome_cohort_id,
    num_outcomes_exposed,
    num_outcomes_unexposed,
    time_at_risk_exposed,
    time_at_risk_unexposed
  FROM @results_table
  WHERE analysis_id = @analysis_id
  "

  results <- DatabaseConnector::renderTranslateQuerySql(
    connection = connection,
    sql = sql,
    results_table = resultsTable,
    analysis_id = analysisId,
    tempEmulationSchema = tempEmulationSchema,
    snakeCaseToCamelCase = TRUE
  )

  if (nrow(results) == 0) {
    return(data.frame())
  }

  diagnostics <- data.frame()

  for (i in seq_len(nrow(results))) {
    row <- results[i, ]

    # Compute MDRR
    mdrr <- computeMdrrForRateRatio(
      exposedPersonTime = row$timeAtRiskExposed,
      unexposedPersonTime = row$timeAtRiskUnexposed,
      exposedEvents = row$numOutcomesExposed,
      unexposedEvents = row$numOutcomesUnexposed,
      alpha = 0.05,
      power = 0.80
    )

    # Check if MDRR is acceptable
    pass <- if (is.na(mdrr)) {
      0L # Fail if MDRR cannot be computed
    } else {
      as.integer(mdrr <= thresholds$mdrrMaxAcceptable)
    }

    diagRow <- data.frame(
      analysis_id = analysisId,
      target_cohort_id = row$targetCohortId,
      outcome_cohort_id = row$outcomeCohortId,
      diagnostic_name = "MDRR",
      diagnostic_value = mdrr,
      pass = pass
    )
    diagnostics <- rbind(diagnostics, diagRow)
  }

  return(diagnostics)
}

#' Compute pre-exposure gain diagnostic
#' @noRd
.computePreExposureGainDiagnostic <- function(connection,
                                              cdmDatabaseSchema,
                                              riskWindowsTable,
                                              outcomeTable,
                                              outcomeDatabaseSchema,
                                              analysisId,
                                              thresholds,
                                              tempEmulationSchema) {
  preExpData <- testPreExposureGain(
    connection = connection,
    riskWindowsTable = riskWindowsTable,
    outcomeTable = outcomeTable,
    outcomeDatabaseSchema = outcomeDatabaseSchema,
    analysisId = analysisId,
    tempEmulationSchema = tempEmulationSchema
  )

  if (nrow(preExpData) == 0) {
    return(data.frame())
  }

  diagnostics <- data.frame()

  for (i in seq_len(nrow(preExpData))) {
    row <- preExpData[i, ]

    # Test passes if proportion <= threshold AND p-value > threshold
    pass <- as.integer(
      row$proportion <= thresholds$maxPreExposureProportion &&
        row$pValue > thresholds$preExposurePThreshold
    )

    # Add proportion diagnostic
    diagRow1 <- data.frame(
      analysis_id = analysisId,
      target_cohort_id = row$targetCohortId,
      outcome_cohort_id = row$outcomeCohortId,
      diagnostic_name = "PRE_EXPOSURE_PROPORTION",
      diagnostic_value = row$proportion,
      pass = as.integer(row$proportion <= thresholds$maxPreExposureProportion)
    )
    diagnostics <- rbind(diagnostics, diagRow1)

    # Add p-value diagnostic
    diagRow2 <- data.frame(
      analysis_id = analysisId,
      target_cohort_id = row$targetCohortId,
      outcome_cohort_id = row$outcomeCohortId,
      diagnostic_name = "PRE_EXPOSURE_P_VALUE",
      diagnostic_value = row$pValue,
      pass = as.integer(row$pValue > thresholds$preExposurePThreshold)
    )
    diagnostics <- rbind(diagnostics, diagRow2)
  }

  return(diagnostics)
}

#' Compute event-dependent observation diagnostic
#' @noRd
.computeEventDependentDiagnostic <- function(connection,
                                             cdmDatabaseSchema,
                                             riskWindowsTable,
                                             outcomeTable,
                                             outcomeDatabaseSchema,
                                             analysisId,
                                             thresholds,
                                             tempEmulationSchema) {
  # Determine outcome table columns
  outcomeTable <- tolower(outcomeTable)
  if (outcomeTable == "condition_era") {
    outcomeStartDate <- "condition_era_start_date"
    outcomeId <- "condition_concept_id"
    outcomePersonId <- "person_id"
  } else if (outcomeTable == "condition_occurrence") {
    outcomeStartDate <- "condition_start_date"
    outcomeId <- "condition_concept_id"
    outcomePersonId <- "person_id"
  } else {
    outcomeStartDate <- "cohort_start_date"
    outcomeId <- "cohort_definition_id"
    outcomePersonId <- "subject_id"
  }

  sql <- "
  SELECT
    rw.exposure_id as target_cohort_id,
    o.@outcome_id as outcome_cohort_id,
    COUNT(DISTINCT rw.person_id) as total_persons_with_outcome,
    SUM(CASE WHEN DATEDIFF(day, o.@outcome_start_date, op.observation_period_end_date) <= 30
             THEN 1 ELSE 0 END) as censored_within_30_days
  FROM @risk_windows_table rw
  INNER JOIN @outcome_database_schema.@outcome_table o
    ON rw.person_id = o.@outcome_person_id
    AND o.@outcome_start_date >= rw.risk_window_start_unexposed
    AND o.@outcome_start_date <= rw.risk_window_end_exposed
  INNER JOIN @cdm_database_schema.observation_period op
    ON rw.person_id = op.person_id
    AND o.@outcome_start_date >= op.observation_period_start_date
    AND o.@outcome_start_date <= op.observation_period_end_date
  WHERE rw.analysis_id = @analysis_id
  GROUP BY rw.exposure_id, o.@outcome_id
  "

  results <- DatabaseConnector::renderTranslateQuerySql(
    connection = connection,
    sql = sql,
    risk_windows_table = riskWindowsTable,
    outcome_database_schema = outcomeDatabaseSchema,
    outcome_table = outcomeTable,
    outcome_start_date = outcomeStartDate,
    outcome_id = outcomeId,
    outcome_person_id = outcomePersonId,
    cdm_database_schema = cdmDatabaseSchema,
    analysis_id = analysisId,
    tempEmulationSchema = tempEmulationSchema,
    snakeCaseToCamelCase = TRUE
  )

  if (nrow(results) == 0) {
    return(data.frame())
  }

  diagnostics <- data.frame()

  for (i in seq_len(nrow(results))) {
    row <- results[i, ]

    proportion <- row$censoredWithin30Days / row$totalPersonsWithOutcome

    diagRow <- data.frame(
      analysis_id = analysisId,
      target_cohort_id = row$targetCohortId,
      outcome_cohort_id = row$outcomeCohortId,
      diagnostic_name = "EVENT_DEPENDENT_OBSERVATION",
      diagnostic_value = proportion,
      pass = as.integer(proportion <= thresholds$maxEventDependentCensoring)
    )
    diagnostics <- rbind(diagnostics, diagRow)
  }

  return(diagnostics)
}

#' Compute time trend diagnostic
#' @noRd
.computeTimeTrendDiagnostic <- function(connection,
                                        riskWindowsTable,
                                        outcomeTable,
                                        outcomeDatabaseSchema,
                                        analysisId,
                                        thresholds,
                                        tempEmulationSchema) {
  timeTrendData <- testTimeTrend(
    connection = connection,
    riskWindowsTable = riskWindowsTable,
    outcomeTable = outcomeTable,
    outcomeDatabaseSchema = outcomeDatabaseSchema,
    analysisId = analysisId,
    tempEmulationSchema = tempEmulationSchema
  )

  if (nrow(timeTrendData) == 0) {
    return(data.frame())
  }

  diagnostics <- data.frame()

  for (i in seq_len(nrow(timeTrendData))) {
    row <- timeTrendData[i, ]

    # Test passes if p-value > threshold (no significant time trend)
    pVal <- row$timeTrendPValue
    pass <- if (is.null(pVal) || length(pVal) == 0 || is.na(pVal)) {
      1L # Pass if cannot compute (not enough data)
    } else {
      as.integer(pVal > thresholds$timeTrendPThreshold)
    }

    diagRow <- data.frame(
      analysis_id = analysisId,
      target_cohort_id = row$targetCohortId,
      outcome_cohort_id = row$outcomeCohortId,
      diagnostic_name = "TIME_TREND_P_VALUE",
      diagnostic_value = if (is.null(pVal) || length(pVal) == 0) NA_real_ else pVal,
      pass = pass
    )
    diagnostics <- rbind(diagnostics, diagRow)
  }

  return(diagnostics)
}

#' Compute sparse data diagnostic
#' @noRd
.computeSparseDataDiagnostic <- function(connection,
                                         resultsTable,
                                         analysisId,
                                         thresholds,
                                         tempEmulationSchema) {
  sql <- "
  SELECT
    target_cohort_id,
    outcome_cohort_id,
    num_outcomes_exposed,
    num_outcomes_unexposed
  FROM @results_table
  WHERE analysis_id = @analysis_id
  "

  results <- DatabaseConnector::renderTranslateQuerySql(
    connection = connection,
    sql = sql,
    results_table = resultsTable,
    analysis_id = analysisId,
    tempEmulationSchema = tempEmulationSchema,
    snakeCaseToCamelCase = TRUE
  )

  if (nrow(results) == 0) {
    return(data.frame())
  }

  diagnostics <- data.frame()

  for (i in seq_len(nrow(results))) {
    row <- results[i, ]

    # Check if both windows have sufficient events
    sparseCheck <- checkSparseData(
      exposedEvents = row$numOutcomesExposed,
      unexposedEvents = row$numOutcomesUnexposed,
      minEvents = thresholds$minEventsPerWindow
    )

    pass <- as.integer(sparseCheck$pass)

    # Add exposed window check
    diagRow1 <- data.frame(
      analysis_id = analysisId,
      target_cohort_id = row$targetCohortId,
      outcome_cohort_id = row$outcomeCohortId,
      diagnostic_name = "SPARSE_EXPOSED",
      diagnostic_value = row$numOutcomesExposed,
      pass = as.integer(row$numOutcomesExposed >= thresholds$minEventsPerWindow)
    )
    diagnostics <- rbind(diagnostics, diagRow1)

    # Add unexposed window check
    diagRow2 <- data.frame(
      analysis_id = analysisId,
      target_cohort_id = row$targetCohortId,
      outcome_cohort_id = row$outcomeCohortId,
      diagnostic_name = "SPARSE_UNEXPOSED",
      diagnostic_value = row$numOutcomesUnexposed,
      pass = as.integer(row$numOutcomesUnexposed >= thresholds$minEventsPerWindow)
    )
    diagnostics <- rbind(diagnostics, diagRow2)
  }

  return(diagnostics)
}
