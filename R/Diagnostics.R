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
#'   \item mdrrMaxAcceptable - Maximum acceptable MDRR (default: 10.0). Higher values indicate low power.
#'   \item maxPreExposureProportion - Maximum proportion of persons with pre-exposure outcomes (default: 0.05)
#'   \item preExposurePThreshold - Significance threshold for pre-exposure gain test (default: 0.05)
#'   \item maxEventDependentCensoring - Maximum proportion censored within 30 days of outcome (default: 0.10)
#'   \item minEventsPerWindow - Minimum events required in each window (default: 3)
#'   \item easeMaxAcceptable - Maximum acceptable EASE (default: 0.25). Requires negative controls.
#' }
#'
#' @export
getDefaultDiagnosticThresholds <- function() {
  list(
    mdrrMaxAcceptable = 10.0, # Max MDRR for adequate power
    maxPreExposureProportion = 0.05, # Max 5% with pre-exposure outcomes
    preExposurePThreshold = 0.05, # Significance level for pre-exposure test
    maxEventDependentCensoring = 0.10, # Max 10% censored within 30 days of outcome
    minEventsPerWindow = 3, # Min 3 events in each window
    easeMaxAcceptable = 0.25 # Max EASE for acceptable systematic error
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
#' @param resultsTable                Name of the results table (contains counts)
#' @param riskWindowsTable            Name of the risk windows table
#' @param outcomeTable                Name of outcome table (e.g., "condition_era", "cohort")
#' @param outcomeDatabaseSchema       Schema containing outcome table
#' @param analysisId                  Analysis identifier
#' @param databaseId                  Database identifier for results export
#' @param estimates                   Data frame of raw SCC results (including rr and se_log_rr)
#' @param diagnostics                 Character vector of diagnostics to run. Options:
#'                                    "all", "mdrr", "pre_exposure_gain", "event_dependent", "ease"
#' @param thresholds                  Named list of diagnostic thresholds (see getDefaultDiagnosticThresholds)
#' @param resultExportManager         ResultModelManager::ResultExportManager instance
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
                              estimates = NULL,
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
    "ease"
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

  # Run EASE diagnostic (systematic error)
  if ("ease" %in% diagnostics && !is.null(estimates)) {
    ParallelLogger::logInfo("- Running EASE (systematic error) diagnostic")
    easeResult <- .computeEaseDiagnostic(
      estimates = estimates,
      analysisId = analysisId,
      thresholds = thresholds
    )
    diagnosticResults <- rbind(diagnosticResults, easeResult)
  }

  # Add database_id to results
  if (nrow(diagnosticResults) > 0) {
    diagnosticResults$database_id <- databaseId

    # Reorder and rename columns to match specification
    # specification uses outcome_cohort_id, target_cohort_id, diagnostic_name, diagnostic_value, pass
    # ensure we don't have analysisId vs analysis_id confusion
    if (!"analysis_id" %in% colnames(diagnosticResults) && "analysisId" %in% colnames(diagnosticResults)) {
      diagnosticResults$analysis_id <- diagnosticResults$analysisId
    }

    # Ensure all required columns exist
    cols <- c("database_id", "analysis_id", "target_cohort_id", "outcome_cohort_id", "diagnostic_name", "diagnostic_value", "pass")
    for (col in cols) {
      if (!col %in% colnames(diagnosticResults)) {
        diagnosticResults[[col]] <- NA
      }
    }

    diagnosticResults <- diagnosticResults |>
      dplyr::select(dplyr::all_of(cols))

    # Compute blinding status
    blindingRows <- .computeBlindingStatus(diagnosticResults)
    if (nrow(blindingRows) > 0) {
      diagnosticResults <- rbind(diagnosticResults, blindingRows)
    }

    # Export all results at once via Manager
    resultExportManager$exportDataFrame(diagnosticResults, "scc_diagnostics_summary")

    ParallelLogger::logInfo(sprintf("Completed %d diagnostic tests", nrow(diagnosticResults)))

    failures <- diagnosticResults |>
      dplyr::filter(.data$pass == 0 & !(.data$diagnostic_name %in% c("UNBLIND", "UNBLIND_FOR_CALIBRATION")))


    if (nrow(failures) > 0) {
      ParallelLogger::logInfo(sprintf("%d diagnostic test(s) failed:", nrow(failures)))
    } else {
      ParallelLogger::logInfo("All diagnostic tests passed")
    }
  } else {
    ParallelLogger::logWarn("No diagnostic results generated")
  }

  return(invisible(diagnosticResults))
}

#' Get diagnostics summary
#'
#' @description
#' Returns a summary of diagnostic results for each target-outcome pair, including
#' blinding status.
#'
#' @param diagnosticResults  Data frame of diagnostic results as returned by runSccDiagnostics
#'
#' @return
#' A data frame with blinding status per target-outcome pair
#'
#' @export
getDiagnosticsSummary <- function(diagnosticResults) {
  if (is.null(diagnosticResults) || nrow(diagnosticResults) == 0) {
    return(data.frame())
  }

  summary <- diagnosticResults |>
    dplyr::filter(.data$diagnostic_name %in% c("UNBLIND", "UNBLIND_FOR_CALIBRATION")) |>
    tidyr::pivot_wider(
      names_from = "diagnostic_name",
      values_from = "pass",
      id_cols = c("database_id", "analysis_id", "target_cohort_id", "outcome_cohort_id")
    )

  return(summary)
}

#' Compute blinding status rows
#' @noRd
.computeBlindingStatus <- function(diagnosticResults) {
  if (nrow(diagnosticResults) == 0) {
    return(data.frame())
  }

  # Group by target-outcome pair and compute aggregate pass status
  blindingStatus <- diagnosticResults |>
    dplyr::group_by(.data$database_id, .data$analysis_id, .data$target_cohort_id, .data$outcome_cohort_id) |>
    dplyr::summarize(
      # Tier 1: unblind_for_calibration - all non-MDRR diagnostics must pass
      pass_for_calibration = as.integer(all(.data$pass[.data$diagnostic_name != "MDRR"] == 1)),
      # Tier 2: unblind - all diagnostics must pass (including MDRR)
      pass_all = as.integer(all(.data$pass == 1)),
      .groups = "drop"
    )

  # Create new rows for the summary
  unblindRows <- blindingStatus |>
    dplyr::transmute(
      .data$database_id,
      .data$analysis_id,
      .data$target_cohort_id,
      .data$outcome_cohort_id,
      diagnostic_name = "UNBLIND",
      diagnostic_value = NA_real_,
      pass = .data$pass_all
    )

  unblindCalibrationRows <- blindingStatus |>
    dplyr::transmute(
      .data$database_id,
      .data$analysis_id,
      .data$target_cohort_id,
      .data$outcome_cohort_id,
      diagnostic_name = "UNBLIND_FOR_CALIBRATION",
      diagnostic_value = NA_real_,
      pass = .data$pass_for_calibration
    )

  return(rbind(unblindRows, unblindCalibrationRows))
}

#' Compute EASE diagnostic
#' @noRd
.computeEaseDiagnostic <- function(estimates, analysisId, thresholds) {
  if (is.null(estimates) || nrow(estimates) == 0) {
    return(data.frame())
  }

  # Ensure columns expected by computeEase exist regardless of case
  # we expect rr and se_log_rr or seLogRr
  # Normalize names to CamelCase for internal logic
  if (!("true_effect_size" %in% colnames(estimates))) {
    return(data.frame())
  }

  negatives <- estimates |>
    dplyr::filter(.data$true_effect_size == 1)

  if (nrow(negatives) == 0) {
    return(data.frame())
  }


  # Map snake_case to CamelCase for common fields if they exist as snake
  nameMap <- c(
    targetCohortId = "target_cohort_id",
    outcomeCohortId = "outcome_cohort_id",
    seLogRr = "se_log_rr"
  )
  
  for (i in seq_along(nameMap)) {
    new <- names(nameMap)[i]
    old <- nameMap[i]
    if (old %in% colnames(negatives) && !(new %in% colnames(negatives))) {
      negatives[[new]] <- negatives[[old]]
    }
  }

  easeResults <- negatives |>
    dplyr::group_by(.data$target_cohort_id) |>
    dplyr::group_map(function(data, grp) {
      # Use the grouping variable from grp
      targetId <- grp[[1]] 

      # computeEase needs rr and seLogRr
      ease <- if (nrow(data) > 1) computeEase(data) else NA_real_

      pass <- if (is.na(ease)) {
        0L # Fail if EASE cannot be computed (insufficient controls)
      } else {
        as.integer(ease <= thresholds$easeMaxAcceptable)
      }

      data.frame(
        analysis_id = analysisId,
        target_cohort_id = targetId,
        outcome_cohort_id = 0, # EASE is per target
        diagnostic_name = "EASE",
        diagnostic_value = ease,
        pass = pass
      )
    }) |>
    dplyr::bind_rows()


  return(easeResults)
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
    cdmDatabaseSchema = cdmDatabaseSchema,
    tempEmulationSchema = tempEmulationSchema
  )

  if (nrow(preExpData) == 0) {
    return(data.frame())
  }

  diagnostics <- data.frame()

  for (i in seq_len(nrow(preExpData))) {
    row <- preExpData[i, ]


    # Test passes if p-value > threshold (not significantly higher before)
    pVal <- row$pValue
    pass <- if (is.na(pVal)) 1L else as.integer(pVal > thresholds$preExposurePThreshold)

    # Add rate ratio diagnostic
    diagRow1 <- data.frame(
      analysis_id = analysisId,
      target_cohort_id = row$targetCohortId,
      outcome_cohort_id = row$outcomeCohortId,
      diagnostic_name = "PRE_EXPOSURE_RATE_RATIO",
      diagnostic_value = row$preExposureRateRatio,
      pass = 1L # We primarily gate on the p-value
    )
    diagnostics <- rbind(diagnostics, diagRow1)

    # Add p-value diagnostic
    diagRow2 <- data.frame(
      analysis_id = analysisId,
      target_cohort_id = row$targetCohortId,
      outcome_cohort_id = row$outcomeCohortId,
      diagnostic_name = "PRE_EXPOSURE_P_VALUE",
      diagnostic_value = pVal,
      pass = pass
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
    SUM(CASE WHEN DATEDIFF(d, o.@outcome_start_date, op.observation_period_end_date) <= 30
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

