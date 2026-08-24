# Copyright 2026 Observational Health Data Sciences and Informatics
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
#'   \item maxEventDependentCensoring - Maximum proportion censored within 30 days of outcome (default: 0.25)
#'   \item minEventsPerWindow - Minimum events required in each window (default: 3)
#'   \item easeMaxAcceptable - Maximum acceptable EASE (default: 0.25). Requires negative controls.
#' }
#'
#' @examples
#' \donttest{
#' if (interactive()) {
#' # Get default thresholds
#'    thresholds <- getDefaultDiagnosticThresholds()
#'
#'    # Modify specific thresholds
#'    customThresholds <- getDefaultDiagnosticThresholds()
#'    customThresholds$mdrrMaxAcceptable <- 5.0
#' }
#' }
#' @export
getDefaultDiagnosticThresholds <- function() {
  list(
    mdrrMaxAcceptable = 10.0, # Max MDRR for adequate power
    maxPreExposureProportion = 0.05, # Max 5% with pre-exposure outcomes
    preExposurePThreshold = 0.05, # Significance level for pre-exposure test
    maxEventDependentCensoring = 0.25, # Max 25% censored within 30 days of outcome
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
#' @param computeThreads              Number of parallel threads to use for CPU-bound diagnostic
#'                                    computations (e.g., MDRR and EASE). Default is the number of
#'                                    available cores minus 1.
#' @param resultExportManager         ResultModelManager::ResultExportManager instance
#'
#' @return
#' Invisible data frame of diagnostic results
#'
#' @examples
#' \donttest{
#' if (interactive()) {
#'   connectionDetails <- Eunomia::getEunomiaConnectionDetails()
#'   connection <- DatabaseConnector::connect(connectionDetails)
#'
#'   diagnostics <- runSccDiagnostics(
#'     connection = connection,
#'     cdmDatabaseSchema = "main",
#'     resultsTable = "#scc_results",
#'     riskWindowsTable = "#risk_windows",
#'     analysisId = 1,
#'     databaseId = "Eunomia",
#'     estimates = resultsData,
#'     diagnostics = "all",
#'     thresholds = getDefaultDiagnosticThresholds()
#'   )
#'
#'   DatabaseConnector::disconnect(connection)
#' }
#' }
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
                              computeThreads = getOption("strategus.SelfControlledCohort.computeThreads",
                                default = parallel::detectCores() - 1
                              ),
                              resultExportManager) {
  if (!DatabaseConnector::dbIsValid(connection)) {
    stop("Invalid connection object")
  }

  checkmate::assertR6(resultExportManager, "ResultExportManager")

  # Ensure all thresholds are present by merging with defaults
  defaultThresholds <- getDefaultDiagnosticThresholds()
  for (name in names(defaultThresholds)) {
    if (is.null(thresholds[[name]])) {
      thresholds[[name]] <- defaultThresholds[[name]]
    }
  }

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
      tempEmulationSchema = tempEmulationSchema,
      computeThreads = computeThreads
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
      tempEmulationSchema = tempEmulationSchema,
      resultsTable = resultsTable
    )
    diagnosticResults <- rbind(diagnosticResults, eventDepResult)
  }

  # Run EASE diagnostic (systematic error)
  if ("ease" %in% diagnostics && !is.null(estimates)) {
    ParallelLogger::logInfo("- Running EASE (systematic error) diagnostic")
    easeResult <- .computeEaseDiagnostic(
      estimates = estimates,
      analysisId = analysisId,
      thresholds = thresholds,
      computeThreads = computeThreads
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
#' @examples
#' \donttest{
#' if (interactive()) {
#' connectionDetails <- Eunomia::getEunomiaConnectionDetails()
#' connection <- DatabaseConnector::connect(connectionDetails)
#'
#' diagnostics <- runSccDiagnostics(
#'   connection = connection,
#'   cdmDatabaseSchema = "main",
#'   resultsTable = "#scc_results",
#'   riskWindowsTable = "#risk_windows",
#'   analysisId = 1,
#'   databaseId = "Eunomia",
#'   estimates = resultsData,
#'   diagnostics = "all",
#'   thresholds = getDefaultDiagnosticThresholds()
#' )
#'
#' DatabaseConnector::disconnect(connection)
#' summary <- getDiagnosticsSummary(diagnostics)
#' }
#' }
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
.computeEaseDiagnostic <- function(estimates, analysisId, thresholds, computeThreads = 1) {
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

  targetGroups <- split(negatives, negatives$target_cohort_id)

  if (computeThreads > 1 && length(targetGroups) > 1) {
    cluster <- ParallelLogger::makeCluster(min(computeThreads, length(targetGroups)))
    ParallelLogger::clusterRequire(cluster, "SelfControlledCohort")
    on.exit(ParallelLogger::stopCluster(cluster), add = TRUE)

    easeResults <- ParallelLogger::clusterApply(
      cluster,
      targetGroups,
      .computeEaseForTarget,
      analysisId = analysisId,
      thresholds = thresholds,
      progressBar = FALSE
    )
    easeResults <- dplyr::bind_rows(easeResults)
  } else {
    easeResults <- dplyr::bind_rows(lapply(
      targetGroups,
      .computeEaseForTarget,
      analysisId = analysisId,
      thresholds = thresholds
    ))
  }

  return(easeResults)
}

#' Compute EASE for a single target cohort
#' @noRd
.computeEaseForTarget <- function(data, analysisId, thresholds) {
  targetId <- data$target_cohort_id[1]

  # computeEase needs rr and seLogRr. Reference it via the package namespace so the
  # function resolves correctly on ParallelLogger cluster workers.
  ease <- if (nrow(data) > 1) SelfControlledCohort::computeEase(data) else NA_real_

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
}



#' Compute MDRR (power) diagnostic
#' @noRd
.computeMdrrDiagnostic <- function(connection,
                                   resultsTable,
                                   analysisId,
                                   thresholds,
                                   tempEmulationSchema,
                                   computeThreads = 1) {
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

  cluster <- ParallelLogger::makeCluster(computeThreads)
  ParallelLogger::clusterRequire(cluster, "SelfControlledCohort")
  on.exit(ParallelLogger::stopCluster(cluster), add = TRUE)

  computeMdrrCallBack <- function(rows, position, cluster, analysisId, thresholds) {
    if (nrow(rows) == 0) {
      return(NULL)
    }
    batches <- ceiling(nrow(rows) / 10000)
    rows <- split(rows, rep_len(seq_len(batches), nrow(rows)))
    rows <- ParallelLogger::clusterApply(
      cluster,
      rows,
      .computeMdrrForRows,
      analysisId = analysisId,
      thresholds = thresholds,
      progressBar = FALSE
    )
    dplyr::bind_rows(rows)
  }

  # Stream the results table in batches to avoid materializing the full
  # target-outcome grid in memory.
  batchResults <- DatabaseConnector::renderTranslateQueryApplyBatched(
    connection = connection,
    sql = sql,
    fun = computeMdrrCallBack,
    args = list(cluster = cluster, analysisId = analysisId, thresholds = thresholds),
    results_table = resultsTable,
    analysis_id = analysisId,
    tempEmulationSchema = tempEmulationSchema,
    snakeCaseToCamelCase = TRUE
  )

  diagnostics <- dplyr::bind_rows(batchResults)

  if (is.null(diagnostics) || nrow(diagnostics) == 0) {
    return(data.frame())
  }

  return(diagnostics)
}

#' Compute MDRR rows for a chunk of results
#' @noRd
.computeMdrrForRows <- function(rows, analysisId, thresholds) {
  # Reference via the package namespace so the function resolves on cluster workers.
  mdrr <- vapply(seq_len(nrow(rows)), function(i) {
    SelfControlledCohort::computeMdrrForRateRatio(
      exposedPersonTime = rows$timeAtRiskExposed[i],
      unexposedPersonTime = rows$timeAtRiskUnexposed[i],
      exposedEvents = rows$numOutcomesExposed[i],
      unexposedEvents = rows$numOutcomesUnexposed[i],
      alpha = 0.05,
      power = 0.80
    )
  }, numeric(1))

  # Check if MDRR is acceptable
  pass <- ifelse(is.na(mdrr), 0L, as.integer(mdrr <= thresholds$mdrrMaxAcceptable))

  data.frame(
    analysis_id = analysisId,
    target_cohort_id = rows$targetCohortId,
    outcome_cohort_id = rows$outcomeCohortId,
    diagnostic_name = "MDRR",
    diagnostic_value = mdrr,
    pass = pass
  )
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
                                             tempEmulationSchema,
                                             resultsTable = NULL) {
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

  # Restrict to the outcomes of interest when possible, so the database does not
  # scan the entire outcome table for concept ids the study never requested.
  outcomeRestriction <- ""
  if (!is.null(resultsTable)) {
    outcomeRestriction <- "
  INNER JOIN (
    SELECT DISTINCT outcome_cohort_id
    FROM @results_table
    WHERE analysis_id = @analysis_id
  ) roi
    ON o.@outcome_id = roi.outcome_cohort_id"
  }

  sql <- sprintf("
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
    AND o.@outcome_start_date <= rw.risk_window_end_exposed%s
  INNER JOIN @cdm_database_schema.observation_period op
    ON rw.person_id = op.person_id
    AND o.@outcome_start_date >= op.observation_period_start_date
    AND o.@outcome_start_date <= op.observation_period_end_date
  WHERE rw.analysis_id = @analysis_id
  GROUP BY rw.exposure_id, o.@outcome_id
  ", outcomeRestriction)

  args <- list(
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
  if (!is.null(resultsTable)) {
    args$results_table <- resultsTable
  }

  results <- do.call(DatabaseConnector::renderTranslateQuerySql, args)

  if (nrow(results) == 0) {
    return(data.frame())
  }

  proportion <- results$censoredWithin30Days / results$totalPersonsWithOutcome

  diagnostics <- data.frame(
    analysis_id = analysisId,
    target_cohort_id = results$targetCohortId,
    outcome_cohort_id = results$outcomeCohortId,
    diagnostic_name = "EVENT_DEPENDENT_OBSERVATION",
    diagnostic_value = proportion,
    pass = as.integer(proportion <= thresholds$maxEventDependentCensoring)
  )

  return(diagnostics)
}

