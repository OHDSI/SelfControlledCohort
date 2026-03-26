# @file SelfControlledCohort.R
#
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

#' @keywords internal
#' @aliases
#' NULL SelfControlledCohort-package
#'
#' @importFrom stats qnorm
#' @importFrom rlang .data
#' @import DatabaseConnector
#'
"_PACKAGE"

computeIrrs <- function(estimates) {
  computeIrr <- function(numOutcomesExposed, numOutcomesUnexposed, timeAtRiskExposed, timeAtRiskUnexposed) {
    if (numOutcomesExposed == 0 && numOutcomesUnexposed == 0) {
      return(c(NA, 0, Inf))
    }
    test <- rateratio.test::rateratio.test(
      x = abs(c(
        numOutcomesExposed,
        numOutcomesUnexposed
      )),
      n = abs(c(
        timeAtRiskExposed,
        timeAtRiskUnexposed
      ))
    )
    return(c(test$estimate[1], test$conf.int))
  }

  irrs <- mapply(computeIrr,
    numOutcomesExposed = estimates$num_outcomes_exposed,
    numOutcomesUnexposed = estimates$num_outcomes_unexposed,
    timeAtRiskExposed = estimates$time_at_risk_exposed,
    timeAtRiskUnexposed = estimates$time_at_risk_unexposed
  )

  estimates$rr <- irrs[1, ]
  estimates$lb_95 <- irrs[2, ]
  estimates$ub_95 <- irrs[3, ]

  estimates$log_rr <- log(estimates$rr)
  estimates$se_log_rr <- (log(estimates$ub_95) - log(estimates$lb_95)) / (2 * qnorm(0.975))
  zTest <- stats::pnorm(estimates$log_rr / estimates$se_log_rr)
  estimates$p_value <- 2 * pmin(zTest, 1 - zTest)
  return(estimates)
}


batchComputeEstimates <- function(connection,
                                  analysisId,
                                  computeThreads,
                                  resultsTable,
                                  resultExportManager,
                                  negativeControlPairs,
                                  controlType,
                                  tempEmulationSchema,
                                  databaseId = NULL) {
  cluster <- ParallelLogger::makeCluster(computeThreads)
  ParallelLogger::clusterRequire(cluster, "rateratio.test")
  andromeda <- Andromeda::andromeda()
  # Clean up, regardless of status
  on.exit(
    {
      ParallelLogger::stopCluster(cluster)
    },
    add = TRUE
  )

  # Writes to andromeda object for later calibrated results and diagnostics
  batchComputeCallBack <- function(rows, position, cluster, andromeda) {
    if (nrow(rows) > 0) {
      batches <- ceiling(nrow(rows) / 10000)
      rows <- split(rows, rep_len(1:batches, nrow(rows)))
      rows <- ParallelLogger::clusterApply(cluster, rows, computeIrrs, progressBar = FALSE)
      rows <- do.call(rbind, rows)
      rows$analysis_id <- analysisId
      
      # Mark negative controls if possible
      # We'll do this later on the whole object for simplicity or here
      
      if (position == 1) {
        andromeda$estimates <- rows
      } else {
        Andromeda::appendToTable(andromeda$estimates, rows)
      }
    }
    NULL
  }

  # Fetch results from server:
  args <- list(cluster = cluster, andromeda = andromeda)
  rsql <- " SELECT * FROM @results_table
            WHERE time_at_risk_exposed > 0
            AND time_at_risk_unexposed > 0"
  DatabaseConnector::renderTranslateQueryApplyBatched(connection,
    rsql,
    results_table = resultsTable,
    fun = batchComputeCallBack,
    tempEmulationSchema = tempEmulationSchema,
    args = args
  )

  if (is.null(andromeda$estimates) || andromeda$estimates |>
    dplyr::count() |>
    dplyr::pull() == 0) {
    ParallelLogger::logInfo("No effect estimates produced")
    Andromeda::close(andromeda)
    return(NULL)
  }

  # Add true_effect_size to estimates for EASE/calibration
  if (length(negativeControlPairs) > 0) {
    ncPairsDf <- do.call(rbind, lapply(negativeControlPairs, function(eo) {
      data.frame(target_cohort_id = as.numeric(eo[[1]]), outcome_cohort_id = as.numeric(eo[[2]]), true_effect_size = 1)
    })) |>
      dplyr::distinct()
    
    # Merge true_effect_size into andromeda table
    andromeda$nc_pairs <- ncPairsDf
    
    andromeda$estimates <- andromeda$estimates |>
      dplyr::mutate(target_cohort_id = as.numeric(.data$target_cohort_id),
                    outcome_cohort_id = as.numeric(.data$outcome_cohort_id)) |>
      dplyr::left_join(andromeda$nc_pairs |> 
                         dplyr::mutate(target_cohort_id = as.numeric(.data$target_cohort_id),
                                       outcome_cohort_id = as.numeric(.data$outcome_cohort_id)), 
                       by = c("target_cohort_id", "outcome_cohort_id"))
  }

  return(andromeda)
}



#' Get Default export manager
#' @description
#' Returns the default export manager class for writing csv file results
#' @inheritParams runSelfControlledCohort
#' @export
getDefaultExportManager <- function(resultExportPath, databaseId) {
  ResultModelManager::createResultExportManager(
    tableSpecification = getResultsDataModelSpecifications(),
    exportDir = resultExportPath,
    databaseId = databaseId
  )
}


#' Export Final Results
#' @noRd
.exportFinalResults <- function(andromeda,
                               resultExportManager,
                               negativeControlPairs,
                               controlType,
                               diagnosticResults,
                               diagnosticThresholds,
                               analysisId) {
  first <- TRUE
  
  if (length(negativeControlPairs) > 0) {
    # Ensure scc_outcome_exposure is exported
    ncPairsDf <- andromeda$nc_pairs |> dplyr::collect()
    resultExportManager$exportDataFrame(ncPairsDf, "scc_outcome_exposure")

    processControlType <- function(groupByCol, filterCol, dataCol) {
      filterColSnake <- SqlRender::camelCaseToSnakeCase(filterCol)
      dataColSnake <- SqlRender::camelCaseToSnakeCase(dataCol)
      groupByColSnake <- SqlRender::camelCaseToSnakeCase(groupByCol)

      ncPairsDf |>
        dplyr::select(-"true_effect_size") |>
        dplyr::group_by(.data[[groupByColSnake]]) |>
        dplyr::group_map(function(nc_data, grp) {
          grpCol <- grp[[groupByColSnake]]

          estimates <- andromeda$estimates |>
            dplyr::filter(.data[[filterColSnake]] == grpCol) |>
            dplyr::collect()

          if (nrow(estimates) > 0) {
            # Identify negatives
            negatives <- estimates |>
              dplyr::filter(!is.na(.data$true_effect_size))

            # Identify outcome-exposure pairs for metadata
            outcomeExposurePairs <- estimates |>
              dplyr::select("target_cohort_id", "outcome_cohort_id") |>
              dplyr::mutate(true_effect_size = NA) |>
              dplyr::distinct()

            resultExportManager$exportDataFrame(outcomeExposurePairs, "scc_outcome_exposure")

            # Filter negatives based on diagnostics if available
            if (!is.null(diagnosticResults)) {
              blindingSummary <- getDiagnosticsSummary(diagnosticResults)
              if (nrow(blindingSummary) > 0) {
                # Keep only negatives that are unblind_for_calibration
                negatives <- negatives |>
                  dplyr::inner_join(
                    blindingSummary |>
                      dplyr::filter(.data$UNBLIND_FOR_CALIBRATION == 1) |>
                      dplyr::select("target_cohort_id", "outcome_cohort_id"),
                    by = c("target_cohort_id", "outcome_cohort_id")
                  )
              }
            }

            # Prepare for calibration
            colnames(estimates) <- SqlRender::snakeCaseToCamelCase(colnames(estimates))
            colnames(negatives) <- SqlRender::snakeCaseToCamelCase(colnames(negatives))

            if (nrow(negatives) > 0) {
              calibratedEstimates <- computeCalibratedRows(
                positives = estimates,
                negatives = negatives
              )
            } else {
              calibratedEstimates <- estimates
              cols <- c("calibratedRr", "calibratedSeLogRr", "calibratedLb95", "calibratedUb95", "calibratedPValue")
              for (name in cols) {
                calibratedEstimates[[name]] <- NA_real_
              }
            }

            colnames(calibratedEstimates) <- SqlRender::camelCaseToSnakeCase(colnames(calibratedEstimates))
            calibratedEstimates$i2 <- NA
            calibratedEstimates$analysis_id <- analysisId
            
            resultExportManager$exportDataFrame(calibratedEstimates, "scc_result")
          }
        })
    }


    if (controlType == "outcome") {
      processControlType(groupByCol = "targetCohortId", filterCol = "targetCohortId", dataCol = "outcomeCohortId")
    } else {
      processControlType(groupByCol = "outcomeCohortId", filterCol = "outcomeCohortId", dataCol = "targetCohortId")
    }
  } else {
    # No controls, just export raw results
    if ("estimates" %in% names(andromeda)) {
      writeBatch <- function(batch) {
        cols <- c("calibrated_rr", "calibrated_se_log_rr", "calibrated_lb_95", "calibrated_ub_95", "calibrated_p_value")
        for (name in cols) {
          batch[[name]] <- NA_real_
        }
        batch$i2 <- NA_real_
        
        outcomeExposurePairs <- batch |>
          dplyr::select("target_cohort_id", "outcome_cohort_id") |>
          dplyr::mutate(true_effect_size = NA) |>
          dplyr::distinct()

        resultExportManager$exportDataFrame(batch, "scc_result")
        resultExportManager$exportDataFrame(outcomeExposurePairs, "scc_outcome_exposure")
        return(invisible(NULL))
      }
      Andromeda::batchApply(andromeda$estimates, writeBatch)
    }
  }
}



#' @title
#' Run self-controlled cohort
#'
#' @description
#' \code{runSelfControlledCohort} generates population-level estimation by comparing exposed and
#' unexposed time among exposed cohort.
#'
#' @details
#' Population-level estimation method that estimates incidence rate comparison of exposed/unexposed
#' time within an exposed cohort.
#' If multiple exposureIds and outcomeIds are provided, estimates will be generated for every
#' combination of exposure and outcome.
#'
#' @references
#' Ryan PB, Schuemie MJ, Madigan D.Empirical performance of a self-controlled cohort method: lessons
#' for developing a risk identification and analysis system. Drug Safety 36 Suppl1:S95-106, 2013
#' @param connectionDetails                An R object of type \code{connectionDetails} created using
#'                                         the function \code{createConnectionDetails} in the
#'                                         \code{DatabaseConnector} package.
#' @param connection                       DatabaseConnector connection instance
#' @param cdmDatabaseSchema                Name of database schema that contains the OMOP CDM and
#'                                         vocabulary.
#' @param tempEmulationSchema              Some database platforms like Oracle and Impala do not truly support temp tables. To emulate temp
#'                                         tables, provide a schema with write privileges where temp tables can be created.

#' @param exposureIds                      A vector containing the drug_concept_ids or
#'                                         cohort_definition_ids of the exposures of interest. If empty,
#'                                         all exposures in the exposure table will be included.
#' @param outcomeIds                       The condition_concept_ids or cohort_definition_ids of the
#'                                         outcomes of interest. If empty, all the outcomes in the
#'                                         outcome table will be included.
#'
#' @param negativeControlPairs             A list of vectors for pairs of negative control
#' @param controlType                      Calibrate effect estimates with outcome (default) or exposure controls
#'
#' @param exposureDatabaseSchema           The name of the database schema that is the location where
#'                                         the exposure data used to define the exposure cohorts is
#'                                         available. If exposureTable = DRUG_ERA,
#'                                         exposureDatabaseSchema is not used by assumed to be
#'                                         cdmSchema.  Requires read permissions to this database.
#' @param exposureTable                    The tablename that contains the exposure cohorts.  If
#'                                         exposureTable <> DRUG_ERA, then expectation is exposureTable
#'                                         has format of COHORT table: cohort_concept_id, SUBJECT_ID,
#'                                         COHORT_START_DATE, COHORT_END_DATE.
#' @param outcomeDatabaseSchema            The name of the database schema that is the location where
#'                                         the data used to define the outcome cohorts is available. If
#'                                         exposureTable = CONDITION_ERA, exposureDatabaseSchema is not
#'                                         used by assumed to be cdmSchema.  Requires read permissions
#'                                         to this database.
#' @param outcomeTable                     The tablename that contains the outcome cohorts.  If
#'                                         outcomeTable <> CONDITION_OCCURRENCE, then expectation is
#'                                         outcomeTable has format of COHORT table:
#'                                         COHORT_DEFINITION_ID, SUBJECT_ID, COHORT_START_DATE,
#'                                         COHORT_END_DATE.
#' @param firstExposureOnly                If TRUE, only use first occurrence of each drug concept id
#'                                         for each person
#' @param firstOutcomeOnly                 If TRUE, only use first occurrence of each condition concept
#'                                         id for each person.
#' @param minAge                           Integer for minimum allowable age.
#' @param maxAge                           Integer for maximum allowable age.
#' @param studyStartDate                   Date for minimum allowable data for index exposure. Date
#'                                         format is 'yyyymmdd'.
#' @param studyEndDate                     Date for maximum allowable data for index exposure. Date
#'                                         format is 'yyyymmdd'.
#' @param addLengthOfExposureExposed       If TRUE, use the duration from drugEraStart -> drugEraEnd as
#'                                         part of timeAtRisk.
#' @param riskWindowStartExposed           Integer of days to add to drugEraStart for start of
#'                                         timeAtRisk (0 to include index date, 1 to start the day
#'                                         after).
#' @param riskWindowEndExposed             Additional window to add to end of exposure period (if
#'                                         addLengthOfExposureExposed = TRUE, then add to exposure end
#'                                         date, else add to exposure start date).
#' @param addLengthOfExposureUnexposed     If TRUE, use the duration from exposure start -> exposure
#'                                         end as part of timeAtRisk looking back before exposure
#'                                         start.
#' @param riskWindowEndUnexposed           Integer of days to add to exposure start for end of
#'                                         timeAtRisk (0 to include index date, -1 to end the day
#'                                         before).
#' @param riskWindowStartUnexposed         Additional window to add to start of exposure period (if
#'                                         addLengthOfExposureUnexposed = TRUE, then add to exposure
#'                                         end date, else add to exposure start date).
#' @param hasFullTimeAtRisk                If TRUE, restrict to people who have full time-at-risk
#'                                         exposed and unexposed.
#' @param riskWindowsTable                 String: optionally store the risk windows in a (non-temporary)
#'                                         table.
#' @param resultsTable                     String: optionally store the summary results (number exposed/
#'                                         unexposed patients per outcome-exposure pair) in a (non-temporary)
#'                                         table. Note that this table does not store the rate ratios, only
#'                                         the values required to calculate rate ratios.
#' @param keepResultsTables                Keep the results tables in place if they exist. This allows the data set
#'                                         to be added to with aditional targets and outcomes.
#'                                         (ignored if temporary tables are used, default)
#' @param extractResults                   Export results to disk. In the case of very large exposure/outcome set
#'                                         pairs it is often more ideal to create a permanent results table with
#'                                         the @resultsTable parameter and extract and calibrate small subsets on
#'                                         demand. Performing calibration across the full set of outcome/exposure pairs
#'                                         can take a significant amount of time.
#'                                         If set to false, no relative risk ratios will be produced.
#' @param resultsDatabaseSchema            Schema to oputput results to. Ignored if resultsTable and
#'                                         riskWindowsTable are temporary.
#' @param washoutPeriod                    Integer to define required time observed before exposure
#'                                         start.
#' @param followupPeriod                   Integer to define required time observed after exposure
#'                                         start.
#' @param computeThreads                   Number of parallel threads for computing IRRs with exact
#'                                         confidence intervals.
#' @param resultExportPath                 Folder where result files are exported
#' @param databaseId                       Unique identifier for database - required
#' @param resultExportManager              ResultModelManager::ResultExportManager instance - customize this to implement
#'                                         an alternative mechanism for exporting results
#' @param analysisId                       An integer unique to this analysis
#' @param analysisDescription              A string description of the analysis (optional)
#' @param runDiagnostics                   If TRUE, run diagnostic tests on the results
#' @param diagnostics                      Character vector specifying which diagnostics to run.
#'                                         Options: "all", "counts", "event_dependent", "pre_exposure",
#'                                         "window_balance", "cohort_stability". Default is "all".
#' @param diagnosticThresholds             Named list of diagnostic thresholds. See getDefaultDiagnosticThresholds()
#'
#' @return
#' An object of type \code{sccResults} containing the results of the analysis.
#' @examples
#' \dontrun{
#' connectionDetails <- createConnectionDetails(
#'   dbms = "sql server",
#'   server = "RNDUSRDHIT07.jnj.com"
#' )
#' sccResult <- runSelfControlledCohort(connectionDetails,
#'   cdmDatabaseSchema = "cdm_truven_mdcr.dbo",
#'   exposureIds = c(767410, 1314924, 907879),
#'   outcomeIds = 444382,
#'   outcomeTable = "condition_era"
#' )
#' runSelfControlledCohort(connectionDetails,
#'   cdmDatabaseSchema = "cdm_truven_mdcr.dbo",
#'   exposureIds = c(767410, 1314924, 907879),
#'   outcomeIds = 444382,
#'   outcomeTable = "condition_era",
#'   returnEstimates = FALSE
#' )
#' }
#' @export
runSelfControlledCohort <- function(connectionDetails = NULL,
                                    cdmDatabaseSchema,
                                    connection = NULL,
                                    tempEmulationSchema = getOption("sqlRenderTempEmulationSchema"),
                                    exposureIds = NULL,
                                    outcomeIds = NULL,
                                    negativeControlPairs = NULL,
                                    controlType = "outcome",
                                    exposureDatabaseSchema = cdmDatabaseSchema,
                                    exposureTable = "drug_era",
                                    outcomeDatabaseSchema = cdmDatabaseSchema,
                                    outcomeTable = "condition_era",
                                    firstExposureOnly = TRUE,
                                    firstOutcomeOnly = TRUE,
                                    minAge = "",
                                    maxAge = "",
                                    studyStartDate = "",
                                    studyEndDate = "",
                                    addLengthOfExposureExposed = TRUE,
                                    riskWindowStartExposed = 1,
                                    riskWindowEndExposed = 30,
                                    addLengthOfExposureUnexposed = TRUE,
                                    riskWindowEndUnexposed = -1,
                                    riskWindowStartUnexposed = -30,
                                    hasFullTimeAtRisk = FALSE,
                                    washoutPeriod = 0,
                                    followupPeriod = 0,
                                    computeThreads = getOption("strategus.SelfControlledCohort.computeThreads",
                                      default = parallel::detectCores() - 1
                                    ),
                                    riskWindowsTable = "#risk_windows",
                                    resultsTable = "#results",
                                    keepResultsTables = TRUE,
                                    extractResults = TRUE,
                                    resultsDatabaseSchema = NULL,
                                    resultExportPath = "scc_result",
                                    databaseId,
                                    analysisId = 1,
                                    analysisDescription = paste("SCC analysis", analysisId),
                                    resultExportManager = getDefaultExportManager(resultExportPath, databaseId),
                                    runDiagnostics = TRUE,
                                    diagnostics = c("all"),
                                    diagnosticThresholds = getDefaultDiagnosticThresholds()) {
  if (riskWindowEndExposed < riskWindowStartExposed && !addLengthOfExposureExposed) {
    stop("Risk window end (exposed) should be on or after risk window start")
  }
  if (riskWindowEndUnexposed < riskWindowStartUnexposed && !addLengthOfExposureUnexposed) {
    stop("Risk window end (unexposed) should be on or after risk window start")
  }
  start <- Sys.time()

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

  checkmate::assertR6(resultExportManager, "ResultExportManager")

  checkmate::assertList(negativeControlPairs, null.ok = TRUE)
  checkmate::assertChoice(controlType, choices = c("outcome", "exposure"))

  if (resultsTable != "#results") {
    if (is.null(resultsDatabaseSchema)) {
      stop("Results table is not temporary and resultsDatabaseSchema is not set")
    }

    resultsTable <- SqlRender::render("@results_database_schema.@results_table",
      results_database_schema = resultsDatabaseSchema,
      results_table = resultsTable
    )
  }

  # Check if connection already open:
  if (is.null(connection)) {
    if (is.null(connectionDetails)) {
      stop("Connection details not set")
    }
    connection <- DatabaseConnector::connect(connectionDetails)
    on.exit(DatabaseConnector::disconnect(connection))
  } else if (!DatabaseConnector::dbIsValid(connection)) {
    stop("Invalid connection object")
  }

  # Merge negative control outcome IDs into the outcome filter so their
  # estimates are included in the results table (needed for calibration).
  allOutcomeIds <- outcomeIds
  if (!is.null(negativeControlPairs) && length(negativeControlPairs) > 0 && !is.null(outcomeIds)) {
    ncOutcomeIds <- if (controlType == "outcome") {
      unique(vapply(negativeControlPairs, function(p) p[[2]], numeric(1)))
    } else {
      unique(vapply(negativeControlPairs, function(p) p[[1]], numeric(1)))
    }
    allOutcomeIds <- unique(c(outcomeIds, ncOutcomeIds))
  }

  if (!is.null(allOutcomeIds)) {
    DatabaseConnector::insertTable(
      connection = connection,
      tableName = "#scc_outcome_ids",
      data = data.frame(outcome_id = allOutcomeIds),
      tempTable = TRUE
    )
  }

  settingsString <- list(
    firstExposureOnly = firstExposureOnly,
    firstOutcomeOnly = firstOutcomeOnly,
    minAge = minAge,
    maxAge = maxAge,
    studyStartDate = studyStartDate,
    studyEndDate = studyEndDate,
    addLengthOfExposureExposed = addLengthOfExposureExposed,
    riskWindowStartExposed = riskWindowStartExposed,
    riskWindowEndExposed = riskWindowEndExposed,
    addLengthOfExposureUnexposed = addLengthOfExposureUnexposed,
    riskWindowEndUnexposed = riskWindowEndUnexposed,
    riskWindowStartUnexposed = riskWindowStartUnexposed,
    hasFullTimeAtRisk = hasFullTimeAtRisk,
    washoutPeriod = washoutPeriod,
    followupPeriod = followupPeriod
  ) |>
    ParallelLogger::convertSettingsToJson() |>
    as.character()

  sccAnalysisSetting <- data.frame(
    analysis_id = analysisId,
    description = analysisDescription,
    settings = settingsString
  )

  resultExportManager$exportDataFrame(sccAnalysisSetting, "scc_analysis_setting", append = FALSE)

  runSccRiskWindows(
    connection = connection,
    cdmDatabaseSchema = cdmDatabaseSchema,
    tempEmulationSchema = tempEmulationSchema,
    exposureIds = exposureIds,
    exposureDatabaseSchema = exposureDatabaseSchema,
    exposureTable = exposureTable,
    firstExposureOnly = firstExposureOnly,
    minAge = minAge,
    maxAge = maxAge,
    studyStartDate = studyStartDate,
    studyEndDate = studyEndDate,
    addLengthOfExposureExposed = addLengthOfExposureExposed,
    riskWindowStartExposed = riskWindowStartExposed,
    riskWindowEndExposed = riskWindowEndExposed,
    addLengthOfExposureUnexposed = addLengthOfExposureUnexposed,
    riskWindowEndUnexposed = riskWindowEndUnexposed,
    riskWindowStartUnexposed = riskWindowStartUnexposed,
    hasFullTimeAtRisk = hasFullTimeAtRisk,
    washoutPeriod = washoutPeriod,
    followupPeriod = followupPeriod,
    riskWindowsTable = riskWindowsTable,
    keepResultsTables = keepResultsTables,
    resultsDatabaseSchema = resultsDatabaseSchema
  )
  if (riskWindowsTable != "#risk_windows") {
    riskWindowsTable <- SqlRender::render("@results_database_schema.@risk_windows_table",
      results_database_schema = resultsDatabaseSchema,
      risk_windows_table = riskWindowsTable
    )
  } else {
    keepResultsTables <- FALSE
  }

  ParallelLogger::logInfo("Retrieving counts from database")
  renderedSql <- SqlRender::loadRenderTranslateSql(
    sqlFilename = "Scc.sql",
    packageName = "SelfControlledCohort",
    dbms = DatabaseConnector::dbms(connection),
    tempEmulationSchema = tempEmulationSchema,
    outcome_ids = if (is.null(allOutcomeIds)) "" else allOutcomeIds,
    outcome_database_schema = outcomeDatabaseSchema,
    outcome_table = outcomeTable,
    outcome_start_date = outcomeStartDate,
    outcome_id = outcomeId,
    analysis_id = analysisId,
    drop_results_table = !keepResultsTables,
    outcome_person_id = outcomePersonId,
    first_outcome_only = firstOutcomeOnly,
    risk_windows_table = riskWindowsTable,
    results_table = resultsTable
  )
  DatabaseConnector::executeSql(connection, renderedSql)

  if (extractResults) {
    # 1. Compute raw effect estimates (IRR/SE)
    ParallelLogger::logInfo("Computing raw effect estimates")
    andromeda <- batchComputeEstimates(
      connection = connection,
      analysisId = analysisId,
      computeThreads = computeThreads,
      resultsTable = resultsTable,
      resultExportManager = resultExportManager,
      negativeControlPairs = negativeControlPairs,
      controlType = controlType,
      tempEmulationSchema = tempEmulationSchema,
      databaseId = databaseId
    )

    if (!is.null(andromeda)) {
      on.exit(Andromeda::close(andromeda), add = TRUE)

      # 2. Run diagnostics (including EASE now that we have estimates)
      diagnosticResults <- NULL
      if (runDiagnostics) {
        ParallelLogger::logInfo("Running diagnostics")
        # Extract estimates from andromeda for diagnostics (usually small enough for memory)
        estimatesDf <- andromeda$estimates |> dplyr::collect()
        
        diagnosticResults <- runSccDiagnostics(
          connection = connection,
          cdmDatabaseSchema = cdmDatabaseSchema,
          tempEmulationSchema = tempEmulationSchema,
          resultsTable = resultsTable,
          riskWindowsTable = riskWindowsTable,
          outcomeTable = outcomeTable,
          outcomeDatabaseSchema = outcomeDatabaseSchema,
          analysisId = analysisId,
          databaseId = databaseId,
          estimates = estimatesDf,
          diagnostics = diagnostics,
          thresholds = diagnosticThresholds,
          resultExportManager = resultExportManager
        )
      }

      # 3. Export metadata and final results
      .getSccRiskWindowStats(
        connection,
        tempEmulationSchema,
        outcomeIds,
        outcomeDatabaseSchema,
        outcomeTable,
        outcomeStartDate,
        outcomeId,
        outcomePersonId,
        analysisId,
        firstOutcomeOnly,
        riskWindowsTable,
        resultExportManager
      )

      .exportFinalResults(
        andromeda = andromeda,
        resultExportManager = resultExportManager,
        negativeControlPairs = negativeControlPairs,
        controlType = controlType,
        diagnosticResults = diagnosticResults,
        diagnosticThresholds = diagnosticThresholds,
        analysisId = analysisId
      )
    }

    resultExportManager$writeManifest(
      packageName = utils::packageName(),
      packageVersion = utils::packageVersion(utils::packageName())
    )
  }





  # Drop temp tables:
  ParallelLogger::logInfo("Cleaning up intermedate tables")
  sql <- SqlRender::loadRenderTranslateSql(
    sqlFilename = "CleanupTables.sql",
    packageName = "SelfControlledCohort",
    dbms = connection@dbms,
    tempEmulationSchema = tempEmulationSchema,
    outcome_ids = outcomeIds,
    exposure_ids = exposureIds,
    risk_windows_table = riskWindowsTable,
    drop_results_table = !keepResultsTables,
    results_table = resultsTable
  )
  DatabaseConnector::executeSql(connection, sql)
  delta <- Sys.time() - start
  ParallelLogger::logInfo(paste("Performing SCC analysis took", signif(delta, 3), attr(delta, "units")))

  return(invisible())
}
