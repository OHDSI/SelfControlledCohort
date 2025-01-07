# @file SelfControlledCohort.R
#
# Copyright 2022 Observational Health Data Sciences and Informatics
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
#' @import DatabaseConnector
#'
"_PACKAGE"

computeIrrs <- function(estimates) {

  computeIrr <- function(numOutcomesExposed, numOutcomesUnexposed, timeAtRiskExposed, timeAtRiskUnexposed) {
    if (numOutcomesExposed == 0 & numOutcomesUnexposed == 0) {
      return(c(NA, 0, Inf))
    }
    test <- rateratio.test::rateratio.test(x = c(numOutcomesExposed,
                                                 numOutcomesUnexposed),
                                           n = c(timeAtRiskExposed,
                                                 timeAtRiskUnexposed))
    return(c(test$estimate[1], test$conf.int))
  }

  irrs <- mapply(computeIrr,
                 numOutcomesExposed = estimates$num_outcomes_exposed,
                 numOutcomesUnexposed = estimates$num_outcomes_unexposed,
                 timeAtRiskExposed = estimates$time_at_risk_exposed,
                 timeAtRiskUnexposed = estimates$time_at_risk_unexposed)

  estimates$rr <- irrs[1,]
  estimates$irr_lb_95 <- irrs[2,]
  estimates$irr_ub_95 <- irrs[3,]

  estimates$log_rr <- log(estimates$rr)
  estimates$se_log_rr <- (log(estimates$irr_ub_95) - log(estimates$irr_lb_95)) / (2 * qnorm(0.975))
  zTest <- stats::pnorm(estimates$log_rr / estimates$se_log_rr)
  estimates$p <- 2 * pmin(zTest, 1 - zTest)
  return(estimates)
}


batchComputeEstimates <- function(connection,
                                  computeThreads,
                                  resultsTable,
                                  resultExportManager,
                                  negativeControlPairs,
                                  controlType) {
  cluster <- ParallelLogger::makeCluster(computeThreads)
  ParallelLogger::clusterRequire(cluster, "rateratio.test")
  andromeda <- Andromeda::andromeda()
  # Clean up, regardless of status
  on.exit({
    ParallelLogger::stopCluster(cluster)
    Andromeda::close(andromeda)
  }, add = TRUE)

  # Writes both to CSV and andromeda object for later calibrated results
  batchComputeCallBack <- function(rows, position, cluster, andromeda) {
    if (nrow(rows) > 0) {
      batches <- ceiling(nrow(rows) / 10000)
      rows <- split(rows, rep_len(1:batches, nrow(rows)))
      rows <- ParallelLogger::clusterApply(cluster, rows, computeIrrs, progressBar = FALSE)
      rows <- do.call(rbind, rows)
    }

    if (position == 1) {
      andromeda$estimates <- rows
    } else {
      Andromeda::appendToTable(andromeda$estimates, rows)
    }

    return(rows)
  }

  # Fetch results from server:
  args <- list(cluster = cluster, andromeda = andromeda)
  resultExportManager$exportQuery(connection,
                                  "SELECT * FROM @results_table", # Query
                                  "scc_result", # output csv file
                                  results_table = resultsTable,
                                  transformFunction = batchComputeCallBack,
                                  transformFunctionArgs = args,
                                  append = FALSE)

  if (is.null(andromeda$estimates)) {
    ParallelLogger::logInfo("No effect estimates produced")
    return(NULL)
  }

  if (length(negativeControlPairs) > 0) {
    ncPairsDf <- do.call(rbind, lapply(negativeControlPairs, function(eo) {
      data.frame(targetCohortId = eo[[1]], outcomeCohortId = eo[[2]])
    }))

    if (controlType == "outcome") {
      ncPairsDf |>
        dplyr::group_by(.data$targetCohortId) |>
        dplyr::group_map(function(data, targetCohortId) {

          estimates <- andromeda$estimates |>
            dplyr::filter(.data$targetCohortId == targetCohortId)

          positives <- estimates |>
            dplyr::filter(!.data$outcomeCohortId %in% data$outcomeCohortId)

          negatives <- estimates |>
            dplyr::filter(.data$outcomeCohortId %in% data$outcomeCohortId)

          calibratedEstimates <- computeCalibratedRows(positives = positives,
                                                       negatives = negatives,
                                                       idCol = "targetCohortId")

          resultExportManager$exportDataFrame(calibratedEstimates, "scc_result", append = TRUE)
        })
    }

    if (controlType == "exposure") {
      ncPairsDf |>
        dplyr::group_by(.data$outcomeCohortId) |>
        dplyr::group_map(function(data, outcomeCohortId) {
          estimates <- andromeda$estimates |>
            dplyr::filter(.data$outcomeCohortId == outcomeCohortId)

          positives <- estimates |>
            dplyr::filter(!.data$targetCohortId %in% data$targetCohortId)

          negatives <- estimates |>
            dplyr::filter(.data$targetCohortId %in% data$targetCohortId)

          calibratedEstimates <- computeCalibratedRows(positives = positives,
                                                       negatives = negatives,
                                                       idCol = "outcomeCohortId")
          resultExportManager$exportDataFrame(calibratedEstimates, "scc_result", append = TRUE)
        })
    }


  }

  return(NULL)
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
#' @param cdmVersion                       Define the OMOP CDM version used: currently support "4" and
#'                                         "5".
#' @param oracleTempSchema                 For Oracle only: the name of the database schema where you
#'                                         want all temporary tables to be managed. Requires
#'                                         create/insert permissions to this database.
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
#' @param computeTarDistribution           If TRUE, computer the distribution of time-at-risk and
#'                                         average absolute time between treatment and outcome. Note,
#'                                         may add significant computation time on some database
#'                                         engines.
#' @param riskWindowsTable                 String: optionally store the risk windows in a (non-temporary)
#'                                         table.
#' @param resultsTable                     String: optionally store the summary results (number exposed/
#'                                         unexposed patients per outcome-exposure pair) in a (non-temporary)
#'                                         table. Note that this table does not store the rate ratios, only
#'                                         the values required to calculate rate ratios.
#' @param resultsDatabaseSchema                    Schema to oputput results to. Ignored if resultsTable and
#'                                         riskWindowsTable are temporary.
#' @param washoutPeriod                    Integer to define required time observed before exposure
#'                                         start.
#' @param followupPeriod                   Integer to define required time observed after exposure
#'                                         start.
#' @param computeThreads                   Number of parallel threads for computing IRRs with exact
#'                                         confidence intervals.
#' @param resultExportPath                 Folder where result files are exported
#' @param outputFolder                     Folder where intermediate files are stored
#' @param databaseId                       Unique identifier for database - required
#' @param resultExportManager              ResultModelManager::ResultExportManager instance - customize this to implement
#'                                         an alternative mechanism for exporting results
#'
#' @return
#' An object of type \code{sccResults} containing the results of the analysis.
#' @examples
#' \dontrun{
#' connectionDetails <- createConnectionDetails(dbms = "sql server",
#'                                              server = "RNDUSRDHIT07.jnj.com")
#' sccResult <- runSelfControlledCohort(connectionDetails,
#'                                      cdmDatabaseSchema = "cdm_truven_mdcr.dbo",
#'                                      exposureIds = c(767410, 1314924, 907879),
#'                                      outcomeIds = 444382,
#'                                      outcomeTable = "condition_era")
#' runSelfControlledCohort(connectionDetails,
#'                         cdmDatabaseSchema = "cdm_truven_mdcr.dbo",
#'                         exposureIds = c(767410, 1314924, 907879),
#'                         outcomeIds = 444382,
#'                         outcomeTable = "condition_era",
#'                         returnEstimates = FALSE)
#' }
#' @export
runSelfControlledCohort <- function(connectionDetails = NULL,
                                    cdmDatabaseSchema,
                                    connection = NULL,
                                    cdmVersion = 5,
                                    tempEmulationSchema = getOption("sqlRenderTempEmulationSchema"),
                                    oracleTempSchema = NULL,
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
                                    computeTarDistribution = FALSE,
                                    computeThreads = 1,
                                    riskWindowsTable = "#risk_windows",
                                    resultsTable = "#results",
                                    resultsDatabaseSchema = NULL,
                                    resultExportPath = "scc_result",
                                    outputFolder = "scc_work",
                                    databaseId,
                                    analysisId = 1,
                                    resultExportManager = ResultModelManager::createResultExportManager(
                                      tableSpecification = getResultsDataModelSpecifications(),
                                      exportDir = resultExportPath,
                                      databaseId = databaseId
                                    )) {
  if (riskWindowEndExposed < riskWindowStartExposed && !addLengthOfExposureExposed)
    stop("Risk window end (exposed) should be on or after risk window start")
  if (riskWindowEndUnexposed < riskWindowStartUnexposed && !addLengthOfExposureUnexposed)
    stop("Risk window end (unexposed) should be on or after risk window start")
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
    if (cdmVersion == "4") {
      outcomeId <- "cohort_concept_id"
    } else {
      outcomeId <- "cohort_definition_id"
    }
    outcomePersonId <- "subject_id"
  }

  checkmate::assertR6(resultExportManager, "ResultExportManager")

  checkmate::assertList(negativeControlPairs, null.ok = TRUE)
  checkmate::assertChoice(controlType, choices = c("outcome", "exposure"))

  if (!dir.exists(outputFolder)) {
    dir.create(outputFolder)
  }

  if (resultsTable != "#results") {
    if (is.null(resultsDatabaseSchema))
      stop("Results table is not temporary and resultsDatabaseSchema is not set")

    resultsTable <- SqlRender::render("@results_database_schema.@results_table",
                                      results_database_schema = resultsDatabaseSchema,
                                      results_table = resultsTable)
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

  if (!is.null(outcomeIds)) {
    DatabaseConnector::insertTable(connection = connection,
                                   tableName = "#scc_outcome_ids",
                                   data = data.frame(outcome_id = outcomeIds),
                                   tempTable = TRUE)
  }

  runSccRiskWindows(connection = connection,
                    cdmDatabaseSchema = cdmDatabaseSchema,
                    cdmVersion = cdmVersion,
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
                    resultsDatabaseSchema = resultsDatabaseSchema)

  if (riskWindowsTable != "#risk_windows") {
    riskWindowsTable <- SqlRender::render("@results_database_schema.@risk_windows_table",
                                          results_database_schema = resultsDatabaseSchema,
                                          risk_windows_table = riskWindowsTable)

  }

  ParallelLogger::logInfo("Retrieving counts from database")
  renderedSql <- SqlRender::loadRenderTranslateSql(sqlFilename = "Scc.sql",
                                                   packageName = "SelfControlledCohort",
                                                   dbms = connection@dbms,
                                                   tempEmulationSchema = tempEmulationSchema,
                                                   outcome_ids = outcomeIds,
                                                   outcome_database_schema = outcomeDatabaseSchema,
                                                   outcome_table = outcomeTable,
                                                   outcome_start_date = outcomeStartDate,
                                                   outcome_id = outcomeId,
                                                   analysis_id = analysisId,
                                                   outcome_person_id = outcomePersonId,
                                                   first_outcome_only = firstOutcomeOnly,
                                                   risk_windows_table = riskWindowsTable,
                                                   results_table = resultsTable)
  DatabaseConnector::executeSql(connection, renderedSql)

  if (computeTarDistribution) {
    .getSccRiskWindowStats(connection,
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
                           resultExportManager)
  }

  ParallelLogger::logInfo("Computing incidence rate ratios and exact confidence intervals")

  batchComputeEstimates(connection = connection,
                        computeThreads = computeThreads,
                        resultsTable = resultsTable,
                        resultExportManager = resultExportManager,
                        negativeControlPairs = negativeControlPairs,
                        controlType = controlType)
  # Drop temp tables:
  ParallelLogger::logInfo("Cleaning up intermedate tables")
  sql <- SqlRender::loadRenderTranslateSql(sqlFilename = "CleanupTables.sql",
                                           packageName = "SelfControlledCohort",
                                           dbms = connection@dbms,
                                           tempEmulationSchema = tempEmulationSchema,
                                           outcome_ids = outcomeIds,
                                           exposure_ids = exposureIds,
                                           results_table = resultsTable)
  DatabaseConnector::executeSql(connection, sql)

  resultExportManager$writeManifest(packageName = utils::packageName(),
                                    packageVersion = packageVersion(utils::packageName()))

  delta <- Sys.time() - start
  ParallelLogger::logInfo(paste("Performing SCC analysis took", signif(delta, 3), attr(delta, "units")))

  return(invisible())
}

