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
                 numOutcomesExposed = estimates$numOutcomesExposed,
                 numOutcomesUnexposed = estimates$numOutcomesUnexposed,
                 timeAtRiskExposed = estimates$timeAtRiskExposed,
                 timeAtRiskUnexposed = estimates$timeAtRiskUnexposed)
  estimates$irr <- irrs[1,]
  estimates$irrLb95 <- irrs[2,]
  estimates$irrUb95 <- irrs[3,]

  estimates$logRr <- log(estimates$irr)
  estimates$seLogRr <- (log(estimates$irrUb95) - log(estimates$irrLb95)) / (2 * qnorm(0.975))
  zTest <- stats::pnorm(estimates$logRr / estimates$seLogRr)
  estimates$p <- 2 * pmin(zTest, 1 - zTest)
  return(estimates)
}

#' @title
#' Run Self-Controlled Cohort Risk Windows
#' @description
#' Compute time at risk exposed and time at risk unexposed for risk window parameters.
#' See `getSccRiskWindowStats` for example usage.
#'
#' @inheritParams runSelfControlledCohort
#' @export
runSccRiskWindows <- function(connection,
                              cdmDatabaseSchema,
                              cdmVersion = 5,
                              tempEmulationSchema = getOption("sqlRenderTempEmulationSchema"),
                              oracleTempSchema = NULL,
                              exposureIds = NULL,
                              exposureDatabaseSchema = cdmDatabaseSchema,
                              exposureTable = "drug_era",
                              firstExposureOnly = TRUE,
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
                              riskWindowsTable = "#risk_windows",
                              resultsDatabaseSchema = NULL) {

  if (!DatabaseConnector::dbIsValid(connection))
    stop("Invalid connection object")

  exposureTable <- tolower(exposureTable)

  if (exposureTable == "drug_era") {
    exposureStartDate <- "drug_era_start_date"
    exposureEndDate <- "drug_era_end_date"
    exposureId <- "drug_concept_id"
    exposurePersonId <- "person_id"
  } else if (exposureTable == "drug_exposure") {
    exposureStartDate <- "drug_exposure_start_date"
    exposureEndDate <- "drug_exposure_end_date"
    exposureId <- "drug_concept_id"
    exposurePersonId <- "person_id"
  } else {
    exposureStartDate <- "cohort_start_date"
    exposureEndDate <- "cohort_end_date"
    if (cdmVersion == "4") {
      exposureId <- "cohort_concept_id"
    } else {
      exposureId <- "cohort_definition_id"
    }
    exposurePersonId <- "subject_id"
  }

  if (!is.null(oracleTempSchema) & is.null(tempEmulationSchema)) {
    tempEmulationSchema <- oracleTempSchema
    warning('OracleTempSchema has been deprecated by DatabaseConnector')
  }

  if (!is.null(exposureIds)) {
    DatabaseConnector::insertTable(connection = connection,
                                   tableName = "#scc_exposure_ids",
                                   data = data.frame(exposure_id = exposureIds),
                                   tempTable = TRUE)
  }

  if (riskWindowsTable != "#risk_windows") {
    if (is.null(resultsDatabaseSchema))
      stop("Risk windows table is not temporary and resultsDatabaseSchema is not set")
    riskWindowsTable <- SqlRender::render("@results_database_schema.@risk_windows_table",
                                          results_database_schema = resultsDatabaseSchema,
                                          risk_windows_table = riskWindowsTable)
  }

  renderedSql <- SqlRender::loadRenderTranslateSql(sqlFilename = "ComputeSccRiskWindows.sql",
                                                   packageName = "SelfControlledCohort",
                                                   dbms = connection@dbms,
                                                   tempEmulationSchema = tempEmulationSchema,
                                                   cdm_database_schema = cdmDatabaseSchema,
                                                   exposure_ids = exposureIds,
                                                   exposure_database_schema = exposureDatabaseSchema,
                                                   exposure_table = exposureTable,
                                                   exposure_start_date = exposureStartDate,
                                                   exposure_end_date = exposureEndDate,
                                                   exposure_id = exposureId,
                                                   exposure_person_id = exposurePersonId,
                                                   first_exposure_only = firstExposureOnly,
                                                   min_age = minAge,
                                                   max_age = maxAge,
                                                   study_start_date = studyStartDate,
                                                   study_end_date = studyEndDate,
                                                   add_length_of_exposure_exposed = addLengthOfExposureExposed,
                                                   risk_window_start_exposed = riskWindowStartExposed,
                                                   risk_window_end_exposed = riskWindowEndExposed,
                                                   add_length_of_exposure_unexposed = addLengthOfExposureUnexposed,
                                                   risk_window_end_unexposed = riskWindowEndUnexposed,
                                                   risk_window_start_unexposed = riskWindowStartUnexposed,
                                                   has_full_time_at_risk = hasFullTimeAtRisk,
                                                   washout_window = washoutPeriod,
                                                   followup_window = followupPeriod,
                                                   risk_windows_table = riskWindowsTable)

  ParallelLogger::logInfo("Computing time at risk exposed and unexposed windows")
  DatabaseConnector::executeSql(connection, renderedSql)
}

.getSccRiskWindowStats <- function(connection,
                                   tempEmulationSchema,
                                   outcomeIds,
                                   outcomeDatabaseSchema,
                                   outcomeTable,
                                   outcomeStartDate,
                                   outcomeId,
                                   outcomePersonId,
                                   firstOutcomeOnly,
                                   riskWindowsTable) {
  ParallelLogger::logInfo("Computing time at risk distribution statistics")
  renderedSql <- SqlRender::loadRenderTranslateSql(sqlFilename = "SccRiskWindowStats.sql",
                                                   packageName = "SelfControlledCohort",
                                                   dbms = connection@dbms,
                                                   tempEmulationSchema = tempEmulationSchema,
                                                   outcome_ids = outcomeIds,
                                                   outcome_database_schema = outcomeDatabaseSchema,
                                                   outcome_table = outcomeTable,
                                                   outcome_start_date = outcomeStartDate,
                                                   outcome_id = outcomeId,
                                                   outcome_person_id = outcomePersonId,
                                                   first_outcome_only = firstOutcomeOnly,
                                                   risk_windows_table = riskWindowsTable)
  DatabaseConnector::executeSql(connection, renderedSql)

  tarStats <- list()
  tarStats$treatmentTimeDistribution <- DatabaseConnector::renderTranslateQuerySql(connection,
                                                                                   "SELECT * FROM #tx_distribution",
                                                                                   snakeCaseToCamelCase = TRUE)
  DatabaseConnector::renderTranslateExecuteSql(connection, "TRUNCATE TABLE #tx_distribution; DROP TABLE #tx_distribution;")


  tarStats$timeToOutcomeDistribution <- DatabaseConnector::renderTranslateQuerySql(connection,
                                                                                   "SELECT * FROM #time_to_dist",
                                                                                   snakeCaseToCamelCase = TRUE)
  DatabaseConnector::renderTranslateExecuteSql(connection, "TRUNCATE TABLE #time_to_dist; DROP TABLE #time_to_dist;")

  tarStats$timeToOutcomeDistributionExposed <- DatabaseConnector::renderTranslateQuerySql(connection,
                                                                                          "SELECT * FROM #time_to_dist_exposed",
                                                                                          snakeCaseToCamelCase = TRUE)
  DatabaseConnector::renderTranslateExecuteSql(connection, "TRUNCATE TABLE #time_to_dist_exposed; DROP TABLE #time_to_dist_exposed;")

  tarStats$timeToOutcomeDistributionUnexposed <- DatabaseConnector::renderTranslateQuerySql(connection,
                                                                                            "SELECT * FROM #time_to_dist_unex",
                                                                                            snakeCaseToCamelCase = TRUE)
  DatabaseConnector::renderTranslateExecuteSql(connection, "TRUNCATE TABLE #time_to_dist_unex; DROP TABLE #time_to_dist_unex;")

  return(tarStats)
}

#' @title
#' Get Self-Controlled Cohort Risk Window Statistics
#' @description
#' Compute statistics from risk windows.
#' @details
#' Requires a risk window table to be created first with `runSccRiskWindows`
#' @inheritParams runSelfControlledCohort
#' @return list containing data frames:
#'          treatmentTimeDistribution,
#'          timeToOutcomeDistribution,
#'          timeToOutcomeDistributionExposed,
#'          timeToOutcomeDistributionUnexposed
#'
#' @examples
#' \dontrun{
#' # First, create the risk windows table
#' connectionDetails <- Eunomia::getEunomiaConnectionDetails()
#' connection <- DatabaseConnector::connect(connectionDetails)
#' riskWindowsTable <- "computed_risk_windows"
#' runSccRiskWindows(connection,
#'                   cdmDatabaseSchema = "main",
#'                   exposureIds = c(1102527, 1125315),
#'                   resultsDatabaseSchema = "main", # This is the schema where the results will be stored
#'                   riskWindowsTable = riskWindowsTable,
#'                   exposureTable = "drug_era")
#' # Get stats based on outcomes of interest
#' tarStats <- getSccRiskWindowStats(connection,
#'                                   outcomeDatabaseSchema = "main",
#'                                   resultsDatabaseSchema = "main",
#'                                   riskWindowsTable = riskWindowsTable,
#'                                   outcomeTable = "condition_era",
#'                                   outcomeIds = 192671)
#'}
#' @export
getSccRiskWindowStats <- function(connection,
                                  outcomeDatabaseSchema,
                                  tempEmulationSchema = getOption("sqlRenderTempEmulationSchema"),
                                  oracleTempSchema = NULL,
                                  outcomeIds = NULL,
                                  cdmVersion = 5,
                                  outcomeTable = "condition_era",
                                  firstOutcomeOnly = TRUE,
                                  resultsDatabaseSchema = NULL,
                                  riskWindowsTable = "#risk_windows") {

  if (!DatabaseConnector::dbIsValid(connection))
    stop("Invalid connection object")

  if (!is.null(oracleTempSchema) & is.null(tempEmulationSchema)) {
    tempEmulationSchema <- oracleTempSchema
    warning('OracleTempSchema has been deprecated by DatabaseConnector')
  }

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

  if (!is.null(outcomeIds)) {
    DatabaseConnector::insertTable(connection = connection,
                                   tableName = "#scc_outcome_ids",
                                   data = data.frame(outcome_id = outcomeIds),
                                   tempTable = TRUE)
  }

  if (riskWindowsTable != "#risk_windows") {
    if (is.null(resultsDatabaseSchema))
      stop("Risk windows table is not temporary and resultsDatabaseSchema is not set")
    riskWindowsTable <- SqlRender::render("@results_database_schema.@risk_windows_table",
                                          results_database_schema = resultsDatabaseSchema,
                                          risk_windows_table = riskWindowsTable)
  }

  .getSccRiskWindowStats(connection,
                         tempEmulationSchema,
                         outcomeIds,
                         outcomeDatabaseSchema,
                         outcomeTable,
                         outcomeStartDate,
                         outcomeId,
                         outcomePersonId,
                         firstOutcomeOnly,
                         riskWindowsTable)
}

batchComputeEstimates <- function(connection,
                                  computeThreads,
                                  resultsTable,
                                  tempEmulationSchema,
                                  postProcessFunction = NULL,
                                  postProcessArgs = list(),
                                  returnEstimates = TRUE) {
  cluster <- ParallelLogger::makeCluster(computeThreads)
  ParallelLogger::clusterRequire(cluster, "rateratio.test")
  # Clean up, regardless of status
  on.exit({
    ParallelLogger::stopCluster(cluster)
  }, add = TRUE)

  batchComputeCallBack <- function(data, position, cluster, postProcessFunction, postProcessArgs) {
    if (nrow(data) > 0) {
      batches <- ceiling(nrow(data) / 10000)
      data <- split(data, rep_len(1:batches, nrow(data)))
      data <- ParallelLogger::clusterApply(cluster, data, computeIrrs, progressBar = FALSE)
      data <- do.call("rbind", data)
    }

    if (is.function(postProcessFunction))
      data <- do.call(postProcessFunction, append(list(data, position), postProcessArgs))

    if (returnEstimates)
      return(data)

    return(data.frame())
  }

  # Fetch results from server:
  args <- list(cluster = cluster, postProcessFunction = postProcessFunction, postProcessArgs = postProcessArgs)
  estimates <- DatabaseConnector::renderTranslateQueryApplyBatched(connection,
                                                                   "SELECT * FROM @results_table",
                                                                   results_table = resultsTable,
                                                                   tempEmulationSchema = tempEmulationSchema,
                                                                   fun = batchComputeCallBack,
                                                                   args = args,
                                                                   snakeCaseToCamelCase = TRUE)


  if (returnEstimates) {
    return(data.frame(estimates))
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
#' @param postProcessFunction              Callback function to handle batches of data. Useful for
#'                                         massive result sets that overflow system memory. See example.
#' @param postProcessArgs                  Arguments for post processing function callback.
#' @param returnEstimates                  Boolean opt to not return estimates, only useful in the case
#'                                         where postProcessFunction is used
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
#'
#' # Using a callback function that writes data to a csv file and not store in memory
#' csvFileName <- "D:/path/to/output.csv"
#' writeSccData <- function(data, position, csvFileName) {
#'   vroom::vroom_write(data, csvFileName, delim = ",", append = position != 1, na = "")
#' }
#'
#' runSelfControlledCohort(connectionDetails,
#'                         cdmDatabaseSchema = "cdm_truven_mdcr.dbo",
#'                         exposureIds = c(767410, 1314924, 907879),
#'                         outcomeIds = 444382,
#'                         outcomeTable = "condition_era",
#'                         postProcessFunction = writeSccData,
#'                         postProcessArgs = list(csvFileName = csvFileName),
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
                                    postProcessFunction = NULL,
                                    postProcessArgs = list(),
                                    returnEstimates = TRUE) {
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

  if (!is.null(oracleTempSchema) & is.null(tempEmulationSchema)) {
    tempEmulationSchema <- oracleTempSchema
    warning('OracleTempSchema has been deprecated by DatabaseConnector')
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
                    firstExposureOnly = TRUE,
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
                                                   outcome_person_id = outcomePersonId,
                                                   first_outcome_only = firstOutcomeOnly,
                                                   risk_windows_table = riskWindowsTable,
                                                   results_table = resultsTable)
  DatabaseConnector::executeSql(connection, renderedSql)

  if (computeTarDistribution) {
    tarStats <- .getSccRiskWindowStats(connection,
                                       tempEmulationSchema,
                                       outcomeIds,
                                       outcomeDatabaseSchema,
                                       outcomeTable,
                                       outcomeStartDate,
                                       outcomeId,
                                       outcomePersonId,
                                       firstOutcomeOnly,
                                       riskWindowsTable)
  }

  ParallelLogger::logInfo("Computing incidence rate ratios and exact confidence intervals")
  estimates <- batchComputeEstimates(connection = connection,
                                     computeThreads = computeThreads,
                                     resultsTable = resultsTable,
                                     tempEmulationSchema = tempEmulationSchema,
                                     postProcessFunction = postProcessFunction,
                                     postProcessArgs = postProcessArgs,
                                     returnEstimates = returnEstimates)
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

  delta <- Sys.time() - start
  ParallelLogger::logInfo(paste("Performing SCC analysis took", signif(delta, 3), attr(delta, "units")))

  result <- list(estimates = estimates,
                 exposureIds = exposureIds,
                 outcomeIds = outcomeIds,
                 call = match.call())

  if (computeTarDistribution) {
    result$tarStats <- tarStats
  }

  class(result) <- "sccResults"
  return(result)
}

#' @export
print.sccResults <- function(x, ...) {
  writeLines("sccResults object")
  writeLines("")
  writeLines(paste("Exposure ID(s):", paste(x$exposureIds, collapse = ",")))
  writeLines(paste("Outcome ID(s):", x$outcomeIds))
}

#' @export
summary.sccResults <- function(object, ...) {
  object$estimates
}
