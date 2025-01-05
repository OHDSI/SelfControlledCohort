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
                                   analysisId,
                                   firstOutcomeOnly,
                                   riskWindowsTable,
                                   exportManager) {
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
                                                   analysis_id = analysisId,
                                                   first_outcome_only = firstOutcomeOnly,
                                                   risk_windows_table = riskWindowsTable)
  DatabaseConnector::executeSql(connection, renderedSql)
  exportManager$exportQuery(connection,
                            "SELECT * FROM #tx_distribution",
                            "scc_stat",
                            append = FALSE)

  DatabaseConnector::renderTranslateExecuteSql(connection,
                                               "TRUNCATE TABLE #tx_distribution; DROP TABLE #tx_distribution;")

  exportManager$exportQuery(connection,
                            "SELECT * FROM #time_to_dist",
                            "scc_stat",
                            append = TRUE)
  DatabaseConnector::renderTranslateExecuteSql(connection,
                                               "TRUNCATE TABLE #time_to_dist; DROP TABLE #time_to_dist;")

  exportManager$exportQuery(connection,
                            "SELECT * FROM #time_to_dist_exposed",
                            "scc_stat",
                            append = FALSE)

  DatabaseConnector::renderTranslateExecuteSql(connection,
                                               "TRUNCATE TABLE #time_to_dist_exposed; DROP TABLE #time_to_dist_exposed;")

  exportManager$exportQuery(connection,
                            "SELECT * FROM #time_to_dist_unex",
                            "scc_stat",
                            append = TRUE)

  DatabaseConnector::renderTranslateExecuteSql(connection,
                                               "TRUNCATE TABLE #time_to_dist_unex; DROP TABLE #time_to_dist_unex;")


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
                                  riskWindowsTable = "#risk_windows",
                                  resultExportPath = "scc_result",
                                  analysisId = 1,
                                  databaseId = NULL,
                                  resultExportManager = ResultModelManager::createResultExportManager(
                                    tableSpecification = getResultsDataModelSpecifications(),
                                    exportDir = resultExportPath,
                                    databaseId = databaseId
                                  )) {

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
                         analysisId,
                         firstOutcomeOnly,
                         riskWindowsTable,
                         resultExportManager)
}