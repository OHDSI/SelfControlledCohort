# @file Analyses.R
#
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


#' Execute function for strategus
#'
#'
execute <- function(connectionDetails,
                    executionSettings,
                    dashboard = NULL,
                    analysisSettings = getSccAnalysisList(),
                    computeThreads = getOption("strategus.SelfControlledCohort.computeThreads",
                                               defaultValue = parallel::detectCores() - 1),
                    exposureCohortIds = getExposureCohortIds(),
                    outcomeCohortIds = getOutcomeCohortIds(),
                    negativeControls = NULL,
                    controlType = "outcome") {
  cli::cli_alert_info("Running scc on {executionSettings$databaseId}")

  if (length(negativeControls) == 0) {
    cli::cli_alert_warning("No negative controls found. Results will not be calibrated")
  }

  cohortTableNames <- CohortGenerator::getCohortTableNames(executionSettings$cohortTable)

  negativeControlsList <- purrr::pmap(negativeControls, function(exposureId, outcomeId, ...) {
    list(exposureId, outcomeId)
  })

  tableSpace <- dashboard$config$databaseSchema
  if (is.null(tableSpace))
    tableSpace <- "all_by_all"

  resultsPath <- file.path("exec", "results", executionSettings$databaseId, tableSpace, "scc_result")
  cli::cli_alert_info("Starting scc execution")
  for (refRow in analysisSettings) {
    getrunSelfControlledCohortArgs <- refRow$runSelfControlledCohortArgs
    resultsExportPath <- file.path(resultsPath, paste0("A_", refRow$analysisId))

    if (file.exists(file.path(resultsExportPath, paste0("manifest.json")))) {
      cli::cli_alert_info("Results manifest found in {resultsExportPath} skipping analysis")
      next
    }

    args <- list(connectionDetails = connectionDetails,
                 cdmDatabaseSchema = executionSettings$cdmDatabaseSchema,
                 exposureDatabaseSchema = executionSettings$workDatabaseSchema,
                 resultsDatabaseSchema = executionSettings$workDatabaseSchema,
                 exposureTable = cohortTableNames$cohortTable,
                 outcomeDatabaseSchema = executionSettings$workDatabaseSchema,
                 outcomeTable = cohortTableNames$cohortTable,
                 exposureIds = exposureCohortIds,
                 outcomeIds = outcomeCohortIds,
                 databaseId = executionSettings$databaseId,
                 controlType = controlType,
                 negativeControlPairs = negativeControlsList,
                 # riskWindowsTable =  "reward_scc_risk_windows",
                 # resultsTable = "reward_scc_results",
                 analysisDescription = refRow$description,
                 analysisId = refRow$analysisId,
                 tempEmulationSchema = executionSettings$tempEmulationSchema
                 resultExportPath = resultsExportPath,
                 computeThreads = computeThreads)

    args <- append(args, getrunSelfControlledCohortArgs)
    do.call(runSelfControlledCohort, args)
  }
  cli::cli_alert_success("Scc analysis complete for {executionSettings$databaseId}")
}