# @file Analyses.R
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


#' Get module information
#'
#' @return A list with module metadata
#'
#' @examples
#' \donttest{
#' moduleInfo <- getModuleInfo()
#' }
#' @export
getModuleInfo <- function() {
  desc <- utils::packageDescription("SelfControlledCohort")

  return(list(
    name = desc$Package,
    version = desc$Version,
    description = desc$Title,
    author = desc$Author,
    maintainer = desc$Maintainer,
    date = desc$Date
  ))
}

#' Create Self-Controlled Cohort Module Specifications
#'
#' @description
#' Creates a specifications object for the Self-Controlled Cohort module
#' to be used within the OHDSI Strategus framework.
#'
#' @param analysisSettings A list of analysis settings containing the analysis
#'   configuration and parameters for self-controlled cohort analyses.
#' @param exposureOutcomeList A list of objects of type \code{exposureOutcome} as created using
#'   the \code{\link{createExposureOutcome}} function. Each object defines an exposure-outcome
#'   pair with exposureId, outcomeId, and optionally trueEffectSize (set to 1 for negative controls).
#' @param computeThreads Integer specifying the number of threads to use for
#'   parallel computation. Default is the number of available cores minus 1.
#'
#' @return A list object of class `SelfControlledCohortModuleSpecifications` and
#'   `ModuleSpecifications` containing the module name, version, repository
#'   information, and analysis settings.
#'
#' @examples
#' \donttest{
#' eo1 <- createExposureOutcome(exposureId = 1124300, outcomeId = 444382)
#' analysis1 <- createSccAnalysis(analysisId = 1,
#'                               description = "Main",
#'                               runSelfControlledCohortArgs = createRunSelfControlledCohortArgs())
#'
#' moduleSpec <- createSelfControlledCohortModuleSpecifications(
#'   analysisSettings = list(analysis1),
#'   exposureOutcomeList = list(eo1)
#' )
#' }
#' @export
createSelfControlledCohortModuleSpecifications <- function(
  analysisSettings,
  exposureOutcomeList,
  computeThreads = parallel::detectCores() - 1
) {
  # Validate exposureOutcomeList
  stopifnot(is.list(exposureOutcomeList))
  stopifnot(length(exposureOutcomeList) > 0)
  for (i in seq_along(exposureOutcomeList)) {
    stopifnot(class(exposureOutcomeList[[i]]) == "exposureOutcome")
  }

  stopifnot(is.list(analysisSettings))
  stopifnot(length(analysisSettings) > 0)

  moduleInfo <- getModuleInfo()

  specifications <- list(
    module = "SelfControlledCohortModule",
    parameters = list(
      version = moduleInfo$version, # Duplicated here because strategus hides the full context!!
      analysisSettings = analysisSettings,
      exposureOutcomeList = exposureOutcomeList,
      computeThreads = computeThreads
    )
  )
  return(specifications)
}

#' Execute Self-Controlled Cohort Analyses for Strategus
#'
#' @description
#' Executes Self-Controlled Cohort analyses within the OHDSI Strategus framework using the provided analysis specifications and settings.
#'
#' @param connectionDetails An object containing the details required to connect to the database.
#' @param executionSettings A list of settings for execution, including database schemas and cohort table names.
#' @param analysisSpecifications A list containing analysis settings, exposure-outcome pairs, and compute thread count.
#' @param databaseId A string identifier for the target database.
#' @param exportFolder Path to the folder where results and exports will be written.
#'
#' @details
#' This function validates the input specifications, extracts unique exposure and outcome cohort IDs, identifies negative control pairs, and iterates over each analysis setting to run the Self-Controlled Cohort analysis. Results are exported to the specified folder. Diagnostic settings are handled per-analysis, and warnings are issued if no negative controls are found.
#'
#' @return
#' No return value. Results are written to the specified export folder as a side effect.
#'
#' @examples
#' if (interactive()) {
#' eo1 <- createExposureOutcome(exposureId = 1124300, outcomeId = 444382)
#' analysis1 <- createSccAnalysis(analysisId = 1,
#'                               description = "Main",
#'                               runSelfControlledCohortArgs = createRunSelfControlledCohortArgs())
#'
#' moduleSpec <- createSelfControlledCohortModuleSpecifications(
#'   analysisSettings = list(analysis1),
#'   exposureOutcomeList = list(eo1)
#' )
#' execute(
#'   connectionDetails = connectionDetails,
#'   executionSettings = list(
#'     databaseSchema = "main",
#'     cohortTable = "cohort",
#'     cdmDatabaseSchema = "main"
#'   ),
#'   analysisSpecifications = moduleSpec,
#'   databaseId = "MyDatabase",
#'   exportFolder = tempdir()
#' )
#' }
#' @export
execute <- function(connectionDetails, executionSettings, analysisSpecifications, databaseId, exportFolder) {
  # Version check
  checkModuleVersion(analysisSpecifications$version)

  # Extract module-specific settings
  analysisSettings <- analysisSpecifications$analysisSettings
  exposureOutcomeList <- analysisSpecifications$exposureOutcomeList
  computeThreads <- analysisSpecifications$computeThreads

  # Validate exposureOutcomeList
  stopifnot(is.list(exposureOutcomeList))
  stopifnot(length(exposureOutcomeList) > 0)
  for (i in seq_along(exposureOutcomeList)) {
    stopifnot(class(exposureOutcomeList[[i]]) == "exposureOutcome")
  }

  # Extract unique exposure and outcome IDs from exposureOutcomeList
  exposureCohortIds <- unique(unlist(lapply(exposureOutcomeList, function(eo) {
    if (is.list(eo$exposureId)) {
      return(unlist(eo$exposureId))
    } else {
      return(eo$exposureId)
    }
  })))

  outcomeCohortIds <- unique(unlist(lapply(exposureOutcomeList, function(eo) {
    if (is.list(eo$outcomeId)) {
      return(unlist(eo$outcomeId))
    } else {
      return(eo$outcomeId)
    }
  })))

  # Extract negative control pairs from exposureOutcomeList
  negativeControlsList <- list()
  for (exposureOutcome in exposureOutcomeList) {
    if (isTRUE(exposureOutcome$trueEffectSize == 1)) {
      exposureId <- if (is.list(exposureOutcome$exposureId)) exposureOutcome$exposureId[[1]] else exposureOutcome$exposureId
      outcomeId <- if (is.list(exposureOutcome$outcomeId)) exposureOutcome$outcomeId[[1]] else exposureOutcome$outcomeId
      negativeControlsList[[length(negativeControlsList) + 1]] <- c(exposureId, outcomeId)
    }
  }


  cli::cli_alert_info("Running scc on {databaseId}")

  if (length(negativeControlsList) == 0) {
    cli::cli_alert_warning("No negative controls found. Results will not be calibrated")
  }

  cohortTableNames <- executionSettings$cohortTableNames

  dir.create(exportFolder, recursive = TRUE, showWarnings = FALSE)

  cli::cli_alert_info("Starting scc execution")
  for (refRow in analysisSettings) {
    getrunSelfControlledCohortArgs <- refRow$runSelfControlledCohortArgs
    resultsExportPath <- file.path(exportFolder, paste0("A_", refRow$analysisId))

    if (file.exists(file.path(resultsExportPath, "manifest.json"))) {
      cli::cli_alert_info("Results manifest found in {resultsExportPath} skipping analysis")
      next
    }

    # Extract analysis-specific diagnostic settings with defaults
    controlType <- if (!is.null(refRow$controlType)) refRow$controlType else "outcome"
    runDiagnostics <- if (!is.null(refRow$runDiagnostics)) refRow$runDiagnostics else TRUE
    diagnostics <- if (!is.null(refRow$diagnostics)) refRow$diagnostics else c("all")
    diagnosticThresholds <- if (!is.null(refRow$diagnosticThresholds)) refRow$diagnosticThresholds else getDefaultDiagnosticThresholds()

    args <- list(
      connectionDetails = connectionDetails,
      cdmDatabaseSchema = executionSettings$cdmDatabaseSchema,
      exposureDatabaseSchema = executionSettings$workDatabaseSchema,
      resultsDatabaseSchema = executionSettings$workDatabaseSchema,
      exposureTable = cohortTableNames$cohortTable,
      outcomeDatabaseSchema = executionSettings$workDatabaseSchema,
      outcomeTable = cohortTableNames$cohortTable,
      exposureIds = exposureCohortIds,
      outcomeIds = outcomeCohortIds,
      databaseId = databaseId,
      controlType = controlType,
      negativeControlPairs = negativeControlsList,
      analysisDescription = refRow$description,
      analysisId = refRow$analysisId,
      tempEmulationSchema = executionSettings$tempEmulationSchema,
      resultExportPath = resultsExportPath,
      computeThreads = computeThreads,
      runDiagnostics = runDiagnostics,
      diagnostics = diagnostics,
      diagnosticThresholds = diagnosticThresholds
    )

    args <- append(args, getrunSelfControlledCohortArgs)
    do.call(runSelfControlledCohort, args)
  }
  cli::cli_alert_success("Scc analysis complete for {executionSettings$databaseId}")
}

#' Get results folders for an analysis specification
#' @param analysisSpecification An analysis specification object containing analysis settings and exposure-outcome pairs.
#' @param exportFolder The base folder where results are exported. Individual analysis results will be in subfolders named A_analysisId.
#' @return A character vector of paths to results folders for each analysis setting.
#'
#' @examples
#' \donttest{
#' resultsFolders <- getResultsFolders(analysisSpec, tempdir())
#' }
#' @export
getResultsFolders <- function(analysisSpecification, exportFolder) {
  exportPaths <- lapply(analysisSpecification, function(refRow) {
    resultsExportPath <- file.path(exportFolder, paste0("A_", refRow$analysisId))
    return(resultsExportPath)
  })
  return(unlist(exportPaths))
}

#' Check module version compatibility
#'
#' @param moduleVersion Character string of the module version from specifications.
#'
#' @return NULL (invisibly). Stops execution if incompatible, warns if older version.
#' @keywords internal
checkModuleVersion <- function(moduleVersion) {
  currentVersion <- getModuleInfo()$version

  # Parse versions
  current <- package_version(currentVersion)
  module <- package_version(moduleVersion)

  # Check if module version is newer than current package
  if (module > current) {
    stop(sprintf(
      "Module specifications version (%s) is newer than installed package version (%s). Please update the SelfControlledCohort package.",
      moduleVersion,
      currentVersion
    ))
  }

  # Check if using older specifications with v2+ package
  if (current >= "2.0.0" && module < current) {
    warning(sprintf(
      "Module specifications version (%s) is older than installed package version (%s). This may work but is not recommended. Consider regenerating module specifications.",
      moduleVersion,
      currentVersion
    ))
  }

  invisible(NULL)
}
