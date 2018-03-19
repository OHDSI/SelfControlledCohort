# @file RunAnalyses.R
#
# Copyright 2018 Observational Health Data Sciences and Informatics
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

#' Run a list of analyses
#'
#' @details
#' Run a list of analyses for the drug-comparator-outcomes of interest. This function will run all
#' specified analyses against all hypotheses of interest, meaning that the total number of outcome
#' models is `length(cmAnalysisList) * length(drugComparatorOutcomesList)`.
#'
#' @param connectionDetails        An R object of type \code{connectionDetails} created using the
#'                                 function \code{createConnectionDetails} in the
#'                                 \code{DatabaseConnector} package.
#' @param cdmDatabaseSchema        The name of the database schema that contains the OMOP CDM instance.
#'                                 Requires read permissions to this database. On SQL Server, this
#'                                 should specifiy both the database and the schema, so for example
#'                                 'cdm_instance.dbo'.
#' @param oracleTempSchema         For Oracle only: the name of the database schema where you want all
#'                                 temporary tables to be managed. Requires create/insert permissions
#'                                 to this database.
#' @param exposureDatabaseSchema   The name of the database schema that is the location where the
#'                                 exposure data used to define the exposure cohorts is available. If
#'                                 exposureTable = DRUG_ERA, exposureDatabaseSchema is not used by
#'                                 assumed to be cdmSchema.  Requires read permissions to this
#'                                 database.
#' @param exposureTable            The tablename that contains the exposure cohorts.  If exposureTable
#'                                 <> DRUG_ERA, then expectation is exposureTable has format of COHORT
#'                                 table: COHORT_DEFINITION_ID, SUBJECT_ID, COHORT_START_DATE,
#'                                 COHORT_END_DATE.
#' @param outcomeDatabaseSchema    The name of the database schema that is the location where the data
#'                                 used to define the outcome cohorts is available. If exposureTable =
#'                                 CONDITION_ERA, exposureDatabaseSchema is not used by assumed to be
#'                                 cdmSchema.  Requires read permissions to this database.
#' @param outcomeTable             The tablename that contains the outcome cohorts.  If outcomeTable <>
#'                                 CONDITION_OCCURRENCE, then expectation is outcomeTable has format of
#'                                 COHORT table: COHORT_DEFINITION_ID, SUBJECT_ID, COHORT_START_DATE,
#'                                 COHORT_END_DATE.
#' @param outputFolder             Name of the folder where all the outputs will written to.
#' @param sccAnalysisList          A list of objects of type \code{sccAnalysis} as created using the
#'                                 \code{\link{createSccAnalysis}} function.
#' @param exposureOutcomeList      A list of objects of type \code{exposureOutcome} as created using
#'                                 the \code{\link{createExposureOutcome}} function.
#' @param cdmVersion               Define the OMOP CDM version used: currently support "4" and "5".
#' @param analysisThreads          The number of parallel threads to use to execute the analyses.
#' @param computeThreads           Number of parallel threads per analysis thread for computing IRRs with exact
#'                                 confidence intervals.
#'
#' @export
runSccAnalyses <- function(connectionDetails,
                           cdmDatabaseSchema,
                           oracleTempSchema = cdmDatabaseSchema,
                           exposureDatabaseSchema = cdmDatabaseSchema,
                           exposureTable = "drug_era",
                           outcomeDatabaseSchema = cdmDatabaseSchema,
                           outcomeTable = "condition_occurrence",
                           cdmVersion = 4,
                           outputFolder = "./SelfControlledCohortOutput",
                           sccAnalysisList,
                           exposureOutcomeList,
                           analysisThreads = 1,
                           computeThreads = 1) {
  for (exposureOutcome in exposureOutcomeList) {
    stopifnot(class(exposureOutcome) == "exposureOutcome")
  }
  for (sccAnalysis in sccAnalysisList) {
    stopifnot(class(sccAnalysis) == "sccAnalysis")
  }
  uniqueOutcomeList <- unique(OhdsiRTools::selectFromList(exposureOutcomeList, "outcomeId"))
  uniqueAnalysisIds <- unlist(unique(OhdsiRTools::selectFromList(sccAnalysisList, "analysisId")))
  if (length(uniqueAnalysisIds) != length(sccAnalysisList)) {
    stop("Duplicate analysis IDs are not allowed")
  }
  if (!file.exists(outputFolder))
    dir.create(outputFolder)

  ### Create reference table ###
  resultsReference <- data.frame()
  for (sccAnalysis in sccAnalysisList) {
    for (outcome in uniqueOutcomeList) {
      outcomeId <- .selectByType(sccAnalysis$outcomeType, outcome$outcomeId, "outcome")
      exposures <- OhdsiRTools::matchInList(exposureOutcomeList, outcome)
      sccResultsFile <- .createSccResultsFileName(outputFolder,
                                                  analysisId = sccAnalysis$analysisId,
                                                  outcomeId = outcomeId)
      for (exposure in exposures) {
        exposureId <- .selectByType(sccAnalysis$exposureType, exposure$exposureId, "exposure")
        resultsReferenceRow <- data.frame(analysisId = sccAnalysis$analysisId,
                                          exposureId = exposureId,
                                          outcomeId = outcomeId,
                                          sccResultsFile = sccResultsFile,
                                          stringsAsFactors = FALSE)
        resultsReference <- rbind(resultsReference, resultsReferenceRow)
      }
    }
  }
  saveRDS(resultsReference, file.path(outputFolder, "resultsReference.rds"))

  writeLines("*** Running multiple analysis ***")
  objectsToCreate <- list()
  for (sccResultsFile in unique(resultsReference$sccResultsFile)) {
    if (!file.exists((sccResultsFile))) {
      refRow <- resultsReference[resultsReference$sccResultsFile == sccResultsFile, ][1, ]
      analysisRow <- OhdsiRTools::matchInList(sccAnalysisList,
                                              list(analysisId = refRow$analysisId))[[1]]
      getrunSelfControlledCohortArgs <- analysisRow$runSelfControlledCohortArgs

      exposureIds <- unique(resultsReference$exposureId[resultsReference$sccResultsFile == sccResultsFile])
      outcomeId <- unique(resultsReference$outcomeId[resultsReference$sccResultsFile == sccResultsFile])

      args <- list(connectionDetails = connectionDetails,
                   cdmDatabaseSchema = cdmDatabaseSchema,
                   exposureDatabaseSchema = exposureDatabaseSchema,
                   exposureTable = exposureTable,
                   outcomeDatabaseSchema = outcomeDatabaseSchema,
                   outcomeTable = outcomeTable,
                   cdmVersion = cdmVersion,
                   exposureIds = exposureIds,
                   outcomeIds = outcomeId,
                   computeThreads = computeThreads)
      args <- append(args, getrunSelfControlledCohortArgs)
      objectsToCreate[[length(objectsToCreate) + 1]] <- list(args = args,
                                                             sccResultsFile = sccResultsFile)
    }
  }
  createSccResultsObject <- function(params) {
    sccResults <- do.call("runSelfControlledCohort", params$args)
    saveRDS(sccResults, params$sccResultsFile)
  }
  if (length(objectsToCreate) != 0) {
    cluster <- OhdsiRTools::makeCluster(analysisThreads)
    OhdsiRTools::clusterRequire(cluster, "SelfControlledCohort")
    dummy <- OhdsiRTools::clusterApply(cluster, objectsToCreate, createSccResultsObject)
    OhdsiRTools::stopCluster(cluster)
  }

  invisible(resultsReference)
}

.createSccResultsFileName <- function(folder, analysisId, outcomeId) {
  name <- paste("SccResults_a", analysisId, "_o", outcomeId, ".rds", sep = "")
  return(file.path(folder, name))
}

.selectByType <- function(type, value, label) {
  if (is.null(type)) {
    if (is.list(value)) {
      stop(paste("Multiple ",
                 label,
                 "s specified, but none selected in analyses (comparatorType).",
                 sep = ""))
    }
    return(value)
  } else {
    if (!is.list(value) || is.null(value[type])) {
      stop(paste(label, "type not found:", type))
    }
    return(value[type])
  }
}

#' Create a summary report of the analyses
#'
#' @param resultsReference   A data.frame as created by the \code{\link{runSccAnalyses}} function.
#'
#' @export
summarizeAnalyses <- function(resultsReference) {
  result <- data.frame()
  for (sccResultsFile in unique(resultsReference$sccResultsFile)) {
    sccResults <- readRDS(sccResultsFile)$estimates
    analysisId <- resultsReference$analysisId[resultsReference$sccResultsFile == sccResultsFile][1]
    sccResults$analysisId <- analysisId
    result <- rbind(result, sccResults)
  }
  return(result)
}
