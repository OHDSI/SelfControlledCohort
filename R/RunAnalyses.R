# @file RunAnalyses.R
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

#' Run a list of analyses
#'
#' @details
#' Run a list of analyses for the drug-comparator-outcomes of interest. This function will run all
#' specified analyses against all hypotheses of interest, meaning that the total number of outcome
#' models is `length(cmAnalysisList) * length(drugComparatorOutcomesList)`.
#'
#' @inheritParams runSelfControlledCohort
#' @param outputFolder             Name of the folder where all the outputs will written to.
#' @param sccAnalysisList          A list of objects of type \code{sccAnalysis} as created using the
#'                                 \code{\link{createSccAnalysis}} function.
#' @param exposureOutcomeList      A list of objects of type \code{exposureOutcome} as created using
#'                                 the \code{\link{createExposureOutcome}} function.
#' @param analysisThreads          The number of parallel threads to use to execute the analyses.
#' @param controlType              Calibrate effect estimates with outcome (default) or exposure controls
#'
#' @export
runSccAnalyses <- function(connectionDetails,
                           cdmDatabaseSchema,
                           tempEmulationSchema = getOption("sqlRenderTempEmulationSchema"),
                           exposureDatabaseSchema = cdmDatabaseSchema,
                           exposureTable = "drug_era",
                           outcomeDatabaseSchema = cdmDatabaseSchema,
                           outcomeTable = "condition_occurrence",
                           cdmVersion = 5,
                           outputFolder = "./SelfControlledCohortOutput",
                           sccAnalysisList,
                           exposureOutcomeList,
                           controlType = "outcome",
                           analysisThreads = 1,
                           computeThreads = 1) {

  # Get negative controls

  checkmate::assertChoice(controlType, c("outcome", "exposure"))
  negatives <- list()
  for (exposureOutcome in exposureOutcomeList) {
    stopifnot(class(exposureOutcome) == "exposureOutcome")
    if (isTRUE(exposureOutcome$trueEffectSize == 1)) {
      negatives[[length(negatives) + 1]] <- c(exposureOutcome$exposureId, exposureOutcome$outcomeId)
    }
  }

  for (sccAnalysis in sccAnalysisList) {
    stopifnot(class(sccAnalysis) == "sccAnalysis")
  }

  uniqueOutcomeList <- unique(ParallelLogger::selectFromList(exposureOutcomeList, "outcomeId"))
  uniqueAnalysisIds <- unlist(unique(ParallelLogger::selectFromList(sccAnalysisList, "analysisId")))
  if (length(uniqueAnalysisIds) != length(sccAnalysisList)) {
    stop("Duplicate analysis IDs are not allowed")
  }
  if (!file.exists(outputFolder))
    dir.create(outputFolder)

  # If any of the results compute the TAR stats, all the analyses must do the same
  computeTarDist <- FALSE
  ### Create reference table ###
  resultsReference <- data.frame()
  for (sccAnalysis in sccAnalysisList) {

    for (outcome in uniqueOutcomeList) {
      outcomeId <- .selectByType(sccAnalysis$outcomeType, outcome$outcomeId, "outcome")
      exposures <- ParallelLogger::matchInList(exposureOutcomeList, outcome)
      sccResultsRef <- .createSccResultsRef(analysisId = sccAnalysis$analysisId)
      for (exposure in exposures) {
        exposureId <- .selectByType(sccAnalysis$exposureType, exposure$exposureId, "exposure")
        resultsReferenceRow <- data.frame(analysisId = sccAnalysis$analysisId,
                                          exposureId = exposureId,
                                          outcomeId = outcomeId,
                                          sccResultsRef = sccResultsRef)
        resultsReference <- rbind(resultsReference, resultsReferenceRow)
      }
    }
  }
  resultsReference$computeTarDist <- computeTarDist
  saveRDS(resultsReference, file.path(outputFolder, "resultsReference.rds"))

  ParallelLogger::logInfo("*** Running multiple analysis ***")
  executionArgList <- list()

  for (sccResultsRef in unique(resultsReference$sccResultsRef)) {
    refRow <- resultsReference[resultsReference$sccResultsRef == sccResultsRef,][1,]
    analysisRow <- ParallelLogger::matchInList(sccAnalysisList,
                                               list(analysisId = refRow$analysisId))[[1]]
    getrunSelfControlledCohortArgs <- analysisRow$runSelfControlledCohortArgs

    getrunSelfControlledCohortArgs$computeTarDistribution <- computeTarDist

    exposureIds <- unique(resultsReference$exposureId[resultsReference$sccResultsRef == sccResultsRef])
    outcomeId <- unique(resultsReference$outcomeId[resultsReference$sccResultsRef == sccResultsRef])

    args <- list(connectionDetails = connectionDetails,
                 cdmDatabaseSchema = cdmDatabaseSchema,
                 exposureDatabaseSchema = exposureDatabaseSchema,
                 exposureTable = exposureTable,
                 outcomeDatabaseSchema = outcomeDatabaseSchema,
                 outcomeTable = outcomeTable,
                 cdmVersion = cdmVersion,
                 exposureIds = exposureIds,
                 outcomeIds = outcomeId,
                 controlType = controlType,
                 analysisId = refRow$analysisId,
                 tempEmulationSchema = tempEmulationSchema,
                 resultExportPath = file.path(outputFolder, paste0("A_", refRow$analysisId)),
                 computeThreads = computeThreads)
    args <- append(args, getrunSelfControlledCohortArgs)
    executionArgList[[length(executionArgList) + 1]] <- args
  }

  if (length(executionArgList) != 0) {
   lapply(executionArgList, runSelfControlledCohort)
  }

  invisible(resultsReference)
}

.createSccResultsRef <- function(analysisId) {
  name <- paste("SccResults_a", analysisId, sep = "")
  return(name)
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
#' @param outputFolder       Name of the folder where all the outputs have been written to.
#'
#' @export
summarizeAnalyses <- function(resultsReference, outputFolder) {
  result <- data.frame()
  for (sccResultsFile in unique(resultsReference$sccResultsRef)) {
    sccResults <- readRDS(file.path(outputFolder, sccResultsRef))$estimates
    if (nrow(sccResults) > 0) {
      analysisId <- resultsReference$analysisId[resultsReference$sccResultsRef == sccResultsRef][1]
      sccResults$analysisId <- analysisId
    }

    result <- rbind(result, sccResults)
  }

  # Return consistent column names
  if (nrow(result) == 0) {
    result <- data.frame(matrix(ncol = 15, nrow = 0))
    colnames(result) <- c("exposureId", "outcomeId", "numPersons", "numExposures", "numOutcomesExposed", "logRr", "seLogRr",
                          "numOutcomesUnexposed", "timeAtRiskExposed", "timeAtRiskUnexposed", "irr", "irrLb95", "irrUb95", "p", "analysisId")
  }
  return(result)
}
