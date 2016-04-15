# @file Analyses.R
#
# Copyright 2016 Observational Health Data Sciences and Informatics
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

#' Create a SelfControlledCohort analysis specification
#'
#' @details
#' Create a set of analysis choices, to be used with the \code{\link{runSccAnalyses}} function.
#'
#' @param analysisId                    An integer that will be used later to refer to this specific
#'                                      set of analysis choices.
#' @param description                   A short description of the analysis.
#' @param exposureType                  If more than one exposure is provided for each exposureOutcome,
#'                                      this field should be used to select the specific exposure to
#'                                      use in this analysis.
#' @param outcomeType                   If more than one outcome is provided for each exposureOutcome,
#'                                      this field should be used to select the specific outcome to use
#'                                      in this analysis.
#' @param runSelfControlledCohortArgs   An object representing the arguments to be used when calling
#'                                      the \code{\link{runSelfControlledCohort}} function.
#'
#' @export
createSccAnalysis <- function(analysisId = 1,
                              description = "",
                              exposureType = NULL,
                              outcomeType = NULL,
                              runSelfControlledCohortArgs) {
  # First: get the default values:
  analysis <- list()
  for (name in names(formals(createSccAnalysis))) {
    analysis[[name]] <- get(name)
  }

  # Next: overwrite defaults with actual values if specified:
  values <- lapply(as.list(match.call())[-1], function(x) eval(x, envir = sys.frame(-3)))
  for (name in names(values)) {
    if (name %in% names(analysis)) {
      analysis[[name]] <- values[[name]]
    }
  }

  class(analysis) <- "sccAnalysis"
  return(analysis)
}

#' Save a list of sccAnalysis to file
#'
#' @description
#' Write a list of objects of type \code{sccAnalysis} to file. The file is in JSON format.
#'
#' @param sccAnalysisList   The sccAnalysis list to be written to file
#' @param file              The name of the file where the results will be written
#'
#' @export
saveSccAnalysisList <- function(sccAnalysisList, file) {
  stopifnot(is.list(sccAnalysisList))
  stopifnot(length(sccAnalysisList) > 0)
  for (i in 1:length(sccAnalysisList)) {
    stopifnot(class(sccAnalysisList[[i]]) == "sccAnalysis")
  }
  write(toJsonWithAttr(sccAnalysisList), file)
}

#' Load a list of sccAnalysis from file
#'
#' @description
#' Load a list of objects of type \code{sccAnalysis} from file. The file is in JSON format.
#'
#' @param file   The name of the file
#'
#' @return
#' A list of objects of type \code{sccAnalysis}.
#'
#' @export
loadsccAnalysisList <- function(file) {
  sccAnalysisList <- fromJsonWithAttr(file)
  for (i in 1:length(sccAnalysisList)) {
    class(sccAnalysisList[[i]]) <- "sccAnalysis"
    for (j in 1:length(sccAnalysisList[[i]])) {
      if (is.list(sccAnalysisList[[i]][[j]])) {
        class(sccAnalysisList[[i]][[j]]) <- "args"
      }
    }
  }
  return(sccAnalysisList)
}

#' Create exposure-outcome combinations.
#'
#' @details
#' Create a hypothesis of interest, to be used with the \code{\link{runSccAnalyses}} function.
#'
#' @param exposureId   A concept ID indentifying the drug of interest in the exposure table. If
#'                     multiple strategies for picking the exposure will be tested in the analysis, a
#'                     named list of numbers can be provided instead. In the analysis, the name of the
#'                     number to be used can be specified using the \code{exposureType} parameter in
#'                     the \code{\link{createSccAnalysis}} function.
#' @param outcomeId    A concept ID indentifying the outcome of interest in the outcome table. If
#'                     multiple strategies for picking the outcome will be tested in the analysis, a
#'                     named list of numbers can be provided instead. In the analysis, the name of the
#'                     number to be used can be specified using the #' \code{outcomeType} parameter in
#'                     the \code{\link{createSccAnalysis}} function.
#'
#' @export
createExposureOutcome <- function(exposureId, outcomeId) {
  exposureOutcome <- list(exposureId = exposureId,
                          outcomeId = outcomeId)
  class(exposureOutcome) <- "exposureOutcome"
  return(exposureOutcome)
}

#' Save a list of exposureOutcome to file
#'
#' @description
#' Write a list of objects of type \code{exposureOutcome} to file. The file is in JSON format.
#'
#' @param exposureOutcomeList   The exposureOutcome list to be written to file
#' @param file                  The name of the file where the results will be written
#'
#' @export
saveExposureOutcomeList <- function(exposureOutcomeList, file) {
  stopifnot(is.list(exposureOutcomeList))
  stopifnot(length(exposureOutcomeList) > 0)
  for (i in 1:length(exposureOutcomeList)) {
    stopifnot(class(exposureOutcomeList[[i]]) == "exposureOutcome")
  }
  write(toJsonWithAttr(exposureOutcomeList), file)
}

#' Load a list of exposureOutcome from file
#'
#' @description
#' Load a list of objects of type \code{exposureOutcome} from file. The file is in JSON format.
#'
#' @param file   The name of the file
#'
#' @return
#' A list of objects of type \code{exposureOutcome}.
#'
#' @export
loadExposureOutcomeList <- function(file) {
  exposureOutcomeList <- fromJsonWithAttr(file)
  for (i in 1:length(exposureOutcomeList)) {
    class(exposureOutcomeList[[i]]) <- "exposureOutcome"
  }
  return(exposureOutcomeList)
}


convertAttrToMember <- function(object){
  if (is.list(object)){
    if (length(object) > 0) {
      for (i in 1:length(object)) {
        if (!is.null(object[[i]])){
          object[[i]] <- convertAttrToMember(object[[i]])
        }
      }
    }
    a <- names(attributes(object))
    a <- a[a != "names"]
    if (length(a) > 0) {
      object[paste("attr",a, sep = "_")] <- attributes(object)[a]
    }
  }
  return(object)
}

toJsonWithAttr <- function(object){
  object <- convertAttrToMember(object)
  return (jsonlite::toJSON(object, pretty = TRUE, force = TRUE, null = "null", auto_unbox = TRUE))
}

convertMemberToAttr <-  function(object){
  if (is.list(object)){
    if (length(object) > 0) {
      for (i in 1:length(object)) {
        if (!is.null(object[[i]])){
          object[[i]] <- convertMemberToAttr(object[[i]])
        }
      }
      attrNames <- names(object)[grep("^attr_", names(object))]
      cleanNames <- gsub("^attr_", "", attrNames)
      if (any(cleanNames == "class")){
        class(object) <- object$attr_class
        object$attr_class <- NULL
        attrNames <- attrNames[attrNames != "attr_class"]
        cleanNames <- cleanNames[cleanNames != "class"]
      }
      attributes(object)[cleanNames] <- object[attrNames]
      object[attrNames] <- NULL
    }
  }
  return(object)
}

fromJsonWithAttr <- function(file) {
  object <- jsonlite::fromJSON(file, simplifyVector = TRUE, simplifyDataFrame = FALSE)
  object <- convertMemberToAttr(object)
  return(object)
}
