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

#' Get Null Distribution from EmpiricalCalibration Package
#' @noRd
getNullDist <- function(negatives) {
  checkmate::assertNames(names(negatives), must.include = c("rr", "seLogRr"))
  negatives <- tidyr::drop_na(negatives)

  if (nrow(negatives) == 0) {
    # This is just to prevent warnings in unit tests
    naNull <- c(NA, NA)
    names(naNull) <- c("mean", "sd")
    class(naNull) <- "null" # This is a null distribution not a NULL reference
    return(naNull)
  }
  EmpiricalCalibration::fitNull(logRr = log(negatives$rr), seLogRr = negatives$seLogRr)
}

#' @title
#' Compute calibrated rows
#' @description
#' Actual calibration is performed here in a dplyr friendly way
#' @param positives   this is the cohort set that should be calibrated
#' @param negatives   these are the negative control cohort results
#' @return
#' data.frame
#' @noRd
computeCalibratedRows <- function(positives,
                                  negatives,
                                  keepCols = c("numExposures", "numPersons", "numOutcomesExposed", "rr", "pValue",
                                               "ub95", "lb95", "seLogRr", "numOutcomesUnexposed", "timeAtRiskExposed",
                                               "timeAtRiskUnexposed", "targetCohortId", "outcomeCohortId")) {

  if (nrow(positives) == 0)
    return(positives)

  checkmate::assertNames(names(positives), must.include = c("rr", "seLogRr", keepCols))
  nullDist <- getNullDist(negatives)
  errorModel <- EmpiricalCalibration::convertNullToErrorModel(nullDist)
  ci <- EmpiricalCalibration::calibrateConfidenceInterval(log(positives$rr),
                                                          positives$seLogRr,
                                                          errorModel)
  pvalue <- EmpiricalCalibration::calibrateP(nullDist, log(positives$rr), positives$seLogRr)
  # Row matches fields in the database excluding the ids, used in dplyr, group_by with keep_true
  result <- data.frame(calibratedPValue = pvalue,
                       calibratedUb95 = exp(ci$logUb95Rr),
                       calibratedLb95 = exp(ci$logLb95Rr),
                       calibratedRr = exp(ci$logRr),
                       calibratedSeLogRr = ci$seLogRr)

  keptColumns <- positives |>
    dplyr::select(dplyr::all_of(keepCols))
  result <- result |>
    dplyr::bind_cols(keptColumns)
  return(result)
}

#' Compute Expected Absolute Systematic Error (EASE)
#'
#' @description
#' Computes the expected absolute systematic error from the null distribution
#' fitted on negative control estimates. EASE summarizes both bias (mean of null)
#' and imprecision (spread of null) into a single metric.
#'
#' @param negatives  Data frame of negative control results with columns `rr` and `seLogRr`
#'
#' @return
#' Numeric EASE value, or NA if the null distribution could not be fitted.
#'
#' @details
#' EASE is computed by fitting a null distribution to the negative control
#' log rate ratios using \code{EmpiricalCalibration::fitNull()}, then calling
#' \code{EmpiricalCalibration::computeExpectedAbsoluteSystematicError()}.
#'
#' Lower values indicate less systematic error. A value of 0 means no detectable
#' bias. The default threshold of 0.25 is aligned with SCCS package conventions.
#'
#' @references
#' Schuemie MJ, Hripcsak G, Ryan PB, Madigan D, Suchard MA. Empirical confidence
#' interval calibration for population-level effect estimation studies in
#' observational healthcare data. PNAS. 2018;115(11):2571-2577.
#'
#' @examples
#' \donttest{
#' # Compute EASE from negative control results
#' negativeControls <- data.frame(
#'   rr = c(0.95, 1.02, 0.98, 1.05),
#'   seLogRr = c(0.2, 0.18, 0.22, 0.19)
#' )
#' ease <- computeEase(negativeControls)
#' }
#' @export
computeEase <- function(negatives) {
  checkmate::assertNames(names(negatives), must.include = c("rr", "seLogRr"))
  negatives <- tidyr::drop_na(negatives)

  if (nrow(negatives) < 2) {
    return(NA_real_)
  }

  tryCatch(
    {
      nullDist <- getNullDist(negatives)
      if (is.na(nullDist["mean"]) || is.na(nullDist["sd"])) {
        return(NA_real_)
      }
      ease <- EmpiricalCalibration::computeExpectedAbsoluteSystematicError(nullDist)
      return(as.numeric(ease))
    },
    error = function(e) {
      ParallelLogger::logWarn(sprintf("Failed to compute EASE: %s", e$message))
      return(NA_real_)
    }
  )
}
