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

#' Compute Minimum Detectable Relative Risk (MDRR) for rate ratio
#'
#' @description
#' Calculates the minimum detectable relative risk for a two-sample Poisson rate comparison
#' using the Signed Root Likelihood (SRL1) method described by Musonda et al. (2006).
#' This diagnostic assesses whether the study has adequate statistical power to detect
#' clinically meaningful effects in a self-controlled design.
#'
#' @param exposedPersonTime    Total person-time in exposed window (in days)
#' @param unexposedPersonTime  Total person-time in unexposed window (in days)
#' @param exposedEvents        Number of outcome events in exposed window
#' @param unexposedEvents      Number of outcome events in unexposed window
#' @param alpha                Significance level (default: 0.05)
#' @param power                Desired power (default: 0.80)
#'
#' @return
#' Numeric value representing the MDRR. Values > 10.0 typically indicate low power.
#'
#' @details
#' The MDRR is the minimum incidence rate ratio that can be detected with the given 
#' sample size, alpha, and power. This implementation uses the SRL1 method from 
#' Musonda (2006), which is more accurate for self-controlled studies than 
#' standard binomial approximations.
#'
#' @references
#' Musonda P, Farrington CP, Whitaker HJ (2006) Samples sizes for self-controlled
#' case series studies, Statistics in Medicine, 15;25(15):2618-31
#'
#' @examples
#' # Calculate MDRR for a study with 100 exposed person-years and 200 unexposed person-years
#' mdrr <- computeMdrrForRateRatio(
#'   exposedPersonTime = 36500,    # 100 person-years in days
#'   unexposedPersonTime = 73000,  # 200 person-years in days
#'   exposedEvents = 10,
#'   unexposedEvents = 15
#' )
#' @export
computeMdrrForRateRatio <- function(exposedPersonTime,
                                    unexposedPersonTime,
                                    exposedEvents,
                                    unexposedEvents,
                                    alpha = 0.05,
                                    power = 0.80) {
  # Handle edge cases
  if (exposedPersonTime <= 0 || unexposedPersonTime <= 0) {
    return(NA_real_)
  }

  if (exposedEvents < 1 || unexposedEvents < 1) {
    return(NA_real_)
  }

  n <- exposedEvents + unexposedEvents


  # r = proportion of time exposed
  r <- exposedPersonTime / (exposedPersonTime + unexposedPersonTime)
  
  if (r <= 0 || r >= 1) {
    return(Inf)
  }

  zAlpha <- qnorm(1 - alpha / 2)
  
  # Power calculation function using SRL1 (Musonda 2006, expression 7)
  computePowerSrl <- function(b, z, r, n) {
    if (b <= 0) return(0)
    eb <- exp(b)
    # A is twice the expected value of the log-likelihood ratio statistic
    A <- 2 * ((eb * r / (eb * r + 1 - r)) * b - log(eb * r + 1 - r))
    # B is the variance-correction term
    B <- b^2 / A * eb * r * (1 - r) / (eb * r + 1 - r)^2
    zb <- (sqrt(n * A) - z) / sqrt(B)
    return(stats::pnorm(zb))
  }

  # Use uniroot to find the log-RR (b) that achieves the target power
  # We search for b in [log(1.0001), log(100)]
  targetFn <- function(b) {
    return(computePowerSrl(b, zAlpha, r, n) - power)
  }
  
  # Check boundaries
  if (targetFn(log(100)) < 0) {
    return(100) # Exceeds search range
  }
  
  if (targetFn(1e-6) > 0) {
    return(1.0) # Power reached at very small RR
  }

  root <- tryCatch({
    stats::uniroot(targetFn, interval = c(1e-6, log(100)), tol = 1e-4)$root
  }, error = function(e) {
    return(NA_real_)
  })

  if (is.na(root)) return(NA_real_)
  
  return(exp(root))
}


#' Test for pre-exposure gain
#'
#' @description
#' Checks whether outcomes occur before the exposure cohort start date, which violates
#' the temporal assumptions of the self-controlled cohort design. This is sometimes called
#' "pre-exposure gain" or "immortal time bias".
#'
#' @param connection              DatabaseConnector connection object
#' @param riskWindowsTable        Name of the risk windows table
#' @param outcomeTable            Name of outcome table
#' @param outcomeDatabaseSchema   Schema containing outcome table
#' @param analysisId              Analysis identifier
#' @param cdmDatabaseSchema       Name of CDM schema
#' @param tempEmulationSchema     Schema for temp table emulation
#'
#' @return
#' Data frame with columns:
#' \itemize{
#'   \item target_cohort_id - Exposure cohort ID
#'   \item outcome_cohort_id - Outcome cohort ID
#'   \item total_persons - Total persons in analysis
#'   \item count_before - Number of outcomes before exposure
#'   \item count_after - Number of outcomes after exposure
#'   \item total_pt_before - Total person-time before exposure
#'   \item total_pt_after - Total person-time after exposure
#'   \item pre_exposure_rate_ratio - Rate ratio (Before vs After)
#'   \item p_value - Rate ratio test p-value (H0: rateBefore <= rateAfter)
#' }
#'
#' @details
#' This diagnostic identifies whether outcomes occur before the cohort start date,
#' which should not happen in a properly designed SCC study. A high proportion of
#' pre-exposure outcomes (>5%) or a significant p-value suggests:
#' \itemize{
#'   \item Exposure definition includes outcomes
#'   \item Outcome definition includes exposures
#'   \item Temporal data quality issues
#'   \item Confounding by indication
#' }
#'
#' @noRd
testPreExposureGain <- function(connection,
                                riskWindowsTable,
                                outcomeTable,
                                outcomeDatabaseSchema,
                                analysisId,
                                cdmDatabaseSchema,
                                tempEmulationSchema) {
  # Determine outcome table columns
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

  sql <- "
  WITH windows AS (
      SELECT 
          rw.exposure_id as target_cohort_id,
          rw.person_id,
          rw.exposure_start_date,
          op.observation_period_start_date as obs_start,
          op.observation_period_end_date as obs_end,
          DATEADD(day, -60, rw.exposure_start_date) as wb_start,
          DATEADD(day, -31, rw.exposure_start_date) as wb_end,
          DATEADD(day, -30, rw.exposure_start_date) as wa_start,
          DATEADD(day, 0, rw.exposure_start_date) as wa_end
      FROM @risk_windows_table rw
      INNER JOIN @cdm_database_schema.observation_period op
          ON rw.person_id = op.person_id
      WHERE rw.analysis_id = @analysis_id
          AND rw.exposure_start_date >= op.observation_period_start_date
          AND rw.exposure_start_date <= op.observation_period_end_date
  ),
  person_pt AS (
      SELECT 
          target_cohort_id,
          person_id,
          CASE 
              WHEN obs_start <= wb_end AND obs_end >= wb_start 
              THEN DATEDIFF(day, CASE WHEN obs_start > wb_start THEN obs_start ELSE wb_start END, CASE WHEN obs_end < wb_end THEN obs_end ELSE wb_end END) + 1
              ELSE 0 
          END as pt_before,
          CASE 
              WHEN obs_start <= wa_end AND obs_end >= wa_start 
              THEN DATEDIFF(day, CASE WHEN obs_start > wa_start THEN obs_start ELSE wa_start END, CASE WHEN obs_end < wa_end THEN obs_end ELSE wa_end END) + 1
              ELSE 0 
          END as pt_after
      FROM windows
  ),
  target_pt AS (
      SELECT 
          target_cohort_id,
          SUM(CAST(pt_before AS BIGINT)) as total_pt_before,
          SUM(CAST(pt_after AS BIGINT)) as total_pt_after
      FROM person_pt
      GROUP BY target_cohort_id
  ),
  pair_counts AS (
      SELECT 
          w.target_cohort_id,
          o.@outcome_id as outcome_cohort_id,
          SUM(CASE WHEN o.@outcome_start_date >= w.wb_start AND o.@outcome_start_date <= w.wb_end THEN 1 ELSE 0 END) as count_before,
          SUM(CASE WHEN o.@outcome_start_date >= w.wa_start AND o.@outcome_start_date <= w.wa_end THEN 1 ELSE 0 END) as count_after
      FROM windows w
      INNER JOIN @outcome_database_schema.@outcome_table o
          ON w.person_id = o.@outcome_person_id
      GROUP BY w.target_cohort_id, o.@outcome_id
  )
  SELECT 
      pc.target_cohort_id,
      pc.outcome_cohort_id,
      pc.count_before,
      pc.count_after,
      tp.total_pt_before,
      tp.total_pt_after
  FROM pair_counts pc
  INNER JOIN target_pt tp ON pc.target_cohort_id = tp.target_cohort_id;
  "

  aggregatedResults <- DatabaseConnector::renderTranslateQuerySql(
    connection = connection,
    sql = sql,
    risk_windows_table = riskWindowsTable,
    cdm_database_schema = cdmDatabaseSchema,
    outcome_database_schema = outcomeDatabaseSchema,
    outcome_table = outcomeTable,
    outcome_id = outcomeId,
    outcome_start_date = outcomeStartDate,
    outcome_person_id = outcomePersonId,
    analysis_id = analysisId,
    tempEmulationSchema = tempEmulationSchema,
    snakeCaseToCamelCase = TRUE
  )

  if (nrow(aggregatedResults) == 0) {
    return(data.frame())
  }

  results <- data.frame()

  for (i in seq_len(nrow(aggregatedResults))) {
    row <- aggregatedResults[i, ]
    
    totalPtBefore <- row$totalPtBefore
    totalPtAfter <- row$totalPtAfter
    countBefore <- row$countBefore
    countAfter <- row$countAfter
    
    # Safe guard
    if (totalPtBefore <= 0 || totalPtAfter <= 0 || (countBefore == 0 && countAfter == 0)) {
       results <- rbind(results, data.frame(
        targetCohortId = row$targetCohortId,
        outcomeCohortId = row$outcomeCohortId,
        preExposureRateRatio = NA_real_,
        pValue = NA_real_
      ))
       next
    }

    # Perform rateratio test (Before vs After)
    # H0: rateBefore <= rateAfter; H1: rateBefore > rateAfter
    testResult <- rateratio.test::rateratio.test(
      x = c(countBefore, countAfter),
      n = c(totalPtBefore, totalPtAfter),
      alternative = "greater"
    )
    
    results <- rbind(results, data.frame(
      targetCohortId = row$targetCohortId,
      outcomeCohortId = row$outcomeCohortId,
      preExposureRateRatio = testResult$estimate[1],
      pValue = testResult$p.value
    ))
  }

  return(results)
}

