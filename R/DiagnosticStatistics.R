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
#' using a power calculation approach. This diagnostic assesses whether the study has
#' adequate statistical power to detect clinically meaningful effects.
#'
#' @param exposedPersonTime    Total person-time in exposed window (in days)
#' @param unexposedPersonTime  Total person-time in unexposed window (in days)
#' @param exposedEvents        Number of outcome events in exposed window
#' @param unexposedEvents      Number of outcome events in unexposed window
#' @param alpha                Significance level (default: 0.05)
#' @param power                Desired power (default: 0.80)
#'
#' @return
#' Numeric value representing the MDRR. Values > 2.0 typically indicate low power.
#'
#' @details
#' The MDRR is calculated using an iterative approach to find the rate ratio that would
#' be detectable with the given sample size, alpha, and power. Lower MDRR values indicate
#' better power. An MDRR > 2.0 suggests the study may only detect large effects.
#'
#' The calculation uses the observed baseline rate in the unexposed window and solves
#' for the rate ratio that achieves the desired power.
#'
#' @references
#' Schuemie MJ, Ryan PB, Hripcsak G, Madigan D, Suchard MA. Improving reproducibility by using
#' high-throughput observational studies with empirical calibration. Phil Trans R Soc A. 2018.
#'
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

  # Calculate baseline rate (unexposed)
  baselineRate <- unexposedEvents / unexposedPersonTime

  if (baselineRate <= 0) {
    return(NA_real_)
  }

  # Total person-time
  totalTime <- exposedPersonTime + unexposedPersonTime

  # Use binary search to find MDRR
  # We're looking for the rate ratio where power = 0.80
  lower <- 1.01
  upper <- 100
  tolerance <- 0.01
  maxIterations <- 100

  for (i in seq_len(maxIterations)) {
    testRR <- (lower + upper) / 2

    # Expected events under alternative hypothesis
    expectedExposed <- baselineRate * testRR * exposedPersonTime
    expectedUnexposed <- baselineRate * unexposedPersonTime
    expectedTotal <- expectedExposed + expectedUnexposed

    # Calculate standard error under alternative
    # SE(log(RR)) = sqrt(1/E1 + 1/E0)
    seLogRR <- sqrt(1 / expectedExposed + 1 / expectedUnexposed)

    # Calculate critical value (two-sided test)
    zAlpha <- qnorm(1 - alpha / 2)
    zPower <- qnorm(power)

    # Effect size needed for detection
    logRR <- log(testRR)

    # Calculate power using normal approximation
    # Power = P(Z > z_alpha - log(RR)/SE)
    calculatedPower <- pnorm(logRR / seLogRR - zAlpha)

    if (abs(calculatedPower - power) < tolerance) {
      return(testRR)
    }

    if (calculatedPower < power) {
      # Need larger RR for this power
      lower <- testRR
    } else {
      # Can detect smaller RR
      upper <- testRR
    }

    if (upper - lower < 0.01) {
      return(testRR)
    }
  }

  # If we didn't converge, return the midpoint
  return((lower + upper) / 2)
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
#'   \item persons_with_pre_exposure_outcome - Persons with outcomes before exposure
#'   \item proportion - Proportion with pre-exposure outcomes
#'   \item p_value - Binomial test p-value (H0: proportion = 0)
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
  SELECT
    rw.exposure_id as target_cohort_id,
    o.@outcome_id as outcome_cohort_id,
    COUNT(DISTINCT rw.person_id) as total_persons,
    COUNT(DISTINCT CASE WHEN o.@outcome_start_date < rw.exposure_start_date
                        THEN rw.person_id ELSE NULL END) as persons_with_pre_exposure_outcome
  FROM @risk_windows_table rw
  LEFT JOIN @outcome_database_schema.@outcome_table o
    ON rw.person_id = o.@outcome_person_id
  WHERE rw.analysis_id = @analysis_id
  GROUP BY rw.exposure_id, o.@outcome_id
  "

  results <- DatabaseConnector::renderTranslateQuerySql(
    connection = connection,
    sql = sql,
    risk_windows_table = riskWindowsTable,
    outcome_database_schema = outcomeDatabaseSchema,
    outcome_table = outcomeTable,
    outcome_start_date = outcomeStartDate,
    outcome_id = outcomeId,
    outcome_person_id = outcomePersonId,
    analysis_id = analysisId,
    tempEmulationSchema = tempEmulationSchema,
    snakeCaseToCamelCase = TRUE
  )

  if (nrow(results) == 0) {
    return(data.frame())
  }

  # Calculate proportion and p-value for each group
  results$proportion <- results$personsWithPreExposureOutcome / results$totalPersons

  # Binomial test: H0 is that proportion = 0 (no pre-exposure outcomes expected)
  # Use one-sided test since we only care if proportion > 0
  results$pValue <- vapply(seq_len(nrow(results)), function(i) {
    if (results$personsWithPreExposureOutcome[i] == 0) {
      return(1.0)
    }
    # Use binomial test
    binom.test(results$personsWithPreExposureOutcome[i],
      results$totalPersons[i],
      p = 0,
      alternative = "greater"
    )$p.value
  }, numeric(1))

  return(results)
}

#' Test for time trend in outcome risk
#'
#' @description
#' Fits a Poisson GLM to test whether outcome risk changes over calendar time.
#' A significant time trend violates the assumption of stable baseline risk needed
#' for valid SCC analysis.
#'
#' @param connection              DatabaseConnector connection object
#' @param riskWindowsTable        Name of the risk windows table
#' @param outcomeTable            Name of outcome table
#' @param outcomeDatabaseSchema   Schema containing outcome table
#' @param analysisId              Analysis identifier
#' @param tempEmulationSchema     Schema for temp table emulation
#'
#' @return
#' Data frame with columns:
#' \itemize{
#'   \item target_cohort_id - Exposure cohort ID
#'   \item outcome_cohort_id - Outcome cohort ID
#'   \item time_trend_p_value - P-value for calendar time coefficient
#'   \item time_trend_coefficient - Coefficient estimate for time trend
#' }
#'
#' @details
#' This diagnostic fits a Poisson regression model:
#' \deqn{log(E[Y]) = \beta_0 + \beta_1 \times calendar\_time + offset(log(time))}
#'
#' A significant time trend (p < 0.05) suggests:
#' \itemize{
#'   \item Seasonal patterns in outcome risk
#'   \item Changes in diagnosis/coding practices
#'   \item Population changes over time
#'   \item Confounding by time-varying factors
#' }
#'
#' @noRd
testTimeTrend <- function(connection,
                          riskWindowsTable,
                          outcomeTable,
                          outcomeDatabaseSchema,
                          analysisId,
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

  # Get outcome counts by calendar month
  sql <- "
  SELECT
    rw.exposure_id as target_cohort_id,
    o.@outcome_id as outcome_cohort_id,
    YEAR(o.@outcome_start_date) * 12 + MONTH(o.@outcome_start_date) as calendar_month,
    COUNT(*) as outcome_count,
    SUM(DATEDIFF(day, rw.risk_window_start_unexposed, rw.risk_window_end_exposed)) as person_time
  FROM @risk_windows_table rw
  INNER JOIN @outcome_database_schema.@outcome_table o
    ON rw.person_id = o.@outcome_person_id
    AND o.@outcome_start_date >= rw.risk_window_start_unexposed
    AND o.@outcome_start_date <= rw.risk_window_end_exposed
  WHERE rw.analysis_id = @analysis_id
  GROUP BY rw.exposure_id, o.@outcome_id,
           YEAR(o.@outcome_start_date) * 12 + MONTH(o.@outcome_start_date)
  "

  results <- DatabaseConnector::renderTranslateQuerySql(
    connection = connection,
    sql = sql,
    risk_windows_table = riskWindowsTable,
    outcome_database_schema = outcomeDatabaseSchema,
    outcome_table = outcomeTable,
    outcome_start_date = outcomeStartDate,
    outcome_id = outcomeId,
    outcome_person_id = outcomePersonId,
    analysis_id = analysisId,
    tempEmulationSchema = tempEmulationSchema,
    snakeCaseToCamelCase = TRUE
  )

  if (nrow(results) == 0) {
    return(data.frame())
  }

  # Fit Poisson GLM for each exposure-outcome pair
  uniquePairs <- unique(results[, c("targetCohortId", "outcomeCohortId")])

  output <- data.frame()

  for (i in seq_len(nrow(uniquePairs))) {
    pair <- uniquePairs[i, ]
    pairData <- results[results$targetCohortId == pair$targetCohortId &
      results$outcomeCohortId == pair$outcomeCohortId, ]

    # Need at least 3 time points
    if (nrow(pairData) < 3) {
      outputRow <- data.frame(
        targetCohortId = pair$targetCohortId,
        outcomeCohortId = pair$outcomeCohortId,
        timeTrendPValue = NA_real_,
        timeTrendCoefficient = NA_real_
      )
      output <- rbind(output, outputRow)
      next
    }

    # Standardize calendar month to start from 0
    pairData$calendarMonthStd <- pairData$calendarMonth - min(pairData$calendarMonth)

    # Fit Poisson GLM
    tryCatch(
      {
        model <- glm(outcomeCount ~ calendarMonthStd + offset(log(pmax(personTime, 1))),
          data = pairData,
          family = poisson(link = "log")
        )

        # Extract coefficient and p-value for time trend
        coefSummary <- summary(model)$coefficients

        if ("calendarMonthStd" %in% rownames(coefSummary)) {
          timeTrendCoef <- coefSummary["calendarMonthStd", "Estimate"]
          timeTrendP <- coefSummary["calendarMonthStd", "Pr(>|z|)"]
        } else {
          timeTrendCoef <- NA_real_
          timeTrendP <- NA_real_
        }

        outputRow <- data.frame(
          targetCohortId = pair$targetCohortId,
          outcomeCohortId = pair$outcomeCohortId,
          timeTrendPValue = timeTrendP,
          timeTrendCoefficient = timeTrendCoef
        )
        output <- rbind(output, outputRow)
      },
      error = function(e) {
        ParallelLogger::logWarn(sprintf(
          "Failed to fit time trend model for exposure %s, outcome %s: %s",
          pair$targetCohortId, pair$outcomeCohortId, e$message
        ))
        outputRow <- data.frame(
          targetCohortId = pair$targetCohortId,
          outcomeCohortId = pair$outcomeCohortId,
          timeTrendPValue = NA_real_,
          timeTrendCoefficient = NA_real_
        )
        output <- rbind(output, outputRow)
      }
    )
  }

  return(output)
}

#' Check for sparse data
#'
#' @description
#' Checks whether there are sufficient events in both exposed and unexposed windows
#' for reliable rate ratio estimation.
#'
#' @param exposedEvents    Number of events in exposed window
#' @param unexposedEvents  Number of events in unexposed window
#' @param minEvents        Minimum required events per window (default: 3)
#'
#' @return
#' List with elements:
#' \itemize{
#'   \item pass - Logical indicating whether check passed
#'   \item message - Character string describing the result
#' }
#'
#' @details
#' Sparse data (< 3 events in either window) can lead to:
#' \itemize{
#'   \item Unstable rate ratio estimates
#'   \item Wide confidence intervals
#'   \item Poor coverage of nominal confidence levels
#' }
#'
#' This is similar to the "zero cell" problem in contingency tables.
#'
#' @export
checkSparseData <- function(exposedEvents, unexposedEvents, minEvents = 3) {
  if (length(exposedEvents) != length(unexposedEvents)) {
    stop("exposedEvents and unexposedEvents must have the same length")
  }

  results <- list()

  for (i in seq_along(exposedEvents)) {
    pass <- exposedEvents[i] >= minEvents && unexposedEvents[i] >= minEvents
    results[[i]] <- list(pass = pass, message = message)
  }

  if (length(results) == 1) {
    return(results[[1]])
  }

  return(results)
}
