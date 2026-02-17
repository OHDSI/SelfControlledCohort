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

library(testthat)
library(SelfControlledCohort)

test_that("computeMdrrForRateRatio returns sensible values", {
  # Test with adequate sample size
  mdrr <- computeMdrrForRateRatio(
    exposedPersonTime = 50000,
    unexposedPersonTime = 150000,
    exposedEvents = 40,
    unexposedEvents = 90,
    alpha = 0.05,
    power = 0.80
  )

  expect_true(is.numeric(mdrr))
  expect_true(mdrr > 1.0)
  expect_true(mdrr < 10.0)
  expect_true(mdrr < 2.0)  # Should be well-powered
})

test_that("computeMdrrForRateRatio handles low power correctly", {
  # Test with small sample size
  mdrr <- computeMdrrForRateRatio(
    exposedPersonTime = 500,
    unexposedPersonTime = 1500,
    exposedEvents = 3,
    unexposedEvents = 7,
    alpha = 0.05,
    power = 0.80
  )

  expect_true(is.numeric(mdrr))
  expect_true(mdrr > 2.0)  # Should be underpowered
})

test_that("computeMdrrForRateRatio handles edge cases", {
  # Zero person-time
  expect_true(is.na(computeMdrrForRateRatio(0, 1000, 10, 20)))
  expect_true(is.na(computeMdrrForRateRatio(1000, 0, 10, 20)))

  # Zero events
  expect_true(is.na(computeMdrrForRateRatio(1000, 2000, 0, 20)))
  expect_true(is.na(computeMdrrForRateRatio(1000, 2000, 10, 0)))

  # Negative values
  expect_true(is.na(computeMdrrForRateRatio(-1000, 2000, 10, 20)))
})

test_that("checkSparseData works correctly", {
  # Test passing case
  result <- checkSparseData(10, 15, minEvents = 3)
  expect_true(result$pass)
  expect_true(grepl("Adequate", result$message))

  # Test failing case - exposed
  result <- checkSparseData(2, 15, minEvents = 3)
  expect_false(result$pass)
  expect_true(grepl("Sparse", result$message))

  # Test failing case - unexposed
  result <- checkSparseData(10, 2, minEvents = 3)
  expect_false(result$pass)
  expect_true(grepl("Sparse", result$message))

  # Test edge case - exactly at threshold
  result <- checkSparseData(3, 3, minEvents = 3)
  expect_true(result$pass)
})

test_that("checkSparseData handles multiple inputs", {
  # Test vectorized input
  results <- checkSparseData(c(5, 2, 10), c(8, 7, 15), minEvents = 3)

  expect_equal(length(results), 3)
  expect_true(results[[1]]$pass)   # 5, 8 - both pass
  expect_false(results[[2]]$pass)  # 2, 7 - exposed fails
  expect_true(results[[3]]$pass)   # 10, 15 - both pass
})

test_that("getDefaultDiagnosticThresholds returns correct structure", {
  thresholds <- getDefaultDiagnosticThresholds()

  expect_type(thresholds, "list")

  # Check all required thresholds exist
  expect_true("mdrrMaxAcceptable" %in% names(thresholds))
  expect_true("maxPreExposureProportion" %in% names(thresholds))
  expect_true("preExposurePThreshold" %in% names(thresholds))
  expect_true("maxEventDependentCensoring" %in% names(thresholds))
  expect_true("timeTrendPThreshold" %in% names(thresholds))
  expect_true("minEventsPerWindow" %in% names(thresholds))

  # Check values are sensible
  expect_equal(thresholds$mdrrMaxAcceptable, 2.0)
  expect_equal(thresholds$maxPreExposureProportion, 0.05)
  expect_equal(thresholds$preExposurePThreshold, 0.05)
  expect_equal(thresholds$maxEventDependentCensoring, 0.10)
  expect_equal(thresholds$timeTrendPThreshold, 0.05)
  expect_equal(thresholds$minEventsPerWindow, 3)
})

test_that("MDRR calculation is consistent", {
  # Same input should give same output
  mdrr1 <- computeMdrrForRateRatio(10000, 20000, 20, 40, 0.05, 0.80)
  mdrr2 <- computeMdrrForRateRatio(10000, 20000, 20, 40, 0.05, 0.80)

  expect_equal(mdrr1, mdrr2)
})

test_that("MDRR increases with smaller sample size", {
  # Larger sample should have lower MDRR (better power)
  mdrr_small <- computeMdrrForRateRatio(1000, 2000, 10, 20, 0.05, 0.80)
  mdrr_large <- computeMdrrForRateRatio(10000, 20000, 100, 200, 0.05, 0.80)

  expect_true(mdrr_small > mdrr_large)
})

test_that("MDRR responds to power parameter", {
  # Higher power requirement should increase MDRR
  mdrr_low_power <- computeMdrrForRateRatio(5000, 10000, 30, 50, 0.05, 0.70)
  mdrr_high_power <- computeMdrrForRateRatio(5000, 10000, 30, 50, 0.05, 0.90)

  expect_true(mdrr_high_power > mdrr_low_power)
})

test_that("checkSparseData validates input", {
  # Different length vectors should error
  expect_error(
    checkSparseData(c(5, 10), c(8), minEvents = 3),
    "same length"
  )
})

test_that("MDRR handles very large samples", {
  # Very large sample should have MDRR close to 1
  mdrr <- computeMdrrForRateRatio(
    exposedPersonTime = 1000000,
    unexposedPersonTime = 3000000,
    exposedEvents = 1000,
    unexposedEvents = 2500,
    alpha = 0.05,
    power = 0.80
  )

  expect_true(is.numeric(mdrr))
  expect_true(mdrr > 1.0)
  expect_true(mdrr < 1.5)  # Should be very well-powered
})

test_that("Diagnostic threshold names don't include old names", {
  thresholds <- getDefaultDiagnosticThresholds()

  # These old threshold names should NOT exist
  expect_false("eventDependentCensoringMaxProportion" %in% names(thresholds))
  expect_false("preExposureRateRatioMaxDeviation" %in% names(thresholds))
  expect_false("minExposedCount" %in% names(thresholds))
  expect_false("minUnexposedCount" %in% names(thresholds))
  expect_false("minPersonCount" %in% names(thresholds))
  expect_false("maxWindowImbalanceRatio" %in% names(thresholds))
  expect_false("minObservationProportion" %in% names(thresholds))
  expect_false("maxTimeTrendPValue" %in% names(thresholds))
})
