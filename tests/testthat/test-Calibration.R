# Sample data for testing
sample_negatives <- data.frame(
  rr = c(1.2, 0.8, 1.0),
  seLogRr = c(0.2, 0.1, 0.3)
)

sample_positives <- data.frame(
  targetCohortId = 1:3,
  outcomeCohortId = 4:6,
  rr = c(1.5, 0.9, 1.1),
  seLogRr = c(0.2, 0.1, 0.3),
  numPersons = c(100, 150, 200),
  numExposures = c(1000, 1500, 2000),
  numOutcomesExposed = c(10, 20, 30),
  numOutcomesUnexposed = c(15, 25, 35),
  timeAtRiskExposed = c(950, 1450, 1950),
  timeAtRiskUnexposed = c(950, 1450, 1950),
  targetCohortId = c(1, 2, 3),
  pValue = 0.05,
  ub95 = c(1.6, 1.1, 1.2),
  lb95 = c(1.2, 0.8, 1.0)
)

test_that("getNullDist function works as expected", {
  expect_error(getNullDist(NULL))

  expect_error(getNullDist(data.frame(rr = c(), seLogRr = c())))

  expect_error(getNullDist(data.frame(rr = 1:3, seLogRr = NULL)))

  result <- getNullDist(sample_negatives)
  expect_true(inherits(result, "null"))  # Assuming the return type
})

test_that("computeCalibratedRows function works as expected", {
  expect_error(computeCalibratedRows(NULL, sample_negatives))

  expect_error(computeCalibratedRows(sample_positives, NULL))

  # Valid call

  result <- computeCalibratedRows(positives = sample_positives,
                                  negatives = sample_negatives)

  checkmate::expect_data_frame(result)
  expect_equal(nrow(result), nrow(sample_positives))

  checkmate::expect_names(names(result),
                          must.include = c("pValue", "ub95", "lb95", "rr", "seLogRr", "numExposures", "numPersons",
                                           "calibratedPValue", "calibratedUb95", "calibratedLb95", "calibratedRr",
                                           "calibratedSeLogRr", "numOutcomesExposed", "numOutcomesUnexposed",
                                           "timeAtRiskExposed", "timeAtRiskUnexposed", "targetCohortId", "outcomeCohortId"))
})
