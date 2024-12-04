# Sample data for testing
sample_negatives <- data.frame(
  rr = c(1.2, 0.8, 1.0),
  seLogRr = c(0.2, 0.1, 0.3)
)

sample_positives <- data.frame(
  rr = c(1.5, 0.9, 1.1),
  seLogRr = c(0.2, 0.1, 0.3),
  cPt = c(100, 150, 200),
  cAtRisk = c(1000, 1500, 2000),
  cCases = c(10, 20, 30),
  tCases = c(15, 25, 35),
  tAtRisk = c(950, 1450, 1950),
  target_cohort_id = c(1, 2, 3)
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

  # Test the expected error when idCol does not exist
  expect_error(computeCalibratedRows(sample_positives, sample_negatives, idCol = "non_existent_id"))

  # Valid call
  result <- computeCalibratedRows(positives = sample_positives,
                                  negatives = sample_negatives,
                                  idCol = "target_cohort_id")

  expect_type(result, "list")  # A tibble is a list
  expect_equal(nrow(result), nrow(sample_positives))  # Expect same number of rows
  expect_named(result, c("pValue", "ub95", "lb95", "rr", "seLogRr", "cPt", "cAtRisk", "cCases", "tCases", "tAtRisk", "target_cohort_id"))
})