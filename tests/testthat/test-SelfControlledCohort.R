test_that("SCC method runs on Eunomia", {
  skip_if_not(dbms == "sqlite", "Eunomia tests - skipping dbms platform tests")
  connectionDetails <- Eunomia::getEunomiaConnectionDetails()
  tConnection <- DatabaseConnector::connect(connectionDetails = connectionDetails)
  withr::defer(DatabaseConnector::disconnect(tConnection), testthat::teardown_env())

  result <- runSelfControlledCohort(connectionDetails = connectionDetails,
                                    cdmDatabaseSchema = "main",
                                    exposureIds = '',
                                    outcomeIds = '')

  expect_s3_class(result$estimates, "data.frame")
  expect_equal(nrow(result$estimates), 6966)
  expect_equal(length(capture_output_lines(print(result))), 4)
  expect_equal(summary(result), result$estimates)

  expect_error(runSelfControlledCohort(connection = tConnection,
                                       cdmDatabaseSchema = "main",
                                       exposureIds = '',
                                       outcomeIds = '',
                                       riskWindowEndExposed = 1,
                                       riskWindowStartExposed = 30,
                                       addLengthOfExposureExposed = F),
               "risk window")

  expect_error(runSelfControlledCohort(connection = tConnection,
                                       cdmDatabaseSchema = "main",
                                       exposureIds = '',
                                       outcomeIds = '',
                                       riskWindowEndUnexposed = -30,
                                       riskWindowStartUnexposed = -1,
                                       addLengthOfExposureUnexposed = F),
               "risk window")

  rm(result)
  result <- runSelfControlledCohort(connection = tConnection,
                                    cdmDatabaseSchema = "main",
                                    exposureIds = '',
                                    outcomeIds = '',
                                    exposureTable = 'drug_exposure',
                                    outcomeTable = 'condition_occurrence',
                                    resultsTable = "test_results_store",
                                    riskWindowsTable = "risk_window",
                                    resultsDatabaseSchema = "main",
                                    computeTarDistribution = TRUE)

  expect_false(is.null(result$tarStats$treatmentTimeDistribution))
  expect_true("mean" %in% colnames(result$tarStats$treatmentTimeDistribution))

  expect_false(is.null(result$tarStats$timeToOutcomeDistribution))
  expect_true("mean" %in% colnames(result$tarStats$timeToOutcomeDistribution))

  expect_false(is.null(result$tarStats$timeToOutcomeDistributionExposed))
  expect_true("mean" %in% colnames(result$tarStats$timeToOutcomeDistributionExposed))

  expect_false(is.null(result$tarStats$timeToOutcomeDistributionUnexposed))
  expect_true("mean" %in% colnames(result$tarStats$timeToOutcomeDistributionUnexposed))


  rdf <- DatabaseConnector::renderTranslateQuerySql(tConnection, "SELECT * from main.test_results_store")
  expect_s3_class(rdf, "data.frame")
  rwdf <- DatabaseConnector::renderTranslateQuerySql(tConnection, "SELECT * from main.risk_window")
  expect_s3_class(rdf, "data.frame")

  expect_error(
    result <- runSelfControlledCohort(connection = tConnection,
                                      cdmDatabaseSchema = "main",
                                      exposureIds = '',
                                      outcomeIds = '',
                                      exposureTable = 'drug_exposure',
                                      outcomeTable = 'condition_occurrence',
                                      resultsTable = "resultsTable",
                                      computeTarDistribution = TRUE),
    "Results table"
  )

  expect_error(
    result <- runSelfControlledCohort(connection = tConnection,
                                      cdmDatabaseSchema = "main",
                                      exposureIds = '',
                                      outcomeIds = '',
                                      exposureTable = 'drug_exposure',
                                      outcomeTable = 'condition_occurrence',
                                      riskWindowsTable = "risk_window",
                                      computeTarDistribution = TRUE),
    "Risk windows table"
  )

  expect_error(
    result <- runSelfControlledCohort(cdmDatabaseSchema = "main",
                                      exposureIds = '',
                                      outcomeIds = '',
                                      addLengthOfExposureUnexposed = F),
    "Connection details not set"
  )
})
