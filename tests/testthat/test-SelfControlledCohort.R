test_that("SCC method runs on Eunomia", {
  connectionDetails <- Eunomia::getEunomiaConnectionDetails()
  tConnection <- DatabaseConnector::connect(connectionDetails = connectionDetails)
  withr::defer(DatabaseConnector::disconnect(tConnection), testthat::teardown_env())
  connectionDetails$conn <- tConnection

  result <- runSelfControlledCohort(connectionDetails = connectionDetails,
                                    cdmDatabaseSchema = "main",
                                    exposureIds = '',
                                    outcomeIds = '')

  expect_s3_class(result$estimates, "data.frame")
  expect_equal(nrow(result$estimates), 6966)
  expect_equal(length(capture_output_lines(print(result))), 4)
  expect_equal(summary(result), result$estimates)

  expect_error(runSelfControlledCohort(connectionDetails = connectionDetails,
                                       cdmDatabaseSchema = "main",
                                       exposureIds = '',
                                       outcomeIds = '',
                                       riskWindowEndExposed = 1,
                                       riskWindowStartExposed = 30,
                                       addLengthOfExposureExposed = F),
               "risk window")

  expect_error(runSelfControlledCohort(connectionDetails = connectionDetails,
                                       cdmDatabaseSchema = "main",
                                       exposureIds = '',
                                       outcomeIds = '',
                                       riskWindowEndUnexposed = -30,
                                       riskWindowStartUnexposed = -1,
                                       addLengthOfExposureUnexposed = F),
               "risk window")

  rm(result)
  result <- runSelfControlledCohort(connectionDetails = connectionDetails,
                                    cdmDatabaseSchema = "main",
                                    exposureIds = '',
                                    outcomeIds = '',
                                    exposureTable = 'drug_exposure',
                                    outcomeTable = 'condition_occurrence',
                                    resultsTable = "test_results_store",
                                    riskWindowsTable = "risk_window",
                                    resultsDatabaseSchema = "main",
                                    computeTarDistribution = TRUE)

  expect_s3_class(result$tarStats, "data.frame")
  expect_true("meanTxTime" %in% colnames(result$tarStats))


  rdf <- DatabaseConnector::renderTranslateQuerySql(tConnection, "SELECT * from main.test_results_store")
  expect_s3_class(rdf, "data.frame")
  rwdf <- DatabaseConnector::renderTranslateQuerySql(tConnection, "SELECT * from main.risk_window")
  expect_s3_class(rdf, "data.frame")

  expect_error(
    result <- runSelfControlledCohort(connectionDetails = connectionDetails,
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
    result <- runSelfControlledCohort(connectionDetails = connectionDetails,
                                      cdmDatabaseSchema = "main",
                                      exposureIds = '',
                                      outcomeIds = '',
                                      exposureTable = 'drug_exposure',
                                      outcomeTable = 'condition_occurrence',
                                      riskWindowsTable = "risk_window",
                                      computeTarDistribution = TRUE),
    "Risk windows table"
  )
})
