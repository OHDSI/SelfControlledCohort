test_that("SCC method runs on Eunomia", {
  skip_if_not(dbms == "sqlite", "Eunomia tests - skipping dbms platform tests")
  datasetName <- "GiBleed"
  dbFile <- tempfile(fileext = paste0(datasetName, ".sqlite"))
  Eunomia::getDatabaseFile(datasetName,
                           cdmVersion = "5.3",
                           dbms = "sqlite",
                           databaseFile = dbFile,
                           inputFormat = "csv",
                           verbose = FALSE,
                           overwrite = FALSE)

  connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = "sqlite", server = dbFile)
  cdmDatabaseSchema <- "main"

  tConnection <- DatabaseConnector::connect(connectionDetails = connectionDetails)
  withr::defer(DatabaseConnector::disconnect(tConnection), testthat::teardown_env())

  result <- runSelfControlledCohort(connectionDetails = connectionDetails,
                                    cdmDatabaseSchema = "main",
                                    databaseId = 1,
                                    exposureIds = '',
                                    outcomeIds = '')

  # expect_s3_class(result$estimates, "data.frame")
  # expect_equal(nrow(result$estimates), 6966)
  # expect_equal(length(capture_output_lines(print(result))), 4)
  # expect_equal(summary(result), result$estimates)

  expect_error(runSelfControlledCohort(connection = tConnection,
                                       cdmDatabaseSchema = "main",
                                       exposureIds = '',
                                       outcomeIds = '',
                                       databaseId = 1,
                                       riskWindowEndExposed = 1,
                                       riskWindowStartExposed = 30,
                                       addLengthOfExposureExposed = F),
               "risk window")

  expect_error(runSelfControlledCohort(connection = tConnection,
                                       cdmDatabaseSchema = "main",
                                       exposureIds = '',
                                       outcomeIds = '',
                                       databaseId = 1,
                                       riskWindowEndUnexposed = -30,
                                       riskWindowStartUnexposed = -1,
                                       addLengthOfExposureUnexposed = F),
               "risk window")

  rm(result)
  result <- runSelfControlledCohort(connection = tConnection,
                                    cdmDatabaseSchema = "main",
                                    exposureIds = '',
                                    outcomeIds = '',
                                    databaseId = 1,
                                    exposureTable = 'drug_exposure',
                                    outcomeTable = 'condition_occurrence',
                                    resultsTable = "test_results_store",
                                    riskWindowsTable = "risk_window",
                                    resultsDatabaseSchema = "main")


  rdf <- DatabaseConnector::renderTranslateQuerySql(tConnection, "SELECT * from main.test_results_store")
  #expect_s3_class(rdf, "data.frame")
  rwdf <- DatabaseConnector::renderTranslateQuerySql(tConnection, "SELECT * from main.risk_window")
  #expect_s3_class(rdf, "data.frame")

  expect_error(
    result <- runSelfControlledCohort(connection = tConnection,
                                      cdmDatabaseSchema = "main",
                                      exposureIds = '',
                                      outcomeIds = '',
                                      databaseId = 1,
                                      exposureTable = 'drug_exposure',
                                      outcomeTable = 'condition_occurrence',
                                      resultsTable = "resultsTable"),
    "Results table"
  )

  expect_error(
    result <- runSelfControlledCohort(connection = tConnection,
                                      cdmDatabaseSchema = "main",
                                      exposureIds = '',
                                      outcomeIds = '',
                                      databaseId = 1,
                                      exposureTable = 'drug_exposure',
                                      outcomeTable = 'condition_occurrence',
                                      riskWindowsTable = "risk_window"),
    "Risk windows table"
  )

  expect_error(
    result <- runSelfControlledCohort(cdmDatabaseSchema = "main",
                                      exposureIds = '',
                                      outcomeIds = '',
                                      databaseId = 1,
                                      addLengthOfExposureUnexposed = F),
    "Connection details not set"
  )
})
