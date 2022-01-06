connection <- DatabaseConnector::connect(connectionDetails)
withr::defer({
  DatabaseConnector::disconnect(connection)
}, testthat::teardown_env())

test_that("General test + errors and warnings", {
  expect_error(runSccRiskWindows(connection = NULL,
                                 cdmDatabaseSchema = cdmDatabaseSchema))

  if (dbms == "oracle") {
    expect_warning(runSccRiskWindows(connection = connection,
                                     cdmDatabaseSchema = cdmDatabaseSchema,
                                     exposureTable = "drug_era",
                                     firstExposureOnly = TRUE,
                                     addLengthOfExposureExposed = F,
                                     riskWindowStartExposed = 1,
                                     riskWindowEndExposed = 30,
                                     addLengthOfExposureUnexposed = TRUE,
                                     riskWindowEndUnexposed = -1,
                                     riskWindowStartUnexposed = -30,
                                     hasFullTimeAtRisk = FALSE,
                                     washoutPeriod = 100,
                                     followupPeriod = 0,
                                     riskWindowsTable = "#risk_windows",
                                     tempEmulationSchema = NULL,
                                     oracleTempSchema = Sys.getenv("CDM5_ORACLE_OHDSI_SCHEMA")))
  } else {
    runSccRiskWindows(connection = connection,
                      cdmDatabaseSchema = cdmDatabaseSchema,
                      exposureTable = "drug_era",
                      firstExposureOnly = TRUE,
                      addLengthOfExposureExposed = F,
                      riskWindowStartExposed = 1,
                      riskWindowEndExposed = 30,
                      addLengthOfExposureUnexposed = TRUE,
                      riskWindowEndUnexposed = -1,
                      riskWindowStartUnexposed = -30,
                      hasFullTimeAtRisk = FALSE,
                      washoutPeriod = 100,
                      followupPeriod = 0,
                      riskWindowsTable = "#risk_windows")
  }

  expect_error(getSccRiskWindowStats(connection = NULL,
                                     outcomeDatabaseSchema = cdmDatabaseSchema))

  stats <- getSccRiskWindowStats(connection, outcomeDatabaseSchema = cdmDatabaseSchema, outcomeIds = 444382)
  expect_false(is.null(stats$treatmentTimeDistribution))
  expect_false(is.null(stats$timeToOutcomeDistribution))
  expect_false(is.null(stats$timeToOutcomeDistributionExposed))
  expect_false(is.null(stats$timeToOutcomeDistributionUnexposed))

  # Invalid connection object
  connectionT <- DatabaseConnector::connect(connectionDetails)
  DatabaseConnector::disconnect(connectionT)
  expect_error(getSccRiskWindowStats(connection = connectionT,
                                     outcomeDatabaseSchema = cdmDatabaseSchema))
  expect_error(runSccRiskWindows(connection = connectionT,
                                 cdmDatabaseSchema = cdmDatabaseSchema))

})

test_that("Using real risk windows tables", {
  # Not all test platforms provide a schema we can create tables in, only sqlite will be used
  skip_if_not(dbms == "sqlite", "Test not available on db platform")

  runSccRiskWindows(connection = connection,
                    cdmDatabaseSchema = cdmDatabaseSchema,
                    exposureTable = "drug_era",
                    firstExposureOnly = TRUE,
                    addLengthOfExposureExposed = F,
                    riskWindowStartExposed = 1,
                    riskWindowEndExposed = 30,
                    addLengthOfExposureUnexposed = TRUE,
                    riskWindowEndUnexposed = -1,
                    riskWindowStartUnexposed = -30,
                    hasFullTimeAtRisk = FALSE,
                    washoutPeriod = 100,
                    followupPeriod = 0,
                    riskWindowsTable = "test_risk_windows",
                    resultsDatabaseSchema = cdmDatabaseSchema)

  stats <- getSccRiskWindowStats(connection,
                                 outcomeDatabaseSchema = cdmDatabaseSchema,
                                 riskWindowsTable = "test_risk_windows",
                                 resultsDatabaseSchema = cdmDatabaseSchema,
                                 outcomeIds = 444382)

  expect_false(is.null(stats$treatmentTimeDistribution))
  expect_false(is.null(stats$timeToOutcomeDistribution))
  expect_false(is.null(stats$timeToOutcomeDistributionExposed))
  expect_false(is.null(stats$timeToOutcomeDistributionUnexposed))
})