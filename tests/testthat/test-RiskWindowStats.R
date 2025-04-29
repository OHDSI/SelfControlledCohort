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
                                     tempEmulationSchema = NULL))
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

  getSccRiskWindowStats(connection, outcomeDatabaseSchema = cdmDatabaseSchema, databaseId = 99, outcomeIds = 444382)

  # Invalid connection object
  connectionT <- DatabaseConnector::connect(connectionDetails)
  DatabaseConnector::disconnect(connectionT)
  expect_error(getSccRiskWindowStats(connection = connectionT,
                                     outcomeDatabaseSchema = cdmDatabaseSchema))
  expect_error(runSccRiskWindows(connection = connectionT,
                                 cdmDatabaseSchema = cdmDatabaseSchema))

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

  getSccRiskWindowStats(connection,
                        databaseId = 99,
                        outcomeDatabaseSchema = cdmDatabaseSchema,
                        riskWindowsTable = "test_risk_windows",
                        resultsDatabaseSchema = cdmDatabaseSchema,
                        outcomeIds = 444382)

})
