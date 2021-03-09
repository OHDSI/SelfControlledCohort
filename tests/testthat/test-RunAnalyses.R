library(testthat)

test_that("multiple analyses", {
  # Analysis.R is checked elsewhere
  exposureOutcome1 <- createExposureOutcome(767410, 444382)
  exposureOutcome2 <- createExposureOutcome(1314924, 444382)
  exposureOutcome3 <- createExposureOutcome(907879, 444382)
  exposureOutcomeList <- list(exposureOutcome1, exposureOutcome2, exposureOutcome3)

  runSelfControlledCohortArgs1 <- createRunSelfControlledCohortArgs(firstExposureOnly = FALSE)
  runSelfControlledCohortArgs2 <- createRunSelfControlledCohortArgs(firstExposureOnly = TRUE)
  sccAnalysis1 <- createSccAnalysis(analysisId = 1,
                                    runSelfControlledCohortArgs = runSelfControlledCohortArgs1)
  sccAnalysis2 <- createSccAnalysis(analysisId = 2,
                                    runSelfControlledCohortArgs = runSelfControlledCohortArgs2)
  sccAnalysisList <- list(sccAnalysis1, sccAnalysis2)

  if (getOption("dbms") == "postgresql") {
    connectionDetails <- createConnectionDetails(dbms = "postgresql",
                                                 user = Sys.getenv("CDM5_POSTGRESQL_USER"),
                                                 password = URLdecode(Sys.getenv("CDM5_POSTGRESQL_PASSWORD")),
                                                 server = Sys.getenv("CDM5_POSTGRESQL_SERVER"))

    cdmDatabaseSchema <- Sys.getenv("CDM5_POSTGRESQL_CDM_SCHEMA")
    cdmVersion <- 5
  }
  if (getOption("dbms") == "sql server") {
    connectionDetails <- createConnectionDetails(dbms = "sql server",
                                                 user = Sys.getenv("CDM5_SQL_SERVER_USER"),
                                                 password = URLdecode(Sys.getenv("CDM5_SQL_SERVER_PASSWORD")),
                                                 server = Sys.getenv("CDM5_SQL_SERVER_SERVER"))
    cdmDatabaseSchema <- Sys.getenv("CDM5_SQL_SERVER_CDM_SCHEMA")
    cdmVersion <- 5
  }
  if (getOption("dbms") == "oracle") {
    connectionDetails <- createConnectionDetails(dbms = "oracle",
                                                 user = Sys.getenv("CDM5_ORACLE_USER"),
                                                 password = URLdecode(Sys.getenv("CDM5_ORACLE_PASSWORD")),
                                                 server = Sys.getenv("CDM5_ORACLE_SERVER"))
    cdmDatabaseSchema <- Sys.getenv("CDM5_ORACLE_CDM_SCHEMA")
    oracleTempSchema <- Sys.getenv("CDM5_ORACLE_OHDSI_SCHEMA")

    cdmVersion <- 5
  }

  outputFolder <- file.path(tempdir(), getOption("dbms"))
  dir.create(outputFolder)
  on.exit(unlink(outputFolder))

  rr <- runSccAnalyses(connectionDetails = connectionDetails,
                       cdmDatabaseSchema = cdmDatabaseSchema,
                       oracleTempSchema = oracleTempSchema,
                       sccAnalysisList = sccAnalysisList,
                       exposureOutcomeList = exposureOutcomeList,
                       outputFolder = outputFolder,
                       computeThreads = 8)

  expect_is(rr, "data.frame")
  expect_true(file.exists(file.path(outputFolder, "resultsReference.rds")))
  apply(rr, 1, function(item) {
    expect_true(file.exists(file.path(outputFolder, item["sccResultsFile"])))
  })

  result <- summarizeAnalyses(rr, outputFolder)
  expect_is(result, "data.frame")
})
