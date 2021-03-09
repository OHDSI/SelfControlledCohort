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
