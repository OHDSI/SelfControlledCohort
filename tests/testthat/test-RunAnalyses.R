library(testthat)

test_that("multiple analyses", {
  # Analysis.R is checked elsewhere
  exposureOutcome1 <- createExposureOutcome(701322, 28060)
  exposureOutcome2 <- createExposureOutcome(715997, 4294548)
  exposureOutcome3 <- createExposureOutcome(701322, 4043241)
  exposureOutcomeList <- list(exposureOutcome1, exposureOutcome2, exposureOutcome3)

  runSelfControlledCohortArgs1 <- createRunSelfControlledCohortArgs(firstExposureOnly = FALSE, computeTarDistribution = TRUE)
  runSelfControlledCohortArgs2 <- createRunSelfControlledCohortArgs(firstExposureOnly = TRUE)
  sccAnalysis1 <- createSccAnalysis(analysisId = 1,
                                    runSelfControlledCohortArgs = runSelfControlledCohortArgs1)
  sccAnalysis2 <- createSccAnalysis(analysisId = 2,
                                    runSelfControlledCohortArgs = runSelfControlledCohortArgs2)
  sccAnalysisList <- list(sccAnalysis1, sccAnalysis2)

  outputFolder <- file.path(tempdir(), getOption("dbms"))
  withr::defer({
    unlink(outputFolder, force = TRUE)
  }, testthat::teardown_env())

  expect_warning(
    rr <- runSccAnalyses(connectionDetails = connectionDetails,
                         cdmDatabaseSchema = cdmDatabaseSchema,
                         oracleTempSchema = oracleTempSchema,
                         sccAnalysisList = sccAnalysisList,
                         exposureOutcomeList = exposureOutcomeList,
                         outputFolder = outputFolder,
                         computeThreads = 1)
  )
  expect_s3_class(rr, "data.frame")
  expect_true(file.exists(file.path(outputFolder, "resultsReference.rds")))
  apply(rr, 1, function(item) {
    expect_true(file.exists(file.path(outputFolder, item["sccResultsFile"])))
  })

  result <- summarizeAnalyses(rr, outputFolder)
  expect_s3_class(result, "data.frame")
})

test_that("Fail on analyses clone", {
  outputFolder <- file.path(tempdir(), getOption("dbms"))
  withr::defer({
    unlink(outputFolder, force = TRUE)
  }, testthat::teardown_env())

  exposureOutcome1 <- createExposureOutcome(767410, 444382)
  exposureOutcome2 <- createExposureOutcome(1314924, 444382)
  exposureOutcome3 <- createExposureOutcome(907879, 444382)
  exposureOutcomeList <- list(exposureOutcome1, exposureOutcome2, exposureOutcome3)

  runSelfControlledCohortArgs <- createRunSelfControlledCohortArgs(firstExposureOnly = FALSE)
  sccAnalysis <- createSccAnalysis(analysisId = 1,
                                    runSelfControlledCohortArgs = runSelfControlledCohortArgs)

  sccAnalysisList <- list(sccAnalysis, sccAnalysis)
  expect_error(runSccAnalyses(connectionDetails = connectionDetails,
                              cdmDatabaseSchema = cdmDatabaseSchema,
                              oracleTempSchema = oracleTempSchema,
                              sccAnalysisList = sccAnalysisList,
                              exposureOutcomeList = exposureOutcomeList,
                              outputFolder = outputFolder,
                              computeThreads = 8))
})