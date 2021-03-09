
test_that("multiple analyses", {

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

  outputFolder <- tempdir()
  rr <- runSccAnalyses(connectionDetails = Eunomia::getEunomiaConnectionDetails(),
                       cdmDatabaseSchema = "main",
                       sccAnalysisList = sccAnalysisList,
                       exposureOutcomeList = exposureOutcomeList,
                       outputFolder = outputFolder,
                       computeThreads = 8)
})
