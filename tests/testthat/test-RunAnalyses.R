library(testthat)

test_that("multiple analyses", {
  # Analysis.R is checked elsewhere
  exposureOutcome1 <- createExposureOutcome(701322, 28060)
  exposureOutcome2 <- createExposureOutcome(715997, 4294548)
  exposureOutcome3 <- createExposureOutcome(701322, 4043241, trueEffectSize = 1)
  exposureOutcomeList <- list(exposureOutcome1, exposureOutcome2, exposureOutcome3)

  runSelfControlledCohortArgs1 <- createRunSelfControlledCohortArgs(firstExposureOnly = FALSE, computeTarDistribution = TRUE)
  runSelfControlledCohortArgs2 <- createRunSelfControlledCohortArgs(firstExposureOnly = TRUE)
  sccAnalysis1 <- createSccAnalysis(analysisId = 1,
                                    runSelfControlledCohortArgs = runSelfControlledCohortArgs1)
  sccAnalysis2 <- createSccAnalysis(analysisId = 2,
                                    runSelfControlledCohortArgs = runSelfControlledCohortArgs2)
  sccAnalysisList <- list(sccAnalysis1, sccAnalysis2)

  withr::with_tempfile("outputFolder", {
    resultsRef <- runSccAnalyses(connectionDetails = connectionDetails,
                                 cdmDatabaseSchema = cdmDatabaseSchema,
                                 sccAnalysisList = sccAnalysisList,
                                 exposureOutcomeList = exposureOutcomeList,
                                 outputFolder = outputFolder,
                                 databaseId = 1,
                                 computeThreads = 1)

    checkmate::expect_data_frame(resultsRef)
    checkManifestFiles(file.path(outputFolder, "A_1"))
    checkManifestFiles(file.path(outputFolder, "A_2"))
  })
})

test_that("Fail on analyses clone", {
  withr::with_tempfile("outputFolder", {
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
                                sccAnalysisList = sccAnalysisList,
                                exposureOutcomeList = exposureOutcomeList,
                                outputFolder = outputFolder,
                                computeThreads = 8))
  })
})
