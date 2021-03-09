library(testthat)

test_that("createSccAnalysis, saveSccAnalysisList, loadSccAnalysisList", {
  args1 <- createRunSelfControlledCohortArgs(riskWindowStartUnexposed = -60)

  analysis <- createSccAnalysis(analysisId = 1, description = "test", runSelfControlledCohortArgs = args1)

  expect_equal(class(analysis), "sccAnalysis")

  args2 <- createRunSelfControlledCohortArgs(addLengthOfExposureExposed = TRUE,
                                             riskWindowStartExposed = 1,
                                             riskWindowEndExposed = 30,
                                             addLengthOfExposureUnexposed = TRUE,
                                             riskWindowEndUnexposed = -1,
                                             riskWindowStartUnexposed = -30)
  analysis2 <- createSccAnalysis(analysisId = 2, description = "test 2", runSelfControlledCohortArgs = args2)

  analysisList <- list(
    item1 = analysis,
    item2 = analysis2
  )

  tmp <- tempfile(fileext = "json")
  saveSccAnalysisList(analysisList, tmp)
  analysisListLoaded <- loadSccAnalysisList(tmp)

  for (loadedAnalaysis in analysisListLoaded) {
    expect_equal(class(loadedAnalaysis), "sccAnalysis")
  }

  # Check for errors
  badAnalysisList <- list(
    item1 = analysis,
    item2 = list() # Should be type of sccAnalysis
  )
  expect_error(saveSccAnalysisList(badAnalysisList, tmp))
})
