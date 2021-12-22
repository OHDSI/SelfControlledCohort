library(testthat)

test_that("createSccAnalysis, saveSccAnalysisList, loadSccAnalysisList", {
  args1 <- createRunSelfControlledCohortArgs(riskWindowStartUnexposed = -60)

  analysis <- createSccAnalysis(analysisId = 1, description = "test", runSelfControlledCohortArgs = args1)

  expect_s3_class(analysis, "sccAnalysis")

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
  withr::defer({
    unlink(tmp, force = TRUE)
  }, testthat::teardown_env())

  saveSccAnalysisList(analysisList, tmp)
  analysisListLoaded <- loadSccAnalysisList(tmp)

  for (loadedAnalaysis in analysisListLoaded) {
    expect_s3_class(loadedAnalaysis, "sccAnalysis")
  }

  # Check for errors
  badAnalysisList <- list(
    item1 = analysis,
    item2 = list() # Should be type of sccAnalysis
  )

  expect_error(saveSccAnalysisList(c(1,2), tmp)) # Not a list
  expect_error(saveSccAnalysisList(list(), tmp)) # Empty
  expect_error(saveSccAnalysisList(badAnalysisList, tmp)) # Not an sccAnalysis object
})


test_that("loadExposureOutcomeList, saveExposureOutcomeList , createExposureOutcome", {
  exposureOutcome1 <- createExposureOutcome(123, 456)
  expect_s3_class(exposureOutcome1, "exposureOutcome")
  exposureOutcome2 <- createExposureOutcome(789, 101112)

  validList <- list(item1 = exposureOutcome1, item2 = exposureOutcome2)

  tmp <- tempfile(fileext = "json")
  withr::defer({
    unlink(tmp, force = TRUE)
  }, testthat::teardown_env())

  saveExposureOutcomeList(exposureOutcomeList = validList, file = tmp)

  loadedList <- loadExposureOutcomeList(tmp)

  for (eo in loadedList) {
    expect_equal(class(eo), "exposureOutcome")
  }

  badList <- list(item1 = exposureOutcome1, item2 = list())
  expect_error(saveExposureOutcomeList(c(1,2), tmp)) # Not a list
  expect_error(saveExposureOutcomeList(list(), tmp)) # Empty
  expect_error(saveExposureOutcomeList(badList, tmp)) # Not an sccAnalysis object
})

test_that(".selectByType works", {
  expect_error(.selectByType(NULL, list(a=1), "test"))
  expect_error(.selectByType("foo", c(), "test"))
  expect_true(.selectByType("foo", list(foo = 1), "test") == 1)
})
