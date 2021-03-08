
test_that("SCC method runs on Eunomia", {
  connectionDetails <- Eunomia::getEunomiaConnectionDetails()
  result <- runSelfControlledCohort(connectionDetails = connectionDetails,
                                    cdmDatabaseSchema = "main",
                                    exposureIds = '',
                                    outcomeIds = '')

  expect_s3_class(result$estimates, "data.frame")
  expect_equal(nrow(result$estimates), 6966)
  expect_equal(length(capture_output_lines(print(result))), 4)
  expect_equal(summary(result), result$estimates)

  expect_error(runSelfControlledCohort(connectionDetails = connectionDetails,
                                       cdmDatabaseSchema = "main",
                                       exposureIds = '',
                                       outcomeIds = '',
                                       riskWindowEndExposed = 1,
                                       riskWindowStartExposed = 30,
                                       addLengthOfExposureExposed = F),
               "risk window")

  expect_error(runSelfControlledCohort(connectionDetails = connectionDetails,
                                       cdmDatabaseSchema = "main",
                                       exposureIds = '',
                                       outcomeIds = '',
                                       riskWindowEndUnexposed = -30,
                                       riskWindowStartUnexposed = -1,
                                       addLengthOfExposureUnexposed = F),
               "risk window")

  rm(result)
  result <- runSelfControlledCohort(connectionDetails = connectionDetails,
                                    cdmDatabaseSchema = "main",
                                    exposureIds = '',
                                    outcomeIds = '',
                                    exposureTable = 'drug_exposure',
                                    outcomeTable = 'condition_occurrence',
                                    computeTarDistribution = TRUE)

  expect_s3_class(result$estimates, "data.frame")
  expect_equal(nrow(result$estimates), 9040)
  expect_true("meanTxTime" %in% colnames(result$estimates))
})
