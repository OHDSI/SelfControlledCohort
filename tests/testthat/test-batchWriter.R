# Tests functionality of multithreaded processing and callback for writing data batches
test_that("batch callback works", {

  writeBack <- function(data, position, writeLocation) {
    write.csv(data, file = writeLocation, row.names = FALSE)
  }

  writeLocation <- tempfile()
  # Test with real db (jdbc connection)
  result <- runSelfControlledCohort(connectionDetails = connectionDetails,
                                    cdmDatabaseSchema = cdmDatabaseSchema,
                                    exposureIds = c(948078),
                                    outcomeIds = 72990,
                                    returnEstimates = FALSE,
                                    postProcessFunction = writeBack,
                                    postProcessArgs = list(writeLocation = writeLocation),
                                    computeThreads = 2)

  readData <- read.csv(writeLocation)
  expect_true(nrow(readData) > 0)
  unlink(writeLocation)

  writeLocation <- tempfile()
  # Test with eunomia
  connectionDetails <- Eunomia::getEunomiaConnectionDetails()
  result <- runSelfControlledCohort(connectionDetails = connectionDetails,
                                    cdmDatabaseSchema = "main",
                                    exposureIds = '',
                                    outcomeIds = '',
                                    returnEstimates = FALSE,
                                    postProcessFunction = writeBack,
                                    postProcessArgs = list(writeLocation = writeLocation),
                                    computeThreads = 2)

  readData <- read.csv(writeLocation)
  expect_true(nrow(readData) > 0)
  unlink(writeLocation)
})