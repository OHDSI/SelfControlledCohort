# Tests functionality of multithreaded processing and callback for writing data batches
test_that("batch callback works", {

  writeBack <- function(data, position, writeLocation) {
    write.csv(data, file = writeLocation, row.names = FALSE)
    return(data)
  }

  if (dbms != "sqlite") {
    exposureIds <- 948078
    outcomeIds <- 72990
  } else {
    exposureIds <- NULL
    outcomeIds <- NULL
  }

  withr::with_tempfile("writeLocation", {
    result <- runSelfControlledCohort(connectionDetails = connectionDetails,
                                      cdmDatabaseSchema = cdmDatabaseSchema,
                                      exposureIds = exposureIds,
                                      outcomeIds = outcomeIds,
                                      returnEstimates = FALSE,
                                      postProcessFunction = writeBack,
                                      postProcessArgs = list(writeLocation = writeLocation),
                                      computeThreads = 2)


    readData <- read.csv(writeLocation)
    expect_true(nrow(readData) > 0)
  })
})
