library(testthat)

# Test a large number of parameter combinations on an empty database just to make sure we dont' have
# simple errors in our code.

testAllParams <- function(connectionDetails,
                          cdmDatabaseSchema) {
  # Open connection once, so it will be reused:
  conn <- DatabaseConnector::connect(connectionDetails)
  on.exit(DatabaseConnector::disconnect(conn), add = TRUE)

  for (outcomeTable in c("condition_era", "cohort")) {
    for (exposureTable in c("drug_era", "cohort")) {
      for (restrictAgeAndYear in c(TRUE, FALSE)) {
        for (addLengthOfExposure in c(TRUE, FALSE)) {
          for (hasFullTimeAtRisk in c(TRUE, FALSE)) {
              if (restrictAgeAndYear) {
                minAge <- "21"
                maxAge <- "65"
                studyStartDate <- "20000101"
                studyEndDate <- "20101231"
              } else {
                minAge <- ""
                maxAge <- ""
                studyStartDate <- ""
                studyEndDate <- ""
              }

              resultPath <- tempfile()
              dir.create(resultPath)
              runSelfControlledCohort(connection = conn,
                                      databaseId = 99,
                                      cdmDatabaseSchema = cdmDatabaseSchema,
                                      exposureIds = c(767410, 1314924, 907879),
                                      exposureTable = exposureTable,
                                      outcomeIds = 444382,
                                      outcomeTable = outcomeTable,
                                      negativeControlPairs = list(c(767410, 444382)),
                                      controlType = "exposure",
                                      minAge = minAge,
                                      maxAge = maxAge,
                                      studyStartDate = studyStartDate,
                                      studyEndDate = studyEndDate,
                                      addLengthOfExposureExposed = addLengthOfExposure,
                                      addLengthOfExposureUnexposed = addLengthOfExposure,
                                      hasFullTimeAtRisk = hasFullTimeAtRisk,
                                      resultExportPath = resultPath)

              checkManifestFiles(resultPath)
          }
        }
      }
    }
  }
}

test_that("SCC", {
  testAllParams(connectionDetails, cdmDatabaseSchema)
})
