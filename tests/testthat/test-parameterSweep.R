library(testthat)

# Test a large number of parameter combinations on an empty database just to make sure we dont' have
# simple errors in our code.

testAllParams <- function(connectionDetails,
                          cdmDatabaseSchema,
                          cdmVersion,
                          oracleTempSchema = NULL) {
  for (outcomeTable in c("condition_era", "cohort")) {
    for (restrictAgeAndYear in c(TRUE, FALSE)) {
      for (stratify in c(TRUE, FALSE)) {
        for (useLengthOfExposure in c(TRUE, FALSE)) {
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
          sccResult <- runSelfControlledCohort(connectionDetails,
                                               cdmDatabaseSchema = cdmDatabaseSchema,
                                               oracleTempSchema = oracleTempSchema,
                                               cdmVersion = cdmVersion,
                                               exposureIds = c(767410, 1314924, 907879),
                                               outcomeIds = 444382,
                                               outcomeTable = outcomeTable,
                                               minAge = minAge,
                                               maxAge = maxAge,
                                               studyStartDate = studyStartDate,
                                               studyEndDate = studyEndDate,
                                               stratifyByGender = stratify,
                                               stratifyByAge = stratify,
                                               stratifyByYear = stratify,
                                               useLengthOfExposureExposed = useLengthOfExposure,
                                               useLengthOfExposureUnexposed = useLengthOfExposure,
                                               hasFullTimeAtRisk = hasFullTimeAtRisk)
          expect_equal(class(sccResult), "sccResults")
          expect_equal(class(summary(sccResult)), "data.frame")
          }
        }
      }
    }
  }
}

test_that("SCC on PostgreSQL", {
  # Postgresql
  connectionDetails <- createConnectionDetails(dbms = "postgresql",
                                               user = Sys.getenv("CDM5_POSTGRESQL_USER"),
                                               password = URLdecode(Sys.getenv("CDM5_POSTGRESQL_PASSWORD")),
                                               server = Sys.getenv("CDM5_POSTGRESQL_SERVER"))

  cdmDatabaseSchema <- Sys.getenv("CDM5_POSTGRESQL_CDM_SCHEMA")
  cdmVersion <- 5

  testAllParams(connectionDetails, cdmDatabaseSchema, cdmVersion)

})

test_that("SCC on SQL Server", {
  # SQL Server
  connectionDetails <- createConnectionDetails(dbms = "sql server",
                                               user = Sys.getenv("CDM5_SQL_SERVER_USER"),
                                               password = URLdecode(Sys.getenv("CDM5_SQL_SERVER_PASSWORD")),
                                               server = Sys.getenv("CDM5_SQL_SERVER_SERVER"))
  cdmDatabaseSchema <- Sys.getenv("CDM5_SQL_SERVER_CDM_SCHEMA")
  cdmVersion <- 5

  testAllParams(connectionDetails, cdmDatabaseSchema, cdmVersion)
})

test_that("SCC on Oracle", {
  # Oracle
  connectionDetails <- createConnectionDetails(dbms = "oracle",
                                               user = Sys.getenv("CDM5_ORACLE_USER"),
                                               password = URLdecode(Sys.getenv("CDM5_ORACLE_PASSWORD")),
                                               server = Sys.getenv("CDM5_ORACLE_SERVER"))
  cdmDatabaseSchema <- Sys.getenv("CDM5_ORACLE_CDM_SCHEMA")
  oracleTempSchema <- Sys.getenv("CDM5_ORACLE_OHDSI_SCHEMA")

  cdmVersion <- 5

  testAllParams(connectionDetails, cdmDatabaseSchema, cdmVersion, oracleTempSchema)

})
