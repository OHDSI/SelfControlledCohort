library(testthat)

test_that("SCC on PostgreSQL", {
  # Postgresql
  connectionDetails <- createConnectionDetails(dbms = "postgresql",
                                     user = Sys.getenv("CDM5_POSTGRESQL_USER"),
                                     password = URLdecode(Sys.getenv("CDM5_POSTGRESQL_PASSWORD")),
                                     server = Sys.getenv("CDM5_POSTGRESQL_SERVER"))

  cdmDatabaseSchema <- Sys.getenv("CDM5_POSTGRESQL_CDM_SCHEMA")
  cdmVersion <- 5

  sccResult <- runSelfControlledCohort(connectionDetails,
                                       cdmDatabaseSchema = cdmDatabaseSchema,
                                       cdmVersion = cdmVersion,
                                       exposureIds = c(767410, 1314924, 907879),
                                       outcomeId = 444382,
                                       outcomeTable = "condition_era")

  expect_equal(class(sccResult), "sccResults")
  summary(sccResult)

  # SQL Server
  connectionDetails <- createConnectionDetails(dbms = "sql server",
                                     user = Sys.getenv("CDM5_SQL_SERVER_USER"),
                                     password = URLdecode(Sys.getenv("CDM5_SQL_SERVER_PASSWORD")),
                                     server = Sys.getenv("CDM5_SQL_SERVER_SERVER"))
  cdmDatabaseSchema <- Sys.getenv("CDM5_SQL_SERVER_CDM_SCHEMA")
  cdmVersion <- 5

  sccResult <- runSelfControlledCohort(connectionDetails,
                                       cdmDatabaseSchema = cdmDatabaseSchema,
                                       cdmVersion = cdmVersion,
                                       exposureIds = c(767410, 1314924, 907879),
                                       outcomeId = 444382,
                                       outcomeTable = "condition_era")

  expect_equal(class(sccResult), "sccResults")
  summary(sccResult)

  # Oracle
  connectionDetails <- createConnectionDetails(dbms = "oracle",
                                     user = Sys.getenv("CDM5_ORACLE_USER"),
                                     password = URLdecode(Sys.getenv("CDM5_ORACLE_PASSWORD")),
                                     server = Sys.getenv("CDM5_ORACLE_SERVER"))
  cdmDatabaseSchema <- Sys.getenv("CDM5_ORACLE_CDM_SCHEMA")
  oracleTempSchema <- Sys.getenv("CDM5_ORACLE_OHDSI_SCHEMA")

  cdmVersion <- 5

  sccResult <- runSelfControlledCohort(connectionDetails,
                                       cdmDatabaseSchema = cdmDatabaseSchema,
                                       oracleTempSchema = oracleTempSchema,
                                       cdmVersion = cdmVersion,
                                       exposureIds = c(767410, 1314924, 907879),
                                       outcomeId = 444382,
                                       outcomeTable = "condition_era")

  expect_equal(class(sccResult), "sccResults")
  summary(sccResult)
})
