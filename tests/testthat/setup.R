# location to download the JDBC drivers used in the tests
jdbcDriverFolder <- tempfile("jdbcDrivers")

withr::defer({
  unlink(jdbcDriverFolder, recursive = TRUE, force = TRUE)
}, testthat::teardown_env())

if (getOption("dbms") == "postgresql") {
  DatabaseConnector::downloadJdbcDrivers("postgresql")
  connectionDetails <- createConnectionDetails(dbms = "postgresql",
                                               user = Sys.getenv("CDM5_POSTGRESQL_USER"),
                                               password = URLdecode(Sys.getenv("CDM5_POSTGRESQL_PASSWORD")),
                                               server = Sys.getenv("CDM5_POSTGRESQL_SERVER"),
                                               pathToDriver = jdbcDriverFolder)

  cdmDatabaseSchema <- Sys.getenv("CDM5_POSTGRESQL_CDM_SCHEMA")
  cdmVersion <- 5
}
if (getOption("dbms") == "sql server") {
  DatabaseConnector::downloadJdbcDrivers("sql server")
  connectionDetails <- createConnectionDetails(dbms = "sql server",
                                               user = Sys.getenv("CDM5_SQL_SERVER_USER"),
                                               password = URLdecode(Sys.getenv("CDM5_SQL_SERVER_PASSWORD")),
                                               server = Sys.getenv("CDM5_SQL_SERVER_SERVER"),
                                               pathToDriver = jdbcDriverFolder)
  cdmDatabaseSchema <- Sys.getenv("CDM5_SQL_SERVER_CDM_SCHEMA")
  cdmVersion <- 5
}
if (getOption("dbms") == "oracle") {
  DatabaseConnector::downloadJdbcDrivers("oracle")
  connectionDetails <- createConnectionDetails(dbms = "oracle",
                                               user = Sys.getenv("CDM5_ORACLE_USER"),
                                               password = URLdecode(Sys.getenv("CDM5_ORACLE_PASSWORD")),
                                               server = Sys.getenv("CDM5_ORACLE_SERVER"),
                                               pathToDriver = jdbcDriverFolder)
  cdmDatabaseSchema <- Sys.getenv("CDM5_ORACLE_CDM_SCHEMA")
  oracleTempSchema <- Sys.getenv("CDM5_ORACLE_OHDSI_SCHEMA")
  cdmVersion <- 5
}