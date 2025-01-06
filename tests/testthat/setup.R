# location to download the JDBC drivers used in the tests
jdbcDriverFolder <- tempfile("jdbcDrivers")

withr::defer({
  unlink(jdbcDriverFolder, recursive = TRUE, force = TRUE)
}, testthat::teardown_env())


dbms <- getOption("dbms", default = "sqlite")
if (dbms == "sqlite") {
  connectionDetails <- Eunomia::getEunomiaConnectionDetails()
  cdmDatabaseSchema <- "main"
  cdmVersion <- 5
}
if (dbms == "postgresql") {
  DatabaseConnector::downloadJdbcDrivers("postgresql", pathToDriver = jdbcDriverFolder)
  connectionDetails <- createConnectionDetails(dbms = "postgresql",
                                               user = Sys.getenv("CDM5_POSTGRESQL_USER"),
                                               password = URLdecode(Sys.getenv("CDM5_POSTGRESQL_PASSWORD")),
                                               server = Sys.getenv("CDM5_POSTGRESQL_SERVER"),
                                               pathToDriver = jdbcDriverFolder)

  cdmDatabaseSchema <- Sys.getenv("CDM5_POSTGRESQL_CDM_SCHEMA")
  cdmVersion <- 5
}
if (dbms == "redshift") {
  DatabaseConnector::downloadJdbcDrivers("redshift", pathToDriver = jdbcDriverFolder)
  connectionDetails <- createConnectionDetails(dbms = "redshift",
                                               user = Sys.getenv("CDM5_REDSHIFT_USER"),
                                               password = URLdecode(Sys.getenv("CDM5_REDSHIFT_PASSWORD")),
                                               server = Sys.getenv("CDM5_REDSHIFT_SERVER"),
                                               pathToDriver = jdbcDriverFolder)

  cdmDatabaseSchema <- Sys.getenv("CDM5_REDSHIFT_CDM_SCHEMA")
  cdmVersion <- 5
}
if (dbms == "sql server") {
  DatabaseConnector::downloadJdbcDrivers("sql server", pathToDriver = jdbcDriverFolder)
  connectionDetails <- createConnectionDetails(dbms = "sql server",
                                               user = Sys.getenv("CDM5_SQL_SERVER_USER"),
                                               password = URLdecode(Sys.getenv("CDM5_SQL_SERVER_PASSWORD")),
                                               server = Sys.getenv("CDM5_SQL_SERVER_SERVER"),
                                               pathToDriver = jdbcDriverFolder)
  cdmDatabaseSchema <- Sys.getenv("CDM5_SQL_SERVER_CDM_SCHEMA")
  cdmVersion <- 5
}

if (dbms == "spark") {
  DatabaseConnector::downloadJdbcDrivers("spark", pathToDriver = jdbcDriverFolder)
  connectionDetails <- createConnectionDetails(dbms = "spark",
                                               user = Sys.getenv("CDM5_SPARK_USER"),
                                               password = URLdecode(Sys.getenv("CDM5_SPARK_PASSWORD")),
                                               connectionString = Sys.getenv("CDM5_SPARK_CONNECTION_STRING"),
                                               pathToDriver = jdbcDriverFolder)
  cdmDatabaseSchema <- Sys.getenv("CDM5_SPARK_CDM_SCHEMA")
  options("sqlRenderTempEmulationSchema" = Sys.getenv("CDM5_SPARK_OHDSI_SCHEMA"))
  cdmVersion <- 5
}