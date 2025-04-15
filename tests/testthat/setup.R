# location to download the JDBC drivers used in the tests
jdbcDriverFolder <- tempfile("jdbcDrivers")
co <- getOption("connectionObserver")
options(connectionObserver=NULL)

withr::defer({
  options(connectionObserver=co)
  unlink(jdbcDriverFolder, recursive = TRUE, force = TRUE)
}, testthat::teardown_env())


dbms <- getOption("dbms", default = "sqlite")
cdmVersion <- 5
if (dbms == "sqlite") {
  datasetName <- "Synthea27Nj"
  #Eunomia::downloadEunomiaData(datasetName = datasetName)
  dbFile <- tempfile(fileext = paste0(datasetName, ".sqlite"))
  Eunomia::getDatabaseFile(datasetName,
                           cdmVersion = "5.4",
                           dbms = "sqlite",
                           databaseFile = dbFile,
                           inputFormat = "csv",
                           verbose = FALSE,
                           overwrite = FALSE)

  connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = "sqlite", server = dbFile)
  cdmDatabaseSchema <- "main"
}
if (dbms == "postgresql") {
  DatabaseConnector::downloadJdbcDrivers("postgresql", pathToDriver = jdbcDriverFolder)
  connectionDetails <- createConnectionDetails(dbms = "postgresql",
                                               user = Sys.getenv("CDM5_POSTGRESQL_USER"),
                                               password = URLdecode(Sys.getenv("CDM5_POSTGRESQL_PASSWORD")),
                                               server = Sys.getenv("CDM5_POSTGRESQL_SERVER"),
                                               pathToDriver = jdbcDriverFolder)

  cdmDatabaseSchema <- Sys.getenv("CDM5_POSTGRESQL_CDM_SCHEMA")
}
if (dbms == "redshift") {
  DatabaseConnector::downloadJdbcDrivers("redshift", pathToDriver = jdbcDriverFolder)
  connectionDetails <- createConnectionDetails(dbms = "redshift",
                                               user = Sys.getenv("CDM5_REDSHIFT_USER"),
                                               password = URLdecode(Sys.getenv("CDM5_REDSHIFT_PASSWORD")),
                                               server = Sys.getenv("CDM5_REDSHIFT_SERVER"),
                                               pathToDriver = jdbcDriverFolder)

  cdmDatabaseSchema <- Sys.getenv("CDM5_REDSHIFT_CDM_SCHEMA")
}
if (dbms == "sql server") {
  DatabaseConnector::downloadJdbcDrivers("sql server", pathToDriver = jdbcDriverFolder)
  connectionDetails <- createConnectionDetails(dbms = "sql server",
                                               user = Sys.getenv("CDM5_SQL_SERVER_USER"),
                                               password = URLdecode(Sys.getenv("CDM5_SQL_SERVER_PASSWORD")),
                                               server = Sys.getenv("CDM5_SQL_SERVER_SERVER"),
                                               pathToDriver = jdbcDriverFolder)
  cdmDatabaseSchema <- Sys.getenv("CDM5_SQL_SERVER_CDM_SCHEMA")
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
}