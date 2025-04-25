# When devtools::load_all is run, create symbolic link for sql directory
# Allows testing with devtools::test on osx and linux
if (Sys.getenv("DEVTOOLS_LOAD") == "true" & .Platform$OS.type == "unix") {
  print("setting sql folder symobolic link")
  packageRoot <- normalizePath(system.file("..", package = "SelfControlledCohort"))
  # Create symbolic link so code can be used in devtools::test()
  linkPath <- file.path(packageRoot, "sql")
  if (!file.exists(linkPath)) {
    R.utils::createLink(link = linkPath, system.file("sql", package = "SelfControlledCohort"))
    options("use.devtools.sql_shim" = TRUE)
    withr::defer(unlink(linkPath), testthat::teardown_env())
  }
}

checkManifestFiles <- function(rootDir) {
  checkmate::expect_file_exists(file.path(rootDir, "manifest.json"))
  manifest <- jsonlite::read_json(file.path(rootDir, "manifest.json"))

  for (file in manifest$files) {
    checkmate::assertFileExists(file.path(rootDir, file))
  }
}

getTestDatabaseSchema <- function(dbms = getOption("dbms", default = "sqlite")) {
  switch(dbms,
         sqlite = "main",
         duckdb = "main",
         postgresql = Sys.getenv("CDM5_POSTGRESQL_CDM_SCHEMA"),
         redshift = Sys.getenv("CDM5_REDSHIFT_CDM_SCHEMA"),
         `sql server` = Sys.getenv("CDM5_SQL_SERVER_CDM_SCHEMA"),
         spark = Sys.getenv("CDM5_SPARK_CDM_SCHEMA"))
}


getTestDatabaseConnectionDetails <- function(dbms = getOption("dbms", default = "sqlite"), jdbcDriverFolder) {
  if (dbms == "sqlite") {
    datasetName <- "GiBleed"
    dbFile <- tempfile(fileext = paste0(datasetName, ".sqlite"))
    Eunomia::getDatabaseFile(datasetName,
                             cdmVersion = "5.3",
                             dbms = "sqlite",
                             databaseFile = dbFile,
                             inputFormat = "csv",
                             verbose = FALSE,
                             overwrite = FALSE)

    connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = "sqlite", server = dbFile)
    Eunomia::createCohorts(connectionDetails)
  }
  if (dbms == "postgresql") {
    DatabaseConnector::downloadJdbcDrivers("postgresql", pathToDriver = jdbcDriverFolder)
    connectionDetails <- createConnectionDetails(dbms = "postgresql",
                                                 user = Sys.getenv("CDM5_POSTGRESQL_USER"),
                                                 password = URLdecode(Sys.getenv("CDM5_POSTGRESQL_PASSWORD")),
                                                 server = Sys.getenv("CDM5_POSTGRESQL_SERVER"),
                                                 pathToDriver = jdbcDriverFolder)
  }
  if (dbms == "redshift") {
    DatabaseConnector::downloadJdbcDrivers("redshift", pathToDriver = jdbcDriverFolder)
    connectionDetails <- createConnectionDetails(dbms = "redshift",
                                                 user = Sys.getenv("CDM5_REDSHIFT_USER"),
                                                 password = URLdecode(Sys.getenv("CDM5_REDSHIFT_PASSWORD")),
                                                 server = Sys.getenv("CDM5_REDSHIFT_SERVER"),
                                                 pathToDriver = jdbcDriverFolder)
  }
  if (dbms == "sql server") {
    DatabaseConnector::downloadJdbcDrivers("sql server", pathToDriver = jdbcDriverFolder)
    connectionDetails <- createConnectionDetails(dbms = "sql server",
                                                 user = Sys.getenv("CDM5_SQL_SERVER_USER"),
                                                 password = URLdecode(Sys.getenv("CDM5_SQL_SERVER_PASSWORD")),
                                                 server = Sys.getenv("CDM5_SQL_SERVER_SERVER"),
                                                 pathToDriver = jdbcDriverFolder)
  }

  if (dbms == "spark") {
    DatabaseConnector::downloadJdbcDrivers("spark", pathToDriver = jdbcDriverFolder)
    connectionDetails <- createConnectionDetails(dbms = "spark",
                                                 user = Sys.getenv("CDM5_SPARK_USER"),
                                                 password = URLdecode(Sys.getenv("CDM5_SPARK_PASSWORD")),
                                                 connectionString = Sys.getenv("CDM5_SPARK_CONNECTION_STRING"),
                                                 pathToDriver = jdbcDriverFolder)
    options("sqlRenderTempEmulationSchema" = Sys.getenv("CDM5_SPARK_OHDSI_SCHEMA"))
  }

  return(connectionDetails)
}
