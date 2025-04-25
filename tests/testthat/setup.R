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
connectionDetails <- getTestDatabaseConnectionDetails(dbms = dbms, jdbcDriverFolder = jdbcDriverFolder)
cdmDatabaseSchema <- getTestDatabaseSchema(dbms = dbms)