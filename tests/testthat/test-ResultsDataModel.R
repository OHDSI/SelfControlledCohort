library(testthat)

# Create test results file
exposureOutcome1 <- createExposureOutcome(1, 3)
exposureOutcome2 <- createExposureOutcome(2, 3)
exposureOutcome3 <- createExposureOutcome(1, 4, trueEffectSize = 1)
exposureOutcomeList <- list(exposureOutcome1, exposureOutcome2, exposureOutcome3)

runSelfControlledCohortArgs1 <- createRunSelfControlledCohortArgs(firstExposureOnly = FALSE)
runSelfControlledCohortArgs2 <- createRunSelfControlledCohortArgs(firstExposureOnly = TRUE)
sccAnalysis1 <- createSccAnalysis(analysisId = 1,
                                  runSelfControlledCohortArgs = runSelfControlledCohortArgs1)
sccAnalysis2 <- createSccAnalysis(analysisId = 2,
                                  runSelfControlledCohortArgs = runSelfControlledCohortArgs2)
sccAnalysisList <- list(sccAnalysis1, sccAnalysis2)

resultsFolder <- tempfile()
dir.create(resultsFolder, showWarnings = FALSE)
withr::defer({
  unlink(resultsFolder, recursive = TRUE)
}, testthat::teardown_env())

runSccAnalyses(connectionDetails = connectionDetails,
               cdmDatabaseSchema = cdmDatabaseSchema,
               sccAnalysisList = sccAnalysisList,
               exposureOutcomeList = exposureOutcomeList,
               exposureTable = "cohort",
               outcomeDatabaseSchema = cdmDatabaseSchema,
               outcomeTable = "cohort",
               resultsFolder = resultsFolder,
               databaseId = "Eunomia",
               computeThreads = 1)


test_that("model spec conforms to standard", {

  df <- getResultsDataModelSpecifications()
  # Check that the necessary columns are present
  expected_columns <- c("tableName", "columnName", "dataType", "isRequired",
                        "primaryKey", "minCellCount", "description")

  expect_true(all(expected_columns %in% colnames(df)),
              info = "Not all expected columns are present.")
  # Check if the dataframe is not empty
  expect_false(nrow(df) == 0, info = "The dataframe is empty.")
})


if (dir.exists(Sys.getenv("DATABASECONNECTOR_JAR_FOLDER"))) {
  jdbcDriverFolder <- Sys.getenv("DATABASECONNECTOR_JAR_FOLDER")
} else {
  jdbcDriverFolder <- "~/.jdbcDrivers"
  dir.create(jdbcDriverFolder, showWarnings = FALSE)
  DatabaseConnector::downloadJdbcDrivers("postgresql", pathToDriver = jdbcDriverFolder)
  withr::defer(
  {
    unlink(jdbcDriverFolder, recursive = TRUE, force = TRUE)
  },
    testthat::teardown_env()
  )
}

postgresConnectionDetails <- DatabaseConnector::createConnectionDetails(
  dbms = "postgresql",
  user = Sys.getenv("CDM5_POSTGRESQL_USER"),
  password = URLdecode(Sys.getenv("CDM5_POSTGRESQL_PASSWORD")),
  server = Sys.getenv("CDM5_POSTGRESQL_SERVER"),
  pathToDriver = jdbcDriverFolder
)

postgresResultsDatabaseSchema <- paste0("r", Sys.getpid(), format(Sys.time(), "%s"), sample(1:100, 1))

databaseFile <- tempfile(fileext = ".sqlite")
sqliteConnectionDetails <- DatabaseConnector::createConnectionDetails(
  dbms = "sqlite",
  server = databaseFile
)
sqliteResultsDatabaseSchema <- "main"

withr::defer({
  connection <- DatabaseConnector::connect(connectionDetails = postgresConnectionDetails)
  sql <- "DROP SCHEMA IF EXISTS @resultsDatabaseSchema CASCADE;"
  DatabaseConnector::renderTranslateExecuteSql(
    sql = sql,
    resultsDatabaseSchema = postgresResultsDatabaseSchema,
    connection = connection
  )

  DatabaseConnector::disconnect(connection)
  unlink(databaseFile, force = TRUE)
},
  testthat::teardown_env()
)

testCreateSchema <- function(connectionDetails, resultsDatabaseSchema) {
  connection <- DatabaseConnector::connect(connectionDetails)
  on.exit(DatabaseConnector::disconnect(connection))
  if (connectionDetails$dbms != "sqlite") {
    sql <- "CREATE SCHEMA @resultsDatabaseSchema;"
    DatabaseConnector::renderTranslateExecuteSql(
      sql = sql,
      resultsDatabaseSchema = resultsDatabaseSchema,
      connection = connection
    )
  }
  suppressWarnings(
    createResultsDataModel(
      connectionDetails = connectionDetails,
      databaseSchema = resultsDatabaseSchema,
      tablePrefix = ""
    )
  )
  specifications <- getResultsDataModelSpecifications()
  for (tableName in unique(specifications$tableName)) {
    expect_true(DatabaseConnector::existsTable(connection = connection,
                                               databaseSchema = resultsDatabaseSchema,
                                               tableName = tableName))
  }
  # Bad schema name
  expect_error(createResultsDataModel(
    connectionDetails = connectionDetails,
    databaseSchema = "non_existant_schema"
  ))
}

test_that("Create schema", {
  testCreateSchema(connectionDetails = postgresConnectionDetails,
                   resultsDatabaseSchema = postgresResultsDatabaseSchema)
  testCreateSchema(connectionDetails = sqliteConnectionDetails,
                   resultsDatabaseSchema = sqliteResultsDatabaseSchema)
})


test_that("Results upload", {
  uploadResults(
    connectionDetails = sqliteConnectionDetails,
    schema = "main",
    resultsFolder = resultsFolder,
    purgeSiteDataBeforeUploading = FALSE
  )
  expect_true(TRUE)
})

test_that("Results upload postgers", {
  skip_if(Sys.getenv("CDM5_POSTGRESQL_SERVER") == "")
  uploadResults(
    connectionDetails = postgresConnectionDetails,
    schema = postgresResultsDatabaseSchema,
    resultsFolder = resultsFolder,
    purgeSiteDataBeforeUploading = FALSE
  )
  expect_true(TRUE)
})
