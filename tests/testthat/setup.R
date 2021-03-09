if (getOption("dbms") == "postgresql") {
  connectionDetails <- createConnectionDetails(dbms = "postgresql",
                                               user = Sys.getenv("CDM5_POSTGRESQL_USER"),
                                               password = URLdecode(Sys.getenv("CDM5_POSTGRESQL_PASSWORD")),
                                               server = Sys.getenv("CDM5_POSTGRESQL_SERVER"))

  cdmDatabaseSchema <- Sys.getenv("CDM5_POSTGRESQL_CDM_SCHEMA")
  cdmVersion <- 5
}
if (getOption("dbms") == "sql server") {
  connectionDetails <- createConnectionDetails(dbms = "sql server",
                                               user = Sys.getenv("CDM5_SQL_SERVER_USER"),
                                               password = URLdecode(Sys.getenv("CDM5_SQL_SERVER_PASSWORD")),
                                               server = Sys.getenv("CDM5_SQL_SERVER_SERVER"))
  cdmDatabaseSchema <- Sys.getenv("CDM5_SQL_SERVER_CDM_SCHEMA")
  cdmVersion <- 5
}
if (getOption("dbms") == "oracle") {
  connectionDetails <- createConnectionDetails(dbms = "oracle",
                                               user = Sys.getenv("CDM5_ORACLE_USER"),
                                               password = URLdecode(Sys.getenv("CDM5_ORACLE_PASSWORD")),
                                               server = Sys.getenv("CDM5_ORACLE_SERVER"))
  cdmDatabaseSchema <- Sys.getenv("CDM5_ORACLE_CDM_SCHEMA")
  oracleTempSchema <- Sys.getenv("CDM5_ORACLE_OHDSI_SCHEMA")

  cdmVersion <- 5
}