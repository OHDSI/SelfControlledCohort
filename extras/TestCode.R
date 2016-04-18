
library(SelfControlledCohort)

pw <- NULL
dbms <- "sql server"
user <- NULL
server <- "RNDUSRDHIT07.jnj.com"
cdmDatabaseSchema <- "cdm_truven_mdcd.dbo"
resultsDatabaseSchema <- "scratch.dbo"
port <- NULL

dbms <- "postgresql"
user <- "postgres"
pw <- "F1r3starter"
server <- "localhost/ohdsi"
cdmDatabaseSchema <- "cdm4_sim"
port <- NULL
cdmVersion <- 4

pw <- NULL
dbms <- "pdw"
user <- NULL
server <- "JRDUSAPSCTL01"
cdmDatabaseSchema <- "cdm_truven_mdcd.dbo"
resultsDatabaseSchema <- "scratch.dbo"
port <- 17001

connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = dbms,
                                                                server = server,
                                                                user = user,
                                                                password = pw,
                                                                port = port)


sccResult <- runSelfControlledCohort(connectionDetails,
                                     cdmDatabaseSchema = cdmDatabaseSchema,
                                     cdmVersion = cdmVersion,
                                     exposureIds = c(767410, 1314924, 907879),
                                     outcomeIds = c(444382, 192671),
                                     outcomeTable = "condition_era")

sccResult
summary(sccResult)

# Test multiple analyses

exposureOutcome1 <- createExposureOutcome(767410, 444382)
exposureOutcome2 <- createExposureOutcome(1314924, 444382)
exposureOutcome3 <- createExposureOutcome(907879, 444382)
exposureOutcomeList <- list(exposureOutcome1, exposureOutcome2, exposureOutcome3)
#saveExposureOutcomeList(exposureOutcomeList, "s:/temp/exposureOutcomeList.txt")


runSelfControlledCohortArgs1 <- createRunSelfControlledCohortArgs(firstOccurrenceDrugOnly = FALSE)
runSelfControlledCohortArgs2 <- createRunSelfControlledCohortArgs(firstOccurrenceDrugOnly = TRUE)
sccAnalysis1 <- createSccAnalysis(analysisId = 1,
                                  runSelfControlledCohortArgs = runSelfControlledCohortArgs1)
sccAnalysis2 <- createSccAnalysis(analysisId = 2,
                                  runSelfControlledCohortArgs = runSelfControlledCohortArgs2)
sccAnalysisList <- list(sccAnalysis1, sccAnalysis2)
#saveSccAnalysisList(sccAnalysisList, "s:/temp/sccAnalysisList.txt")

outputFolder <- "s:/temp/Scc"

pw <- NULL
dbms <- "sql server"
user <- NULL
server <- "RNDUSRDHIT07.jnj.com"
cdmDatabaseSchema <- "cdm_truven_mdcd.dbo"
resultsDatabaseSchema <- "scratch.dbo"
port <- NULL
cdmVersion <- 4

dbms <- "postgresql"
user <- "postgres"
pw <- "F1r3starter"
server <- "localhost/ohdsi"
cdmDatabaseSchema <- "cdm4_sim"
port <- NULL
cdmVersion <- 4

pw <- NULL
dbms <- "pdw"
user <- NULL
server <- "JRDUSAPSCTL01"
cdmDatabaseSchema <- "cdm_truven_mdcd.dbo"
resultsDatabaseSchema <- "scratch.dbo"
port <- 17001
cdmVersion <- 4

connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = dbms,
                                                                server = server,
                                                                user = user,
                                                                password = pw,
                                                                port = port)

rr <- runSccAnalyses(connectionDetails = connectionDetails,
                     cdmDatabaseSchema = cdmDatabaseSchema,
                     cdmVersion = cdmVersion,
                     sccAnalysisList = sccAnalysisList,
                     exposureOutcomeList = exposureOutcomeList,
                     outputFolder = outputFolder)

rr <- readRDS(file.path(outputFolder, "resultsReference.rds"))

res <- summarizeAnalyses(rr)








#### PRoblem


setwd('s:/temp')
options('fftempdir' = 's:/fftemp')

pw <- NULL
dbms <- "pdw"
user <- NULL
server <- "JRDUSAPSCTL01"
cdmDatabaseSchema <- "CDM_Truven_MDCD_V5.dbo"
oracleTempSchema <- NULL
outcomeDatabaseSchema <- "scratch.dbo"
outcomeTable <- "mschuemie_outcomes"
port <- 17001
cdmVersion <- "5"

connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = dbms,
                                                                server = server,
                                                                user = user,
                                                                password = pw,
                                                                port = port)

exposureId <- 1124300
outcomeId <- 1000


x <- SelfControlledCohort::runSelfControlledCohort(connectionDetails = connectionDetails,
                                                   cdmDatabaseSchema = cdmDatabaseSchema,
                                                   oracleTempSchema = oracleTempSchema,
                                                   exposureTable = "drug_era",
                                                   outcomeDatabaseSchema = outcomeDatabaseSchema,
                                                   outcomeTable = outcomeTable,
                                                   cdmVersion = cdmVersion,
                                                   exposureIds = exposureId,
                                                   outcomeIds = outcomeId,
                                                   firstOccurrenceDrugOnly = FALSE,
                                                   firstOccurrenceConditionOnly = FALSE,
                                                   useLengthOfExposureExposed = TRUE,
                                                   timeAtRiskExposedStart = 1,
                                                   surveillanceExposed = 0,
                                                   hasFullTimeAtRisk = FALSE,
                                                   timeAtRiskUnexposedStart = -1,
                                                   useLengthOfExposureUnexposed = TRUE,
                                                   surveillanceUnexposed = 0,
                                                   washoutWindow = 183,
                                                   followupWindow = 183)


summary(x)
x$sql
SqlRender::writeSql(x$sql, "s:/temp/sql.sql")




