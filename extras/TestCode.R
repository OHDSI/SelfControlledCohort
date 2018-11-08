library(SelfControlledCohort)
options(fftempdir = "c:/fftemp")


pw <- NULL
dbms <- "pdw"
user <- NULL
cdmDatabaseSchema <- "cdm_truven_mdcd_v780.dbo"
cohortDatabaseSchema <- "scratch.dbo"
oracleTempSchema <- NULL
cohortTable <- "mschuemi_cc_vignette"
server <- Sys.getenv("PDW_SERVER")
port <- Sys.getenv("PDW_PORT")
outputFolder <- "c:/temp/scc"
oracleTempSchema <- "5"

connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = dbms,
                                                                server = server,
                                                                user = user,
                                                                password = pw,
                                                                port = port)


sccResult <- runSelfControlledCohort(connectionDetails,
                                     cdmDatabaseSchema = cdmDatabaseSchema,
                                     oracleTempSchema = oracleTempSchema,
                                     cdmVersion = cdmVersion,
                                     exposureIds = c(767410, 1713332, 907879),
                                     outcomeIds = c(444382, 192671),
                                     outcomeTable = "condition_era")

sccResult
summary(sccResult)


# Test multiple analyses

exposureOutcome1 <- createExposureOutcome(767410, 192671)
exposureOutcome2 <- createExposureOutcome(1314924, 192671)
exposureOutcome3 <- createExposureOutcome(907879, 192671)
exposureOutcomeList <- list(exposureOutcome1, exposureOutcome2, exposureOutcome3)
# saveExposureOutcomeList(exposureOutcomeList, 's:/temp/exposureOutcomeList.txt')


runSelfControlledCohortArgs1 <- createRunSelfControlledCohortArgs(firstExposureOnly = FALSE)
runSelfControlledCohortArgs2 <- createRunSelfControlledCohortArgs(firstExposureOnly = TRUE)
sccAnalysis1 <- createSccAnalysis(analysisId = 1,
                                  runSelfControlledCohortArgs = runSelfControlledCohortArgs1)
sccAnalysis2 <- createSccAnalysis(analysisId = 2,
                                  runSelfControlledCohortArgs = runSelfControlledCohortArgs2)
sccAnalysisList <- list(sccAnalysis1, sccAnalysis2)
# saveSccAnalysisList(sccAnalysisList, 's:/temp/sccAnalysisList.txt')

outputFolder <- "c:/temp/Scc"

rr <- runSccAnalyses(connectionDetails = connectionDetails,
                     cdmDatabaseSchema = cdmDatabaseSchema,
                     sccAnalysisList = sccAnalysisList,
                     exposureOutcomeList = exposureOutcomeList,
                     outputFolder = outputFolder,
                     computeThreads = 2)

rr <- readRDS(file.path(outputFolder, "resultsReference.rds"))

res <- summarizeAnalyses(rr, outputFolder)





