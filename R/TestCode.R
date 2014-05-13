# Temporary placeholder for testing code until we figure out unit testing with DB and filesys dependencies
sccTestRoutines <- function(){
  #Test: create analysesDetails
  analysesDetails <- NULL
  analysesDetails <- appendToSccAnalysesDetails(createSccAnalysisDetails(analysisId = 1,firstOccurrenceDrugOnly=TRUE),analysesDetails)
  analysesDetails <- appendToSccAnalysesDetails(createSccAnalysisDetails(analysisId = 2,firstOccurrenceDrugOnly=FALSE),analysesDetails)
  
  #Test: write analysesDetails to file
  writeSccAnalysesDetailsToFile(analysesDetails,"c:/temp/test.csv")
  
  #Test: read analysisDetails from file
  analysesDetails2 <- readSccAnalysesDetailsFromFile("c:/temp/test.csv")
  
  #Test: check if two objects are the same
  min(as.character(analysesDetails) == as.character(analysesDetails2)) == 1
  
  #Test: create the connectDetails
  connectionDetails <- createConnectionDetails(dbms="sql server", server="RNDUSRDHIT07.jnj.com")
  connectionDetails <- createConnectionDetails(dbms="oracle",user="system",password="F1r3starter",server="xe")
  
  #Test: run the method:
  sccResults <- selfControlledCohort(analysesDetails, connectionDetails, cdmSchema="cdm4_sim", resultsSchema="scratch", createResultsTable = TRUE, sourceName = "cdm_truven_mdcr", exposuresOfInterest = c(767410,1314924,907879), outcomesOfInterest = c(444382, 79106, 138825), outcomeTable = "condition_era") 
  
  #Test: fetch summary data:
  s <- summary(sccResults)
  
  
  #Drop temp tables:
  conn <- connect(connectionDetails)
  dbSendUpdate(conn,"ALTER SESSION SET current_schema =  cdm4_sim")
  dbSendUpdate(conn,"DROP TABLE age_group")
  dbSendUpdate(conn,"DROP TABLE scc_exposure_summary")
  dbSendUpdate(conn,"DROP TABLE scc_outcome_summary")
  dbDisconnect(conn)
  
  #For translation testing: write rendered SQL to file:
  analysesDetails <- NULL
  analysesDetails <- appendToSccAnalysesDetails(createSccAnalysisDetails(analysisId = 1,firstOccurrenceDrugOnly=TRUE),analysesDetails)
  connectionDetails <- createConnectionDetails(dbms="sql server", server="RNDUSRDHIT07.jnj.com")
  sccResults <- selfControlledCohort(analysesDetails, connectionDetails, cdmSchema="cdm4_sim", resultsSchema="scratch", createResultsTable = TRUE, sourceName = "cdm4_sim", exposuresOfInterest = c(915981), outcomesOfInterest = c(437041), outcomeTable = "condition_era") 
  write.table(sccResults$sql,"c:/temp/scc.sql")
  
  #Test: sql validity test across parameters
  analysesDetails <- NULL
  analysisId = 1
  
  drugTypeConceptIdList = c(38000182)
  conditionTypeConceptIdList = c(38000247)
  genderConceptIdList = c(8507,8532)
  shrinkage = 0.0001
  for (firstOccurrenceDrugOnly in c(TRUE,FALSE))
    for (firstOccurrenceConditionOnly in c(TRUE,FALSE))
      for (minAge in c("","18")) 
        for (maxAge in c("", "65"))
          for (minIndex in c("","2000")) 
            for (maxIndex in c("","2010")) 
              for (stratifyGender in c(TRUE,FALSE))
                for (stratifyAge in c(TRUE,FALSE))
                  for (stratifyIndex in c(TRUE,FALSE))
                    for (useLengthOfExposureExposed in c(TRUE,FALSE))
                      for (timeAtRiskExposedStart in c(0,1))
                        for (surveillanceExposed in c(30,180,9999))
                          for (useLengthOfExposureUnexposed in c(TRUE,FALSE))
                            for (timeAtRiskUnexposedStart in c(-1,0))
                              for (surveillanceUnexposed in c(-30,-180,-9999))
                                for (hasFullTimeAtRisk in c(TRUE,FALSE))
                                  for (washoutPeriodLength in c(0,180))
                                    for (followupPeriodLength in c(0,180)){
                                      analysisDetails<- createSccAnalysisDetails(analysisId = analysisId,
                                                                                 firstOccurrenceDrugOnly = firstOccurrenceDrugOnly,
                                                                                 firstOccurrenceConditionOnly = firstOccurrenceConditionOnly,
                                                                                 drugTypeConceptIdList = drugTypeConceptIdList,
                                                                                 conditionTypeConceptIdList = conditionTypeConceptIdList,
                                                                                 genderConceptIdList = genderConceptIdList,
                                                                                 minAge = minAge,
                                                                                 maxAge = maxAge,
                                                                                 minIndex = minIndex,
                                                                                 maxIndex = maxIndex,
                                                                                 stratifyGender = stratifyGender,
                                                                                 stratifyAge = stratifyAge,
                                                                                 stratifyIndex = stratifyIndex,
                                                                                 useLengthOfExposureExposed = useLengthOfExposureExposed,
                                                                                 timeAtRiskExposedStart = timeAtRiskExposedStart,
                                                                                 surveillanceExposed = surveillanceExposed,
                                                                                 useLengthOfExposureUnexposed = useLengthOfExposureUnexposed,
                                                                                 timeAtRiskUnexposedStart = timeAtRiskUnexposedStart,
                                                                                 surveillanceUnexposed = surveillanceUnexposed,
                                                                                 hasFullTimeAtRisk = hasFullTimeAtRisk,
                                                                                 washoutPeriodLength = washoutPeriodLength,
                                                                                 followupPeriodLength = followupPeriodLength,
                                                                                 shrinkage = shrinkage                        
                                      )
                                      analysesDetails <- appendToSccAnalysesDetails(analysisDetails,analysesDetails)
                                      analysisId = analysisId + 1
                                    }
  
  writeSccAnalysesDetailsToFile(analysesDetails,"c:/temp/test.csv")
  
  
  sql <- as.character(read.table("c:/temp/preTranslate.sql")$x)
  
  sql <- translateSql(sql,"sql server","oracle")$sql
  write.table(sql,file="c:/temp/postTranslate.sql")
  
}