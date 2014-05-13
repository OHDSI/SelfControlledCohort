# @file SelfControlledCohort.R
#
# Copyright 2014 Observational Health Data Sciences and Informatics
#
# This file is part of SelfControlledCohort
# 
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
# 
#     http://www.apache.org/licenses/LICENSE-2.0
# 
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#
# @author Observational Health Data Sciences and Informatics
# @author Martijn Schuemie
# @author Patrick Ryan

#' @title selfControlledCohort
#'
#' @description
#' \code{selfControlledCohort} generates population-level estimation from OMOP CDMv4 instance by comparing exposed and unexposed time among exposed cohort.
#'
#' @usage 
#' selfControlledCohort(connectionDetails, cdmSchema, resultsSchema, resultsTablePrefix = "scc", createResultsTable = TRUE, sourceName = "", exposuresOfInterest = c(), outcomesOfInterest = c(), exposureTable = "drug_era", outcomeTable = "condition_era", analysisId = 1, firstOccurrenceDrugOnly = TRUE, firstOccurrenceConditionOnly = TRUE, drugTypeConceptIdList = c(38000182), conditionTypeConceptIdList = c(38000247), genderConceptIdList = c(8507,8532), minAge = "", maxAge = "", minIndex = "", maxIndex = "", stratifyGender = FALSE, stratifyAge = FALSE, stratifyIndex = FALSE, useLengthOfExposureExposed = TRUE, timeAtRiskExposedStart = 1, surveillanceExposed = 30, useLengthOfExposureUnexposed = TRUE, timeAtRiskUnexposedStart = -1, surveillanceUnexposed = -30, hasFullTimeAtRisk = FALSE, washoutPeriodLength = 0, followupPeriodLength = 0, shrinkage = 0.0001)
#' selfControlledCohort(sccAnalysisDetails, connectionDetails, cdmSchema, resultsSchema, resultsTablePrefix = "scc", createResultsTable = TRUE, sourceName = "", exposuresOfInterest = c(), outcomesOfInterest = c(), exposureTable = "drug_era", outcomeTable = "condition_era")
#' selfControlledCohort(sccAnalysesDetails, connectionDetails, cdmSchema, resultsSchema, resultsTablePrefix = "scc", createResultsTable = TRUE, sourceName = "", exposuresOfInterest = c(), outcomesOfInterest = c(), exposureTable = "drug_era", outcomeTable = "condition_era")
#'
#' @details
#' PATRICK HOMEWORK:   complete details
#'  
#' @param connectionDetails  An R object of type ConnectionDetail (details for the function that contains server info, database type, optionally username/password, port)
#' @param cdmSchema  		string name of databsae schema that contains OMOP CDM and vocabulary
#' @param resultsSchema		string name of database schema that we can write results to
#' @param resultsTable  name of the table in the \code{resultsSchema} where the results will be written 
#' @param createResultsTable if true, a new empty table will be created to store the results. If false, results will be inserted into the existing table.
#' @param sourceName		string name of the database, as recorded in results
#' @param exposuresOfInterest  list of DRUG_CONCEPT_IDs to study, if NULL, then all DRUG_CONCEPT_IDs will be used
#' @param outcomesOfInterest	list of CONDITION_CONCET_IDs to study, if NULL, all CONDITIONS considered as potential outcomes
#' @param exposureTable	drugEra or cohort
#' @param outcomeTable	conditionEra or cohort
#' @param analysisId  	A unique identifier that can later be used to identify the results of this analysis
#' @param firstOccurrenceDrugOnly	if TRUE, only use first occurrence of each drug concept id for each person
#' @param firstOccurrenceConditionOnly	if TRUE, only use first occurrence of each condition concept id for each person
#' @param drugTypeConceptIdList	which drug_type to use:  generally only use 1 value (ex:  30d era)
#' @param conditionTypeConceptIdList	which condition_type to use:  generally only use 1 value (ex:  30d era)
#' @param genderConceptIdList	list of gender_concept_id, generally use MALE (8507) and FEMALE (8532)
#' @param minAge	integer for minimum allowable age
#' @param maxAge	integer for maximum allowable age
#' @param minIndex	date for minimum allowable data for index exposure
#' @param maxIndex	date for maximum allowable data for index exposure
#' @param stratifyGender	if TRUE, analysis will be calculated overall, and stratified across all gender groups
#' @param stratifyAge	if TRUE, analysis will be calculated overall, and stratified across all age groups  (using AGE_GROUP table below)
#' @param stratifyIndex	if TRUE, analysis will be calculated overall, and stratified across all years of the index dates
#' @param useLengthOfExposureExposed	if TRUE, use the duration from drugEraStart -> drugEraEnd as part of timeAtRisk
#' @param timeAtRiskExposedStart	integer of days to add to drugEraStart for start of timeAtRisk (0 to include index date, 1 to start the day after)
#' @param surveillanceExposed	additional window to add to end of exposure period (if useLengthOfExposureExposed = TRUE, then add to exposure end date, else add to exposure start date)
#' @param useLengthOfExposureUnexposed	if TRUE, use the duration from exposure start -> exposure end as part of timeAtRisk looking back before exposure start
#' @param timeAtRiskUnexposedStart	integer of days to add to exposure start for start of timeAtRisk (0 to include index date, -1 to start the day before)
#' @param surveillanceUnexposed	additional window to add to end of exposure period (if useLengthOfExposureUnexposed = TRUE, then add to exposure end date, else add to exposure start date)
#' @param hasFullTimeAtRisk	if TRUE, restrict to people who have full time-at-risk exposed and unexposed
#' @param washoutPeriodLength	integer to define required time observed before exposure start
#' @param followupPeriodLength	integer to define required time observed after exposure start
#' @param shrinkage	shrinkage used in IRR calculations, required >0 to deal with 0 case counts, but larger number means more shrinkage
#' @param sccAnalysisDetails  object specifyng a set of analysis choices
#' @param sccAnalysesDetails  object specifyng one or serveral sets of analysis choices
#' 
#' @return An object of type \code{sccResults} containing details for connecting to the database containing the results 
#' @examples \dontrun{
#'   connectionDetails <- createConnectionDetails(dbms="sql server", server="RNDUSRDHIT07.jnj.com")
#'   sccResult <- selfControlledCohort(connectionDetails, "cdm_truven_mdcr", "scratch", sourceName = "cdm_truven_mdcr", exposuresOfInterest = c(767410,1314924,907879), outcomesOfInterest = c(444382, 79106, 138825), outcomeTable = "condition_era")
#'   plot(sccResult)
#' }
#' @export
selfControlledCohort <- function(...){
  UseMethod("selfControlledCohort") 
}

#' @export
selfControlledCohort.connectionDetails <- function (connectionDetails, 
                                                    cdmSchema, 
                                                    resultsSchema, 
                                                    resultsTablePrefix = "scc", 
                                                    createResultsTable = TRUE, 
                                                    sourceName = "", 
                                                    exposuresOfInterest = c(), 
                                                    outcomesOfInterest = c(), 
                                                    exposureTable = "drug_era",
                                                    outcomeTable = "condition_era",
                                                    analysisId = 1,
                                                    firstOccurrenceDrugOnly = TRUE,
                                                    firstOccurrenceConditionOnly = TRUE,
                                                    drugTypeConceptIdList = c(38000182),
                                                    conditionTypeConceptIdList = c(38000247),
                                                    genderConceptIdList = c(8507,8532),
                                                    minAge = "",
                                                    maxAge = "",
                                                    minIndex = "",
                                                    maxIndex = "",
                                                    stratifyGender = FALSE,
                                                    stratifyAge = FALSE,
                                                    stratifyIndex = FALSE,
                                                    useLengthOfExposureExposed = TRUE,
                                                    timeAtRiskExposedStart = 1,
                                                    surveillanceExposed = 30,
                                                    useLengthOfExposureUnexposed = TRUE,
                                                    timeAtRiskUnexposedStart = -1,
                                                    surveillanceUnexposed = -30,
                                                    hasFullTimeAtRisk = FALSE,
                                                    washoutPeriodLength = 0,
                                                    followupPeriodLength = 0,
                                                    shrinkage = 0.0001){
  if (exposureTable == "cohort"){
    exposureStartDate = "cohort_start_date";
    exposureEndDate = "cohort_end_date";
    exposureConceptId = "cohort_concept_id";
    exposurePersonId=  "subject_id"
  } else if (exposureTable == "drug_era"){
    exposureStartDate = "drug_era_start_date";
    exposureEndDate = "drug_era_end_date";
    exposureConceptId = "drug_concept_id";
    exposurePersonId=  "person_id"
  }
  if (outcomeTable == "cohort"){
    outcomeStartDate = "cohort_start_date";
    outcomeEndDate = "cohort_end_date";
    outcomeConceptId = "cohort_concept_id";
    outcomePersonId=  "subject_id"
  } else if (outcomeTable == "condition_era"){
    outcomeStartDate = "condition_era_start_date";
    outcomeEndDate = "condition_era_end_date";
    outcomeConceptId = "condition_concept_id";
    outcomePersonId=  "person_id"
  }
  
  pathToSql <- system.file(paste("sql/",connectionDetails$dbms,sep=""), "SccParameterizedSQL.sql", package="SelfControlledCohort")
  mustTranslate <- !file.exists(pathToSql)
  if (mustTranslate) # If DBMS-specific code does not exists, load SQL Server code and translate after rendering
    pathToSql <- system.file(paste("sql/","sql server",sep=""), "SccParameterizedSQL.sql", package="SelfControlledCohort")      
  parameterizedSql <- readChar(pathToSql,file.info(pathToSql)$size)  
  
  renderedSql <- renderSql(parameterizedSql[1], 
                           cdmSchema = cdmSchema, 
                           resultsSchema = resultsSchema, 
                           resultsTablePrefix = resultsTablePrefix, 
                           createResultsTable = createResultsTable,
                           sourceName = sourceName,
                           analysisId = analysisId,
                           exposuresOfInterest = exposuresOfInterest, 
                           outcomesOfInterest = outcomesOfInterest, 
                           exposureTable = exposureTable,
                           exposureStartDate = exposureStartDate,
                           exposureEndDate = exposureEndDate,
                           exposureConceptId = exposureConceptId,
                           exposurePersonId = exposurePersonId,
                           outcomeTable = outcomeTable,
                           outcomeStartDate = outcomeStartDate,
                           outcomeEndDate = outcomeEndDate,
                           outcomeConceptId = outcomeConceptId,
                           outcomePersonId = outcomePersonId,
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
  )$sql
  
  write.table(renderedSql,file="c:/temp/preTranslate.sql")
  
  if (mustTranslate)
    renderedSql <- translateSql(renderedSql, "sql server", connectionDetails$dbms)$sql
  
  write.table(renderedSql,file="c:/temp/postTranslate.sql")   
    
  #Check if connection already open:
  if (is.null(connectionDetails$conn)){
    conn <- connect(connectionDetails)
  } else {
    conn <- connectionDetails$conn
  }
  
  writeLines(paste("Executing analysis (analysisId = ",analysisId,"). This could take a while",sep=""))
  i <- 1
  for (sqlStatement in splitSql(renderedSql)){
    write.table(sqlStatement,file=paste("c:/temp/sql_",i,".sql",sep=""))
    i = i + 1
    dbSendUpdate(conn, sqlStatement)
  }
  writeLines(paste("Finished analysis (analysisId = ",analysisId,"). Results can now be found in ",resultsSchema,".",resultsTablePrefix,"_results, analyses documented in ",
                   resultsSchema,".",resultsTablePrefix,"_analysis",sep=""))
    
  resultsConnectionDetails <- connectionDetails
  resultsConnectionDetails$schema = resultsSchema
  resultsConnectionDetails$conn <- NULL
    
  result <- list(resultsConnectionDetails = resultsConnectionDetails, 
                 resultsTable = paste(resultsTablePrefix,"_results",sep=""),
                 analysisTable = paste(resultsTablePrefix,"_analysis",sep=""),
                 sourceName = sourceName,
                 analysisIds = analysisId,
                 exposuresOfInterest = exposuresOfInterest,
                 outcomesOfInterest = outcomesOfInterest,
                 sql = renderedSql,
                 call = match.call())
  
  class(result) <- "sccResults"
  
  if (is.null(connectionDetails$conn)){
    dbDisconnect(conn)
    result <- addResults(result)
  }

  result
}

addResults <- function(results) {
  conn <- connect(results$resultsConnectionDetails)
  sql <- "SELECT * FROM @table WHERE sourceName = '@sourceName' AND analysisId IN (@analysisIds) AND exposureConceptId IN (@exposureConceptIds) AND outcomeConceptId IN (@outcomeConceptIds)"
  sql <- renderSql(sql,
                   table = results$resultsTable, 
                   sourceName = results$sourceName,
                   analysisIds = results$analysisIds, 
                   exposureConceptIds = results$exposuresOfInterest,
                   outcomeConceptIds = results$outcomesOfInterest
  )$sql
  results$effectEstimates <- dbGetQuery(conn,sql)
  sql <- "SELECT * FROM @table WHERE analysisId IN (@analysisIds)" 
  sql <- renderSql(sql,
                   table = results$analysisTable, 
                   analysisIds = results$analysisIds
  )$sql
  results$analyses <- dbGetQuery(conn,sql)
  dbDisconnect(conn)
  results
}

#' @export
selfControlledCohort.sccAnalysisDetails <- function(sccAnalysisDetails, 
                                                    connectionDetails, 
                                                    cdmSchema, 
                                                    resultsSchema, 
                                                    resultsTablePrefix = "scc", 
                                                    createResultsTable = TRUE, 
                                                    sourceName = "", 
                                                    exposuresOfInterest = c(), 
                                                    outcomesOfInterest = c(), 
                                                    exposureTable = "drug_era",
                                                    outcomeTable = "cohort"){
  arguments = sccAnalysisDetails
  arguments$connectionDetails = connectionDetails
  arguments$cdmSchema = cdmSchema
  arguments$resultsSchema = resultsSchema
  arguments$resultsTablePrefix = resultsTablePrefix
  arguments$createResultsTable = createResultsTable
  arguments$sourceName = sourceName
  arguments$exposuresOfInterest = exposuresOfInterest
  arguments$outcomesOfInterest = outcomesOfInterest
  arguments$exposureTable = exposureTable
  arguments$outcomeTable = outcomeTable												
  
  do.call("selfControlledCohort.connectionDetails",arguments)										
}

#' @export
selfControlledCohort.sccAnalysesDetails <- function(sccAnalysesDetails, 
                                                    connectionDetails, 
                                                    cdmSchema, 
                                                    resultsSchema, 
                                                    resultsTablePrefix = "scc", 
                                                    createResultsTable = TRUE, 
                                                    sourceName = "", 
                                                    exposuresOfInterest = c(), 
                                                    outcomesOfInterest = c(), 
                                                    exposureTable = "drug_era",
                                                    outcomeTable = "condition_era"
){
  connectionDetails$conn <- connect(connectionDetails)
  
  sql <- c()  
  analysisIds <- c()
  for (i in 1:length(sccAnalysesDetails)) {
    sccResults <- selfControlledCohort.sccAnalysisDetails(sccAnalysesDetails[[i]], 
                                                          connectionDetails = connectionDetails, 
                                                          cdmSchema = cdmSchema, 
                                                          resultsSchema = resultsSchema, 
                                                          resultsTablePrefix = resultsTablePrefix, 
                                                          createResultsTable = createResultsTable, 
                                                          sourceName = sourceName, 
                                                          exposuresOfInterest = exposuresOfInterest, 
                                                          outcomesOfInterest = outcomesOfInterest, 
                                                          exposureTable = exposureTable,
                                                          outcomeTable = outcomeTable)
    sql <- c(sql,sccResults$sql)
    analysisIds <- c(analysisIds,sccResults$analysisIds)
    createResultsTable = FALSE # no point in overwriting results of previous analysis in same run
  }
  dbDisconnect(connectionDetails$conn)
  connectionDetails$conn <- NULL
  
  sccResults$sql <- sql
  sccResults$analysisIds <- analysisIds
  sccResults <- addResults(sccResults)
  
  sccResults
}

#' @title createSccAnalysisDetails
#'
#' @description
#' \code{createSccAnalysisDetails} generates an object specifying one set of analysis choices
#' for the self-controlled cohort method.
#'  
#' @param analysisId		A unique identifier that can later be used to identify the results of this analysis
#' @param firstOccurrenceDrugOnly	if TRUE, only use first occurrence of each drug concept id for each person
#' @param firstOccurrenceConditionOnly	if TRUE, only use first occurrence of each condition concept id for each person
#' @param drugTypeConceptIdList	which drug_type to use:  generally only use 1 value (ex:  30d era)
#' @param conditionTypeConceptIdList	which condition_type to use:  generally only use 1 value (ex:  30d era)
#' @param genderConceptIdList	list of gender_concept_id, generally use MALE (8507) and FEMALE (8532)
#' @param minAge	integer for minimum allowable age
#' @param maxAge	integer for maximum allowable age
#' @param minIndex	date for minimum allowable data for index exposure
#' @param maxIndex	date for maximum allowable data for index exposure
#' @param stratifyGender	if TRUE, analysis will be calculated overall, and stratified across all gender groups
#' @param stratifyAge	if TRUE, analysis will be calculated overall, and stratified across all age groups  (using AGE_GROUP table below)
#' @param stratifyIndex	if TRUE, analysis will be calculated overall, and stratified across all years of the index dates
#' @param useLengthOfExposureExposed	if TRUE, use the duration from drugEraStart -> drugEraEnd as part of timeAtRisk
#' @param timeAtRiskExposedStart	integer of days to add to drugEraStart for start of timeAtRisk (0 to include index date, 1 to start the day after)
#' @param surveillanceExposed	additional window to add to end of exposure period (if useLengthOfExposureExposed = TRUE, then add to exposure end date, else add to exposure start date)
#' @param useLengthOfExposureUnexposed	if TRUE, use the duration from exposure start -> exposure end as part of timeAtRisk looking back before exposure start
#' @param timeAtRiskUnexposedStart	integer of days to add to exposure start for start of timeAtRisk (0 to include index date, -1 to start the day before)
#' @param surveillanceUnexposed	additional window to add to end of exposure period (if useLengthOfExposureUnexposed = TRUE, then add to exposure end date, else add to exposure start date)
#' @param hasFullTimeAtRisk	if TRUE, restrict to people who have full time-at-risk exposed and unexposed
#' @param washoutPeriodLength	integer to define required time observed before exposure start
#' @param followupPeriodLength	integer to define required time observed after exposure start
#' @param shrinkage	shrinkage used in IRR calculations, required >0 to deal with 0 case counts, but larger number means more shrinkage
#' 
#' @return An object of type \code{sccAnalysisDetails} containing the details of the analysis to be run
#' @examples \dontrun{
#'   connectionDetails <- createConnectionDetails(dbms="sql server", server="RNDUSRDHIT07.jnj.com")
#'   analysesDetails <- NULL
#'   analysesDetails <- appendToSccAnalysesDetails(createSccAnalysisDetails(analysisId = 1,firstOccurrenceDrugOnly=TRUE),analysesDetails)
#'   analysesDetails <- appendToSccAnalysesDetails(createSccAnalysisDetails(analysisId = 2,firstOccurrenceDrugOnly=FALSE),analysesDetails)
#'   sccResult <- selfControlledCohort(analysesDetails, connectionDetails, "cdm_truven_mdcr", "scratch", sourceName = "cdm_truven_mdcr", exposuresOfInterest = c(767410,1314924,907879), outcomesOfInterest = c(444382, 79106, 138825), outcomeTable = "condition_era")
#'   plot(sccResult)
#' }
#' @export
createSccAnalysisDetails <- function (analysisId = 1,
                                      firstOccurrenceDrugOnly = TRUE,
                                      firstOccurrenceConditionOnly = TRUE,
                                      drugTypeConceptIdList = c(38000182),
                                      conditionTypeConceptIdList = c(38000247),
                                      genderConceptIdList = c(8507,8532),
                                      minAge = "",
                                      maxAge = "",
                                      minIndex = "",
                                      maxIndex = "",
                                      stratifyGender = FALSE,
                                      stratifyAge = FALSE,
                                      stratifyIndex = FALSE,
                                      useLengthOfExposureExposed = TRUE,
                                      timeAtRiskExposedStart = 1,
                                      surveillanceExposed = 30,
                                      useLengthOfExposureUnexposed = TRUE,
                                      timeAtRiskUnexposedStart = -1,
                                      surveillanceUnexposed = -30,
                                      hasFullTimeAtRisk = FALSE,
                                      washoutPeriodLength = 0,
                                      followupPeriodLength = 0,
                                      shrinkage = 0.0001){
  #First: get the default values:
  analysisDetails <- list()
  for (name in names(formals(createSccAnalysisDetails))){
    analysisDetails[[name]] = get(name)
  }
  
  #Next: overwrite defaults with actual values if specified:
  values <- as.list(match.call())
  for (name in names(values)){
    if (name %in% names(analysisDetails))
      analysisDetails[[name]] = values[[name]]
  }
  
  class(analysisDetails) <- "sccAnalysisDetails"
  analysisDetails
}

#' @title appendToSccAnalysesDetails
#'
#' @description
#' \code{appendToSccAnalysesDetails} adds an object of type \code{analysisDetails} to an object of type \code{analysesDetails}. 
#'  
#' @param sccAnalysisDetails  	the \code{sccAnalysisDetails} to be added to the \code{sccAnalysesDetails} object
#' @param sccAnalysesDetails    object to append the \code{sccAnalysisDetails} to. If not specified, an new \code{sccAnalysesDetails} will be created
#' @return An object of type \code{sccAnalysesDetails} containing the details of one or several analyses to be run
#' @examples \dontrun{
#'   connectionDetails <- createConnectionDetails(dbms="sql server", server="RNDUSRDHIT07.jnj.com")
#'   analysesDetails <- NULL
#'   analysesDetails <- appendToSccAnalysesDetails(createSccAnalysisDetails(analysisId = 1,firstOccurrenceDrugOnly=TRUE),analysesDetails)
#'   analysesDetails <- appendToSccAnalysesDetails(createSccAnalysisDetails(analysisId = 2,firstOccurrenceDrugOnly=FALSE),analysesDetails)
#'   sccResult <- selfControlledCohort(analysesDetails, connectionDetails, "cdm_truven_mdcr", "scratch", sourceName = "cdm_truven_mdcr", exposuresOfInterest = c(767410,1314924,907879), outcomesOfInterest = c(444382, 79106, 138825), outcomeTable = "condition_era")
#'   plot(sccResult)
#' }
#' @export
appendToSccAnalysesDetails <- function(sccAnalysisDetails,sccAnalysesDetails = NULL){
  stopifnot(class(sccAnalysisDetails) == "sccAnalysisDetails")
  if (is.null(sccAnalysesDetails)){
    sccAnalysesDetails = list()
    class(sccAnalysesDetails) <- "sccAnalysesDetails"
  }
  sccAnalysesDetails[[length(sccAnalysesDetails)+1]] <- sccAnalysisDetails
  sccAnalysesDetails
}

#' @title writeSccAnalysesDetailsToFile
#'
#' @description
#' \code{writeSccAnalysesDetailsToFile} writes an object of type \code{analysesDetails} to a CSV file
#'  
#' @param sccAnalysesDetails    the \code{sccAnalysesDetails} to be written to file
#' @param file                  the name of the file where the results will be written
#' @examples \dontrun{
#'   analysesDetails <- NULL
#'   analysesDetails <- appendToSccAnalysesDetails(createSccAnalysisDetails(analysisId = 1,firstOccurrenceDrugOnly=TRUE),analysesDetails)
#'   analysesDetails <- appendToSccAnalysesDetails(createSccAnalysisDetails(analysisId = 2,firstOccurrenceDrugOnly=FALSE),analysesDetails)
#'   writeSccAnalysesDetailsToFile(analysesDetails,"c:/temp/test.csv")
#' }
#' @export
writeSccAnalysesDetailsToFile <- function(sccAnalysesDetails, file){
  stopifnot(class(sccAnalysesDetails) == "sccAnalysesDetails")
  
  #Convert sccAnalysesDetails to a data.frame, converting any nested vectors into pipe-delimited strings:
  f <- sccAnalysesDetails
  d <- data.frame()
  for (row in 1:length(f)){
    class(f[[row]]) <- "list"
    for (column in 1:length(f[[row]])){
      if ((class(f[[row]][[column]]) == "numeric") && (length(f[[row]][[column]]) > 1))
        f[[row]][[column]] = paste(f[[row]][[column]],collapse=",")
    }
    d <- rbind(d,as.data.frame(f[[row]]))
  }
  
  write.csv(d,file=file, row.names=FALSE)
}


#' @title readSccAnalysesDetailsFromFile
#'
#' @description
#' \code{readSccAnalysesDetailsFromFile} reads an object of type \code{analysesDetails} from a CSV file
#'  
#' @param file                  the name of the file to be loaded
#' @return An object of type \code{analysesDetails}
#' @examples \dontrun{
#'   connectionDetails <- createConnectionDetails(dbms="sql server", server="RNDUSRDHIT07.jnj.com")
#'   analysesDetails <- readSccAnalysesDetailsFromFile("c:/temp/test.csv")
#'   sccResult <- selfControlledCohort(analysesDetails, connectionDetails, "cdm_truven_mdcr", "scratch", sourceName = "cdm_truven_mdcr", exposuresOfInterest = c(767410,1314924,907879), outcomesOfInterest = c(444382, 79106, 138825), outcomeTable = "condition_era")
#'   plot(sccResult)
#' }
#' @export
readSccAnalysesDetailsFromFile <- function(file){
  d <- read.csv(file)
  d[is.na(d)] <- ""
  sccAnalysesDetails <- list()
  for (row in 1:nrow(d)){
    sccAnalysisDetails <- as.list(d[row,])
    for (column in c("drugTypeConceptIdList","conditionTypeConceptIdList","conditionTypeConceptIdList","genderConceptIdList")){
      sccAnalysisDetails[[column]] <- as.numeric(unlist(strsplit(as.character(d[row,column]),",")))
    }
    class(sccAnalysisDetails) = "sccAnalysisDetails"
    sccAnalysesDetails[[length(sccAnalysesDetails)+1]] <- sccAnalysisDetails
  }
  class(sccAnalysesDetails) <- "sccAnalysesDetails"
  sccAnalysesDetails
}

#' @export
print.sccResults <- function(sccResults){
  sccResults$effectEstimates
}

#' @export
summary.sccResults <- function(sccResults){
  sccResults$effectEstimates
}


#' @export
plot.sccResults <- function(sccResults){
  colnames(sccResults$effectEstimates) <- toupper(colnames(sccResults$effectEstimates))
    ggplot(sccResults$effectEstimates, aes(x=as.factor(EXPOSURECONCEPTID), y=IRR,ymin=IRRLB95, ymax=IRRUB95)) + 
      geom_hline(yintercept=1, colour ="#888888", lty=1, lw=1) +
      geom_point(size=2,alpha=0.7) +
      geom_errorbar(width=.1,alpha=0.7) +
      coord_flip() +  
      facet_grid(ANALYSISID~OUTCOMECONCEPTID) +
      scale_y_log10()
}

