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
#' @details
#' PATRICK HOMEWORK:   complete details
#'  
#' @param connectionDetails  An R object of type ConnectionDetail (details for the function that contains server info, database type, optionally username/password, port)
#' @param cdmSchema			string name of databsae schema that contains OMOP CDM and vocabulary
#' @param resultsSchema		string name of database schema that we can write results to
#' @param resultsTable  name of the table in the \code{resultsSchema} where the results will be written 
#' @param createResultsTable if true, a new empty table will be created to store the results. If false, results will be inserted into the existing table.
#' @param sourceName		string name of the database, as recorded in results
#' @param analysisId		A unique identifier that can later be used to identify the results of this analysis
#' @param exposuresOfInterest  list of DRUG_CONCEPT_IDs to study, if NULL, then all DRUG_CONCEPT_IDs will be used
#' @param outcomesOfInterest	list of CONDITION_CONCET_IDs to study, if NULL, all CONDITIONS considered as potential outcomes
#' @param exposureTable	drugEra or cohort
#' @param outcomeTable	conditionEra or cohort
#' @param firstOccurrenceDrugOnly	if 1, only use first occurrence of each drug concept id for each person in DRUG_ERA table
#' @param firstOccurrenceConditionOnly	if 1, only use first occurrence of each condition concept id for each person in CONDITION_ERA table
#' @param drugTypeConceptJdList	which DRUG_TYPE to use:  generally only use 1 value (ex:  30d era)
#' @param conditionTypeConceptJdList	which CONDITION_TYPE to use:  generally only use 1 value (ex:  30d era)
#' @param genderConceptJdList	list of GENDER_CONCEPT_IDs, generally use MALE (8507) and FEMALE (8532)
#' @param minAge	integer for minimum allowable age
#' @param maxAge	integer for maximum allowable age
#' @param minJndex	date for minimum allowable data for index exposure
#' @param maxJndex	date for maximum allowable data for index exposure
#' @param stratifyGender	if 1, analysis will be calculated overall, and stratified across all gender groups
#' @param stratifyAge	if 1, analysis will be calculated overall, and stratified across all age groups  (using AGE_GROUP table below)
#' @param stratifyIndex	if 1, analysis will be calculated overall, and stratified across all years of the index dates
#' @param useLengthOfExposureExposed	if 1, use the duration from drugEraStart -> drugEraEnd as part of timeAtRisk
#' @param timeAtRiskExposedStart	integer of days to add to drugEraStart for start of timeAtRisk (0 to include index date, 1 to start the day after)
#' @param surveillanceExposed	additional window to add to end of exposure period (if USE_LENGTH_OF_EXPOSURE_EXPOSED = 1, then add to DRUG_ERA_END, else add to DRUG_ERA_START)
#' @param useLengthOfExposureUnexposed	if 1, use the duration from drugEraStart -> drugEraEnd as part of timeAtRisk looking back before drugEraStart
#' @param timeAtRiskUnexposedStart	integer of days to add to drugEraStart for start of timeAtRisk (0 to include index date, -1 to start the day before)
#' @param surveillanceUnexposed	additional window to add to end of exposure period (if USE_LENGTH_OF_EXPOSURE_UNEXPOSED = 1, then add to DRUG_ERA_END, else add to DRUG_ERA_START)
#' @param hasFullTimeAtRisk	if 1, restrict to people who have full time-at-risk exposed and unexposed
#' @param washoutPeriodLength	integer to define required time observed before exposure start
#' @param followupPeriodLength	integer to define required time observed after exposure start
#' @param shrinkage	shrinkage used in IRR calculations, required >0 to deal with 0 case counts, but larger number means more shrinkage
#' 
#' @return An object of type \code{sccResults} containing details for connecting to the database containing the results 
#' @examples \dontrun{
#'   connectionDetails <- createConnectionDetails(dbms="sql server", server="RNDUSRDHIT07.jnj.com")
#'   sccResult <- selfControlledCohort(connectionDetails, "cdm_truven_mdcr", "scratch", sourceName = "cdm_truven_mdcr", exposuresOfInterest = c(767410,1314924,907879), outcomesOfInterest = c(444382, 79106, 138825), outcomeTable = "condition_era")
#'   plot(sccResult)
#' }
#' @export
selfControlledCohort <- function (connectionDetails, 
                                  cdmSchema, 
                                  resultsSchema, 
                                  resultsTablePrefix = "self_controlled_cohort", 
                                  createResultsTable = TRUE, 
                                  sourceName = "", 
                                  analysisId = 1,
                                  exposuresOfInterest = c(), 
                                  outcomesOfInterest = c(), 
                                  exposureTable = "drug_era",
                                  outcomeTable = "cohort",
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
  
  pathToSql <- system.file("sql", "SccParameterizedSQL.sql", package="SelfControlledCohort")
  #pathToSql <- "C:\\Users\\mschuemi\\Documents\\RStudio SVN workspace\\SelfControlledCohort\\inst\\sql\\SccParameterizedSQLV2.sql"
  parameterizedSql <- readChar(pathToSql,file.info(pathToSql)$size)
  
  renderedSql <- renderSql(parameterizedSql[1], 
                           cdm_schema = cdmSchema, 
                           results_schema = resultsSchema, 
                           results_table_prefix = resultsTablePrefix, 
                           create_results_table = createResultsTable,
                           source_name = sourceName,
                           analysis_id = analysisId,
                           exposures_of_interest = exposuresOfInterest, 
                           outcomes_of_interest = outcomesOfInterest, 
                           exposure_table = exposureTable,
                           exposure_start_date = exposureStartDate,
                           exposure_end_date = exposureEndDate,
                           exposure_concept_id = exposureConceptId,
                           exposure_person_id = exposurePersonId,
                           outcome_table = outcomeTable,
                           outcome_start_date = outcomeStartDate,
                           outcome_end_date = outcomeEndDate,
                           outcome_concept_id = outcomeConceptId,
                           outcome_person_id = outcomePersonId,
                           first_occurrence_drug_only = firstOccurrenceDrugOnly,
                           first_occurrence_condition_only = firstOccurrenceDrugOnly,
                           drug_type_concept_id_list = drugTypeConceptIdList,
                           condition_type_concept_id_list = conditionTypeConceptIdList,
                           gender_concept_id_list = genderConceptIdList,
                           min_age = minAge,
                           max_age = maxAge,
                           min_index = minIndex,
                           max_index = maxIndex,
                           stratify_gender = stratifyGender,
                           stratify_age = stratifyAge,
                           stratify_index = stratifyIndex,
                           use_length_of_exposure_exposed = useLengthOfExposureExposed,
                           time_at_risk_exposed_start = timeAtRiskExposedStart,
                           surveillance_exposed = surveillanceExposed,
                           use_length_of_exposure_unexposed = useLengthOfExposureUnexposed,
                           time_at_risk_unexposed_start = timeAtRiskUnexposedStart,
                           surveillance_unexposed = surveillanceUnexposed,
                           has_full_time_at_risk = hasFullTimeAtRisk,
                           washout_period_length = washoutPeriodLength,
                           followup_period_length = followupPeriodLength,
                           shrinkage = shrinkage                        
  )$sql
  conn <- connect(connectionDetails)
  
  writeLines("Executing large query. This could take a while")
  dbSendUpdate(conn, renderedSql)
  writeLines(paste("Done. Results can now be found in ",resultsSchema,".",resultsTablePrefix,"_results, analyses documented in ",resultsSchema,".",resultsTablePrefix,"_analysis",sep=""))
  
  dbDisconnect(conn)
  
  resultsConnectionDetails <- connectionDetails
  resultsConnectionDetails$schema = resultsSchema
  
  result <- list(resultsConnectionDetails = resultsConnectionDetails, 
                 resultsTable = paste(resultsTablePrefix,"_results",sep=""),
                 analysisTable = paste(resultsTablePrefix,"_analysis",sep=""),
                 sql = renderedSql,
                 call = match.call())
  class(result) <- "sccResults"
  result
}

