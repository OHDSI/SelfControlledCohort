# @file SelfControlledCohort.R
#
# Copyright 2016 Observational Health Data Sciences and Informatics
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

#' SelfControlledCohort
#'
#' @docType package
#' @name SelfControlledCohort
#' @importFrom RJDBC dbDisconnect
#' @import DatabaseConnector
NULL


#' @title
#' Run self-controlled cohort
#'
#' @description
#' \code{runSelfControlledCohort} generates population-level estimation by comparing exposed and
#' unexposed time among exposed cohort.
#'
#' @details
#' Population-level estimation method that estimates incidence rate comparison of exposed/unexposed
#' time within an exposed cohort.
#'
#' If multiple exposureIds and outcomeIds are provided, estimates will be generated for every
#' combination of exposure and outcome.
#'
#' @references
#' Ryan PB, Schuemie MJ, Madigan D.Empirical performance of a self-controlled cohort method: lessons
#' for developing a risk identification and analysis system. Drug Safety 36 Suppl1:S95-106, 2013
#'
#' @param connectionDetails                An R object of type \code{connectionDetails} created using
#'                                         the function \code{createConnectionDetails} in the
#'                                         \code{DatabaseConnector} package.
#' @param cdmDatabaseSchema                Name of database schema that contains the OMOP CDM and
#'                                         vocabulary.
#' @param cdmVersion                       Define the OMOP CDM version used: currently support "4" and
#'                                         "5".
#' @param oracleTempSchema                 For Oracle only: the name of the database schema where you
#'                                         want all temporary tables to be managed. Requires
#'                                         create/insert permissions to this database.
#' @param exposureIds                      A vector containing the drug_concept_ids or
#'                                         cohort_definition_ids of the exposures of interest
#' @param outcomeIds                       The condition_concept_ids or cohort_definition_ids of the
#'                                         outcomes of interest
#' @param exposureDatabaseSchema           The name of the database schema that is the location where
#'                                         the exposure data used to define the exposure cohorts is
#'                                         available. If exposureTable = DRUG_ERA,
#'                                         exposureDatabaseSchema is not used by assumed to be
#'                                         cdmSchema.  Requires read permissions to this database.
#'
#' @param exposureTable                    The tablename that contains the exposure cohorts.  If
#'                                         exposureTable <> DRUG_ERA, then expectation is exposureTable
#'                                         has format of COHORT table: cohort_concept_id, SUBJECT_ID,
#'                                         COHORT_START_DATE, COHORT_END_DATE.
#' @param outcomeDatabaseSchema            The name of the database schema that is the location where
#'                                         the data used to define the outcome cohorts is available. If
#'                                         exposureTable = CONDITION_ERA, exposureDatabaseSchema is not
#'                                         used by assumed to be cdmSchema.  Requires read permissions
#'                                         to this database.
#' @param outcomeTable                     The tablename that contains the outcome cohorts.  If
#'                                         outcomeTable <> CONDITION_OCCURRENCE, then expectation is
#'                                         outcomeTable has format of COHORT table:
#'                                         COHORT_DEFINITION_ID, SUBJECT_ID, COHORT_START_DATE,
#'                                         COHORT_END_DATE.
#' @param firstOccurrenceDrugOnly          If TRUE, only use first occurrence of each drug concept id
#'                                         for each person
#' @param firstOccurrenceConditionOnly     If TRUE, only use first occurrence of each condition concept
#'                                         id for each person.
#' @param outcomeConditionTypeConceptIds   A list of TYPE_CONCEPT_ID values that will restrict
#'                                         condition occurrences.  Only applicable if outcomeTable =
#'                                         CONDITION_OCCURRENCE.
#' @param genderConceptIds                 of gender_concept_id, generally use MALE (8507) and FEMALE
#'                                         (8532).
#' @param minAge                           Integer for minimum allowable age.
#' @param maxAge                           Integer for maximum allowable age.
#' @param studyStartDate                   Date for minimum allowable data for index exposure. Date
#'                                         format is 'yyyymmdd'.
#' @param studyEndDate                     Date for maximum allowable data for index exposure. Date
#'                                         format is 'yyyymmdd'.
#' @param stratifyByGender                 If TRUE, analysis will be calculated overall, and stratified
#'                                         across all gender groups.
#' @param stratifyByAge                    If TRUE, analysis will be calculated overall, and stratified
#'                                         across all age groups (using AGE_GROUP table below).
#' @param stratifyByYear                   If TRUE, analysis will be calculated overall, and stratified
#'                                         across all years of the index dates.
#' @param useLengthOfExposureExposed       If TRUE, use the duration from drugEraStart -> drugEraEnd as
#'                                         part of timeAtRisk.
#' @param timeAtRiskExposedStart           Integer of days to add to drugEraStart for start of
#'                                         timeAtRisk (0 to include index date, 1 to start the day
#'                                         after).
#' @param surveillanceExposed              Additional window to add to end of exposure period (if
#'                                         useLengthOfExposureExposed = TRUE, then add to exposure end
#'                                         date, else add to exposure start date).
#' @param useLengthOfExposureUnexposed     If TRUE, use the duration from exposure start -> exposure
#'                                         end as part of timeAtRisk looking back before exposure
#'                                         start.
#' @param timeAtRiskUnexposedStart         Integer of days to add to exposure start for start of
#'                                         timeAtRisk (0 to include index date, -1 to start the day
#'                                         before).
#' @param surveillanceUnexposed            Additional window to add to end of exposure period (if
#'                                         useLengthOfExposureUnexposed = TRUE, then add to exposure
#'                                         end date, else add to exposure start date).
#' @param hasFullTimeAtRisk                If TRUE, restrict to people who have full time-at-risk
#'                                         exposed and unexposed.
#' @param washoutWindow                    Integer to define required time observed before exposure
#'                                         start.
#' @param followupWindow                   Integer to define required time observed after exposure
#'                                         start.
#'
#' @return
#' An object of type \code{sccResults} containing the results of the analysis.
#' @examples
#' \dontrun{
#' connectionDetails <- createConnectionDetails(dbms = "sql server",
#'                                              server = "RNDUSRDHIT07.jnj.com")
#' sccResult <- runSelfControlledCohort(connectionDetails,
#'                                      cdmDatabaseSchema = "cdm_truven_mdcr.dbo",
#'                                      exposureIds = c(767410, 1314924, 907879),
#'                                      outcomeIds = 444382,
#'                                      outcomeTable = "condition_era")
#' }
#' @export
runSelfControlledCohort <- function(connectionDetails,
                                    cdmDatabaseSchema,
                                    cdmVersion = 5,
                                    oracleTempSchema,
                                    exposureIds,
                                    outcomeIds,
                                    exposureDatabaseSchema = cdmDatabaseSchema,
                                    exposureTable = "drug_era",
                                    outcomeDatabaseSchema = cdmDatabaseSchema,
                                    outcomeTable = "condition_era",
                                    firstOccurrenceDrugOnly = TRUE,
                                    firstOccurrenceConditionOnly = TRUE,
                                    outcomeConditionTypeConceptIds = c(38000247),
                                    genderConceptIds = c(8507, 8532),
                                    minAge = "",
                                    maxAge = "",
                                    studyStartDate = "",
                                    studyEndDate = "",
                                    stratifyByGender = FALSE,
                                    stratifyByAge = FALSE,
                                    stratifyByYear = FALSE,
                                    useLengthOfExposureExposed = TRUE,
                                    timeAtRiskExposedStart = 1,
                                    surveillanceExposed = 30,
                                    useLengthOfExposureUnexposed = TRUE,
                                    timeAtRiskUnexposedStart = -1,
                                    surveillanceUnexposed = -30,
                                    hasFullTimeAtRisk = FALSE,
                                    washoutWindow = 0,
                                    followupWindow = 0) {
  exposureTable <- tolower(exposureTable)
  outcomeTable <- tolower(outcomeTable)
  if (exposureTable == "drug_era") {
    exposureStartDate <- "drug_era_start_date"
    exposureEndDate <- "drug_era_end_date"
    exposureConceptId <- "drug_concept_id"
    exposurePersonId <- "person_id"
  } else if (exposureTable == "drug_exposure") {
    exposureStartDate <- "drug_exposure_start_date"
    exposureEndDate <- "drug_exposure_end_date"
    exposureConceptId <- "drug_concept_id"
    exposurePersonId <- "person_id"
  } else {
    exposureStartDate <- "cohort_start_date"
    exposureEndDate <- "cohort_end_date"
    if (cdmVersion == "4") {
      exposureConceptId <- "cohort_concept_id"
    } else {
      exposureConceptId <- "cohort_definition_id"
    }
    exposurePersonId <- "subject_id"
  }

  if (outcomeTable == "condition_era") {
    outcomeStartDate <- "condition_era_start_date"
    outcomeEndDate <- "condition_era_end_date"
    outcomeConceptId <- "condition_concept_id"
    outcomePersonId <- "person_id"
  } else if (outcomeTable == "condition_occurrence") {
    outcomeStartDate <- "condition_start_date"
    outcomeEndDate <- "condition_end_date"
    outcomeConceptId <- "condition_concept_id"
    outcomePersonId <- "person_id"
  } else {
    outcomeStartDate <- "cohort_start_date"
    outcomeEndDate <- "cohort_end_date"
    if (cdmVersion == "4") {
      outcomeConceptId <- "cohort_concept_id"
    } else {
      outcomeConceptId <- "cohort_definition_id"
    }
    outcomePersonId <- "subject_id"
  }

  # Check if connection already open:
  if (is.null(connectionDetails$conn)) {
    conn <- DatabaseConnector::connect(connectionDetails)
  } else {
    conn <- connectionDetails$conn
  }

  renderedSql <- SqlRender::loadRenderTranslateSql(sqlFilename = "SccParameterizedSQL.sql",
                                                   packageName = "SelfControlledCohort",
                                                   dbms = connectionDetails$dbms,
                                                   oracleTempSchema = oracleTempSchema,
                                                   cdm_database_schema = cdmDatabaseSchema,
                                                   exposure_ids = exposureIds,
                                                   outcome_id = outcomeIds,
                                                   exposure_database_schema = exposureDatabaseSchema,
                                                   exposure_table = exposureTable,
                                                   exposure_start_date = exposureStartDate,
                                                   exposure_end_date = exposureEndDate,
                                                   exposure_concept_id = exposureConceptId,
                                                   exposure_person_id = exposurePersonId,
                                                   outcome_database_schema = outcomeDatabaseSchema,
                                                   outcome_table = outcomeTable,
                                                   outcome_start_date = outcomeStartDate,
                                                   outcome_end_date = outcomeEndDate,
                                                   outcome_concept_id = outcomeConceptId,
                                                   outcome_person_id = outcomePersonId,
                                                   first_occurrence_drug_only = firstOccurrenceDrugOnly,
                                                   first_occurrence_condition_only = firstOccurrenceConditionOnly,
                                                   outcome_condition_type_concept_ids = outcomeConditionTypeConceptIds,
                                                   gender_concept_ids = genderConceptIds,
                                                   min_age = minAge,
                                                   max_age = maxAge,
                                                   study_start_date = studyStartDate,
                                                   study_end_date = studyEndDate,
                                                   stratify_by_gender = stratifyByGender,
                                                   stratify_by_age = stratifyByAge,
                                                   stratify_by_year = stratifyByYear,
                                                   use_length_of_exposure_exposed = useLengthOfExposureExposed,
                                                   time_at_risk_exposed_start = timeAtRiskExposedStart,
                                                   surveillance_exposed = surveillanceExposed,
                                                   use_length_of_exposure_unexposed = useLengthOfExposureUnexposed,
                                                   time_at_risk_unexposed_start = timeAtRiskUnexposedStart,
                                                   surveillance_unexposed = surveillanceUnexposed,
                                                   has_full_time_at_risk = hasFullTimeAtRisk,
                                                   washout_window = washoutWindow,
                                                   followup_window = followupWindow)
  writeLines("Executing analysis")
  DatabaseConnector::executeSql(conn, renderedSql)

  # Fetch results from server:
  sql <- "SELECT * FROM #results"
  sql <- SqlRender::translateSql(sql,
                                 targetDialect = connectionDetails$dbms,
                                 oracleTempSchema = oracleTempSchema)$sql
  estimates <- DatabaseConnector::querySql(conn, sql)
  colnames(estimates) <- SqlRender::snakeCaseToCamelCase(colnames(estimates))
  if (nrow(estimates) > 0) {
    for (i in 1:nrow(estimates)) {
      test <- rateratio.test::rateratio.test(x = c(estimates$numOutcomesExposed[i], estimates$numOutcomesUnexposed[i]),
                                             n = c(estimates$timeAtRiskExposed[i], estimates$timeAtRiskUnexposed[i]))
      estimates$incidenceRateRatio[i] <- test$estimate[1]
      estimates$irrLb95[i] <- test$conf.int[1]
      estimates$irrUb95[i] <- test$conf.int[2]
    }
    estimates$logRr <- log(estimates$incidenceRateRatio)
    estimates$seLogRr <- (log(estimates$irrUb95) - log(estimates$irrLb95)) / (2 * qnorm(0.975))
  }
  # Drop temp table:
  sql <- "TRUNCATE TABLE #results; DROP TABLE #results;"
  sql <- SqlRender::translateSql(sql,
                                 targetDialect = connectionDetails$dbms,
                                 oracleTempSchema = oracleTempSchema)$sql
  DatabaseConnector::executeSql(conn, sql, progressBar = FALSE, reportOverallTime = FALSE)

  if (is.null(connectionDetails$conn)) {
    RJDBC::dbDisconnect(conn)
  }

  result <- list(estimates = estimates,
                 exposureIds = exposureIds,
                 outcomeIds = outcomeIds,
                 call = match.call(),
                 sql = renderedSql)

  class(result) <- "sccResults"

  return(result)
}


#' @export
print.sccResults <- function(x, ...) {
  writeLines("sccResults object")
  writeLines("")
  writeLines(paste("Exposure ID(s):", paste(x$exposureIds, collapse = ",")))
  writeLines(paste("Outcome ID(s):", x$outcomeIds))
}

#' @export
summary.sccResults <- function(object, ...) {
  object$estimates
}
