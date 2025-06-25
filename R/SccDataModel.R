# Copyright 2022 Observational Health Data Sciences and Informatics
#
# This file is part of Reward
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

#' SccDataModel
#' @description
#' An interface to the common evidence model that uses works directly with a database schema
#' @field connection DatabaseConnector::connection instance
#' @field vocabularySchema OMOP vocabulary schema (must include concept and concept ancestor tables)
#' @field resultsSchema schema containing reward references and results
SccDataModel <- R6::R6Class(
  "SccDataModel",
  public = list(
    connection = NULL,
    config = NULL,
    resultsSchema = NULL,
    #' @description
    #' initialize backend object.
    #' @param connectionHandler.
    initialize = function(connectionHandler, resultsDatabaseSettings) {
      self$connection <- connectionHandler
      checkmate::assertString(resultsDatabaseSettings$resultsDatabaseSchema)
      self$resultsSchema <- resultsDatabaseSettings$resultsDatabaseSchema
    },

    #' Query database x
    #' @param sql     query string
    #' @param results_schema      (optional) schema string
    #' @param ... @seealso `SqlRender::render`
    queryDb = function(sql, resultsSchema = self$resultsSchema, ...) {
      self$connection$queryDb(sql, results_schema = self$resultsSchema, ...)
    },

    #' Get Data Sources available
    #' @description
    #' List of data sources
    #' @returns data.frame of data sources
    #'
    getDataSources = function() {
      self$queryDb("SELECT * FROM  @results_schema.database_meta_data")
    },

    #' @description
    #' Get outcome cohort definition set
    #' @param cohortIds         numeric vector of cohort ids or null
    getOutcomeCohorts = function(cohortIds = NULL) {
      # make a proper cohort definition set from sql and cohort json
      sql <- "SELECT cd.* FROM @results_schema.cg_cohort_definition cd
      INNER JOIN @results_schema.scc_outcome_exposure ec ON cd.cohort_definition_id = ec.outcome_cohort_id
      {@cohort_ids != ''}? {WHERE cohort_definition_id IN (@cohort_ids)}
      "
      self$connection$queryDb(sql,
                              cohort_ids = cohortIds,
                              results_schema = self$resultsSchema)
    },

    #' @description
    #' Get exposure cohort definition set
    #' @param cohortIds         numeric vector of cohort ids or null
    getExposureCohorts = function(cohortIds = NULL) {
      # make a proper cohort definition set from sql and cohort json
      sql <- "SELECT cd.* FROM @results_schema.cg_cohort_definition cd
      INNER JOIN @results_schema.scc_outcome_exposure ec ON cd.cohort_definition_id = ec.target_cohort_id
      {@cohort_ids != ''}? {WHERE cohort_definition_id IN (@cohort_ids)}
      "
      self$connection$queryDb(sql,
                              cohort_ids = cohortIds,
                              results_schema = self$resultsSchema)
    },

    #' @description
    #' Get expsoure cohort concept sets
    #' @param cohortIds         numeric vector of cohort ids or null
    getExposureCohortConceptSets = function(cohortIds = NULL) {
      sql <- "SELECT ccs.* FROM @results_schema.cohort_concept_set ccs
      INNER JOIN @results_schema.exposure_cohort ec ON ccs.cohort_definition_id = ec.cohort_definition_id
      {@cohort_ids != ''}? {WHERE ccs.cohort_definition_id IN (@cohort_ids)}
      "
      self$connection$queryDb(sql,
                              cohort_ids = cohortIds,
                              results_schema = self$resultsSchema)
    },

    #' @description
    #' Get outcome cohort concept sets for all cohorts specified
    #' @param cohortIds         numeric vector of cohort ids or null
    getOutcomeCohortConceptSets = function(cohortIds = NULL) {
      sql <-
        "SELECT ccs.*, oc.outcome_type FROM @results_schema.cohort_concept_set ccs
      INNER JOIN @results_schema.outcome_cohort oc ON ccs.cohort_definition_id = oc.cohort_definition_id
      {@cohort_ids != ''}? {WHERE ccs.cohort_definition_id IN (@cohort_ids)}
      "
      self$connection$queryDb(sql,
                              cohort_ids = cohortIds,
                              results_schema = self$resultsSchema)
    },

    #' @description
    #' Get getCohort data for one cohort
    #' @param cohortDefinitionId         cohort identifier (not null, integer)
    getCohort = function(cohortDefinitionId) {
      checkmate::assert_number(cohortDefinitionId)
      sql <- "SELECT cd.* FROM @results_schema.cg_cohort_definition cd
      WHERE cohort_definition_id = @cohort_definition_id"
      cohortDf <- self$connection$queryDb(sql,
                                          cohort_definition_id = cohortDefinitionId,
                                          results_schema = self$resultsSchema)

      cohort <- setNames(split(cohortDf, seq(nrow(cohortDf))), rownames(cohortDf))[[1]]
      #
      # cohort$conceptSets <-
      #   self$getCohortConceptSet(cohortDefinitionId)


      cohort$conceptSets <- list()
      return(cohort)
    },

    #' Get analysis settings, converting json text to list
    getAnalysisSettings = function() {
      sql <- "SELECT * FROM @results_schema.scc_analysis_setting"
      rows <- self$connection$queryDb(sql, results_schema = self$resultsSchema)
      rows$settings <- lapply(rows$settings, ParallelLogger::convertJsonToSettings)
      return(rows)
    },

    #' Returns SCC results for a cohort id
    #'
    #' @param cohortDefinitionId          Cohort Id
    #' @param outcomeType               Outcome types
    #' @param conceptSet                conceptset type
    #' @param isExposure                  Boolean - is this for exposure cohort?
    getNegativeControlSccResults = function(cohortDefinitionId,
                                            isExposure,
                                            outcomeType = NULL,
                                            conceptSet = NULL) {
      checkmate::assertLogical(isExposure)
      checkmate::assertIntegerish(outcomeType, null.ok = !isExposure)
      # Get negative controls from cem connection
      cemConnection <- self$getCemConnection()
      if (is.null(conceptSet))
        conceptSet <- self$getCohortConceptSet(cohortDefinitionId)

      if (isExposure) {
        controlConcepts <-
          cemConnection$getSuggestedControlCondtions(conceptSet)
      } else {
        controlConcepts <-
          cemConnection$getSuggestedControlIngredients(conceptSet)
      }

      if (nrow(controlConcepts)) {
        cohortIds <- controlConcepts$conceptId * 1000
        if (isExposure) {
          cohortIds <- cohortIds + outcomeType
        }

        sql <- "SELECT * FROM @results_schema.scc_result sr
        INNER JOIN @results_schema.scc_outcome_exposure soe ON sr.target_cohort_id = seo.target_cohort_id
                                                            AND seo.outcome_cohort_id = sr.outcome_cohort_id
        WHERE {@exposure} ? {sr.target_cohort_id} : {sr.outcome_cohort_id} = @cohort_definition_id
        AND {@exposure} ? {sr.outcome_cohort_id} : {sr.target_cohort_id} IN (@cohort_ids)
        AND rr IS NOT NULL"

        res <- self$connection$queryDb(
          sql,
          cohort_definition_id = cohortDefinitionId,
          cohort_ids = cohortIds,
          results_schema = self$resultsSchema,
          exposure = isExposure
        )
        return(res)
      }
      return(data.frame())
    },

    #' Count any query as subquery - (note: will be inneficient in many situations)
    #' @param query                 Sql query string
    #' @param ...                   @seealso `SqlRender::render`
    #' @param render                Optional - call sqlrender to render first or not
    countQuery = function(query, ..., render = TRUE) {
      if (render) {
        query <- SqlRender::render(query, ...)
      }

      res <- self$connection$queryDb("SELECT count(*) as CNT FROM (@sub_query) AS qur", sub_query = query)
      return(res$cnt)
    }
  )
)