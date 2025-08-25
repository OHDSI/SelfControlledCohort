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

    #' get cohorts from a direct concept id mapping
    #' @description
    #' Does not traverse heirachy, cohort ids are returned if they contain the concept id directly and it is not excluded
    #' @param conceptIds
    getCohortsFromConceptIds = function(conceptIds) {
      if (!length(conceptIds))
        return(NULL)

      sql <- "
      SELECT DISTINCT cd.cohort_definition_id
      FROM @results_schema.cg_cohort_definition cd
      INNER JOIN @results_schema.cg_cohort_concept_set ccs ON cd.cohort_definition_id = ccs.cohort_definition_id
      INNER JOIN @results_schema.cg_concept_set cs ON cs.concept_set_id = ccs.concept_set_id AND is_excluded = 0
      WHERE cs.concept_id IN (@ot_concepts)"
      self$queryDb(sql, ot_concepts = conceptIds) |>
          dplyr::pull()
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
    #' @param databaseId                  Database Id
    getNegativeControlSccResults = function(cohortDefinitionId, databaseId) {

      sql <- "
        SELECT sr.*
        FROM @results_schema.scc_result sr
        INNER JOIN @results_schema.scc_outcome_exposure soe ON
                    sr.target_cohort_id = soe.target_cohort_id AND soe.outcome_cohort_id = sr.outcome_cohort_id
        WHERE (sr.target_cohort_id = @cohort_definition_id OR sr.outcome_cohort_id = @cohort_definition_id)
        AND sr.database_id = '@database_id'
        AND soe.true_effect_size = 1
        AND rr IS NOT NULL"

      res <- self$connection$queryDb(
        sql,
        cohort_definition_id = cohortDefinitionId,
        results_schema = self$resultsSchema,
        database_id = databaseId
      )
      return(res)
    },

    #' Shiny Dashboard - main query
    #'
    #' @param benefitThreshold thereshold to consider a benefit
    #' @param lowerBenefitThereshold thereshold to consider a benefit
    #'        (lower bounds, e.g. exclude benefits below 0.1 to rule out potential indications)
    #' @param riskThreshold threshold to consider effect estimate a risk
    #' @param pValueCut pvalue hacking
    #' @param requiredBenefitSources required sources to be in results
    #' @param filterByMeta filter by meta analysis RR?
    #' @param outcomeCohortTypes outcome cohorts to filter by type of
    #' @param calibrated calibrated result
    #' @param benefitCount minimum number of benefits found
    #' @param riskCount number of accerptable risks
    #' @param targetCohorts target cohort ids
    #' @param outcomeCohorts outcome cohort ids
    #' @param exposureClasses exposureClasses filter
    #' @param orderByCol Order by which column?
    #' @param ascending ascending order?
    #' @param limit Row limit
    #' @param offset Row Offset
    #' @param excludedConcepts concept id's to exclude from results
    getFilteredTableResultsQuery = function(benefitThreshold = 0.5,
                                            lowerBenefitThereshold = 0.0,
                                            riskThreshold = 2.0,
                                            pValueCut = 0.05,
                                            requiredBenefitSources = NULL,
                                            filterByMeta = FALSE,
                                            outcomeCohortTypes = c(0, 1, 2, 3),
                                            calibrated = TRUE,
                                            benefitCount = 1,
                                            riskCount = 0,
                                            targetCohorts = NULL,
                                            outcomeCohorts = NULL,
                                            excludedOutcomeCohorts = NULL,
                                            excludedTargetCohorts = NULL,
                                            exposureClasses = NULL,
                                            orderByCol = NULL,
                                            ascending = NULL,
                                            excludedConcepts = NULL,
                                            targetSearchText = "",
                                            outcomeSearchText = "",
                                            limit = NULL,
                                            analysisId = 1,
                                            offset = NULL) {
      calibrated <- ifelse(calibrated, 1, 0)
      filterOutcomes <- length(outcomeCohortTypes) > 0
      query <- SqlRender::loadRenderTranslateSql(
        sqlFilename = file.path("dashboard", "mainTable.sql"),
        packageName = utils::packageName(),
        risk = riskThreshold,
        lower_benefit = lowerBenefitThereshold,
        benefit = benefitThreshold,
        p_cut_value = pValueCut,
        analysis_id = analysisId,
        # filter_outcome_types = filterOutcomes,
        # outcome_types = outcomeCohortTypes,
        risk_count = riskCount,
        benefit_count = benefitCount,
        # calibrated = calibrated,
        #show_exposure_classes = !self$config$exposureDashboard,
        filter_by_meta_analysis = filterByMeta,
        outcome_cohorts = outcomeCohorts,
        excluded_outcome_cohorts = excludedOutcomeCohorts,
        target_cohorts = targetCohorts,
        excluded_target_cohorts = excludedTargetCohorts,
        exposure_classes = exposureClasses,
        required_benefit_sources = paste0("'", requiredBenefitSources, "'"),
        required_benefit_count = length(requiredBenefitSources),
        outcome_search_text = outcomeSearchText,
        target_search_text = targetSearchText,
        excluded_concepts = excludedConcepts,
        vocabulary_schema = self$resultsSchema,
        order_by = orderByCol,
        ascending = ascending,
        limit = limit,
        offset = offset,
        schema = self$resultsSchema
      )
      return(query)
    },

    #' Get Filtered Table Results
    #' @description
    #' @params ...     Params for getFilteredTableResultsQuery
    getFilteredTableResults = function(...) {
      sql <- self$getFilteredTableResultsQuery(...)
      self$connection$queryDb(sql)
    },

    #' Get Filtered Table Results
    #' @description
    #' Get results count - filtered by specified parameters
    #' @params ...     Params for getFilteredTableResults
    getFilteredTableResultsCount = function(...) {
      sql <- self$getFilteredTableResultsQuery(...)
      self$countQuery(sql, render = FALSE)
    },

    #' Get table of meta analysis results
    #'
    #' @param exposureId exposure cohort id
    #' @param outcomeId outcome Cohort id
    #' @param analysisId outcome Cohort id
    getMetaAnalysisTable = function(exposureId, outcomeId, analysisId = 1) {
      sql <- "
        SELECT
            COALESCE(ds.cdm_source_abbreviation, r.database_id) as source_name,
            r.*
        FROM @results_schema.scc_result r
        LEFT JOIN @results_schema.database_meta_data ds ON ds.database_id = r.database_id
        WHERE r.OUTCOME_COHORT_ID = @outcome
        AND r.TARGET_COHORT_ID = @treatment
        AND r.analysis_id = @analysis_id
        ORDER BY r.database_id
      "
      return(self$queryDb(sql, treatment = exposureId, outcome = outcomeId, analysis_id = analysisId))
    },

    #' Get Forest plot table
    #'
    #' @param exposureId  exposure Id
    #' @param outcomeId   outcome Id
    #' @param calibrated  get calibrated results?
    getForestPlotTable = function(exposureId, outcomeId, analysisId, calibrated) {
      sql <- "
      {DEFAULT @use_calibration = TRUE}
        SELECT
            r.database_id,
            coalesce(ds.cdm_source_abbreviation, 'Meta Analysis') as source_name,
            {@use_calibration} ? {
            r.CALIBRATED_RR as RR,
            r.CALIBRATED_LB_95 as LB_95,
            r.CALIBRATED_UB_95 as UB_95,
            r.CALIBRATED_P_VALUE as P_VALUE,
            r.CALIBRATED_SE_LOG_RR as SE_LOG_RR,
            } : {
            r.RR,
            r.LB_95,
            r.UB_95,
            r.P_VALUE,
            r.SE_LOG_RR,
            }
            r.I2
        FROM @results_schema.scc_result r
        LEFT JOIN @results_schema.database_meta_data ds ON ds.database_id = r.database_id
            WHERE r.OUTCOME_COHORT_ID = @outcome
            AND r.TARGET_COHORT_ID = @treatment
            AND r.analysis_id = @analysis_id
      "
      table <-
        self$queryDb(sql,
                     treatment = exposureId,
                     outcome = outcomeId,
                     analysis_id = analysisId,
                     use_calibration = calibrated)

      # custom order of data.frame
      metaRow <- table |> dplyr::filter(.data$databaseId == 'meta-analysis')
      otherRows <- table |> dplyr::filter(.data$databaseId != 'meta-analysis')
      table <- dplyr::bind_rows(otherRows, metaRow)
      return(table)
    },

    #' Get Summary Statistics
    #'
    #' @param statType        statistic type
    #' @param exposureId      exposure cohort id
    #' @param outcomeId       outcome cohort id
    #' @param sourceIds       cohort source ids
    #' @param tableName       table name
    #' @param analysisId      Analysis setting id
    getSummaryStats = function(statType,
                               exposureId,
                               outcomeId,
                               sourceIds = NULL,
                               analysisId = 1) {
      self$queryDb(
        "
      SELECT
        ds.cdm_source_abbreviation,
        ds.database_id,
        stat_type,
        round(mean, 3) as mean,
        round(sd, 3) as sd,
        minimum as min,
        p10,
        p25,
        median,
        p75,
        p90,
        maximum as max,
        total
      FROM @results_schema.scc_stat tts
      INNER JOIN @results_schema.database_meta_data ds ON tts.database_id = ds.database_id
      WHERE stat_type IN (@stat_types)
      AND target_cohort_id = @treatment AND outcome_cohort_id = @outcome
      AND mean is not NULL
      AND analysis_id = @analysis_id
      {@source_ids != ''} ? {AND ds.database_id IN (@source_ids)}",
        stat_types = paste0("'", statType, "'"),
        analysis_id = analysisId,
        treatment = exposureId,
        outcome = outcomeId,
        source_ids = sourceIds
      )
    },

    #' getTimeOnTreatmentStats
    #'
    #' @param ...
    getTimeToOutcomeStats = function(...) {
      self$getSummaryStats(statType = c("time_to_outcome", "time_to_outcome_exposed", "time_to_outcome_unexposed"), ...)
    },

    #' getTimeOnTreatmentStats
    #'
    #' @param ...
    getTimeOnTreatmentStats = function(...) {
      self$getSummaryStats(statType = "time_exposed", ...)
    },

    #' getTimeOnTreatmentStats
    #'
    #' @param ...
    getTimeToOutcomeExposedStats = function(...) {
      self$getSummaryStats(statType = "time_to_outcome_exposed", ...)
    },

    #' getTimeOnTreatmentStats
    #'
    #' @param ...
    getTimeToOutcomeUnexposedStats = function(...) {
      self$getSummaryStats(statType = "time_to_outcome_unexposed", ...)
    },

    #' get target cohort info for the dashboard
    #' @description
    #' This will either be for exposures or outcomes, depending on the dashboard type
    getTargetCohortInfo = function(targetCohortIds) {
      checkmate::assertNumeric(targetCohortIds)
      sql <- "
      SELECT c.cohort_definition_id,
            cohort_name,
            count(sr.rr > 0) as n_estimates
      FROM @results_schema.cg_cohort_definition c
      INNER JOIN @results_schema.scc_result sr ON (c.cohort_definition_id = sr.target_cohort_id or c.cohort_definition_id = sr.outcome_cohort_id)
                                                  AND sr.database_id = 'meta-analysis'
      WHERE c.cohort_definition_id IN (@target_cohorts)
      GROUP BY c.cohort_definition_id
      " |>
        self$queryDb(target_cohorts = targetCohortIds)
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
