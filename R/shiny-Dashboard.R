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

strQueryWrap <- function(vec) {
  vec <- gsub("'", "''", vec)
  paste0("'", vec, "'", sep = "")
}

#' @title
#' Dashboard instance
#' @description
#' Requires a server appConfig instance to be loaded in environment see scoping of launchDashboard
#' This can be obtained with rewardb::loadappConfig(...)
#' @param input shiny input object
#' @param output shiny output object
#' @param session shiny session
sccModule <- function(id = "scc-module", model, appConfig = model$config) {
  cli::cli_alert_info("Loading scc module")
  ns <- shiny::NS(id)
  shiny::moduleServer(id, function(input, output, session) {

    output$dataSourceTable <- reactable::renderReactable({
      tbl <- model$getDataSources() |> dplyr::select("databaseId", "cdmSourceAbbreviation", "cdmVersion")
      colnames(tbl) <- SqlRender::camelCaseToTitleCase(colnames(tbl))
      reactable::reactable(tbl)
    })


    querySearchSelector <- function(search, searchType, existingValues) {
      if (isTRUE(nchar(search) < 3 & existingValues == ""))
        return(NULL)

      # cleanup to prevent sql injection - remove bad chars and limit length
      search <- substr(gsub(";|\'", "", search), 1, 100)
      search <- paste0("'%%", search, "%%'")
      sql <- "
      SELECT cd.cohort_definition_id, cd.cohort_name
         FROM (
            SELECT
                DISTINCT cd.cohort_definition_id, cd.cohort_name FROM @results_schema.cg_cohort_definition cd
            INNER JOIN @results_schema.scc_outcome_exposure oe on
            {@search_type == 'outcome'} ? {oe.outcome_cohort_id} : {oe.target_cohort_id} = cd.cohort_definition_id
            WHERE lower(cd.cohort_name) LIKE @search
            ORDER BY cohort_name LIMIT 10
      ) cd
      {@existing_ids != ''} ?{
      UNION

      SELECT cd.cohort_definition_id, cd.cohort_name FROM @results_schema.cg_cohort_definition cd
      WHERE cd.cohort_definition_id IN (@existing_ids)
      }

      "
      result <- model$queryDb(sql, search = tolower(search), search_type = searchType, existing_ids = existingValues)

      if (nrow(result) == 0)
        return(NULL)

      choices <- result$cohortDefinitionId
      names(choices) <- result$cohortName
      return(choices)
    }

    shiny::observeEvent(input$outcomeSearchBox, {
      shiny::req(input$outcomeSearchBox)
      if (nchar(input$outcomeSearchBox) > 2) {
        choices <- querySearchSelector(input$outcomeSearchBox, "outcome", input$outcomeSearch)
        shiny::updateSelectizeInput(
          session = session,
          inputId = "outcomeSearch",
          server = TRUE,
          choices = choices,
          selected = input$outcomeSearch
        )
      }
    })

    shiny::observeEvent(input$targetSearchBox, {
      shiny::req(input$targetSearchBox)
      if (nchar(input$targetSearchBox) > 2) {
        choices <- querySearchSelector(input$targetSearchBox, "target", input$targetSearch)
        shiny::updateSelectizeInput(
          session = session,
          inputId = "targetSearch",
          server = TRUE,
          choices = choices,
          selected = input$targetSearch
        )
      }
    })

    shiny::observeEvent(input$excludedOutcomeSearchBox, {
      shiny::req(input$excludedOutcomeSearchBox)
      if (nchar(input$excludedOutcomeSearchBox) > 2) {
        choices <- querySearchSelector(input$excludedOutcomeSearchBox, "outcome", input$excludedOutcomeSearch)
        shiny::updateSelectizeInput(
          session = session,
          inputId = "excludedOutcomeSearch",
          server = TRUE,
          choices = choices,
          selected = input$excludedOutcomeSearch
        )
      }
    })

    shiny::observeEvent(input$excludedTargetSearchBox, {
      shiny::req(input$excludedTargetSearchBox)
      if (nchar(input$excludedTargetSearchBox) > 2) {
        choices <- querySearchSelector(input$excludedTargetSearchBox, "target", input$excludedTargetSearch)
        shiny::updateSelectizeInput(
          session = session,
          inputId = "excludedTargetSearch",
          server = TRUE,
          choices = choices,
          selected = input$excludedTargetSearch
        )
      }
    })


    openTargetsIngredientSearch <- function(searchString, existingValues) {
      if (isTRUE(nchar(searchString) < 3 & existingValues == ""))
        return(NULL)

      # cleanup to prevent sql injection - remove bad chars and limit length
      search <- substr(gsub(";|\'", "", searchString), 1, 100)
      search <- paste0("'%%", search, "%%'")
      sql <- "
      SELECT *
      FROM (
        SELECT
          tccs.cohort_definition_id, ot.drug_name
        FROM @open_targets_database_schema.opentargets_to_reward_relationships ot
        INNER JOIN @results_schema.cg_concept_set tcs ON tcs.concept_id = ot.ingredient_concept_id AND tcs.is_excluded = 0
        INNER JOIN  @results_schema.cg_cohort_concept_set tccs ON tcs.concept_set_id = tccs.concept_set_id

        WHERE lower(CONCAT(ot.drug_name, ot.drug_id)) LIKE @search
        ORDER BY drug_name LIMIT 10
      ) cd
      {@existing_ids != ''} ?{
      UNION

       SELECT
        SELECT
          tccs.cohort_definition_id, ot.drug_name
        FROM @open_targets_database_schema.opentargets_to_reward_relationships ot
        INNER JOIN @results_schema.cg_concept_set tcs ON tcs.concept_id = ot.ingredient_concept_id AND tcs.is_excluded = 0
        INNER JOIN  @results_schema.cg_cohort_concept_set tccs ON tcs.concept_set_id = tccs.concept_set_id
      WHERE tccs.cohort_definition_id IN (@existing_ids)
      }

      "
      result <- model$queryDb(sql,
                              search = tolower(search),
                              open_targets_database_schema = appConfig$openTargetsDatabaseSchema,
                              existing_ids = existingValues)

      if (nrow(result) == 0)
        return(NULL)

      choices <- result$cohortDefinitionId
      names(choices) <- result$drugName
      return(choices)

    }

    shiny::observeEvent(input$openTargetsIngredientSearchBox, {
      shiny::req(input$openTargetsIngredientSearchBox)
      if (nchar(input$openTargetsIngredientSearchBox) > 2) {
        choices <- openTargetsIngredientSearch(input$openTargetsIngredientSearchBox, input$openTargetsIngredientSearch)
        shiny::updateSelectizeInput(
          session = session,
          inputId = "openTargetsIngredientSearch",
          server = TRUE,
          choices = choices,
          selected = input$openTargetsIngredientSearch
        )
      }
    })

    # Concepts to exclude from search
    excludedConcepts <- shiny::reactive({
      concepts <- c()
      if (length(input$excludedConcepts) > 0) {
        tryCatch({
          concepts <- unlist(lapply(strsplit(input$excludedConcepts, ","), as.numeric))
        }, error = function(...) { })
      }
      return(concepts)
    })

    getMainTableParams <- shiny::eventReactive(input$genResults, {
      params <- list(benefitThreshold = input$cutrange1[2],
                     lowerBenefitThereshold = input$cutrange1[1],
                     riskThreshold = input$cutrange2,
                     pValueCut = input$pCut,
                     requiredBenefitSources = input$requiredDataSources,
                     filterByMeta = input$filterThreshold == "Meta analysis",
                     calibrated = TRUE,
                     benefitCount = input$scBenefit,
                     riskCount = input$scRisk,
                     outcomeCohorts = input$outcomeSearch,
                     targetCohorts = c(input$targetSearch, input$openTargetsIngredientSearch),
                     excludedTargetCohorts = input$excludedTargetSearch,
                     excludedOutcomeCohorts = input$excludedOutcomeSearch,
                     analysisId = input$analysisId,
                     targetSearchText = input$targetSearchText,
                     outcomeSearchText = input$outcomeSearchText,
                     excludedConcepts = excludedConcepts(),
                     exposureClasses = c())

      return(params)
    })

    getMainTableCount <- shiny::reactive({
      params <- getMainTableParams()
      res <- do.call(model$getFilteredTableResultsCount, params)
      return(res)
    })

    mainTablePage <- shiny::reactiveVal(1)
    mainTableMaxPages <- shiny::reactive({
      recordCount <- getMainTableCount()
      ceiling(recordCount / as.integer(input$mainTablePageSize))
    })

    shiny::observeEvent(input$mainTableNext, {
      mainTablePage <- mainTablePage() + 1
      if (mainTablePage <= mainTableMaxPages()) {
        mainTablePage(mainTablePage)
      }

    })
    shiny::observeEvent(input$mainTablePrevious, {
      mainTablePage <- mainTablePage() - 1
      if (mainTablePage > 0) {
        mainTablePage(mainTablePage)
      }
    })

    getMainTablePage <- shiny::reactive({
      return(mainTablePage())
    })

    output$mainTablePage <- shiny::renderUI({
      numPages <- mainTableMaxPages()
      suppressWarnings(obj <- shiny::selectizeInput(ns("mainTablePage"), "Page", choices = 1:numPages, selected = mainTablePage()))
      obj
    })

    shiny::observeEvent(input$mainTablePage, {
      mainTablePage(as.integer(input$mainTablePage))
    })

    output$mainTableNumPages <- shiny::renderText({
      recordCount <- getMainTableCount()
      numPages <- ceiling(recordCount / as.integer(input$mainTablePageSize))
      return(paste("Page", getMainTablePage(), "of", numPages))
    })

    output$mainTableCount <- shiny::renderText({
      res <- getMainTableCount()
      offset <- max(getMainTablePage() - 1, 0) * as.integer(input$mainTablePageSize) + 1
      endNum <- min(offset + as.integer(input$mainTablePageSize) - 1, res)
      str <- paste("Displaying", offset, "to", endNum, "of", res, "results")
      return(str)
    })

    # Subset of results for harm, risk and treatement categories
    # Logic: either select everything or select a user defined subset
    mainTableReac <- shiny::reactive({
      params <- getMainTableParams()
      params$limit <- input$mainTablePageSize
      params$offset <- max(getMainTablePage() - 1, 0) * as.integer(input$mainTablePageSize)
      params$orderByCol <- input$mainTableSortBy
      params$ascending <- input$mainTableOrderAscending
      do.call(model$getFilteredTableResults, params)
    })

    output$mainTable <- DT::renderDataTable({
      df <- mainTableReac()
      if (length(df$i2)) {
        df$i2 <- formatC(df$i2, digits = 2, format = "f")
      }

      colnames(df) <- SqlRender::camelCaseToTitleCase(colnames(df))

      table <- DT::datatable(
        df, selection = "single", options = list(dom = 't', pageLength = input$mainTablePageSize, ordering = F),
        rownames = FALSE
      )
      return(table)
    })

    # This links the app components together
    selectedExposureOutcome <- shiny::reactive({
      ids <- input$mainTable_rows_selected
      filtered1 <- mainTableReac()

      if (!length(ids)) {
        return(NULL)
      }

      filtered2 <- filtered1[ids,]
      filtered2$calibrationType <- "none"
      filtered2$analysisId <- input$analysisId
      return(filtered2)
    })

    ingredientConetpInput <- shiny::reactive({
      selected <- selectedExposureOutcome()
      if (is.null(selected))
        return(data.frame())
      model$getExposureCohortConceptSets(selected$targetCohortId)
    })

    conditionConceptInput <- shiny::reactive({
      selected <- selectedExposureOutcome()
      if (is.null(selected))
        return(data.frame())
      model$getOutcomeCohortConceptSets(selected$outcomeCohortId)
    })

    selectedCohort <- shiny::reactive({
      selected <- selectedExposureOutcome()

      if (model$config$exposureDashboard) {
        cohortId <- selected$targetCohortId
        selectedOutcomeType <- 0 # TODO
        conceptSet <- ingredientConetpInput()
      } else {
        cohortId <- selected$outcomeCohortId
        selectedOutcomeType <- 0
        conceptSet <- conditionConceptInput()
      }

      list(
        cohortDefinitionId = cohortId,
        isExposure = model$config$exposureDashboard,
        selectedOutcomeType = selectedOutcomeType,
        conceptSet = conceptSet,
        analysisId = input$analysisId
      )
    })

    controlSelector <- if (isTRUE(appConfig$dashboardType == "outcome")) "outcomeCohortId" else "targetCohortId"

    metaAnalysisTableServer("metaTable", model, selectedExposureOutcome)
    forestPlotServer("forestPlot", model, selectedExposureOutcome)
    calibrationPlotServer("calibrationPlot", model, selectedExposureOutcome, dashboardControlSelector = controlSelector)
    timeOnTreatmentServer("timeOnTreatment", model, selectedExposureOutcome)
    tabPanelTimeOnTreatment <- tabPanel("Time on treatment", boxPlotModuleUi(ns("timeOnTreatment")))
    shiny::appendTab(inputId = "outcomeResultsTabs", tabPanelTimeOnTreatment)
    timeToOutcomeServer("timeToOutcome", model, selectedExposureOutcome)
    tabPanelTimeToOutcome <- tabPanel("Time to outcome", boxPlotModuleUi(ns("timeToOutcome")))
    shiny::appendTab(inputId = "outcomeResultsTabs", tabPanelTimeToOutcome)

    fullDataDownload <- shiny::reactive({
      model$getFilteredTableResults(benefitThreshold = input$cutrange1[2],
                                    lowerBenefitThereshold = input$cutrange1[1],
                                    riskThreshold = input$cutrange2,
                                    pValueCut = input$pCut,
                                    filterByMeta = input$filterThreshold == "Meta analysis",
                                    calibrated = TRUE,
                                    benefitCount = input$scBenefit,
                                    riskCount = input$scRisk)
    })

    output$treatmentOutcomeStr <- shiny::renderText({
      s <- selectedExposureOutcome()
      return(paste(s$targetCohortName, " - ", s$outcomeCohortName))
    })

    output$downloadData <- shiny::downloadHandler(
      filename = function() {
        paste0(appConfig$shortName, '-full_results', input$cutrange1[2], '-', input$cutrange2, '.csv')
      },
      content = function(file) {
        write.csv(fullDataDownload(), file, row.names = FALSE)
      }
    )

    output$downloadFullData <- shiny::downloadHandler(
      filename = function() {
        paste0(appConfig$shortName, '-export.csv')
      },
      content = function(file) {
        data <- model$getFullDataSet()
        write.csv(data, file, row.names = FALSE)
      })

    getNegativeControls <- shiny::reactive({
      model$getNegativeControls()
    })

    output$downloadControls <- downloadHandler(
      filename = function() {
        paste0(appConfig$shortName, '-negative-controls.csv')
      },
      content = function(file) {
        write.csv(getNegativeControls(), file, row.names = FALSE)
      })

    getIndications <- shiny::reactive({
      model$getMappedAssociations()
    })

    output$downloadIndications <- shiny::downloadHandler(
      filename = function() {
        paste0(appConfig$shortName, '-indications.csv')
      },
      content = function(file) {
        write.csv(getIndications(), file, row.names = FALSE)
      }
    )

    # Subset without limit
    mainTableDownload <- shiny::reactive({
      params <- getMainTableParams()
      do.call(model$getFilteredTableResults, params)
    })

    output$downloadFullTable <- shiny::downloadHandler(
      filename = function() {
        paste0(appConfig$shortName, '-filtered-', input$cutrange1[2], '-', input$cutrange2, '.csv')
      },
      content = function(file) {
        write.csv(mainTableDownload(), file, row.names = FALSE)
      }
    )


    output$targetCohortTable <- reactable::renderReactable({
      tbl <- model$getTargetCohortInfo(appConfig$targetCohortIds)
      colnames(tbl) <- SqlRender::camelCaseToTitleCase(colnames(tbl))
      reactable::reactable(
        tbl,
        columns = list(
          "N Estimates" = reactable::colDef(format = reactable::colFormat(separators = TRUE))
        )
      )
    })
  })
}

#' Create Dashboard Config
#' @param resultsDatabaseSchema name of the dashboard
#' @param dashboardName name of the dashboard
#' @param dataSources to be used (optional, can be set on dashboard load)
#' @param ... additional parmeters to be user defined, not used by app
#' @export
createDashboardConfig <- function(resultsDatabaseSchema,
                                  dashboardName = "SCC dashboard",
                                  shortName = "SCC",
                                  dashboardType = "exposure",
                                  targetCohortIds,
                                  ...) {
  return(list(
    dashboardName = "SCC dashboard",
    shortName = shortName,
    dataSources = list(),
    resultsDatabaseSchema = resultsDatabaseSchema,
    dashboardType = dashboardType,
    targetCohortIds = targetCohortIds,
    ...
  ))
}

#' @title
#' Launch the REWARD Shiny app dashboard
#' @description
#' Launches a Shiny app for a given configuration file
#' @param connectionDetails
#' @param dashboardConfig see createDashboardConfig
#'
#' @export
launchDashboard <- function(connectionDetails, dashboardConfig) {
  connectionHandler <- ResultModelManager::PooledConnectionHandler$new(connectionDetails)
  model <- SccDataModel$new(connectionHandler, dashboardConfig)

  cli::cli_alert_info("Launching dashboard")
  # data sources available
  ds <- model$getDataSources()
  dashboardConfig$dataSources <- ds$databaseId
  names(dashboardConfig$dataSources) <- ds$cdmSourceAbbreviation

  # Settings available
  aRes <- model$getAnalysisSettings()
  choices <- aRes$analysisId
  names(choices) <- paste(aRes$description)
  dashboardConfig$analysisSettings <- choices

  serverFunc <- function(input, output, session) {
    sccModule(model = model, appConfig = dashboardConfig)
  }

  shiny::shinyApp(server = serverFunc,
                  ui = sccUi(dashboardConfig = dashboardConfig),
                  enableBookmarking = "url")
}
