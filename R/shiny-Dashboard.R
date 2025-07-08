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
sccModule <- function(id = "scc-module", model) {
  cli::cli_alert_info("Loading scc module")
  appConfig <- model$config
  ns <- shiny::NS(id)
  shiny::moduleServer(id, function(input, output, session) {

    dataSourceInfo <- shiny::reactive({
      cli::cli_alert_info("Getting data sources")
      model$getDataSources()
    })
    output$dataSourceTable <- reactable::renderReactable({
      tbl <- dataSourceInfo() |> dplyr::select("databaseId", "cdmSourceAbbreviation", "cdmVersion")
      colnames(tbl) <- SqlRender::camelCaseToTitleCase(colnames(tbl))
      reactable::reactable(tbl)
    })

    output$requiredDataSources <- shiny::renderUI({
      cli::cli_alert_info("render data sources")
      dsInfo <- dataSourceInfo()
      dsChoices <- dsInfo$databaseId
      names(dsChoices) <- dsInfo$cdmSourceAbbreviation
      shinyWidgets::pickerInput(ns("requiredDataSources"),
                                label = "Select required data sources for benefit:",
                                choices = dsChoices,
                                options = shinyWidgets::pickerOptions(actionsBox = TRUE),
                                multiple = TRUE)
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
                     outcomeCohorts = input$outcomeCohorts,
                     targetCohorts = input$targetCohorts,
                     analysisId = input$analysisId,
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

    exposureCohorts <- shiny::reactive({
      model$getExposureCohorts()
    })

    outcomeCohorts <- shiny::reactive({
      model$getOutcomeCohorts()
    })

    shiny::observe({
      ocC <- outcomeCohorts()
      outcomeCohortChoices <- ocC$cohortDefinitionId
      names(outcomeCohortChoices) <- ocC$cohortName

      shiny::updateSelectizeInput(session, "outcomeCohorts", choices = outcomeCohortChoices, server = TRUE)

      ecC <- exposureCohorts()
      exposureCohortChoices <- ecC$cohortDefinitionId
      names(exposureCohortChoices) <- ecC$cohortName

      shiny::updateSelectizeInput(session, "targetCohorts", choices = exposureCohortChoices, server = TRUE)
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

    metaAnalysisTableServer("metaTable", model, selectedExposureOutcome)
    forestPlotServer("forestPlot", model, selectedExposureOutcome)
    calibrationPlotServer("calibrationPlot", model, selectedExposureOutcome)
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
  })
}

#' Create Dashboard Config
#' @param resultsDatabaseSchema name of the dashboard
#' @param dashboardName name of the dashboard
#' @param dataSources to be used (optional, can be set on dashboard load)
#' @export
createDashboardConfig <- function(resultsDatabaseSchema,
                                  dashboardName = "SCC dashboard",
                                  shortName = "SCC") {
  return(list(
    dashboardName = "SCC dashboard",
    shortName = shortName,
    dataSources = list(),
    resultsDatabaseSchema = resultsDatabaseSchema
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
  dashboardConfig$dataSources <- model$getDataSources()$databaseId

  # Settings available
  aRes <- model$queryDb("SELECT * from @results_schema.scc_analysis_setting")
  choices <- aRes$analysisId
  names(choices) <- paste(aRes$analysisId, "-", aRes$description)
  dashboardConfig$analysisSettings <- choices

    serverFunc <- function(input, output, session) {
      sccModule(model = model)
    }

  shiny::shinyApp(server = serverFunc,
                  ui = sccUi(dashboardConfig = dashboardConfig),
                  enableBookmarking = "url")
}
