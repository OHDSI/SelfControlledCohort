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
sccModule <- function(id = "Reward", model) {
  appConfig <- model$config
  ns <- shiny::NS(id)
  shiny::moduleServer(id, function(input, output, session) {

    dataSourceInfo <- shiny::reactive({ model$getDataSources() })
    output$dataSourceTable <- reactable::renderReactable({
      tbl <- dataSourceInfo() %>% dplyr::select(sourceId, sourceName, cdmVersion)
      colnames(tbl) <- SqlRender::camelCaseToTitleCase(colnames(tbl))
      reactable::reactable(tbl)
    })

    output$requiredDataSources <- shiny::renderUI({
      shinyWidgets::pickerInput(ns("requiredDataSources"),
                                label = "Select required data sources for benefit:",
                                choices = dataSourceInfo()$sourceName,
                                options = shinyWidgets::pickerOptions(actionsBox = TRUE),
                                multiple = TRUE)
    })

    requiredBenefitSources <- shiny::reactive({
      dsi <- dataSourceInfo()
      dsi[dsi$sourceName %in% input$requiredDataSources,]$sourceId
    })

    # Concepts to exclude from search
    excludedConcepts <- shiny::reactive({
      concepts <- c()
      if (length(input$excludedConcepts) > 0) {
        tryCatch({
          concepts <- unlist(lapply(strsplit(input$excludedConcepts, ","), as.numeric))
        }, error = function(...) {})
      }
      return(concepts)
    })

    getMainTableParams <- shiny::reactive({
      exposureClassNames <- if (!appConfig$exposureDashboard & length(input$exposureClass)) strQueryWrap(input$exposureClass) else NULL
      outcomeTypes <- input$outcomeCohortTypes

      params <- list(benefitThreshold = input$cutrange1[2],
                     lowerBenefitThereshold = input$cutrange1[1],
                     riskThreshold = input$cutrange2,
                     pValueCut = input$pCut,
                     requiredBenefitSources = requiredBenefitSources(),
                     filterByMeta = input$filterThreshold == "Meta analysis",
                     outcomeCohortTypes = outcomeTypes,
                     calibrated = input$calibrated,
                     benefitCount = input$scBenefit,
                     riskCount = input$scRisk,
                     outcomeCohorts = input$outcomeCohorts,
                     targetCohorts = input$targetCohorts,
                     excludedConcepts = excludedConcepts(),
                     exposureClasses = exposureClassNames)

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
      names(outcomeCohortChoices) <- ocC$shortName

      shiny::updateSelectizeInput(session, "outcomeCohorts", choices = outcomeCohortChoices, server = TRUE)

      ecC <- exposureCohorts()
      exposureCohortChoices <- ecC$cohortDefinitionId
      names(exposureCohortChoices) <- ecC$shortName

      shiny::updateSelectizeInput(session, "targetCohorts", choices = exposureCohortChoices, server = TRUE)

      if (!appConfig$exposureDashboard) {
        shiny::updateSelectizeInput(session, "exposureClass", choices = model$getExposureClassNames(), server = TRUE)
      }
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
        analysisId = 1
      )
    })

    metaAnalysisTableServer("metaTable", model, selectedExposureOutcome)
    forestPlotServer("forestPlot", model, selectedExposureOutcome)
    calibrationPlotServer("calibrationPlot", model, selectedCohort)
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
                                    calibrated = input$calibrated,
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
        paste0(appConfig$short_name, '-negative-controls.csv')
      },
      content = function(file) {
        write.csv(getNegativeControls(), file, row.names = FALSE)
      })

    getIndications <- shiny::reactive({
      model$getMappedAssociations()
    })

    output$downloadIndications <- shiny::downloadHandler(
      filename = function() {
        paste0(appConfig$short_name, '-indications.csv')
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
        paste0(appConfig$short_name, '-filtered-', input$cutrange1[2], '-', input$cutrange2, '.csv')
      },
      content = function(file) {
        write.csv(mainTableDownload(), file, row.names = FALSE)
      }
    )


    # Add cohort diagnostics module if options present
    if (isTRUE(appConfig$showCohortDiagnostics)) {
      cdModuleVal <- shiny::reactiveVal(0)
      shiny::observeEvent(input$sidebarMenu, {
        if (input$sidebarMenu == "cohortDiagnostics" & cdModuleVal() == 0) {
          cdModuleVal(1)
          shiny::withProgress({
            cdSettings <- list(
              schema = model$resultsSchema,
              vocabularyDatabaseSchema = model$resultsSchema,
              cdTablePrefix = model$config$cdTablePrefix,
              cgTable = "cohort",
              databaseTable = "database"
            )

            OhdsiShinyModules::cohortDiagnosticsServer(id = "cohortDiagnostics",
                                                       connectionHandler = model$connection,
                                                       resultDatabaseSettings = cdSettings)
          }, message = "loading cohort diagnostics")
        }
      })
    }
  })
}

dashboardInstance <- function(input,
                              output,
                              session,
                              model = .GlobalEnv$.model) {

  shiny::onStop(function() { model$finalize() })
  rewardModule(model = model)
}

dashboardUi <- function(...) {
  rewardUi(appConfig = loadDashboardConfiguration(.GlobalEnv$dashboardConfigPath))
}

#' @title
#' Launch the REWARD Shiny app dashboard
#' @description
#' Launches a Shiny app for a given configuration file
#' @param appConfigPath path to configuration file. This is loaded in to the local environment with the appConfig variable
#'
#' @export
launchDashboard <- function(dashboardConfigPath,
                            configPath = NULL,
                            connectionDetails = NULL,
                            resultDatabaseSchema = NULL) {

  if (all(is.null(c(configPath, connectionDetails)))) {
    stop("Must specify config path or connectionDetails")
  }

  .GlobalEnv$dashboardConfigPath <- normalizePath(dashboardConfigPath)

  dashboardConfig <- loadDashboardConfiguration(dashboardConfigPath)
  if (is.null(connectionDetails)) {
    config <- loadGlobalConfiguration(configPath)
    connectionDetails <- config$connectionDetails
  }

  if (!is.null(connectionDetails) && connectionDetails$dbms == "sqlite") {
    resultDatabaseSchema <- "main"
    vocabularyDatabaseSchema <- "main"
  } else if (is.null(resultDatabaseSchema)) {
    resultDatabaseSchema <- dashboardConfig$shortName
    vocabularyDatabaseSchema <- config$vocabularySchema
  } else {
    vocabularyDatabaseSchema <- config$vocabularySchema
  }

  .GlobalEnv$.model <- DashboardDataModel$new(dashboardConfigPath = dashboardConfigPath,
                                              connectionDetails = connectionDetails,
                                              cemConnectionDetails = config$cemConnectionDetails,
                                              resultDatabaseSchema = resultDatabaseSchema,
                                              vocabularyDatabaseSchema = vocabularyDatabaseSchema,
                                              usePooledConnection = FALSE)

  shiny::shinyApp(server = dashboardInstance, dashboardUi, enableBookmarking = "url")
}
