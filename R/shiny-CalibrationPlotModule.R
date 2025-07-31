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

#' UI for calibarion plot - uses namespacing
calibrationPlotUi <- function(id,
                              figureTitle = "Figure 1.",
                              figureText = "Plot of calibration of effect estimates. Blue dots are negative control effect estimates.") {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(
      shiny::column(width = 2),
      shiny::column(
        shinycssloaders::withSpinner(
          shiny::div(
            style = "max-width: 1000px; min-width: 330px; margin-left: auto; margin-right: auto; aspect-ratio: 16/9;",
            shiny::plotOutput(ns("calibrationPlot"), width = "100%", height = "100%"))
        ),
        shinycssloaders::withSpinner(reactable::reactableOutput(ns("nullDistribution"))), width = 8)
    ),
    shiny::div(
      shiny::selectInput(
        ns("databaseSelection"),
        label = "Select data source",
        choices = c()
      ),
      shiny::strong(figureTitle),
      shiny::p(figureText),
      shiny::downloadButton(ns("downloadCalibrationPlot"), "Save")
    )
  )
}

#' Calibration plot siny server module
#' @description
#' Display calibration plots for reward cohorts based on data available in system
#' Also shows Expected Absolute Systematic Error
#'
#' @param id                shiny namespace should be consistent with UI
#' @param model             SccDataModel R6 class instance
#' @param selectedCohort    cohort object reactive
#'
#' @export
calibrationPlotServer <- function(id, model, selectedCohort, dashboardControlSelector = "targetCohortId") {
  checkmate::assertR6(model, "SccDataModel")
  checkmate::assert(shiny::is.reactive(selectedCohort))

  server <- shiny::moduleServer(id, function(input, output, session) {
    dataSources <- model$getDataSources()
    shiny::observe({
      dataSourceChoices <- c(dataSources$databaseId, 'meta-analysis')
      names(dataSourceChoices) <- c(dataSources$cdmSourceAbbreviation, 'Meta Analysis')
      shiny::updateSelectInput(inputId = "databaseSelection", choices = dataSourceChoices, selected = 'meta-analysis')
    })


    getCalibrationPlot <- shiny::reactive({
      shiny::validate(shiny::need(!is.null(input$databaseSelection), message = "database must be selected"))
      cohort <- selectedCohort()

      plot <- ggplot2::ggplot()
      if (!is.null(cohort)) {
        negatives <- model$getNegativeControlSccResults(cohort[dashboardControlSelector], databaseId = input$databaseSelection)

        if (nrow(negatives)) {
          plotNegatives <- negatives[negatives$rr > 0,]
          plot <- EmpiricalCalibration::plotCalibrationEffect(logRrNegatives = log(plotNegatives$rr),
                                                              xLimits = c(min(0.25, plotNegatives$rr), max(10, plotNegatives$rr)),
                                                              yLimits = c(0.0, max(1.5, exp(plotNegatives$seLogRr))),
                                                              seLogRrNegatives = plotNegatives$seLogRr)
        }
      }
      return(plot)
    })


    output$calibrationPlot <- shiny::renderPlot({
      getCalibrationPlot()
    })


    nullDistData <- shiny::reactive({
      cohort <- selectedCohort()
      negatives <- model$getNegativeControlSccResults(cohort[dashboardControlSelector], databaseId = input$databaseSelection)
      subset <- negatives |> dplyr::filter(.data$analysisId == cohort$analysisId &
                                             !is.na(rr) &
                                             !is.null(rr))
      null <- EmpiricalCalibration::fitNull(log(subset$rr), subset$seLogRr)
      systematicError <- EmpiricalCalibration::computeExpectedAbsoluteSystematicError(null)
      df <- data.frame(
        "databaseId" = input$databaseSelection,
        "mean" = round(exp(null[["mean"]]), 3),
        "sd" = round(exp(null[["sd"]]), 3),
        "EASE" = round(systematicError, 3),
        "n" = nrow(subset)
      )
      return(df)
    })

    getNullDistTable <- shiny::reactive({
      nullDistData() |>
        dplyr::select("databaseId", "n", "mean", "sd", "EASE")
    })

    output$nullDistribution <- reactable::renderReactable({
      shiny::validate(shiny::need(!is.null(input$databaseSelection), message = "database must be selected"))
      nullDist <- getNullDistTable()
      colnames(nullDist) <- SqlRender::camelCaseToTitleCase(colnames(nullDist))
      reactable::reactable(nullDist)
    })

    output$downloadCalibrationPlot <- shiny::downloadHandler(
      filename = function() {
        "calibration-plot.png"
      },
      content = function(file) {
        ggplot2::ggsave(file, plot = getCalibrationPlot(), device = "png")
      }
    )
  })

  return(server)
}
