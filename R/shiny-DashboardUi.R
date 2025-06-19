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

#' Ui for rewardb dashboard
#' @param request shiny request object
#' @export
sccUi <- function(id = "Reward",
                     appConfig) {
  ns <- shiny::NS(id)
  # This hides the outcome exporues/result pairing
  metaDisplayCondtion <- "typeof input.mainTable_rows_selected  !== 'undefined' && input.mainTable_rows_selected.length > 0"

  filterBox <- shinydashboard::box(
    shinydashboard::box(
      shiny::selectizeInput(
        inputId = ns("targetCohorts"),
        label = "Drug exposures:",
        choices = NULL,
        multiple = TRUE),
      shiny::selectizeInput(
        inputId = ns("outcomeCohorts"),
        label = "Disease outcomes:",
        choices = NULL,
        multiple = TRUE)),
    shinydashboard::box(
      shiny::selectizeInput(
        inputId = ns("exposureClass"),
        label = "Drug exposure classes:",
        choices = NULL,
        multiple = TRUE),
      shinyWidgets::pickerInput(
        inputId = ns("outcomeCohortTypes"),
        "Outcome Cohort Types:",
        choices = c("ATLAS defined" = 3, "Inpatient" = 1, "Two diagnosis codes" = 0, "One diagnosis code" = 2),
        selected = c(),
        options = shinyWidgets::pickerOptions(
          actionsBox = TRUE,
          noneSelectedText = "Filter by subset"
        ),
        multiple = TRUE),
      shiny::textAreaInput(inputId = ns("excludedConcepts"), label = "Exclude concept ids", NULL),
      shiny::tags$p("Excludes and child concepts of specified concept ids. Separate with comma"),
      width = 6
    ),
    width = 12,
    title = "Filter Cohorts",
    collapsible = TRUE
  )

  mainResults <- shinydashboard::box(
    shiny::fluidRow(
      shiny::column(
        width = 2,
        shiny::uiOutput(ns("mainTablePage"))
      ),
      shiny::column(
        width = 6,
        shiny::selectInput(
          inputId = ns("mainTableSortBy"),
          "Sort by column",
          choices = list(
            "Outcome id" = "OUTCOME_COHORT_ID",
            "Exposure id" = "TARGET_COHORT_ID",
            "Exposure name" = "TARGET_COHORT_NAME",
            "Outcome name" = "OUTCOME_COHORT_NAME",
            "I-squared" = "I2",
            "IRR" = "META_RR",
            "Sources with scc risk" = "RISK_COUNT",
            "Sources with scc benefit" = "BENEFIT_COUNT"),
          selected = "META_RR"
        )
      ),
      shiny::column(
        width = 2,
        shiny::radioButtons(ns("mainTableOrderAscending"), "", c("Ascending" = "ASC", "Descending" = "DESC"))),
      shiny::column(
        width = 2,
        shiny::selectInput(
          ns("mainTablePageSize"),
          "Show per page",
          choices = c(5, 10, 15, 20, 25, 50, 100), selected = 10
        )
      )
    ),
    shinycssloaders::withSpinner(DT::dataTableOutput(ns("mainTable"))),
    shiny::hr(),
    shiny::fluidRow(
      shiny::column(
        width = 4,
        shiny::textOutput(ns("mainTableCount")),
        shiny::actionButton(ns("mainTablePrevious"), "Previous Page")
      ),
      shiny::column(width = 6),
      shiny::column(
        width = 2,
        shiny::textOutput(ns("mainTableNumPages")),
        shiny::actionButton(ns("mainTableNext"), "Next Page"))),
    shiny::hr(),
    shiny::downloadButton(ns("downloadFullTable"), "Download"),
    width = 12
  )

  rPanel <- shiny::conditionalPanel(
    condition = metaDisplayCondtion,
    ns = ns,
    shinydashboard::box(
      shiny::HTML(paste("<h4 id='mainR'>", textOutput(ns("treatmentOutcomeStr")), "</h4>")),
      shiny::tabsetPanel(
        id = ns("outcomeResultsTabs"),
        type = "pills",
        shiny::tabPanel("Detailed results", metaAnalysisTableUi(ns("metaTable"))),
        shiny::tabPanel("Forest plot", forestPlotUi(ns("forestPlot"))),
        shiny::tabPanel("Calibration plot",
                        calibrationPlotUi(ns("calibrationPlot"),
                                          figureTitle = "Figure 2."))),
      width = 12)
  )

  aboutTab <- shiny::tagList(
    shinydashboard::box(
      shiny::includeHTML(system.file("html", "about.html", package = "Reward")),
      width = 12,
      title = paste("Real World Assessment and Research of Drug performance (REWARD)")),
    shinydashboard::box(
      width = 6,
      title = "Data sources",
      shinycssloaders::withSpinner(reactable::reactableOutput(outputId = ns("dataSourceTable"))
      )
    ),
    shinydashboard::box(
      shiny::p(appConfig$description),
      shiny::p("Click the dashboard option to see the results. The sidebar options allow filtering of results based on risk and benift IRR thresholds"),
      shiny::downloadButton(
        ns("downloadData"),
        "Download filtered results as a csv"),
      shiny::downloadButton(
        ns("downloadFullData"),
        "Download full results"),
      width = 6,
      title = paste("About this dashboard -", appConfig$dashboardName
      )
    )
  )


  tabs <- list(
    shinydashboard::tabItem(tabName = "about", aboutTab),
    shinydashboard::tabItem(tabName = "results", shiny::fluidRow(filterBox, mainResults, rPanel))
  )

  if (isTRUE(appConfig$showCohortDiagnostics)) {
    tabs[[3]] <- shinydashboard::tabItem(tabName = "cohortDiagnostics",
                                         OhdsiShinyModules::cohortDiagnosticsView(id = ns("cohortDiagnostics")))
  }

  body <- shinydashboard::dashboardBody(
    do.call(shinydashboard::tabItems, tabs)
  )

  sidebar <- shinydashboard::dashboardSidebar(
    shinydashboard::sidebarMenu(
      id = ns("sidebarMenu"),
      shinydashboard::menuItem("About", tabName = "about", icon = icon("rectangle-list")),
      shinydashboard::menuItem("Results", tabName = "results", icon = icon("table")),
      if (isTRUE(appConfig$showCohortDiagnostics)) {
        shinydashboard::menuItem("Cohort Diagnostics", tabName = "cohortDiagnostics", icon = icon("users"))
      } else {
        shiny::p()
      },
      shiny::sliderInput(ns("cutrange1"), "Benefit Threshold:", min = 0.1, max = 0.9, step = 0.1, value = c(0.2, 0.5)),
      shiny::sliderInput(ns("cutrange2"), "Risk Threshold:", min = 1.1, max = 2.5, step = 0.1, value = 2),
      shiny::sliderInput(ns("pCut"), "P-value cut off:", min = 0.0, max = 1.0, step = 0.01, value = 0.05),
      shiny::checkboxInput(ns("calibrated"), "Threshold with empirically calibrated IRR", TRUE),
      shiny::radioButtons(ns("filterThreshold"), "Threshold benefit by:", c("Data sources", "Meta analysis")),
      shiny::sliderInput(
        ns("scBenefit"),
        "Minimum sources with self control benefit:",
        min = 0,
        max = length(appConfig$dataSources),
        step = 1,
        value = 1),
      shiny::sliderInput(
        ns("scRisk"),
        "Maximum sources with self control risk:",
        min = 0,
        max = length(appConfig$dataSources),
        step = 1,
        value = 0
      ),
      shinycssloaders::withSpinner(shiny::uiOutput(ns("requiredDataSources"))),
      shiny::bookmarkButton()))

  appTitle <- paste(appConfig$dashboardName)
  # Put them together into a dashboardPage
  ui <- shinydashboard::dashboardPage(
    shinydashboard::dashboardHeader(
      title = appTitle,
      shiny::tags$li(
        class = "dropdown",
        style = "margin-top: 8px !important; margin-right : 5px !important")
    ),
    sidebar,
    body
  )
  return(ui)
}
