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
sccUi <- function(id = "scc-module", dashboardConfig) {
  ns <- shiny::NS(id)
  # This hides the outcome exporues/result pairing
  metaDisplayCondtion <- "typeof input.mainTable_rows_selected  !== 'undefined' && input.mainTable_rows_selected.length > 0"

  paramUi <- shinydashboard::box(

    shiny::column(
      width = 4,
      shiny::sliderInput(inputId = ns("cutrange1"), "Benefit Threshold:", min = 0.1, max = 0.9, step = 0.1, value = c(0.2, 0.5)),
      shiny::sliderInput(inputId = ns("cutrange2"), "Risk Threshold:", min = 1.1, max = 2.5, step = 0.1, value = 2),
      shiny::sliderInput(inputId = ns("pCut"), "P-value cut off:", min = 0.0, max = 1.0, step = 0.01, value = 0.05),
    ),
    shiny::column(
      width = 4,
      shiny::sliderInput(
        inputId = ns("scBenefit"),
        "Minimum sources with self control benefit:",
        min = 0,
        max = length(dashboardConfig$dataSources),
        step = 1,
        value = 1),
      shiny::sliderInput(
        ns("scRisk"),
        "Maximum sources with self control risk:",
        min = 0,
        max = length(dashboardConfig$dataSources),
        step = 1,
        value = 0
      )
    ),
    shiny::column(
      width = 4,
      shiny::radioButtons(ns("filterThreshold"), "Threshold benefit by:", c("Data sources", "Meta analysis")),
      # shiny::checkboxInput(ns("calibrated"), "Threshold with empirically calibrated IRR", TRUE),
      shiny::selectInput(
        inputId = ns("analysisId"),
        label = "Analysis setting:",
        choices = dashboardConfig$analysisSettings
      ),
      shiny::selectInput(
        inputId = ns("requiredDataSources"),
        label = "Select required data sources for benefit:",
        choices = dashboardConfig$dataSources,
        multiple = TRUE
      )
    ),
    width = 12,
    title = "Benfit/Risk parameters",
    collapsible = TRUE
  )

  filterBox <- shinydashboard::box(
    shiny::fluidRow(
      shiny::column(
        shiny::selectizeInput(inputId = ns("outcomeSearch"), label = "Filter outcomes", choices = c(), multiple = TRUE),
        shiny::tags$script(shiny::HTML(sprintf("
        $(document).on('keyup', '#%s-outcomeSearch-selectized', function(e){
          var val = $(this).val();
          Shiny.setInputValue('%s', val, {priority: 'event'});
        });
      ", id, ns("outcomeSearchBox")))),
        width = 6
      ),
      shiny::column(
        #,
        shiny::selectizeInput(inputId = ns("targetSearch"), label = "Filter exposures", choices = c(), multiple = TRUE),
        shiny::tags$script(shiny::HTML(sprintf("
        $(document).on('keyup', '#%s-targetSearch-selectized', function(e){
          var val = $(this).val();
          Shiny.setInputValue('%s', val, {priority: 'event'});
        });
      ", id, ns("targetSearchBox")))),
        width = 6
      )
    ),
    shiny::checkboxInput(ns("advancedSearch"), label = "Show advanced search options"),
    shiny::conditionalPanel(
      condition = "input.advancedSearch",
      ns = ns,
      shiny::h4("Exclusion"),
      shiny::fluidRow(
        shiny::column(
          #shiny::textAreaInput(inputId = ns("excludedConcepts"), label = "Exclude concept ids", NULL),
          #shiny::tags$p("Excludes and child concepts of specified concept ids. Separate with comma"),
          shiny::selectizeInput(inputId = ns("excludedTargetSearch"), label = "Exclude exposure cohorts", choices = c(), multiple = TRUE),
          shiny::tags$script(shiny::HTML(sprintf("
        $(document).on('keyup', '#%s-excludedTargetSearch-selectized', function(e){
          var val = $(this).val();
          Shiny.setInputValue('%s', val, {priority: 'event'});
        });
      ", id, ns("excludedTargetSearchBox")))),
          width = 6
        ),
        shiny::column(
          shiny::selectizeInput(inputId = ns("excludedOutcomeSearch"), label = "Exclude outcomes cohorts", choices = c(), multiple = TRUE),
          shiny::tags$script(shiny::HTML(sprintf("
        $(document).on('keyup', '#%s-excludedOutcomeSearch-selectized', function(e){
          var val = $(this).val();
          Shiny.setInputValue('%s', val, {priority: 'event'});
        });
      ", id, ns("excludedOutcomeSearchBox")))),
          width = 6
        )
      ),
      shiny::h4("Free text"),
      shiny::fluidRow(
        shiny::column(shiny::p("Use free text strings to filter outcomes and targets (e.g. 'ATC' or [PL])"), width = 12),
        shiny::column(
          shiny::textInput(inputId = ns("outcomeSearchText"), label = "Outcomes filter string", placeholder = "search"),
          width = 6
        ),
        shiny::column(
          shiny::textInput(inputId = ns("targetSearchText"), label = "Exposure filter string", placeholder = "search"),
          width = 6
        )
      ),
      if (length(dashboardConfig$openTargetsDatabaseSchema)) {
        openTargetsSearchUiBlock(id = ns("openTargetsSearch"))
      }
    ),
    shiny::fluidRow(
      shiny::column(
        width = 12,
        shiny::actionButton(inputId = ns("genResults"), label = "Get Results")
      )
    ),
    width = 12,
    title = "Filter Cohorts",
    collapsible = TRUE
  )

  mainResults <- shiny::conditionalPanel(
    condition = "input.genResults > 0",
    ns = ns,
    shinydashboard::box(
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
  )

  rPanel <- shiny::conditionalPanel(
    condition = metaDisplayCondtion,
    ns = ns,
    shinydashboard::box(
      shiny::HTML(paste("<h4 id='mainR'>", shiny::textOutput(ns("treatmentOutcomeStr")), "</h4>")),
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

  aboutTab <- shiny::fluidPage(
    shinydashboard::box(
      width = 6,
      title = "Data sources",
      shinycssloaders::withSpinner(reactable::reactableOutput(outputId = ns("dataSourceTable"))
      )
    ),
    shinydashboard::box(
      shiny::p(dashboardConfig$description),
      shiny::p("Click the dashboard option to see the results. The sidebar options allow filtering of results based on risk and benift IRR thresholds"),
      shiny::downloadButton(
        ns("downloadData"),
        "Download filtered results as a csv"),
      shiny::downloadButton(
        ns("downloadFullData"),
        "Download full results"),
      width = 6,
      title = paste("About this dashboard -", dashboardConfig$dashboardName
      )
    ),
    shinydashboard::box(
      title = "Target cohorts",
      width = 12,
      shinycssloaders::withSpinner(reactable::reactableOutput(outputId = ns("targetCohortTable")))
    )
  )


  tabs <- list(
    shinydashboard::tabItem(tabName = "about", aboutTab),
    shinydashboard::tabItem(tabName = "results", shiny::fluidRow(paramUi, filterBox, mainResults, rPanel))
  )

  body <- shinydashboard::dashboardBody(
    do.call(shinydashboard::tabItems, tabs)
  )

  sidebar <- shinydashboard::dashboardSidebar(
    shinydashboard::sidebarMenu(
      id = ns("sidebarMenu"),
      shinydashboard::menuItem("About", tabName = "about", icon = shiny::icon("rectangle-list")),
      shinydashboard::menuItem("Results", tabName = "results", icon = shiny::icon("table"))
      #shiny::bookmarkButton()
    )
  )

  appTitle <- paste(dashboardConfig$dashboardName)
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
