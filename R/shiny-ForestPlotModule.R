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

#' @title
#' Forest plot
#' @description
#' Create a forest plot
#' @param table data.frame with columns RR, LB_95, UB_95
#' @return ggplot plot
forestPlot <- function(table) {
  table$SOURCE_ID <- as.character(table$sourceId)
  label <- paste0("IRR= ", round(table$rr * 1, 2),
                  "; 95% CI= (", round(table$lb95, 2), " - ", round(table$ub95, 2), ")")
  plot <- ggplot2::ggplot(
    table,
    ggplot2::aes(
      y = factor(sourceName, level = rev(sourceName)),
      x = rr,
      color = sourceId,
      xmin = lb95,
      xmax = ub95,
      label = label
    )
  ) +
    ggplot2::geom_pointrange() +
    ggplot2::geom_text(vjust = 0, nudge_y = 0.2, size = 3) +
    ggplot2::geom_errorbarh(height = 0.1) +
    ggplot2::geom_vline(xintercept = 1.0, linetype = 2) +
    ggplot2::ylab("Database") +
    ggplot2::scale_x_continuous(trans = "log2") +
    ggplot2::xlab("Relative Risk") +
    ggplot2::theme(text = ggplot2::element_text(size = 11), legend.position = "none")
  return(plot)
}

forestPlotUi <- function(id) {
  shiny::tagList(
    shinycssloaders::withSpinner(plotly::plotlyOutput(shiny::NS(id, "forestPlot"), height = 500)),
    shiny::hr(),
    shiny::fluidRow(
      shinydashboard::box(
        strong("Figure 1."),
        paste("Forest plot of effect estimates from each database"),
        shiny::br(),
        shiny::downloadButton(
          shiny::NS(id, "downloadForestPlot"), "Save Plot"),
        width = 6
      ),
      shinydashboard::box(
        shinyWidgets::pickerInput(
          shiny::NS(id, "forestPlotCalibrated"),
          "Display:",
          choices = list(
            "Uncalibrated results" = 0,
            "Calibrated Results" = 1
          ),
          selected = c(0, 1),
          options = shinyWidgets::pickerOptions(
            actionsBox = TRUE,
            noneSelectedText = ""
          ),
          multiple = TRUE),
        width = 6)
    )
  )
}

forestPlotServer <- function(id, model, selectedExposureOutcome) {
  server <- shiny::moduleServer(id, function(input, output, session) {
    forestPlotTable <- shiny::reactive({
      s <- selectedExposureOutcome()
      exposureId <- s$targetCohortId
      outcomeId <- s$outcomeCohortId

      if (length(outcomeId) & length(exposureId)) {
        shiny::updateTabsetPanel(session, "mainPanel", "Detail")
        calibOpts <- if (length(input$forestPlotCalibrated)) input$forestPlotCalibrated else c(0, 1)
        res <- model$getForestPlotTable(exposureId, outcomeId, as.numeric(calibOpts))
        return(res)
      }
      return(data.frame())
    })

    output$forestPlot <- plotly::renderPlotly({
      df <- forestPlotTable()
      if (nrow(df) > 0) {
        return(plotly::ggplotly(forestPlot(df)))
      }
    })

    output$downloadForestPlot <- shiny::downloadHandler(filename = function() {
      s <- selectedExposureOutcome()
      treatment <- s$targetCohortId
      outcome <- s$outcomeCohortId
      paste0(model$schemaName, '-forest-plot-', treatment, "-", outcome, '.png')
    }, content = function(file) {
      df <- forestPlotTable()
      ggplot2::ggsave(file, plot = forestPlot(df), device = "png")
    }
    )
  })
  return(server)
}
