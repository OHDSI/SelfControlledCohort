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

  label <- paste0("IRR= ", round(table$rr * 1, 2),
                  "; 95% CI= (", round(table$lb95, 2), " - ", round(table$ub95, 2), ")")
  # factor to ensure meta-analysis goes last
  table$sourceName <- factor(table$sourceName, level = rev(table$sourceName))

  table <- table |>
    dplyr::mutate(color = dplyr::if_else(.data$databaseId == "meta-analysis", "firebrick", "steelblue"))

  rangeMin <- min(table$lb95, na.rm = TRUE)
  rangeMax <- max(table$ub95, na.rm = TRUE)
  minExp <- floor(log2(rangeMin))
  maxExp <- ceiling(log2(rangeMax))
  pow2Breaks <- round(2^(minExp:maxExp), 3)

  plot <- ggplot2::ggplot(
    table,
    ggplot2::aes(
      y = sourceName,
      x = rr,
      color = color,
      xmin = lb95,
      xmax = ub95,
      label = label
    )
  ) +
    ggplot2::geom_pointrange() +
    #ggplot2::geom_text(vjust = 0, nudge_y = 0.2, size = 3) +
    ggplot2::geom_text(
      ggplot2::aes(label = label, size = 5.2),
      size = 3,
      nudge_y = -0.1,
      color = "black"
    ) +
    ggplot2::geom_errorbarh(height = 0.1) +
    ggplot2::geom_vline(xintercept = 1.0, linetype = 2) +
    ggplot2::ylab("Database") +
    ggplot2::scale_x_continuous(trans = "log2", breaks = pow2Breaks, labels = pow2Breaks) +
    ggplot2::xlab("Relative Risk") +
    ggplot2::theme(text = ggplot2::element_text(size = 15), legend.position = "none")
  return(plot)
}

forestPlotUi <- function(id) {
  shiny::tagList(
    shinycssloaders::withSpinner(
      shiny::div(
        style = "max-width: 1000px; min-width: 330px; margin-left: auto; margin-right: auto; aspect-ratio: 16/9;",
        shiny::plotOutput(shiny::NS(id, "forestPlot"), width = "100%", height = "100%")
      )
    ),
    shiny::hr(),
    shiny::fluidRow(
      shinydashboard::box(
        shiny::strong("Figure 1."),
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
          selected = 1,
          multiple = FALSE),
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
        calibOpts <- if (length(input$forestPlotCalibrated) && !is.na(input$forestPlotCalibrated)) input$forestPlotCalibrated else 1
        res <- model$getForestPlotTable(exposureId, outcomeId, s$analysisId, as.logical(as.numeric(calibOpts)))
        return(res)
      }
      return(data.frame())
    })

    output$forestPlot <- shiny::renderPlot({
      df <- forestPlotTable()
      if (nrow(df) > 0) {
        return(forestPlot(df))
      }
    })

    output$downloadForestPlot <- shiny::downloadHandler(filename = function() {
      s <- selectedExposureOutcome()
      treatment <- s$targetCohortId
      outcome <- s$outcomeCohortId
      paste0(model$resultsSchema, '-forest-plot-', treatment, "-", outcome, '.png')
    }, content = function(file) {
      df <- forestPlotTable()
      ggplot2::ggsave(file, plot = forestPlot(df), device = "png")
    }
    )
  })
  return(server)
}
