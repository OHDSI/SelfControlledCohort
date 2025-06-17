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
#' Box Plot Distribution
#' @description
#' Create a boxplot
#' @param data data.frame
#' @return ggplot plot
boxPlotDist <- function(data) {
  if (nrow(data) == 0) {
    return(ggplot2::ggplot())
  }

  plot <- ggplot2::ggplot(data = data) +
    ggplot2::aes(x = sourceName,
                 ymin = min,
                 lower = p25,
                 middle = median,
                 upper = p75,
                 ymax = max,
                 average = mean,
                 sd = sd,
                 group = sourceName,
                 y = median) +
    ggplot2::geom_errorbar(size = 0.5) +
    ggplot2::geom_boxplot(stat = "identity", fill = rgb(0, 0, 0.8, alpha = 0.25), size = 0.2) +
    ggplot2::xlab("Data source") +
    ggplot2::ylab("Time in days")

  return(plot)
}

#' Returns a reference to a server function
#'
#' @param distStatsFunc call to model that takes treament id and outcome id and returns stats dataframe
#' @param caption text caption string
#' @param selectedExposureOutcome exposure outcome reactive
boxPlotModuleServer <- function(distStatsFunc, caption, selectedExposureOutcome) {
  serverFunction <- function(input, output, session) {
    getDistStats <- shiny::reactive({
      s <- selectedExposureOutcome()
      treatment <- s$targetCohortId
      outcome <- s$outcomeCohortId

      data <- distStatsFunc(exposureId = treatment, outcomeId = outcome, sourceIds = s$usedDataSources)

      return(data)
    })

    output$statsTable <- DT::renderDataTable({
      data <- getDistStats()

      if (nrow(data) == 0) {
        return (data.frame())
      }

      output <- DT::datatable(
        data,
        colnames = c("Source", "Mean", "sd", "Min", "P10", "P25", "Median", "P75", "P90", "Max"),
        options = list(dom = 't', columnDefs = list(list(visible = FALSE, targets = c(0)))),
        caption = caption
      )
      return(output)
    })

    output$distPlot <- shiny::renderPlot({
      dt <- getDistStats()
      plot <- boxPlotDist(dt)
      return(plot)
    })
  }
  return(serverFunction)
}


boxPlotModuleUi <- function(id) {
  shiny::tagList(
    shinycssloaders::withSpinner(shiny::plotOutput(NS(id, "distPlot"))),
    shinycssloaders::withSpinner(DT::dataTableOutput(NS(id, "statsTable")))
  )
}

#' Wrapper around boxplot module
timeOnTreatmentServer <- function(id, model, selectedExposureOutcome) {
  caption <- "Table: Shows time on treatment for population od patients exposed to medication that experience the outcome of interest."
  server <- shiny::moduleServer(id, boxPlotModuleServer(model$getTimeOnTreatmentStats, caption, selectedExposureOutcome))
  return(server)
}

#' Wrapper around boxplot module
timeToOutcomeServer <- function(id, model, selectedExposureOutcome) {
  caption <- "Table: shows distribution of absolute difference of time between exposure and outcome for population of patients exposed to medication that expeirence the outcome."
  server <- shiny::moduleServer(id, boxPlotModuleServer(model$getTimeToOutcomeStats, caption, selectedExposureOutcome))
  return(server)
}

