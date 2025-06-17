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

metaAnalysisTableUi <- function(id) {
  shiny::tagList(
    shinycssloaders::withSpinner(DT::dataTableOutput(NS(id, "fullResultsTable"))),
    shiny::hr(),
    shiny::downloadButton(NS(id, "downloadSubTable"), "Download table")
  )
}

metaAnalysisTableServer <- function(id, model, selectedExposureOutcome) {
  server <- shiny::moduleServer(id, function(input, output, session) {
    metaAnalysisTbl <- shiny::reactive({
      s <- selectedExposureOutcome()
      exposureId <- s$targetCohortId
      outcomeId <- s$outcomeCohortId
      if (length(outcomeId) & length(exposureId)) {
        return(model$getMetaAnalysisTable(exposureId, outcomeId))
      }
      return(data.frame())
    })

    fullResultsTable <- shiny::reactive({
      table3 <- metaAnalysisTbl()
      if (length(table3) == 0) {
        return(data.frame())
      }

      if (nrow(table3) >= 1) {
        table3$unexposedCases <- formatC(table3$cCases, digits = 0, format = "f")
        table3$exposedCases <- formatC(table3$tCases, digits = 0, format = "f")
        table3$rr <- formatC(table3$rr, digits = 2, format = "f")
        table3$lb95 <- formatC(table3$lb95, digits = 2, format = "f")
        table3$ub95 <- formatC(table3$ub95, digits = 2, format = "f")
        table3$pValue <- formatC(table3$pValue, digits = 2, format = "f")

        table3$calibratedRr <- formatC(table3$calibratedRr, digits = 2, format = "f")
        table3$calibratedLb95 <- formatC(table3$calibratedLb95, digits = 2, format = "f")
        table3$calibratedUb95 <- formatC(table3$calibratedUb95, digits = 2, format = "f")
        table3$calibratedPValue <- formatC(table3$calibratedPValue, digits = 2, format = "f")


        table3 <- table3 %>% dplyr::select(
          sourceName,
          rr,
          ci95,
          pValue,
          calibratedRr,
          calibratedCi95,
          calibratedPValue,
          unexposedCases,
          exposedCases
        )

        colnames(table3) <- SqlRender::camelCaseToTitleCase(colnames(table3))
        table4 <- DT::datatable(
          table3, rownames = FALSE, escape = FALSE, options = list(dom = 't'),
          caption = "* Indicates values after empirical calibration"
        )
        return(table4)
      }
    })

    output$fullResultsTable <- DT::renderDataTable({
      tryCatch({
        return(fullResultsTable())
      },
      error = function(e) {
        ParallelLogger::logError(e)
        return(data.frame())
      })
    })

    output$downloadSubTable <- downloadHandler(
      filename = function() {
        s <- selectedExposureOutcome()
        treatment <- s$targetCohortId
        outcome <- s$outcomeCohortId
        paste0(model$resultsSchema, '-results-', treatment, "-", outcome, '.csv')
      },
      content = function(file) {
        write.csv(metaAnalysisTbl(), file, row.names = FALSE)
      }
    )
  })
}
