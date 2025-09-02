createDbSearchableSelectizeInput <- function(inputId, label, ...) {
  hiddenInputBoxId <- paste0(inputId, "Box")
  jsCode <- glue::glue(
    "$(document).on('keyup', '#{{inputId}}-selectized', function(e) {
      var val = $(this).val();
      Shiny.setInputValue('{{hiddenInputBoxId}}', val, {priority: 'event'});
     });
    ", .open = "{{", .close = "}}")
  shiny::tagList(
    shiny::selectizeInput(
      inputId = inputId,
      label = label,
      choices = c(),
      ...
    ),
    shiny::tags$script(shiny::HTML(jsCode))
  )
}


handleDbSearchableSelectizeInput <- function(input, session, inputId, searchChoiceFun) {
  searchBoxId <- paste0(inputId, "Box")

  shiny::observeEvent(input[[searchBoxId]], {
    shiny::req(input[[searchBoxId]])
    if (nchar(input[[searchBoxId]]) > 2) {
      choices <- searchChoiceFun(input[[searchBoxId]], input[[inputId]])
      shiny::updateSelectizeInput(
        session = session,
        inputId = inputId,
        server = TRUE,
        choices = choices,
        selected = input$openTargetsIndicationSearch
      )
    }
  })
}

#' UI For open targets search module
#'
#' @description
#' Adds ui elements
#' @param id     namespace id
openTargetsSearchUiBlock <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h4("OpenTargets"),
    shiny::fluidRow(
      shiny::column(
        width = 6,
        createDbSearchableSelectizeInput(
          ns("openTargetsIngredientsSearch"),
          label = "Include only open targets ingredients",
          multiple = TRUE
        )
      )
    ),
    # shiny::fluidRow(
    #   # Search only by indications - approved or not
    #   shiny::column(
    #     width = 6,
    #     createDbSearchableSelectizeInput(
    #       ns("openTargetsIndicationSearch"),
    #       label = "Include exposures with only matched indications",
    #       multiple = TRUE
    #     ),
    #     shiny::checkboxInput(
    #       inputId = ns("openTargetsIncidationFilterUseApproved"),
    #       value = TRUE,
    #       label = "Use approved indications only"
    #     )
    #   ),
    #   # Exclude by indication, approved or not
    #   shiny::column(
    #     width = 6,
    #     createDbSearchableSelectizeInput(
    #       ns("openTargetsExcludeIndicationSearch"),
    #       label = "Exclude exposures by matched indications",
    #       multiple = TRUE
    #     )
    #   )
    # ),
    shiny::fluidRow(
      shiny::column(
        width = 6,
        shiny::selectizeInput(
          inputId = ns("openTargetsGeneSymbolFilter"),
          label = "Filter exposures by gene symbool",
          multiple = TRUE, # hack for now to stop it selecting automatically
          choices = c(),
          selected = c()
        )
      ),
      shiny::column(
        width = 6,
        shiny::selectizeInput(
          inputId = ns("openTargetsDrugMechanismSearch"),
          label = "Select gene mechanism of action",
          multiple = TRUE,
          choices = c(),
          selected = c()
        )
      )
    )
  )
}

safeInput <- function(search) {
  search <- substr(gsub(";|\'", "", search), 1, 100)
  return(search)
}

#' Open targets to omop vocabulary filtering shiny module
#' @description
#' Hooks up the search for open targets.
#' This returns a set of reactive values that can be used to filter concepts in the omop CDM.
#' @param id namespace id
#' @param model scc data model
#' @param openTargetsDatabaseSchema database schema where open targets results live
openTargetsSearchModule <- function(id, model, openTargetsDatabaseSchema) {
  server <- shiny::moduleServer(id, function(input, output, session) {

    ### --- Gene symbol filterting
    shiny::observe({

      gsChoicesRes <- model$queryDb("SELECT DISTINCT approved_symbol, approved_name FROM @open_targets_schema.gene_symbols ORDER BY approved_symbol;",
                                    warnOnMissingParameters = FALSE,
                                    open_targets_schema = openTargetsDatabaseSchema)

      gsChoices <- gsChoicesRes$approvedSymbol
      names(gsChoices) <- paste(gsChoicesRes$approvedSymbol, " - ", gsChoicesRes$approvedName)
      shiny::updateSelectizeInput(session = session,
                                  inputId = "openTargetsGeneSymbolFilter",
                                  server = TRUE,
                                  choices = gsChoices,
                                  selected = NULL)
    })

    shiny::observeEvent(input$openTargetsGeneSymbolFilter, {
      ### ANNOYING SHINY ISSUE - when openTargetsGeneSymbolFilter is emptied this event does not trigger
      # This means selected mechanisms of action will stay there.
      if (!length(input$openTargetsGeneSymbolFilter)) {

        shiny::updateSelectInput(session = session,
                                 inputId = "openTargetsDrugMechanismSearch",
                                 selected = c(),
                                 choices = c())
        return(NULL)
      }

      geneSymbols <- glue::glue("'{safeInput(input$openTargetsGeneSymbolFilter)}'")
      mechanismChoicesRes <- model$queryDb(
        "
      SELECT DISTINCT mechanism_of_action
      FROM @open_targets_schema.mechanisms m
      INNER JOIN @open_targets_schema.gene_symbols gs ON m.drug_id = gs.drug_id
      where gs.approved_symbol IN (@gene_symbol)
      ",
        warnOnMissingParameters = FALSE,
        gene_symbol = geneSymbols,
        open_targets_schema = openTargetsDatabaseSchema
      )

      mechanismChoices <- mechanismChoicesRes$mechanismOfAction
      shiny::updateSelectInput(session = session,
                               inputId = "openTargetsDrugMechanismSearch",
                               selected = input$openTargetsDrugMechanismSearch,
                               choices = mechanismChoices)
    })

    getAppliedGeneAndMechanismConcepts <- shiny::reactive({
      if (length(input$openTargetsGeneSymbolFilter) == 0) {
        return(NULL)
      }

      if (length(input$openTargetsDrugMechanismSearch)) {
        # Filter by gene and mechanism of action
        res  <- model$queryDb(
          "
        SELECT DISTINCT m.ingredient_concept_id
        FROM @open_targets_schema.mechanisms m
        INNER JOIN @open_targets_schema.gene_symbols gs ON m.drug_id = gs.drug_id
        where gs.approved_symbol IN (@gene_symbol)
        AND m.mechanism_of_action IN (@mechanism_of_action)
        ",
          warnOnMissingParameters = FALSE,
          gene_symbol = glue::glue("'{safeInput(input$openTargetsGeneSymbolFilter)}'"),
          mechanism_of_action = glue::glue("'{safeInput(input$openTargetsDrugMechanismSearch)}'"),
          open_targets_schema = openTargetsDatabaseSchema
        )
        return(res$ingredientConceptId)
      }

      # Filter by mechanism of action only.
      res <- model$queryDb("SELECT DISTINCT ingredient_concept_id FROM @open_targets_schema.gene_symbols WHERE approved_symbol IN (@gene_symbols);",
                           warnOnMissingParameters = FALSE,
                           gene_symbols = glue::glue("'{safeInput(input$openTargetsGeneSymbolFilter)}'"),
                           open_targets_schema = openTargetsDatabaseSchema)

      return(res$ingredientConceptId)
    })


    ### --- Ingredient search handling
    openTargetsIngredientSearch <- function(searchString, existingValues) {
      if (isTRUE(nchar(searchString) < 3 & existingValues == ""))
        return(NULL)

      # cleanup to prevent sql injection - remove bad chars and limit length
      search <- substr(gsub(";|\'", "", searchString), 1, 100)
      search <- paste0("'%%", search, "%%'")
      sql <- "
      SELECT *
        FROM (
          SELECT DISTINCT ot.drug_name, ot.drug_id
          FROM @open_targets_database_schema.opentargets_to_reward_relationships ot
          WHERE lower(CONCAT(ot.drug_name, ot.drug_id)) LIKE @search
          ORDER BY drug_name LIMIT 10
        ) cd
      {@existing_ids != ''} ? {
      UNION

      SELECT DISTINCT ot.drug_name, ot.drug_id
      FROM @open_targets_database_schema.opentargets_to_reward_relationships ot
      WHERE ot.drug_id IN (@existing_ids)
      }
      "
      result <- model$queryDb(sql,
                              search = tolower(search),
                              warnOnMissingParameters = FALSE,
                              open_targets_database_schema = openTargetsDatabaseSchema,
                              existing_ids = existingValues)

      if (nrow(result) == 0)
        return(NULL)

      choices <- result$drugId
      names(choices) <- paste(result$drugId, " - ", result$drugName)
      return(choices)
    }

    handleDbSearchableSelectizeInput(input = input,
                                     inputId = "openTargetsIngredientsSearch",
                                     session = session,
                                     searchChoiceFun = openTargetsIngredientSearch)

    getDirectIngredientInclusion <- shiny::reactive({

      if (!length(input$openTargetsIngredientsSearch))
        return(NULL)

      sql <- "SELECT DISTINCT ot.ingredient_concept_id
      FROM @open_targets_database_schema.opentargets_to_reward_relationships ot
      WHERE ot.drug_id IN (@existing_ids)"

     result <- model$queryDb(sql,
                            search = tolower(search),
                            warnOnMissingParameters = FALSE,
                            open_targets_database_schema = openTargetsDatabaseSchema,
                            existing_ids = input$openTargetsIngredientsSearch)

      return(result$ingredientConceptId)
    })

    ### --- Indication search handling
    openTargetsIndicationsSearch <- function(searchString, existingValues) {
      if (isTRUE(nchar(searchString) < 3 & existingValues == ""))
        return(NULL)

      # cleanup to prevent sql injection - remove bad chars and limit length
      search <- substr(gsub(";|\'\"", "", searchString), 1, 100)
      search <- paste0("'%", search, "%'")

      sql <- "
      SELECT *
        FROM (
          SELECT DISTINCT indication_name, indication_id
          FROM @open_targets_database_schema.indications
          WHERE lower(CONCAT(indication_name, indication_id)) LIKE @search
          ORDER BY indication_name LIMIT 10
        ) cd
      {@existing_ids != ''} ? {
      UNION

      SELECT DISTINCT indication_name, indication_id
      FROM @open_targets_database_schema.indications
      WHERE indication_id IN (@existing_ids)
      }
      "
      result <- model$queryDb(sql,
                              search = tolower(search),
                              warnOnMissingParameters = FALSE,
                              open_targets_database_schema = openTargetsDatabaseSchema,
                              existing_ids = glue::glue("'{existingValues}'"))

      if (nrow(result) == 0)
        return(NULL)

      choices <- result$indicationId
      names(choices) <- paste(result$indicationId, " - ", result$indicationName)
      return(choices)
    }

    handleDbSearchableSelectizeInput(input = input,
                                     inputId = "openTargetsIndicationSearch",
                                     session = session,
                                     searchChoiceFun = openTargetsIndicationsSearch)

    getIndicationInclusions <- shiny::reactive({

      if (!length(input$openTargetsIndicationSearch))
        return(NULL)

      sql <- "SELECT DISTINCT exposure_ingredient_concept_id
      FROM @open_targets_database_schema.indications
      WHERE indication_id IN (@existing_ids)
      {@require_approved_indications} ? {AND approved = 1}
      "
      
     searchStr <- glue::glue("'{input$openTargetsIndicationSearch}'")
     result <- model$queryDb(sql,
                            warnOnMissingParameters = FALSE,
                            require_approved_indications = isTRUE(input$openTargetsIncidationFilterUseApproved),
                            open_targets_database_schema = openTargetsDatabaseSchema,
                            existing_ids = searchStr)

      return(result$exposureIngredientConceptId)
    })

    handleDbSearchableSelectizeInput(input = input,
                                     inputId = "openTargetsExcludeIndicationSearch",
                                     session = session,
                                     searchChoiceFun = openTargetsIndicationsSearch)

    getIndicationExclusions <- shiny::reactive({

      if (!length(input$openTargetsExcludeIndicationSearch))
        return(NULL)

      sql <- "SELECT DISTINCT exposure_ingredient_concept_id
      FROM @open_targets_database_schema.indications
      WHERE indication_id IN (@existing_ids)
      {@require_approved_indications} ? {AND approved = 1}
      "
     searchStr <- glue::glue("'{input$openTargetsExcludeIndicationSearch}'")
     result <- model$queryDb(sql,
                            warnOnMissingParameters = FALSE,
                            require_approved_indications = isTRUE(input$openTargetsIncidationFilterUseApproved),
                            open_targets_database_schema = openTargetsDatabaseSchema,
                            existing_ids = searchStr)

      return(result$exposureIngredientConceptId)
    })

    return(shiny::reactive({
      list(
        includedIngredientConcepts =  c(getAppliedGeneAndMechanismConcepts(), getDirectIngredientInclusion(), getIndicationInclusions()),
        excludedIngredientConcepts = c(getIndicationExclusions())
      )
    }))
  })
  return(server)
}