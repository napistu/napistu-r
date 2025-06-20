#' Shiny Select Entity Test
#'
#' Demo for selecting an entity (species or reaction). Currently just species.
#'
#' @inheritParams validate_napistu_list
#'
#' @returns a Shiny app
#'
#' @examples
#'
#' if (interactive()) {
#'     setup_napistu_list(create_napistu_config())
#'     shiny_selectEntity_test(napistu_list)
#' }
#' @export
shiny_selectEntity_test <- function(napistu_list) {
    
    validate_napistu_list(napistu_list)
    
    shiny::shinyApp(
        ui = shiny::fluidPage(
    
            # Sidebar with a slider input for the number of bins
            shiny::sidebarLayout(
                shiny::sidebarPanel(
                    selectEntityInput("choose_an_entity", "test entity")
                ),
                shiny::mainPanel()
            )
        ),
        server = function(input, output, session) {
            selected_species <- selectEntityServer("choose_an_entity", napistu_list)
    
            shiny::observe({
                shiny::req(selected_species())
                cli::cli_alert_info("You selected species ID: {.field {selected_species()}}")
            })
        }
    )
}

#' Select Entity Input
#'
#' UI components for entity selection shiny module
#'
#' @inheritParams shiny::moduleServer
#' @param gui_label GUI choice label
#'
#' @returns Nothing; used for side-effects
#'
#' @export
selectEntityInput <- function(id, gui_label) {
    checkmate::assertCharacter(id, len = 1)
    checkmate::assertCharacter(gui_label, len = 1)

    ns <- shiny::NS(id)

    shiny::tagList(
        shiny::radioButtons(
            ns("select_entity_by"),
            glue::glue("Select {gui_label} based on:"),
            choices = c("name", "identifier"),
            inline = TRUE
        ),
        shiny::uiOutput(ns("select_entity_ontology")),
        shiny::selectizeInput(ns("selected_entity"), NULL, choices = NULL)
    )
}

#' Select Entity Server
#'
#' Server-side components for the entity selection shiny module
#'
#' @inheritParams shiny::moduleServer
#' @inheritParams validate_napistu_list
#'
#' @returns Nothing; used for side-effects
#'
#' @export
selectEntityServer <- function(id, napistu_list) {
    
    checkmate::assertCharacter(id, len = 1)
    validate_napistu_list(napistu_list)
    species_names <- napistu_list$species_names
    identifiers_nest <- napistu_list$identifiers_nest
    
    shiny::moduleServer(
        id,
        ## Below is the module function
        function(input, output, session) {
            
            cli::cli_alert_info("Starting selectEntityServer")
            ns <- session$ns
            
            # create sorting ui based on selected sort mode
            
            shiny::observe({
                output$select_entity_ontology <- shiny::renderUI({
                    shiny::req(input$select_entity_by)
                    
                    if (input$select_entity_by == "identifier") {
                        shiny::radioButtons(ns("choose_entity_ontology"), "Ontology", choices = identifiers_nest$ontology)
                    } else {
                        NULL
                    }
                })
            })
            
            cli::cli_alert_info("Select entity by name or ontology done")
            
            # initialize a client-side selectize input
            
            shiny::observe({
                req(input$select_entity_by)
                
                # define choices based on whether names or identifiers were chosen in select_entity_by
                
                if (input$select_entity_by == "name") {
                    entity_choices <- species_names$s_name
                } else if (input$select_entity_by == "identifier") {
                    shiny::req(input$choose_entity_ontology)
                    
                    ontology_data <- identifiers_nest %>%
                        dplyr::filter(ontology == input$choose_entity_ontology) %>%
                        {
                            .$ontology_ids[[1]]
                        }
                    
                    entity_choices <- ontology_data$identifier
                } else {
                    stop("Invalid select entity choice")
                }
                
                shiny::updateSelectizeInput(session, "selected_entity", choices = entity_choices, server = TRUE)
            })
            
            cli::cli_alert_info("Available entities have been updated")
            
            # find the species ID corresponding to the selected name / identifier
            
            selected_entity <- reactive({
                req(input$selected_entity)
                
                if (input$select_entity_by == "name") {
                    rownames(species_names)[species_names$s_name == input$selected_entity]
                } else if (input$select_entity_by == "identifier") {
                    req(input$choose_entity_ontology)
                    ontology_data <- identifiers_nest %>%
                        dplyr::filter(ontology == input$choose_entity_ontology) %>%
                        {.$ontology_ids[[1]]}
                    
                    ontology_data$s_id[ontology_data$identifier == input$selected_entity]
                } else {
                    cli::cli_abort("Invalid select entity choice")
                }
            })
            
            cli::cli_alert_info("Finished selectEntityServer")
            
            unique_selected_entity <- reactive({
                req(selected_entity())
                
                n_entries <- length(selected_entity())
                if (n_entries == 0) {
                    stop("No entry was selected - this is unexpected behavior")
                } else if (n_entries > 1) {
                    warning(glue::glue("{n_entries} entries were selected; the first will be used"))
                    selected_entity()[1]
                } else {
                    selected_entity()
                }
            })
            
            return(unique_selected_entity)
        }
    )
}
