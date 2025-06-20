#' Shiny Basic Info Test
#'
#' Demo for querying the reactions a species participates in.
#'
#' @inheritParams validate_napistu_list
#'
#' @returns a shiny app
#'
#' @examples
#'
#' if (interactive()) {
#'   setup_napistu_list(create_napistu_config())
#'   shiny_basicInfo_test(napistu_list)
#' }
#' @export
shiny_basicInfo_test <- function(napistu_list) {
    
    validate_napistu_list(napistu_list)
    
    shiny::shinyApp(
        ui = shiny::fluidPage(
            basicInfoInput("entity_info_app")
        ),
        server = function(input, output, session) {
            basicInfoServer("entity_info_app", napistu_list)
        }
    )
}
#' Basic Info Input
#'
#' UI components for the basic info shiny module
#'
#' @inheritParams shiny::moduleServer
#' @param gui_label GUI choice label
#'
#' @returns Nothing; used for side-effects
#'
#' @export
basicInfoInput <- function(id, gui_label) {
    checkmate::assertCharacter(id, len = 1)
    ns <- shiny::NS(id)
    shiny::sidebarLayout(
        shiny::sidebarPanel(
            selectEntityInput(ns("basic_info_entity"), "test entity")
        ),
        shiny::mainPanel(DT::dataTableOutput(ns("summary_table")))
    )
}
#' Basic Info Server
#'
#' Server-side components for basic info shiny module
#'
#' @inheritParams shiny::moduleServer
#' @inheritParams validate_napistu_list
#'
#' @returns Nothing; used for side-effects
#'
#' @export
basicInfoServer <- function(id, napistu_list) {
    
    checkmate::assertCharacter(id, len = 1)
    validate_napistu_list(napistu_list)
    napistu <- napistu_list$python_modules$napistu
    sbml_dfs <- napistu_list$sbml_dfs
    
    shiny::moduleServer(
        id,
        ## Below is the module function
        function(input, output, session) {
            selected_basic_entity <- selectEntityServer("basic_info_entity", napistu_list)
            shiny::observe({
                shiny::req(selected_basic_entity())
                cli::cli_alert_info("You selected species ID: {selected_basic_entity()}")
                species_status <- napistu$sbml_dfs_core$species_status(selected_basic_entity(), sbml_dfs)
                req(species_status)
                output$summary_table <- DT::renderDataTable(species_status)
            })
        }
    )
}