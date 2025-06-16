#' Shiny Basic Info Test
#'
#' Demo for querying the reactions a species participates in.
#'
#' @inheritParams basicInfoServer
#'
#' @returns a shiny app
#'
#' @examples
#'
#' if (interactive()) {
#'   interactive_initialization_wrapper()
#'   napistu <- reticulate::import("napistu")
#'
#'   shiny_basicInfo_test(
#'     species_names,
#'     species_identifiers,
#'     sbml_dfs,
#'     napistu = napistu
#'   )
#' }
#' @export
shiny_basicInfo_test <- function(species_names, species_identifiers, sbml_dfs, napistu) {
    checkmate::assertDataFrame(species_names)
    checkmate::assertDataFrame(species_identifiers)
    checkmate::assertClass(sbml_dfs, "napistu.sbml_dfs_core.SBML_dfs")
    checkmate::assertClass(napistu, "python.builtin.module")
    shiny::shinyApp(
        ui = shiny::fluidPage(
            basicInfoInput("entity_info_app")
        ),
        server = function(input, output, session) {
            basicInfoServer("entity_info_app", species_names, species_identifiers, sbml_dfs, napistu)
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
#' @inheritParams selectEntityServer
#' @inheritParams neighborhoodServer
#'
#' @returns Nothing; used for side-effects
#'
#' @export
basicInfoServer <- function(id,
                            species_names,
                            species_identifiers,
                            sbml_dfs,
                            napistu) {
    checkmate::assertCharacter(id, len = 1)
    checkmate::assertDataFrame(species_names)
    checkmate::assertDataFrame(species_identifiers)
    checkmate::assertClass(sbml_dfs, "napistu.sbml_dfs_core.SBML_dfs")
    checkmate::assertClass(napistu, "python.builtin.module")
    shiny::moduleServer(
        id,
        ## Below is the module function
        function(input, output, session) {
            selected_basic_entity <- selectEntityServer("basic_info_entity", species_names, species_identifiers)
            shiny::observe({
                shiny::req(selected_basic_entity())
                cli::cli_alert_info("You selected species ID: {selected_basic_entity()}")
                species_status <- napistu$sbml$species_status(selected_basic_entity(), sbml_dfs)
                req(species_status)
                output$summary_table <- DT::renderDataTable(species_status)
            })
        }
    )
}