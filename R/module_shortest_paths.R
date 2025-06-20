#' Shiny Shortest Paths Test
#'
#' Demo for finding the shortest paths between pairs of entities.
#'
#' @inheritParams validate_napistu_list
#'
#' @returns a shiny app
#'
#' @examples
#'
#' if (interactive()) {
#'   setup_napistu_list(create_napistu_config())
#'   shiny_shortestPaths_test(napistu_list)
#' }
#' @export
shiny_shortestPaths_test <- function(napistu_list) {
    
    validate_napistu_list(napistu_list)
    
    shiny::shinyApp(
        ui = shiny::fluidPage(
            shortestPathsInput("shortest_paths_app")
        ),
        server = function(input, output, session) {
            shortestPathsServer("shortest_paths_app", napistu_list)
        }
    )
}

#' Shortest Paths Input
#'
#' UI components for the shortest paths shiny module
#'
#' @inheritParams shiny::moduleServer
#'
#' @returns Nothing; used for side-effects
#'
#' @export
shortestPathsInput <- function(id) {
    checkmate::assertCharacter(id, len = 1)
    
    ns <- shiny::NS(id)
    
    shiny::sidebarLayout(
        shiny::sidebarPanel(
            shiny::tagList(
                shiny::h3("Choose source and target destination"),
                selectEntityInput(ns("source_entity"), "source reaction species"),
                selectEntityInput(ns("dest_entity"), "destination reaction species")
            )
        ),
        shiny::mainPanel(
            shiny::tabsetPanel(
                type = "tabs",
                shiny::tabPanel(
                    "Plot",
                    romic::plotsaverInput(ns("ggsave_path"), "wide"),
                    shiny::plotOutput(ns("shortest_paths_plot"))
                ),
                shiny::tabPanel(
                    "Table",
                    DT::dataTableOutput(ns("shortest_paths_table")),
                    shiny::downloadButton(ns("download_paths_table"), "Download .tsv")
                )
            )
        )
    )
}

#' Shortest Paths Server
#'
#' Server-side components for the shortest paths shiny module
#'
#' @inheritParams shiny::moduleServer
#' @inheritParams validate_napistu_list
#'
#' @returns Nothing; used for side-effects
#'
#' @export
shortestPathsServer <- function(
    id,
    napistu_list
) {
    
    checkmate::assertCharacter(id, len = 1)
    validate_napistu_list(napistu_list)
    
    shiny::moduleServer(
        id,
        ## Below is the module function
        function(input, output, session) {
            ns <- session$ns
            
            selected_source_entity <- selectEntityServer("source_entity", napistu_list)
            selected_dest_entity <- selectEntityServer("dest_entity", napistu_list)
            
            shiny::observe({
                req(selected_source_entity(), selected_dest_entity())
                cli::cli_alert_info("Finding path from {selected_source_entity()} to {selected_dest_entity()}")
                
                # find shortest paths
                shortest_paths_summary <- summarize_shortest_paths(
                    napistu_list,
                    selected_source_entity(),
                    selected_dest_entity()
                )
                
                req(shortest_paths_summary)
                output$shortest_paths_plot <- renderPlot(
                    shortest_paths_summary$shortest_paths_plot,
                    height = 1000
                )
                output$shortest_paths_table <- DT::renderDataTable(
                    add_url_links(shortest_paths_summary$shortest_paths_table),
                    escape = FALSE
                )
                
                # download plot
                romic::plotsaverServer(
                    "ggsave_path",
                    shortest_paths_summary$shortest_paths_plot
                )
                
                # download table
                output$download_paths_table <- downloadHandler(
                    filename = glue::glue("shortest_paths.tsv"),
                    content = function(file) {
                        vroom::vroom_write(shortest_paths_summary$shortest_paths_table, file)
                    }
                )
            })
        }
    )
}
