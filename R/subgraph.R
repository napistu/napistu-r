#' Create Subgraph
#' 
#' Filter to subgraphs containing a set of vertices and their connection
#' 
#' @inheritParams validate_napistu_list
#' @param max_components the number of components to retrun
#' 
#' @returns a list of the `max_components` largest weakly-connected components
#' 
#' @examples
#' suppressPackageStartupMessages(library(dplyr))
#' setup_napistu_list(create_napistu_config())
#' subgraph_vertices <- sample(napistu_list$napistu_graph$vs["name"], 150)
#' subgraphs <- define_subgraphs(napistu_list, subgraph_vertices, max_components = 2)
#' @export
define_subgraphs <- function (napistu_list, subgraph_vertices, max_components = 4L) {
    
    validate_napistu_list(napistu_list)
    napistu_graph <- napistu_list$napistu_graph
    napistu <- napistu_list$python_modules$napistu
    vertex_names <- load_vertex_seq(subgraph_vertices)
    checkmate::assert_number(max_components, lower = 1, upper = Inf)
    max_components = as.integer(round(max_components))
    
    subgraph <- napistu$network$ig_utils$create_induced_subgraph(
        napistu_graph, 
        vertices = subgraph_vertices,
        n_vertices = NULL
    ) 
    
    # select the top N largest subgraphs
    top_components_graphs <- napistu$network$ig_utils$filter_to_largest_subgraphs(
        subgraph, max_components
    )
    
    subgraph_list <- purrr::map(
        top_components_graphs,
        extend_components_list,
        napistu_list = napistu_list
    )
    
    return(subgraph_list)
}

#' Extend Components List
#' 
#' @param component_graph a weakly connected Python igraph
#' @inheritParams validate_napistu_list
#'
#' @returns a list with:
#' \describe{
#'     \item{component_graph}{The component subgraph}
#'     \item{reaction_sources}{A table mapping reactions to pathway sources}
#' }
extend_components_list <- function (component_graph, napistu_list) {
    
    validate_napistu_list(napistu_list)
    sbml_dfs <- napistu_list$sbml_dfs
    napistu <- napistu_list$python_modules$napistu
    checkmate::assert_class(component_graph, "napistu.network.ng_core.NapistuGraph")
    
    g_list <- napistu$network$ig_utils$graph_to_pandas_dfs(g)
    vertices <- g_list[[1]] %>%
        tibble::as_tibble() %>%
        dplyr::rename(node = name)
    
    reaction_sources <- napistu$network$ng_utils$get_minimal_sources_edges(vertices, sbml_dfs)
    
    component_list <- list(
        component_graph = component_graph,
        reaction_sources = reaction_sources
    )
    
    return(component_list)
}



load_vertex_seq <- function (vertices) {
    # either a character vector or an igraph (py) vertex seq
    if ("character" %in% class(vertices)) {
        return(vertices)
    } else if ("igraph.seq.VertexSeq" %in% class(subgraph_vertices)) {
        vertex_names <- subgraph_vertices["name"]
        
        if (length(vertex_names) != length(vertices)) {
            cli::cli_abort("Some vertices selected in {.function load_vertex_seq} were unnamed")
        }
        
        return(vertex_names)
    } else {
        cli::cli_abort("vertices was not a character vector or igraph VertexSeq")
    }
}

#' Plot Subgraphs
#' 
#' @inheritParams validate_napistu_list
#'
#' @examples
#' suppressPackageStartupMessages(library(dplyr))
#' setup_napistu_list(create_napistu_config())
#' subgraph_vertices <- napistu_list$napistu_graph$vs[1:50]
#' subgraphs_list <- create_subgraphs(napistu_list, subgraph_vertices, max_components = 2)
plot_subgraphs <- function (napistu_list, subgraphs_list) {
    
        
}

#' Plot Subgraph
plot_subgraph <- function (napistu_list, subgraph_list) {
    
    
    
    neighborhood_network <- igraph::graph_from_data_frame(
        edges,
        directed = napistu_graph$is_directed(),
        vertices = vertices
    )
    
    plot_one_neighborhood_v2(
        neighborhood_network = neighborhood_network,
        edge_sources = edge_sources,
        score_label = score_label,
        score_palette = score_palette,
        network_layout = network_layout,
        edge_width = edge_width
    )
    
}

prepare_score_overlays <- function () {
    
    if (!is.null(score_overlay)) {
        checkmate::assertDataFrame(score_overlay)
        stopifnot(c("s_id", "score") %in% colnames(score_overlay))
        
        vertices <- vertices %>%
            dplyr::left_join(
                score_overlay %>%
                    dplyr::select(s_id, score),
                by = "s_id"
            )
    } else {
        # ignore label even if one was passed
        score_label <- NULL
    }
}

add_pathway_sources <- function (vertices, edges, edge_sources) {
    # add pathway sources to help organize layout
    if (!("NULL" %in% class(edge_sources))) {
        edges <- edges %>%
            dplyr::bind_rows(
                edge_sources %>%
                    dplyr::select(from = "r_id", to = "pathway_id")
            )
        
        vertices <- vertices %>%
            dplyr::bind_rows(edge_sources %>% dplyr::distinct(name = pathway_id))
    }
    
    out <- list(
        vertices = vertices,
        edges = edges
    )
}