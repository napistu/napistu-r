#' Create Subgraph
#' 
#' Filter to an induced subgraph containing a set of vertices and their connections
#' 
#' @inheritParams validate_napistu_list
#' @param max_components the number of components to retrun
#' 
#' @returns a list of the `max_components` largest weakly-connected components
#' 
#' @examples
#' suppressPackageStartupMessages(library(dplyr))
#' setup_napistu_list(create_napistu_config())
#' subgraph_vertices <- sample(napistu_list$napistu_graph$vs["name"], 100)
#' define_subgraphs(napistu_list, subgraph_vertices, max_components = 2)
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
#' Add additional information needed for plotting to a weakly connected component
#' graph.
#' 
#' @inheritParams validate_napistu_list
#' @param component_graph a weakly connected Python igraph
#'
#' @returns a list with:
#' \describe{
#'     \item{component_graph}{The component subgraph}
#'     \item{reaction_sources}{A table mapping reactions to pathway sources}
#' }
extend_components_list <- function (napistu_list, component_graph) {
    
    validate_napistu_list(napistu_list)
    sbml_dfs <- napistu_list$sbml_dfs
    napistu <- napistu_list$python_modules$napistu
    checkmate::assert_class(component_graph, "napistu.network.ng_core.NapistuGraph")
    
    vertices_and_edges <- napistu$network$ig_utils$graph_to_pandas_dfs(component_graph)
    vertices <- vertices_and_edges[[1]] %>%
        tibble::as_tibble() %>%
        dplyr::rename(node = name)
    
    reaction_sources <- napistu$network$ng_utils$get_minimal_sources_edges(vertices, sbml_dfs)
    
    component_list <- list(
        component_graph = component_graph,
        reaction_sources = reaction_sources
    )
    
    return(component_list)
}


#' Plot Subgraphs
#' 
#' @inheritParams validate_napistu_list
#'
#' @examples
#' setup_napistu_list(create_napistu_config())
#' subgraph_vertices <- sample(napistu_list$napistu_graph$vs["name"], 100)
#' subgraph_list <- define_subgraphs(napistu_list, subgraph_vertices, max_components = 2)
plot_subgraph <- function (napistu_list, subgraph_list) {
    
        
}

#' Plot One Component
#' 
#' @inheritParams validate_napistu_list
#' @inheritParams prepare_score_overlays
#' @inheritParams label_vertices
#' 
#' @examples
#' suppressPackageStartupMessages(library(dplyr))
#' setup_napistu_list(create_napistu_config())
#' subgraph_vertices <- sample(napistu_list$napistu_graph$vs["name"], 100)
#' subgraph_list <- define_subgraphs(napistu_list, subgraph_vertices, max_components = 2)
#' component_list <- subgraph_list[[1]]
#' score_overlay <- tibble::tibble(name = sample(subgraph_vertices, 20)) %>%
#'     dplyr::mutate(score = abs(rnorm(dplyr::n())))
#' plot_one_component(
#'     napistu_list,
#'     component_list
#' )
plot_one_component <- function (napistu_list, component_list, join_scores_on = "name", max_labeled_species = 20) {
    
    validate_napistu_list(napistu_list)
    napistu <- napistu_list$python_modules$napistu
    
    component_graph <- component_list$component_graph
    vertices_and_edges <- napistu$network$ig_utils$graph_to_pandas_dfs(component_graph)
    vertices <- vertices_and_edges[[1]] %>%
        dplyr::mutate(ggtext_label = dplyr::case_when(
            node_type == "reaction" ~ NA_character_,
            .default = node_name
            ),
            node_orientation = "default"
        ) %>%
        dplyr::select(-index)
    
    edges <- vertices_and_edges[[2]] %>%
        dplyr::select(-source, -target)
    reaction_sources <- component_list$reaction_sources

    # add scores if they are present
    if (!is.null(score_overlay)) {
        vertices <- prepare_score_overlays(vertices, score_overlay, join_on = join_scores_on)
    } else {
        score_label <- NULL
    }
    
    # order vertices
    if (!is.null(score_overlay)) {
        vertices <- label_vertices(
            vertices %>% dplyr::arrange(desc(score)),
            max_labeled_species
        )
    } else {
        vertices <- vertices %>%
            # shuffle
            sample_n(dplyr::n())
    }
    
    # add pathway sources to help organize layout
    if (!is.null(reaction_sources)) {
        edges <- edges %>%
            dplyr::bind_rows(
                reaction_sources %>%
                    dplyr::select(from = "r_id", to = "pathway_id")
            )
        
        vertices <- vertices %>%
            dplyr::bind_rows(reaction_sources %>% dplyr::distinct(name = pathway_id))
    }
    
    component_network <- igraph::graph_from_data_frame(
        edges,
        directed = component_graph$is_directed(),
        vertices = vertices
    )
    
    network_layout = "kk"
    score_label = NULL
    score_palette = "log2 fold-change"
    
    grob <- plot_one_component_render(
        component_network = component_network,
        edge_sources = edge_sources,
        score_label = score_label,
        score_palette = score_palette,
        network_layout = network_layout,
        edge_width = edge_width
    )
    
    return(grob)
}

plot_one_component_render <- function (
    component_network,
    edge_sources,
    score_label,
    score_palette,
    network_layout,
    edge_width = 0.1
) {
    
    rendering_prep_list <- prepare_rendering(component_network, edge_sources, network_layout)
    component_grob <- rendering_prep_list$network_grob
    vertices_df <- rendering_prep_list$vertices_df
    pathway_coords <- rendering_prep_list$pathway_coords
    
    # title setup
    nodes_summary <- vertices_df %>%
        count(node_type) %>%
        dplyr::mutate(label = glue::glue("{n} {node_type}")) %>%
        purrr::pluck("label") %>%
        glue::glue_collapse(sep = " & ")
    
    plot_title <- glue::glue(
        "Visualizing a subgraph containing {nodes_summary} vertices"
    )
    
    if (!is.null(score_label)) {
        plot_title <- plot_title + glue::glue("<br>overlaying **{score_label}**")
    }
    
    if (!is.null(edge_sources)) {
        component_grob <- add_pathway_outlines(component_grob, pathway_coords)
    }
    
    color_scheme <- add_node_color_palette(
        component_grob,
        vertices_df,
        score_palette
    )
    color_by <- color_scheme$color_by
    component_grob <- color_scheme$grob
    
    component_grob <- add_edges_by_reversibility(component_grob, edge_width)
    
    # neighborhood-specific plotting
    
    component_grob <- component_grob +
        ggraph::geom_node_point(aes(
            shape = factor(node_type),
            color = !!color_by,
            size = 6
        )) 
    
    component_grob <- add_node_names_and_themes(
        component_grob,
        vertices_df,
        plot_title
    )
    
    return(component_grob)
}

prepare_score_overlays <- function (vertices, score_overlay = NULL, score_label = NULL, join_on = "s_id") {
    
    checkmate::assert_data_frame(vertices)
    checkmate::assert_choice(join_on, colnames(vertices))
    
    if (!is.null(score_overlay)) {
        checkmate::assertDataFrame(score_overlay)
        if (!(join_on %in% colnames(score_overlay))) {
            cli::cli_abort(
                "{.field {join_on}} is not present in {.arg score_overlay} but
                you are trying to join on {join_on}"
            )
        }
        
        if (!("score" %in% colnames(score_overlay))) {
            cli::cli_abort(
                "{.field score} is missing from {.arg score_overlay}.
                It must be included if score_overlay is provided"
            )
        }
        stopifnot(c("s_id", "score") %in% colnames(score_overlay))
        
        vertices <- vertices %>%
            dplyr::left_join(
                score_overlay %>%
                    dplyr::select(!!rlang::sym(join_on), score),
                by = join_on
            )
    } else {
        # ignore label even if one was passed
        score_label <- NULL
    }
    
    out <- list(
        vertices = vertices,
        score_label = score_label
    )
    return(out)
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
