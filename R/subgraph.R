#' Create Subgraph
#' 
#' Filter to an induced subgraph containing a set of vertices and their connections
#' 
#' @inheritParams validate_napistu_list
#' @param subgraph_vertices the vertices to retain in the subgraph
#' @param max_components the number of components to return
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


#' Plot Subgraphs
#' 
#' @inheritParams validate_napistu_list
#' @param subgraph_list a list-of-lists produced by \link{define_subgraphs}
#'     containing graphs and pathway-associations for the largest weakly
#'     connected components
#' @inheritParams prepare_score_overlays
#' @inheritParams label_vertices
#' @param ... additional arguments to pass to \link{plot_one_component}
#'
#' @examples
#' setup_napistu_list(create_napistu_config())
#' subgraph_vertices <- sample(napistu_list$napistu_graph$vs["name"], 100)
#' subgraph_list <- define_subgraphs(napistu_list, subgraph_vertices, max_components = 2)
#' plot_subgraph(napistu_list, subgraph_list, vertex_size = 3, edge_width = 0.5)
#' @export
plot_subgraph <- function (
    napistu_list,
    subgraph_list,
    score_overlay = NULL,
    join_scores_on = "name",
    max_labeled_species = 20,
    ...
) {

    component_grobs <- purrr::map(
        subgraph_list,
        plot_one_component,
        napistu_list = napistu_list,
        score_overlay = score_overlay,
        join_scores_on = join_scores_on,
        max_labeled_species = max_labeled_species,
        ...
    )
    
    patchwork::wrap_plots(component_grobs, ncol = 1)
}

#' Plot One Component
#' 
#' @inheritParams validate_napistu_list
#' @param component_list a list produced by \link{define_subgraphs} containing
#'     the `component_graph` and `reaction_sources`
#' @inheritParams prepare_score_overlays
#' @inheritParams label_vertices
#' @inheritParams prepare_rendering
#' @inheritParams plot_one_neighborhood
#' @inheritParams add_edges_by_reversibility
#' @inheritParams validate_vertex_size
#' @inheritParams validate_max_labeled_species
#' @inheritParams validate_target_plot_width
#' 
#' @examples
#' suppressPackageStartupMessages(library(dplyr))
#' setup_napistu_list(create_napistu_config())
#' subgraph_vertices <- sample(napistu_list$napistu_graph$vs["name"], 100)
#' subgraph_list <- define_subgraphs(napistu_list, subgraph_vertices, max_components = 2)
#' component_list <- subgraph_list[[1]]
#' score_overlay <- tibble::tibble(name = sample(subgraph_vertices, 20)) %>%
#'     dplyr::mutate(score = abs(stats::rnorm(dplyr::n())))
#' 
#' plot_one_component(
#'     napistu_list,
#'     component_list,
#'     score_overlay = score_overlay,
#'     score_palette = "log2 fold-change",
#'     edge_width = 0.5,
#'     target_plot_width = 6
#' )
#' 
#' # advanced features
#' plot_one_component(
#'     napistu_list,
#'     component_list,
#'     score_overlay = score_overlay,
#'     score_palette = viridis::scale_color_viridis(),
#'     vertex_size = 2,
#'     edge_width = 0.5,
#'     show_edges_if = list(weight = list(cutoff = 0.6, retain = "below"))
#' )
#' @export
plot_one_component <- function (
    napistu_list,
    component_list,
    score_overlay = NULL,
    score_label = NULL,
    score_palette = NULL,
    join_scores_on = "name",
    vertex_size = 6,
    network_layout = "fr",
    edge_weights = NULL,
    edge_width = 0.5,
    show_edges_if = NULL,
    max_labeled_species = 20,
    target_plot_width = 6
) {
    
    validate_network_layout(network_layout)
    validate_score_palette(score_palette, score_overlay)
    checkmate::assert_number(edge_width, na.ok = TRUE, lower = 0)
    validate_show_edges_if(show_edges_if)
    validate_vertex_size(vertex_size)
    validate_max_labeled_species(max_labeled_species)
    validate_target_plot_width(target_plot_width)
    
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
        vertices <- prepare_score_overlays(
            vertices,
            score_overlay,
            join_scores_on = join_scores_on
        )
    } else {
        score_label <- NULL
    }
    
    # order vertices
    if (!is.null(score_overlay)) {
        vertices <- label_vertices(
            vertices %>% dplyr::arrange(dplyr::desc(abs(score))),
            max_labeled_species
        )
    } else {
        vertices <- vertices %>%
            # shuffle
            dplyr::sample_n(dplyr::n())
    }
    
    # add pathway sources to help organize layout
    graph_tbls_w_sources = add_sources_to_graph(vertices, edges, reaction_sources)
    
    component_network <- igraph::graph_from_data_frame(
        graph_tbls_w_sources$edges,
        directed = component_graph$is_directed(),
        vertices = graph_tbls_w_sources$vertices
    )
    
    grob <- plot_one_component_render(
        component_network = component_network,
        reaction_sources = reaction_sources,
        score_label = score_label,
        score_palette = score_palette,
        network_layout = network_layout,
        edge_weights = edge_weights,
        edge_width = edge_width,
        show_edges_if = show_edges_if,
        vertex_size = vertex_size,
        target_plot_width = target_plot_width
    )
    
    return(grob)
}


#' Extend Components List
#' 
#' Add additional information needed for plotting to a weakly connected component
#' graph.
#' 
#' @inheritParams validate_napistu_list
#' @param component_graph a weakly connected Python igraph
#' @param min_pw_size the minimum size of a pathway to be considered
#'
#' @returns a list with:
#' \describe{
#'     \item{component_graph}{The component subgraph}
#'     \item{reaction_sources}{A table mapping reactions to pathway sources}
#' }
#' @keywords internal
extend_components_list <- function (napistu_list, component_graph, min_pw_size = 3) {
    
    validate_napistu_list(napistu_list)
    sbml_dfs <- napistu_list$sbml_dfs
    reactions_source_total_counts <- napistu_list$reactions_source_total_counts
    napistu <- napistu_list$python_modules$napistu
    checkmate::assert_class(component_graph, "napistu.network.ng_core.NapistuGraph")
    
    vertices_and_edges <- napistu$network$ig_utils$graph_to_pandas_dfs(component_graph)
    vertices <- vertices_and_edges[[1]] %>%
        tibble::as_tibble() %>%
        dplyr::rename(node = name)
    
    reaction_sources <- napistu$network$ng_utils$get_minimal_sources_edges(
        vertices,
        sbml_dfs,
        min_pw_size = 1,
        source_total_counts = reactions_source_total_counts
    )
    
    component_list <- list(
        component_graph = component_graph,
        reaction_sources = reaction_sources
    )
    
    return(component_list)
}


plot_one_component_render <- function (
    component_network,
    reaction_sources,
    score_label,
    score_palette,
    network_layout = "fr",
    edge_weights = NULL,
    edge_width = 0.5,
    show_edges_if = NULL,
    vertex_size = 6,
    target_plot_width = 6
) {
    
    validate_network_layout(network_layout)
    checkmate::assert_number(edge_width, na.ok = TRUE, lower = 0)
    validate_show_edges_if(show_edges_if)
    checkmate::assert_number(vertex_size, lower = 0)
    validate_target_plot_width(target_plot_width)
    
    rendering_prep_list <- prepare_rendering(
        component_network,
        reaction_sources = reaction_sources,
        network_layout = network_layout,
        edge_weights = edge_weights,
        target_plot_width = target_plot_width
    )
    component_grob <- rendering_prep_list$network_grob
    vertices_df <- rendering_prep_list$vertices_df
    pathway_coords <- rendering_prep_list$pathway_coords
    
    # title setup
    nodes_summary <- vertices_df %>%
        dplyr::count(node_type) %>%
        dplyr::mutate(label = glue::glue("{n} {node_type}")) %>%
        purrr::pluck("label") %>%
        glue::glue_collapse(sep = " & ")
    
    plot_title <- glue::glue(
        "Visualizing a subgraph containing {nodes_summary} vertices"
    )
    
    if (!is.null(score_label)) {
        plot_title <- plot_title + glue::glue("<br>overlaying **{score_label}**")
    }
    
    if (!is.null(reaction_sources) && nrow(reaction_sources) > 0) {
        component_grob <- add_pathway_outlines(component_grob, pathway_coords)
    }
    
    color_scheme <- add_node_color_palette(
        component_grob,
        vertices_df,
        score_palette
    )
    color_by <- color_scheme$color_by
    component_grob <- color_scheme$grob
    
    if (!is.na(edge_width) && edge_width > 0) {
        component_grob <- add_edges_by_reversibility(component_grob, edge_width, show_edges_if, vertex_size)
    }
    
    # neighborhood-specific plotting
    
    component_grob <- component_grob +
        ggraph::geom_node_point(aes(
            shape = factor(node_type),
            color = !!color_by,
            size = vertex_size
        )) 
    
    component_grob <- add_node_names_and_themes(
        component_grob,
        vertices_df,
        plot_title
    )
    
    return(component_grob)
}