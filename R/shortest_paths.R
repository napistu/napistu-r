#' Summarize Shortest Paths
#'
#' @param source_species_id species id to start with
#' @param dest_species_id species id to end at
#' @inheritParams validate_napistu_list
#'
#' @examples
#' setup_napistu_list(create_napistu_config())
#' # NOTE - you may have to run this a few times to find a valid path between 2 random nodes
#' source_species_id <- random_species(napistu_list)
#' dest_species_id <- random_species(napistu_list)
#'
#' summarize_shortest_paths(
#'     napistu_list,
#'     source_species_id,
#'     dest_species_id
#' )
#' @export
summarize_shortest_paths <- function(napistu_list, source_species_id, dest_species_id) {
    
    validate_napistu_list(napistu_list)
    sbml_dfs <- napistu_list$sbml_dfs
    napistu_graph <- napistu_list$napistu_graph
    napistu <- napistu_list$python_modules$napistu
    precomputed_distances <- load_optional_list_value(napistu_list, "precomputed_distances")
    
    checkmate::assertCharacter(source_species_id)
    checkmate::assertCharacter(dest_species_id)
    
    cli::cli_alert_info("Starting summarize_shortest_paths")
    print(glue::glue("Searching for path between {source_species_id} and {dest_species_id}"))
    
    target_species_paths <- napistu$network$ng_utils$compartmentalize_species_pairs(sbml_dfs, source_species_id, dest_species_id)
    
    if (nrow(target_species_paths) > 10) {
        cli::cli_alert_info("{nrow(target_species_paths)} pairs of source and destination species; will only us the first 10")
        target_species_paths <- target_species_paths %>%
            dplyr::slice(1:10)
    }
    
    shortest_paths_list <- try(
        napistu$network$paths$find_all_shortest_reaction_paths(
            napistu_graph,
            sbml_dfs,
            target_species_paths,
            weight_var = "weight",
            precomputed_distances = precomputed_distances
        ),
        silent = TRUE
    )
    
    cli::cli_alert_info("Shortest paths calculated")
    
    if (inherits(shortest_paths_list, "try-error")) {
        cli::cli_alert_warning(paste0("napistu$network$paths$find_all_shortest_reaction_paths errored due to: ", attr(shortest_paths_list, "condition")))
        
        disconnected_plot <- ggplot(data.frame(x = 0, y = 0), aes(x = x, y = y)) +
            geom_text(label = glue::glue("No path exists between these species"), size = 10) +
            theme(text = element_blank(), line = element_blank())
        
        return(list(
            shortest_paths_plot = disconnected_plot,
            shortest_paths_table = tibble::tibble()
        ))
    }
    
    shortest_paths_grob <- plot_shortest_path_network(napistu_list, shortest_paths_list, max_labeled_species = 10L)
    
    return(list(
        shortest_paths_plot = shortest_paths_grob,
        shortest_paths_table = shortest_paths_list[[1]] %>%
            dplyr::select(origin, dest, path, step, node, node_type, label, weight, url) %>%
            dplyr::arrange(origin, dest, path, step)
    ))
}

#' Plot Shortest Path Network
#'
#' @inheritParams validate_napistu_list
#' @param shortest_paths_list results from napistu$network$find_all_shortest_reaction_paths
#' @inheritParams plot_one_neighborhood
#'
#' @examples
#' # NOTE - you may have to run this a few times to find a valid path between 2 random nodes
#' setup_napistu_list(create_napistu_config())
#' sbml_dfs <- napistu_list$sbml_dfs
#' napistu_graph <- napistu_list$napistu_graph
#' napistu <- napistu_list$python_modules$napistu
#' source_species_id <- random_species(napistu_list)
#' dest_species_id <- random_species(napistu_list)
#'
#' target_species_paths <- napistu$network$ng_utils$compartmentalize_species_pairs(
#'     sbml_dfs,
#'     source_species_id,
#'     dest_species_id
#' )
#'
#' shortest_paths_list <- try(napistu$network$paths$find_all_shortest_reaction_paths(
#'     napistu_graph,
#'     sbml_dfs,
#'     target_species_paths,
#'     weight_var = "weight"
#' ),silent = TRUE)
#'
#' if (!("try-error" %in% class(shortest_paths_list))) {
#'     plot_shortest_path_network(
#'         napistu_list,
#'         shortest_paths_list,
#'         max_labeled_species = 10L
#'     )
#' }
#' @export
plot_shortest_path_network <- function(
    napistu_list,
    shortest_paths_list,
    max_labeled_species = 10L
) {
    
    validate_napistu_list(napistu_list)
    napistu_graph <- napistu_list$napistu_graph
    checkmate::assert_list(shortest_paths_list, len = 4)
    checkmate::assert_count(max_labeled_species)
    
    # suppress some nodes labels to avoid overplotting
    nodes_to_label <-
        # preserve source and destination
        c(
            shortest_paths_list[[1]] %>%
                {
                    .$node[.$node == .$origin | .$node == .$dest]
                } %>%
                unique(),
            shortest_paths_list[[1]] %>%
                dplyr::filter(
                    !(node == origin | node == dest),
                    node_type == "species"
                ) %>%
                dplyr::distinct(node, weight) %>%
                dplyr::arrange(weight) %>%
                dplyr::slice(1:max_labeled_species) %>%
                {
                    .$node
                }
        )
    
    all_shortest_reaction_paths_df <- shortest_paths_list[[1]] %>%
        dplyr::relocate(node) %>%
        dplyr::group_by(node) %>%
        dplyr::slice(1) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(
            special_node = factor(
                dplyr::case_when(
                    node == origin ~ "origin",
                    node == dest ~ "destination",
                    TRUE ~ "mediator"
                ),
                levels = c("origin", "destination", "mediator")
            ),
            node_name = label,
            label = dplyr::case_when(
                node %in% nodes_to_label ~ stringr::str_wrap(node_name, 40),
                TRUE ~ NA_character_
            )
        )
    all_shortest_reaction_path_edges_df <- shortest_paths_list[[2]] %>%
        dplyr::relocate(from, to)
    
    # summarize path
    path_labels <- all_shortest_reaction_path_edges_df %>%
        dplyr::group_by(origin, dest, path) %>%
        dplyr::summarize(
            steps = dplyr::n(),
            score = sum(weight),
            .groups = "drop"
        ) %>%
        dplyr::arrange(score) %>%
        dplyr::group_by(dest) %>%
        dplyr::slice(1) %>%
        dplyr::ungroup() %>%
        dplyr::filter(!is.na(origin), !is.na(dest)) %>%
        dplyr::group_by(dest) %>%
        dplyr::mutate(
            path_qual_score = dplyr::case_when(
                score > 50 ~ "dubious",
                score > 30 ~ "possible",
                score > 10 ~ "likely",
                TRUE ~ "highly likely"
            ),
            path_label = glue::glue(
                "**{path_qual_score}** path<br>
        *steps* = {steps}<br>
        *score* = {score}"
            )
        )
    
    # use source info when available
    reaction_sources <- shortest_paths_list[[3]]
    if (!("NULL" %in% class(reaction_sources))) {
        all_shortest_reaction_path_edges_df <- all_shortest_reaction_path_edges_df %>%
            dplyr::bind_rows(
                reaction_sources %>%
                    dplyr::select(from = "r_id", to = "pathway_id")
            )
        
        all_shortest_reaction_paths_df <- all_shortest_reaction_paths_df %>%
            dplyr::bind_rows(reaction_sources %>% dplyr::distinct(node = pathway_id))
    }
    
    shortest_paths_network <- igraph::graph_from_data_frame(
        all_shortest_reaction_path_edges_df,
        directed = napistu_graph$is_directed(),
        vertices = all_shortest_reaction_paths_df
    )
    
    cli::cli_alert_info("R igraph network created")
    
    # format title
    terminal_species <- all_shortest_reaction_paths_df %>%
        dplyr::filter(special_node != "mediator") %>%
        dplyr::group_by(special_node) %>%
        dplyr::slice(1) %>%
        dplyr::ungroup() %>%
        dplyr::rowwise() %>%
        dplyr::mutate(species_name = stringr::str_split(node_name, " \\[([A-Za-z0-9 -]+)\\]$")[[1]][1])
    
    plot_title <- glue::glue(
        "Shortest path(s)<br>
     **from:** {terminal_species$species_name[terminal_species$special_node == 'origin']}<br>
     **to:** {terminal_species$species_name[terminal_species$special_node == 'destination']}
     "
    )
    
    gg_network_layout <- layout_with_reaction_sources(shortest_paths_network)
    
    shortest_paths_grob <- ggraph::ggraph(graph = gg_network_layout)
    
    path_labels_layout <- gg_network_layout %>%
        dplyr::select(name, x, y) %>%
        dplyr::inner_join(path_labels, by = c("name" = "dest"))
    
    if (!("NULL" %in% class(reaction_sources)) && nrow(reaction_sources) > 0) {
        # add reaction sources if available
        pathway_coords <- layout_pathway_sources(gg_network_layout, reaction_sources)
        
        shortest_paths_grob <- shortest_paths_grob +
            geom_rect(
                data = pathway_coords,
                aes(
                    xmin = x_min - 0.05,
                    xmax = x_max + 0.05,
                    ymin = y_min - 0.05,
                    ymax = y_max + 0.05,
                    fill = label_wrap
                ),
                alpha = 0.2
            ) +
            scale_fill_viridis_d("Pathway")
    }
    
    shortest_paths_grob <- shortest_paths_grob +
        ggraph::geom_edge_link(color = "gray25", arrow = grid::arrow(type = "closed", length = unit(0.15, "inches"))) +
        ggraph::geom_node_point(aes(shape = factor(node_type), color = special_node), size = 3) +
        ggraph::geom_node_label(aes(label = label, color = special_node),
                                repel = TRUE, alpha = 0.7, size = 4,
                                label.padding = 0.1, fill = "gray90"
        ) +
        ggtext::geom_richtext(data = path_labels_layout, aes(x = x + 0.2, y = y, label = path_label), color = "RED", hjust = 0, size = 3) +
        scale_shape_manual("Type", values = c("reaction" = 15, "species" = 19)) +
        scale_color_manual(values = c("origin" = "blue", "destination" = "red", "mediator" = "gray25")) +
        guides(
            shape = guide_legend(override.aes = list(size = 8, color = "gray25"), nrow = 2),
            fill = guide_legend(nrow = 2, byrow = TRUE),
            color = "none"
        ) +
        theme_void() +
        theme(
            plot.background = element_rect(fill = "white"),
            legend.text = element_text(color = "black"),
            legend.title = element_text(color = "black"),
            legend.position = "bottom",
            plot.title.position = "plot",
            plot.title = ggtext::element_markdown(size = 11, lineheight = 1.2)
        ) +
        labs(title = plot_title)
    
    return(shortest_paths_grob)
}

#' Find All Shortest Reaction Paths
#' 
#' @inheritParams validate_napistu_list
#' @param source_species_id species ID of the upstream/source vertex
#' @param dest_species_id species ID of the downstream/destination vertex
#' @param weight_var variable in edges to use for edge weights
#'
#' @returns All the shortest paths between `source_species_id` and `dest_species_id`
#'
#' @examples
#' setup_napistu_list(create_napistu_config())
#' source_species_id <- random_species(napistu_list)
#' dest_species_id <- random_species(napistu_list)
#' find_all_shortest_paths(napistu_list, source_species_id, dest_species_id)
#' @export
find_all_shortest_paths <- function(
    napistu_list,
    source_species_id,
    dest_species_id,
    weight_var = "weights"
) {
    
    validate_napistu_list(napistu_list)
    sbml_dfs <- napistu_list$sbml_dfs
    napistu_graph <- napistu_list$napistu_graph
    precomputed_distances <- load_optional_list_value(napistu_list, "precomputed_distances")
    napistu <- napistu_list$python_modules$napistu
    
    target_species_paths <- napistu$network$ng_utils$compartmentalize_species_pairs(
        sbml_dfs,
        source_species_id,
        dest_species_id
    )
    
    shortest_paths_list <- try(
        napistu$network$paths$find_all_shortest_reaction_paths(
            napistu_graph,
            sbml_dfs,
            target_species_paths,
            weight_var = weight_var,
            precomputed_distances = precomputed_distances
        ),
        silent = TRUE
    )
   
    return(shortest_paths_list)
}
