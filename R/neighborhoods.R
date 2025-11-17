#' Create Neighborhood Table
#'
#' @inheritParams validate_napistu_list
#' @param species_id species identifier for focal node
#' @inheritParams validate_neighborhoods_network_type
#' @param max_steps number of steps away from focal node allowed
#' @param max_neighbors prune to this number of upstream regulators and
#'   downstream targets
#' @inheritParams validate_verbose
#'
#' @returns a tibble containing one row per neighborhood with nested lists as attributes:
#' \describe{
#'     \item{sc_name}{A human readible name for the focal vertex}
#'     \item{s_id}{The internal unique species id of the focal vertex}
#'     \item{c_id}{The internal unique compartment id of the focal vertex}
#'     \item{sc_id}{The internal unique compartmentalized species id of the focal vertex}
#'     \item{sc_Source}{The Source object for the focal vertex}
#'     \item{vertices}{The vertices in the focal vertex's neighborhood}
#'     \item{edges}{The edges in the focal vertex's neighborhood}
#'     \item{reaction_sources}{The source pathways of the reaction vertices}
#' }
#'
#' @examples
#' setup_napistu_list(create_napistu_config())
#' species_id <- random_species(napistu_list)
#' 
#' create_neighborhood_table(
#'     species_id,
#'     napistu_list = napistu_list,
#'     network_type = "hourglass",
#'     max_steps = 5L,
#' )
#' @export
create_neighborhood_table <- function(
    napistu_list,
    species_id,
    network_type = "hourglass",
    max_steps = 3L,
    max_neighbors = 10L,
    verbose = FALSE
) {
    
    validate_napistu_list(napistu_list)
    napistu_graph <- napistu_list$napistu_graph
    sbml_dfs <- napistu_list$sbml_dfs
    reactions_source_total_counts <- napistu_list$reactions_source_total_counts
    napistu <- napistu_list$python_modules$napistu
    precomputed_distances <- load_optional_list_value(napistu_list, "precomputed_distances")
    
    checkmate::assertCharacter(species_id, len = 1)
    validate_neighborhoods_network_type(network_type)
    checkmate::assertInteger(max_steps, len = 1, lower = 1)
    checkmate::assertInteger(max_neighbors, len = 1, lower = 1)
    validate_verbose(verbose)
    
    cli::cli_alert_info("Starting create_neighborhood_table")
    
    compartmentalized_species <- napistu$network$ng_utils$compartmentalize_species(sbml_dfs, species_id)
    compartmentalized_species_ids <- compartmentalized_species[["sc_id"]]
    
    compartmentalized_species_attrs <- sbml_dfs$compartmentalized_species %>%
        dplyr::filter(rownames(.) %in% compartmentalized_species_ids) %>%
        dplyr::mutate(
            sc_id = rownames(.),
            sc_id = factor(sc_id, levels = compartmentalized_species_ids)
        ) %>%
        dplyr::as_tibble() %>%
        dplyr::arrange(sc_id)
    
    if (
        nrow(compartmentalized_species_attrs) !=
        length(compartmentalized_species_ids)
    ) {
        cli::cli_abort(
            "compartmentalized species attributes were not aligned to IDs,
      this is unexpected behavior"
        )
    }
    
    cli::cli_alert_info(
        "Species -> compartmentalized species finished
    Neighborhoods will be created for {.field {length(compartmentalized_species_ids)}} compartment{?s}:
    {compartmentalized_species_ids}"
    )
    
    neighborhoods <- napistu$network$neighborhoods$find_and_prune_neighborhoods(
        sbml_dfs,
        napistu_graph,
        compartmentalized_species = compartmentalized_species_ids,
        precomputed_distances = precomputed_distances,
        source_total_counts = reactions_source_total_counts,
        order = max_steps,
        network_type = network_type,
        top_n = max_neighbors,
        min_pw_size = 1L,
        verbose = verbose
    )
    
    cli::cli_alert_info("Extracting neighborhood attributes")
    
    neighborhood_table <- compartmentalized_species_attrs %>%
        dplyr::mutate(
            neighborhoods = neighborhoods,
            vertices = purrr::map(
                neighborhoods,
                extract_neighborhood_df,
                "vertices",
                is_null_valid = FALSE
            ),
            edges = purrr::map(
                neighborhoods,
                extract_neighborhood_df,
                "edges",
                is_null_valid = FALSE
            ),
            reaction_sources = purrr::map(
                neighborhoods,
                extract_neighborhood_df,
                "reaction_sources"
            )
        ) %>%
        dplyr::select(-neighborhoods) %>%
        # sort by neighborhood size
        dplyr::arrange(dplyr::desc(purrr::map_int(vertices, nrow)))
    
    cli::cli_alert_info("Completed create_neighborhood_table")
    
    return(neighborhood_table)
}

#' Plot Neighborhoods
#'
#' @inheritParams validate_napistu_list
#' @param neighborhood_table Neighborhood table created by
#'   \code{create_neighborhood_table}
#' @inheritParams create_neighborhood_table
#' @inheritParams plot_one_neighborhood
#' @param ... extra arguments passed to \code{\link{plot_one_neighborhood}}
#'
#' @examples
#' suppressPackageStartupMessages(library(dplyr))
#' setup_napistu_list(create_napistu_config())
#' species_id <- random_species(napistu_list)
#'
#' neighborhood_table <- create_neighborhood_table(
#'     napistu_list,
#'     species_id = species_id,
#'     max_steps = 5L,
#'     max_neighbors = 20L,
#' )
#'
#' # score_overlay <- summarize_indication(
#' #   napistu_list,
#' #   disease_id = "EFO_0000400",
#' #   create_neighborhood_summary_table(neighborhood_table)
#' # )
#'
#' score_overlay <- neighborhood_table %>%
#'     dplyr::select(vertices) %>%
#'     tidyr::unnest(vertices) %>%
#'     dplyr::filter(node_type == "species") %>%
#'     dplyr::distinct(s_id) %>%
#'     dplyr::sample_frac(0.5) %>%
#'     dplyr::mutate(score = stats::rnorm(dplyr::n()))
#'
#' plot_neighborhoods(
#'     napistu_list,
#'     neighborhood_table,
#'     score_overlay,
#'     score_label = "random scores",
#'     score_palette = "log2 fold-change"
#' )
#' @export
plot_neighborhoods <- function(
    napistu_list,
    neighborhood_table,
    score_overlay = NULL,
    score_label = NULL,
    ...
) {
   
    validate_napistu_list(napistu_list)
    validate_neighborhood_table(neighborhood_table)
    checkmate::assertDataFrame(score_overlay, null.ok = TRUE)
    checkmate::assertString(score_label, null.ok = TRUE)
    
    cli::cli_alert_info("Starting plot_one_neighborhood")
    
    neighborhood_table <- neighborhood_table %>%
        # plot each neighborhood
        dplyr::mutate(neighborhood_grob = purrr::pmap(
            list(
                vertices, edges, reaction_sources, sc_id, sc_name),
                plot_one_neighborhood,
                napistu_list = napistu_list,
                score_overlay = score_overlay,
                score_label = score_label,
                ...
                )
            )
    
    cli::cli_alert_info("Aggregating neighborhood plots")
    
    if (nrow(neighborhood_table) > 4) {
        cli::cli_alert_info(
            "{neighborhood_table$s_id[1]} was present in {nrow(neighborhood_table)} compartments, the 4 largest compartment-specific neighborhoods will be shown, excluding
      {paste(neighborhood_table$sc_name[5:nrow(neighborhood_table)], collapse = ', ')}"
        )
        n_plots <- 4
    } else {
        n_plots <- nrow(neighborhood_table)
    }
    
    arranged_neighborhood_plots <- try({
        plots_to_combine <- neighborhood_table$neighborhood_grob[1:n_plots]
        patchwork::wrap_plots(plots_to_combine, ncol = 1)
    }, silent = TRUE)
    
    if ("try-error" %in% class(arranged_neighborhood_plots)) {
        arranged_neighborhood_plots <- ggplot(
            data.frame(x = 0, y = 0), aes(x = x, y = y)
        ) +
            geom_text(
                label = glue::glue("Plotting failed :("),
                size = 10
            ) +
            theme(text = element_blank(), line = element_blank())
    }
    
    return(arranged_neighborhood_plots)
}

#' Plot One Neighborhood
#'
#' @inheritParams validate_napistu_list
#' @param vertices table of species and reactions, produced by \link{create_neighborhood_table}
#' @param edges table of connections between species and reactions, produced by \link{create_neighborhood_table}
#' @param reaction_sources table describing the model(s) each reaction comes from, produced by \link{create_neighborhood_table}
#' @param sc_id compartmentalized species identifier of focal node
#' @param sc_name name of focal node
#' @inheritParams create_neighborhood_table
#' @inheritParams validate_score_overlay_and_join_scores_on
#' @param score_label optional, name of disease being overlaid
#' @inheritParams validate_score_palette
#' @inheritParams validate_vertex_size
#' @inheritParams validate_network_layout
#' @inheritParams add_edges_by_reversibility
#' @inheritParams process_weights_for_layout
#' @inheritParams validate_max_labeled_species
#' @inheritParams validate_target_plot_width
#'
#' @returns a ggplot2 grob
#'
#' @examples
#' suppressPackageStartupMessages(library(dplyr))
#' setup_napistu_list(create_napistu_config())
#' species_id <- random_species(napistu_list)
#'
#' neighborhood_table <- create_neighborhood_table(
#'     napistu_list,
#'     species_id,
#'     network_type = "hourglass",
#'     max_neighbors = 30L,
#'     max_steps = 15L
#' )
#'
#' entry <- 1
#' vertices <- neighborhood_table$vertices[[entry]]
#' edges <- neighborhood_table$edges[[entry]]
#' reaction_sources <- neighborhood_table$reaction_sources[[entry]]
#' sc_id <- neighborhood_table$sc_id[entry]
#' sc_name <- neighborhood_table$sc_name[entry]
#'
#' score_overlay <- vertices %>%
#'     dplyr::filter(node_type == "species") %>%
#'     dplyr::distinct(s_id) %>%
#'     dplyr::sample_frac(0.5) %>%
#'     dplyr::mutate(score = stats::rnorm(dplyr::n()))
#'
#' # score_overlay <- summarize_indication(
#' #   napistu_list,
#' #   disease_id = "EFO_0000400",
#' #   create_neighborhood_summary_table(neighborhood_table)
#' #   )
#'
#' plot_one_neighborhood(
#'     napistu_list,
#'     vertices,
#'     edges,
#'     reaction_sources,
#'     sc_id,
#'     sc_name,
#'     score_overlay = NULL
#' )
#'
#' plot_one_neighborhood(
#'     napistu_list,
#'     vertices,
#'     edges,
#'     reaction_sources,
#'     sc_id,
#'     sc_name,
#'     score_overlay = score_overlay,
#'     score_palette = "log2 fold-change",
#'     edge_width = 0.5
#' )
#' 
#' # advanced features
#' plot_one_neighborhood(
#'     napistu_list,
#'     vertices,
#'     edges,
#'     reaction_sources,
#'     sc_id,
#'     sc_name,
#'     score_overlay = score_overlay,
#'     score_palette = viridis::scale_color_viridis(),
#'     edge_width = 0.5,
#'     show_edges_if = list(weight = list(cutoff = 0.6, retain = "below"))
#' )
#' @export
plot_one_neighborhood <- function(
    napistu_list,
    vertices,
    edges,
    reaction_sources,
    sc_id,
    sc_name,
    score_overlay = NULL,
    score_label = NULL,
    score_palette = NULL,
    join_scores_on = "s_id",
    vertex_size = 6,
    network_layout = "fr",
    edge_weights = NULL,
    edge_width = 0.5,
    show_edges_if = NULL,
    max_labeled_species = 30L,
    target_plot_width = 6
) {
    
    validate_napistu_list(napistu_list)
    napistu_graph <- napistu_list$napistu_graph
    checkmate::assert_data_frame(vertices)
    checkmate::assert_data_frame(edges)
    stopifnot(class(sc_id) %in% c("factor", "character"), length(sc_id) == 1)
    stopifnot(class(sc_name) %in% c("factor", "character"), length(sc_name) == 1)
    validate_score_overlay_and_join_scores_on(score_overlay, join_scores_on)
    checkmate::assert_string(score_label, null.ok = TRUE)
    validate_score_palette(score_palette, score_overlay)
    validate_vertex_size(vertex_size)
    checkmate::assert_integerish(max_labeled_species, len = 1, min = 1)
    checkmate::assert_string(network_layout)
    checkmate::assert_numeric(edge_width, len = 1, min = 0)
    validate_show_edges_if(show_edges_if)
    validate_max_labeled_species(max_labeled_species)
    validate_target_plot_width(target_plot_width)
    
    cli::cli_alert_info("Starting plot_one_neighborhood")
    
    if (nrow(edges) == 0) {
        return(stub_grob(glue::glue("{sc_name} has zero neighbors")))
    }
    
    # add labels
    vertices <- label_vertices(
        vertices %>% dplyr::arrange(path_weight),
        max_labeled_species
    )
    
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
    
    steps_colors <- vertices %>%
        dplyr::distinct(path_length) %>%
        dplyr::filter(!is.na(path_length)) %>%
        dplyr::arrange(path_length) %>%
        dplyr::mutate(path_color = grDevices::colorRampPalette(c("white", "dodgerblue"))(dplyr::n()))
    
    # add pathway sources to help organize layout
    graph_tbls_w_sources = add_sources_to_graph(vertices, edges, reaction_sources)
    
    neighborhood_network <- igraph::graph_from_data_frame(
        graph_tbls_w_sources$edges,
        directed = napistu_graph$is_directed(),
        vertices = graph_tbls_w_sources$vertices
    )
    
    plot_one_neighborhood_render(
        neighborhood_network = neighborhood_network,
        reaction_sources = reaction_sources,
        score_label = score_label,
        score_palette = score_palette,
        network_layout = network_layout,
        vertex_size = vertex_size,
        edge_weights = edge_weights,
        edge_width = edge_width,
        show_edges_if = show_edges_if,
        target_plot_width = target_plot_width
    )
}

plot_one_neighborhood_render <- function(
    neighborhood_network,
    reaction_sources,
    score_label = NULL,
    score_palette = NULL,
    network_layout = "fr",
    vertex_size = 6,
    edge_weights = NULL,
    edge_width = 0.5,
    show_edges_if = NULL,
    target_plot_width = 6
) {
    
    rendering_prep_list <- prepare_rendering(
        neighborhood_network,
        reaction_sources = reaction_sources,
        network_layout = network_layout,
        edge_weights = edge_weights,
        target_plot_width = target_plot_width
    )
    
    neighborhood_grob <- rendering_prep_list$network_grob
    vertices_df <- rendering_prep_list$vertices_df
    pathway_coords <- rendering_prep_list$pathway_coords
    
    focal_species <- vertices_df %>%
        dplyr::filter(node_orientation == "focal")
    stopifnot(nrow(focal_species) == 1)
    neighbors <- vertices_df %>%
        dplyr::filter(node_orientation != "focal")
    
    # title setup
    n_hops <- max(vertices_df$path_length)
    focal_species_name <- stringr::str_replace(focal_species["species_name"][[1]], "<br>", " ")
    focal_compartment <- stringr::str_trim(focal_species[["compartment"]])
    compartment_label <- ifelse(focal_compartment == "", "", glue::glue(" ({focal_compartment})"))
    plot_title <- glue::glue(
        "**{n_hops}** hops neighborhood of<br>
     **{focal_species_name}**{compartment_label}"
    )
    
    if (!is.null(score_label)) {
        plot_title <- plot_title + glue::glue("<br>overlaying **{score_label}**")
    }
    
    if (!is.null(reaction_sources) && nrow(reaction_sources) > 0) {
        neighborhood_grob <- add_pathway_outlines(neighborhood_grob, pathway_coords)
    }
    
    color_scheme <- add_node_color_palette(
        neighborhood_grob,
        vertices_df,
        score_palette
    )
    color_by <- color_scheme$color_by
    neighborhood_grob <- color_scheme$grob
    
    if (!is.na(edge_width) && edge_width > 0) {
        neighborhood_grob <- add_edges_by_reversibility(
            neighborhood_grob,
            edge_width,
            show_edges_if,
            vertex_size = vertex_size
        )
    }
    
    # neighborhood-specific plotting
    
    neighborhood_grob <- neighborhood_grob +
        # add focal node border
        ggraph::geom_node_point(
            data = focal_species,
            aes(x = x, y = y),
            size = vertex_size * 1.1,
            color = "black"
        ) +
        # add focal nodes
        ggraph::geom_node_point(
            aes(shape = factor(node_type), color = !!color_by),
            size = vertex_size
        )+
        # add number of steps
        geom_text(
            data = focal_species,
            aes(x = x, y = y, label = path_length),
            color = "gray10",
            size = vertex_size
        ) +
        geom_text(
            data = neighbors %>% dplyr::filter(path_length <= 9),
            aes(x = x, y = y, label = path_length),
            color = "gray70",
            size = vertex_size * 0.95
        )
    
    neighborhood_grob <- add_node_names_and_themes(
        neighborhood_grob,
        vertices_df,
        plot_title
    )
    
    neighborhood_grob
    return(neighborhood_grob)
}


#' Create Neighborhood Summary Table
#' 
#' Unnest the neighborhoods' vertices
#'
#' @inheritParams validate_neighborhood_table
#' 
#' @returns an unnested table of vertices across all neighborhoods
#' @export
#' @keywords internal
create_neighborhood_summary_table <- function(neighborhood_table) {
    validate_neighborhood_table(neighborhood_table)
    
    neighborhood_summary_table <- neighborhood_table %>%
        dplyr::select(sc_name, vertices) %>%
        tidyr::unnest(vertices) %>%
        dplyr::rename(
            neighbor_sc_id = name,
            neighbor_sc_name = node_name
        )
    
    return(neighborhood_summary_table)
}

extract_neighborhood_df <- function(entry_list, entry_field, is_null_valid = TRUE) {
    chosen_table <- entry_list[[entry_field]]
    
    if (is.null(chosen_table)) {
        if (is_null_valid) {
            return(NULL)
        } else {
            stop("entry_list was NULL and is_null_valid was TRUE")
        }
    }
    
    reticulate::py_to_r(chosen_table) %>%
        dplyr::as_tibble()
}