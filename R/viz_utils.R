#' Prepare Score Overlays
#' 
#' Merge a table of vertex-level data with a network representation.
#' 
#' @param vertices a table of vertices containing the variable specified in `join_on`
#' @param score_overlay optional, vertex-level scores containing `score`
#'   and the merging attribute specified in `join_on`
#' @param join_scores_on variable to use when merging vertices and score
#' @keywords internal
prepare_score_overlays <- function (vertices, score_overlay = NULL, join_scores_on = "s_id") {
    
    checkmate::assert_data_frame(vertices)
    checkmate::assert_data_frame(score_overlay, null.ok = TRUE)
    checkmate::assert_choice(join_scores_on, colnames(vertices))
    
    if (!is.null(score_overlay)) {
        checkmate::assertDataFrame(score_overlay)
        if (!(join_scores_on %in% colnames(score_overlay))) {
            cli::cli_abort(
                "{.field {join_scores_on}} is not present in {.arg score_overlay} but
                you are trying to join on {join_scores_on}"
            )
        }
        
        if (!("score" %in% colnames(score_overlay))) {
            cli::cli_abort(
                "{.field score} is missing from {.arg score_overlay}.
                It must be included if score_overlay is provided"
            )
        }
        
        vertices <- vertices %>%
            dplyr::left_join(
                score_overlay %>%
                    dplyr::select(!!rlang::sym(join_scores_on), score),
                by = join_scores_on
            )
    } 
    
    return(vertices)
}

#' Label Vertices
#' 
#' Takes a pre-sorted table of vertices and annotates the top entries with
#' visually appealing ggtext labels.
#' 
#' @param vertices a table of the vertices to plot
#' @param max_labeled_species the number of vertices to try to label. Some
#'   labels may be dropped to improve clarity.
#' @param node_types_to_label what `node_type`s in `vertices` to consider for labeling
#' @param always_label always include these vertices regardless of their priority. Provide vertex names (generally starting with SC or R).
#'
#' @returns vertices with a `ggtext_label` variable added
#' @keywords internal
label_vertices <- function(
    vertices,
    max_labeled_species,
    node_types_to_label = "species",
    always_label = NULL
) {
    
    checkmate::assert_data_frame(vertices)
    checkmate::assert_integerish(max_labeled_species)
    checkmate::assert_character(node_types_to_label)
    checkmate::assert_character(always_label, null.ok = TRUE)
                                 
    REQUIRED_VERTEX_ATTRS <- c("name", "node_type", "node_orientation")
    missing_reqs <- setdiff(REQUIRED_VERTEX_ATTRS, colnames(vertices))
    if (length(missing_reqs) > 0) {
        cli::cli_abort("{length(missing_reqs)} required variable{?s} missing from {.arg vertices}: {missing_reqs}")
    }
    
    nodes_to_label <- vertices %>%
        dplyr::filter(node_type %in% node_types_to_label) %>%
        dplyr::slice(1:max_labeled_species) %>%
        {.$name}
    
    if (!is.null(always_label)) {
        total_label <- length(always_label)
        non_matches <- always_label[!(always_label %in% vertices$name)]
        
        if (non_matches > 0) {
            cli::cli_alert_info(
                "Can't label {length(non_matches)} of the {total_label} labels in {.arg always_label} because they are not present in the vertice's names: {non_matches}"
            )
        }
    }
    
    nodes_to_label <- unique(c(nodes_to_label, always_label))
    
    annotated_vertices <- vertices %>%
        dplyr::mutate(
            selected_node_name = ifelse(name %in% nodes_to_label, node_name, NA_character_)
        ) %>%
        dplyr::rowwise() %>%
        dplyr::mutate(
            # extract name and compartment from species name
            compartment = stringr::str_match(selected_node_name, "\\[([A-Za-z0-9 -]+)\\]$")[2],
            species_name = stringr::str_split(selected_node_name, " \\[([A-Za-z0-9 -]+)\\]$")[[1]][1]
        ) %>%
        dplyr::ungroup()
    
    defined_compartments <- unique(annotated_vertices$compartment[!is.na(annotated_vertices$compartment)])
    if ("focal" %in% vertices$node_orientation) {
        focal_compartment <- annotated_vertices$compartment[annotated_vertices$node_orientation == "focal"]
    } else if (length(defined_compartments) == 1){
        focal_compartment <- defined_compartments
    } else {
        focal_compartment <- "cellular_component" # stub
    }
    
    out <- annotated_vertices %>%
        dplyr::mutate(
            # ignore compartment if its the same as the focal species compartment
            compartment = ifelse(compartment == focal_compartment & node_orientation != "focal", NA, compartment),
            compartment = stringr::str_c("\n", stringr::str_trunc(compartment, 20)),
            # replace with string so glue works
            compartment = ifelse(is.na(compartment), "", compartment),
            species_name = stringr::str_wrap(stringr::str_trunc(species_name, 40), 25),
            label = ifelse(!is.na(species_name), glue::glue("{species_name}{compartment}"), NA),
            ggtext_label = ifelse(
                !is.na(species_name),
                glue::glue("**{species_name}**{compartment}") %>%
                    stringr::str_replace_all(" ?\\n ?", "<br>"),
                NA
            )
        )
    
    return(out)
}

#' Stub Grob
#' 
#' Create a blank ggplot which can be interested in Shiny apps if a valid plot
#' can't be created
#' 
#' @param stub_str the text to show in the stub
#' 
#' @returns the stubbed grob
#' @keywords internal
stub_grob <- function (stub_str) {
    grob <- invalid_plot <- ggplot(
        data.frame(x = 0, y = 0), aes(x = x, y = y)
    ) +
        geom_text(label = stub_str, size = 10) +
        theme(text = element_blank(), line = element_blank())
    
    return(grob)
    
}

#' Prepare Rendering
#' 
#' Prepare for the network plot by laying out the pathway in a way that loosly
#' groups related vertices and by suppressed overplotted labels
#' 
#' @param r_graph an R igraph network
#' @param reaction_sources an optional mapping from `r_id` to `pathway_id` and `name`. 
#' @inheritParams get_layout_properties
#' 
#' @returns a list containing
#' \describe{
#'     \item{neighborhood_grob}{A ggplot `grob` with no aethetics or geoms added}
#'     \item{vertices_df}{A table of vertices with upstream metadata plus coordinates and updated labels}
#'     \item{pathway_coords}{The bounding box of reaction's assigned to each pathway}
#' }
#' @keywords internal
prepare_rendering <- function (r_graph, reaction_sources, network_layout, edge_weights) {
    
    checkmate::assert_class(r_graph, "igraph")
    
    gg_network_layout <- layout_with_reaction_sources(
        r_graph,
        network_layout,
        edge_weights
    )
    network_grob <- ggraph::ggraph(graph = gg_network_layout)
    
    # find obscured labels due to overplotting
    obscured_labels <- find_obscured_labels(gg_network_layout)
    graph_height <- diff(range(gg_network_layout$y))
    vertices_df <- tibble::as_tibble(gg_network_layout) %>%
        dplyr::mutate(
            ggtext_label = ifelse(ggtext_label %in% obscured_labels, NA, ggtext_label),
            n_lines = stringr::str_count(ggtext_label, "<br>"),
            label_y = y + ((n_lines + 3) * graph_height / 120)
        )
    
    if (!is.null(reaction_sources) && nrow(reaction_sources) > 0) {
        pathway_coords <- layout_pathway_sources(gg_network_layout, reaction_sources)
    } else {
        pathway_coords <- NULL
    }
    
    out <- list(
        network_grob = network_grob,
        vertices_df = vertices_df,
        pathway_coords = pathway_coords
    )
    
    return(out)
}

#' Get Layout Properties
#'
#' @param network_layout method to used for creating a network layout (e.g., `fr`, `kk`, `drl`)
#' 
#' @returns a list containing:
#' \describe{
#'     \item{layout_fxn}{The igraph function used for laying out the graph}
#'     \item{weights_attract}{TRUE if large weights pull vertices together; FALSE if large weights push them apart}
#' }
#'
#' @keywords internal
get_layout_properties <- function (network_layout = "fr") {
    
    VALID_LAYOUTS <- c("fr", "kk", "drl")
    checkmate::assert_choice(network_layout, VALID_LAYOUTS)

    out <- if (network_layout == "fr") {
        list(
            layout_fxn = igraph::layout_with_fr,
            weights_attract = TRUE
        )
    } else if (network_layout == "kk") {
        list(
            layout_fxn = igraph::layout_with_kk,
            weights_attract = FALSE
        )
    } else if (network_layout == "drl") {
        list(
            layout_fxn = igraph::layout_with_drl,
            weights_attract = FALSE
        )
    }
    
    return(out)
}

#' Process Weights for Graph Layout
#'
#' @param r_graph An R igraph object
#' @param edge_weights Numeric vector of edge weights, character string naming an edge attribute,
#'   NULL to use graph's "weight" attribute, or NA to explicitly use no weights
#' @param network_layout Layout method being used (affects weight interpretation)
#'
#' @returns Numeric vector of processed weights, or NA if no weights available/requested
#'
#' @examples
#' \dontrun{
#' # Create a sample graph
#' g <- igraph::make_ring(4)
#' igraph::E(g)$weight <- c(0.1, 0.5, 0.2, 0.8)
#' 
#' # Use default "weight" attribute with FR layout (weights inverted for attraction)
#' process_weights_for_layout(g, network_layout = "fr", edge_weights = NULL)
#' 
#' # Use custom attribute with KK layout (weights used as-is for repulsion)
#' igraph::E(g)$confidence <- c(0.9, 0.3, 0.7, 0.1)
#' process_weights_for_layout(g, network_layout = "kk", edge_weights = "confidence")
#' 
#' # Explicitly provide weights
#' process_weights_for_layout(g, network_layout = "drl", edge_weights = c(0.2, 0.4, 0.6, 0.8))
#' 
#' # Explicitly use no weights
#' process_weights_for_layout(g, network_layout = "fr", edge_weights = NA)
#' }
#' @keywords internal
process_weights_for_layout <- function (r_graph, network_layout, edge_weights = NULL) {
    
    checkmate::assert_class(r_graph, "igraph")
    checkmate::assert_string(network_layout)
    
    # Step 1: Handle explicit NA (user wants no weights)
    if (length(edge_weights) == 1 && is.na(edge_weights)) {
        return(NA)
    }
    
    # Step 2: Extract weights based on input type
    if (is.null(edge_weights)) {
        # Check if there is a weight attribute in the graph
        if ("weight" %in% igraph::edge_attr_names(r_graph)) {
            edge_weights <- igraph::edge_attr(r_graph, "weight")
        } else {
            # No weights provided or found
            cli::cli_alert_warning("No edge weights provided and no 'weight' attribute found in graph. Weights will not be used.")
            return(NA)
        }
    } else if (is.character(edge_weights) && length(edge_weights) == 1) {
        # edge_weights is an attribute name
        if (!edge_weights %in% igraph::edge_attr_names(r_graph)) {
            cli::cli_alert_warning("Edge attribute '{edge_weights}' not found in graph. Weights will not be used.")
            return(NA)
        }
        edge_weights <- igraph::edge_attr(r_graph, edge_weights)
    }
    
    # Step 3: Validate weights
    if (!is.numeric(edge_weights)) {
        cli::cli_alert_warning("Edge weights must be numeric. Weights will not be used.")
        return(NA)
    }
    
    if (any(edge_weights <= 0, na.rm = TRUE)) {
        cli::cli_alert_warning("Edge weights must be positive. Found non-positive values. Weights will not be used.")
        return(NA)
    }
    
    if (length(edge_weights) != igraph::ecount(r_graph)) {
        cli::cli_alert_warning("Length of edge weights ({length(edge_weights)}) does not match number of edges in graph ({igraph::ecount(r_graph)}). Weights will not be used.")
        return(NA)
    }
    
    # node_type == "pathway" vertices and edges from members to pathway are
    # to assist with network layout (and these vertices are later removed)
    # currently, the edges linking members to pathways don't receive a weight
    # so we add one here
    node_types <- igraph::vertex_attr(r_graph, "node_type")
    if (any(is.na(node_types))) {
        cli::cli_abort("Some vertices were missing node types. Please contact a dev.")
    }
    
    pathway_nodes <- igraph::vertex_attr(r_graph, "name")[igraph::vertex_attr(r_graph, "node_type") == NAPISTU_CONSTANTS$NODE_TYPES[["PATHWAY"]]]
    if (length(pathway_nodes) > 0) {
        edgelist <- igraph::as_edgelist(r_graph)
        valid_missing_weight_mask <- edgelist[,1] %in% pathway_nodes | edgelist[,2] %in% pathway_nodes
        
        edge_weights[valid_missing_weight_mask] <- min(edge_weights, na.rm = TRUE)
    }
    
    # flag legitimate missing edge weights
    
    if (any(is.na(edge_weights))) {
        cli::cli_alert_warning("Weights attribute includes NAs. Weights will not be used.")
        return(NA)
    }
    
    # Step 4: Determine if weights need inversion based on layout type
    weights_attract <- get_layout_properties(network_layout)$weights_attract
    
    # Step 5: Process weights based on layout requirements
    # In Napistu, small weights = high confidence interactions
    # For attraction layouts (FR): invert so high confidence becomes strong attraction
    # For repulsion layouts (KK, DRL): use as-is since small values = weak connections
    
    processed_weights <- if (weights_attract) {
        # Invert weights using reciprocal transformation
        # High confidence (small values) -> strong attraction (large values)
        epsilon <- min(edge_weights[edge_weights > 0], na.rm = TRUE) * 0.01
        1 / (edge_weights + epsilon)
    } else {
        # Use weights as-is for repulsion-based layouts
        edge_weights
    }
    
    # Ensure all weights are positive (required by igraph)
    processed_weights <- pmax(processed_weights, .Machine$double.eps)
    
    return(processed_weights)
}

#' Layout With Reaction Sources
#' 
#' Create a `ggraph` layout trying to cluster vertices which are in the same pathway
#' and with appropriate handling of edge weights.
#' 
#' @inheritParams process_weights_for_layout 
#' 
#' @returns a `ggraph` `gg_network_layout` object
#' 
#' @keywords internal
layout_with_reaction_sources <- function(r_graph, network_layout = "fr", edge_weights = NULL) {
    
    checkmate::assertClass(r_graph, "igraph")
    # create a layout
    set.seed(1234)
    
    layout_fxn <- get_layout_properties(network_layout)$layout_fxn
    processed_edge_weights <- process_weights_for_layout(
        r_graph,
        network_layout = network_layout,
        edge_weights = edge_weights
    )
    
    # perform network layout
    network_layout_coords <- do.call(
        layout_fxn,
        list(graph = r_graph, weights = edge_weights, dim = 2)
    )
    
    # pull out vertex coordinates to provide more flexibility
    network_layout_coords <- as.data.frame(network_layout_coords)
    colnames(network_layout_coords) <- c("x", "y")
    
    # remove pathways as layout nodes
    is_pathway_node <- igraph::vertex_attr(r_graph, "node_type") == NAPISTU_CONSTANTS$NODE_TYPES[["PATHWAY"]]
    network_layout_coords <- network_layout_coords[!is_pathway_node, ]
    r_graph <- igraph::delete_vertices(r_graph, igraph::vertex_attr(r_graph, "name")[is_pathway_node])
    
    # generate a ggraph object with node coordinates (for ggplot2-based plotting)
    gg_network_layout <- ggraph::create_layout(r_graph, layout = "manual", x = network_layout_coords$x, y = network_layout_coords$y)
    
    return(gg_network_layout)
}


layout_pathway_sources <- function(gg_network_layout, reaction_sources, max_pathways = 8L) {
    pathway_reaction_coords <- gg_network_layout %>%
        dplyr::select(name, x, y) %>%
        dplyr::inner_join(
            reaction_sources %>%
                dplyr::select(r_id, label = name),
            by = c("name" = "r_id")
        )
    
    if (nrow(pathway_reaction_coords) == 0) {
        cli::cli_alert_warning(
            "zero pathways were represented in the network despite passing
            a non-null value for `reaction_sources`. Returning NULL"
        )
        return(NULL)
    }
    
    pathway_coords <- pathway_reaction_coords %>%
        dplyr::group_by(label) %>%
        dplyr::summarize(
            n_species = dplyr::n(),
            x_min = min(x),
            x_max = max(x),
            y_min = min(y),
            y_max = max(y),
            w = x_max - x_min,
            h = y_max - y_min
        ) %>%
        dplyr::mutate(label_wrap = stringr::str_wrap(label, width = 50)) %>%
        dplyr::arrange(dplyr::desc(n_species)) %>%
        dplyr::filter(n_species > 1) %>%
        dplyr::slice(1:max_pathways) %>%
        dplyr::mutate(label_wrap = factor(label_wrap, levels = label_wrap))
    
    pathway_coords
}

find_obscured_labels <- function(gg_network_layout) {
    plot_char_width <- 100
    plot_char_height <- 50
    graph_width <- diff(range(gg_network_layout$x))
    graph_height <- diff(range(gg_network_layout$y))
    
    # format labels ordered by plotting priority
    labels <- gg_network_layout %>%
        as.data.frame() %>%
        dplyr::select(x, y, ggtext_label) %>%
        dplyr::filter(!is.na(ggtext_label)) %>%
        dplyr::mutate(n_lines = stringr::str_count(ggtext_label, "<br>") + 1) %>%
        dplyr::rowwise() %>%
        dplyr::mutate(
            max_width = max(purrr::map_int(
                stringr::str_split(stringr::str_replace_all(ggtext_label, "\\*\\*", ""), "<br>")[[1]],
                stringr::str_length
            ))
        ) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(
            row_id = 1:dplyr::n(),
            # adding characters to width and height to represent box
            w = graph_width * ((max_width + 2) / plot_char_width),
            h = graph_height * ((n_lines + 1) / plot_char_height),
            x_min = x - w / 2,
            x_max = x + w / 2,
            y_min = y - h / 2,
            y_max = y + h / 2
        )
    
    # define all x all combinations
    overlapping_labels <- tidyr::expand_grid(
        row_1 = labels$row_id,
        row_2 = labels$row_id
    ) %>%
        dplyr::left_join(
            labels %>%
                dplyr::select(
                    row_1 = row_id,
                    x_min_1 = x_min,
                    x_max_1 = x_max,
                    y_min_1 = y_min,
                    y_max_1 = y_max
                ),
            by = "row_1"
        ) %>%
        dplyr::left_join(
            labels %>%
                dplyr::select(
                    row_2 = row_id,
                    x_min_2 = x_min,
                    x_max_2 = x_max,
                    y_min_2 = y_min,
                    y_max_2 = y_max
                ),
            by = "row_2"
        ) %>%
        # filter to lower-diagonal comparison
        dplyr::filter(row_1 < row_2) %>%
        # find overlaps
        dplyr::mutate(
            # calculate overlaps in x, and y separately
            # two ranges overlap if the end point of one end of a range is between
            # the min and max of the other
            x_overlap = dplyr::case_when(
                x_min_1 < x_max_2 & x_min_1 > x_min_2 ~ TRUE,
                x_max_1 < x_max_2 & x_max_1 > x_min_2 ~ TRUE,
                x_min_2 < x_max_1 & x_min_2 > x_min_1 ~ TRUE,
                x_max_2 < x_max_1 & x_max_2 > x_min_1 ~ TRUE,
                TRUE ~ FALSE
            ),
            y_overlap = dplyr::case_when(
                y_min_1 < y_max_2 & y_min_1 > y_min_2 ~ TRUE,
                y_max_1 < y_max_2 & y_max_1 > y_min_2 ~ TRUE,
                y_min_2 < y_max_1 & y_min_2 > y_min_1 ~ TRUE,
                y_max_2 < y_max_1 & y_max_2 > y_min_1 ~ TRUE,
                TRUE ~ FALSE
            ),
            collision = x_overlap & y_overlap
        ) %>%
        dplyr::filter(collision)
    
    if (nrow(overlapping_labels) == 0) {
        return(NULL)
    }
    
    invalid_labels <- NULL
    while (nrow(overlapping_labels) != 0) {
        # remove the lower priority label of the first row since this will involve
        # the highest plotting priority label.
        # discard all overlaps involving the pruned label
        removed_label <- overlapping_labels$row_2[1]
        invalid_labels <- c(invalid_labels, removed_label)
        
        overlapping_labels <- overlapping_labels %>%
            dplyr::filter(
                row_1 != removed_label,
                row_2 != removed_label
            )
    }
    
    # obscured labels
    return(labels$ggtext_label[labels$row_id %in% invalid_labels])
}

#' Select Score Overlay Palette
#' 
#' Create a palette for coloring nodes based on vertex-level scores.
#' 
#' @param score_palette the name of the palette to use
#' \describe{
#'     \item{indication_scores}{A palette going from [0,1]}
#'     \item{log2 fold-change}{A symmetric blue-black-yellow palette centered on zero}
#' }
#' @param ... extra arguments to pass into the color palette
#' @keywords internal
select_score_overlay_palette <- function(score_palette, ...) {
    
    SCORE_PALETTE_NAMES <- c("indication scores", "log2 fold-change")
    if (is.null(score_palette)) {
        cli::cli_abort("Please provide a value for {.arg score_palette}. Valid palettes are {.field {SCORE_PALETTE_NAMES}}")
    }
    
    if (!(score_palette %in% SCORE_PALETTE_NAMES)) {
        cli::cli_abort("{.field {score_palette}} is not a valid value for {.arg score_palette}. Valid palettes are {.field {SCORE_PALETTE_NAMES}}")
    }
    
    if (score_palette == "indication scores") {
        score_palette_obj <- scale_color_gradientn(
            "Open Target's Indication Score",
            colors = c("gray90", "yellow", "orange", "orangered", "red"),
            limits = c(0, 1),
            breaks = c(0, 0.1, 0.4, 1),
            trans = "sqrt",
            ...
        )
        
    } else if (score_palette == "log2 fold-change") {
        score_palette_obj <- scale_color_gradient2(
            expression(log[2] ~ "fold-change"),
            low = "steelblue1",
            mid = "black",
            high = "yellow",
            midpoint = 0,
            ...
        )
    }
    
    return(score_palette_obj)
}

add_pathway_outlines <- function(grob, pathway_coords) {
    out <- grob + geom_rect(
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
    
    return(out)
}


add_node_color_palette <- function (grob, vertices_df, score_palette) {
    
    if ("score" %in% colnames(vertices_df)) {
        color_by <- rlang::sym("score")
        
        # add the score palette
        score_palette_obj <- select_score_overlay_palette(score_palette)
        out_grob <- grob + score_palette_obj
    } else {
        color_by <- rlang::quo(factor(node_orientation))
        
        out_grob <- grob +
            scale_color_manual(values = c(
                "default" = "gray25",
                "upstream" = "gray25",
                "downstream" = "gray25",
                "focal" = "white"
            )) +
            guides(color = "none")
    } 
    
    out <- list(
        grob = out_grob,
        color_by = color_by
    )
    
    return(out)
}

#' Add Edges By Reversibility with Scalable Arrows
#' 
#' @param grob a ggplot2 grob
#' @param edge_width width of edges on graph
#' @param vertex_size vertex size in ggplot2 units (should match the size used in geom_node_point)
#' @param arrow_scale_factor multiplier for arrow size relative to edge width (default: 2.5)
#' @param vertex_cap_scale multiplier for vertex cap relative to vertex size (default: 0.02)
#' @param min_arrow_size minimum arrow size in inches (default: 0.05)
#' @param max_arrow_size maximum arrow size in inches (default: 0.3)
#' 
#' @keywords internal
add_edges_by_reversibility <- function(
    grob,
    edge_width,
    vertex_size = 6, 
    arrow_scale_factor = 0.1,
    vertex_cap_scale = 0.02,
    min_arrow_size = 0.02, 
    max_arrow_size = 1
) {
    
    # Arrow size scales with edge width
    base_arrow_size <- edge_width * arrow_scale_factor
    arrow_size <- pmax(min_arrow_size, pmin(max_arrow_size, base_arrow_size))
    
    # Vertex cap scales with vertex size
    vertex_cap_radius <- vertex_size * vertex_cap_scale
    vertex_cap <- ggraph::circle(vertex_cap_radius, "inches")
    
    out <- grob +
        # add edges - irreversible with scalable arrows positioned at vertex edge
        ggraph::geom_edge_link(
            data = ggraph_get_edges_by_reversibility(FALSE),
            color = "gray25",
            arrow = grid::arrow(
                type = "closed", 
                length = unit(arrow_size, "inches")
            ),
            edge_width = edge_width,
            end_cap = vertex_cap,      # Positions arrow at vertex boundary
            start_cap = vertex_cap     # Also respect start vertex boundary
        ) +
        # add edges - reversible (no arrows)
        ggraph::geom_edge_link(
            data = ggraph_get_edges_by_reversibility(TRUE),
            color = "gray25",
            edge_width = edge_width
        )
    
    return(out)
}


add_node_names_and_themes <- function (grob, vertices_df, plot_title) {
    
    out <- grob +
        ggtext::geom_richtext(
            data = vertices_df %>%
                dplyr::filter(!is.na(ggtext_label)),
            aes(x = x, y = label_y, label = ggtext_label),
            size = 3
        ) +
        scale_shape_manual("Type", values = c("reaction" = 15, "species" = 19)) +
        scale_size_identity() +
        guides(
            size = "none",
            shape = guide_legend(override.aes = list(size = 10), byrow = TRUE)
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
    
    return(out)
}


ggraph_get_edges_by_reversibility <- function (
    is_reversible,
    format = "short",
    collapse = "none",
    ...
) {
    
    # modification of ggraph::get_edges to include filtering to subsets of edges
    
    checkmate::assertLogical(is_reversible, len = 1)
    checkmate::assertString(format)
    checkmate::assertString(collapse)
    
    collapse_all_edges <- get("collapse_all_edges", envir = asNamespace("ggraph"))
    collapse_dir_edges <- get("collapse_dir_edges", envir = asNamespace("ggraph"))
    format_short_edges <- get("format_short_edges", envir = asNamespace("ggraph"))
    format_long_edges <- get("format_long_edges", envir = asNamespace("ggraph"))
    data_frame0 <- get("data_frame0", envir = asNamespace("ggraph"))
    
    if (!collapse %in% c("none", "all", "direction")) {
        cli::cli_abort("{.arg collapse} must be either {.val none}, {.val all} or {.val direction}")
    }
    dots <- rlang::enquos(...)
    
    function(layout) {
        
        edges <- ggraph::collect_edges(layout)
        
        # filter by reversibility
        if (!("r_isreversible" %in% colnames(edges))) {
            cli::cli_abort("Edges must have a column named 'r_isreversible'")
        }
        
        edges <- edges %>%
            dplyr::filter(r_isreversible == is_reversible)
        
        edges <- switch(
            collapse,
            none = edges,
            all = collapse_all_edges(edges),
            direction = collapse_dir_edges(edges)
        )
        edges <- switch(
            format,
            short = format_short_edges(edges, layout),
            long = format_long_edges(edges, layout),
            cli::cli_abort("Unknown {.arg format}. Use either {.val short} or {.val long}")
        )
        
        extra_data <- lapply(dots, function(x) {
            val <- rlang::eval_tidy(x, edges)
            rep(val, length.out = nrow(edges))
        })
        if (length(extra_data) > 0) {
            edges <- cbind(edges, data_frame0(!!!extra_data))
        }
        attr(edges, "type_ggraph") <- "edge_ggraph"
        edges
    }
}

add_sources_to_graph <- function (vertices, edges, reaction_sources) {
    
    checkmate::assert_data_frame(vertices)
    checkmate::assert_data_frame(edges)
    
    
    edgelist_veritices <- unique(c(edges$from, edges$to))
    extra_edgelist_veritices <- setdiff(edgelist_veritices, vertices$name)
    if (length(extra_edgelist_veritices) > 0) {
        cli::cli_abort("{length(extra_edgelist_veritices) vertices were present in edges but not vertices")
    }
    
    if (!is.null(reaction_sources) && nrow(reaction_sources) > 0) {
        
        extra_source_vertices <- setdiff(reaction_sources$r_id, vertices$name)
        if (length(extra_source_vertices) > 0) {
            cli::cli_abort("{length(extra_source_vertices)} vertices were present in reaction_sources but not vertices")
        }
        
        
        edges <- edges %>%
            dplyr::bind_rows(
                reaction_sources %>%
                    dplyr::select(from = "r_id", to = "pathway_id")
            )
        
        vertices <- vertices %>%
            dplyr::bind_rows(
                reaction_sources %>%
                    dplyr::distinct(name = pathway_id) %>%
                    dplyr::mutate(node_type = NAPISTU_CONSTANTS$NODE_TYPES[["PATHWAY"]])
            )
    }
    
    return(list(edges = edges, vertices = vertices))
}
