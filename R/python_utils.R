#' Convert SBML DFs
#' 
#' Convert a Python Napistu SBML_dfs pathway representation into R tables. This
#' will remove some attributes (identifiers and sources) and all the methods which
#' are essential for the sbml_dfs to function as an SBML_dfs obbject but
#' the core relational structure of the pathway can be more easily explored
#' after this conversion.
#' 
#' @param sbml_dfs an sbml_dfs object, generally accessed from a `napistu_list` 
#' 
#' @returns an S3 r_sbml_dfs object which is a list with one table for each of
#' SBML_dfs' core attributes (compartments, species, compartmentalized_species,
#' reactions, and reaction_species)
#'
#' @examples
#' setup_napistu_list(create_napistu_config())
#' convert_sbml_dfs(napistu_list$sbml_dfs)
#' @export
convert_sbml_dfs <- function (sbml_dfs) {
    
    checkmate::assert_class(sbml_dfs, "napistu.sbml_dfs_core.SBML_dfs")
    
    schema <- sbml_dfs$schema
    required_entities <- names(schema)
    
    r_sbml_dfs <- list()
    for (entity_type in required_entities) {
        r_sbml_dfs[[entity_type]] <- convert_sbml_dfs_table(
            sbml_df = sbml_dfs[[entity_type]],
            table_schema = schema[[entity_type]]
        )
    }
    
    class(r_sbml_dfs) <- c("r_sbml_dfs", "list")
    return(r_sbml_dfs)
}


#' Convert Napistu Graph
#' 
#' Convert a Python `graph.Graph` object to a list of vertices and edges.
#' 
#' @param napistu_graph a Python NapistuGraph object (a subclass of igraph.Graph)
#' @inheritParams validate_napistu_list
#'
#' @returns an S3 r_napistu_graph object which is a list containing vertices
#' and edges.
#' 
#' @examples
#' setup_napistu_list(create_napistu_config())
#' convert_napistu_graph(napistu_list$napistu_graph, napistu_list)
#' @export
convert_napistu_graph <- function (napistu_graph, napistu_list) {
    
    checkmate::assert_class(napistu_graph, "igraph.Graph")
    validate_napistu_list(napistu_list)
    
    graph_tuple <- napistu_list$python_modules$napistu$network$ig_utils$graph_to_pandas_dfs(
        napistu_graph
    ) %>%
        purrr::map(tibble::as_tibble)
    
    names(graph_tuple) <- c("vertices", "edges")
    
    class(graph_tuple) <- c("r_napistu_graph", "list")
    return(graph_tuple)
}


#' Print method for r_sbml_dfs objects
#' @param x An r_sbml_dfs object
#' @param ... Additional arguments (not used)
#' @export
#' @keywords internal
print.r_sbml_dfs <- function(x, ...) {
    cli::cli_h1("SBML Data Frames Collection")
    
    if (length(x) == 0) {
        cli::cli_alert_info("Empty collection")
        return(invisible(x))
    }
    
    for (entity_name in names(x)) {
        df <- x[[entity_name]]
        n_rows <- nrow(df)
        n_cols <- ncol(df)
        col_names <- colnames(df)
        
        # Use field annotation for the entity type
        cli::cli_text("{.field {entity_name}}: {n_rows} row{?s}, {n_cols} column{?s}")
        
        # Show columns with proper styling
        if (n_cols > 0) {
            cli::cli_text("  Columns: {.val {col_names}}")
        }
        
        cli::cli_text("")  # Add spacing
    }
    
    invisible(x)
}

#' Print method for r_napistu_graph objects
#' @param x An r_napistu_graph object
#' @param ... Additional arguments (not used)
#' @export
#' @keywords internal
print.r_napistu_graph <- function(x, ...) {
    cli::cli_h1("Napistu Graph")
    
    if (length(x) == 0) {
        cli::cli_alert_info("Empty graph")
        return(invisible(x))
    }
    
    # Get vertices and edges info
    vertices_df <- x$vertices
    edges_df <- x$edges
    
    n_vertices <- nrow(vertices_df)
    n_edges <- nrow(edges_df)
    
    # Graph summary
    cli::cli_text("Graph with {.strong {n_vertices}} vertice{?s} and {.strong {n_edges}} edge{?s}")
    cli::cli_text("")
    
    # Vertices info
    if (n_vertices > 0) {
        vertex_cols <- colnames(vertices_df)
        cli::cli_text("{.field vertices}: {n_vertices} row{?s}, {ncol(vertices_df)} column{?s}")
        cli::cli_text("  Columns: {.val {vertex_cols}}")
        cli::cli_text("")
    }
    
    # Edges info
    if (n_edges > 0) {
        edge_cols <- colnames(edges_df)
        cli::cli_text("{.field edges}: {n_edges} row{?s}, {ncol(edges_df)} column{?s}")
        cli::cli_text("  Columns: {.val {edge_cols}}")
        cli::cli_text("")
    }
    
    invisible(x)
}

convert_sbml_dfs_table <- function (sbml_df, table_schema) {
    
    # drop identifiers and sources since they are list of custom python objects
    excluded_vars <- NULL
    if ("id" %in% names(table_schema)) {
        excluded_vars <- c(excluded_vars, table_schema$id)
    }
    if ("source" %in% names(table_schema)) {
        excluded_vars <- c(excluded_vars, table_schema$source)
    }
    valid_vars <- setdiff(colnames(sbml_df), excluded_vars)
    
    r_sbml_df <- sbml_df[,valid_vars,drop = FALSE] %>%
        tibble::rownames_to_column(table_schema$pk) %>%
        tibble::as_tibble()
    
    return(r_sbml_df)
}


load_vertex_seq <- function (vertices) {
    # either a character vector or an igraph (py) vertex seq
    if ("character" %in% class(vertices)) {
        return(vertices)
    } else if ("igraph.seq.VertexSeq" %in% class(vertices)) {
        vertex_names <- vertices["name"]
        
        if (length(vertex_names) != length(vertices)) {
            cli::cli_abort("Some vertices selected in {.function load_vertex_seq} were unnamed")
        }
        
        return(vertex_names)
    } else {
        cli::cli_abort("vertices was not a character vector or igraph VertexSeq")
    }
}