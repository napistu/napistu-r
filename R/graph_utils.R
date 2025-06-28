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