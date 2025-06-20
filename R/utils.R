random_species <- function (napistu_list) {
    return(sample(rownames(napistu_list$sbml_dfs$species), 1))
}

random_cspecies <- function (napistu_list) {
    return(sample(rownames(napistu_list$sbml_dfs$compartmentalized_species), 1))
}

load_optional_list_value <- function (l, value) {
    if (value %in% names(l)) {
        return(l[[value]])
    } else {
        return(NULL)
    }
}

`%||%` <- function(x, y) if (is.null(x)) y else x