#' Random Species
#' 
#' Selects a random gene/molecule for demos
#' 
#' @inheritParams validate_napistu_list
#' 
#' @returns a species ID matching the primary key of the `species` table of the
#' SBML_dfs object.
#' 
#' @examples
#' setup_napistu_list(create_napistu_config(), overwrite = TRUE)
#' random_species(napistu_list)
#' @export
#' @keywords internal
random_species <- function (napistu_list) {
    validate_napistu_list(napistu_list)
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
