#' Validate Napistu Configuration Object
#'
#' @param napistu_config A napistu configuration object of class 'napistu_config'
#'   containing all necessary configuration settings for the loading required
#'   assets and configuring and/or using an exiting appropriate Python environment
#'   
#' @return Invisible TRUE if valid, throws error if invalid
#' 
#' @keywords internal
validate_napistu_config <- function(napistu_config) {
    checkmate::assert_class(napistu_config, NAPISTU_CONSTANTS$NAPISTU_CONFIG_CLASS)
    return(invisible(TRUE))
}

#' Validate Assets Config
#' 
#' @param assets_config List containing asset configuration settings, typically
#'   extracted from the main napistu_config object
#'   
#' @return Invisible TRUE if valid, throws error if invalid
#' 
#' @keywords internal
validate_assets_config <- function (assets_config) {
    
    checkmate::assert_list(assets_config)
    purrr::walk(assets_config, checkmate::assert_string)
    return(invisible(TRUE))
}

#' Validate Python Config
#' 
#' @param python_config List describing whether to use the system python,
#'   virtual environment or conda and what environment to use if using an
#'   environment manager. Typically extracted from the main napistu_config
#'   object.
#'   
#' @return Invisible TRUE if valid, throws error if invalid
#' 
#' @keywords internal
validate_python_config <- function (python_config) {
    
    checkmate::assert_list(python_config)
    purrr::walk(python_config, checkmate::assert_string)
    return(invisible(TRUE))
}

#' Validate Asset Paths
#' 
#' @param asset_paths List containing the paths of to-be-loaded assets
#'   
#' @return Invisible TRUE if valid, throws error if invalid
#' 
#' @keywords internal
validate_asset_paths <- function (asset_paths) {
    
    checkmate::assert_list(asset_paths)
    purrr::walk(asset_paths, checkmate::assert_string)
    purrr::walk(asset_paths, checkmate::test_file_exists)
    return(invisible(TRUE))
}

#' Validate Verbose Parameter
#'
#' @param verbose Logical scalar; if TRUE (default), prints informative messages
#'   about the current operation progress. If FALSE, runs silently.
#'
#' @return Invisible TRUE if valid, throws error if invalid
#' 
#' @keywords internal
validate_verbose <- function(verbose) {
    checkmate::assert_logical(verbose, len = 1)
    return(invisible(TRUE))
}

#' Validate Verbose Parameter
#'
#' @param overwrite Overwrite existing caches 
#'
#' @return Invisible TRUE if valid, throws error if invalid
#' 
#' @keywords internal
validate_overwrite <- function(overwrite) {
    checkmate::assert_logical(overwrite, len = 1)
    return(invisible(TRUE))
}

#' Validate Napistu List
#' 
#' Verify that a Napistu List contains the expected Napistu assets
#' 
#' @param napistu_list A list containing loaded assets and bindings to
#'   Python modules.
#'
#' \describe{
#'     \item{sbml_dfs}{
#'         SBML_dfs - the core pathway representation of the Napistu Python library
#'         }
#'     \item{napistu_graph}{
#'         Network graph - a Python igraph subclass with Napistu-specific
#'         attributes and methods
#'         }
#'     \item{species_identifiers}{Species identifier mappings}
#'     \item{precomputed_distances}{optional, distances between species nodes}
#'     \item{species_names}{
#'          A tibble containing the names of all genes, proteins, molecules, etc
#'          }
#'     \item{identifiers_nest}{
#'          A tibble with one row per ontology and a nested tibble containing
#'          all the identifiers and their corresponding molecular species
#'          }
#'     \item{python_modules}{A named list of Python modules: `napistu`}
#'     \item{python_environment}{See \link{validate_python_environment}}
#'     \item{napistu_config}{
#'         A `napistu_config` object dictating how the `napistu_list` was initialized
#'     }
#'     \item{loaded_at}{A date-time object indicating when `napistu_list` was initialized}
#'   }
#'
#' @return Invisible TRUE if valid, throws error if invalid
#' 
#' @keywords internal
validate_napistu_list <- function (napistu_list) {
    
    checkmate::assert_class(napistu_list, NAPISTU_CONSTANTS$NAPISTU_LIST_CLASS)
    
    # validate napistu_list-level assets
    
    ensure_required_keys(names(napistu_list), NAPISTU_CONSTANTS$NAPISTU_LIST_LEVEL_VARS, "napistu_list")
    checkmate::assert_class(napistu_list$napistu_config, NAPISTU_CONSTANTS$NAPISTU_CONFIG_CLASS)
    checkmate::assert_class(napistu_list$loaded_at, "POSIXct")
    
    # validate asset and python lists which are merged in the napistu_list
    
    validate_asset_list(napistu_list)
    validate_python_list(napistu_list)
    
    return(invisible(TRUE))
}

#' Validate Assets List
#' 
#' @param asset_list A list containing loaded assets including:
#' \describe{
#'     \item{sbml_dfs}{
#'         SBML_dfs - the core pathway representation of the Napistu Python library
#'         }
#'     \item{napistu_graph}{
#'         Network graph - a Python igraph subclass with Napistu-specific
#'         attributes and methods
#'         }
#'     \item{species_identifiers}{Species identifier mappings}
#'     \item{precomputed_distances}{optional, distances between species nodes}
#'     \item{species_names}{
#'          A tibble containing the names of all genes, proteins, molecules, etc
#'          }
#'     \item{identifiers_nest}{
#'          A tibble with one row per ontology and a nested tibble containing
#'          all the identifiers and their corresponding molecular species
#'          }
#'   }
#'
#' @return Invisible TRUE if valid, throws error if invalid
#' 
#' @keywords internal
validate_asset_list <- function (asset_list) {
    
    REQUIRED_KEYS <- c(NAPISTU_CONSTANTS$REQUIRED_ASSETS, NAPISTU_CONSTANTS$REQUIRED_DERIVED_ASSETS)
    ensure_required_keys(names(asset_list), REQUIRED_KEYS, "asset_list")
    
    # required attributes
    checkmate::assertClass(asset_list$sbml_dfs, "napistu.sbml_dfs_core.SBML_dfs")
    checkmate::assertClass(asset_list$napistu_graph, "igraph.Graph")
    checkmate::assertDataFrame(asset_list$species_identifiers)
    
    # optional attributes
    # TO DO - this needs to be deserialized correctly
    if (!is.null(asset_list$precomputed_distances)) {
        checkmate::assertString(asset_list$precomputed_distances)
    }
    
    # required derived attributes
    
    checkmate::assertDataFrame(asset_list$species_names)
    checkmate::assertDataFrame(asset_list$identifiers_nest)
    
    return(invisible(TRUE))
}

#' Validate Python List
#'
#' @param python_list A list containing reticulate bindings to Python packages and summaries of the Python environment.
#' \describe{
#'     \item{python_modules}{A named list of Python modules: `napistu`}
#'     \item{python_environment}{See \link{validate_python_environment}}
#' }
#' 
#' @return Invisible TRUE if valid, throws error if invalid
#' 
#' @keywords internal
validate_python_list <- function (python_list) {
    
    checkmate::assertList(python_list)
    ensure_required_keys(names(python_list), NAPISTU_CONSTANTS$PYTHON_CONFIG_VARS, "python_list")
    
    # validate modules
    python_modules <- names(python_list$python_modules)
    missing_modules <- setdiff(NAPISTU_CONSTANTS$REQUIRED_PYTHON_MODULES, python_modules)
    if (length(missing_modules) > 0) {
        cli::cli_abort("The {.arg  python_list$python_modules} is missing reticulate bindings to the following package{?s}: {.field {missing_modules}}")
    }
    purrr::walk(python_modules, "python.builtin.module")
    
    # validate environment spec
    validate_python_environment(python_list$python_environment)
    
    return(invisible(TRUE))
}

#' Validate Python Environment
#' 
#' @param python_environment Summary of the loaded Python environment and specification for cleanup procedure used by \link{cleanup_napistu}
#' \describe{
#'     \item{path}{The environment being used}
#'     \item{type}{The type of reticulate configuration: "virtualenv", "conda", "python"}
#'     \item{created_by_napistu}{Was the environment created by Napistu? If so it can be removed by \link{cleanup_napistu}}
#'     \item{miniconda_installed}{Was Miniconda installed by Napistu? If so it can be removed by \link{cleanup_napistu}}
#' }
#' 
#' @return Invisible TRUE if valid, throws error if invalid
#' 
#' @keywords internal
validate_python_environment <- function (python_environment) {
    
    checkmate::assertList(python_environment)
    ensure_required_keys(names(python_environment), NAPISTU_CONSTANTS$PYTHON_ENV_VARS, "python_environment")
    
    checkmate::assert_string(python_environment$path)
    checkmate::assert_string(python_environment$type)
    checkmate::assert_logical(python_environment$created_by_napistu, len = 1)
    checkmate::assert_logical(python_environment$miniconda_installed, len = 1)
    
    return(invisible(TRUE))
}

ensure_required_keys <- function (keys, required_keys, object_name) {
    
    checkmate::assert_character(keys)
    checkmate::assert_character(required_keys)
    checkmate::assert_string(object_name)
    
    missing_keys <- setdiff(required_keys, keys)
    if (length(missing_keys) > 0) {
        cli::cli_abort("The {.arg {object_name}} list is malformed; it is missing the following key{?s}: {.field {missing_keys}}")
    }
    
}
