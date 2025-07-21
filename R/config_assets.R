#' Load Assets
#'
#' Load a set of Napistu files
#'
#' @inheritParams validate_napistu_config
#' @inheritParams validate_python_list
#' @inheritParams validate_verbose
#' 
#' @return A list containing loaded assets including:
#' 
#' \describe{
#'     \item{sbml_dfs}{
#'         SBML_dfs - the core pathway representation of the Napistu Python library
#'      }
#'      \item{napistu_graph}{
#'         Network graph - a Python igraph subclass with Napistu-specific
#'         attributes and methods
#'      }
#'      \item{species_identifiers}{Species identifier mappings}
#'      \item{precomputed_distances}{optional, distances between species nodes}
#'       \item{species_names}{
#'          A tibble containing the names of all genes, proteins, molecules, etc
#'      }
#'      \item{identifiers_nest}{
#'          A tibble with one row per ontology and a nested tibble containing
#'          all the identifiers and their corresponding molecular species
#'      }
#'      \item{reactions_source_total_counts}{
#'          A pd.Series containing the number of reactions each pathway is
#'          associated with
#'      }
#' }
#' 
#' @export
load_assets <- function(napistu_config, python_list, verbose = TRUE) {
    
    validate_napistu_config(napistu_config)
    validate_python_list(python_list)
    validate_verbose(verbose)
    
    assets_config <- napistu_config$assets_config
    validate_assets_config(assets_config)
    
    if (length(assets_config) == 0) {
        if (verbose) {
            cli::cli_inform("No assets configuration specified, loading bundled package data")
        }
        asset_paths <- get_bundled_asset_paths()
    } else {
        if (verbose) {
            cli::cli_inform("Loading assets from configured paths")
        }
        asset_paths <- get_configured_asset_paths(assets_config, verbose)
    }
    
    assets <- load_assets_from_paths(asset_paths, python_list, verbose) %>%
        create_derived_assets(python_list)
    
    return(assets)
}

create_derived_assets <- function (assets, python_list) {
    
    assets$species_names <- assets$sbml_dfs$species %>%
        dplyr::select(-s_Identifiers, -s_Source)
    
    assets$identifiers_nest <- assets$species_identifiers %>%
        tidyr::nest(ontology_ids = -ontology) %>%
        dplyr::mutate(n = purrr::map_int(ontology_ids, nrow)) %>%
        dplyr::arrange(dplyr::desc(n)) %>%
        dplyr::mutate(ontology = factor(ontology, levels = ontology))

    napistu_source_module <- python_list$python_modules$napistu$source
    r_source_total_counts <- napistu_source_module$get_source_total_counts(
        assets$sbml_dfs,
        "reactions"
    )
    
    assets$reactions_source_total_counts <- create_pandas_series(
        data = r_source_total_counts,
        index = names(r_source_total_counts),
        name = "total_counts"
    )
    
    return(assets)
}


#' Load Single Asset File
#'
#' @param file_path Path to asset file
#' @inheritParams validate_python_list 
#' @param asset_name Name of asset for context
#' 
#' @return Loaded asset object
#' 
#' @export
load_single_asset <- function(file_path, python_list, asset_name) {
    
    checkmate::assert_file_exists(file_path)
    validate_python_list(python_list)
    napistu <- python_list$python_modules$napistu
    checkmate::assertString(asset_name)
    
    file_ext <- tools::file_ext(file_path)
    
    # Validate supported file extension
    if (!file_ext %in% NAPISTU_CONSTANTS$SUPPORTED_EXTENSIONS) {
        cli::cli_abort(c(
            "Unsupported file type for {.field {asset_name}}: {.val {file_ext}}",
            "i" = "File: {.file {file_path}}",
            "i" = "Supported extensions: {.val {NAPISTU_CONSTANTS$SUPPORTED_EXTENSIONS}}"
        ))
    }
    
    obj <- tryCatch({
        switch(file_ext,
               "pkl" = reticulate::py_load_object(file_path),
               "tsv" = readr::read_tsv(file_path, show_col_types = FALSE),
               "json" = jsonlite::fromJSON(file_path),
               "parquet" = napistu$utils$load_parquet(file_path)
        )
    }, error = function(e) {
        cli::cli_abort(
            "Failed to load {.field {asset_name}} from {.file {file_path}}: {e$message}"
        )
    })
    
    return(obj)
} 


#' Get Bundled Asset Sources
#'
#' @return Named list of asset file paths for bundled assets
#' @keywords internal
get_bundled_asset_paths <- function() {
    
    bundled_test_pathway_path <- system.file("extdata/test_pathway", package = "napistu.r")
    asset_filenames <- NAPISTU_CONSTANTS$ASSET_FILENAMES
    
    # Create sources list for all assets (required + optional)
    all_assets <- c(NAPISTU_CONSTANTS$REQUIRED_ASSETS, NAPISTU_CONSTANTS$OPTIONAL_ASSETS)
    asset_paths <- list()
    
    for (asset_name in all_assets) {
        file_name <- asset_filenames[[asset_name]]
        asset_path <- file.path(bundled_test_pathway_path, file_name)
        
        # Only include if file actually exists
        if (file.exists(asset_path)) {
            asset_paths[[asset_name]] <- asset_path
        }
    }
    
    return(asset_paths)
}

#' Get Configured Asset Sources
#'
#' @inheritParams validate_assets_config
#' @inheritParams validate_verbose
#'
#' @return Named list of asset file paths for configured assets
#' @keywords internal
get_configured_asset_paths <- function(assets_config, verbose = TRUE) {
    
    validate_assets_config(assets_config, minimal_validation = TRUE)
    validate_verbose(verbose)
    
    # Handle directory-based configuration first
    if ("assets_dir" %in% names(assets_config)) {
        assets_config <- resolve_directory_assets(assets_config, verbose)
    }
    
    # Validate required assets are specified
    required_assets <- NAPISTU_CONSTANTS$REQUIRED_ASSETS
    missing_required <- setdiff(required_assets, names(assets_config))
    
    if (length(missing_required) > 0) {
        cli::cli_abort(c(
            "Required asset{?s} missing from configuration: {.field {missing_required}}",
            "i" = "The following asset{?s} {?is/are} required: {.field {required_assets}}"
        ))
    }
    
    # Validate all specified files exist
    sources <- list()
    all_specified <- names(assets_config)[names(assets_config) != "assets_dir"]
    
    for (asset_name in all_specified) {
        asset_path <- assets_config[[asset_name]]
        
        if (!file.exists(asset_path)) {
            cli::cli_abort(c(
                "Asset file not found: {.file {asset_path}}",
                "i" = "Asset: {.field {asset_name}}"
            ))
        }
        
        sources[[asset_name]] <- asset_path
    }
    
    sources
}

#' Load Assets from Paths
#'
#' @inheritParams validate_asset_paths
#' @inheritParams validate_python_list
#' @inheritParams validate_verbose
#' 
#' @return List of loaded assets
#' @keywords internal
load_assets_from_paths <- function(asset_paths, python_list, verbose = TRUE) {
    
    validate_asset_paths(asset_paths)
    validate_python_list(python_list)
    validate_verbose(verbose)
    
    assets <- list()
    
    # Load required assets
    for (asset_name in NAPISTU_CONSTANTS$REQUIRED_ASSETS) {
        asset_path <- asset_paths[[asset_name]]
        
        if (is.null(asset_path)) {
            cli::cli_abort("Required asset not found: {.field {asset_name}}")
        }
        
        if (verbose) {
            cli::cli_inform("Loading {.field {asset_name}} from {.file {basename(asset_path)}}")
        }
        assets[[asset_name]] <- load_single_asset(asset_path, python_list, asset_name)
    }
    
    # Load optional assets if available
    for (asset_name in NAPISTU_CONSTANTS$OPTIONAL_ASSETS) {
        asset_path <- asset_paths[[asset_name]]
        
        if (!is.null(asset_path)) {
            if (verbose) {
                cli::cli_inform("Loading optional {.field {asset_name}} from {.file {basename(asset_path)}}")
            }
            assets[[asset_name]] <- load_single_asset(asset_path, python_list, asset_name)
        } else {
            if (verbose) {
                cli::cli_inform("Optional asset {.field {asset_name}} not available")   
            }
        }
    }
    
    if (verbose) {
        cli::cli_alert_success("Assets loaded successfully")
    }
    
    return(assets)
}

#' Resolve Directory-Based Asset Configuration
#'
#' @inheritParams validate_assets_config
#' @inheritParams validate_verbose
#' 
#' @return Resolved assets configuration with individual file paths
#' @keywords internal
resolve_directory_assets <- function(assets_config, verbose = TRUE) {
    
    validate_assets_config(assets_config, minimal_validation = TRUE)
    validate_verbose(verbose)
    
    assets_dir <- assets_config$assets_dir
    
    if (!dir.exists(assets_dir)) {
        cli::cli_abort("Assets directory does not exist: {.path {assets_dir}}")
    }
    
    if (verbose) {
        cli::cli_inform("Resolving assets from directory: {.path {assets_dir}}")
    }
    
    asset_filenames <- NAPISTU_CONSTANTS$ASSET_FILENAMES
    resolved_config <- list()  # Start fresh
    
    # Process all possible assets (required + optional)
    all_assets <- c(NAPISTU_CONSTANTS$REQUIRED_ASSETS, NAPISTU_CONSTANTS$OPTIONAL_ASSETS)
    
    for (asset_name in all_assets) {
        if (asset_name %in% names(assets_config)) {
            # Case 2: File specified - treat as relative to directory
            relative_path <- assets_config[[asset_name]]
            resolved_config[[asset_name]] <- file.path(assets_dir, relative_path)
        } else {
            # Case 1: File not specified - use default filename
            default_filename <- asset_filenames[[asset_name]]
            file_path <- file.path(assets_dir, default_filename)
            if (file.exists(file_path)) {
                resolved_config[[asset_name]] <- file_path
            }
        }
    }
    
    resolved_config
}
