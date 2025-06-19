#' Load Assets
#'
#' @param config Napistu configuration object
#' @inheritParams setup_napistu_list
#' 
#' @return List of loaded assets
load_assets <- function(config, verbose = TRUE) {
    
    checkmate::assert_class(config, "napistu_config")
    checkmate::assert_logical(verbose, len = 1)
    
    assets_config <- config$assets
    
    if (length(assets_config) == 0) {
        if (verbose) {
            cli::cli_inform("No assets configuration specified, loading bundled package data")
        }
        asset_sources <- get_bundled_asset_sources()
    } else {
        if (verbose) {
            cli::cli_inform("Loading assets from configured paths")
        }
        asset_sources <- get_configured_asset_sources(assets_config, verbose)
    }
    
    load_assets_from_sources(asset_sources, verbose)
}

#' Get Bundled Asset Sources
#'
#' @return Named list of asset file paths for bundled assets
get_bundled_asset_sources <- function() {
    
    bundled_test_pathway_path <- system.file("extdata/test_pathway", package = "napistu.r")
    asset_filenames <- NAPISTU_CONSTANTS$ASSET_FILENAMES
    
    # Create sources list for all assets (required + optional)
    all_assets <- c(NAPISTU_CONSTANTS$REQUIRED_ASSETS, NAPISTU_CONSTANTS$OPTIONAL_ASSETS)
    sources <- list()
    
    for (asset_name in all_assets) {
        file_name <- asset_filenames[[asset_name]]
        asset_path <- file.path(bundled_test_pathway_path, file_name)
        
        # Only include if file actually exists
        if (file.exists(asset_path)) {
            sources[[asset_name]] <- asset_path
        }
    }
    
    sources
}

#' Get Configured Asset Sources
#'
#' @param assets_config Assets configuration from user
#' @inheritParams setup_napistu_list
#'
#' @return Named list of asset file paths for configured assets
get_configured_asset_sources <- function(assets_config, verbose = TRUE) {
    
    checkmate::assert_list(assets_config)
    checkmate::assert_logical(verbose, len = 1)
    
    # Handle directory-based configuration first
    if ("asset_dir" %in% names(assets_config)) {
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
    all_specified <- names(assets_config)[names(assets_config) != "asset_dir"]
    
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

#' Load Assets from Sources
#'
#' @param asset_sources Named list of asset file paths
#' @inheritParams setup_napistu_list
#' 
#' @return List of loaded assets
load_assets_from_sources <- function(asset_sources, verbose = TRUE) {
    
    checkmate::assertList(asset_sources)
    checkmate::assertLogical(verbose, len = 1)
    
    assets <- list()
    
    # Load required assets
    for (asset_name in NAPISTU_CONSTANTS$REQUIRED_ASSETS) {
        asset_path <- asset_sources[[asset_name]]
        
        if (is.null(asset_path)) {
            cli::cli_abort("Required asset not found: {.field {asset_name}}")
        }
        
        if (verbose) {
            cli::cli_inform("Loading {.field {asset_name}} from {.file {basename(asset_path)}}")
        }
        assets[[asset_name]] <- load_single_asset(asset_path, asset_name)
    }
    
    # Load optional assets if available
    for (asset_name in NAPISTU_CONSTANTS$OPTIONAL_ASSETS) {
        asset_path <- asset_sources[[asset_name]]
        
        if (!is.null(asset_path)) {
            if (verbose) {
                cli::cli_inform("Loading optional {.field {asset_name}} from {.file {basename(asset_path)}}")
            }
           assets[[asset_name]] <- load_single_asset(asset_path, asset_name)
        } else {
            if (verbose) {
                cli::cli_inform("Optional asset {.field {asset_name}} not available")   
            }
        }
    }
    
    if (verbose) {
        cli::cli_alert_success("Assets loaded successfully")
        }
    
    assets
}

#' Resolve Directory-Based Asset Configuration
#'
#' @param assets_config Assets configuration with asset_dir path
#' @inheritParams setup_napistu_list
#' 
#' @return Resolved assets configuration with individual file paths
resolve_directory_assets <- function(assets_config, verbose = TRUE) {
    
    checkmate::assertList(assets_config)
    checkmate::assertLogical(verbose, len = 1)
    
    assets_dir <- assets_config$asset_dir
    
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

#' Load Single Asset File
#'
#' @param file_path Path to asset file
#' @param asset_name Name of asset for context
#' @return Loaded asset object
load_single_asset <- function(file_path, asset_name) {
    file_ext <- tools::file_ext(file_path)
    
    # Validate supported file extension
    if (!file_ext %in% NAPISTU_CONSTANTS$SUPPORTED_EXTENSIONS) {
        cli::cli_abort(c(
            "Unsupported file type for {.field {asset_name}}: {.val {file_ext}}",
            "i" = "File: {.file {file_path}}",
            "i" = "Supported extensions: {.val {NAPISTU_CONSTANTS$SUPPORTED_EXTENSIONS}}"
        ))
    }
    
    tryCatch({
        switch(file_ext,
               "pkl" = reticulate::py_load_object(file_path),
               "tsv" = readr::read_tsv(file_path, show_col_types = FALSE),
               "json" = jsonlite::fromJSON(file_path)
        )
    }, error = function(e) {
        cli::cli_abort(
            "Failed to load {.field {asset_name}} from {.file {file_path}}: {e$message}"
        )
    })
}