#' Complete Napistu Setup
#'
#' @param config Napistu configuration object or path to config file
#' 
#' @return Napistu environment object with all components
#' 
#' @export
#'
#' @examples
#' \dontrun{
#' # Using configuration object
#' config <- napistu_config()
#' napistu_env <- setup_napistu(config)
#' 
#' # Using config file
#' napistu_env <- setup_napistu("my_config.yml")
#' }
setup_napistu <- function(config) {
    if (is.character(config)) {
        config <- load_napistu_config(config)
    }
    
    checkmate::assert_class(config, "napistu_config")
    
    cli::cli_h1("Setting up Napistu environment")
    
    # Setup Python environment
    python_env <- setup_python_env(config)
    
    # Load assets
    assets <- load_assets(config)
    
    # Create combined environment object
    env <- structure(
        c(
            assets,
            list(
                python_modules = python_env$modules,
                python_environment = python_env$environment,
                config = config,
                loaded_at = Sys.time()
            )
        ),
        class = "napistu_env"
    )
    
    cli::cli_alert_success("Napistu environment setup complete")
    
    return(env)
}

#' Create Napistu Configuration
#'
#' @param python List of Python environment configuration options
#' @param assets List of asset file paths or configuration
#' 
#' @return Configuration object
#' 
#' @export
#'
#' @examples
#' \dontrun{
#' # Direct configuration
#' config <- napistu_config(
#'   python = list(virtualenv = "/path/to/venv"),
#'   assets = list(
#'     sbml_dfs = "/path/to/sbml.pkl",
#'     napistu_graph = "/path/to/graph.pkl",
#'     species_identifiers = "/path/to/species.tsv"
#'   )
#' )
#' 
#' # Minimal configuration (uses defaults)
#' config <- napistu_config()
#' }
napistu_config <- function(python = list(), assets = list()) {
    checkmate::assert_list(python, names = "named")
    checkmate::assert_list(assets, names = "named")
    
    config <- list(
        python = python,
        assets = assets,
        created_at = Sys.time()
    )
    
    class(config) <- c("napistu_config", "list")
    config
}

#' Load Napistu Configuration from YAML
#'
#' @param path Path to YAML configuration file
#' @return Configuration object
#' @export
load_napistu_config <- function(file_path) {
    checkmate::assert_file_exists(file_path)
    
    if (!requireNamespace("yaml", quietly = TRUE)) {
        cli::cli_abort("Package {.pkg yaml} is required for loading config files")
    }
    
    cli::cli_inform("Loading configuration from {.file {path}}")
    
    config_data <- tryCatch(
        yaml::read_yaml(file_path),
        error = function(e) {
            cli::cli_abort("Failed to parse YAML config: {e$message}")
        }
    )
    
    # Apply defaults for missing sections
    python_config <- config_data$python %||% list()
    assets_config <- config_data$assets %||% list()
    
    napistu_config(python = python_config, assets = assets_config)
}

#' Setup Python Environment
#'
#' @param config Napistu configuration object
#' @return List containing Python modules and environment info
setup_python_env <- function(config) {
    checkmate::assert_class(config, "napistu_config")
    
    python_config <- config$python
    
    # Handle different Python environment configurations
    if (length(python_config) == 0) {
        cli::cli_inform("No Python configuration specified, setting up conda environment")
        env_info <- create_default_conda_env()
    } else {
        env_info <- configure_existing_python(python_config)
    }
    
    # Validate and import required modules
    modules <- validate_and_import_modules(env_info$path)
    
    # Return both modules and environment metadata
    list(
        modules = modules,
        environment = env_info
    )
}

#' Configure Existing Python Environment
#'
#' @param python_config Python configuration list
#' @return List with environment path and metadata
configure_existing_python <- function(python_config) {
    # Validate only one environment type is specified
    env_types <- NAPISTU_CONSTANTS$PYTHON_ENV_TYPES
    specified_types <- intersect(names(python_config), env_types)
    
    if (length(specified_types) == 0) {
        cli::cli_abort("No valid Python environment specified. Use one of: {.val {env_types}}")
    }
    
    if (length(specified_types) > 1) {
        cli::cli_abort("Multiple Python environment types specified: {.val {specified_types}}")
    }
    
    env_type <- specified_types[1]
    env_path <- python_config[[env_type]]
    
    checkmate::assert_string(env_path, min.chars = 1)
    
    cli::cli_inform("Configuring Python environment: {.val {env_type}} = {.path {env_path}}")
    
    tryCatch({
        switch(env_type,
               "virtualenv" = {
                   checkmate::assert_directory_exists(env_path)
                   reticulate::use_virtualenv(env_path, required = TRUE)
               },
               "conda" = {
                   reticulate::use_condaenv(env_path, required = TRUE)
               },
               "python" = {
                   checkmate::assert_file_exists(env_path)
                   reticulate::use_python(env_path, required = TRUE)
               }
        )
        
        # Validate Python version after configuration
        validate_python_version()
        
    }, error = function(e) {
        cli::cli_abort("Failed to configure {env_type} environment: {e$message}")
    })
    
    list(
        path = env_path,
        type = env_type,
        created_by_napistu = FALSE,
        miniconda_installed = FALSE
    )
}

#' Load Assets
#'
#' @param config Napistu configuration object
#' @return List of loaded assets
load_assets <- function(config) {
    checkmate::assert_class(config, "napistu_config")
    
    assets_config <- config$assets
    
    if (length(assets_config) == 0) {
        cli::cli_inform("No assets configuration specified, loading bundled package data")
        load_bundled_assets()
    } else {
        load_configured_assets(assets_config)
    }
}

#' Load Bundled Package Assets
#'
#' @return List of bundled assets
load_bundled_assets <- function() {
    cli::cli_inform("Loading bundled assets from package")
    
    asset_filenames <- NAPISTU_CONSTANTS$ASSET_FILENAMES
    assets <- list()
    
    # Load required assets
    bundled_test_pathway_path <- system.file("extdata/test_pathway", package = "napistu.r")
    
    for (asset_name in NAPISTU_CONSTANTS$REQUIRED_ASSETS) {
        file_name <- asset_filenames[[asset_name]]
        asset_path <- file.path(bundled_test_pathway_path, file_name)
        
        if (!file.exists(asset_path)) {
            cli::cli_abort("Required bundled asset not found: {.file {file_name}}")
        }
        
        cli::cli_inform("Loading {.field {asset_name}} from {.file {file_name}}")
        assets[[asset_name]] <- load_single_asset(asset_path, asset_name)
    }
    
    # Load optional assets if available
    for (asset_name in NAPISTU_CONSTANTS$OPTIONAL_ASSETS) {
        file_name <- asset_filenames[[asset_name]]
        asset_path <- file.path(bundled_test_pathway_path, file_name)
        
        if (file.exists(asset_path)) {
            cli::cli_inform("Loading optional {.field {asset_name}} from {.file {file_name}}")
            assets[[asset_name]] <- load_single_asset(asset_path, asset_name)
        } else {
            cli::cli_inform("Optional asset {.field {asset_name}} not available in package")
        }
    }
    
    cli::cli_alert_success("Bundled assets loaded successfully")
    
    return(assets)
}

#' Load Configured Assets
#'
#' @param assets_config Assets configuration list
#' @return List of loaded assets
load_configured_assets <- function(assets_config) {
    cli::cli_inform("Loading assets from configured paths")
    
    # Check for directory-based configuration
    if ("path" %in% names(assets_config)) {
        assets_config <- resolve_directory_assets(assets_config)
    }
    
    # Validate required assets are specified
    required_assets <- NAPISTU_CONSTANTS$REQUIRED_ASSETS
    missing_required <- setdiff(required_assets, names(assets_config))
    
    if (length(missing_required) > 0) {
        cli::cli_abort(
            "Required assets missing from configuration: {.field {missing_required}}",
            "i" = "Required assets: {.field {required_assets}}"
        )
    }
    
    # Load required assets
    assets <- list()
    
    for (asset_name in required_assets) {
        asset_path <- assets_config[[asset_name]]
        checkmate::assert_file_exists(asset_path, .var.name = glue::glue("assets${asset_name}"))
        
        cli::cli_inform("Loading {.field {asset_name}} from {.file {asset_path}}")
        assets[[asset_name]] <- load_single_asset(asset_path, asset_name)
    }
    
    # Load optional assets if specified
    for (asset_name in NAPISTU_CONSTANTS$OPTIONAL_ASSETS) {
        if (asset_name %in% names(assets_config)) {
            asset_path <- assets_config[[asset_name]]
            checkmate::assert_file_exists(asset_path, .var.name = glue::glue("assets${asset_name}"))
            
            cli::cli_inform("Loading optional {.field {asset_name}} from {.file {asset_path}}")
            assets[[asset_name]] <- load_single_asset(asset_path, asset_name)
        }
    }
    
    cli::cli_alert_success("Configured assets loaded successfully")
    assets
}

#' Resolve Directory-Based Asset Configuration
#'
#' @param assets_config Assets configuration with directory path
#' @return Resolved assets configuration with individual file paths
resolve_directory_assets <- function(assets_config) {
    assets_dir <- assets_config$path
    checkmate::assert_directory_exists(assets_dir)
    
    cli::cli_inform("Resolving assets from directory: {.path {assets_dir}}")
    
    asset_filenames <- NAPISTU_CONSTANTS$ASSET_FILENAMES
    resolved_config <- assets_config
    resolved_config$path <- NULL  # Remove directory specification
    
    # Only add files that aren't already explicitly specified
    for (asset_name in names(asset_filenames)) {
        if (!asset_name %in% names(assets_config)) {
            file_path <- file.path(assets_dir, asset_filenames[[asset_name]])
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
#' @param asset_type Type of asset for appropriate loading method
#' @return Loaded asset object
load_single_asset <- function(file_path, asset_type) {
    file_ext <- tools::file_ext(file_path)
    
    # Validate supported file extension
    if (!file_ext %in% NAPISTU_CONSTANTS$SUPPORTED_EXTENSIONS) {
        cli::cli_abort(
            "Unsupported file type for {.file {file_path}}: {.val {file_ext}}",
            "i" = "Supported extensions: {.val {NAPISTU_CONSTANTS$SUPPORTED_EXTENSIONS}}"
        )
    }
    
    switch(file_ext,
           "pkl" = {
               tryCatch({
                   reticulate::py_load_object(file_path)
               }, error = function(e) {
                   cli::cli_abort("Failed to load pickle file {.file {file_path}}: {e$message}")
               })
           },
           "tsv" = {
               tryCatch({
                   readr::read_tsv(file_path, show_col_types = FALSE)
               }, error = function(e) {
                   cli::cli_abort("Failed to load TSV file {.file {file_path}}: {e$message}")
               })
           },
           "json" = {
               tryCatch({
                   jsonlite::fromJSON(file_path)
               }, error = function(e) {
                   cli::cli_abort("Failed to load JSON file {.file {file_path}}: {e$message}")
               })
           }
    )
}

# S3 methods for configuration and environment objects

#' @export
print.napistu_config <- function(x, ...) {
    cli::cli_h2("Napistu Configuration")
    
    if (length(x$python) > 0) {
        cli::cli_text("Python: {.field {names(x$python)}} = {.path {x$python}}")
    } else {
        cli::cli_text("Python: {.emph default (conda env '{NAPISTU_CONSTANTS$DEFAULT_CONDA_ENV_NAME}')}")
    }
    
    if (length(x$assets) > 0) {
        cli::cli_text("Assets:")
        purrr::iwalk(x$assets, ~ cli::cli_text("  {.field {.y}}: {.path {.x}}"))
    } else {
        cli::cli_text("Assets: {.emph bundled package data}")
    }
    
    invisible(x)
}

#' @export
print.napistu_env <- function(x, ...) {
    cli::cli_h2("Napistu Environment")
    
    asset_names <- setdiff(names(x), c("python_modules", "python_environment", "config", "loaded_at"))
    cli::cli_text("Assets: {.field {asset_names}}")
    cli::cli_text("Python modules: {.field {names(x$python_modules)}}")
    cli::cli_text("Python environment: {.val {x$python_environment$path}} ({.val {x$python_environment$type}})")
    
    if (x$python_environment$created_by_napistu) {
        cleanup_items <- character()
        if (x$python_environment$type == "conda") {
            cleanup_items <- c(cleanup_items, "conda environment")
        }
        if (x$python_environment$miniconda_installed) {
            cleanup_items <- c(cleanup_items, "miniconda")
        }
        
        if (length(cleanup_items) > 0) {
            items_text <- paste(cleanup_items, collapse = " and ")
            cli::cli_text("{.emph Created {items_text} - can be cleaned up}")
        }
    }
    
    cli::cli_text("Loaded: {.timestamp {x$loaded_at}}")
    
    invisible(x)
}
