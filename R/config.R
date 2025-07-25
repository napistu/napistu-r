#' Setup Napistu List
#'
#' Load a config specifying a Python environment and paths to Napistu assets
#'   as a list providing defaults as fall-backs.
#'
#' @inheritParams validate_napistu_config
#' @param napistu_list_object The name that setup_napistu_env() will be assigned to.
#' If this object already exists then setup will be skipped unless `overwrite` is TRUE
#' @inheritParams validate_overwrite
#' @inheritParams validate_verbose
#' @param skip_validation if FALSE, use `validate_asset_list_thorough` to 
#' comprehensively validate assets. If TRUE, asset validation is skipped. This
#' can be helpful when working with assets which have already been validated
#' because asset validation can be slow.
#' 
#' @return None; output is assigned to `napistu_list_object`
#' 
#' @examples
#' \dontrun{
#' napistu_config <- create_napistu_config()
#' setup_napistu_list(napistu_config)
#' 
#' # Using config file
#' setup_napistu_list("my_config.yml")
#' }
#' @export
setup_napistu_list <- function(
    napistu_config,
    napistu_list_object = "napistu_list",
    overwrite = FALSE,
    verbose = TRUE,
    skip_validation = FALSE
) {
    
    checkmate::assert_string(napistu_list_object)
    checkmate::assert_logical(overwrite, len = 1)
    checkmate::assert_logical(verbose, len = 1)
    checkmate::assert_logical(skip_validation, len = 1)
    
    if (napistu_list_object %in% ls(envir = parent.frame()) && !overwrite) {
        # the environment is already configured; nothing to do
        return (invisible(NULL))
    } 
    
    if (is.character(napistu_config)) {
        napistu_config <- load_napistu_config(napistu_config)
    }
    
    checkmate::assert_class(napistu_config, NAPISTU_CONSTANTS$NAPISTU_CONFIG_CLASS)
    
    if (verbose) {
        cli::cli_h1("Setting up Napistu environment")
    }
    
    # Setup Python environment
    python_list <- setup_python_env(napistu_config, verbose)
    
    # Load assets
    asset_list <- load_assets(napistu_config, python_list, verbose)
    
    # Create combined environment object
    napistu_list <- structure(
        c(
            asset_list,
            python_list,
            list(
                napistu_config = napistu_config,
                loaded_at = Sys.time()
            )
        ),
        class = NAPISTU_CONSTANTS$NAPISTU_LIST_CLASS
    )
    
    if (!skip_validation) {
        if (verbose) {
            cli::cli_alert_info("Comprehensively validating assets")
        }
        validate_asset_list_thorough(napistu_list)
    } else {
        if (verbose) {
            cli::cli_alert_info("Skipping asset validation")
        }
    }
    
    if (verbose) {
        cli::cli_alert_success("Napistu environment setup complete - creating {.field {napistu_list_object}} object in the parent environment")
    }
        
    assign(napistu_list_object, napistu_list, envir = parent.frame())
    
    return(invisible(NULL))
}

#' Create Napistu Configuration
#'
#' @inheritParams validate_python_config
#' @inheritParams validate_assets_config
#' 
#' @return Configuration object
#' 
#' @examples
#' \dontrun{
#' # Direct configuration
#' napistu_config <- create_napistu_config(
#'   python_config = list(virtualenv = "/path/to/venv"),
#'   assets_config = list(
#'     sbml_dfs = "/path/to/sbml.pkl",
#'     napistu_graph = "/path/to/graph.pkl",
#'     species_identifiers = "/path/to/species.tsv"
#'   )
#' )
#' 
#' # Minimal configuration (uses defaults)
#' napistu_config <- create_napistu_config()
#' }
#' @export
create_napistu_config <- function(python_config = list(), assets_config = list()) {
    
    validate_python_config(python_config)
    validate_assets_config(assets_config)
    
    napistu_config <- list(
        python_config = python_config,
        assets_config = assets_config,
        created_at = Sys.time()
    )
    
    class(napistu_config) <- c(NAPISTU_CONSTANTS$NAPISTU_CONFIG_CLASS, "list")
    
    return(napistu_config)
}

#' Load Napistu Configuration from YAML
#' 
#' Reads a config containing a `python_config` and/or `assets_config`.
#'
#' @param file_path Path to YAML configuration file
#' @inheritParams validate_verbose
#' 
#' @return Configuration object
#' 
#' @export
load_napistu_config <- function(file_path, verbose = TRUE) {
    
    checkmate::assert_file_exists(file_path)
    validate_verbose(verbose)
    
    if (!requireNamespace("yaml", quietly = TRUE)) {
        cli::cli_abort("Package {.pkg yaml} is required for loading config files")
    }
    
    if (verbose) {
        cli::cli_inform("Loading configuration from {.file {file_path}}")
    }
    
    config_data <- tryCatch(
        yaml::read_yaml(file_path),
        error = function(e) {
            cli::cli_abort("Failed to parse YAML config: {e$message}")
        }
    )
    
    unrecognized_yaml_specs <- setdiff(names(config_data), NAPISTU_CONSTANTS$NAPISTU_CONFIGS)
    if (length(unrecognized_yaml_specs) > 0) {
        cli::cli_abort("{unrecognized_yaml_specs} {?is/are} not a recognized component of the {.arg napistu_config}")
    }
    
    # Apply defaults for missing sections
    python_config <- config_data$python_config %||% list()
    assets_config <- config_data$assets_config %||% list()
    
    napistu_config <- create_napistu_config(
        python_config = python_config,
        assets_config = assets_config
    )
    
    return(napistu_config)
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
print.napistu_list <- function(x, ...) {
    cli::cli_h2("Napistu Environment")
    
    asset_names <- setdiff(names(x), c("python_modules", "python_environment", "napistu_config", "loaded_at"))
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
