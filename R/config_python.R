#' Setup Python Environment
#'
#' @param napistu_config Napistu configuration object
#' @param verbose provide extra logging
#' 
#' @return List containing Python modules and environment info
setup_python_env <- function(napistu_config, verbose = TRUE) {
    
    checkmate::assert_class(config, "napistu_config")
    checkmate::assertLogical(verbose, len = 1)
    
    python_config <- config$python
    
    # Handle different Python environment configurations
    if (length(python_config) == 0) {
        if (verbose) {
            cli::cli_inform("No Python configuration specified, setting up conda environment")
        }
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
#' @inheritParams setup_napistu_list
#' 
#' @return List with environment path and metadata
configure_existing_python <- function(python_config, verbose = TRUE) {
    
    checkmate::assertList(python_config)
    checkmate::assertLogical(verbose, len = 1)
    
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
    
    if (verbose) {
        cli::cli_inform("Configuring Python environment: {.val {env_type}} = {.path {env_path}}")
    }
    
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
        validate_python_version(verbose)
        
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

#' Validate and Import Python Modules
#'
#' @param required_modules a character vector of modules to import
#' @inheritParams setup_napistu_list
#'
#' @return List of imported Python modules
validate_and_import_modules <- function(
    required_modules = NAPISTU_CONSTANTS$REQUIRED_PYTHON_MODULES,
    verbose = TRUE
    ) {
    
    checkmate::assertCharacter(required_modules)
    checkmate::assertLogical(verbose, len = 1)
    
    modules <- list()
    
    for (module_name in required_modules) {
        cli::cli_inform("Importing Python module: {.pkg {module_name}}")
        
        modules[[module_name]] <- tryCatch({
            reticulate::import(module_name)
        }, error = function(e) {
            cli::cli_abort(c(
                "Failed to import required Python module {.pkg {module_name}}: {e$message}",
                "i" = "The existing conda environment may be missing {.pkg {module_name}}",
                "i" = "Consider not passing a Python environment to let Napistu create one"
            ))
        })
    }
    
    cli::cli_alert_success("Python environment validated successfully")
    
    return(modules)
}

validate_python_version <- function(verbose) {
    
    checkmate::assertLogical(verbose, len = 1)
    
    min_version <- NAPISTU_CONSTANTS$MINIMUM_PYTHON_VERSION
    
    tryCatch({
        # Get version from reticulate
        current_py_version <- as.character(reticulate::py_version())
        
        if (utils::compareVersion(current_py_version, min_version) < 0) {
            cli::cli_abort(c(
                "Configured Python version {py_version} is below minimum required {min_version}",
                "i" = "Napistu requires Python {min_version} or later"
            ))
        }
        
        if (verbose) {
            cli::cli_inform("Python version {py_version} meets requirements")
        }
        
    }, error = function(e) {
        cli::cli_warn("Could not validate Python version: {e$message}")
    })
}