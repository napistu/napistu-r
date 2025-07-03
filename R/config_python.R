#' Setup Python Environment
#'
#' @param napistu_config Napistu configuration object
#' @param verbose provide extra logging
#' 
#' @return A `python_list` containing Python modules and environment info
#' @keywords internal
setup_python_env <- function(napistu_config, verbose = TRUE) {
    
    checkmate::assert_class(napistu_config, "napistu_config")
    checkmate::assertLogical(verbose, len = 1)
    
    python_config <- napistu_config$python
    
    # Handle different Python environment configurations
    if (length(python_config) == 0) {
        if (verbose) {
            cli::cli_inform("No Python configuration specified, setting up conda environment")
        }
        python_environment <- create_default_conda_env()
    } else {
        python_environment <- configure_existing_python(python_config)
    }
    
    # Validate and import required modules
    python_modules <- validate_and_import_modules(verbose = verbose)
    
    # Return both modules and environment metadata
    list(
        python_modules = python_modules,
        python_environment = python_environment
    )
}

#' Configure Existing Python Environment
#'
#' @param python_config Python configuration list
#' @inheritParams setup_napistu_list
#' 
#' @return List with environment path and metadata
#' @keywords internal
configure_existing_python <- function(python_config, verbose = TRUE) {
    
    validate_python_config(python_config)
    checkmate::assertLogical(verbose, len = 1)
    
    env_types <- NAPISTU_CONSTANTS$PYTHON_ENV_TYPES
    specified_types <- intersect(names(python_config), env_types)
    
    env_type <- specified_types[1]
    env_path <- ensure_absolute_path(python_config[[env_type]])
    
    checkmate::assert_string(env_path, min.chars = 1)
    
    if (verbose) {
        cli::cli_inform("Configuring Python environment: {.val {env_type}} = {.path {env_path}}")
    }
    
    if (env_type == "conda") {
        conda_env_name <- python_config$conda_env_name
    }
    
    tryCatch({
        switch(env_type,
               "virtualenv" = {
                   checkmate::assert_directory_exists(env_path)
                   reticulate::use_virtualenv(env_path, required = TRUE)
               },
               "conda" = {
                   reticulate::use_condaenv(conda_env_name, env_path, required = TRUE)
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
#' @inheritParams setup_napistu_list
#'
#' @return List of imported Python modules
#' @keywords internal
validate_and_import_modules <- function(verbose = TRUE) {
    
    required_modules <- NAPISTU_CONSTANTS$REQUIRED_PYTHON_MODULES
    installed_modules <- reticulate::py_list_packages()
    
    # Check for presence of all required modules
    missing_modules <- setdiff(names(required_modules), stringr::str_trim(installed_modules$package))
    if (length(missing_modules) > 0) {
        cli::cli_abort(
            "{length(missing_modules)} required Python module{?s} {?is/are} not installed: {.field {missing_modules}}
            in your Python environment: {reticulate::py_exe()}"
        )
    }
    
    # Check for minimum versions
    for (module_name in names(required_modules)) {
        min_version <- required_modules[[module_name]]
        if (!is.na(min_version)) {
            installed_version <- installed_modules[
                installed_modules$package == module_name, "version"
            ]
            if (length(installed_version) == 0) {
                cli::cli_warn(
                    "Python module {module_name} requires at least version {min_version} but its version could not be determined"
                )
                next
            }
            
            if (utils::compareVersion(installed_version, min_version) < 0) {
                cli::cli_abort(
                    "Python module {module_name} requires version {min_version}
                    or higher, but version {installed_version} is installed."
                )
            }
        }
    }
    
    # Import modules if all checks pass
    imported_modules <- list()
    for (module_name in names(required_modules)) {
        imported_modules[[module_name]] <- reticulate::import(
            module_name,
            delay_load = TRUE
        )
    }
    
    return(imported_modules)
}

validate_python_version <- function(verbose) {
    checkmate::assertLogical(verbose, len = 1)
    
    min_version <- NAPISTU_CONSTANTS$MINIMUM_PYTHON_VERSION
    
    tryCatch({
        # Get version from reticulate
        current_py_version <- as.character(reticulate::py_version())
        if (length(current_py_version) == 0) {
            cli::cli_alert_info("Python version not available for validation")
            return (NULL)
        }
        
        if (utils::compareVersion(current_py_version, min_version) < 0) {
            cli::cli_abort(c(
                "Configured Python version {py_version} is below minimum required {min_version}",
                "i" = "Napistu requires Python {min_version} or later"
            ))
        }
        
        if (verbose) {
            cli::cli_inform("Python version {current_py_version} meets requirements")
        }
        
    }, error = function(e) {
        cli::cli_warn("Could not validate Python version: {e$message}")
    })
    
    return(TRUE)
}
