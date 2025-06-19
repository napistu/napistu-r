#' Validate and Import Python Modules
#'
#' @param required_modules a character vector of modules to import
#'
#' @return List of imported Python modules
validate_and_import_modules <- function(required_modules = NAPISTU_CONSTANTS$REQUIRED_PYTHON_MODULES) {
    
    modules <- list()
    
    for (module_name in required_modules) {
        cli::cli_inform("Importing Python module: {.pkg {module_name}}")
        
        modules[[module_name]] <- tryCatch({
            reticulate::import(module_name)
        }, error = function(e) {
            cli::cli_abort(
                "Failed to import required Python module {.pkg {module_name}}: {e$message}",
                i = "The existing conda environment may be missing {.pkg {module_name}}",
                i = "Consider not passing a Python environment to let Napistu create one"
            )
        })
    }
    
    cli::cli_alert_success("Python environment validated successfully")
    
    return(modules)
}

#' Validate Current Python Version
validate_python_version <- function() {
    
    min_version <- NAPISTU_CONSTANTS$MINIMUM_PYTHON_VERSION
    
    tryCatch({
        # Get version from reticulate
        current_py_version <- as.character(reticulate::py_version())
        
        if (utils::compareVersion(current_py_version, min_version) < 0) {
            cli::cli_abort(
                "Configured Python version {py_version} is below minimum required {min_version}",
                i = "Napistu requires Python {min_version} or later"
            )
        }
        
        cli::cli_inform("Python version {py_version} meets requirements")
        
    }, error = function(e) {
        cli::cli_warn("Could not validate Python version: {e$message}")
    })
}