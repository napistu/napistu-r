#' Create Default Conda Environment
#'
#' @param env_name The name of the to-be-created conda environment
#'
#' @return List with environment info and metadata
create_default_conda_env <- function(
    env_name = NAPISTU_CONSTANTS$DEFAULT_CONDA_ENV_NAME
    ) {
    
    min_python <- NAPISTU_CONSTANTS$MINIMUM_PYTHON_VERSION
    miniconda_installed <- FALSE
    
    # Get conda installation (install miniconda if needed)
    conda_exe <- tryCatch({
        get_conda_installation()
    }, error = function(e) {
        # If conda not found, try installing miniconda
        cli::cli_inform("Conda not available, installing miniconda...")
        
        tryCatch({
            reticulate::install_miniconda(force = FALSE)
            miniconda_installed <- TRUE
            cli::cli_alert_success("Miniconda installed successfully")
            
            # Now try to get conda again
            get_conda_installation()
            
        }, error = function(install_error) {
            cli::cli_abort(
                "Failed to set up Python environment for napistu",
                x = "Miniconda installation failed: {install_error$message}",
                i = "Please manually configure Python {min_python}+ using:",
                i = "  config <- napistu_config(python = list(virtualenv = '/path/to/python{min_python}/venv'))"
            )
        })
    })
    
    # Check if environment already exists
    if (conda_env_exists(env_name, conda_exe)) {
        cli::cli_inform("Using existing conda environment {.val {env_name}}")
        
        reticulate::use_condaenv(env_name, conda = conda_exe, required = TRUE)
        
        # Validate Python version and packages
        validate_python_version()
        validate_and_import_modules_current()
        
        return(list(
            path = env_name,
            type = "conda",
            created_by_napistu = FALSE,  # We didn't create it
            miniconda_installed = miniconda_installed
        ))
    }
    
    # Create new environment
    create_conda_environment(env_name, min_python, conda_exe)
    reticulate::use_condaenv(env_name, conda = conda_exe, required = TRUE)
    
    list(
        path = env_name,
        type = "conda",
        created_by_napistu = TRUE,
        miniconda_installed = miniconda_installed
    )
}

#' Get Conda Installation
#'
#' @return Path to conda executable
get_conda_installation <- function() {
    # First try conda in PATH
    conda_path <- Sys.which("conda")
    if (conda_path != "") {
        if (validate_conda_executable(conda_path)) {
            return(conda_path)
        }
    }
    
    # Try miniconda default location
    miniconda_path <- tryCatch({
        reticulate::miniconda_path()
    }, error = function(e) {
        NULL
    })
    
    if (!is.null(miniconda_path) && dir.exists(miniconda_path)) {
        conda_exe <- if (.Platform$OS.type == "windows") {
            file.path(miniconda_path, "Scripts", "conda.exe")
        } else {
            file.path(miniconda_path, "bin", "conda")
        }
        
        if (!file.exists(conda_exe)) {
            cli::cli_abort(
                "Miniconda installation found but conda executable is missing",
                i = "Miniconda path: {.path {miniconda_path}}",
                i = "Expected conda at: {.path {conda_exe}}",
                i = "Please reinstall miniconda or manually configure Python"
            )
        }
        
        if (validate_conda_executable(conda_exe)) {
            return(conda_exe)
        }
    }
    
    # No conda found anywhere
    cli::cli_abort(
        "Conda is not available",
        i = "Please install conda/miniconda or manually configure Python using:",
        i = "  config <- napistu_config(python = list(virtualenv = '/path/to/venv'))"
    )
}

#' Validate Conda Executable
#'
#' @param conda_exe Path to conda executable
#' @return Logical indicating if conda works
validate_conda_executable <- function(conda_exe) {
    tryCatch({
        result <- system2(conda_exe, "--version", stdout = TRUE, stderr = TRUE)
        if (length(result) > 0 && !is.null(result)) {
            return(TRUE)
        }
        
        cli::cli_abort(
            "Conda executable found but not working",
            i = "Conda path: {.path {conda_exe}}",
            i = "Please check your conda installation"
        )
    }, error = function(e) {
        cli::cli_abort(
            "Conda executable failed to run",
            i = "Conda path: {.path {conda_exe}}",
            i = "Error: {e$message}"
        )
    })
}

#' Check if Conda Environment Exists
#'
#' @param env_name Name of conda environment
#' @param conda_exe Path to conda executable
#' @return Logical indicating if environment exists
conda_env_exists <- function(env_name, conda_exe) {
    tryCatch({
        envs <- reticulate::conda_list(conda = conda_exe)
        return(env_name %in% envs$name)
    }, error = function(e) {
        cli::cli_warn("Could not list conda environments: {e$message}")
        return(FALSE)
    })
}

#' Create Conda Environment
#'
#' @param env_name Name of environment to create
#' @param python_version Python version to install
#' @param conda_exe Path to conda executable
#' @return NULL (side effect: creates environment)
create_conda_environment <- function(env_name, python_version, conda_exe) {
    cli::cli_inform("Creating conda environment {.val {env_name}} with Python {python_version}")
    
    tryCatch({
        reticulate::conda_create(
            envname = env_name,
            packages = c(glue::glue("python={python_version}"), "pip"),
            conda = conda_exe
        )
        
        cli::cli_inform("Installing {.pkg {NAPISTU_CONSTANTS$REQUIRED_PYTHON_MODULES}} in conda environment")
        reticulate::conda_install(
            envname = env_name,
            packages = NAPISTU_CONSTANTS$REQUIRED_PYTHON_MODULES,
            pip = TRUE,
            conda = conda_exe
        )
        
        cli::cli_alert_success("Conda environment {.val {env_name}} created successfully")
        
    }, error = function(e) {
        cli::cli_abort("Failed to create conda environment: {e$message}")
    })
}

#' Clean Up Napistu Environment
#'
#' Removes conda environment and miniconda if they were created by napistu
#'
#' @param env Napistu environment object
#' @param force Logical, whether to force removal without confirmation
#' @export
cleanup_napistu_env <- function(napistu_env, force = FALSE) {
    checkmate::assert_class(napistu_env, "napistu_env")
    
    if (!napistu_env$python_environment$created_by_napistu) {
        cli::cli_inform("Python environment was not created by napistu - no cleanup needed")
        return(invisible(NULL))
    }
    
    napistu_env_info <- napistu_env$python_environment
    
    if (!force && interactive()) {
        cleanup_items <- character()
        if (env_info$type == "conda") {
            cleanup_items <- c(cleanup_items, glue::glue("conda environment '{env_info$path}'"))
        }
        if (env_info$miniconda_installed) {
            cleanup_items <- c(cleanup_items, "miniconda installation")
        }
        
        if (length(cleanup_items) > 0) {
            items_text <- paste(cleanup_items, collapse = " and ")
            response <- readline(glue::glue("Remove {items_text}? (y/N): "))
            if (!tolower(response) %in% c("y", "yes")) {
                cli::cli_inform("Cleanup cancelled")
                return(invisible(NULL))
            }
        }
    }
    
    # Remove miniconda if we installed it
    if (napistu_env_info$miniconda_installed) {
        cli::cli_inform("Removing miniconda installation")
        tryCatch({
            reticulate::miniconda_uninstall()
            cli::cli_alert_success("Miniconda removed")
        }, error = function(e) {
            cli::cli_warn("Failed to remove miniconda: {e$message}")
        })
        
        return (invisible(NULL))
    }
    
    # Remove conda environment - skip if we removed conda already
    if (napistu_env_info$type == "conda") {
        cli::cli_inform("Removing conda environment: {.val {napistu_env_info$path}}")
        tryCatch({
            reticulate::conda_remove(envname = napistu_env_info$path)
            cli::cli_alert_success("Conda environment removed")
        }, error = function(e) {
            cli::cli_warn("Failed to remove conda environment: {e$message}")
        })
    }
    
    return (invisible(NULL))
}