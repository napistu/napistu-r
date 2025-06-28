#' Clean Up Napistu Environment
#'
#' Removes conda environment and miniconda if they were created by napistu
#'
#' @param napistu_list Napistu environment object
#' @param force Logical, whether to force removal without confirmation
#' 
#' @export
cleanup_napistu <- function(napistu_list, force = FALSE) {
    
    checkmate::assert_class(napistu_list, NAPISTU_CONSTANTS$NAPISTU_LIST_CLASS)
    checkmate::assert_logical(force, len = 1)
    
    python_env <- napistu_list$python_environment
    
    if (!python_env$created_by_napistu) {
        cli::cli_inform("Python environment was not created by napistu - no cleanup needed")
        return(invisible(NULL))
    }
    
    if (!force && interactive()) {
        cleanup_items <- character()
        if (python_env$type == "conda") {
            cleanup_items <- c(cleanup_items, glue::glue("conda environment '{python_env$path}'"))
        }
        if (python_env$miniconda_installed) {
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
    if (python_env$miniconda_installed) {
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
    if (python_env$type == "conda") {
        cli::cli_inform("Removing conda environment: {.val {python_env$path}}")
        tryCatch({
            reticulate::conda_remove(envname = python_env$path)
            cli::cli_alert_success("Conda environment removed")
        }, error = function(e) {
            cli::cli_warn("Failed to remove conda environment: {e$message}")
        })
    }
    
    return (invisible(NULL))
}


#' Create Default Conda Environment
#'
#' Setup an conda environment with Napistu installed. This will install miniconda
#' if needed (with user permission) and create an environment if-needed in
#' an existing conda/miniconda installation or the on-the-fly installation. This
#' process can be undone using \link{cleanup_napistu}.
#'
#' @param env_name The name of the to-be-created conda environment
#' @inheritParams setup_napistu_list
#'
#' @return List with environment info and metadata
#' @keywords internal
create_default_conda_env <- function(
    env_name = NAPISTU_CONSTANTS$DEFAULT_CONDA_ENV_NAME,
    verbose = TRUE
) {
    
    checkmate::assertString(env_name)
    checkmate::assertLogical(verbose, len = 1)
    
    min_python <- NAPISTU_CONSTANTS$MINIMUM_PYTHON_VERSION
    miniconda_installed <- FALSE
    
    # Get conda installation (install miniconda if needed)
    conda_exe <- tryCatch({
        get_conda_installation()
    }, conda_not_available_error = function(e) {
        # If conda not found, try installing miniconda
        cli::cli_alert_danger("Conda not available, installing miniconda...")
        
        # ask for user confirmation in an interactive session
        confirm_install_miniconda()
        
        tryCatch({
            reticulate::install_miniconda(force = FALSE)
            miniconda_installed <- TRUE
            cli::cli_alert_success("Miniconda installed successfully. You can remove it with {.field cleanup_napistu(napistu_list)} once you're done")
            
            # Now try to get conda again
            get_conda_installation()
            
        }, error = function(install_error) {
            cli::cli_abort(c(
                "Failed to set up Python environment for napistu",
                "x" = "Miniconda installation failed: {install_error$message}",
                "i" = "Please manually configure Python {min_python}+ using:",
                "i" = "  napistu_config <- napistu_config(python = list(virtualenv = '/path/to/python{min_python}/venv'))"
            ))
        })
    }, error = function(e) {
        # Re-throw other errors (broken installations, etc.)
        rlang::abort(e$message, parent = e)
    })
    
    # Check if environment already exists
    if (conda_env_exists(env_name, conda_exe)) {
        cli::cli_inform("Using existing conda environment {.val {env_name}}")
        
        reticulate::use_condaenv(env_name, conda = conda_exe, required = TRUE)
        
        # Validate Python version and packages
        validate_python_version(verbose)
        validate_and_import_modules(verbose = verbose)
        
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
#' @keywords internal
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
            cli::cli_abort(c(
                "Miniconda installation found but conda executable is missing",
                "i" = "Miniconda path: {.path {miniconda_path}}",
                "i" = "Expected conda at: {.path {conda_exe}}",
                "i" = "Please reinstall miniconda or manually configure Python"
            ))
        }
        
        if (validate_conda_executable(conda_exe)) {
            return(conda_exe)
        }
    }
    
    # No conda found anywhere
    rlang::abort("Conda is not available", class = "conda_not_available_error")
}

#' Validate Conda Executable
#'
#' @param conda_exe Path to conda executable
#' @return Logical indicating if conda works
#' @keywords internal
validate_conda_executable <- function(conda_exe) {
    tryCatch({
        result <- system2(conda_exe, "--version", stdout = TRUE, stderr = TRUE)
        if (length(result) > 0 && !is.null(result)) {
            return(TRUE)
        }
        
        cli::cli_abort(c(
            "Conda executable found but not working",
            "i" = "Conda path: {.path {conda_exe}}",
            "i" = "Please check your conda installation"
        ))
    }, error = function(e) {
        cli::cli_abort(c(
            "Conda executable failed to run",
            "i" = "Conda path: {.path {conda_exe}}",
            "i" = "Error: {e$message}"
        ))
    })
}

#' Check if Conda Environment Exists
#'
#' @param env_name Name of conda environment
#' @param conda_exe Path to conda executable
#' @return Logical indicating if environment exists
#' @keywords internal
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
#' Create a conda environment with Napistu installed.
#'
#' @param env_name Name of environment to create
#' @param python_version Python version to install
#' @param conda_exe Path to conda executable
#' @return NULL (side effect: creates environment)
#' @keywords internal
create_conda_environment <- function(env_name, python_version, conda_exe) {
    cli::cli_inform("Creating conda environment {.val {env_name}} with Python {python_version}")
    
    tryCatch({
        reticulate::conda_create(
            envname = env_name,
            packages = c(glue::glue("python={python_version}"), "pip"),
            conda = conda_exe
        )
        
        # Extract package names from module spec
        modules_spec <- process_required_modules_spec(NAPISTU_CONSTANTS$REQUIRED_PYTHON_MODULES)
        
        cli::cli_inform("Installing {.pkg {modules_spec}} in conda environment")
        reticulate::conda_install(
            envname = env_name,
            packages = modules_spec,
            pip = TRUE,
            conda = conda_exe
        )
        
        cli::cli_alert_success(c("Conda environment {.val {env_name}} created successfully"))
        cli::cli_alert_info("It can be removed with {.field cleanup_napistu()}")
        
    }, error = function(e) {
        cli::cli_abort("Failed to create conda environment: {e$message}")
    })
}

confirm_install_miniconda <- function () {
    if (interactive()) {
        cli::cli_rule("Python Environment Setup")
        cli::cli_alert_warning("Conda not available for napistu setup")
        cli::cli_text("")
        cli::cli_text("Options:")
        cli::cli_text("  1. Install miniconda automatically (recommended)")
        cli::cli_text("  2. Cancel and configure manually")
        cli::cli_text("")
        cli::cli_text("Miniconda will be installed to: {.path {reticulate::miniconda_path()}}")
        cli::cli_text("If you install it you can remove it once you're done with: {.field cleanup_napistu()}")
        
        response <- readline("Choice (1/2): ")
        if (response != "1") {
            cli::cli_abort(c(
                "Setup cancelled by user",
                "i" = "Configure manually with: napistu_config <- napistu_config(python = list(conda = 'your-env'))"
            ))
        }
    }
    
    return (invisible(NULL))
}


#' Process Required Modules Specification
#'
#' @param required_modules Named character vector of modules with optional version constraints
#' @return Character vector of package specifications for conda/pip
#' @keywords internal
process_required_modules_spec <- function(required_modules) {
    checkmate::assertNamed(required_modules)
    
    # Build package specifications with version constraints
    package_specs <- character(length(required_modules))
    for (i in seq_along(required_modules)) {
        package_name <- names(required_modules)[i]
        version <- required_modules[[i]]
        
        if (is.na(version) || version == "") {
            # No version constraint
            package_specs[i] <- package_name
        } else {
            # Add version constraint
            package_specs[i] <- paste0(package_name, ">=", version)
        }
    }
    
    return(package_specs)
}