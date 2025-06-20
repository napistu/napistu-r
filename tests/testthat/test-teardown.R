# Runs after ALL tests
cli::cli_alert_info("Tearing down test environment...")

cleanup_napistu(napistu_list)

# Validate cleanup with proper test structure
test_that("cleanup removes all test artifacts", {
    
    python_environment <- napistu_list$python_environment
    
    if (python_environment$miniconda_installed) {
        # miniconda installation cleanup
        expect_false(dir.exists(reticulate::miniconda_path()))
    }
    
    if (!python_environment$miniconda_installed && python_environment$created_by_napistu) {
        # conda environment cleanup
        available_conda_envs <- reticulate::conda_list()$name
        expect_false(python_environment$path %in% available_conda_envs)
    }
})

cli::cli_alert_success("Teardown validation complete")
