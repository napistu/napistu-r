cli::cli_alert_info("Setting up napistu test environment...")

# use the default Python configuration approach (falling back to miniconda installation)
# and use bundled Napistu assets
# these modifications to the system environment will be undone in teardown-napistu.R

setup_napistu_list(create_napistu_config())

test_that("napistu_list fixture is successfully initialized from defaults", {
    validate_napistu_list(napistu_list)
})
