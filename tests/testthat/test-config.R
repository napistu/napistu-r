test_that("create_napistu_config creates valid configuration objects", {
    # Empty config
    napistu_config <- create_napistu_config()
    expect_s3_class(napistu_config, "napistu_config")
    expect_equal(napistu_config$python, list())
    expect_equal(napistu_config$assets, list())
    expect_true("created_at" %in% names(napistu_config))
    
    # Config with parameters
    napistu_config <- create_napistu_config(
        python = list(conda = "test-env"),
        assets = list(sbml_dfs = "/path/to/file.pkl")
    )
    expect_equal(napistu_config$python$conda, "test-env")
    expect_equal(napistu_config$assets$sbml_dfs, "/path/to/file.pkl")
})

# YAML loading tests
test_that("load_napistu_config loads simple YAML correctly", {
    skip_if_not_installed("yaml")
    
    yaml_content <- c(
        "python:",
        "  conda: test-env",
        "assets:",
        "  asset_dir: /path/to/assets"
    )
    
    temp_file <- tempfile(fileext = ".yml")
    writeLines(yaml_content, temp_file)
    
    napistu_config <- load_napistu_config(temp_file, verbose = FALSE)
    
    expect_s3_class(napistu_config, "napistu_config")
    expect_equal(napistu_config$python$conda, "test-env")
    expect_equal(napistu_config$assets$asset_dir, "/path/to/assets")
    
    unlink(temp_file)
})

test_that("load_napistu_config handles empty YAML", {
    skip_if_not_installed("yaml")
    
    temp_file <- tempfile(fileext = ".yml")
    writeLines("", temp_file)
    
    napistu_config <- load_napistu_config(temp_file, verbose = FALSE)
    
    expect_s3_class(napistu_config, "napistu_config")
    expect_equal(napistu_config$python, list())
    expect_equal(napistu_config$assets, list())
    
    unlink(temp_file)
})

test_that("load_napistu_config fails on missing files", {
    expect_error(
        load_napistu_config("nonexistent.yml"),
        "does not exist"
    )
})

# Integration test without external dependencies
test_that("asset loading workflow with directory config", {
    temp_dir <- tempfile()
    dir.create(temp_dir)
    
    # Create standard asset files
    asset_files <- c("sbml_dfs.pkl", "napistu_graph.pkl", "species_identifiers.tsv")
    for (file in asset_files) {
        writeLines("mock content", file.path(temp_dir, file))
    }
    
    # Create config
    napistu_config <- create_napistu_config(assets = list(asset_dir = temp_dir))
    
    # Mock the actual loading functions to avoid reticulate dependencies
    testthat::local_mocked_bindings(
        load_single_asset = function(path, name) {
            paste("loaded", name, "from", basename(path))
        }
    )
    
    # Test the workflow
    sources <- get_configured_asset_sources(napistu_config$assets, verbose = FALSE)
    expect_length(sources, 3)
    expect_true(all(file.exists(unlist(sources))))
    
    assets <- load_assets_from_sources(sources, verbose = FALSE)
    expect_length(assets, 3)
    expect_true(all(grepl("loaded", assets)))
    
    unlink(temp_dir, recursive = TRUE)
})