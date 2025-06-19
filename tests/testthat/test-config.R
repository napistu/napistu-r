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

test_that("load_assets handles directory config correctly", {
    temp_dir <- tempfile()
    dir.create(temp_dir)
    
    # Create a real TSV file (works without Python)
    tsv_file <- file.path(temp_dir, "species_identifiers.tsv")
    writeLines(c("id\tname", "S1\tglucose"), tsv_file)
    
    # Create empty pickle files
    file.create(file.path(temp_dir, "sbml_dfs.pkl"))
    file.create(file.path(temp_dir, "napistu_graph.pkl"))
    
    # Create config that points to this directory
    napistu_config <- create_napistu_config(assets = list(asset_dir = temp_dir))
    
    # Mock only the external dependency (reticulate)
    testthat::local_mocked_bindings(
        py_load_object = function(path) list(mock = basename(path)),
        .package = "reticulate"
    )
    
    # Test the actual public function
    assets <- load_assets(napistu_config, verbose = FALSE)
    
    # Verify structure
    expect_type(assets, "list")
    expect_true("species_identifiers" %in% names(assets))
    expect_s3_class(assets$species_identifiers, "data.frame")
    
    unlink(temp_dir, recursive = TRUE)
})