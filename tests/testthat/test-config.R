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
        "python_config:",
        "  conda: test-env",
        "assets_config:",
        "  assets_dir: /path/to/assets"
    )
    
    temp_file <- tempfile(fileext = ".yml")
    writeLines(yaml_content, temp_file)
    
    napistu_config <- load_napistu_config(temp_file, verbose = FALSE)
    
    expect_s3_class(napistu_config, "napistu_config")
    expect_equal(napistu_config$python_config$conda, "test-env")
    expect_equal(napistu_config$assets_config$assets_dir, "/path/to/assets")
    
    unlink(temp_file)
})

test_that("load_napistu_config handles empty YAML", {
    skip_if_not_installed("yaml")
    
    temp_file <- tempfile(fileext = ".yml")
    writeLines("", temp_file)
    
    napistu_config <- load_napistu_config(temp_file, verbose = FALSE)
    
    expect_s3_class(napistu_config, "napistu_config")
    expect_equal(napistu_config$python_config, list())
    expect_equal(napistu_config$assets_config, list())
    
    unlink(temp_file)
})

test_that("load_napistu_config fails on missing files", {
    expect_error(
        load_napistu_config("nonexistent.yml"),
        "does not exist"
    )
})
