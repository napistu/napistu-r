test_that("create_napistu_config creates valid configuration objects", {
    # Empty config
    napistu_config <- create_napistu_config()
    expect_s3_class(napistu_config, "napistu_config")
    expect_equal(napistu_config$python, list())
    expect_equal(napistu_config$assets, list())
    expect_true("created_at" %in% names(napistu_config))
    
    # Config with parameters
    withr::with_tempdir({
        temp_dir <- getwd()  # This is the temporary directory created by with_tempdir
        
        # Create mock assets in the temporary directory
        assets_dir <- file.path(temp_dir, "assets")
        dir.create(assets_dir)
        
        # Create the mock files
        file_names <- c("foo.pkl", "bar.pkl", "baz.tsv")
        for (file_name in file_names) {
            file_path <- file.path(assets_dir, file_name)
            writeLines(character(0), file_path)
        }
        
        napistu_config <- create_napistu_config(
            python = list(conda = "test-env", conda_env_name = "foo"),
            assets = list(
                assets_dir = assets_dir,
                sbml_dfs = "foo.pkl",
                napistu_graph = "bar.pkl",
                species_identifiers = "baz.tsv"
            )
        )
    })
    
    expect_equal(napistu_config$python$conda, "test-env")
    expect_equal(napistu_config$assets$sbml_dfs, "foo.pkl")
})

# YAML loading tests
test_that("load_napistu_config loads simple YAML correctly", {
    skip_if_not_installed("yaml")
    skip_if_not_installed("withr")
    
    withr::with_tempdir({
        temp_dir <- getwd()  # This is the temporary directory created by with_tempdir
        
        # Create mock assets in the temporary directory
        assets_dir <- file.path(temp_dir, "assets")
        dir.create(assets_dir)
        
        # Create the mock files
        file_names <- c("sbml_dfs.pkl", "napistu_graph.pkl", "species_identifiers.tsv")
        for (file_name in file_names) {
            file_path <- file.path(assets_dir, file_name)
            writeLines(character(0), file_path)
        }
        
        # Create YAML content with the assets directory path
        yaml_content <- c(
            "python_config:",
            "  conda: test-env",
            "  conda_env_name: foo",
            "assets_config:",
            paste0("  assets_dir: ", assets_dir)
        )
        
        # Create a temporary YAML file inside the temp directory
        yaml_file <- file.path(temp_dir, "config.yml")
        writeLines(yaml_content, yaml_file)
        
        # Load the config
        napistu_config <- load_napistu_config(yaml_file, verbose = FALSE)
        
        # Test assertions
        expect_s3_class(napistu_config, "napistu_config")
        expect_equal(napistu_config$python_config$conda, "test-env")
        expect_equal(napistu_config$assets_config$assets_dir, assets_dir)
    })
    # Here, with_tempdir has cleaned up everything in the temporary directory
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
