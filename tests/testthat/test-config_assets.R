test_that("resolve_directory_assets handles default filenames", {
    temp_dir <- tempfile()
    dir.create(temp_dir)
    
    # Create mock asset files with standard names
    standard_files <- c("sbml_dfs.pkl", "napistu_graph.pkl", "species_identifiers.tsv")
    for (file in standard_files) {
        file.create(file.path(temp_dir, file))
    }
    
    assets_config <- list(asset_dir = temp_dir)
    resolved <- resolve_directory_assets(assets_config, verbose = FALSE)
    
    expect_equal(resolved$sbml_dfs, file.path(temp_dir, "sbml_dfs.pkl"))
    expect_equal(resolved$napistu_graph, file.path(temp_dir, "napistu_graph.pkl"))
    expect_equal(resolved$species_identifiers, file.path(temp_dir, "species_identifiers.tsv"))
    
    unlink(temp_dir, recursive = TRUE)
})

test_that("resolve_directory_assets handles custom relative paths", {
    temp_dir <- tempfile()
    dir.create(temp_dir)
    
    # Create files with custom names
    file.create(file.path(temp_dir, "custom_sbml.pkl"))
    file.create(file.path(temp_dir, "napistu_graph.pkl"))  # Standard name for others
    
    assets_config <- list(
        asset_dir = temp_dir,
        sbml_dfs = "custom_sbml.pkl"  # Custom relative path
    )
    
    resolved <- resolve_directory_assets(assets_config, verbose = FALSE)
    
    expect_equal(resolved$sbml_dfs, file.path(temp_dir, "custom_sbml.pkl"))
    expect_equal(resolved$napistu_graph, file.path(temp_dir, "napistu_graph.pkl"))
    
    unlink(temp_dir, recursive = TRUE)
})

test_that("resolve_directory_assets fails on missing directory", {
    assets_config <- list(asset_dir = "/nonexistent/directory")
    
    expect_error(
        resolve_directory_assets(assets_config),
        "Assets directory does not exist",
        class = "rlang_error"
    )
})

test_that("get_configured_asset_paths handles absolute paths", {
    temp_dir <- tempfile()
    dir.create(temp_dir)
    
    # Create actual files for testing
    sbml_file <- file.path(temp_dir, "test_sbml.pkl")
    graph_file <- file.path(temp_dir, "test_graph.pkl")
    species_file <- file.path(temp_dir, "test_species.tsv")
    
    file.create(c(sbml_file, graph_file, species_file))
    
    assets_config <- list(
        sbml_dfs = sbml_file,
        napistu_graph = graph_file,
        species_identifiers = species_file
    )
    
    sources <- get_configured_asset_paths(assets_config)
    
    expect_equal(sources$sbml_dfs, sbml_file)
    expect_equal(sources$napistu_graph, graph_file)
    expect_equal(sources$species_identifiers, species_file)
    
    unlink(temp_dir, recursive = TRUE)
})

test_that("get_configured_asset_paths fails on missing required assets", {
    assets_config <- list(
        sbml_dfs = "/path/to/file.pkl"
        # Missing other required assets
    )
    
    expect_error(
        get_configured_asset_paths(assets_config),
        "Required assets missing from configuration",
        class = "rlang_error"
    )
})

test_that("get_configured_asset_paths fails on missing files", {
    assets_config <- list(
        sbml_dfs = "/nonexistent/file.pkl",
        napistu_graph = "/nonexistent/graph.pkl", 
        species_identifiers = "/nonexistent/species.tsv"
    )
    
    expect_error(
        get_configured_asset_paths(assets_config),
        "Asset file not found",
        class = "rlang_error"
    )
})

test_that("load_single_asset handles TSV files", {
    temp_dir <- tempfile()
    dir.create(temp_dir)
    
    # Create real TSV file (no mocking needed)
    tsv_file <- file.path(temp_dir, "test.tsv")
    writeLines(c("col1\tcol2", "val1\tval2"), tsv_file)
    
    result <- load_single_asset(tsv_file, "test_tsv")
    
    expect_s3_class(result, "data.frame")
    expect_equal(colnames(result), c("col1", "col2"))
    expect_equal(nrow(result), 1)
    
    unlink(temp_dir, recursive = TRUE)
})

test_that("load_single_asset handles pickle files", {
    temp_dir <- tempfile()
    dir.create(temp_dir)
    
    pkl_file <- file.path(temp_dir, "test.pkl")
    file.create(pkl_file)
    
    # Mock just the reticulate function with correct syntax
    testthat::local_mocked_bindings(
        py_load_object = function(path) list(mock = "pickle_data"),
        .package = "reticulate"
    )
    
    result <- load_single_asset(pkl_file, "test_pkl")
    expect_equal(result$mock, "pickle_data")
    
    unlink(temp_dir, recursive = TRUE)
})

test_that("load_single_asset handles JSON files", {
    temp_dir <- tempfile()
    dir.create(temp_dir)
    
    # Create real JSON file (no mocking needed)
    json_file <- file.path(temp_dir, "test.json")
    writeLines('{"test": "value"}', json_file)
    
    result <- load_single_asset(json_file, "test_json")
    expect_equal(result$test, "value")
    
    unlink(temp_dir, recursive = TRUE)
})

test_that("load_single_asset fails on unsupported file types", {
    temp_file <- tempfile(fileext = ".xyz")
    file.create(temp_file)
    
    expect_error(
        load_single_asset(temp_file, "test_asset"),
        "Unsupported file type",
        class = "rlang_error"
    )
    
    unlink(temp_file)
})