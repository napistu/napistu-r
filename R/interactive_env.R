#' Interactive Initialization Wrapper
#'
#' Determine whether object created by interactive_initialization() are
#'   are present in the global environment, and if not call
#'   interactive_initialization()
#'
#' @param testing_env Logical indicating whether to load assets for testing (TRUE)
#'   or, prime-time pathway representations (FALSE)
#'
#' @returns 0, invisibly
#'
#' @export
interactive_initialization_wrapper <- function(testing_env = FALSE) {
    required_globals <- c(
        "napistu",
        "sbml_dfs",
        "napistu_graph",
        "precomputed_distances",
        "species_names",
        "species_identifiers"
    )
    
    if (!all(required_globals %in% ls(envir = globalenv()))) {
        interactive_initialization(testing_env)
    }
    
    return(invisible(0))
}

#' Interactive Initialization
#'
#' Add objects which support MechNet to the global environment. This can be
#'   used to run the full app or test its interactive and non-interactive
#'   components.
#'
#' @param testing_env Work off a simple environment
#'
#' Note that this approach breaks lots of conventions (assignment to global
#' environment; sourcing packages within functions, ...).
interactive_initialization <- function(testing_env = FALSE) {
    
    checkmate::assertLogical(testing_env, len = 1)
    
    MY_LOCAL_VENV <- "<<PATH_TO_VENV>>"
    
    MY_NAPISTU_ASSETS <- tibble::tribble(
        ~ type, ~ object, ~local_path,
        "sbml_dfs", "consensus_nocofactors.pkl", "<<PATH_TO_ASSETS>>/output_sbml_dfs.pkl",
        "species_identifiers", "species_identifiers.tsv", "<<PATH_TO_ASSETS>>/species_identifiers.tsv",
        "napistu graph", "regulatory_graph.pkl", "<<PATH_TO_ASSETS>>/regulatory_graph.pkl",
        "regulatory distances", "precomputed_distances.pkl", "<<PATH_TO_ASSETS>>/precomputed_distances.pkl"
    )
    
    MY_LOCAL_NAPISTU_PATH <- "<<PATH_TO_NAPISTUPY>>/napistu-py"
    
    checkmate::assertDirectoryExists(MY_LOCAL_VENV)
    checkmate::assertDirectoryExists(MY_LOCAL_NAPISTU_PATH)
    reticulate::use_virtualenv(MY_LOCAL_VENV, required = TRUE)
    
    # bind python packages - note the global assignment of "napistu"
    assign("napistu", reticulate::import("napistu"), envir = globalenv())
    
    # load R packages
    suppressPackageStartupMessages(library(dplyr))
    library(ggplot2)
    
    if (testing_env) {
        napistu_list <- load_assets_for_testing(MY_LOCAL_NAPISTU_PATH)
    } else {
        napistu_list <- load_assets_from_files(MY_NAPISTU_ASSETS)
    }
    
    species_names <- napistu_list$sbml_dfs$species %>%
        dplyr::select(-s_Identifiers, -s_Source)
    
    identifiers <- napistu_list$species_identifiers %>%
        tidyr::nest(ontology_ids = -ontology) %>%
        dplyr::mutate(n = purrr::map_int(ontology_ids, nrow)) %>%
        dplyr::arrange(dplyr::desc(n)) %>%
        dplyr::mutate(ontology = factor(ontology, levels = ontology))
    
    # assign to global environment
    assign("sbml_dfs", napistu_list$sbml_dfs, envir = globalenv())
    assign("napistu_graph", napistu_list$napistu_graph, envir = globalenv())
    assign("precomputed_distances", napistu_list$precomputed_distances, envir = globalenv())
    assign("species_identifiers", napistu_list$species_identifiers, envir = globalenv())
    assign("identifiers", napistu_list$species_identifiers, envir = globalenv())
    assign("species_names", species_names, envir = globalenv())
    
    return(invisible(0))
}

load_assets_for_testing <- function(napistu_path) {
    
    os <- reticulate::import("os")
    
    test_data <- file.path(napistu_path, "src", "tests", "test_data")
    checkmate::assertDirectoryExists(test_data)
    
    pw_index <- napistu$indices$PWIndex(os$path$join(test_data, "pw_index_metabolism.tsv"))
    sbml_dfs_dict <- napistu$consensus$construct_sbml_dfs_dict(pw_index)
    sbml_dfs <- napistu$consensus$construct_sbml_dfs(sbml_dfs_dict, pw_index)
    napistu_graph <- napistu$network$net_create$process_napistu_graph(sbml_dfs, graph_type="surrogate")
    precomputed_distances <- napistu$network$precompute$precompute_distances(napistu_graph)
    
    species_identifiers <- sbml_dfs$get_identifiers("species")
    species_identifiers <- napistu$sbml_dfs_core$filter_to_characteristic_species_ids(species_identifiers)
    
    output <- list(
        sbml_dfs = sbml_dfs,
        napistu_graph = napistu_graph,
        precomputed_distances = precomputed_distances,
        species_identifiers = species_identifiers
    )
    
    return(output)
}

load_assets_from_files <- function(napistu_assets) {
    
    napistu_asset_paths <- pluck_required_napistu_assets(
        napistu_assets,
        c("sbml_dfs", "species_identifiers", "napistu_graph", "precomputed_distances")
    )
    
    cli::cli_alert_info("Unpickling sbml dfs pathway representation")
    
    # download consensus model with pickle
    # if there are weird import errors then the napistu python package probably needs to be reinstalled
    sbml_dfs <- napistu$utils$load_pickle(napistu_asset_paths[["sbml_dfs"]])
    sbml_dfs$validate()
    
    # read a python igraph network
    cli::cli_alert_info("Unpickling network graph")
    napistu_graph <- napistu$utils$load_pickle(napistu_asset_paths[["napistu graph"]])
    
    cli::cli_alert_info("Unpickling pre-computed distances")
    precomputed_distances <- napistu$utils$load_pickle(napistu_asset_paths[["directed regulatory distances"]])
    
    cli::cli_alert_info("Organizing identifiers")
    species_identifiers <- readr::read_tsv(napistu_asset_paths[["species identifiers"]], show_col_types = FALSE)
    
    output <- list(
        sbml_dfs = sbml_dfs,
        napistu_graph = napistu_graph,
        precomputed_distances = precomputed_distances,
        species_identifiers = species_identifiers
    )
    
    return(output)
}

validate_napistu_assets <- function(napistu_assets) {
    
    checkmate::assertDataFrame(napistu_assets)
    
    # check for a mis-formatted DF
    required_napistu_asset_vars <- c("type", "object", "local_path")
    missing_vars <- setdiff(required_napistu_asset_vars, colnames(napistu_assets))
    
    if (length(missing_vars) > 0) {
        cli::cli_abort(
            "The following required column{?s} {?is/are} missing from the Napistu assets data frame: {.field {missing_vars}}"
        )
    }
    
    # check for missing assets
    missing_assets <- napistu_assets %>%
        dplyr::filter(!purrr::map_lgl(local_path, file.exists))
    
    if (nrow(missing_assets) > 0) {
        cli::cli_abort(
            "The following {nrow(missing_assets)} asset{?s} {?is/are} missing from the local file system: {.field {missing_assets$object}}.
        They were expected at {.file {missing_assets$local_path}}"
        )
    }
    
    # check for duplicated assets
    nonunique_assets <- unique(napistu_assets$type[duplicated(napistu_assets$type)])
    if (length(nonunique_assets) > 0) {
        cli::cli_abort(
            "The following asset type{?s} {?is/are} duplicated in the Napistu assets data frame: {.field {nonunique_assets}}"
        )
    }
    
    return(invisible(0))
    
}

pluck_required_napistu_assets <- function(napistu_assets, required_assets) {
    
    checkmate::assertDataFrame(napistu_assets)
    validate_napistu_assets(napistu_assets)
    
    checkmate::assertCharacter(required_assets, min.len = 1)
    missing_asset_types <- setdiff(required_assets, napistu_assets$type)
    if (length(missing_asset_types) > 0) {
        cli::cli_abort(
            "The following required asset type{?s} {?is/are} missing from the Napistu assets data frame: {.field {missing_asset_types}}"
        )
    }
    
    named_asset_paths <- napistu_assets %>%
        dplyr::filter(type %in% required_assets) %>%
        {stats::setNames(.$local_path, .$type)}
    
    return(named_asset_paths)
}