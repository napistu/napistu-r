python_env <- create_default_conda_env()

napistu <- reticulate::import("napistu")

ASSET_NAME <- "test_pathway"
OUT_PATH <- "inst/extdata"

if (file.exists(OUT_PATH)) {
    unlink(OUT_PATH, recursive = TRUE)
}

x <- napistu$gcs$downloads$load_public_napistu_asset(
    ASSET_NAME,
    OUT_PATH,
    subasset = "sbml_dfs"
)

file.remove(file.path(OUT_PATH, glue::glue("{ASSET_NAME}.tar.gz")))

resource_fork_files <- list.files(
    file.path(OUT_PATH, ASSET_NAME),
    pattern = "^\\._", 
    recursive = TRUE, 
    full.names = TRUE,
    all.files = TRUE
)
if (length(resource_fork_files) > 0) {
    file.remove(resource_fork_files)
}