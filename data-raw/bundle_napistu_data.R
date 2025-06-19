python_env <- create_default_conda_env()

napistu <- reticulate::import("napistu")

ASSET_NAME <- "test_pathway"
OUT_PATH <- "inst/extdata"

x <- napistu$gcs$downloads$load_public_napistu_asset(
    ASSET_NAME,
    OUT_PATH,
    subasset = "sbml_dfs"
)

file.remove(file.path(OUT_PATH, glue::glue("{ASSET_NAME}.tar.gz")))
