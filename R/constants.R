NAPISTU_CONSTANTS <- list(
    # Standard asset filenames (same for bundled and directory-based)
    ASSET_FILENAMES = list(
        sbml_dfs = "sbml_dfs.pkl",
        napistu_graph = "napistu_graph.pkl",
        species_identifiers = "species_identifiers.tsv",
        precomputed_distances = "precomputed_distances.json"
    ),
    
    NAPISTU_LIST_OBJECT = "napistu_list",
    NAPISTU_LIST_CLASS = "napistu_list",
    NAPISTU_CONFIG_CLASS = "napistu_config",
    
    # Asset requirements
    REQUIRED_ASSETS = c("sbml_dfs", "napistu_graph", "species_identifiers"),
    OPTIONAL_ASSETS = c("precomputed_distances"),
    
    # Python configuration
    PYTHON_ENV_TYPES = c("virtualenv", "conda", "python"),
    REQUIRED_PYTHON_MODULES = c("napistu"),
    MINIMUM_PYTHON_VERSION = "3.11",
    DEFAULT_CONDA_ENV_NAME = "napistu-env",
    
    # File extensions
    SUPPORTED_EXTENSIONS = c("pkl", "tsv", "json"),
    
    # Default paths
    DEFAULT_VENV_NAME = ".venv"
)