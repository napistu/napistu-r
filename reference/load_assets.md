# Load Assets

Load a set of Napistu files

## Usage

``` r
load_assets(napistu_config, python_list, verbose = TRUE)
```

## Arguments

- napistu_config:

  A napistu configuration object of class \`napistu_config\` containing
  all necessary configuration settings for the loading required assets
  and configuring and/or using an exiting appropriate Python
  environment. This config can be read from a yaml file using
  [load_napistu_config](https://napistu.github.io/napistu-r/reference/load_napistu_config.md)
  or directly specified in R using
  [create_napistu_config](https://napistu.github.io/napistu-r/reference/create_napistu_config.md).
  Contains:

  python_config

  :   A shallow list defining the Python environment. See
      [validate_python_config](https://napistu.github.io/napistu-r/reference/validate_python_config.md)

  assets_config

  :   A shallow list defining the Napistu assets to use. See
      [validate_assets_config](https://napistu.github.io/napistu-r/reference/validate_assets_config.md)

  created_at

  :   Time stamp

- python_list:

  A list containing reticulate bindings to Python packages and summaries
  of the Python environment.

  python_modules

  :   A named list of Python modules: \`napistu\`

  python_environment

  :   See
      [validate_python_environment](https://napistu.github.io/napistu-r/reference/validate_python_environment.md)

- verbose:

  Logical scalar; if TRUE (default), prints informative messages about
  the current operation progress. If FALSE, runs silently.

## Value

A list containing loaded assets including:

- sbml_dfs:

  SBML_dfs - the core pathway representation of the Napistu Python
  library

- napistu_graph:

  Network graph - a Python igraph subclass with Napistu-specific
  attributes and methods

- species_identifiers:

  Species identifier mappings

- precomputed_distances:

  optional, distances between species nodes

- species_names:

  A tibble containing the names of all genes, proteins, molecules, etc

- identifiers_nest:

  A tibble with one row per ontology and a nested tibble containing all
  the identifiers and their corresponding molecular species

- reactions_source_total_counts:

  A pd.Series containing the number of reactions each pathway is
  associated with
