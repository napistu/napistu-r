# Create Subgraph

Filter to an induced subgraph containing a set of vertices and their
connections

## Usage

``` r
define_subgraphs(napistu_list, subgraph_vertices, max_components = 4L)
```

## Arguments

- napistu_list:

  A list containing loaded assets and bindings to Python modules.

  sbml_dfs

  :   SBML_dfs - the core pathway representation of the Napistu Python
      library

  napistu_graph

  :   Network graph - a Python igraph subclass with Napistu-specific
      attributes and methods

  species_identifiers

  :   Species identifier mappings

  precomputed_distances

  :   optional, distances between species nodes

  species_names

  :   A tibble containing the names of all genes, proteins, molecules,
      etc

  identifiers_nest

  :   A tibble with one row per ontology and a nested tibble containing
      all the identifiers and their corresponding molecular species

  python_modules

  :   A named list of Python modules: \`napistu\`

  python_environment

  :   See
      [validate_python_environment](https://napistu.github.io/napistu-r/reference/validate_python_environment.md)

  napistu_config

  :   A \`napistu_config\` object dictating how the \`napistu_list\` was
      initialized

  loaded_at

  :   A date-time object indicating when \`napistu_list\` was
      initialized

- subgraph_vertices:

  the vertices to retain in the subgraph

- max_components:

  the number of components to return

## Value

a list of the \`max_components\` largest weakly-connected components

## Examples

``` r
suppressPackageStartupMessages(library(dplyr))
setup_napistu_list(create_napistu_config())
#> 
#> ── Setting up Napistu environment ──────────────────────────────────────────────
#> No Python configuration specified, setting up conda environment
#> Using existing conda environment "napistu-env"
#> Python version 3.11 meets requirements
#> No assets configuration specified, loading bundled package data
#> ℹ Creating derived assets
#> ℹ Creating a table of species names to support lookups by name
#> ℹ Loading sbml_dfs from sbml_dfs.pkl
#> Error in value[[3L]](cond): Failed to load sbml_dfs from
#> /home/runner/work/_temp/Library/napistu.r/extdata/test_pathway/sbml_dfs.pkl:
#> ImportError: /usr/lib/x86_64-linux-gnu/libcrypto.so.3: version `OPENSSL_3.3.0'
#> not found (required by
#> /usr/share/miniconda/envs/napistu-env/lib/python3.11/lib-dynload/_ssl.cpython-311-x86_64-linux-gnu.so)
#> Run `reticulate::py_last_error()` for details.
subgraph_vertices <- sample(napistu_list$napistu_graph$vs["name"], 100)
#> Error: object 'napistu_list' not found
define_subgraphs(napistu_list, subgraph_vertices, max_components = 2)
#> Error: object 'napistu_list' not found
```
