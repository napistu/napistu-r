# Convert Napistu Graph

Convert a Python \`graph.Graph\` object to a list of vertices and edges.

## Usage

``` r
convert_napistu_graph(napistu_graph, napistu_list)
```

## Arguments

- napistu_graph:

  a Python NapistuGraph object (a subclass of igraph.Graph)

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

## Value

an S3 r_napistu_graph object which is a list containing vertices and
edges.

## Examples

``` r
setup_napistu_list(create_napistu_config())
#> 
#> ── Setting up Napistu environment ──────────────────────────────────────────────
#> No Python configuration specified, setting up conda environment
#> Creating conda environment "napistu-env" with Python 3.11
#> + /usr/share/miniconda/bin/conda create --yes --name napistu-env 'python=3.11' pip --quiet -c conda-forge
#> Installing napistu>=0.5.2 in conda environment
#> ✔ Conda environment "napistu-env" created successfully
#> ℹ It can be removed with cleanup_napistu()
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
convert_napistu_graph(napistu_list$napistu_graph, napistu_list)
#> Error: object 'napistu_list' not found
```
