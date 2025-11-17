# Plot Subgraphs

Plot Subgraphs

## Usage

``` r
plot_subgraph(
  napistu_list,
  subgraph_list,
  score_overlay = NULL,
  join_scores_on = "name",
  max_labeled_species = 20,
  ...
)
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

- subgraph_list:

  a list-of-lists produced by
  [define_subgraphs](https://napistu.github.io/napistu-r/reference/define_subgraphs.md)
  containing graphs and pathway-associations for the largest weakly
  connected components

- score_overlay:

  optional, vertex-level scores containing \`score\` and the merging
  attribute specified in \`join_on\`

- join_scores_on:

  variable to use when merging vertices and score

- max_labeled_species:

  maximum number of species to label (to avoid overplotting). Labels
  which are likely to overlap are removed based on the graph's layout
  and the value of \`target_plot_width\`.

- ...:

  additional arguments to pass to
  [plot_one_component](https://napistu.github.io/napistu-r/reference/plot_one_component.md)

## Examples

``` r
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
subgraph_list <- define_subgraphs(napistu_list, subgraph_vertices, max_components = 2)
#> Error: object 'napistu_list' not found
plot_subgraph(napistu_list, subgraph_list, vertex_size = 3, edge_width = 0.5)
#> Error: object 'subgraph_list' not found
```
