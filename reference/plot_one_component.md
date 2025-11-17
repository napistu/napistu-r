# Plot One Component

Plot One Component

## Usage

``` r
plot_one_component(
  napistu_list,
  component_list,
  score_overlay = NULL,
  score_label = NULL,
  score_palette = NULL,
  join_scores_on = "name",
  vertex_size = 6,
  network_layout = "fr",
  edge_weights = NULL,
  edge_width = 0.5,
  show_edges_if = NULL,
  max_labeled_species = 20,
  target_plot_width = 6
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

- component_list:

  a list produced by
  [define_subgraphs](https://napistu.github.io/napistu-r/reference/define_subgraphs.md)
  containing the \`component_graph\` and \`reaction_sources\`

- score_overlay:

  optional, vertex-level scores containing \`score\` and the merging
  attribute specified in \`join_on\`

- score_label:

  optional, name of disease being overlaid

- score_palette:

  optional, color palette for scores. If provided this can be a string
  defining built-in palettes or a custom palette

  log2 fold-change

  :   A blue -\> black -\> yellow color palette which is symmetric
      around zero

  indication scores

  :   A gray -\> yellow -\> orange -\> red palette ranging from 0-1

  otherwise

  :   A \`Scale\` object defining a custom palette

- join_scores_on:

  variable to use when merging vertices and score

- vertex_size:

  vertices' size

- network_layout:

  method to used for creating a network layout (e.g., \`fr\`, \`kk\`,
  \`drl\`)

- edge_weights:

  Numeric vector of edge weights, character string naming an edge
  attribute, NULL to use graph's "weight" attribute, or NA to explicitly
  use no weights

- edge_width:

  width of edges on graph

- show_edges_if:

  A named list containing filters to apply based on edge attributes.
  Each element should be a named list with:

  - `cutoff`: Numeric value specifying the threshold

  - `retain`: Character string, either "above" or "below"

  Example format:
  `list( weight = list(cutoff = 0.5, retain = "above"), correlation = list(cutoff = 0.9, retain = "below") )`

- max_labeled_species:

  maximum number of species to label (to avoid overplotting). Labels
  which are likely to overlap are removed based on the graph's layout
  and the value of \`target_plot_width\`.

- target_plot_width:

  optional, specification for how large the planned plot is. knowing
  this helps to allow for more labels in a large plot where obscured
  labels will be filtered.

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
subgraph_list <- define_subgraphs(napistu_list, subgraph_vertices, max_components = 2)
#> Error: object 'napistu_list' not found
component_list <- subgraph_list[[1]]
#> Error: object 'subgraph_list' not found
score_overlay <- tibble::tibble(name = sample(subgraph_vertices, 20)) %>%
    dplyr::mutate(score = abs(stats::rnorm(dplyr::n())))
#> Error: object 'subgraph_vertices' not found

plot_one_component(
    napistu_list,
    component_list,
    score_overlay = score_overlay,
    score_palette = "log2 fold-change",
    edge_width = 0.5,
    target_plot_width = 6
)
#> Error: object 'score_overlay' not found

# advanced features
plot_one_component(
    napistu_list,
    component_list,
    score_overlay = score_overlay,
    score_palette = viridis::scale_color_viridis(),
    vertex_size = 2,
    edge_width = 0.5,
    show_edges_if = list(weight = list(cutoff = 0.6, retain = "below"))
)
#> Error: object 'score_overlay' not found
```
