# Plot Shortest Path Network

Plot Shortest Path Network

## Usage

``` r
plot_shortest_path_network(
  napistu_list,
  shortest_paths_list,
  max_labeled_species = 10L,
  network_layout = "fr",
  edge_weights = NULL
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

- shortest_paths_list:

  results from napistu\$network\$find_all_shortest_reaction_paths

- max_labeled_species:

  maximum number of species to label (to avoid overplotting). Labels
  which are likely to overlap are removed based on the graph's layout
  and the value of \`target_plot_width\`.

- network_layout:

  method to used for creating a network layout (e.g., \`fr\`, \`kk\`,
  \`drl\`)

- edge_weights:

  Numeric vector of edge weights, character string naming an edge
  attribute, NULL to use graph's "weight" attribute, or NA to explicitly
  use no weights

## Examples

``` r
# NOTE - you may have to run this a few times to find a valid path between 2 random nodes
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
#> ImportError:
#> /usr/share/miniconda/envs/napistu-env/lib/python3.11/lib-dynload/pyexpat.cpython-311-x86_64-linux-gnu.so:
#> undefined symbol: XML_SetAllocTrackerActivationThreshold Run
#> `reticulate::py_last_error()` for details.
sbml_dfs <- napistu_list$sbml_dfs
#> Error: object 'napistu_list' not found
napistu_graph <- napistu_list$napistu_graph
#> Error: object 'napistu_list' not found
napistu <- napistu_list$python_modules$napistu
#> Error: object 'napistu_list' not found
source_species_id <- random_species(napistu_list)
#> Error: object 'napistu_list' not found
dest_species_id <- random_species(napistu_list)
#> Error: object 'napistu_list' not found

target_species_paths <- napistu$network$ng_utils$compartmentalize_species_pairs(
    sbml_dfs,
    source_species_id,
    dest_species_id
)
#> Error: object 'napistu' not found

shortest_paths_list <- try(napistu$network$paths$find_all_shortest_reaction_paths(
    napistu_graph,
    sbml_dfs,
    target_species_paths,
    weight_var = "weight"
), silent = TRUE)

if (!("try-error" %in% class(shortest_paths_list))) {
    plot_shortest_path_network(
        napistu_list,
        shortest_paths_list,
        max_labeled_species = 10L
    )
}
```
