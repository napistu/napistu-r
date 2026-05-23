# Create Neighborhood Table

Create Neighborhood Table

## Usage

``` r
create_neighborhood_table(
  napistu_list,
  species_id,
  network_type = "hourglass",
  max_steps = 3L,
  max_neighbors = 10L,
  verbose = FALSE
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

- species_id:

  species identifier for focal node

- network_type:

  what type of neighborhood should be formed (ignored if `napistu_graph`
  is undirected).

  downstream

  :   descendants of the focal node

  upstream

  :   ancestors of the focal node

  hourglass

  :   descendants and ancestors of focal node

- max_steps:

  number of steps away from focal node allowed

- max_neighbors:

  prune to this number of upstream regulators and downstream targets

- verbose:

  Logical scalar; if TRUE (default), prints informative messages about
  the current operation progress. If FALSE, runs silently.

## Value

a tibble containing one row per neighborhood with nested lists as
attributes:

- sc_name:

  A human readible name for the focal vertex

- s_id:

  The internal unique species id of the focal vertex

- c_id:

  The internal unique compartment id of the focal vertex

- sc_id:

  The internal unique compartmentalized species id of the focal vertex

- sc_Source:

  The Source object for the focal vertex

- vertices:

  The vertices in the focal vertex's neighborhood

- edges:

  The edges in the focal vertex's neighborhood

- reaction_sources:

  The source pathways of the reaction vertices

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
#> ImportError:
#> /usr/share/miniconda/envs/napistu-env/lib/python3.11/lib-dynload/pyexpat.cpython-311-x86_64-linux-gnu.so:
#> undefined symbol: XML_SetAllocTrackerActivationThreshold Run
#> `reticulate::py_last_error()` for details.
species_id <- random_species(napistu_list)
#> Error: object 'napistu_list' not found

create_neighborhood_table(
    species_id,
    napistu_list = napistu_list,
    network_type = "hourglass",
    max_steps = 5L,
)
#> Error: object 'napistu_list' not found
```
