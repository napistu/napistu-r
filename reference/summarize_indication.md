# Summarize Indications

Using a molecular neighborhood, find all Open Target scores for a
specified disease.

## Usage

``` r
summarize_indication(napistu_list, disease_id, neighborhood_summary_table)
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

- disease_id:

  A disease to search from the EFO ontology

- neighborhood_summary_table:

  tibble produced by `create_neighborhood_summary_table`

## Value

A tibble of species names and disease scores

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
#> ImportError:
#> /usr/share/miniconda/envs/napistu-env/lib/python3.11/lib-dynload/pyexpat.cpython-311-x86_64-linux-gnu.so:
#> undefined symbol: XML_SetAllocTrackerActivationThreshold Run
#> `reticulate::py_last_error()` for details.
species_id <- random_species(napistu_list)
#> Error: object 'napistu_list' not found

neighborhood_summary_table <- create_neighborhood_table(
    napistu_list,
    species_id,
    max_steps = 7L
    ) %>%
    create_neighborhood_summary_table()
#> Error: object 'napistu_list' not found
   
disease_id <- "EFO_0000400" # diabetes
# uncomment once https://github.com/napistu/napistu/issues/14 is resolved
# summarize_indication(napistu_list, disease_id, neighborhood_summary_table)
```
