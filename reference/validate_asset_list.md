# Validate Assets List

Validate Assets List

## Usage

``` r
validate_asset_list(asset_list)
```

## Arguments

- asset_list:

  A list containing loaded assets including:

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

## Value

Invisible TRUE if valid, throws error if invalid
