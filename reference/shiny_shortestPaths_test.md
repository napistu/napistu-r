# Shiny Shortest Paths Test

Demo for finding the shortest paths between pairs of entities.

## Usage

``` r
shiny_shortestPaths_test(napistu_list)
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

## Value

a shiny app

## Examples

``` r
if (interactive()) {
  setup_napistu_list(create_napistu_config())
  shiny_shortestPaths_test(napistu_list)
}
```
