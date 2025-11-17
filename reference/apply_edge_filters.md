# Apply Edge Filtering Based on Show Edges If Specification

Takes a tibble containing edge data and applies filters based on a
show_edges_if specification. Validates that all required attributes are
present in the tibble before applying filters.

## Usage

``` r
apply_edge_filters(edges_tibble, show_edges_if)
```

## Arguments

- edges_tibble:

  A tibble containing edge data with attribute columns

- show_edges_if:

  A named list containing filters to apply based on edge attributes.
  Each element should be a named list with:

  - `cutoff`: Numeric value specifying the threshold

  - `retain`: Character string, either "above" or "below"

  See
  [`validate_show_edges_if`](https://napistu.github.io/napistu-r/reference/validate_show_edges_if.md)
  for detailed format requirements.

## Value

A filtered tibble containing only edges that pass all specified filters
