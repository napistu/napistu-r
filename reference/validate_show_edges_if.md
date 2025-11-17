# Validate Show Edges If Specification

Validates the structure and content of a show_edges_if specification
used for filtering edges based on their attributes for visualization
purposes.

## Usage

``` r
validate_show_edges_if(show_edges_if = NULL)
```

## Arguments

- show_edges_if:

  A named list containing filters to apply based on edge attributes.
  Each element should be a named list with:

  - `cutoff`: Numeric value specifying the threshold

  - `retain`: Character string, either "above" or "below"

  Example format:
  `list( weight = list(cutoff = 0.5, retain = "above"), correlation = list(cutoff = 0.9, retain = "below") )`

## Value

Invisible TRUE if validation passes, otherwise throws an error
