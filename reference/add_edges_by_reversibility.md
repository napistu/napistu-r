# Add Edges By Reversibility with Scalable Arrows

Add Edges By Reversibility with Scalable Arrows

## Usage

``` r
add_edges_by_reversibility(
  grob,
  edge_width,
  show_edges_if = NULL,
  vertex_size = 6,
  arrow_scale_factor = 0.1,
  vertex_cap_scale = 0.02,
  min_arrow_size = 0.02,
  max_arrow_size = 1
)
```

## Arguments

- grob:

  a ggplot2 grob

- edge_width:

  width of edges on graph

- show_edges_if:

  A named list containing filters to apply based on edge attributes.
  Each element should be a named list with:

  - `cutoff`: Numeric value specifying the threshold

  - `retain`: Character string, either "above" or "below"

  Example format:
  `list( weight = list(cutoff = 0.5, retain = "above"), correlation = list(cutoff = 0.9, retain = "below") )`

- vertex_size:

  vertex size in ggplot2 units (should match the size used in
  geom_node_point)

- arrow_scale_factor:

  multiplier for arrow size relative to edge width (default: 2.5)

- vertex_cap_scale:

  multiplier for vertex cap relative to vertex size (default: 0.02)

- min_arrow_size:

  minimum arrow size in inches (default: 0.05)

- max_arrow_size:

  maximum arrow size in inches (default: 0.3)
