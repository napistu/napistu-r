# Prepare Rendering

Prepare for the network plot by laying out the pathway in a way that
loosly groups related vertices and by suppressed overplotted labels

## Usage

``` r
prepare_rendering(
  r_graph,
  reaction_sources,
  network_layout,
  edge_weights,
  target_plot_width = 6
)
```

## Arguments

- r_graph:

  an R igraph network

- reaction_sources:

  an optional mapping from \`r_id\` to \`pathway_id\` and \`name\`.

- network_layout:

  method to used for creating a network layout (e.g., \`fr\`, \`kk\`,
  \`drl\`)

- target_plot_width:

  optional, specification for how large the planned plot is. knowing
  this helps to allow for more labels in a large plot where obscured
  labels will be filtered.

## Value

a list containing

- neighborhood_grob:

  A ggplot \`grob\` with no aethetics or geoms added

- vertices_df:

  A table of vertices with upstream metadata plus coordinates and
  updated labels

- pathway_coords:

  The bounding box of reaction's assigned to each pathway
