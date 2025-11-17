# Layout With Reaction Sources

Create a \`ggraph\` layout trying to cluster vertices which are in the
same pathway and with appropriate handling of edge weights.

## Usage

``` r
layout_with_reaction_sources(
  r_graph,
  network_layout = "fr",
  edge_weights = NULL
)
```

## Arguments

- r_graph:

  An R igraph object

- network_layout:

  Layout method being used (affects weight interpretation)

- edge_weights:

  Numeric vector of edge weights, character string naming an edge
  attribute, NULL to use graph's "weight" attribute, or NA to explicitly
  use no weights

## Value

a \`ggraph\` \`gg_network_layout\` object
