# Get Layout Properties

Get Layout Properties

## Usage

``` r
get_layout_properties(network_layout = "fr")
```

## Arguments

- network_layout:

  method to used for creating a network layout (e.g., \`fr\`, \`kk\`,
  \`drl\`)

## Value

a list containing:

- layout_fxn:

  The igraph function used for laying out the graph

- weights_attract:

  TRUE if large weights pull vertices together; FALSE if large weights
  push them apart
