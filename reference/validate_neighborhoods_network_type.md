# Validate Neighborhood Network Type

Validate Neighborhood Network Type

## Usage

``` r
validate_neighborhoods_network_type(network_type)
```

## Arguments

- network_type:

  what type of neighborhood should be formed (ignored if `napistu_graph`
  is undirected).

  downstream

  :   descendants of the focal node

  upstream

  :   ancestors of the focal node

  hourglass

  :   descendants and ancestors of focal node

## Value

Invisible TRUE if valid, throws error if invalid
