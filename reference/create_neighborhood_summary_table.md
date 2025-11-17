# Create Neighborhood Summary Table

Unnest the neighborhoods' vertices

## Usage

``` r
create_neighborhood_summary_table(neighborhood_table)
```

## Arguments

- neighborhood_table:

  a tibble produced by
  [create_neighborhood_table](https://napistu.github.io/napistu-r/reference/create_neighborhood_table.md)
  containing one row per neighborhood with nested lists as attributes:

  sc_name

  :   A human readible name for the focal vertex

  s_id

  :   The internal unique species id of the focal vertex

  c_id

  :   The internal unique compartment id of the focal vertex

  sc_id

  :   The internal unique compartmentalized species id of the focal
      vertex

  sc_Source

  :   The Source object for the focal vertex

  vertices

  :   The vertices in the focal vertex's neighborhood

  edges

  :   The edges in the focal vertex's neighborhood

  reaction_sources

  :   The source pathways of the reaction vertices

## Value

an unnested table of vertices across all neighborhoods
