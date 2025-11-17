# Validate Max Labeled Species

Validate Max Labeled Species

## Usage

``` r
validate_max_labeled_species(max_labeled_species)
```

## Arguments

- max_labeled_species:

  maximum number of species to label (to avoid overplotting). Labels
  which are likely to overlap are removed based on the graph's layout
  and the value of \`target_plot_width\`.

## Value

Invisible TRUE if valid, throws error if invalid
