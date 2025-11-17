# Label Vertices

Takes a pre-sorted table of vertices and annotates the top entries with
visually appealing ggtext labels.

## Usage

``` r
label_vertices(
  vertices,
  max_labeled_species,
  node_types_to_label = "species",
  always_label = NULL
)
```

## Arguments

- vertices:

  a table of the vertices to plot

- max_labeled_species:

  maximum number of species to label (to avoid overplotting). Labels
  which are likely to overlap are removed based on the graph's layout
  and the value of \`target_plot_width\`.

- node_types_to_label:

  what \`node_type\`s in \`vertices\` to consider for labeling

- always_label:

  always include these vertices regardless of their priority. Provide
  vertex names (generally starting with SC or R).

## Value

vertices with a \`ggtext_label\` variable added
