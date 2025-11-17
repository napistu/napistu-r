# Prepare Score Overlays

Merge a table of vertex-level data with a network representation.

## Usage

``` r
prepare_score_overlays(vertices, score_overlay = NULL, join_scores_on = "s_id")
```

## Arguments

- vertices:

  a table of vertices containing the variable specified in \`join_on\`

- score_overlay:

  optional, vertex-level scores containing \`score\` and the merging
  attribute specified in \`join_on\`

- join_scores_on:

  variable to use when merging vertices and score
