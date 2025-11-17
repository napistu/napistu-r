# Validate Score Palette

Validate Score Palette

## Usage

``` r
validate_score_palette(score_palette, score_overlay)
```

## Arguments

- score_palette:

  optional, color palette for scores. If provided this can be a string
  defining built-in palettes or a custom palette

  log2 fold-change

  :   A blue -\> black -\> yellow color palette which is symmetric
      around zero

  indication scores

  :   A gray -\> yellow -\> orange -\> red palette ranging from 0-1

  otherwise

  :   A \`Scale\` object defining a custom palette

- score_overlay:

  optional, vertex-level scores containing \`score\` and the merging
  attribute specified in \`join_on\`

## Value

Invisible TRUE if valid, throws error if invalid
