# Summarize Open Targets - Targets

Using Open Target's GraphQL API to read and format indications and
tractability information for a set of genes.

## Usage

``` r
summarize_open_targets_targets(target_ensembl_gene_ids, verbose = FALSE)
```

## Arguments

- target_ensembl_gene_ids:

  a character vector of ensembl gene names

- verbose:

  print the request body and raw response for each batch

## Value

a list containing:

- targets:

  tibble summarizing each target and their known chemical modulators

- indications:

  tibble summarizing disease/phenotypic associations scores related to
  each target. NULL if no scores are present

## Examples

``` r
target_ensembl_gene_ids <- c(
  "ENSG00000999999",
  "ENSG00000119718",
  "ENSG00000175354",
  "ENSG00000091831"
  )
summarize_open_targets_targets(target_ensembl_gene_ids)
#> Warning: No results for 1 ensembl IDs
#> were found in Open Targets:
#> ENSG00000999999
#> $targets
#> # A tibble: 3 × 13
#>   ensembl_id      approvedSymbol approvedName             small molecule tract…¹
#>   <chr>           <chr>          <chr>                    <chr>                 
#> 1 ENSG00000119718 EIF2B2         "eukaryotic translation… "Structure with Ligan…
#> 2 ENSG00000175354 PTPN2          "protein tyrosine phosp… "Druggable Family, Hi…
#> 3 ENSG00000091831 ESR1           "estrogen receptor 1"    "Approved Drug, Drugg…
#> # ℹ abbreviated name: ¹​`small molecule tractability`
#> # ℹ 9 more variables: `antibody tractability` <chr>,
#> #   `protac tractability` <chr>, `<NA>` <chr>, drugs <chr>,
#> #   `high quality probes` <chr>, `N probes` <int>, `o/e synonymous` <dbl>,
#> #   `o/e non-synonymous` <dbl>, `o/e loss-of-function` <dbl>
#> 
#> $indications
#> # A tibble: 75 × 13
#>    approvedSymbol disease_name      score affected_pathway animal_model clinical
#>    <chr>          <chr>             <dbl>            <dbl>        <dbl>    <dbl>
#>  1 EIF2B2         CACH syndrome      0.82             NA          NA       NA   
#>  2 EIF2B2         leukoencephalopa…  0.78             NA          NA       NA   
#>  3 ESR1           breast cancer      0.74             NA          NA        0.99
#>  4 EIF2B2         leukoencephalopa…  0.72             NA          NA       NA   
#>  5 ESR1           osteoporosis       0.72             NA           0.4      0.98
#>  6 ESR1           breast carcinoma   0.71              0.4        NA        0.88
#>  7 ESR1           breast neoplasm    0.69             NA          NA        0.94
#>  8 ESR1           neoplasm           0.67             NA          NA        0.95
#>  9 ESR1           endometriosis      0.66             NA          NA        0.9 
#> 10 ESR1           Estrogen resista…  0.66             NA           0.57    NA   
#> # ℹ 65 more rows
#> # ℹ 7 more variables: genetic_association <dbl>, genetic_literature <dbl>,
#> #   literature <dbl>, rna_expression <dbl>, somatic_mutation <dbl>,
#> #   therapeutic_area <chr>, description <chr>
#> 
```
