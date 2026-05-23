# Query Open Targets - Indications

Using Open Target's GraphQL API return indication score for a
disease/phenotype of interest across a set of genes.

## Usage

``` r
query_open_targets_indications(
  target_ensembl_gene_ids,
  disease_id,
  verbose = FALSE
)
```

## Arguments

- target_ensembl_gene_ids:

  a character vector of ensembl gene names

- disease_id:

  A disease to search from the EFO ontology

- verbose:

  print the request body and raw response

## Value

a list of target attributes

## Examples

``` r
target_ensembl_gene_ids <- c("ENSG00000090104", "ENSG00000163599")
disease_id <- "EFO_0001060" # celiac's disease
query_open_targets_indications(target_ensembl_gene_ids, disease_id)
#> $id
#> [1] "EFO_0001060"
#> 
#> $name
#> [1] "celiac disease"
#> 
#> $target_scores
#> $target_scores$count
#> [1] 2
#> 
#> $target_scores$rows
#> $target_scores$rows[[1]]
#> $target_scores$rows[[1]]$score
#> [1] 0.4969143
#> 
#> $target_scores$rows[[1]]$target
#> $target_scores$rows[[1]]$target$id
#> [1] "ENSG00000090104"
#> 
#> $target_scores$rows[[1]]$target$approvedSymbol
#> [1] "RGS1"
#> 
#> $target_scores$rows[[1]]$target$approvedName
#> [1] "regulator of G protein signaling 1"
#> 
#> 
#> 
#> $target_scores$rows[[2]]
#> $target_scores$rows[[2]]$score
#> [1] 0.4708346
#> 
#> $target_scores$rows[[2]]$target
#> $target_scores$rows[[2]]$target$id
#> [1] "ENSG00000163599"
#> 
#> $target_scores$rows[[2]]$target$approvedSymbol
#> [1] "CTLA4"
#> 
#> $target_scores$rows[[2]]$target$approvedName
#> [1] "cytotoxic T-lymphocyte associated protein 4"
#> 
#> 
#> 
#> 
#> 
```
