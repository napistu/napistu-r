# Bioconductor Org Function

Call a function from one of the bioconductor org packages

## Usage

``` r
bioconductor_org_function(object_type, species)
```

## Arguments

- object_type:

  Type of function to call

- species:

  Species name

## Value

a tibble for function calls which can be coerced to a tabular result or
the raw results otherwise

## Examples

``` r
if (FALSE) { # \dontrun{
bioconductor_org_function("ENZYME", "Homo sapiens")
} # }
```
