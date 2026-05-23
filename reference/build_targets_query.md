# Build Open Targets GraphQL Query for Targets

Returns the GraphQL query string used by \[post_open_targets_query()\].
Each comment in the query body names the format function that processes
that section, making it easy to locate what needs updating when the Open
Targets API changes a field name.

## Usage

``` r
build_targets_query()
```

## Value

a single character string containing the GraphQL query
