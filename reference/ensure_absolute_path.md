# Convert a path to an absolute path

If the path is already absolute (begins with "/"), it is returned as-is.
If the path is relative, it is converted to an absolute path using
normalizePath().

## Usage

``` r
ensure_absolute_path(unresolved_path)
```

## Arguments

- unresolved_path:

  Character string representing a file or directory path

## Value

Character string with the absolute path
