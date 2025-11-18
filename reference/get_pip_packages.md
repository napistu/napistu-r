# Get installed Python packages from pip

Retrieves the list of installed Python packages in the current
reticulate environment using \`pip freeze\`. Returns results in a tidy
format with separate columns for package names and versions.

## Usage

``` r
get_pip_packages()
```

## Value

A tibble with three columns:

- package:

  Package name (character)

- version:

  Package version (character)

- full:

  Full pip freeze output format "package==version" (character)

## Details

This function calls \`pip freeze\` using the Python executable that
reticulate is currently configured to use (via
\`reticulate::py_exe()\`). It parses the output into a tidy format,
splitting package names and versions.

The function assumes packages follow the standard pip format of
"package==version". Edge cases like git URLs or local paths may not
parse correctly but will still appear in the \`full\` column.

## See also

\[reticulate::py_list_packages()\], \[reticulate::py_exe()\]

## Examples

``` r
if (FALSE) { # \dontrun{
# Get all installed packages
installed <- get_pip_packages()

# Check for specific packages
required <- c("numpy", "pandas", "scipy")
installed |>
  dplyr::filter(package %in% required)

# Find missing packages
missing <- setdiff(required, installed$package)
} # }
```
