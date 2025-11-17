# Load Assets from Paths

Load Assets from Paths

## Usage

``` r
load_assets_from_paths(asset_paths, python_list, verbose = TRUE)
```

## Arguments

- asset_paths:

  List containing the paths of to-be-loaded assets

- python_list:

  A list containing reticulate bindings to Python packages and summaries
  of the Python environment.

  python_modules

  :   A named list of Python modules: \`napistu\`

  python_environment

  :   See
      [validate_python_environment](https://napistu.github.io/napistu-r/reference/validate_python_environment.md)

- verbose:

  Logical scalar; if TRUE (default), prints informative messages about
  the current operation progress. If FALSE, runs silently.

## Value

List of loaded assets
