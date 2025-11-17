# Load Single Asset File

Load Single Asset File

## Usage

``` r
load_single_asset(file_path, python_list, asset_name)
```

## Arguments

- file_path:

  Path to asset file

- python_list:

  A list containing reticulate bindings to Python packages and summaries
  of the Python environment.

  python_modules

  :   A named list of Python modules: \`napistu\`

  python_environment

  :   See
      [validate_python_environment](https://napistu.github.io/napistu-r/reference/validate_python_environment.md)

- asset_name:

  Name of asset for context

## Value

Loaded asset object
