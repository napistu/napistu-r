# Validate Python Environment

Validate Python Environment

## Usage

``` r
validate_python_environment(python_environment)
```

## Arguments

- python_environment:

  Summary of the loaded Python environment and specification for cleanup
  procedure used by
  [cleanup_napistu](https://napistu.github.io/napistu-r/reference/cleanup_napistu.md)

  path

  :   The environment being used

  type

  :   The type of reticulate configuration: "virtualenv", "conda",
      "python"

  created_by_napistu

  :   Was the environment created by Napistu? If so it can be removed by
      [cleanup_napistu](https://napistu.github.io/napistu-r/reference/cleanup_napistu.md)

  miniconda_installed

  :   Was Miniconda installed by Napistu? If so it can be removed by
      [cleanup_napistu](https://napistu.github.io/napistu-r/reference/cleanup_napistu.md)

## Value

Invisible TRUE if valid, throws error if invalid
