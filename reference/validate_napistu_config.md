# Validate Napistu Configuration Object

Validate Napistu Configuration Object

## Usage

``` r
validate_napistu_config(napistu_config)
```

## Arguments

- napistu_config:

  A napistu configuration object of class \`napistu_config\` containing
  all necessary configuration settings for the loading required assets
  and configuring and/or using an exiting appropriate Python
  environment. This config can be read from a yaml file using
  [load_napistu_config](https://napistu.github.io/napistu-r/reference/load_napistu_config.md)
  or directly specified in R using
  [create_napistu_config](https://napistu.github.io/napistu-r/reference/create_napistu_config.md).
  Contains:

  python_config

  :   A shallow list defining the Python environment. See
      [validate_python_config](https://napistu.github.io/napistu-r/reference/validate_python_config.md)

  assets_config

  :   A shallow list defining the Napistu assets to use. See
      [validate_assets_config](https://napistu.github.io/napistu-r/reference/validate_assets_config.md)

  created_at

  :   Time stamp

## Value

Invisible TRUE if valid, throws error if invalid
