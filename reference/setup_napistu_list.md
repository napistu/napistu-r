# Setup Napistu List

Load a config specifying a Python environment and paths to Napistu
assets as a list providing defaults as fall-backs.

## Usage

``` r
setup_napistu_list(
  napistu_config,
  napistu_list_object = "napistu_list",
  overwrite = FALSE,
  verbose = TRUE,
  skip_validation = FALSE
)
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

- napistu_list_object:

  The name that setup_napistu_env() will be assigned to. If this object
  already exists then setup will be skipped unless \`overwrite\` is TRUE

- overwrite:

  Overwrite existing caches

- verbose:

  Logical scalar; if TRUE (default), prints informative messages about
  the current operation progress. If FALSE, runs silently.

- skip_validation:

  if FALSE, use \`validate_asset_list_thorough\` to comprehensively
  validate assets. If TRUE, asset validation is skipped. This can be
  helpful when working with assets which have already been validated
  because asset validation can be slow.

## Value

None; output is assigned to \`napistu_list_object\`

## Examples

``` r
if (FALSE) { # \dontrun{
napistu_config <- create_napistu_config()
setup_napistu_list(napistu_config)

# Using config file
setup_napistu_list("my_config.yml")
} # }
```
