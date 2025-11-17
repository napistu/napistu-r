# Create Default Conda Environment

Setup an conda environment with Napistu installed. This will install
miniconda if needed (with user permission) and create an environment
if-needed in an existing conda/miniconda installation or the on-the-fly
installation. This process can be undone using
[cleanup_napistu](https://napistu.github.io/napistu-r/reference/cleanup_napistu.md).

## Usage

``` r
create_default_conda_env(env_name = "napistu-env", verbose = TRUE)
```

## Arguments

- env_name:

  The name of the to-be-created conda environment

- verbose:

  Logical scalar; if TRUE (default), prints informative messages about
  the current operation progress. If FALSE, runs silently.

## Value

List with environment info and metadata
