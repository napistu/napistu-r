# Validate Python Config

Validate Python Config

## Usage

``` r
validate_python_config(python_config)
```

## Arguments

- python_config:

  List describing the Python environment to use as part of the
  \`napistu_config\`. This can be the system python, a virtual
  environment or conda. If \`python_config\` is an empty list, a
  miniconda environment and installation can be established on-the-fly.

  virtualenv

  :   Optional, path to a virtual environment

  conda

  :   Optional, path to a conda executable. This can be found with
      \`reticulate::conda_exe()\`. If defined, the \`conda_env_name\`
      should also be defined.

  conda_env_name

  :   Optional, name of the conda environment to use. A suitable
      environment can be created using
      [create_conda_environment](https://napistu.github.io/napistu-r/reference/create_conda_environment.md).

  python

  :   Optional, path to a Python distribution.

## Value

Invisible TRUE if valid, throws error if invalid
