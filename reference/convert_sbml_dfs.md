# Convert SBML DFs

Convert a Python Napistu SBML_dfs pathway representation into R tables.
This will remove some attributes (identifiers and sources) and all the
methods which are essential for the sbml_dfs to function as an SBML_dfs
obbject but the core relational structure of the pathway can be more
easily explored after this conversion.

## Usage

``` r
convert_sbml_dfs(sbml_dfs)
```

## Arguments

- sbml_dfs:

  an sbml_dfs object, generally accessed from a \`napistu_list\`

## Value

an S3 r_sbml_dfs object which is a list with one table for each of
SBML_dfs' core attributes (compartments, species,
compartmentalized_species, reactions, and reaction_species)

## Examples

``` r
setup_napistu_list(create_napistu_config())
#> 
#> ── Setting up Napistu environment ──────────────────────────────────────────────
#> No Python configuration specified, setting up conda environment
#> Using existing conda environment "napistu-env"
#> Python version 3.11 meets requirements
#> No assets configuration specified, loading bundled package data
#> ℹ Creating derived assets
#> ℹ Creating a table of species names to support lookups by name
#> ℹ Loading sbml_dfs from sbml_dfs.pkl
#> Error in value[[3L]](cond): Failed to load sbml_dfs from
#> /home/runner/work/_temp/Library/napistu.r/extdata/test_pathway/sbml_dfs.pkl:
#> ImportError:
#> /usr/share/miniconda/envs/napistu-env/lib/python3.11/lib-dynload/pyexpat.cpython-311-x86_64-linux-gnu.so:
#> undefined symbol: XML_SetAllocTrackerActivationThreshold Run
#> `reticulate::py_last_error()` for details.
convert_sbml_dfs(napistu_list$sbml_dfs)
#> Error: object 'napistu_list' not found
```
