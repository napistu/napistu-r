# Napistu in R!

The `napistu` R package which is part of the [Napitsu project](https://github.com/napistu/napistu).

## Repo Contents

- Functions for working with bioconductor's organism-specific packages.
- Functions for querying Open Target's graphQL database to summarize targets and their top indications.
- Shiny modules and related functions for exploring networks interactively.

## Setup directions

This package can be installed in several ways:

1.  Install from GitHub with a PAT:
    1. Install `napistu-r` in an interactive R session using remotes (or devtools) from GitHub:

```r
remotes::install_github("napistu/napistu-r")
```

2.  Clone and build locally:

```bash
!bash
git clone https://github.com/napistu/napistu-r.git
R CMD build napistu-r
```
