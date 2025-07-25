% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/config_assets.R
\name{get_configured_asset_paths}
\alias{get_configured_asset_paths}
\title{Get Configured Asset Sources}
\usage{
get_configured_asset_paths(assets_config, verbose = TRUE)
}
\arguments{
\item{assets_config}{List containing asset configuration settings as part of
the `napistu_config`. If this is an empty list, the `test_pathway` bundled
with `napistu-r` will be used.
  
\describe{
    \item{assets_dir}{
        relative to this directory.
    }
    \item{sbml_dfs}{
        Path to a pickled `SBML_dfs` object generated by the
        Napistu Python package. `SBML_df`s are self-contained relational
        representations of a pathway.
    }
    \item{napistu_graph}{
        Path to a pickled `NapistuGraph` object generated by the Napistu
        Python package. `NapistuGraph`s are a subclass of `igraph.Graph`.
        }
    \item{species_identifiers}{
        Path to a .tsv containing the systematic identifiers for all
        of the molecular species in the `SBML_dfs`.
    }
    \item{precomputed_distances}{
        Optional, path to a .parquet file containing pairwise distances
        between molecular species. Defining this can speed up the calculation
        of shortest paths and neighborhoods.
    }
}}

\item{verbose}{Logical scalar; if TRUE (default), prints informative messages
about the current operation progress. If FALSE, runs silently.}
}
\value{
Named list of asset file paths for configured assets
}
\description{
Get Configured Asset Sources
}
\keyword{internal}
