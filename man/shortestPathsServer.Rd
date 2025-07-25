% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/module_shortest_paths.R
\name{shortestPathsServer}
\alias{shortestPathsServer}
\title{Shortest Paths Server}
\usage{
shortestPathsServer(id, napistu_list)
}
\arguments{
\item{id}{An ID string that corresponds with the ID used to call the module's
UI function.}

\item{napistu_list}{A list containing loaded assets and bindings to
  Python modules.

\describe{
    \item{sbml_dfs}{
        SBML_dfs - the core pathway representation of the Napistu Python library
        }
    \item{napistu_graph}{
        Network graph - a Python igraph subclass with Napistu-specific
        attributes and methods
        }
    \item{species_identifiers}{Species identifier mappings}
    \item{precomputed_distances}{optional, distances between species nodes}
    \item{species_names}{
         A tibble containing the names of all genes, proteins, molecules, etc
         }
    \item{identifiers_nest}{
         A tibble with one row per ontology and a nested tibble containing
         all the identifiers and their corresponding molecular species
         }
    \item{python_modules}{A named list of Python modules: `napistu`}
    \item{python_environment}{See \link{validate_python_environment}}
    \item{napistu_config}{
        A `napistu_config` object dictating how the `napistu_list` was initialized
    }
    \item{loaded_at}{A date-time object indicating when `napistu_list` was initialized}
  }}
}
\value{
Nothing; used for side-effects
}
\description{
Server-side components for the shortest paths shiny module
}
