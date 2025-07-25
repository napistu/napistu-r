% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/subgraph.R
\name{plot_subgraph}
\alias{plot_subgraph}
\title{Plot Subgraphs}
\usage{
plot_subgraph(
  napistu_list,
  subgraph_list,
  score_overlay = NULL,
  join_scores_on = "name",
  max_labeled_species = 20,
  ...
)
}
\arguments{
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

\item{subgraph_list}{a list-of-lists produced by \link{define_subgraphs}
containing graphs and pathway-associations for the largest weakly
connected components}

\item{score_overlay}{optional, vertex-level scores containing `score`
and the merging attribute specified in `join_on`}

\item{join_scores_on}{variable to use when merging vertices and score}

\item{max_labeled_species}{the number of vertices to try to label. Some
labels may be dropped to improve clarity.}

\item{...}{additional arguments to pass to \link{plot_one_component}}
}
\description{
Plot Subgraphs
}
\examples{
setup_napistu_list(create_napistu_config())
subgraph_vertices <- sample(napistu_list$napistu_graph$vs["name"], 100)
subgraph_list <- define_subgraphs(napistu_list, subgraph_vertices, max_components = 2)
plot_subgraph(napistu_list, subgraph_list, vertex_size = 3, edge_width = 0.5)
}
