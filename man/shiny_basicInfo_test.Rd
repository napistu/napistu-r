% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/module_basic_info.R
\name{shiny_basicInfo_test}
\alias{shiny_basicInfo_test}
\title{Shiny Basic Info Test}
\usage{
shiny_basicInfo_test(species_names, species_identifiers, consensus_model, cpr)
}
\arguments{
\item{species_names}{A tibble containing names of all reaction species.}

\item{species_identifiers}{A tibble containing systematic identifiers for all reaction species nested by ontology.}

\item{consensus_model}{A python pathway object with class cpr.sbml.SBML_dfs}

\item{cpr}{reticulate connection to the cpr python package}
}
\value{
a shiny app
}
\description{
Demo for querying the reactions a species participates in.
}
\examples{

if (interactive()) {
  interactive_initialization_wrapper()
  cpr <- reticulate::import("cpr")

  shiny_basicInfo_test(
    species_names,
    species_identifiers,
    consensus_model,
    cpr = cpr
  )
}
}
