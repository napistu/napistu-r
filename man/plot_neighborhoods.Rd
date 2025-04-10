% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/neighborhoods.R
\name{plot_neighborhoods}
\alias{plot_neighborhoods}
\title{Summarize Neighborhoods}
\usage{
plot_neighborhoods(
  neighborhood_table,
  consensus_graph,
  score_overlay = NULL,
  score_label = NULL,
  ...
)
}
\arguments{
\item{neighborhood_table}{Neighborhood table created by
\code{create_neighborhood_table}}

\item{consensus_graph}{igraph model of network}

\item{score_overlay}{optional, scores of neighbors for an indication from \link{summarize_indication}.}

\item{score_label}{optional, name of disease being overlaid.}

\item{...}{extra arguments passed to \code{\link{plot_one_neighborhood}}}
}
\description{
Summarize Neighborhoods
}
\examples{

if (interactive()) {
  interactive_initialization_wrapper(TRUE)

  species_id <- rownames(consensus_model$species)[
    consensus_model$species$s_name == "cAMP"
    ]
  species_id <- sample(rownames(consensus_model$species), 1)

  neighborhood_table <- create_neighborhood_table(
    species_id = species_id,
    consensus_model = consensus_model,
    consensus_graph = consensus_graph,
    cpr = cpr,
    max_steps = 3L,
    max_neighbors = 40L,
  )

 # score_overlay <- summarize_indication(
 #   disease_id = "EFO_0000400",
 #   consensus_model,
 #   create_neighborhood_summary_table(neighborhood_table),
 #   species_identifiers
 # )

 score_overlay <- neighborhood_table \%>\%
   dplyr::select(vertices) \%>\%
   tidyr::unnest(vertices) \%>\%
   dplyr::filter(node_type == "species") \%>\%
   dplyr::distinct(s_id) \%>\%
   dplyr::sample_frac(0.5) \%>\%
   dplyr::mutate(score = rnorm(n()))

  neighborhood_summaries <- plot_neighborhoods(
    neighborhood_table,
    consensus_graph,
    score_overlay,
    score_label = "diabetes mellitus",
    score_palette = "log2 fold-change"
  )
}
}
