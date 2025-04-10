% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/netcontextr_utils.R
\name{gene_expression_by_tissue}
\alias{gene_expression_by_tissue}
\title{Determine gene by tissue expression}
\usage{
gene_expression_by_tissue(
  expression_data,
  feature_defining_var = "ensembl_gene_id",
  invalid_vars = c("ensembl_geneTranscript_id", "Description")
)
}
\arguments{
\item{expression_data}{GTEX data loaded by `load_and_clean_gtex_data`}

\item{feature_defining_var}{A variable in \code{expression_data} defining
a distinct feature}

\item{invalid_vars}{Variables in \code{expression_data} which should be dropped
before applying zFPKM because they are either non-numeric or invalid contexts.}
}
\value{
a tibble containing all of the information from the GTEX flat text
 file plus an addition "tissue" column, which is a list of tibbles, with
 each tibble containing a single column denoting all of the tissues within
 which a gene is expressed (or a one row one column tibble with NA if none)
}
\description{
`gene_expression_by_tissue` uses GTEX tissue-specific gene expression data
loaded by `load_and_clean_gtex_data` along with the zFPKM normalization
algorithm/package to determine within which tissue (if any) each gene in
"expression_data" is expressed. More specifically, a node is considered to be
expressed if the corresponding gene's zFPKM value is greater than or equal to
-3. This cutoff is based on empirical data from ENCODE and corresponds to the
approximate threshold where the the ratio of active to repressed promoters
begins to be greater than 1. In other words, this algorithm attempts to
separate biologically relevant genes (associated with active promoters) from
ultralow-expression genes (associated with repressed promoters). -3 is a
liberal threshold, erring on the side of capturing some noisy genes rather
than missing active ones. After determining expression TRUE/FALSE this
function creates a "tissue" column which is a list of tibbles (all of the
tissues where a given gene is expressed are denoted in the single column
tibble for that gene). The "tissue" column can be used to annotate the genes
in a netcontextr graph object using the `annotate_genes` function. After
annotation, the netcontextr graph can then be trimmed using the
`trim_network_by_gene_attribute` function, preferably by specifying a
specific tissue of interest, since the filtering to all edges where the two
nodes overlap in one or more tissues is very memory intensive and will likely
crash (and is also not particularly meaningful)
}
\examples{
if(interactive()){
  # load gtex expression data
  gtex_file <- "data-raw/GTEx_Analysis_2017-06-05_v8_RNASeQCv1.1.9_gene_median_tpm.gct.gz"
  gtex_expression_data <- load_and_clean_gtex_data(gtex_file)
  # determine gene expression by tissue using zFPKM
  gtex_expression_data_zFPKM <- gene_expression_by_tissue(gtex_expression_data)
}
}
