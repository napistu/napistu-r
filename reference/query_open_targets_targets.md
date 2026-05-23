# Query Open Targets - Targets

Using Open Target's GraphQL API return gene-metadata and
disease/phenotype associations for a set of genes.

## Usage

``` r
query_open_targets_targets(
  target_ensembl_gene_ids,
  post_n_genes = 10,
  verbose = FALSE
)
```

## Arguments

- target_ensembl_gene_ids:

  a character vector of ensembl gene names

- post_n_genes:

  separate queries into posts of this number of genes

- verbose:

  print the request body and raw response for each batch

## Value

a list of target attributes

## Examples

``` r
target_ensembl_gene_ids <- c("ENSG00000119718", "ENSG00000175354")
query_open_targets_targets(target_ensembl_gene_ids)
#> [[1]]
#> [[1]]$id
#> [1] "ENSG00000119718"
#> 
#> [[1]]$approvedSymbol
#> [1] "EIF2B2"
#> 
#> [[1]]$approvedName
#> [1] "eukaryotic translation initiation factor 2B subunit beta"
#> 
#> [[1]]$associatedDiseases
#> [[1]]$associatedDiseases$count
#> [1] 166
#> 
#> [[1]]$associatedDiseases$rows
#> [[1]]$associatedDiseases$rows[[1]]
#> [[1]]$associatedDiseases$rows[[1]]$score
#> [1] 0.8246344
#> 
#> [[1]]$associatedDiseases$rows[[1]]$datatypeScores
#> [[1]]$associatedDiseases$rows[[1]]$datatypeScores[[1]]
#> [[1]]$associatedDiseases$rows[[1]]$datatypeScores[[1]]$id
#> [1] "genetic_association"
#> 
#> [[1]]$associatedDiseases$rows[[1]]$datatypeScores[[1]]$score
#> [1] 0.9325183
#> 
#> 
#> [[1]]$associatedDiseases$rows[[1]]$datatypeScores[[2]]
#> [[1]]$associatedDiseases$rows[[1]]$datatypeScores[[2]]$id
#> [1] "genetic_literature"
#> 
#> [[1]]$associatedDiseases$rows[[1]]$datatypeScores[[2]]$score
#> [1] 0.8651953
#> 
#> 
#> 
#> [[1]]$associatedDiseases$rows[[1]]$disease
#> [[1]]$associatedDiseases$rows[[1]]$disease$id
#> [1] "Orphanet_135"
#> 
#> [[1]]$associatedDiseases$rows[[1]]$disease$name
#> [1] "CACH syndrome"
#> 
#> [[1]]$associatedDiseases$rows[[1]]$disease$description
#> NULL
#> 
#> [[1]]$associatedDiseases$rows[[1]]$disease$therapeuticAreas
#> [[1]]$associatedDiseases$rows[[1]]$disease$therapeuticAreas[[1]]
#> [[1]]$associatedDiseases$rows[[1]]$disease$therapeuticAreas[[1]]$name
#> [1] "genetic, familial or congenital disease"
#> 
#> 
#> 
#> 
#> 
#> [[1]]$associatedDiseases$rows[[2]]
#> [[1]]$associatedDiseases$rows[[2]]$score
#> [1] 0.7830533
#> 
#> [[1]]$associatedDiseases$rows[[2]]$datatypeScores
#> [[1]]$associatedDiseases$rows[[2]]$datatypeScores[[1]]
#> [[1]]$associatedDiseases$rows[[2]]$datatypeScores[[1]]$id
#> [1] "literature"
#> 
#> [[1]]$associatedDiseases$rows[[2]]$datatypeScores[[1]]$score
#> [1] 0.1933138
#> 
#> 
#> [[1]]$associatedDiseases$rows[[2]]$datatypeScores[[2]]
#> [[1]]$associatedDiseases$rows[[2]]$datatypeScores[[2]]$id
#> [1] "genetic_association"
#> 
#> [[1]]$associatedDiseases$rows[[2]]$datatypeScores[[2]]$score
#> [1] 0.856228
#> 
#> 
#> [[1]]$associatedDiseases$rows[[2]]$datatypeScores[[3]]
#> [[1]]$associatedDiseases$rows[[2]]$datatypeScores[[3]]$id
#> [1] "genetic_literature"
#> 
#> [[1]]$associatedDiseases$rows[[2]]$datatypeScores[[3]]$score
#> [1] 0.8827042
#> 
#> 
#> 
#> [[1]]$associatedDiseases$rows[[2]]$disease
#> [[1]]$associatedDiseases$rows[[2]]$disease$id
#> [1] "MONDO_0800448"
#> 
#> [[1]]$associatedDiseases$rows[[2]]$disease$name
#> [1] "leukoencephalopathy with vanishing white matter"
#> 
#> [[1]]$associatedDiseases$rows[[2]]$disease$description
#> [1] "A new leukoencephalopathy, the CACH syndrome (Childhood Ataxia with Central nervous system Hypomyelination) or VWM (Vanishing White Matter) was identified on clinical and MRI criteria. Classically, this disease is characterized by (1) an onset between 2 and 5 years of age, with a cerebello-spastic syndrome exacerbated by episodes of fever or head trauma leading to death after 5 to 10 years of disease evolution, (2) a diffuse involvement of the white matter on cerebral MRI with a CSF-like signal intensity (cavitation), (3) a recessive autosomal mode of inheritance, (4) neuropathologic findings consistent with a cavitating orthochromatic leukodystrophy with increased number of oligodendrocytes with sometimes �foamy'' aspect."
#> 
#> [[1]]$associatedDiseases$rows[[2]]$disease$therapeuticAreas
#> [[1]]$associatedDiseases$rows[[2]]$disease$therapeuticAreas[[1]]
#> [[1]]$associatedDiseases$rows[[2]]$disease$therapeuticAreas[[1]]$name
#> [1] "nervous system disease"
#> 
#> 
#> [[1]]$associatedDiseases$rows[[2]]$disease$therapeuticAreas[[2]]
#> [[1]]$associatedDiseases$rows[[2]]$disease$therapeuticAreas[[2]]$name
#> [1] "genetic, familial or congenital disease"
#> 
#> 
#> 
#> 
#> 
#> [[1]]$associatedDiseases$rows[[3]]
#> [[1]]$associatedDiseases$rows[[3]]$score
#> [1] 0.7200241
#> 
#> [[1]]$associatedDiseases$rows[[3]]$datatypeScores
#> [[1]]$associatedDiseases$rows[[3]]$datatypeScores[[1]]
#> [[1]]$associatedDiseases$rows[[3]]$datatypeScores[[1]]$id
#> [1] "literature"
#> 
#> [[1]]$associatedDiseases$rows[[3]]$datatypeScores[[1]]$score
#> [1] 0.1345052
#> 
#> 
#> [[1]]$associatedDiseases$rows[[3]]$datatypeScores[[2]]
#> [[1]]$associatedDiseases$rows[[3]]$datatypeScores[[2]]$id
#> [1] "genetic_association"
#> 
#> [[1]]$associatedDiseases$rows[[3]]$datatypeScores[[2]]$score
#> [1] 0.6079308
#> 
#> 
#> [[1]]$associatedDiseases$rows[[3]]$datatypeScores[[3]]
#> [[1]]$associatedDiseases$rows[[3]]$datatypeScores[[3]]$id
#> [1] "genetic_literature"
#> 
#> [[1]]$associatedDiseases$rows[[3]]$datatypeScores[[3]]$score
#> [1] 0.9631731
#> 
#> 
#> 
#> [[1]]$associatedDiseases$rows[[3]]$disease
#> [[1]]$associatedDiseases$rows[[3]]$disease$id
#> [1] "MONDO_0020507"
#> 
#> [[1]]$associatedDiseases$rows[[3]]$disease$name
#> [1] "leukoencephalopathy with vanishing white matter 1"
#> 
#> [[1]]$associatedDiseases$rows[[3]]$disease$description
#> [1] "Any leukoencephalopathy with vanishing white matter in which the cause of the disease is a variation in the EIF2B1 gene."
#> 
#> [[1]]$associatedDiseases$rows[[3]]$disease$therapeuticAreas
#> [[1]]$associatedDiseases$rows[[3]]$disease$therapeuticAreas[[1]]
#> [[1]]$associatedDiseases$rows[[3]]$disease$therapeuticAreas[[1]]$name
#> [1] "nervous system disease"
#> 
#> 
#> [[1]]$associatedDiseases$rows[[3]]$disease$therapeuticAreas[[2]]
#> [[1]]$associatedDiseases$rows[[3]]$disease$therapeuticAreas[[2]]$name
#> [1] "genetic, familial or congenital disease"
#> 
#> 
#> 
#> 
#> 
#> [[1]]$associatedDiseases$rows[[4]]
#> [[1]]$associatedDiseases$rows[[4]]$score
#> [1] 0.6443069
#> 
#> [[1]]$associatedDiseases$rows[[4]]$datatypeScores
#> [[1]]$associatedDiseases$rows[[4]]$datatypeScores[[1]]
#> [[1]]$associatedDiseases$rows[[4]]$datatypeScores[[1]]$id
#> [1] "genetic_association"
#> 
#> [[1]]$associatedDiseases$rows[[4]]$datatypeScores[[1]]$score
#> [1] 0.9078533
#> 
#> 
#> [[1]]$associatedDiseases$rows[[4]]$datatypeScores[[2]]
#> [[1]]$associatedDiseases$rows[[4]]$datatypeScores[[2]]$id
#> [1] "genetic_literature"
#> 
#> [[1]]$associatedDiseases$rows[[4]]$datatypeScores[[2]]$score
#> [1] 0.6079308
#> 
#> 
#> 
#> [[1]]$associatedDiseases$rows[[4]]$disease
#> [[1]]$associatedDiseases$rows[[4]]$disease$id
#> [1] "MONDO_0957870"
#> 
#> [[1]]$associatedDiseases$rows[[4]]$disease$name
#> [1] "leukoencephalopathy with vanishing white matter 2"
#> 
#> [[1]]$associatedDiseases$rows[[4]]$disease$description
#> NULL
#> 
#> [[1]]$associatedDiseases$rows[[4]]$disease$therapeuticAreas
#> [[1]]$associatedDiseases$rows[[4]]$disease$therapeuticAreas[[1]]
#> [[1]]$associatedDiseases$rows[[4]]$disease$therapeuticAreas[[1]]$name
#> [1] "nervous system disease"
#> 
#> 
#> [[1]]$associatedDiseases$rows[[4]]$disease$therapeuticAreas[[2]]
#> [[1]]$associatedDiseases$rows[[4]]$disease$therapeuticAreas[[2]]$name
#> [1] "genetic, familial or congenital disease"
#> 
#> 
#> 
#> 
#> 
#> [[1]]$associatedDiseases$rows[[5]]
#> [[1]]$associatedDiseases$rows[[5]]$score
#> [1] 0.597687
#> 
#> [[1]]$associatedDiseases$rows[[5]]$datatypeScores
#> [[1]]$associatedDiseases$rows[[5]]$datatypeScores[[1]]
#> [[1]]$associatedDiseases$rows[[5]]$datatypeScores[[1]]$id
#> [1] "literature"
#> 
#> [[1]]$associatedDiseases$rows[[5]]$datatypeScores[[1]]$score
#> [1] 0.1667585
#> 
#> 
#> [[1]]$associatedDiseases$rows[[5]]$datatypeScores[[2]]
#> [[1]]$associatedDiseases$rows[[5]]$datatypeScores[[2]]$id
#> [1] "genetic_association"
#> 
#> [[1]]$associatedDiseases$rows[[5]]$datatypeScores[[2]]$score
#> [1] 0.6079308
#> 
#> 
#> [[1]]$associatedDiseases$rows[[5]]$datatypeScores[[3]]
#> [[1]]$associatedDiseases$rows[[5]]$datatypeScores[[3]]$id
#> [1] "genetic_literature"
#> 
#> [[1]]$associatedDiseases$rows[[5]]$datatypeScores[[3]]$score
#> [1] 0.8274614
#> 
#> 
#> 
#> [[1]]$associatedDiseases$rows[[5]]$disease
#> [[1]]$associatedDiseases$rows[[5]]$disease$id
#> [1] "EFO_0700130"
#> 
#> [[1]]$associatedDiseases$rows[[5]]$disease$name
#> [1] "ovarioleukodystrophy"
#> 
#> [[1]]$associatedDiseases$rows[[5]]$disease$description
#> [1] "The �ovarioleukodystrophies� comprise a group of rare leukodystrophies associated with primary or premature ovarian failure."
#> 
#> [[1]]$associatedDiseases$rows[[5]]$disease$therapeuticAreas
#> [[1]]$associatedDiseases$rows[[5]]$disease$therapeuticAreas[[1]]
#> [[1]]$associatedDiseases$rows[[5]]$disease$therapeuticAreas[[1]]$name
#> [1] "genetic, familial or congenital disease"
#> 
#> 
#> [[1]]$associatedDiseases$rows[[5]]$disease$therapeuticAreas[[2]]
#> [[1]]$associatedDiseases$rows[[5]]$disease$therapeuticAreas[[2]]$name
#> [1] "disorder of visual system"
#> 
#> 
#> 
#> 
#> 
#> [[1]]$associatedDiseases$rows[[6]]
#> [[1]]$associatedDiseases$rows[[6]]$score
#> [1] 0.5507284
#> 
#> [[1]]$associatedDiseases$rows[[6]]$datatypeScores
#> [[1]]$associatedDiseases$rows[[6]]$datatypeScores[[1]]
#> [[1]]$associatedDiseases$rows[[6]]$datatypeScores[[1]]$id
#> [1] "literature"
#> 
#> [[1]]$associatedDiseases$rows[[6]]$datatypeScores[[1]]$score
#> [1] 0.08032163
#> 
#> 
#> [[1]]$associatedDiseases$rows[[6]]$datatypeScores[[2]]
#> [[1]]$associatedDiseases$rows[[6]]$datatypeScores[[2]]$id
#> [1] "affected_pathway"
#> 
#> [[1]]$associatedDiseases$rows[[6]]$datatypeScores[[2]]$score
#> [1] 0.9018902
#> 
#> 
#> 
#> [[1]]$associatedDiseases$rows[[6]]$disease
#> [[1]]$associatedDiseases$rows[[6]]$disease$id
#> [1] "EFO_0005772"
#> 
#> [[1]]$associatedDiseases$rows[[6]]$disease$name
#> [1] "neurodegenerative disease"
#> 
#> [[1]]$associatedDiseases$rows[[6]]$disease$description
#> [1] "A disorder of the central nervous system characterized by gradual and progressive loss of neural tissue and neurologic function."
#> 
#> [[1]]$associatedDiseases$rows[[6]]$disease$therapeuticAreas
#> [[1]]$associatedDiseases$rows[[6]]$disease$therapeuticAreas[[1]]
#> [[1]]$associatedDiseases$rows[[6]]$disease$therapeuticAreas[[1]]$name
#> [1] "nervous system disease"
#> 
#> 
#> 
#> 
#> 
#> [[1]]$associatedDiseases$rows[[7]]
#> [[1]]$associatedDiseases$rows[[7]]$score
#> [1] 0.4161469
#> 
#> [[1]]$associatedDiseases$rows[[7]]$datatypeScores
#> [[1]]$associatedDiseases$rows[[7]]$datatypeScores[[1]]
#> [[1]]$associatedDiseases$rows[[7]]$datatypeScores[[1]]$id
#> [1] "literature"
#> 
#> [[1]]$associatedDiseases$rows[[7]]$datatypeScores[[1]]$score
#> [1] 0.01215862
#> 
#> 
#> [[1]]$associatedDiseases$rows[[7]]$datatypeScores[[2]]
#> [[1]]$associatedDiseases$rows[[7]]$datatypeScores[[2]]$id
#> [1] "genetic_association"
#> 
#> [[1]]$associatedDiseases$rows[[7]]$datatypeScores[[2]]$score
#> [1] 0.6839221
#> 
#> 
#> 
#> [[1]]$associatedDiseases$rows[[7]]$disease
#> [[1]]$associatedDiseases$rows[[7]]$disease$id
#> [1] "HP_0008209"
#> 
#> [[1]]$associatedDiseases$rows[[7]]$disease$name
#> [1] "Premature ovarian insufficiency"
#> 
#> [[1]]$associatedDiseases$rows[[7]]$disease$description
#> [1] "Amenorrhea due to loss of ovarian function before the age of 40. Primary ovarian insuficiency (POI) is a state of female hypergonadotropic hypogonadism. It can manifest as primary amenorrhea with onset before menarche or secondary amenorrhea."
#> 
#> [[1]]$associatedDiseases$rows[[7]]$disease$therapeuticAreas
#> [[1]]$associatedDiseases$rows[[7]]$disease$therapeuticAreas[[1]]
#> [[1]]$associatedDiseases$rows[[7]]$disease$therapeuticAreas[[1]]$name
#> [1] "phenotype"
#> 
#> 
#> 
#> 
#> 
#> [[1]]$associatedDiseases$rows[[8]]
#> [[1]]$associatedDiseases$rows[[8]]$score
#> [1] 0.3695799
#> 
#> [[1]]$associatedDiseases$rows[[8]]$datatypeScores
#> [[1]]$associatedDiseases$rows[[8]]$datatypeScores[[1]]
#> [[1]]$associatedDiseases$rows[[8]]$datatypeScores[[1]]$id
#> [1] "genetic_association"
#> 
#> [[1]]$associatedDiseases$rows[[8]]$datatypeScores[[1]]$score
#> [1] 0.6079308
#> 
#> 
#> 
#> [[1]]$associatedDiseases$rows[[8]]$disease
#> [[1]]$associatedDiseases$rows[[8]]$disease$id
#> [1] "MONDO_0015520"
#> 
#> [[1]]$associatedDiseases$rows[[8]]$disease$name
#> [1] "late infantile CACH syndrome"
#> 
#> [[1]]$associatedDiseases$rows[[8]]$disease$description
#> NULL
#> 
#> [[1]]$associatedDiseases$rows[[8]]$disease$therapeuticAreas
#> [[1]]$associatedDiseases$rows[[8]]$disease$therapeuticAreas[[1]]
#> [[1]]$associatedDiseases$rows[[8]]$disease$therapeuticAreas[[1]]$name
#> [1] "nervous system disease"
#> 
#> 
#> [[1]]$associatedDiseases$rows[[8]]$disease$therapeuticAreas[[2]]
#> [[1]]$associatedDiseases$rows[[8]]$disease$therapeuticAreas[[2]]$name
#> [1] "genetic, familial or congenital disease"
#> 
#> 
#> 
#> 
#> 
#> [[1]]$associatedDiseases$rows[[9]]
#> [[1]]$associatedDiseases$rows[[9]]$score
#> [1] 0.3695799
#> 
#> [[1]]$associatedDiseases$rows[[9]]$datatypeScores
#> [[1]]$associatedDiseases$rows[[9]]$datatypeScores[[1]]
#> [[1]]$associatedDiseases$rows[[9]]$datatypeScores[[1]]$id
#> [1] "genetic_association"
#> 
#> [[1]]$associatedDiseases$rows[[9]]$datatypeScores[[1]]$score
#> [1] 0.6079308
#> 
#> 
#> 
#> [[1]]$associatedDiseases$rows[[9]]$disease
#> [[1]]$associatedDiseases$rows[[9]]$disease$id
#> [1] "MONDO_0015521"
#> 
#> [[1]]$associatedDiseases$rows[[9]]$disease$name
#> [1] "juvenile or adult CACH syndrome"
#> 
#> [[1]]$associatedDiseases$rows[[9]]$disease$description
#> NULL
#> 
#> [[1]]$associatedDiseases$rows[[9]]$disease$therapeuticAreas
#> [[1]]$associatedDiseases$rows[[9]]$disease$therapeuticAreas[[1]]
#> [[1]]$associatedDiseases$rows[[9]]$disease$therapeuticAreas[[1]]$name
#> [1] "nervous system disease"
#> 
#> 
#> [[1]]$associatedDiseases$rows[[9]]$disease$therapeuticAreas[[2]]
#> [[1]]$associatedDiseases$rows[[9]]$disease$therapeuticAreas[[2]]$name
#> [1] "genetic, familial or congenital disease"
#> 
#> 
#> 
#> 
#> 
#> [[1]]$associatedDiseases$rows[[10]]
#> [[1]]$associatedDiseases$rows[[10]]$score
#> [1] 0.3695799
#> 
#> [[1]]$associatedDiseases$rows[[10]]$datatypeScores
#> [[1]]$associatedDiseases$rows[[10]]$datatypeScores[[1]]
#> [[1]]$associatedDiseases$rows[[10]]$datatypeScores[[1]]$id
#> [1] "genetic_association"
#> 
#> [[1]]$associatedDiseases$rows[[10]]$datatypeScores[[1]]$score
#> [1] 0.6079308
#> 
#> 
#> 
#> [[1]]$associatedDiseases$rows[[10]]$disease
#> [[1]]$associatedDiseases$rows[[10]]$disease$id
#> [1] "MONDO_0015519"
#> 
#> [[1]]$associatedDiseases$rows[[10]]$disease$name
#> [1] "congenital or early infantile CACH syndrome"
#> 
#> [[1]]$associatedDiseases$rows[[10]]$disease$description
#> NULL
#> 
#> [[1]]$associatedDiseases$rows[[10]]$disease$therapeuticAreas
#> [[1]]$associatedDiseases$rows[[10]]$disease$therapeuticAreas[[1]]
#> [[1]]$associatedDiseases$rows[[10]]$disease$therapeuticAreas[[1]]$name
#> [1] "nervous system disease"
#> 
#> 
#> [[1]]$associatedDiseases$rows[[10]]$disease$therapeuticAreas[[2]]
#> [[1]]$associatedDiseases$rows[[10]]$disease$therapeuticAreas[[2]]$name
#> [1] "genetic, familial or congenital disease"
#> 
#> 
#> 
#> 
#> 
#> [[1]]$associatedDiseases$rows[[11]]
#> [[1]]$associatedDiseases$rows[[11]]$score
#> [1] 0.3400135
#> 
#> [[1]]$associatedDiseases$rows[[11]]$datatypeScores
#> [[1]]$associatedDiseases$rows[[11]]$datatypeScores[[1]]
#> [[1]]$associatedDiseases$rows[[11]]$datatypeScores[[1]]$id
#> [1] "genetic_association"
#> 
#> [[1]]$associatedDiseases$rows[[11]]$datatypeScores[[1]]$score
#> [1] 0.5592963
#> 
#> 
#> 
#> [[1]]$associatedDiseases$rows[[11]]$disease
#> [[1]]$associatedDiseases$rows[[11]]$disease$id
#> [1] "MONDO_0019019"
#> 
#> [[1]]$associatedDiseases$rows[[11]]$disease$name
#> [1] "osteogenesis imperfecta"
#> 
#> [[1]]$associatedDiseases$rows[[11]]$disease$description
#> [1] "Osteogenesis imperfecta (OI) comprises a heterogeneous group of genetic disorders characterized by increased bone fragility, low bone mass, and susceptibility to bone fractures with variable severity."
#> 
#> [[1]]$associatedDiseases$rows[[11]]$disease$therapeuticAreas
#> [[1]]$associatedDiseases$rows[[11]]$disease$therapeuticAreas[[1]]
#> [[1]]$associatedDiseases$rows[[11]]$disease$therapeuticAreas[[1]]$name
#> [1] "genetic, familial or congenital disease"
#> 
#> 
#> [[1]]$associatedDiseases$rows[[11]]$disease$therapeuticAreas[[2]]
#> [[1]]$associatedDiseases$rows[[11]]$disease$therapeuticAreas[[2]]$name
#> [1] "musculoskeletal or connective tissue disease"
#> 
#> 
#> [[1]]$associatedDiseases$rows[[11]]$disease$therapeuticAreas[[3]]
#> [[1]]$associatedDiseases$rows[[11]]$disease$therapeuticAreas[[3]]$name
#> [1] "phenotype"
#> 
#> 
#> 
#> 
#> 
#> [[1]]$associatedDiseases$rows[[12]]
#> [[1]]$associatedDiseases$rows[[12]]$score
#> [1] 0.3393817
#> 
#> [[1]]$associatedDiseases$rows[[12]]$datatypeScores
#> [[1]]$associatedDiseases$rows[[12]]$datatypeScores[[1]]
#> [[1]]$associatedDiseases$rows[[12]]$datatypeScores[[1]]$id
#> [1] "genetic_association"
#> 
#> [[1]]$associatedDiseases$rows[[12]]$datatypeScores[[1]]$score
#> [1] 0.5582571
#> 
#> 
#> 
#> [[1]]$associatedDiseases$rows[[12]]$disease
#> [[1]]$associatedDiseases$rows[[12]]$disease$id
#> [1] "EFO_0000508"
#> 
#> [[1]]$associatedDiseases$rows[[12]]$disease$name
#> [1] "genetic disorder"
#> 
#> [[1]]$associatedDiseases$rows[[12]]$disease$description
#> [1] "Genetic diseases are diseases in which inherited genes predispose to increased risk. The genetic disorders associated with cancer often result from an alteration or mutation in a single gene. The diseases range from rare dominant cancer family syndrome to familial tendencies in which low-penetrance genes may interact with other genes or environmental factors to induce cancer. Research may involve clinical, epidemiologic, and laboratory studies of persons, families, and populations at high risk of these disorders."
#> 
#> [[1]]$associatedDiseases$rows[[12]]$disease$therapeuticAreas
#> [[1]]$associatedDiseases$rows[[12]]$disease$therapeuticAreas[[1]]
#> [[1]]$associatedDiseases$rows[[12]]$disease$therapeuticAreas[[1]]$name
#> [1] "genetic, familial or congenital disease"
#> 
#> 
#> 
#> 
#> 
#> [[1]]$associatedDiseases$rows[[13]]
#> [[1]]$associatedDiseases$rows[[13]]$score
#> [1] 0.3326219
#> 
#> [[1]]$associatedDiseases$rows[[13]]$datatypeScores
#> [[1]]$associatedDiseases$rows[[13]]$datatypeScores[[1]]
#> [[1]]$associatedDiseases$rows[[13]]$datatypeScores[[1]]$id
#> [1] "genetic_association"
#> 
#> [[1]]$associatedDiseases$rows[[13]]$datatypeScores[[1]]$score
#> [1] 0.5471377
#> 
#> 
#> 
#> [[1]]$associatedDiseases$rows[[13]]$disease
#> [[1]]$associatedDiseases$rows[[13]]$disease$id
#> [1] "HP_0000707"
#> 
#> [[1]]$associatedDiseases$rows[[13]]$disease$name
#> [1] "Abnormality of the nervous system"
#> 
#> [[1]]$associatedDiseases$rows[[13]]$disease$description
#> [1] "An abnormality of the nervous system."
#> 
#> [[1]]$associatedDiseases$rows[[13]]$disease$therapeuticAreas
#> [[1]]$associatedDiseases$rows[[13]]$disease$therapeuticAreas[[1]]
#> [[1]]$associatedDiseases$rows[[13]]$disease$therapeuticAreas[[1]]$name
#> [1] "phenotype"
#> 
#> 
#> 
#> 
#> 
#> [[1]]$associatedDiseases$rows[[14]]
#> [[1]]$associatedDiseases$rows[[14]]$score
#> [1] 0.3294746
#> 
#> [[1]]$associatedDiseases$rows[[14]]$datatypeScores
#> [[1]]$associatedDiseases$rows[[14]]$datatypeScores[[1]]
#> [[1]]$associatedDiseases$rows[[14]]$datatypeScores[[1]]$id
#> [1] "literature"
#> 
#> [[1]]$associatedDiseases$rows[[14]]$datatypeScores[[1]]$score
#> [1] 0.0729517
#> 
#> 
#> [[1]]$associatedDiseases$rows[[14]]$datatypeScores[[2]]
#> [[1]]$associatedDiseases$rows[[14]]$datatypeScores[[2]]$id
#> [1] "genetic_association"
#> 
#> [[1]]$associatedDiseases$rows[[14]]$datatypeScores[[2]]$score
#> [1] 0.538313
#> 
#> 
#> 
#> [[1]]$associatedDiseases$rows[[14]]$disease
#> [[1]]$associatedDiseases$rows[[14]]$disease$id
#> [1] "EFO_0001645"
#> 
#> [[1]]$associatedDiseases$rows[[14]]$disease$name
#> [1] "coronary artery disease"
#> 
#> [[1]]$associatedDiseases$rows[[14]]$disease$description
#> [1] "Thickening and loss of elasticity of the CORONARY ARTERIES, leading to progressive arterial insufficiency (CORONARY DISEASE)."
#> 
#> [[1]]$associatedDiseases$rows[[14]]$disease$therapeuticAreas
#> [[1]]$associatedDiseases$rows[[14]]$disease$therapeuticAreas[[1]]
#> [[1]]$associatedDiseases$rows[[14]]$disease$therapeuticAreas[[1]]$name
#> [1] "cardiovascular disease"
#> 
#> 
#> 
#> 
#> 
#> [[1]]$associatedDiseases$rows[[15]]
#> [[1]]$associatedDiseases$rows[[15]]$score
#> [1] 0.2878425
#> 
#> [[1]]$associatedDiseases$rows[[15]]$datatypeScores
#> [[1]]$associatedDiseases$rows[[15]]$datatypeScores[[1]]
#> [[1]]$associatedDiseases$rows[[15]]$datatypeScores[[1]]$id
#> [1] "affected_pathway"
#> 
#> [[1]]$associatedDiseases$rows[[15]]$datatypeScores[[1]]$score
#> [1] 0.473479
#> 
#> 
#> 
#> [[1]]$associatedDiseases$rows[[15]]$disease
#> [[1]]$associatedDiseases$rows[[15]]$disease$id
#> [1] "MONDO_0002561"
#> 
#> [[1]]$associatedDiseases$rows[[15]]$disease$name
#> [1] "lysosomal storage disease"
#> 
#> [[1]]$associatedDiseases$rows[[15]]$disease$description
#> [1] "A metabolic disorder caused by mutations in proteins critical for lysosomal function, including lysosomal enzymes, lysosomal integral membrane proteins, and proteins involved in the post-translational modification and trafficking of lysosomal proteins."
#> 
#> [[1]]$associatedDiseases$rows[[15]]$disease$therapeuticAreas
#> [[1]]$associatedDiseases$rows[[15]]$disease$therapeuticAreas[[1]]
#> [[1]]$associatedDiseases$rows[[15]]$disease$therapeuticAreas[[1]]$name
#> [1] "genetic, familial or congenital disease"
#> 
#> 
#> [[1]]$associatedDiseases$rows[[15]]$disease$therapeuticAreas[[2]]
#> [[1]]$associatedDiseases$rows[[15]]$disease$therapeuticAreas[[2]]$name
#> [1] "nutritional or metabolic disease"
#> 
#> 
#> 
#> 
#> 
#> [[1]]$associatedDiseases$rows[[16]]
#> [[1]]$associatedDiseases$rows[[16]]$score
#> [1] 0.2301928
#> 
#> [[1]]$associatedDiseases$rows[[16]]$datatypeScores
#> [[1]]$associatedDiseases$rows[[16]]$datatypeScores[[1]]
#> [[1]]$associatedDiseases$rows[[16]]$datatypeScores[[1]]$id
#> [1] "genetic_association"
#> 
#> [[1]]$associatedDiseases$rows[[16]]$datatypeScores[[1]]$score
#> [1] 0.3786497
#> 
#> 
#> 
#> [[1]]$associatedDiseases$rows[[16]]$disease
#> [[1]]$associatedDiseases$rows[[16]]$disease$id
#> [1] "EFO_0005422"
#> 
#> [[1]]$associatedDiseases$rows[[16]]$disease$name
#> [1] "skin aging"
#> 
#> [[1]]$associatedDiseases$rows[[16]]$disease$description
#> [1] "The gradual irreversible changes in structure of skin that occur as a result of the passage of time.. In humans, skin aging can be precipitated as a result of weather and sun exposure, and expresses through the appearance of wrinkles and localised changes in skin pigmentation"
#> 
#> [[1]]$associatedDiseases$rows[[16]]$disease$therapeuticAreas
#> [[1]]$associatedDiseases$rows[[16]]$disease$therapeuticAreas[[1]]
#> [[1]]$associatedDiseases$rows[[16]]$disease$therapeuticAreas[[1]]$name
#> [1] "biological_process"
#> 
#> 
#> 
#> 
#> 
#> [[1]]$associatedDiseases$rows[[17]]
#> [[1]]$associatedDiseases$rows[[17]]$score
#> [1] 0.1291967
#> 
#> [[1]]$associatedDiseases$rows[[17]]$datatypeScores
#> [[1]]$associatedDiseases$rows[[17]]$datatypeScores[[1]]
#> [[1]]$associatedDiseases$rows[[17]]$datatypeScores[[1]]$id
#> [1] "genetic_association"
#> 
#> [[1]]$associatedDiseases$rows[[17]]$datatypeScores[[1]]$score
#> [1] 0.2125187
#> 
#> 
#> 
#> [[1]]$associatedDiseases$rows[[17]]$disease
#> [[1]]$associatedDiseases$rows[[17]]$disease$id
#> [1] "HP_0000924"
#> 
#> [[1]]$associatedDiseases$rows[[17]]$disease$name
#> [1] "Abnormality of the skeletal system"
#> 
#> [[1]]$associatedDiseases$rows[[17]]$disease$description
#> [1] "An abnormality of the skeletal system."
#> 
#> [[1]]$associatedDiseases$rows[[17]]$disease$therapeuticAreas
#> [[1]]$associatedDiseases$rows[[17]]$disease$therapeuticAreas[[1]]
#> [[1]]$associatedDiseases$rows[[17]]$disease$therapeuticAreas[[1]]$name
#> [1] "phenotype"
#> 
#> 
#> 
#> 
#> 
#> [[1]]$associatedDiseases$rows[[18]]
#> [[1]]$associatedDiseases$rows[[18]]$score
#> [1] 0.0819928
#> 
#> [[1]]$associatedDiseases$rows[[18]]$datatypeScores
#> [[1]]$associatedDiseases$rows[[18]]$datatypeScores[[1]]
#> [[1]]$associatedDiseases$rows[[18]]$datatypeScores[[1]]$id
#> [1] "genetic_association"
#> 
#> [[1]]$associatedDiseases$rows[[18]]$datatypeScores[[1]]$score
#> [1] 0.1348719
#> 
#> 
#> 
#> [[1]]$associatedDiseases$rows[[18]]$disease
#> [[1]]$associatedDiseases$rows[[18]]$disease$id
#> [1] "EFO_0004213"
#> 
#> [[1]]$associatedDiseases$rows[[18]]$disease$name
#> [1] "otosclerosis"
#> 
#> [[1]]$associatedDiseases$rows[[18]]$disease$description
#> [1] "Formation of spongy bone in the labyrinth capsule which can progress toward the stapes (stapedial fixation) or anteriorly toward the cochlea leading to conductive, sensorineural, or mixed hearing loss. Several genes are associated with familial otosclerosis with varied clinical signs."
#> 
#> [[1]]$associatedDiseases$rows[[18]]$disease$therapeuticAreas
#> [[1]]$associatedDiseases$rows[[18]]$disease$therapeuticAreas[[1]]
#> [[1]]$associatedDiseases$rows[[18]]$disease$therapeuticAreas[[1]]$name
#> [1] "disorder of ear"
#> 
#> 
#> [[1]]$associatedDiseases$rows[[18]]$disease$therapeuticAreas[[2]]
#> [[1]]$associatedDiseases$rows[[18]]$disease$therapeuticAreas[[2]]$name
#> [1] "nervous system disease"
#> 
#> 
#> [[1]]$associatedDiseases$rows[[18]]$disease$therapeuticAreas[[3]]
#> [[1]]$associatedDiseases$rows[[18]]$disease$therapeuticAreas[[3]]$name
#> [1] "genetic, familial or congenital disease"
#> 
#> 
#> 
#> 
#> 
#> [[1]]$associatedDiseases$rows[[19]]
#> [[1]]$associatedDiseases$rows[[19]]$score
#> [1] 0.06788977
#> 
#> [[1]]$associatedDiseases$rows[[19]]$datatypeScores
#> [[1]]$associatedDiseases$rows[[19]]$datatypeScores[[1]]
#> [[1]]$associatedDiseases$rows[[19]]$datatypeScores[[1]]$id
#> [1] "literature"
#> 
#> [[1]]$associatedDiseases$rows[[19]]$datatypeScores[[1]]$score
#> [1] 0.5583676
#> 
#> 
#> [[1]]$associatedDiseases$rows[[19]]$datatypeScores[[2]]
#> [[1]]$associatedDiseases$rows[[19]]$datatypeScores[[2]]$id
#> [1] "genetic_association"
#> 
#> [[1]]$associatedDiseases$rows[[19]]$datatypeScores[[2]]$score
#> [1] 0
#> 
#> 
#> 
#> [[1]]$associatedDiseases$rows[[19]]$disease
#> [[1]]$associatedDiseases$rows[[19]]$disease$id
#> [1] "MONDO_0008903"
#> 
#> [[1]]$associatedDiseases$rows[[19]]$disease$name
#> [1] "lung cancer"
#> 
#> [[1]]$associatedDiseases$rows[[19]]$disease$description
#> [1] "A malignant neoplasm involving the lung."
#> 
#> [[1]]$associatedDiseases$rows[[19]]$disease$therapeuticAreas
#> [[1]]$associatedDiseases$rows[[19]]$disease$therapeuticAreas[[1]]
#> [[1]]$associatedDiseases$rows[[19]]$disease$therapeuticAreas[[1]]$name
#> [1] "respiratory or thoracic disease"
#> 
#> 
#> [[1]]$associatedDiseases$rows[[19]]$disease$therapeuticAreas[[2]]
#> [[1]]$associatedDiseases$rows[[19]]$disease$therapeuticAreas[[2]]$name
#> [1] "cancer or benign tumor"
#> 
#> 
#> 
#> 
#> 
#> [[1]]$associatedDiseases$rows[[20]]
#> [[1]]$associatedDiseases$rows[[20]]$score
#> [1] 0.06660039
#> 
#> [[1]]$associatedDiseases$rows[[20]]$datatypeScores
#> [[1]]$associatedDiseases$rows[[20]]$datatypeScores[[1]]
#> [[1]]$associatedDiseases$rows[[20]]$datatypeScores[[1]]$id
#> [1] "genetic_association"
#> 
#> [[1]]$associatedDiseases$rows[[20]]$datatypeScores[[1]]$score
#> [1] 0.1095526
#> 
#> 
#> 
#> [[1]]$associatedDiseases$rows[[20]]$disease
#> [[1]]$associatedDiseases$rows[[20]]$disease$id
#> [1] "EFO_0009959"
#> 
#> [[1]]$associatedDiseases$rows[[20]]$disease$name
#> [1] "diverticular disease"
#> 
#> [[1]]$associatedDiseases$rows[[20]]$disease$description
#> [1] "A complex disorder characterised by mucosal outpouchings (diverticulae) of the colonic wall which can become infected and inflamed leading to diverticulitis, perforation and bleeding."
#> 
#> [[1]]$associatedDiseases$rows[[20]]$disease$therapeuticAreas
#> [[1]]$associatedDiseases$rows[[20]]$disease$therapeuticAreas[[1]]
#> [[1]]$associatedDiseases$rows[[20]]$disease$therapeuticAreas[[1]]$name
#> [1] "gastrointestinal disease"
#> 
#> 
#> 
#> 
#> 
#> [[1]]$associatedDiseases$rows[[21]]
#> [[1]]$associatedDiseases$rows[[21]]$score
#> [1] 0.05878022
#> 
#> [[1]]$associatedDiseases$rows[[21]]$datatypeScores
#> [[1]]$associatedDiseases$rows[[21]]$datatypeScores[[1]]
#> [[1]]$associatedDiseases$rows[[21]]$datatypeScores[[1]]$id
#> [1] "genetic_association"
#> 
#> [[1]]$associatedDiseases$rows[[21]]$datatypeScores[[1]]$score
#> [1] 0.096689
#> 
#> 
#> 
#> [[1]]$associatedDiseases$rows[[21]]$disease
#> [[1]]$associatedDiseases$rows[[21]]$disease$id
#> [1] "EFO_0004214"
#> 
#> [[1]]$associatedDiseases$rows[[21]]$disease$name
#> [1] "Abdominal Aortic Aneurysm"
#> 
#> [[1]]$associatedDiseases$rows[[21]]$disease$description
#> [1] "Enlargement and ballooning of the vessel that supplies arterial blood to the abdomen, pelvis and legs."
#> 
#> [[1]]$associatedDiseases$rows[[21]]$disease$therapeuticAreas
#> [[1]]$associatedDiseases$rows[[21]]$disease$therapeuticAreas[[1]]
#> [[1]]$associatedDiseases$rows[[21]]$disease$therapeuticAreas[[1]]$name
#> [1] "cardiovascular disease"
#> 
#> 
#> 
#> 
#> 
#> [[1]]$associatedDiseases$rows[[22]]
#> [[1]]$associatedDiseases$rows[[22]]$score
#> [1] 0.0532001
#> 
#> [[1]]$associatedDiseases$rows[[22]]$datatypeScores
#> [[1]]$associatedDiseases$rows[[22]]$datatypeScores[[1]]
#> [[1]]$associatedDiseases$rows[[22]]$datatypeScores[[1]]$id
#> [1] "genetic_association"
#> 
#> [[1]]$associatedDiseases$rows[[22]]$datatypeScores[[1]]$score
#> [1] 0.08751012
#> 
#> 
#> 
#> [[1]]$associatedDiseases$rows[[22]]$disease
#> [[1]]$associatedDiseases$rows[[22]]$disease$id
#> [1] "MONDO_0021201"
#> 
#> [[1]]$associatedDiseases$rows[[22]]$disease$name
#> [1] "skin infection"
#> 
#> [[1]]$associatedDiseases$rows[[22]]$disease$description
#> [1] "An inflammatory process affecting the skin, caused by bacteria, viruses, parasites, or fungi. Examples of bacterial infection include carbuncles, furuncles, impetigo, erysipelas, and abscesses. Examples of viral infection include shingles, warts, molluscum contagiosum, and pityriasis rosea. Examples of parasitic infection include scabies and lice. Examples of fungal infection include athlete's foot, yeast infection, and ringworm."
#> 
#> [[1]]$associatedDiseases$rows[[22]]$disease$therapeuticAreas
#> [[1]]$associatedDiseases$rows[[22]]$disease$therapeuticAreas[[1]]
#> [[1]]$associatedDiseases$rows[[22]]$disease$therapeuticAreas[[1]]$name
#> [1] "infectious disease"
#> 
#> 
#> [[1]]$associatedDiseases$rows[[22]]$disease$therapeuticAreas[[2]]
#> [[1]]$associatedDiseases$rows[[22]]$disease$therapeuticAreas[[2]]$name
#> [1] "integumentary system disease"
#> 
#> 
#> 
#> 
#> 
#> [[1]]$associatedDiseases$rows[[23]]
#> [[1]]$associatedDiseases$rows[[23]]$score
#> [1] 0.0532001
#> 
#> [[1]]$associatedDiseases$rows[[23]]$datatypeScores
#> [[1]]$associatedDiseases$rows[[23]]$datatypeScores[[1]]
#> [[1]]$associatedDiseases$rows[[23]]$datatypeScores[[1]]$id
#> [1] "genetic_association"
#> 
#> [[1]]$associatedDiseases$rows[[23]]$datatypeScores[[1]]$score
#> [1] 0.08751012
#> 
#> 
#> 
#> [[1]]$associatedDiseases$rows[[23]]$disease
#> [[1]]$associatedDiseases$rows[[23]]$disease$id
#> [1] "EFO_0010692"
#> 
#> [[1]]$associatedDiseases$rows[[23]]$disease$name
#> [1] "subcutaneous tissue infection"
#> 
#> [[1]]$associatedDiseases$rows[[23]]$disease$description
#> [1] "Any infection of the subcutaneous tissue."
#> 
#> [[1]]$associatedDiseases$rows[[23]]$disease$therapeuticAreas
#> [[1]]$associatedDiseases$rows[[23]]$disease$therapeuticAreas[[1]]
#> [[1]]$associatedDiseases$rows[[23]]$disease$therapeuticAreas[[1]]$name
#> [1] "infectious disease"
#> 
#> 
#> 
#> 
#> 
#> [[1]]$associatedDiseases$rows[[24]]
#> [[1]]$associatedDiseases$rows[[24]]$score
#> [1] 0.04743968
#> 
#> [[1]]$associatedDiseases$rows[[24]]$datatypeScores
#> [[1]]$associatedDiseases$rows[[24]]$datatypeScores[[1]]
#> [[1]]$associatedDiseases$rows[[24]]$datatypeScores[[1]]$id
#> [1] "literature"
#> 
#> [[1]]$associatedDiseases$rows[[24]]$datatypeScores[[1]]$score
#> [1] 0.3901734
#> 
#> 
#> 
#> [[1]]$associatedDiseases$rows[[24]]$disease
#> [[1]]$associatedDiseases$rows[[24]]$disease$id
#> [1] "EFO_1001951"
#> 
#> [[1]]$associatedDiseases$rows[[24]]$disease$name
#> [1] "colorectal carcinoma"
#> 
#> [[1]]$associatedDiseases$rows[[24]]$disease$description
#> [1] "A malignant epithelial neoplasm that arises from the colon or rectum and invades through the muscularis mucosa into the submucosa. The vast majority are adenocarcinomas."
#> 
#> [[1]]$associatedDiseases$rows[[24]]$disease$therapeuticAreas
#> [[1]]$associatedDiseases$rows[[24]]$disease$therapeuticAreas[[1]]
#> [[1]]$associatedDiseases$rows[[24]]$disease$therapeuticAreas[[1]]$name
#> [1] "gastrointestinal disease"
#> 
#> 
#> [[1]]$associatedDiseases$rows[[24]]$disease$therapeuticAreas[[2]]
#> [[1]]$associatedDiseases$rows[[24]]$disease$therapeuticAreas[[2]]$name
#> [1] "cancer or benign tumor"
#> 
#> 
#> 
#> 
#> 
#> [[1]]$associatedDiseases$rows[[25]]
#> [[1]]$associatedDiseases$rows[[25]]$score
#> [1] 0.04342563
#> 
#> [[1]]$associatedDiseases$rows[[25]]$datatypeScores
#> [[1]]$associatedDiseases$rows[[25]]$datatypeScores[[1]]
#> [[1]]$associatedDiseases$rows[[25]]$datatypeScores[[1]]$id
#> [1] "literature"
#> 
#> [[1]]$associatedDiseases$rows[[25]]$datatypeScores[[1]]$score
#> [1] 0.3571593
#> 
#> 
#> 
#> [[1]]$associatedDiseases$rows[[25]]$disease
#> [[1]]$associatedDiseases$rows[[25]]$disease$id
#> [1] "EFO_0000571"
#> 
#> [[1]]$associatedDiseases$rows[[25]]$disease$name
#> [1] "lung adenocarcinoma"
#> 
#> [[1]]$associatedDiseases$rows[[25]]$disease$description
#> [1] "A carcinoma that arises from the lung and is characterized by the presence of malignant glandular epithelial cells. There is a male predilection with a male to female ratio of 2:1. Usually lung adenocarcinoma is asymptomatic and is identified through screening studies or as an incidental radiologic finding. If clinical symptoms are present they include shortness of breath, cough, hemoptysis, chest pain, and fever. Tobacco smoke is a known risk factor."
#> 
#> [[1]]$associatedDiseases$rows[[25]]$disease$therapeuticAreas
#> [[1]]$associatedDiseases$rows[[25]]$disease$therapeuticAreas[[1]]
#> [[1]]$associatedDiseases$rows[[25]]$disease$therapeuticAreas[[1]]$name
#> [1] "respiratory or thoracic disease"
#> 
#> 
#> [[1]]$associatedDiseases$rows[[25]]$disease$therapeuticAreas[[2]]
#> [[1]]$associatedDiseases$rows[[25]]$disease$therapeuticAreas[[2]]$name
#> [1] "cancer or benign tumor"
#> 
#> 
#> 
#> 
#> 
#> 
#> 
#> [[1]]$tractability
#> [[1]]$tractability[[1]]
#> [[1]]$tractability[[1]]$label
#> [1] "Approved Drug"
#> 
#> [[1]]$tractability[[1]]$modality
#> [1] "SM"
#> 
#> [[1]]$tractability[[1]]$value
#> [1] FALSE
#> 
#> 
#> [[1]]$tractability[[2]]
#> [[1]]$tractability[[2]]$label
#> [1] "Advanced Clinical"
#> 
#> [[1]]$tractability[[2]]$modality
#> [1] "SM"
#> 
#> [[1]]$tractability[[2]]$value
#> [1] FALSE
#> 
#> 
#> [[1]]$tractability[[3]]
#> [[1]]$tractability[[3]]$label
#> [1] "Phase 1 Clinical"
#> 
#> [[1]]$tractability[[3]]$modality
#> [1] "SM"
#> 
#> [[1]]$tractability[[3]]$value
#> [1] FALSE
#> 
#> 
#> [[1]]$tractability[[4]]
#> [[1]]$tractability[[4]]$label
#> [1] "Structure with Ligand"
#> 
#> [[1]]$tractability[[4]]$modality
#> [1] "SM"
#> 
#> [[1]]$tractability[[4]]$value
#> [1] TRUE
#> 
#> 
#> [[1]]$tractability[[5]]
#> [[1]]$tractability[[5]]$label
#> [1] "High-Quality Ligand"
#> 
#> [[1]]$tractability[[5]]$modality
#> [1] "SM"
#> 
#> [[1]]$tractability[[5]]$value
#> [1] FALSE
#> 
#> 
#> [[1]]$tractability[[6]]
#> [[1]]$tractability[[6]]$label
#> [1] "High-Quality Pocket"
#> 
#> [[1]]$tractability[[6]]$modality
#> [1] "SM"
#> 
#> [[1]]$tractability[[6]]$value
#> [1] FALSE
#> 
#> 
#> [[1]]$tractability[[7]]
#> [[1]]$tractability[[7]]$label
#> [1] "Med-Quality Pocket"
#> 
#> [[1]]$tractability[[7]]$modality
#> [1] "SM"
#> 
#> [[1]]$tractability[[7]]$value
#> [1] FALSE
#> 
#> 
#> [[1]]$tractability[[8]]
#> [[1]]$tractability[[8]]$label
#> [1] "Druggable Family"
#> 
#> [[1]]$tractability[[8]]$modality
#> [1] "SM"
#> 
#> [[1]]$tractability[[8]]$value
#> [1] FALSE
#> 
#> 
#> [[1]]$tractability[[9]]
#> [[1]]$tractability[[9]]$label
#> [1] "Approved Drug"
#> 
#> [[1]]$tractability[[9]]$modality
#> [1] "AB"
#> 
#> [[1]]$tractability[[9]]$value
#> [1] FALSE
#> 
#> 
#> [[1]]$tractability[[10]]
#> [[1]]$tractability[[10]]$label
#> [1] "Advanced Clinical"
#> 
#> [[1]]$tractability[[10]]$modality
#> [1] "AB"
#> 
#> [[1]]$tractability[[10]]$value
#> [1] FALSE
#> 
#> 
#> [[1]]$tractability[[11]]
#> [[1]]$tractability[[11]]$label
#> [1] "Phase 1 Clinical"
#> 
#> [[1]]$tractability[[11]]$modality
#> [1] "AB"
#> 
#> [[1]]$tractability[[11]]$value
#> [1] FALSE
#> 
#> 
#> [[1]]$tractability[[12]]
#> [[1]]$tractability[[12]]$label
#> [1] "UniProt loc high conf"
#> 
#> [[1]]$tractability[[12]]$modality
#> [1] "AB"
#> 
#> [[1]]$tractability[[12]]$value
#> [1] FALSE
#> 
#> 
#> [[1]]$tractability[[13]]
#> [[1]]$tractability[[13]]$label
#> [1] "GO CC high conf"
#> 
#> [[1]]$tractability[[13]]$modality
#> [1] "AB"
#> 
#> [[1]]$tractability[[13]]$value
#> [1] FALSE
#> 
#> 
#> [[1]]$tractability[[14]]
#> [[1]]$tractability[[14]]$label
#> [1] "UniProt loc med conf"
#> 
#> [[1]]$tractability[[14]]$modality
#> [1] "AB"
#> 
#> [[1]]$tractability[[14]]$value
#> [1] FALSE
#> 
#> 
#> [[1]]$tractability[[15]]
#> [[1]]$tractability[[15]]$label
#> [1] "UniProt SigP or TMHMM"
#> 
#> [[1]]$tractability[[15]]$modality
#> [1] "AB"
#> 
#> [[1]]$tractability[[15]]$value
#> [1] FALSE
#> 
#> 
#> [[1]]$tractability[[16]]
#> [[1]]$tractability[[16]]$label
#> [1] "GO CC med conf"
#> 
#> [[1]]$tractability[[16]]$modality
#> [1] "AB"
#> 
#> [[1]]$tractability[[16]]$value
#> [1] FALSE
#> 
#> 
#> [[1]]$tractability[[17]]
#> [[1]]$tractability[[17]]$label
#> [1] "Human Protein Atlas loc"
#> 
#> [[1]]$tractability[[17]]$modality
#> [1] "AB"
#> 
#> [[1]]$tractability[[17]]$value
#> [1] TRUE
#> 
#> 
#> [[1]]$tractability[[18]]
#> [[1]]$tractability[[18]]$label
#> [1] "Approved Drug"
#> 
#> [[1]]$tractability[[18]]$modality
#> [1] "PR"
#> 
#> [[1]]$tractability[[18]]$value
#> [1] FALSE
#> 
#> 
#> [[1]]$tractability[[19]]
#> [[1]]$tractability[[19]]$label
#> [1] "Advanced Clinical"
#> 
#> [[1]]$tractability[[19]]$modality
#> [1] "PR"
#> 
#> [[1]]$tractability[[19]]$value
#> [1] FALSE
#> 
#> 
#> [[1]]$tractability[[20]]
#> [[1]]$tractability[[20]]$label
#> [1] "Phase 1 Clinical"
#> 
#> [[1]]$tractability[[20]]$modality
#> [1] "PR"
#> 
#> [[1]]$tractability[[20]]$value
#> [1] FALSE
#> 
#> 
#> [[1]]$tractability[[21]]
#> [[1]]$tractability[[21]]$label
#> [1] "Literature"
#> 
#> [[1]]$tractability[[21]]$modality
#> [1] "PR"
#> 
#> [[1]]$tractability[[21]]$value
#> [1] FALSE
#> 
#> 
#> [[1]]$tractability[[22]]
#> [[1]]$tractability[[22]]$label
#> [1] "UniProt Ubiquitination"
#> 
#> [[1]]$tractability[[22]]$modality
#> [1] "PR"
#> 
#> [[1]]$tractability[[22]]$value
#> [1] FALSE
#> 
#> 
#> [[1]]$tractability[[23]]
#> [[1]]$tractability[[23]]$label
#> [1] "Database Ubiquitination"
#> 
#> [[1]]$tractability[[23]]$modality
#> [1] "PR"
#> 
#> [[1]]$tractability[[23]]$value
#> [1] TRUE
#> 
#> 
#> [[1]]$tractability[[24]]
#> [[1]]$tractability[[24]]$label
#> [1] "Half-life Data"
#> 
#> [[1]]$tractability[[24]]$modality
#> [1] "PR"
#> 
#> [[1]]$tractability[[24]]$value
#> [1] TRUE
#> 
#> 
#> [[1]]$tractability[[25]]
#> [[1]]$tractability[[25]]$label
#> [1] "Small Molecule Binder"
#> 
#> [[1]]$tractability[[25]]$modality
#> [1] "PR"
#> 
#> [[1]]$tractability[[25]]$value
#> [1] FALSE
#> 
#> 
#> [[1]]$tractability[[26]]
#> [[1]]$tractability[[26]]$label
#> [1] "Approved Drug"
#> 
#> [[1]]$tractability[[26]]$modality
#> [1] "OC"
#> 
#> [[1]]$tractability[[26]]$value
#> [1] FALSE
#> 
#> 
#> [[1]]$tractability[[27]]
#> [[1]]$tractability[[27]]$label
#> [1] "Advanced Clinical"
#> 
#> [[1]]$tractability[[27]]$modality
#> [1] "OC"
#> 
#> [[1]]$tractability[[27]]$value
#> [1] FALSE
#> 
#> 
#> [[1]]$tractability[[28]]
#> [[1]]$tractability[[28]]$label
#> [1] "Phase 1 Clinical"
#> 
#> [[1]]$tractability[[28]]$modality
#> [1] "OC"
#> 
#> [[1]]$tractability[[28]]$value
#> [1] FALSE
#> 
#> 
#> 
#> [[1]]$geneticConstraint
#> [[1]]$geneticConstraint[[1]]
#> [[1]]$geneticConstraint[[1]]$constraintType
#> [1] "syn"
#> 
#> [[1]]$geneticConstraint[[1]]$exp
#> [1] 184.61
#> 
#> [[1]]$geneticConstraint[[1]]$obs
#> [1] 155
#> 
#> [[1]]$geneticConstraint[[1]]$score
#> [1] 1.1877
#> 
#> [[1]]$geneticConstraint[[1]]$oe
#> [1] 0.83962
#> 
#> [[1]]$geneticConstraint[[1]]$oeLower
#> [1] 0.736
#> 
#> [[1]]$geneticConstraint[[1]]$oeUpper
#> [1] 0.959
#> 
#> 
#> [[1]]$geneticConstraint[[2]]
#> [[1]]$geneticConstraint[[2]]$constraintType
#> [1] "mis"
#> 
#> [[1]]$geneticConstraint[[2]]$exp
#> [1] 471.95
#> 
#> [[1]]$geneticConstraint[[2]]$obs
#> [1] 359
#> 
#> [[1]]$geneticConstraint[[2]]$score
#> [1] 1.8987
#> 
#> [[1]]$geneticConstraint[[2]]$oe
#> [1] 0.76068
#> 
#> [[1]]$geneticConstraint[[2]]$oeLower
#> [1] 0.697
#> 
#> [[1]]$geneticConstraint[[2]]$oeUpper
#> [1] 0.83
#> 
#> 
#> [[1]]$geneticConstraint[[3]]
#> [[1]]$geneticConstraint[[3]]$constraintType
#> [1] "lof"
#> 
#> [[1]]$geneticConstraint[[3]]$exp
#> [1] 33.465
#> 
#> [[1]]$geneticConstraint[[3]]$obs
#> [1] 31
#> 
#> [[1]]$geneticConstraint[[3]]$score
#> [1] 1.6143e-10
#> 
#> [[1]]$geneticConstraint[[3]]$oe
#> [1] 0.92635
#> 
#> [[1]]$geneticConstraint[[3]]$oeLower
#> [1] 0.695
#> 
#> [[1]]$geneticConstraint[[3]]$oeUpper
#> [1] 1.25
#> 
#> 
#> 
#> [[1]]$chemicalProbes
#> [[1]]$chemicalProbes[[1]]
#> [[1]]$chemicalProbes[[1]]$id
#> [1] "DNL343"
#> 
#> [[1]]$chemicalProbes[[1]]$control
#> [1] ""
#> 
#> [[1]]$chemicalProbes[[1]]$drugId
#> NULL
#> 
#> [[1]]$chemicalProbes[[1]]$mechanismOfAction
#> [[1]]$chemicalProbes[[1]]$mechanismOfAction[[1]]
#> [1] "activator"
#> 
#> 
#> [[1]]$chemicalProbes[[1]]$isHighQuality
#> [1] TRUE
#> 
#> [[1]]$chemicalProbes[[1]]$urls
#> [[1]]$chemicalProbes[[1]]$urls[[1]]
#> [[1]]$chemicalProbes[[1]]$urls[[1]]$url
#> [1] "http://www.chemicalprobes.org/"
#> 
#> 
#> 
#> 
#> 
#> [[1]]$drugAndClinicalCandidates
#> [[1]]$drugAndClinicalCandidates$rows
#> list()
#> 
#> 
#> 
#> [[2]]
#> [[2]]$id
#> [1] "ENSG00000175354"
#> 
#> [[2]]$approvedSymbol
#> [1] "PTPN2"
#> 
#> [[2]]$approvedName
#> [1] "protein tyrosine phosphatase non-receptor type 2"
#> 
#> [[2]]$associatedDiseases
#> [[2]]$associatedDiseases$count
#> [1] 511
#> 
#> [[2]]$associatedDiseases$rows
#> [[2]]$associatedDiseases$rows[[1]]
#> [[2]]$associatedDiseases$rows[[1]]$score
#> [1] 0.5212055
#> 
#> [[2]]$associatedDiseases$rows[[1]]$datatypeScores
#> [[2]]$associatedDiseases$rows[[1]]$datatypeScores[[1]]
#> [[2]]$associatedDiseases$rows[[1]]$datatypeScores[[1]]$id
#> [1] "literature"
#> 
#> [[2]]$associatedDiseases$rows[[1]]$datatypeScores[[1]]$score
#> [1] 0.7064825
#> 
#> 
#> [[2]]$associatedDiseases$rows[[1]]$datatypeScores[[2]]
#> [[2]]$associatedDiseases$rows[[1]]$datatypeScores[[2]]$id
#> [1] "rna_expression"
#> 
#> [[2]]$associatedDiseases$rows[[1]]$datatypeScores[[2]]$score
#> [1] 0.01773991
#> 
#> 
#> [[2]]$associatedDiseases$rows[[1]]$datatypeScores[[3]]
#> [[2]]$associatedDiseases$rows[[1]]$datatypeScores[[3]]$id
#> [1] "genetic_association"
#> 
#> [[2]]$associatedDiseases$rows[[1]]$datatypeScores[[3]]$score
#> [1] 0.8216251
#> 
#> 
#> 
#> [[2]]$associatedDiseases$rows[[1]]$disease
#> [[2]]$associatedDiseases$rows[[1]]$disease$id
#> [1] "EFO_0000685"
#> 
#> [[2]]$associatedDiseases$rows[[1]]$disease$name
#> [1] "rheumatoid arthritis"
#> 
#> [[2]]$associatedDiseases$rows[[1]]$disease$description
#> [1] "A chronic, systemic autoimmune disorder characterized by inflammation in the synovial membranes and articular surfaces. It manifests primarily as a symmetric, erosive polyarthritis that spares the axial skeleton and is typically associated with the presence in the serum of rheumatoid factor."
#> 
#> [[2]]$associatedDiseases$rows[[1]]$disease$therapeuticAreas
#> [[2]]$associatedDiseases$rows[[1]]$disease$therapeuticAreas[[1]]
#> [[2]]$associatedDiseases$rows[[1]]$disease$therapeuticAreas[[1]]$name
#> [1] "immune system disease"
#> 
#> 
#> [[2]]$associatedDiseases$rows[[1]]$disease$therapeuticAreas[[2]]
#> [[2]]$associatedDiseases$rows[[1]]$disease$therapeuticAreas[[2]]$name
#> [1] "musculoskeletal or connective tissue disease"
#> 
#> 
#> [[2]]$associatedDiseases$rows[[1]]$disease$therapeuticAreas[[3]]
#> [[2]]$associatedDiseases$rows[[1]]$disease$therapeuticAreas[[3]]$name
#> [1] "phenotype"
#> 
#> 
#> 
#> 
#> 
#> [[2]]$associatedDiseases$rows[[2]]
#> [[2]]$associatedDiseases$rows[[2]]$score
#> [1] 0.5060445
#> 
#> [[2]]$associatedDiseases$rows[[2]]$datatypeScores
#> [[2]]$associatedDiseases$rows[[2]]$datatypeScores[[1]]
#> [[2]]$associatedDiseases$rows[[2]]$datatypeScores[[1]]$id
#> [1] "literature"
#> 
#> [[2]]$associatedDiseases$rows[[2]]$datatypeScores[[1]]$score
#> [1] 0.4633332
#> 
#> 
#> [[2]]$associatedDiseases$rows[[2]]$datatypeScores[[2]]
#> [[2]]$associatedDiseases$rows[[2]]$datatypeScores[[2]]$id
#> [1] "genetic_association"
#> 
#> [[2]]$associatedDiseases$rows[[2]]$datatypeScores[[2]]$score
#> [1] 0.809238
#> 
#> 
#> 
#> [[2]]$associatedDiseases$rows[[2]]$disease
#> [[2]]$associatedDiseases$rows[[2]]$disease$id
#> [1] "MONDO_0005147"
#> 
#> [[2]]$associatedDiseases$rows[[2]]$disease$name
#> [1] "type 1 diabetes mellitus"
#> 
#> [[2]]$associatedDiseases$rows[[2]]$disease$description
#> [1] "A chronic condition characterized by minimal or absent production of insulin by the pancreas."
#> 
#> [[2]]$associatedDiseases$rows[[2]]$disease$therapeuticAreas
#> [[2]]$associatedDiseases$rows[[2]]$disease$therapeuticAreas[[1]]
#> [[2]]$associatedDiseases$rows[[2]]$disease$therapeuticAreas[[1]]$name
#> [1] "endocrine system disease"
#> 
#> 
#> [[2]]$associatedDiseases$rows[[2]]$disease$therapeuticAreas[[2]]
#> [[2]]$associatedDiseases$rows[[2]]$disease$therapeuticAreas[[2]]$name
#> [1] "gastrointestinal disease"
#> 
#> 
#> [[2]]$associatedDiseases$rows[[2]]$disease$therapeuticAreas[[3]]
#> [[2]]$associatedDiseases$rows[[2]]$disease$therapeuticAreas[[3]]$name
#> [1] "immune system disease"
#> 
#> 
#> [[2]]$associatedDiseases$rows[[2]]$disease$therapeuticAreas[[4]]
#> [[2]]$associatedDiseases$rows[[2]]$disease$therapeuticAreas[[4]]$name
#> [1] "nutritional or metabolic disease"
#> 
#> 
#> [[2]]$associatedDiseases$rows[[2]]$disease$therapeuticAreas[[5]]
#> [[2]]$associatedDiseases$rows[[2]]$disease$therapeuticAreas[[5]]$name
#> [1] "phenotype"
#> 
#> 
#> 
#> 
#> 
#> [[2]]$associatedDiseases$rows[[3]]
#> [[2]]$associatedDiseases$rows[[3]]$score
#> [1] 0.4843617
#> 
#> [[2]]$associatedDiseases$rows[[3]]$datatypeScores
#> [[2]]$associatedDiseases$rows[[3]]$datatypeScores[[1]]
#> [[2]]$associatedDiseases$rows[[3]]$datatypeScores[[1]]$id
#> [1] "literature"
#> 
#> [[2]]$associatedDiseases$rows[[3]]$datatypeScores[[1]]$score
#> [1] 0.09342821
#> 
#> 
#> [[2]]$associatedDiseases$rows[[3]]$datatypeScores[[2]]
#> [[2]]$associatedDiseases$rows[[3]]$datatypeScores[[2]]$id
#> [1] "animal_model"
#> 
#> [[2]]$associatedDiseases$rows[[3]]$datatypeScores[[2]]$score
#> [1] 0.2926579
#> 
#> 
#> [[2]]$associatedDiseases$rows[[3]]$datatypeScores[[3]]
#> [[2]]$associatedDiseases$rows[[3]]$datatypeScores[[3]]$id
#> [1] "genetic_association"
#> 
#> [[2]]$associatedDiseases$rows[[3]]$datatypeScores[[3]]$score
#> [1] 0.7800291
#> 
#> 
#> 
#> [[2]]$associatedDiseases$rows[[3]]$disease
#> [[2]]$associatedDiseases$rows[[3]]$disease$id
#> [1] "EFO_0005140"
#> 
#> [[2]]$associatedDiseases$rows[[3]]$disease$name
#> [1] "autoimmune disease"
#> 
#> [[2]]$associatedDiseases$rows[[3]]$disease$description
#> [1] "Autoimmune disease or disorder is a disease characterized by an immune response of an organism against parts of itself causing pathology e.g. Graves' disease."
#> 
#> [[2]]$associatedDiseases$rows[[3]]$disease$therapeuticAreas
#> [[2]]$associatedDiseases$rows[[3]]$disease$therapeuticAreas[[1]]
#> [[2]]$associatedDiseases$rows[[3]]$disease$therapeuticAreas[[1]]$name
#> [1] "immune system disease"
#> 
#> 
#> 
#> 
#> 
#> [[2]]$associatedDiseases$rows[[4]]
#> [[2]]$associatedDiseases$rows[[4]]$score
#> [1] 0.4842303
#> 
#> [[2]]$associatedDiseases$rows[[4]]$datatypeScores
#> [[2]]$associatedDiseases$rows[[4]]$datatypeScores[[1]]
#> [[2]]$associatedDiseases$rows[[4]]$datatypeScores[[1]]$id
#> [1] "literature"
#> 
#> [[2]]$associatedDiseases$rows[[4]]$datatypeScores[[1]]$score
#> [1] 0.2369199
#> 
#> 
#> [[2]]$associatedDiseases$rows[[4]]$datatypeScores[[2]]
#> [[2]]$associatedDiseases$rows[[4]]$datatypeScores[[2]]$id
#> [1] "genetic_association"
#> 
#> [[2]]$associatedDiseases$rows[[4]]$datatypeScores[[2]]$score
#> [1] 0.7846761
#> 
#> 
#> 
#> [[2]]$associatedDiseases$rows[[4]]$disease
#> [[2]]$associatedDiseases$rows[[4]]$disease$id
#> [1] "EFO_0000384"
#> 
#> [[2]]$associatedDiseases$rows[[4]]$disease$name
#> [1] "Crohn's disease"
#> 
#> [[2]]$associatedDiseases$rows[[4]]$disease$description
#> [1] "A gastrointestinal disorder characterized by chronic inflammation involving all layers of the intestinal wall, noncaseating granulomas affecting the intestinal wall and regional lymph nodes, and transmural fibrosis. Crohn disease most commonly involves the terminal ileum; the colon is the second most common site of involvement."
#> 
#> [[2]]$associatedDiseases$rows[[4]]$disease$therapeuticAreas
#> [[2]]$associatedDiseases$rows[[4]]$disease$therapeuticAreas[[1]]
#> [[2]]$associatedDiseases$rows[[4]]$disease$therapeuticAreas[[1]]$name
#> [1] "immune system disease"
#> 
#> 
#> [[2]]$associatedDiseases$rows[[4]]$disease$therapeuticAreas[[2]]
#> [[2]]$associatedDiseases$rows[[4]]$disease$therapeuticAreas[[2]]$name
#> [1] "gastrointestinal disease"
#> 
#> 
#> [[2]]$associatedDiseases$rows[[4]]$disease$therapeuticAreas[[3]]
#> [[2]]$associatedDiseases$rows[[4]]$disease$therapeuticAreas[[3]]$name
#> [1] "genetic, familial or congenital disease"
#> 
#> 
#> 
#> 
#> 
#> [[2]]$associatedDiseases$rows[[5]]
#> [[2]]$associatedDiseases$rows[[5]]$score
#> [1] 0.4789509
#> 
#> [[2]]$associatedDiseases$rows[[5]]$datatypeScores
#> [[2]]$associatedDiseases$rows[[5]]$datatypeScores[[1]]
#> [[2]]$associatedDiseases$rows[[5]]$datatypeScores[[1]]$id
#> [1] "literature"
#> 
#> [[2]]$associatedDiseases$rows[[5]]$datatypeScores[[1]]$score
#> [1] 0.1320306
#> 
#> 
#> [[2]]$associatedDiseases$rows[[5]]$datatypeScores[[2]]
#> [[2]]$associatedDiseases$rows[[5]]$datatypeScores[[2]]$id
#> [1] "genetic_association"
#> 
#> [[2]]$associatedDiseases$rows[[5]]$datatypeScores[[2]]$score
#> [1] 0.7812363
#> 
#> 
#> 
#> [[2]]$associatedDiseases$rows[[5]]$disease
#> [[2]]$associatedDiseases$rows[[5]]$disease$id
#> [1] "EFO_0000676"
#> 
#> [[2]]$associatedDiseases$rows[[5]]$disease$name
#> [1] "psoriasis"
#> 
#> [[2]]$associatedDiseases$rows[[5]]$disease$description
#> [1] "An autoimmune condition characterized by red, well-delineated plaques with silvery scales that are usually on the extensor surfaces and scalp. They can occasionally present with these manifestations: pustules; erythema and scaling in intertriginous areas, and erythroderma, that are often distributed on extensor surfaces and scalp."
#> 
#> [[2]]$associatedDiseases$rows[[5]]$disease$therapeuticAreas
#> [[2]]$associatedDiseases$rows[[5]]$disease$therapeuticAreas[[1]]
#> [[2]]$associatedDiseases$rows[[5]]$disease$therapeuticAreas[[1]]$name
#> [1] "immune system disease"
#> 
#> 
#> [[2]]$associatedDiseases$rows[[5]]$disease$therapeuticAreas[[2]]
#> [[2]]$associatedDiseases$rows[[5]]$disease$therapeuticAreas[[2]]$name
#> [1] "integumentary system disease"
#> 
#> 
#> [[2]]$associatedDiseases$rows[[5]]$disease$therapeuticAreas[[3]]
#> [[2]]$associatedDiseases$rows[[5]]$disease$therapeuticAreas[[3]]$name
#> [1] "genetic, familial or congenital disease"
#> 
#> 
#> 
#> 
#> 
#> [[2]]$associatedDiseases$rows[[6]]
#> [[2]]$associatedDiseases$rows[[6]]$score
#> [1] 0.4772409
#> 
#> [[2]]$associatedDiseases$rows[[6]]$datatypeScores
#> [[2]]$associatedDiseases$rows[[6]]$datatypeScores[[1]]
#> [[2]]$associatedDiseases$rows[[6]]$datatypeScores[[1]]$id
#> [1] "literature"
#> 
#> [[2]]$associatedDiseases$rows[[6]]$datatypeScores[[1]]$score
#> [1] 0.3875231
#> 
#> 
#> [[2]]$associatedDiseases$rows[[6]]$datatypeScores[[2]]
#> [[2]]$associatedDiseases$rows[[6]]$datatypeScores[[2]]$id
#> [1] "genetic_association"
#> 
#> [[2]]$associatedDiseases$rows[[6]]$datatypeScores[[2]]$score
#> [1] 0.7656488
#> 
#> 
#> 
#> [[2]]$associatedDiseases$rows[[6]]$disease
#> [[2]]$associatedDiseases$rows[[6]]$disease$id
#> [1] "EFO_0003767"
#> 
#> [[2]]$associatedDiseases$rows[[6]]$disease$name
#> [1] "inflammatory bowel disease"
#> 
#> [[2]]$associatedDiseases$rows[[6]]$disease$description
#> [1] "A spectrum of small and large bowel inflammatory diseases of unknown etiology. It includes Crohn's disease, ulcerative colitis, and colitis of indeterminate type."
#> 
#> [[2]]$associatedDiseases$rows[[6]]$disease$therapeuticAreas
#> [[2]]$associatedDiseases$rows[[6]]$disease$therapeuticAreas[[1]]
#> [[2]]$associatedDiseases$rows[[6]]$disease$therapeuticAreas[[1]]$name
#> [1] "immune system disease"
#> 
#> 
#> [[2]]$associatedDiseases$rows[[6]]$disease$therapeuticAreas[[2]]
#> [[2]]$associatedDiseases$rows[[6]]$disease$therapeuticAreas[[2]]$name
#> [1] "gastrointestinal disease"
#> 
#> 
#> [[2]]$associatedDiseases$rows[[6]]$disease$therapeuticAreas[[3]]
#> [[2]]$associatedDiseases$rows[[6]]$disease$therapeuticAreas[[3]]$name
#> [1] "genetic, familial or congenital disease"
#> 
#> 
#> 
#> 
#> 
#> [[2]]$associatedDiseases$rows[[7]]
#> [[2]]$associatedDiseases$rows[[7]]$score
#> [1] 0.4341924
#> 
#> [[2]]$associatedDiseases$rows[[7]]$datatypeScores
#> [[2]]$associatedDiseases$rows[[7]]$datatypeScores[[1]]
#> [[2]]$associatedDiseases$rows[[7]]$datatypeScores[[1]]$id
#> [1] "literature"
#> 
#> [[2]]$associatedDiseases$rows[[7]]$datatypeScores[[1]]$score
#> [1] 0.1067901
#> 
#> 
#> [[2]]$associatedDiseases$rows[[7]]$datatypeScores[[2]]
#> [[2]]$associatedDiseases$rows[[7]]$datatypeScores[[2]]$id
#> [1] "genetic_association"
#> 
#> [[2]]$associatedDiseases$rows[[7]]$datatypeScores[[2]]$score
#> [1] 0.4156386
#> 
#> 
#> [[2]]$associatedDiseases$rows[[7]]$datatypeScores[[3]]
#> [[2]]$associatedDiseases$rows[[7]]$datatypeScores[[3]]$id
#> [1] "genetic_literature"
#> 
#> [[2]]$associatedDiseases$rows[[7]]$datatypeScores[[3]]$score
#> [1] 0.6079308
#> 
#> 
#> 
#> [[2]]$associatedDiseases$rows[[7]]$disease
#> [[2]]$associatedDiseases$rows[[7]]$disease$id
#> [1] "MONDO_0007915"
#> 
#> [[2]]$associatedDiseases$rows[[7]]$disease$name
#> [1] "systemic lupus erythematosus"
#> 
#> [[2]]$associatedDiseases$rows[[7]]$disease$description
#> [1] "An autoimmune multi-organ disease typically associated with vasculopathy and autoantibody production. Most patients have antinuclear antibodies (ANA). The presence of anti-dsDNA or anti-Smith antibodies are highly-specific."
#> 
#> [[2]]$associatedDiseases$rows[[7]]$disease$therapeuticAreas
#> [[2]]$associatedDiseases$rows[[7]]$disease$therapeuticAreas[[1]]
#> [[2]]$associatedDiseases$rows[[7]]$disease$therapeuticAreas[[1]]$name
#> [1] "immune system disease"
#> 
#> 
#> [[2]]$associatedDiseases$rows[[7]]$disease$therapeuticAreas[[2]]
#> [[2]]$associatedDiseases$rows[[7]]$disease$therapeuticAreas[[2]]$name
#> [1] "urinary system disease"
#> 
#> 
#> [[2]]$associatedDiseases$rows[[7]]$disease$therapeuticAreas[[3]]
#> [[2]]$associatedDiseases$rows[[7]]$disease$therapeuticAreas[[3]]$name
#> [1] "musculoskeletal or connective tissue disease"
#> 
#> 
#> [[2]]$associatedDiseases$rows[[7]]$disease$therapeuticAreas[[4]]
#> [[2]]$associatedDiseases$rows[[7]]$disease$therapeuticAreas[[4]]$name
#> [1] "phenotype"
#> 
#> 
#> 
#> 
#> 
#> [[2]]$associatedDiseases$rows[[8]]
#> [[2]]$associatedDiseases$rows[[8]]$score
#> [1] 0.4231873
#> 
#> [[2]]$associatedDiseases$rows[[8]]$datatypeScores
#> [[2]]$associatedDiseases$rows[[8]]$datatypeScores[[1]]
#> [[2]]$associatedDiseases$rows[[8]]$datatypeScores[[1]]$id
#> [1] "literature"
#> 
#> [[2]]$associatedDiseases$rows[[8]]$datatypeScores[[1]]$score
#> [1] 0.03039654
#> 
#> 
#> [[2]]$associatedDiseases$rows[[8]]$datatypeScores[[2]]
#> [[2]]$associatedDiseases$rows[[8]]$datatypeScores[[2]]$id
#> [1] "genetic_association"
#> 
#> [[2]]$associatedDiseases$rows[[8]]$datatypeScores[[2]]$score
#> [1] 0.6945911
#> 
#> 
#> 
#> [[2]]$associatedDiseases$rows[[8]]$disease
#> [[2]]$associatedDiseases$rows[[8]]$disease$id
#> [1] "EFO_0004705"
#> 
#> [[2]]$associatedDiseases$rows[[8]]$disease$name
#> [1] "hypothyroidism"
#> 
#> [[2]]$associatedDiseases$rows[[8]]$disease$description
#> [1] "Abnormally low levels of thyroid hormone."
#> 
#> [[2]]$associatedDiseases$rows[[8]]$disease$therapeuticAreas
#> [[2]]$associatedDiseases$rows[[8]]$disease$therapeuticAreas[[1]]
#> [[2]]$associatedDiseases$rows[[8]]$disease$therapeuticAreas[[1]]$name
#> [1] "endocrine system disease"
#> 
#> 
#> 
#> 
#> 
#> [[2]]$associatedDiseases$rows[[9]]
#> [[2]]$associatedDiseases$rows[[9]]$score
#> [1] 0.4096991
#> 
#> [[2]]$associatedDiseases$rows[[9]]$datatypeScores
#> [[2]]$associatedDiseases$rows[[9]]$datatypeScores[[1]]
#> [[2]]$associatedDiseases$rows[[9]]$datatypeScores[[1]]$id
#> [1] "literature"
#> 
#> [[2]]$associatedDiseases$rows[[9]]$datatypeScores[[1]]$score
#> [1] 0.705681
#> 
#> 
#> [[2]]$associatedDiseases$rows[[9]]$datatypeScores[[2]]
#> [[2]]$associatedDiseases$rows[[9]]$datatypeScores[[2]]$id
#> [1] "genetic_association"
#> 
#> [[2]]$associatedDiseases$rows[[9]]$datatypeScores[[2]]$score
#> [1] 0.6386398
#> 
#> 
#> 
#> [[2]]$associatedDiseases$rows[[9]]$disease
#> [[2]]$associatedDiseases$rows[[9]]$disease$id
#> [1] "EFO_0000729"
#> 
#> [[2]]$associatedDiseases$rows[[9]]$disease$name
#> [1] "ulcerative colitis"
#> 
#> [[2]]$associatedDiseases$rows[[9]]$disease$description
#> [1] "An inflammatory bowel disease involving the mucosal surface of the large intestine and rectum. It may present with an acute or slow onset and follows an intermittent or continuous course. Signs and symptoms include abdominal pain, diarrhea, fever, weight loss, and intestinal hemorrhage."
#> 
#> [[2]]$associatedDiseases$rows[[9]]$disease$therapeuticAreas
#> [[2]]$associatedDiseases$rows[[9]]$disease$therapeuticAreas[[1]]
#> [[2]]$associatedDiseases$rows[[9]]$disease$therapeuticAreas[[1]]$name
#> [1] "gastrointestinal disease"
#> 
#> 
#> [[2]]$associatedDiseases$rows[[9]]$disease$therapeuticAreas[[2]]
#> [[2]]$associatedDiseases$rows[[9]]$disease$therapeuticAreas[[2]]$name
#> [1] "immune system disease"
#> 
#> 
#> [[2]]$associatedDiseases$rows[[9]]$disease$therapeuticAreas[[3]]
#> [[2]]$associatedDiseases$rows[[9]]$disease$therapeuticAreas[[3]]$name
#> [1] "genetic, familial or congenital disease"
#> 
#> 
#> 
#> 
#> 
#> [[2]]$associatedDiseases$rows[[10]]
#> [[2]]$associatedDiseases$rows[[10]]$score
#> [1] 0.4074056
#> 
#> [[2]]$associatedDiseases$rows[[10]]$datatypeScores
#> [[2]]$associatedDiseases$rows[[10]]$datatypeScores[[1]]
#> [[2]]$associatedDiseases$rows[[10]]$datatypeScores[[1]]$id
#> [1] "literature"
#> 
#> [[2]]$associatedDiseases$rows[[10]]$datatypeScores[[1]]$score
#> [1] 0.09068809
#> 
#> 
#> [[2]]$associatedDiseases$rows[[10]]$datatypeScores[[2]]
#> [[2]]$associatedDiseases$rows[[10]]$datatypeScores[[2]]$id
#> [1] "genetic_association"
#> 
#> [[2]]$associatedDiseases$rows[[10]]$datatypeScores[[2]]$score
#> [1] 0.6656169
#> 
#> 
#> 
#> [[2]]$associatedDiseases$rows[[10]]$disease
#> [[2]]$associatedDiseases$rows[[10]]$disease$id
#> [1] "EFO_0001060"
#> 
#> [[2]]$associatedDiseases$rows[[10]]$disease$name
#> [1] "celiac disease"
#> 
#> [[2]]$associatedDiseases$rows[[10]]$disease$description
#> [1] "An autoimmune genetic disorder with an unknown pattern of inheritance that primarily affects the digestive tract. It is caused by intolerance to dietary gluten. Consumption of gluten protein triggers an immune response which damages small intestinal villi and prevents adequate absorption of nutrients. Clinical signs include abdominal cramping, diarrhea or constipation and weight loss. If untreated, the clinical course may progress to malnutrition, anemia, osteoporosis and an increased risk of intestinal malignancies. However, the prognosis is favorable with successful avoidance of gluten in the diet."
#> 
#> [[2]]$associatedDiseases$rows[[10]]$disease$therapeuticAreas
#> [[2]]$associatedDiseases$rows[[10]]$disease$therapeuticAreas[[1]]
#> [[2]]$associatedDiseases$rows[[10]]$disease$therapeuticAreas[[1]]$name
#> [1] "gastrointestinal disease"
#> 
#> 
#> [[2]]$associatedDiseases$rows[[10]]$disease$therapeuticAreas[[2]]
#> [[2]]$associatedDiseases$rows[[10]]$disease$therapeuticAreas[[2]]$name
#> [1] "genetic, familial or congenital disease"
#> 
#> 
#> [[2]]$associatedDiseases$rows[[10]]$disease$therapeuticAreas[[3]]
#> [[2]]$associatedDiseases$rows[[10]]$disease$therapeuticAreas[[3]]$name
#> [1] "immune system disease"
#> 
#> 
#> 
#> 
#> 
#> [[2]]$associatedDiseases$rows[[11]]
#> [[2]]$associatedDiseases$rows[[11]]$score
#> [1] 0.3887307
#> 
#> [[2]]$associatedDiseases$rows[[11]]$datatypeScores
#> [[2]]$associatedDiseases$rows[[11]]$datatypeScores[[1]]
#> [[2]]$associatedDiseases$rows[[11]]$datatypeScores[[1]]$id
#> [1] "literature"
#> 
#> [[2]]$associatedDiseases$rows[[11]]$datatypeScores[[1]]$score
#> [1] 0.6300325
#> 
#> 
#> [[2]]$associatedDiseases$rows[[11]]$datatypeScores[[2]]
#> [[2]]$associatedDiseases$rows[[11]]$datatypeScores[[2]]$id
#> [1] "genetic_literature"
#> 
#> [[2]]$associatedDiseases$rows[[11]]$datatypeScores[[2]]$score
#> [1] 0.6079308
#> 
#> 
#> 
#> [[2]]$associatedDiseases$rows[[11]]$disease
#> [[2]]$associatedDiseases$rows[[11]]$disease$id
#> [1] "EFO_0005856"
#> 
#> [[2]]$associatedDiseases$rows[[11]]$disease$name
#> [1] "arthritis"
#> 
#> [[2]]$associatedDiseases$rows[[11]]$disease$description
#> [1] "Arthritis (from Greek arthro-, joint + -itis, inflammation; plural: arthritides) is a form of joint disorder that involves inflammation of one or more joints."
#> 
#> [[2]]$associatedDiseases$rows[[11]]$disease$therapeuticAreas
#> [[2]]$associatedDiseases$rows[[11]]$disease$therapeuticAreas[[1]]
#> [[2]]$associatedDiseases$rows[[11]]$disease$therapeuticAreas[[1]]$name
#> [1] "musculoskeletal or connective tissue disease"
#> 
#> 
#> [[2]]$associatedDiseases$rows[[11]]$disease$therapeuticAreas[[2]]
#> [[2]]$associatedDiseases$rows[[11]]$disease$therapeuticAreas[[2]]$name
#> [1] "phenotype"
#> 
#> 
#> 
#> 
#> 
#> [[2]]$associatedDiseases$rows[[12]]
#> [[2]]$associatedDiseases$rows[[12]]$score
#> [1] 0.3821172
#> 
#> [[2]]$associatedDiseases$rows[[12]]$datatypeScores
#> [[2]]$associatedDiseases$rows[[12]]$datatypeScores[[1]]
#> [[2]]$associatedDiseases$rows[[12]]$datatypeScores[[1]]$id
#> [1] "literature"
#> 
#> [[2]]$associatedDiseases$rows[[12]]$datatypeScores[[1]]$score
#> [1] 0.03799567
#> 
#> 
#> [[2]]$associatedDiseases$rows[[12]]$datatypeScores[[2]]
#> [[2]]$associatedDiseases$rows[[12]]$datatypeScores[[2]]$id
#> [1] "animal_model"
#> 
#> [[2]]$associatedDiseases$rows[[12]]$datatypeScores[[2]]$score
#> [1] 0.3955721
#> 
#> 
#> [[2]]$associatedDiseases$rows[[12]]$datatypeScores[[3]]
#> [[2]]$associatedDiseases$rows[[12]]$datatypeScores[[3]]$id
#> [1] "genetic_literature"
#> 
#> [[2]]$associatedDiseases$rows[[12]]$datatypeScores[[3]]$score
#> [1] 0.6079308
#> 
#> 
#> 
#> [[2]]$associatedDiseases$rows[[12]]$disease
#> [[2]]$associatedDiseases$rows[[12]]$disease$id
#> [1] "MONDO_0015517"
#> 
#> [[2]]$associatedDiseases$rows[[12]]$disease$name
#> [1] "common variable immunodeficiency"
#> 
#> [[2]]$associatedDiseases$rows[[12]]$disease$description
#> [1] "Common variable immunodeficiency (CVID) comprises a heterogeneous group of diseases characterized by a significant hypogammaglobulinemia of unknown cause, failure to produce specific antibodies after immunizations and susceptibility to bacterial infections, predominantly caused by encapsulated bacteria."
#> 
#> [[2]]$associatedDiseases$rows[[12]]$disease$therapeuticAreas
#> [[2]]$associatedDiseases$rows[[12]]$disease$therapeuticAreas[[1]]
#> [[2]]$associatedDiseases$rows[[12]]$disease$therapeuticAreas[[1]]$name
#> [1] "immune system disease"
#> 
#> 
#> [[2]]$associatedDiseases$rows[[12]]$disease$therapeuticAreas[[2]]
#> [[2]]$associatedDiseases$rows[[12]]$disease$therapeuticAreas[[2]]$name
#> [1] "hematologic disease"
#> 
#> 
#> [[2]]$associatedDiseases$rows[[12]]$disease$therapeuticAreas[[3]]
#> [[2]]$associatedDiseases$rows[[12]]$disease$therapeuticAreas[[3]]$name
#> [1] "genetic, familial or congenital disease"
#> 
#> 
#> 
#> 
#> 
#> [[2]]$associatedDiseases$rows[[13]]
#> [[2]]$associatedDiseases$rows[[13]]$score
#> [1] 0.3705038
#> 
#> [[2]]$associatedDiseases$rows[[13]]$datatypeScores
#> [[2]]$associatedDiseases$rows[[13]]$datatypeScores[[1]]
#> [[2]]$associatedDiseases$rows[[13]]$datatypeScores[[1]]$id
#> [1] "literature"
#> 
#> [[2]]$associatedDiseases$rows[[13]]$datatypeScores[[1]]$score
#> [1] 0.03039654
#> 
#> 
#> [[2]]$associatedDiseases$rows[[13]]$datatypeScores[[2]]
#> [[2]]$associatedDiseases$rows[[13]]$datatypeScores[[2]]$id
#> [1] "genetic_literature"
#> 
#> [[2]]$associatedDiseases$rows[[13]]$datatypeScores[[2]]$score
#> [1] 0.6079308
#> 
#> 
#> 
#> [[2]]$associatedDiseases$rows[[13]]$disease
#> [[2]]$associatedDiseases$rows[[13]]$disease$id
#> [1] "MONDO_0004670"
#> 
#> [[2]]$associatedDiseases$rows[[13]]$disease$name
#> [1] "lupus erythematosus"
#> 
#> [[2]]$associatedDiseases$rows[[13]]$disease$description
#> [1] "An autoimmune, connective tissue chronic inflammatory disorder affecting the skin, joints, kidneys, lungs, heart, and the peripheral blood cells. It is more commonly seen in women than men. Variants include discoid and systemic lupus erythematosus."
#> 
#> [[2]]$associatedDiseases$rows[[13]]$disease$therapeuticAreas
#> [[2]]$associatedDiseases$rows[[13]]$disease$therapeuticAreas[[1]]
#> [[2]]$associatedDiseases$rows[[13]]$disease$therapeuticAreas[[1]]$name
#> [1] "immune system disease"
#> 
#> 
#> [[2]]$associatedDiseases$rows[[13]]$disease$therapeuticAreas[[2]]
#> [[2]]$associatedDiseases$rows[[13]]$disease$therapeuticAreas[[2]]$name
#> [1] "musculoskeletal or connective tissue disease"
#> 
#> 
#> 
#> 
#> 
#> [[2]]$associatedDiseases$rows[[14]]
#> [[2]]$associatedDiseases$rows[[14]]$score
#> [1] 0.3640189
#> 
#> [[2]]$associatedDiseases$rows[[14]]$datatypeScores
#> [[2]]$associatedDiseases$rows[[14]]$datatypeScores[[1]]
#> [[2]]$associatedDiseases$rows[[14]]$datatypeScores[[1]]$id
#> [1] "genetic_association"
#> 
#> [[2]]$associatedDiseases$rows[[14]]$datatypeScores[[1]]$score
#> [1] 0.5987835
#> 
#> 
#> 
#> [[2]]$associatedDiseases$rows[[14]]$disease
#> [[2]]$associatedDiseases$rows[[14]]$disease$id
#> [1] "EFO_1001494"
#> 
#> [[2]]$associatedDiseases$rows[[14]]$disease$name
#> [1] "psoriasis vulgaris"
#> 
#> [[2]]$associatedDiseases$rows[[14]]$disease$description
#> [1] "Plaque psoriasis is the most common presentation of psoriasis. It presents as small to large, well demarcated, red, scaly and thickened areas of skin. It most likely to affect elbows, knees, and lower back but may arise on any part of the body."
#> 
#> [[2]]$associatedDiseases$rows[[14]]$disease$therapeuticAreas
#> [[2]]$associatedDiseases$rows[[14]]$disease$therapeuticAreas[[1]]
#> [[2]]$associatedDiseases$rows[[14]]$disease$therapeuticAreas[[1]]$name
#> [1] "immune system disease"
#> 
#> 
#> [[2]]$associatedDiseases$rows[[14]]$disease$therapeuticAreas[[2]]
#> [[2]]$associatedDiseases$rows[[14]]$disease$therapeuticAreas[[2]]$name
#> [1] "integumentary system disease"
#> 
#> 
#> [[2]]$associatedDiseases$rows[[14]]$disease$therapeuticAreas[[3]]
#> [[2]]$associatedDiseases$rows[[14]]$disease$therapeuticAreas[[3]]$name
#> [1] "genetic, familial or congenital disease"
#> 
#> 
#> 
#> 
#> 
#> [[2]]$associatedDiseases$rows[[15]]
#> [[2]]$associatedDiseases$rows[[15]]$score
#> [1] 0.3624806
#> 
#> [[2]]$associatedDiseases$rows[[15]]$datatypeScores
#> [[2]]$associatedDiseases$rows[[15]]$datatypeScores[[1]]
#> [[2]]$associatedDiseases$rows[[15]]$datatypeScores[[1]]$id
#> [1] "genetic_association"
#> 
#> [[2]]$associatedDiseases$rows[[15]]$datatypeScores[[1]]$score
#> [1] 0.596253
#> 
#> 
#> 
#> [[2]]$associatedDiseases$rows[[15]]$disease
#> [[2]]$associatedDiseases$rows[[15]]$disease$id
#> [1] "EFO_0000701"
#> 
#> [[2]]$associatedDiseases$rows[[15]]$disease$name
#> [1] "skin disease"
#> 
#> [[2]]$associatedDiseases$rows[[15]]$disease$description
#> [1] "Any deviation from the normal structure or function of the skin or subcutaneous tissue that is manifested by a characteristic set of symptoms and signs."
#> 
#> [[2]]$associatedDiseases$rows[[15]]$disease$therapeuticAreas
#> [[2]]$associatedDiseases$rows[[15]]$disease$therapeuticAreas[[1]]
#> [[2]]$associatedDiseases$rows[[15]]$disease$therapeuticAreas[[1]]$name
#> [1] "integumentary system disease"
#> 
#> 
#> 
#> 
#> 
#> [[2]]$associatedDiseases$rows[[16]]
#> [[2]]$associatedDiseases$rows[[16]]$score
#> [1] 0.3385734
#> 
#> [[2]]$associatedDiseases$rows[[16]]$datatypeScores
#> [[2]]$associatedDiseases$rows[[16]]$datatypeScores[[1]]
#> [[2]]$associatedDiseases$rows[[16]]$datatypeScores[[1]]$id
#> [1] "literature"
#> 
#> [[2]]$associatedDiseases$rows[[16]]$datatypeScores[[1]]$score
#> [1] 0.2294939
#> 
#> 
#> [[2]]$associatedDiseases$rows[[16]]$datatypeScores[[2]]
#> [[2]]$associatedDiseases$rows[[16]]$datatypeScores[[2]]$id
#> [1] "genetic_association"
#> 
#> [[2]]$associatedDiseases$rows[[16]]$datatypeScores[[2]]$score
#> [1] 0.5454528
#> 
#> 
#> 
#> [[2]]$associatedDiseases$rows[[16]]$disease
#> [[2]]$associatedDiseases$rows[[16]]$disease$id
#> [1] "MONDO_0002406"
#> 
#> [[2]]$associatedDiseases$rows[[16]]$disease$name
#> [1] "dermatitis"
#> 
#> [[2]]$associatedDiseases$rows[[16]]$disease$description
#> [1] "An inflammatory process affecting the skin. Signs include red rash, itching, and blister formation. Representative examples are contact dermatitis, atopic dermatitis, and seborrheic dermatitis."
#> 
#> [[2]]$associatedDiseases$rows[[16]]$disease$therapeuticAreas
#> [[2]]$associatedDiseases$rows[[16]]$disease$therapeuticAreas[[1]]
#> [[2]]$associatedDiseases$rows[[16]]$disease$therapeuticAreas[[1]]$name
#> [1] "integumentary system disease"
#> 
#> 
#> 
#> 
#> 
#> [[2]]$associatedDiseases$rows[[17]]
#> [[2]]$associatedDiseases$rows[[17]]$score
#> [1] 0.3308293
#> 
#> [[2]]$associatedDiseases$rows[[17]]$datatypeScores
#> [[2]]$associatedDiseases$rows[[17]]$datatypeScores[[1]]
#> [[2]]$associatedDiseases$rows[[17]]$datatypeScores[[1]]$id
#> [1] "literature"
#> 
#> [[2]]$associatedDiseases$rows[[17]]$datatypeScores[[1]]$score
#> [1] 0.7118589
#> 
#> 
#> [[2]]$associatedDiseases$rows[[17]]$datatypeScores[[2]]
#> [[2]]$associatedDiseases$rows[[17]]$datatypeScores[[2]]$id
#> [1] "genetic_association"
#> 
#> [[2]]$associatedDiseases$rows[[17]]$datatypeScores[[2]]$score
#> [1] 0.5085962
#> 
#> 
#> 
#> [[2]]$associatedDiseases$rows[[17]]$disease
#> [[2]]$associatedDiseases$rows[[17]]$disease$id
#> [1] "EFO_0003872"
#> 
#> [[2]]$associatedDiseases$rows[[17]]$disease$name
#> [1] "colitis"
#> 
#> [[2]]$associatedDiseases$rows[[17]]$disease$description
#> [1] "Inflammation of the colon."
#> 
#> [[2]]$associatedDiseases$rows[[17]]$disease$therapeuticAreas
#> [[2]]$associatedDiseases$rows[[17]]$disease$therapeuticAreas[[1]]
#> [[2]]$associatedDiseases$rows[[17]]$disease$therapeuticAreas[[1]]$name
#> [1] "gastrointestinal disease"
#> 
#> 
#> [[2]]$associatedDiseases$rows[[17]]$disease$therapeuticAreas[[2]]
#> [[2]]$associatedDiseases$rows[[17]]$disease$therapeuticAreas[[2]]$name
#> [1] "immune system disease"
#> 
#> 
#> [[2]]$associatedDiseases$rows[[17]]$disease$therapeuticAreas[[3]]
#> [[2]]$associatedDiseases$rows[[17]]$disease$therapeuticAreas[[3]]$name
#> [1] "genetic, familial or congenital disease"
#> 
#> 
#> 
#> 
#> 
#> [[2]]$associatedDiseases$rows[[18]]
#> [[2]]$associatedDiseases$rows[[18]]$score
#> [1] 0.3143982
#> 
#> [[2]]$associatedDiseases$rows[[18]]$datatypeScores
#> [[2]]$associatedDiseases$rows[[18]]$datatypeScores[[1]]
#> [[2]]$associatedDiseases$rows[[18]]$datatypeScores[[1]]$id
#> [1] "literature"
#> 
#> [[2]]$associatedDiseases$rows[[18]]$datatypeScores[[1]]$score
#> [1] 0.2084864
#> 
#> 
#> [[2]]$associatedDiseases$rows[[18]]$datatypeScores[[2]]
#> [[2]]$associatedDiseases$rows[[18]]$datatypeScores[[2]]$id
#> [1] "genetic_association"
#> 
#> [[2]]$associatedDiseases$rows[[18]]$datatypeScores[[2]]$score
#> [1] 0.5067368
#> 
#> 
#> 
#> [[2]]$associatedDiseases$rows[[18]]$disease
#> [[2]]$associatedDiseases$rows[[18]]$disease$id
#> [1] "EFO_0000707"
#> 
#> [[2]]$associatedDiseases$rows[[18]]$disease$name
#> [1] "squamous cell carcinoma"
#> 
#> [[2]]$associatedDiseases$rows[[18]]$disease$description
#> [1] "A carcinoma arising from squamous epithelial cells. Morphologically, it is characterized by the proliferation of atypical, often pleomorphic squamous cells. Squamous cell carcinomas are graded by the degree of cellular differentiation as well, moderately, or poorly differentiated. Well differentiated carcinomas are usually associated with keratin production and the presence of intercellular bridges between adjacent cells. Representative examples are lung squamous cell carcinoma, skin squamous cell carcinoma, and cervical squamous cell carcinoma."
#> 
#> [[2]]$associatedDiseases$rows[[18]]$disease$therapeuticAreas
#> [[2]]$associatedDiseases$rows[[18]]$disease$therapeuticAreas[[1]]
#> [[2]]$associatedDiseases$rows[[18]]$disease$therapeuticAreas[[1]]$name
#> [1] "cancer or benign tumor"
#> 
#> 
#> 
#> 
#> 
#> [[2]]$associatedDiseases$rows[[19]]
#> [[2]]$associatedDiseases$rows[[19]]$score
#> [1] 0.3097457
#> 
#> [[2]]$associatedDiseases$rows[[19]]$datatypeScores
#> [[2]]$associatedDiseases$rows[[19]]$datatypeScores[[1]]
#> [[2]]$associatedDiseases$rows[[19]]$datatypeScores[[1]]$id
#> [1] "literature"
#> 
#> [[2]]$associatedDiseases$rows[[19]]$datatypeScores[[1]]$score
#> [1] 0.01823792
#> 
#> 
#> [[2]]$associatedDiseases$rows[[19]]$datatypeScores[[2]]
#> [[2]]$associatedDiseases$rows[[19]]$datatypeScores[[2]]$id
#> [1] "genetic_association"
#> 
#> [[2]]$associatedDiseases$rows[[19]]$datatypeScores[[2]]$score
#> [1] 0.5085962
#> 
#> 
#> 
#> [[2]]$associatedDiseases$rows[[19]]$disease
#> [[2]]$associatedDiseases$rows[[19]]$disease$id
#> [1] "MONDO_0043579"
#> 
#> [[2]]$associatedDiseases$rows[[19]]$disease$name
#> [1] "enteritis"
#> 
#> [[2]]$associatedDiseases$rows[[19]]$disease$description
#> [1] "Inflammation of the small intestine."
#> 
#> [[2]]$associatedDiseases$rows[[19]]$disease$therapeuticAreas
#> [[2]]$associatedDiseases$rows[[19]]$disease$therapeuticAreas[[1]]
#> [[2]]$associatedDiseases$rows[[19]]$disease$therapeuticAreas[[1]]$name
#> [1] "gastrointestinal disease"
#> 
#> 
#> 
#> 
#> 
#> [[2]]$associatedDiseases$rows[[20]]
#> [[2]]$associatedDiseases$rows[[20]]$score
#> [1] 0.3080609
#> 
#> [[2]]$associatedDiseases$rows[[20]]$datatypeScores
#> [[2]]$associatedDiseases$rows[[20]]$datatypeScores[[1]]
#> [[2]]$associatedDiseases$rows[[20]]$datatypeScores[[1]]$id
#> [1] "genetic_association"
#> 
#> [[2]]$associatedDiseases$rows[[20]]$datatypeScores[[1]]$score
#> [1] 0.5067368
#> 
#> 
#> 
#> [[2]]$associatedDiseases$rows[[20]]$disease
#> [[2]]$associatedDiseases$rows[[20]]$disease$id
#> [1] "EFO_0002916"
#> 
#> [[2]]$associatedDiseases$rows[[20]]$disease$name
#> [1] "esophageal carcinoma"
#> 
#> [[2]]$associatedDiseases$rows[[20]]$disease$description
#> [1] "Esophageal carcinoma (EC) is a tumor arising in the epithelial cells lining the esophagus and can be divided into two subtypes: esophageal squamous cell carcinoma (ESCC) and esophageal adenocarcinoma (EAC)."
#> 
#> [[2]]$associatedDiseases$rows[[20]]$disease$therapeuticAreas
#> [[2]]$associatedDiseases$rows[[20]]$disease$therapeuticAreas[[1]]
#> [[2]]$associatedDiseases$rows[[20]]$disease$therapeuticAreas[[1]]$name
#> [1] "gastrointestinal disease"
#> 
#> 
#> [[2]]$associatedDiseases$rows[[20]]$disease$therapeuticAreas[[2]]
#> [[2]]$associatedDiseases$rows[[20]]$disease$therapeuticAreas[[2]]$name
#> [1] "cancer or benign tumor"
#> 
#> 
#> 
#> 
#> 
#> [[2]]$associatedDiseases$rows[[21]]
#> [[2]]$associatedDiseases$rows[[21]]$score
#> [1] 0.3061254
#> 
#> [[2]]$associatedDiseases$rows[[21]]$datatypeScores
#> [[2]]$associatedDiseases$rows[[21]]$datatypeScores[[1]]
#> [[2]]$associatedDiseases$rows[[21]]$datatypeScores[[1]]$id
#> [1] "genetic_association"
#> 
#> [[2]]$associatedDiseases$rows[[21]]$datatypeScores[[1]]$score
#> [1] 0.5035531
#> 
#> 
#> 
#> [[2]]$associatedDiseases$rows[[21]]$disease
#> [[2]]$associatedDiseases$rows[[21]]$disease$id
#> [1] "HP_0002024"
#> 
#> [[2]]$associatedDiseases$rows[[21]]$disease$name
#> [1] "Malabsorption"
#> 
#> [[2]]$associatedDiseases$rows[[21]]$disease$description
#> [1] "Impaired ability to absorb one or more nutrients from the intestine."
#> 
#> [[2]]$associatedDiseases$rows[[21]]$disease$therapeuticAreas
#> [[2]]$associatedDiseases$rows[[21]]$disease$therapeuticAreas[[1]]
#> [[2]]$associatedDiseases$rows[[21]]$disease$therapeuticAreas[[1]]$name
#> [1] "phenotype"
#> 
#> 
#> 
#> 
#> 
#> [[2]]$associatedDiseases$rows[[22]]
#> [[2]]$associatedDiseases$rows[[22]]$score
#> [1] 0.2834916
#> 
#> [[2]]$associatedDiseases$rows[[22]]$datatypeScores
#> [[2]]$associatedDiseases$rows[[22]]$datatypeScores[[1]]
#> [[2]]$associatedDiseases$rows[[22]]$datatypeScores[[1]]$id
#> [1] "literature"
#> 
#> [[2]]$associatedDiseases$rows[[22]]$datatypeScores[[1]]$score
#> [1] 0.06079308
#> 
#> 
#> [[2]]$associatedDiseases$rows[[22]]$datatypeScores[[2]]
#> [[2]]$associatedDiseases$rows[[22]]$datatypeScores[[2]]$id
#> [1] "genetic_association"
#> 
#> [[2]]$associatedDiseases$rows[[22]]$datatypeScores[[2]]$score
#> [1] 0.4632826
#> 
#> 
#> 
#> [[2]]$associatedDiseases$rows[[22]]$disease
#> [[2]]$associatedDiseases$rows[[22]]$disease$id
#> [1] "EFO_0004268"
#> 
#> [[2]]$associatedDiseases$rows[[22]]$disease$name
#> [1] "sclerosing cholangitis"
#> 
#> [[2]]$associatedDiseases$rows[[22]]$disease$description
#> [1] "A chronic, autoimmune inflammatory liver disorder characterized by narrowing and scarring of the lumen of the bile ducts. It is often seen in patients with ulcerative colitis. Signs and symptoms include jaundice, fatigue, and malabsorption. It may lead to cirrhosis and liver failure."
#> 
#> [[2]]$associatedDiseases$rows[[22]]$disease$therapeuticAreas
#> [[2]]$associatedDiseases$rows[[22]]$disease$therapeuticAreas[[1]]
#> [[2]]$associatedDiseases$rows[[22]]$disease$therapeuticAreas[[1]]$name
#> [1] "endocrine system disease"
#> 
#> 
#> [[2]]$associatedDiseases$rows[[22]]$disease$therapeuticAreas[[2]]
#> [[2]]$associatedDiseases$rows[[22]]$disease$therapeuticAreas[[2]]$name
#> [1] "gastrointestinal disease"
#> 
#> 
#> [[2]]$associatedDiseases$rows[[22]]$disease$therapeuticAreas[[3]]
#> [[2]]$associatedDiseases$rows[[22]]$disease$therapeuticAreas[[3]]$name
#> [1] "phenotype"
#> 
#> 
#> 
#> 
#> 
#> [[2]]$associatedDiseases$rows[[23]]
#> [[2]]$associatedDiseases$rows[[23]]$score
#> [1] 0.2822905
#> 
#> [[2]]$associatedDiseases$rows[[23]]$datatypeScores
#> [[2]]$associatedDiseases$rows[[23]]$datatypeScores[[1]]
#> [[2]]$associatedDiseases$rows[[23]]$datatypeScores[[1]]$id
#> [1] "literature"
#> 
#> [[2]]$associatedDiseases$rows[[23]]$datatypeScores[[1]]$score
#> [1] 0.02127758
#> 
#> 
#> [[2]]$associatedDiseases$rows[[23]]$datatypeScores[[2]]
#> [[2]]$associatedDiseases$rows[[23]]$datatypeScores[[2]]$id
#> [1] "genetic_association"
#> 
#> [[2]]$associatedDiseases$rows[[23]]$datatypeScores[[2]]$score
#> [1] 0.4632826
#> 
#> 
#> 
#> [[2]]$associatedDiseases$rows[[23]]$disease
#> [[2]]$associatedDiseases$rows[[23]]$disease$id
#> [1] "EFO_0003898"
#> 
#> [[2]]$associatedDiseases$rows[[23]]$disease$name
#> [1] "ankylosing spondylitis"
#> 
#> [[2]]$associatedDiseases$rows[[23]]$disease$description
#> [1] "An autoimmune chronic inflammatory disorder characterized by inflammation in the vertebral joints of the spine and sacroiliac joints. It predominantly affects young males. Patients present with stiffness and pain in the spine."
#> 
#> [[2]]$associatedDiseases$rows[[23]]$disease$therapeuticAreas
#> [[2]]$associatedDiseases$rows[[23]]$disease$therapeuticAreas[[1]]
#> [[2]]$associatedDiseases$rows[[23]]$disease$therapeuticAreas[[1]]$name
#> [1] "immune system disease"
#> 
#> 
#> [[2]]$associatedDiseases$rows[[23]]$disease$therapeuticAreas[[2]]
#> [[2]]$associatedDiseases$rows[[23]]$disease$therapeuticAreas[[2]]$name
#> [1] "musculoskeletal or connective tissue disease"
#> 
#> 
#> [[2]]$associatedDiseases$rows[[23]]$disease$therapeuticAreas[[3]]
#> [[2]]$associatedDiseases$rows[[23]]$disease$therapeuticAreas[[3]]$name
#> [1] "phenotype"
#> 
#> 
#> 
#> 
#> 
#> [[2]]$associatedDiseases$rows[[24]]
#> [[2]]$associatedDiseases$rows[[24]]$score
#> [1] 0.2776587
#> 
#> [[2]]$associatedDiseases$rows[[24]]$datatypeScores
#> [[2]]$associatedDiseases$rows[[24]]$datatypeScores[[1]]
#> [[2]]$associatedDiseases$rows[[24]]$datatypeScores[[1]]$id
#> [1] "literature"
#> 
#> [[2]]$associatedDiseases$rows[[24]]$datatypeScores[[1]]$score
#> [1] 0.07193848
#> 
#> 
#> [[2]]$associatedDiseases$rows[[24]]$datatypeScores[[2]]
#> [[2]]$associatedDiseases$rows[[24]]$datatypeScores[[2]]$id
#> [1] "genetic_association"
#> 
#> [[2]]$associatedDiseases$rows[[24]]$datatypeScores[[2]]$score
#> [1] 0.4531306
#> 
#> 
#> 
#> [[2]]$associatedDiseases$rows[[24]]$disease
#> [[2]]$associatedDiseases$rows[[24]]$disease$id
#> [1] "EFO_0001421"
#> 
#> [[2]]$associatedDiseases$rows[[24]]$disease$name
#> [1] "liver disease"
#> 
#> [[2]]$associatedDiseases$rows[[24]]$disease$description
#> [1] "Pathological processes of the LIVER."
#> 
#> [[2]]$associatedDiseases$rows[[24]]$disease$therapeuticAreas
#> [[2]]$associatedDiseases$rows[[24]]$disease$therapeuticAreas[[1]]
#> [[2]]$associatedDiseases$rows[[24]]$disease$therapeuticAreas[[1]]$name
#> [1] "endocrine system disease"
#> 
#> 
#> [[2]]$associatedDiseases$rows[[24]]$disease$therapeuticAreas[[2]]
#> [[2]]$associatedDiseases$rows[[24]]$disease$therapeuticAreas[[2]]$name
#> [1] "gastrointestinal disease"
#> 
#> 
#> 
#> 
#> 
#> [[2]]$associatedDiseases$rows[[25]]
#> [[2]]$associatedDiseases$rows[[25]]$score
#> [1] 0.2677296
#> 
#> [[2]]$associatedDiseases$rows[[25]]$datatypeScores
#> [[2]]$associatedDiseases$rows[[25]]$datatypeScores[[1]]
#> [[2]]$associatedDiseases$rows[[25]]$datatypeScores[[1]]$id
#> [1] "genetic_association"
#> 
#> [[2]]$associatedDiseases$rows[[25]]$datatypeScores[[1]]$score
#> [1] 0.4403949
#> 
#> 
#> 
#> [[2]]$associatedDiseases$rows[[25]]$disease
#> [[2]]$associatedDiseases$rows[[25]]$disease$id
#> [1] "EFO_0003779"
#> 
#> [[2]]$associatedDiseases$rows[[25]]$disease$name
#> [1] "Hashimoto's thyroiditis"
#> 
#> [[2]]$associatedDiseases$rows[[25]]$disease$description
#> [1] "An autoimmune disorder caused by the production of autoantibodies against thyroid tissue. There is progressive destruction of the thyroid follicles leading to hypothyroidism."
#> 
#> [[2]]$associatedDiseases$rows[[25]]$disease$therapeuticAreas
#> [[2]]$associatedDiseases$rows[[25]]$disease$therapeuticAreas[[1]]
#> [[2]]$associatedDiseases$rows[[25]]$disease$therapeuticAreas[[1]]$name
#> [1] "immune system disease"
#> 
#> 
#> [[2]]$associatedDiseases$rows[[25]]$disease$therapeuticAreas[[2]]
#> [[2]]$associatedDiseases$rows[[25]]$disease$therapeuticAreas[[2]]$name
#> [1] "endocrine system disease"
#> 
#> 
#> 
#> 
#> 
#> 
#> 
#> [[2]]$tractability
#> [[2]]$tractability[[1]]
#> [[2]]$tractability[[1]]$label
#> [1] "Approved Drug"
#> 
#> [[2]]$tractability[[1]]$modality
#> [1] "SM"
#> 
#> [[2]]$tractability[[1]]$value
#> [1] FALSE
#> 
#> 
#> [[2]]$tractability[[2]]
#> [[2]]$tractability[[2]]$label
#> [1] "Advanced Clinical"
#> 
#> [[2]]$tractability[[2]]$modality
#> [1] "SM"
#> 
#> [[2]]$tractability[[2]]$value
#> [1] FALSE
#> 
#> 
#> [[2]]$tractability[[3]]
#> [[2]]$tractability[[3]]$label
#> [1] "Phase 1 Clinical"
#> 
#> [[2]]$tractability[[3]]$modality
#> [1] "SM"
#> 
#> [[2]]$tractability[[3]]$value
#> [1] FALSE
#> 
#> 
#> [[2]]$tractability[[4]]
#> [[2]]$tractability[[4]]$label
#> [1] "Structure with Ligand"
#> 
#> [[2]]$tractability[[4]]$modality
#> [1] "SM"
#> 
#> [[2]]$tractability[[4]]$value
#> [1] TRUE
#> 
#> 
#> [[2]]$tractability[[5]]
#> [[2]]$tractability[[5]]$label
#> [1] "High-Quality Ligand"
#> 
#> [[2]]$tractability[[5]]$modality
#> [1] "SM"
#> 
#> [[2]]$tractability[[5]]$value
#> [1] TRUE
#> 
#> 
#> [[2]]$tractability[[6]]
#> [[2]]$tractability[[6]]$label
#> [1] "High-Quality Pocket"
#> 
#> [[2]]$tractability[[6]]$modality
#> [1] "SM"
#> 
#> [[2]]$tractability[[6]]$value
#> [1] FALSE
#> 
#> 
#> [[2]]$tractability[[7]]
#> [[2]]$tractability[[7]]$label
#> [1] "Med-Quality Pocket"
#> 
#> [[2]]$tractability[[7]]$modality
#> [1] "SM"
#> 
#> [[2]]$tractability[[7]]$value
#> [1] FALSE
#> 
#> 
#> [[2]]$tractability[[8]]
#> [[2]]$tractability[[8]]$label
#> [1] "Druggable Family"
#> 
#> [[2]]$tractability[[8]]$modality
#> [1] "SM"
#> 
#> [[2]]$tractability[[8]]$value
#> [1] TRUE
#> 
#> 
#> [[2]]$tractability[[9]]
#> [[2]]$tractability[[9]]$label
#> [1] "Approved Drug"
#> 
#> [[2]]$tractability[[9]]$modality
#> [1] "AB"
#> 
#> [[2]]$tractability[[9]]$value
#> [1] FALSE
#> 
#> 
#> [[2]]$tractability[[10]]
#> [[2]]$tractability[[10]]$label
#> [1] "Advanced Clinical"
#> 
#> [[2]]$tractability[[10]]$modality
#> [1] "AB"
#> 
#> [[2]]$tractability[[10]]$value
#> [1] FALSE
#> 
#> 
#> [[2]]$tractability[[11]]
#> [[2]]$tractability[[11]]$label
#> [1] "Phase 1 Clinical"
#> 
#> [[2]]$tractability[[11]]$modality
#> [1] "AB"
#> 
#> [[2]]$tractability[[11]]$value
#> [1] FALSE
#> 
#> 
#> [[2]]$tractability[[12]]
#> [[2]]$tractability[[12]]$label
#> [1] "UniProt loc high conf"
#> 
#> [[2]]$tractability[[12]]$modality
#> [1] "AB"
#> 
#> [[2]]$tractability[[12]]$value
#> [1] FALSE
#> 
#> 
#> [[2]]$tractability[[13]]
#> [[2]]$tractability[[13]]$label
#> [1] "GO CC high conf"
#> 
#> [[2]]$tractability[[13]]$modality
#> [1] "AB"
#> 
#> [[2]]$tractability[[13]]$value
#> [1] TRUE
#> 
#> 
#> [[2]]$tractability[[14]]
#> [[2]]$tractability[[14]]$label
#> [1] "UniProt loc med conf"
#> 
#> [[2]]$tractability[[14]]$modality
#> [1] "AB"
#> 
#> [[2]]$tractability[[14]]$value
#> [1] TRUE
#> 
#> 
#> [[2]]$tractability[[15]]
#> [[2]]$tractability[[15]]$label
#> [1] "UniProt SigP or TMHMM"
#> 
#> [[2]]$tractability[[15]]$modality
#> [1] "AB"
#> 
#> [[2]]$tractability[[15]]$value
#> [1] FALSE
#> 
#> 
#> [[2]]$tractability[[16]]
#> [[2]]$tractability[[16]]$label
#> [1] "GO CC med conf"
#> 
#> [[2]]$tractability[[16]]$modality
#> [1] "AB"
#> 
#> [[2]]$tractability[[16]]$value
#> [1] FALSE
#> 
#> 
#> [[2]]$tractability[[17]]
#> [[2]]$tractability[[17]]$label
#> [1] "Human Protein Atlas loc"
#> 
#> [[2]]$tractability[[17]]$modality
#> [1] "AB"
#> 
#> [[2]]$tractability[[17]]$value
#> [1] FALSE
#> 
#> 
#> [[2]]$tractability[[18]]
#> [[2]]$tractability[[18]]$label
#> [1] "Approved Drug"
#> 
#> [[2]]$tractability[[18]]$modality
#> [1] "PR"
#> 
#> [[2]]$tractability[[18]]$value
#> [1] FALSE
#> 
#> 
#> [[2]]$tractability[[19]]
#> [[2]]$tractability[[19]]$label
#> [1] "Advanced Clinical"
#> 
#> [[2]]$tractability[[19]]$modality
#> [1] "PR"
#> 
#> [[2]]$tractability[[19]]$value
#> [1] FALSE
#> 
#> 
#> [[2]]$tractability[[20]]
#> [[2]]$tractability[[20]]$label
#> [1] "Phase 1 Clinical"
#> 
#> [[2]]$tractability[[20]]$modality
#> [1] "PR"
#> 
#> [[2]]$tractability[[20]]$value
#> [1] FALSE
#> 
#> 
#> [[2]]$tractability[[21]]
#> [[2]]$tractability[[21]]$label
#> [1] "Literature"
#> 
#> [[2]]$tractability[[21]]$modality
#> [1] "PR"
#> 
#> [[2]]$tractability[[21]]$value
#> [1] FALSE
#> 
#> 
#> [[2]]$tractability[[22]]
#> [[2]]$tractability[[22]]$label
#> [1] "UniProt Ubiquitination"
#> 
#> [[2]]$tractability[[22]]$modality
#> [1] "PR"
#> 
#> [[2]]$tractability[[22]]$value
#> [1] FALSE
#> 
#> 
#> [[2]]$tractability[[23]]
#> [[2]]$tractability[[23]]$label
#> [1] "Database Ubiquitination"
#> 
#> [[2]]$tractability[[23]]$modality
#> [1] "PR"
#> 
#> [[2]]$tractability[[23]]$value
#> [1] TRUE
#> 
#> 
#> [[2]]$tractability[[24]]
#> [[2]]$tractability[[24]]$label
#> [1] "Half-life Data"
#> 
#> [[2]]$tractability[[24]]$modality
#> [1] "PR"
#> 
#> [[2]]$tractability[[24]]$value
#> [1] TRUE
#> 
#> 
#> [[2]]$tractability[[25]]
#> [[2]]$tractability[[25]]$label
#> [1] "Small Molecule Binder"
#> 
#> [[2]]$tractability[[25]]$modality
#> [1] "PR"
#> 
#> [[2]]$tractability[[25]]$value
#> [1] TRUE
#> 
#> 
#> [[2]]$tractability[[26]]
#> [[2]]$tractability[[26]]$label
#> [1] "Approved Drug"
#> 
#> [[2]]$tractability[[26]]$modality
#> [1] "OC"
#> 
#> [[2]]$tractability[[26]]$value
#> [1] FALSE
#> 
#> 
#> [[2]]$tractability[[27]]
#> [[2]]$tractability[[27]]$label
#> [1] "Advanced Clinical"
#> 
#> [[2]]$tractability[[27]]$modality
#> [1] "OC"
#> 
#> [[2]]$tractability[[27]]$value
#> [1] FALSE
#> 
#> 
#> [[2]]$tractability[[28]]
#> [[2]]$tractability[[28]]$label
#> [1] "Phase 1 Clinical"
#> 
#> [[2]]$tractability[[28]]$modality
#> [1] "OC"
#> 
#> [[2]]$tractability[[28]]$value
#> [1] FALSE
#> 
#> 
#> 
#> [[2]]$geneticConstraint
#> [[2]]$geneticConstraint[[1]]
#> [[2]]$geneticConstraint[[1]]$constraintType
#> [1] "syn"
#> 
#> [[2]]$geneticConstraint[[1]]$exp
#> [1] 125.81
#> 
#> [[2]]$geneticConstraint[[1]]$obs
#> [1] 125
#> 
#> [[2]]$geneticConstraint[[1]]$score
#> [1] 0.039378
#> 
#> [[2]]$geneticConstraint[[1]]$oe
#> [1] 0.99356
#> 
#> [[2]]$geneticConstraint[[1]]$oeLower
#> [1] 0.858
#> 
#> [[2]]$geneticConstraint[[1]]$oeUpper
#> [1] 1.153
#> 
#> 
#> [[2]]$geneticConstraint[[2]]
#> [[2]]$geneticConstraint[[2]]$constraintType
#> [1] "mis"
#> 
#> [[2]]$geneticConstraint[[2]]$exp
#> [1] 362.47
#> 
#> [[2]]$geneticConstraint[[2]]$obs
#> [1] 273
#> 
#> [[2]]$geneticConstraint[[2]]$score
#> [1] 1.7163
#> 
#> [[2]]$geneticConstraint[[2]]$oe
#> [1] 0.75316
#> 
#> [[2]]$geneticConstraint[[2]]$oeLower
#> [1] 0.681
#> 
#> [[2]]$geneticConstraint[[2]]$oeUpper
#> [1] 0.833
#> 
#> 
#> [[2]]$geneticConstraint[[3]]
#> [[2]]$geneticConstraint[[3]]$constraintType
#> [1] "lof"
#> 
#> [[2]]$geneticConstraint[[3]]$exp
#> [1] 33.429
#> 
#> [[2]]$geneticConstraint[[3]]$obs
#> [1] 13
#> 
#> [[2]]$geneticConstraint[[3]]$score
#> [1] 0.51018
#> 
#> [[2]]$geneticConstraint[[3]]$oe
#> [1] 0.38889
#> 
#> [[2]]$geneticConstraint[[3]]$oeLower
#> [1] 0.252
#> 
#> [[2]]$geneticConstraint[[3]]$oeUpper
#> [1] 0.618
#> 
#> 
#> 
#> [[2]]$chemicalProbes
#> [[2]]$chemicalProbes[[1]]
#> [[2]]$chemicalProbes[[1]]$id
#> [1] "ABBV-CLS-484"
#> 
#> [[2]]$chemicalProbes[[1]]$control
#> [1] ""
#> 
#> [[2]]$chemicalProbes[[1]]$drugId
#> [1] "CHEMBL5095164"
#> 
#> [[2]]$chemicalProbes[[1]]$mechanismOfAction
#> [[2]]$chemicalProbes[[1]]$mechanismOfAction[[1]]
#> [1] "inhibitor"
#> 
#> 
#> [[2]]$chemicalProbes[[1]]$isHighQuality
#> [1] TRUE
#> 
#> [[2]]$chemicalProbes[[1]]$urls
#> [[2]]$chemicalProbes[[1]]$urls[[1]]
#> [[2]]$chemicalProbes[[1]]$urls[[1]]$url
#> [1] "http://www.chemicalprobes.org/"
#> 
#> 
#> 
#> 
#> 
#> [[2]]$drugAndClinicalCandidates
#> [[2]]$drugAndClinicalCandidates$rows
#> list()
#> 
#> 
#> 
```
