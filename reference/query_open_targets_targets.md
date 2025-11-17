# Query Open Targets - Targets

Using Open Target's GraphQL API return gene-metadata and
disease/phenotype associations for a set of genes.

## Usage

``` r
query_open_targets_targets(target_ensembl_gene_ids, post_n_genes = 10)
```

## Arguments

- target_ensembl_gene_ids:

  a character vector of ensembl gene names

- post_n_genes:

  separate queries into posts of this number of genes

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
#> [1] 155
#> 
#> [[1]]$associatedDiseases$rows
#> [[1]]$associatedDiseases$rows[[1]]
#> [[1]]$associatedDiseases$rows[[1]]$score
#> [1] 0.7745653
#> 
#> [[1]]$associatedDiseases$rows[[1]]$datatypeScores
#> [[1]]$associatedDiseases$rows[[1]]$datatypeScores[[1]]
#> [[1]]$associatedDiseases$rows[[1]]$datatypeScores[[1]]$id
#> [1] "genetic_association"
#> 
#> [[1]]$associatedDiseases$rows[[1]]$datatypeScores[[1]]$score
#> [1] 0.8949783
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
#> [1] 0.6733895
#> 
#> [[1]]$associatedDiseases$rows[[2]]$datatypeScores
#> [[1]]$associatedDiseases$rows[[2]]$datatypeScores[[1]]
#> [[1]]$associatedDiseases$rows[[2]]$datatypeScores[[1]]$id
#> [1] "literature"
#> 
#> [[1]]$associatedDiseases$rows[[2]]$datatypeScores[[1]]$score
#> [1] 0.193249
#> 
#> 
#> [[1]]$associatedDiseases$rows[[2]]$datatypeScores[[2]]
#> [[1]]$associatedDiseases$rows[[2]]$datatypeScores[[2]]$id
#> [1] "genetic_association"
#> 
#> [[1]]$associatedDiseases$rows[[2]]$datatypeScores[[2]]$score
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
#> [1] "A new leukoencephalopathy, the CACH syndrome (Childhood Ataxia with Central nervous system Hypomyelination) or VWM (Vanishing White Matter) was identified on clinical and MRI criteria. Classically, this disease is characterized by (1) an onset between 2 and 5 years of age, with a cerebello-spastic syndrome exacerbated by episodes of fever or head trauma leading to death after 5 to 10 years of disease evolution, (2) a diffuse involvement of the white matter on cerebral MRI with a CSF-like signal intensity (cavitation), (3) a recessive autosomal mode of inheritance, (4) neuropathologic findings consistent with a cavitating orthochromatic leukodystrophy with increased number of oligodendrocytes with sometimes \"foamy'' aspect."
#> 
#> [[1]]$associatedDiseases$rows[[2]]$disease$therapeuticAreas
#> [[1]]$associatedDiseases$rows[[2]]$disease$therapeuticAreas[[1]]
#> [[1]]$associatedDiseases$rows[[2]]$disease$therapeuticAreas[[1]]$name
#> [1] "genetic, familial or congenital disease"
#> 
#> 
#> [[1]]$associatedDiseases$rows[[2]]$disease$therapeuticAreas[[2]]
#> [[1]]$associatedDiseases$rows[[2]]$disease$therapeuticAreas[[2]]$name
#> [1] "nervous system disease"
#> 
#> 
#> 
#> 
#> 
#> [[1]]$associatedDiseases$rows[[3]]
#> [[1]]$associatedDiseases$rows[[3]]$score
#> [1] 0.597687
#> 
#> [[1]]$associatedDiseases$rows[[3]]$datatypeScores
#> [[1]]$associatedDiseases$rows[[3]]$datatypeScores[[1]]
#> [[1]]$associatedDiseases$rows[[3]]$datatypeScores[[1]]$id
#> [1] "literature"
#> 
#> [[1]]$associatedDiseases$rows[[3]]$datatypeScores[[1]]$score
#> [1] 0.1667585
#> 
#> 
#> [[1]]$associatedDiseases$rows[[3]]$datatypeScores[[2]]
#> [[1]]$associatedDiseases$rows[[3]]$datatypeScores[[2]]$id
#> [1] "genetic_association"
#> 
#> [[1]]$associatedDiseases$rows[[3]]$datatypeScores[[2]]$score
#> [1] 0.7835553
#> 
#> 
#> 
#> [[1]]$associatedDiseases$rows[[3]]$disease
#> [[1]]$associatedDiseases$rows[[3]]$disease$id
#> [1] "EFO_0700130"
#> 
#> [[1]]$associatedDiseases$rows[[3]]$disease$name
#> [1] "ovarioleukodystrophy"
#> 
#> [[1]]$associatedDiseases$rows[[3]]$disease$description
#> [1] "The \"ovarioleukodystrophies\" comprise a group of rare leukodystrophies associated with primary or premature ovarian failure."
#> 
#> [[1]]$associatedDiseases$rows[[3]]$disease$therapeuticAreas
#> [[1]]$associatedDiseases$rows[[3]]$disease$therapeuticAreas[[1]]
#> [[1]]$associatedDiseases$rows[[3]]$disease$therapeuticAreas[[1]]$name
#> [1] "genetic, familial or congenital disease"
#> 
#> 
#> [[1]]$associatedDiseases$rows[[3]]$disease$therapeuticAreas[[2]]
#> [[1]]$associatedDiseases$rows[[3]]$disease$therapeuticAreas[[2]]$name
#> [1] "disorder of visual system"
#> 
#> 
#> 
#> 
#> 
#> [[1]]$associatedDiseases$rows[[4]]
#> [[1]]$associatedDiseases$rows[[4]]$score
#> [1] 0.5507269
#> 
#> [[1]]$associatedDiseases$rows[[4]]$datatypeScores
#> [[1]]$associatedDiseases$rows[[4]]$datatypeScores[[1]]
#> [[1]]$associatedDiseases$rows[[4]]$datatypeScores[[1]]$id
#> [1] "literature"
#> 
#> [[1]]$associatedDiseases$rows[[4]]$datatypeScores[[1]]$score
#> [1] 0.0802735
#> 
#> 
#> [[1]]$associatedDiseases$rows[[4]]$datatypeScores[[2]]
#> [[1]]$associatedDiseases$rows[[4]]$datatypeScores[[2]]$id
#> [1] "affected_pathway"
#> 
#> [[1]]$associatedDiseases$rows[[4]]$datatypeScores[[2]]$score
#> [1] 0.9018902
#> 
#> 
#> 
#> [[1]]$associatedDiseases$rows[[4]]$disease
#> [[1]]$associatedDiseases$rows[[4]]$disease$id
#> [1] "EFO_0005772"
#> 
#> [[1]]$associatedDiseases$rows[[4]]$disease$name
#> [1] "neurodegenerative disease"
#> 
#> [[1]]$associatedDiseases$rows[[4]]$disease$description
#> [1] "A disorder of the central nervous system characterized by gradual and progressive loss of neural tissue and neurologic function."
#> 
#> [[1]]$associatedDiseases$rows[[4]]$disease$therapeuticAreas
#> [[1]]$associatedDiseases$rows[[4]]$disease$therapeuticAreas[[1]]
#> [[1]]$associatedDiseases$rows[[4]]$disease$therapeuticAreas[[1]]$name
#> [1] "nervous system disease"
#> 
#> 
#> 
#> 
#> 
#> [[1]]$associatedDiseases$rows[[5]]
#> [[1]]$associatedDiseases$rows[[5]]$score
#> [1] 0.5040614
#> 
#> [[1]]$associatedDiseases$rows[[5]]$datatypeScores
#> [[1]]$associatedDiseases$rows[[5]]$datatypeScores[[1]]
#> [[1]]$associatedDiseases$rows[[5]]$datatypeScores[[1]]$id
#> [1] "literature"
#> 
#> [[1]]$associatedDiseases$rows[[5]]$datatypeScores[[1]]$score
#> [1] 0.1345052
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
#> 
#> [[1]]$associatedDiseases$rows[[5]]$disease
#> [[1]]$associatedDiseases$rows[[5]]$disease$id
#> [1] "MONDO_0020507"
#> 
#> [[1]]$associatedDiseases$rows[[5]]$disease$name
#> [1] "leukoencephalopathy with vanishing white matter 1"
#> 
#> [[1]]$associatedDiseases$rows[[5]]$disease$description
#> [1] "Any leukoencephalopathy with vanishing white matter in which the cause of the disease is a variation in the EIF2B1 gene."
#> 
#> [[1]]$associatedDiseases$rows[[5]]$disease$therapeuticAreas
#> [[1]]$associatedDiseases$rows[[5]]$disease$therapeuticAreas[[1]]
#> [[1]]$associatedDiseases$rows[[5]]$disease$therapeuticAreas[[1]]$name
#> [1] "genetic, familial or congenital disease"
#> 
#> 
#> [[1]]$associatedDiseases$rows[[5]]$disease$therapeuticAreas[[2]]
#> [[1]]$associatedDiseases$rows[[5]]$disease$therapeuticAreas[[2]]$name
#> [1] "nervous system disease"
#> 
#> 
#> 
#> 
#> 
#> [[1]]$associatedDiseases$rows[[6]]
#> [[1]]$associatedDiseases$rows[[6]]$score
#> [1] 0.4161469
#> 
#> [[1]]$associatedDiseases$rows[[6]]$datatypeScores
#> [[1]]$associatedDiseases$rows[[6]]$datatypeScores[[1]]
#> [[1]]$associatedDiseases$rows[[6]]$datatypeScores[[1]]$id
#> [1] "literature"
#> 
#> [[1]]$associatedDiseases$rows[[6]]$datatypeScores[[1]]$score
#> [1] 0.01215862
#> 
#> 
#> [[1]]$associatedDiseases$rows[[6]]$datatypeScores[[2]]
#> [[1]]$associatedDiseases$rows[[6]]$datatypeScores[[2]]$id
#> [1] "genetic_association"
#> 
#> [[1]]$associatedDiseases$rows[[6]]$datatypeScores[[2]]$score
#> [1] 0.6839221
#> 
#> 
#> 
#> [[1]]$associatedDiseases$rows[[6]]$disease
#> [[1]]$associatedDiseases$rows[[6]]$disease$id
#> [1] "HP_0008209"
#> 
#> [[1]]$associatedDiseases$rows[[6]]$disease$name
#> [1] "Premature ovarian insufficiency"
#> 
#> [[1]]$associatedDiseases$rows[[6]]$disease$description
#> [1] "Amenorrhea due to loss of ovarian function before the age of 40. Primary ovarian insuficiency (POI) is a state of female hypergonadotropic hypogonadism. It can manifest as primary amenorrhea with onset before menarche or secondary amenorrhea."
#> 
#> [[1]]$associatedDiseases$rows[[6]]$disease$therapeuticAreas
#> [[1]]$associatedDiseases$rows[[6]]$disease$therapeuticAreas[[1]]
#> [[1]]$associatedDiseases$rows[[6]]$disease$therapeuticAreas[[1]]$name
#> [1] "phenotype"
#> 
#> 
#> 
#> 
#> 
#> [[1]]$associatedDiseases$rows[[7]]
#> [[1]]$associatedDiseases$rows[[7]]$score
#> [1] 0.3695799
#> 
#> [[1]]$associatedDiseases$rows[[7]]$datatypeScores
#> [[1]]$associatedDiseases$rows[[7]]$datatypeScores[[1]]
#> [[1]]$associatedDiseases$rows[[7]]$datatypeScores[[1]]$id
#> [1] "genetic_association"
#> 
#> [[1]]$associatedDiseases$rows[[7]]$datatypeScores[[1]]$score
#> [1] 0.6079308
#> 
#> 
#> 
#> [[1]]$associatedDiseases$rows[[7]]$disease
#> [[1]]$associatedDiseases$rows[[7]]$disease$id
#> [1] "MONDO_0015520"
#> 
#> [[1]]$associatedDiseases$rows[[7]]$disease$name
#> [1] "late infantile CACH syndrome"
#> 
#> [[1]]$associatedDiseases$rows[[7]]$disease$description
#> NULL
#> 
#> [[1]]$associatedDiseases$rows[[7]]$disease$therapeuticAreas
#> [[1]]$associatedDiseases$rows[[7]]$disease$therapeuticAreas[[1]]
#> [[1]]$associatedDiseases$rows[[7]]$disease$therapeuticAreas[[1]]$name
#> [1] "genetic, familial or congenital disease"
#> 
#> 
#> [[1]]$associatedDiseases$rows[[7]]$disease$therapeuticAreas[[2]]
#> [[1]]$associatedDiseases$rows[[7]]$disease$therapeuticAreas[[2]]$name
#> [1] "nervous system disease"
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
#> [1] "MONDO_0015521"
#> 
#> [[1]]$associatedDiseases$rows[[8]]$disease$name
#> [1] "juvenile or adult CACH syndrome"
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
#> [1] "MONDO_0015519"
#> 
#> [[1]]$associatedDiseases$rows[[9]]$disease$name
#> [1] "congenital or early infantile CACH syndrome"
#> 
#> [[1]]$associatedDiseases$rows[[9]]$disease$description
#> NULL
#> 
#> [[1]]$associatedDiseases$rows[[9]]$disease$therapeuticAreas
#> [[1]]$associatedDiseases$rows[[9]]$disease$therapeuticAreas[[1]]
#> [[1]]$associatedDiseases$rows[[9]]$disease$therapeuticAreas[[1]]$name
#> [1] "genetic, familial or congenital disease"
#> 
#> 
#> [[1]]$associatedDiseases$rows[[9]]$disease$therapeuticAreas[[2]]
#> [[1]]$associatedDiseases$rows[[9]]$disease$therapeuticAreas[[2]]$name
#> [1] "nervous system disease"
#> 
#> 
#> 
#> 
#> 
#> [[1]]$associatedDiseases$rows[[10]]
#> [[1]]$associatedDiseases$rows[[10]]$score
#> [1] 0.3400135
#> 
#> [[1]]$associatedDiseases$rows[[10]]$datatypeScores
#> [[1]]$associatedDiseases$rows[[10]]$datatypeScores[[1]]
#> [[1]]$associatedDiseases$rows[[10]]$datatypeScores[[1]]$id
#> [1] "genetic_association"
#> 
#> [[1]]$associatedDiseases$rows[[10]]$datatypeScores[[1]]$score
#> [1] 0.5592963
#> 
#> 
#> 
#> [[1]]$associatedDiseases$rows[[10]]$disease
#> [[1]]$associatedDiseases$rows[[10]]$disease$id
#> [1] "MONDO_0019019"
#> 
#> [[1]]$associatedDiseases$rows[[10]]$disease$name
#> [1] "osteogenesis imperfecta"
#> 
#> [[1]]$associatedDiseases$rows[[10]]$disease$description
#> [1] "Osteogenesis imperfecta (OI) comprises a heterogeneous group of genetic disorders characterized by increased bone fragility, low bone mass, and susceptibility to bone fractures with variable severity."
#> 
#> [[1]]$associatedDiseases$rows[[10]]$disease$therapeuticAreas
#> [[1]]$associatedDiseases$rows[[10]]$disease$therapeuticAreas[[1]]
#> [[1]]$associatedDiseases$rows[[10]]$disease$therapeuticAreas[[1]]$name
#> [1] "musculoskeletal or connective tissue disease"
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
#> [1] 0.3382305
#> 
#> [[1]]$associatedDiseases$rows[[11]]$datatypeScores
#> [[1]]$associatedDiseases$rows[[11]]$datatypeScores[[1]]
#> [[1]]$associatedDiseases$rows[[11]]$datatypeScores[[1]]$id
#> [1] "genetic_association"
#> 
#> [[1]]$associatedDiseases$rows[[11]]$datatypeScores[[1]]$score
#> [1] 0.5563635
#> 
#> 
#> 
#> [[1]]$associatedDiseases$rows[[11]]$disease
#> [[1]]$associatedDiseases$rows[[11]]$disease$id
#> [1] "EFO_0000508"
#> 
#> [[1]]$associatedDiseases$rows[[11]]$disease$name
#> [1] "genetic disorder"
#> 
#> [[1]]$associatedDiseases$rows[[11]]$disease$description
#> [1] "Genetic diseases are diseases in which inherited genes predispose to increased risk. The genetic disorders associated with cancer often result from an alteration or mutation in a single gene. The diseases range from rare dominant cancer family syndrome to familial tendencies in which low-penetrance genes may interact with other genes or environmental factors to induce cancer. Research may involve clinical, epidemiologic, and laboratory studies of persons, families, and populations at high risk of these disorders."
#> 
#> [[1]]$associatedDiseases$rows[[11]]$disease$therapeuticAreas
#> [[1]]$associatedDiseases$rows[[11]]$disease$therapeuticAreas[[1]]
#> [[1]]$associatedDiseases$rows[[11]]$disease$therapeuticAreas[[1]]$name
#> [1] "genetic, familial or congenital disease"
#> 
#> 
#> 
#> 
#> 
#> [[1]]$associatedDiseases$rows[[12]]
#> [[1]]$associatedDiseases$rows[[12]]$score
#> [1] 0.3326219
#> 
#> [[1]]$associatedDiseases$rows[[12]]$datatypeScores
#> [[1]]$associatedDiseases$rows[[12]]$datatypeScores[[1]]
#> [[1]]$associatedDiseases$rows[[12]]$datatypeScores[[1]]$id
#> [1] "genetic_association"
#> 
#> [[1]]$associatedDiseases$rows[[12]]$datatypeScores[[1]]$score
#> [1] 0.5471377
#> 
#> 
#> 
#> [[1]]$associatedDiseases$rows[[12]]$disease
#> [[1]]$associatedDiseases$rows[[12]]$disease$id
#> [1] "HP_0000707"
#> 
#> [[1]]$associatedDiseases$rows[[12]]$disease$name
#> [1] "Abnormality of the nervous system"
#> 
#> [[1]]$associatedDiseases$rows[[12]]$disease$description
#> [1] "An abnormality of the nervous system."
#> 
#> [[1]]$associatedDiseases$rows[[12]]$disease$therapeuticAreas
#> [[1]]$associatedDiseases$rows[[12]]$disease$therapeuticAreas[[1]]
#> [[1]]$associatedDiseases$rows[[12]]$disease$therapeuticAreas[[1]]$name
#> [1] "phenotype"
#> 
#> 
#> 
#> 
#> 
#> [[1]]$associatedDiseases$rows[[13]]
#> [[1]]$associatedDiseases$rows[[13]]$score
#> [1] 0.2930104
#> 
#> [[1]]$associatedDiseases$rows[[13]]$datatypeScores
#> [[1]]$associatedDiseases$rows[[13]]$datatypeScores[[1]]
#> [[1]]$associatedDiseases$rows[[13]]$datatypeScores[[1]]$id
#> [1] "literature"
#> 
#> [[1]]$associatedDiseases$rows[[13]]$datatypeScores[[1]]$score
#> [1] 0.0729517
#> 
#> 
#> [[1]]$associatedDiseases$rows[[13]]$datatypeScores[[2]]
#> [[1]]$associatedDiseases$rows[[13]]$datatypeScores[[2]]$id
#> [1] "genetic_association"
#> 
#> [[1]]$associatedDiseases$rows[[13]]$datatypeScores[[2]]$score
#> [1] 0.4783322
#> 
#> 
#> 
#> [[1]]$associatedDiseases$rows[[13]]$disease
#> [[1]]$associatedDiseases$rows[[13]]$disease$id
#> [1] "EFO_0001645"
#> 
#> [[1]]$associatedDiseases$rows[[13]]$disease$name
#> [1] "coronary artery disease"
#> 
#> [[1]]$associatedDiseases$rows[[13]]$disease$description
#> [1] "Thickening and loss of elasticity of the CORONARY ARTERIES, leading to progressive arterial insufficiency (CORONARY DISEASE)."
#> 
#> [[1]]$associatedDiseases$rows[[13]]$disease$therapeuticAreas
#> [[1]]$associatedDiseases$rows[[13]]$disease$therapeuticAreas[[1]]
#> [[1]]$associatedDiseases$rows[[13]]$disease$therapeuticAreas[[1]]$name
#> [1] "cardiovascular disease"
#> 
#> 
#> 
#> 
#> 
#> [[1]]$associatedDiseases$rows[[14]]
#> [[1]]$associatedDiseases$rows[[14]]$score
#> [1] 0.2878425
#> 
#> [[1]]$associatedDiseases$rows[[14]]$datatypeScores
#> [[1]]$associatedDiseases$rows[[14]]$datatypeScores[[1]]
#> [[1]]$associatedDiseases$rows[[14]]$datatypeScores[[1]]$id
#> [1] "affected_pathway"
#> 
#> [[1]]$associatedDiseases$rows[[14]]$datatypeScores[[1]]$score
#> [1] 0.473479
#> 
#> 
#> 
#> [[1]]$associatedDiseases$rows[[14]]$disease
#> [[1]]$associatedDiseases$rows[[14]]$disease$id
#> [1] "MONDO_0002561"
#> 
#> [[1]]$associatedDiseases$rows[[14]]$disease$name
#> [1] "lysosomal storage disease"
#> 
#> [[1]]$associatedDiseases$rows[[14]]$disease$description
#> [1] "A metabolic disorder caused by mutations in proteins critical for lysosomal function, including lysosomal enzymes, lysosomal integral membrane proteins, and proteins involved in the post-translational modification and trafficking of lysosomal proteins."
#> 
#> [[1]]$associatedDiseases$rows[[14]]$disease$therapeuticAreas
#> [[1]]$associatedDiseases$rows[[14]]$disease$therapeuticAreas[[1]]
#> [[1]]$associatedDiseases$rows[[14]]$disease$therapeuticAreas[[1]]$name
#> [1] "nutritional or metabolic disease"
#> 
#> 
#> [[1]]$associatedDiseases$rows[[14]]$disease$therapeuticAreas[[2]]
#> [[1]]$associatedDiseases$rows[[14]]$disease$therapeuticAreas[[2]]$name
#> [1] "genetic, familial or congenital disease"
#> 
#> 
#> 
#> 
#> 
#> [[1]]$associatedDiseases$rows[[15]]
#> [[1]]$associatedDiseases$rows[[15]]$score
#> [1] 0.2569327
#> 
#> [[1]]$associatedDiseases$rows[[15]]$datatypeScores
#> [[1]]$associatedDiseases$rows[[15]]$datatypeScores[[1]]
#> [[1]]$associatedDiseases$rows[[15]]$datatypeScores[[1]]$id
#> [1] "genetic_association"
#> 
#> [[1]]$associatedDiseases$rows[[15]]$datatypeScores[[1]]$score
#> [1] 0.4226348
#> 
#> 
#> 
#> [[1]]$associatedDiseases$rows[[15]]$disease
#> [[1]]$associatedDiseases$rows[[15]]$disease$id
#> [1] "OBA_1000110"
#> 
#> [[1]]$associatedDiseases$rows[[15]]$disease$name
#> [1] "bone tissue density"
#> 
#> [[1]]$associatedDiseases$rows[[15]]$disease$description
#> [1] "The mass density of a bone tissue."
#> 
#> [[1]]$associatedDiseases$rows[[15]]$disease$therapeuticAreas
#> [[1]]$associatedDiseases$rows[[15]]$disease$therapeuticAreas[[1]]
#> [[1]]$associatedDiseases$rows[[15]]$disease$therapeuticAreas[[1]]$name
#> [1] "measurement"
#> 
#> 
#> 
#> 
#> 
#> [[1]]$associatedDiseases$rows[[16]]
#> [[1]]$associatedDiseases$rows[[16]]$score
#> [1] 0.1896323
#> 
#> [[1]]$associatedDiseases$rows[[16]]$datatypeScores
#> [[1]]$associatedDiseases$rows[[16]]$datatypeScores[[1]]
#> [[1]]$associatedDiseases$rows[[16]]$datatypeScores[[1]]$id
#> [1] "genetic_association"
#> 
#> [[1]]$associatedDiseases$rows[[16]]$datatypeScores[[1]]$score
#> [1] 0.2495446
#> 
#> 
#> 
#> [[1]]$associatedDiseases$rows[[16]]$disease
#> [[1]]$associatedDiseases$rows[[16]]$disease$id
#> [1] "EFO_0004527"
#> 
#> [[1]]$associatedDiseases$rows[[16]]$disease$name
#> [1] "mean corpuscular hemoglobin"
#> 
#> [[1]]$associatedDiseases$rows[[16]]$disease$description
#> [1] "The MCH is  the average mass of hemoglobin per red blood cell in a sample of blood and is calculated by dividing the total mass of hemoglobin by the RBC count"
#> 
#> [[1]]$associatedDiseases$rows[[16]]$disease$therapeuticAreas
#> [[1]]$associatedDiseases$rows[[16]]$disease$therapeuticAreas[[1]]
#> [[1]]$associatedDiseases$rows[[16]]$disease$therapeuticAreas[[1]]$name
#> [1] "measurement"
#> 
#> 
#> 
#> 
#> 
#> [[1]]$associatedDiseases$rows[[17]]
#> [[1]]$associatedDiseases$rows[[17]]$score
#> [1] 0.1831362
#> 
#> [[1]]$associatedDiseases$rows[[17]]$datatypeScores
#> [[1]]$associatedDiseases$rows[[17]]$datatypeScores[[1]]
#> [[1]]$associatedDiseases$rows[[17]]$datatypeScores[[1]]$id
#> [1] "genetic_association"
#> 
#> [[1]]$associatedDiseases$rows[[17]]$datatypeScores[[1]]$score
#> [1] 0.3012452
#> 
#> 
#> 
#> [[1]]$associatedDiseases$rows[[17]]$disease
#> [[1]]$associatedDiseases$rows[[17]]$disease$id
#> [1] "EFO_0010626"
#> 
#> [[1]]$associatedDiseases$rows[[17]]$disease$name
#> [1] "placenta growth factor measurement"
#> 
#> [[1]]$associatedDiseases$rows[[17]]$disease$description
#> [1] "quantification of placenta growth factor in a sample"
#> 
#> [[1]]$associatedDiseases$rows[[17]]$disease$therapeuticAreas
#> [[1]]$associatedDiseases$rows[[17]]$disease$therapeuticAreas[[1]]
#> [[1]]$associatedDiseases$rows[[17]]$disease$therapeuticAreas[[1]]$name
#> [1] "measurement"
#> 
#> 
#> 
#> 
#> 
#> [[1]]$associatedDiseases$rows[[18]]
#> [[1]]$associatedDiseases$rows[[18]]$score
#> [1] 0.1511099
#> 
#> [[1]]$associatedDiseases$rows[[18]]$datatypeScores
#> [[1]]$associatedDiseases$rows[[18]]$datatypeScores[[1]]
#> [[1]]$associatedDiseases$rows[[18]]$datatypeScores[[1]]$id
#> [1] "genetic_association"
#> 
#> [[1]]$associatedDiseases$rows[[18]]$datatypeScores[[1]]$score
#> [1] 0.2485642
#> 
#> 
#> 
#> [[1]]$associatedDiseases$rows[[18]]$disease
#> [[1]]$associatedDiseases$rows[[18]]$disease$id
#> [1] "OBA_0003460"
#> 
#> [[1]]$associatedDiseases$rows[[18]]$disease$name
#> [1] "erythrocyte volume"
#> 
#> [[1]]$associatedDiseases$rows[[18]]$disease$description
#> [1] "The volume of a erythrocyte."
#> 
#> [[1]]$associatedDiseases$rows[[18]]$disease$therapeuticAreas
#> [[1]]$associatedDiseases$rows[[18]]$disease$therapeuticAreas[[1]]
#> [[1]]$associatedDiseases$rows[[18]]$disease$therapeuticAreas[[1]]$name
#> [1] "measurement"
#> 
#> 
#> 
#> 
#> 
#> [[1]]$associatedDiseases$rows[[19]]
#> [[1]]$associatedDiseases$rows[[19]]$score
#> [1] 0.1359433
#> 
#> [[1]]$associatedDiseases$rows[[19]]$datatypeScores
#> [[1]]$associatedDiseases$rows[[19]]$datatypeScores[[1]]
#> [[1]]$associatedDiseases$rows[[19]]$datatypeScores[[1]]$id
#> [1] "genetic_association"
#> 
#> [[1]]$associatedDiseases$rows[[19]]$datatypeScores[[1]]$score
#> [1] 0.2236163
#> 
#> 
#> 
#> [[1]]$associatedDiseases$rows[[19]]$disease
#> [[1]]$associatedDiseases$rows[[19]]$disease$id
#> [1] "OBA_0005494"
#> 
#> [[1]]$associatedDiseases$rows[[19]]$disease$name
#> [1] "aging rate"
#> 
#> [[1]]$associatedDiseases$rows[[19]]$disease$description
#> [1] "The rate of the aging process."
#> 
#> [[1]]$associatedDiseases$rows[[19]]$disease$therapeuticAreas
#> [[1]]$associatedDiseases$rows[[19]]$disease$therapeuticAreas[[1]]
#> [[1]]$associatedDiseases$rows[[19]]$disease$therapeuticAreas[[1]]$name
#> [1] "measurement"
#> 
#> 
#> 
#> 
#> 
#> [[1]]$associatedDiseases$rows[[20]]
#> [[1]]$associatedDiseases$rows[[20]]$score
#> [1] 0.1342753
#> 
#> [[1]]$associatedDiseases$rows[[20]]$datatypeScores
#> [[1]]$associatedDiseases$rows[[20]]$datatypeScores[[1]]
#> [[1]]$associatedDiseases$rows[[20]]$datatypeScores[[1]]$id
#> [1] "genetic_association"
#> 
#> [[1]]$associatedDiseases$rows[[20]]$datatypeScores[[1]]$score
#> [1] 0.2208727
#> 
#> 
#> 
#> [[1]]$associatedDiseases$rows[[20]]$disease
#> [[1]]$associatedDiseases$rows[[20]]$disease$id
#> [1] "EFO_0004627"
#> 
#> [[1]]$associatedDiseases$rows[[20]]$disease$name
#> [1] "IGF-1 measurement"
#> 
#> [[1]]$associatedDiseases$rows[[20]]$disease$description
#> [1] "Is the quantification of Insulin-like growth factor 1 (IGF-1), also called somatomedin C. IGF-1 is a hormone similar in molecular structure to insulin. It plays an important role in childhood growth and continues to have anabolic effects in adults. Levels of IGF-1 are known to increase in some cancers."
#> 
#> [[1]]$associatedDiseases$rows[[20]]$disease$therapeuticAreas
#> [[1]]$associatedDiseases$rows[[20]]$disease$therapeuticAreas[[1]]
#> [[1]]$associatedDiseases$rows[[20]]$disease$therapeuticAreas[[1]]$name
#> [1] "measurement"
#> 
#> 
#> 
#> 
#> 
#> [[1]]$associatedDiseases$rows[[21]]
#> [[1]]$associatedDiseases$rows[[21]]$score
#> [1] 0.09813727
#> 
#> [[1]]$associatedDiseases$rows[[21]]$datatypeScores
#> [[1]]$associatedDiseases$rows[[21]]$datatypeScores[[1]]
#> [[1]]$associatedDiseases$rows[[21]]$datatypeScores[[1]]$id
#> [1] "genetic_association"
#> 
#> [[1]]$associatedDiseases$rows[[21]]$datatypeScores[[1]]$score
#> [1] 0.1614284
#> 
#> 
#> 
#> [[1]]$associatedDiseases$rows[[21]]$disease
#> [[1]]$associatedDiseases$rows[[21]]$disease$id
#> [1] "EFO_0009270"
#> 
#> [[1]]$associatedDiseases$rows[[21]]$disease$name
#> [1] "heel bone mineral density"
#> 
#> [[1]]$associatedDiseases$rows[[21]]$disease$description
#> [1] "Quantification of the mineral density of the heel bone"
#> 
#> [[1]]$associatedDiseases$rows[[21]]$disease$therapeuticAreas
#> [[1]]$associatedDiseases$rows[[21]]$disease$therapeuticAreas[[1]]
#> [[1]]$associatedDiseases$rows[[21]]$disease$therapeuticAreas[[1]]$name
#> [1] "measurement"
#> 
#> 
#> 
#> 
#> 
#> [[1]]$associatedDiseases$rows[[22]]
#> [[1]]$associatedDiseases$rows[[22]]$score
#> [1] 0.07398726
#> 
#> [[1]]$associatedDiseases$rows[[22]]$datatypeScores
#> [[1]]$associatedDiseases$rows[[22]]$datatypeScores[[1]]
#> [[1]]$associatedDiseases$rows[[22]]$datatypeScores[[1]]$id
#> [1] "genetic_association"
#> 
#> [[1]]$associatedDiseases$rows[[22]]$datatypeScores[[1]]$score
#> [1] 0.1217034
#> 
#> 
#> 
#> [[1]]$associatedDiseases$rows[[22]]$disease
#> [[1]]$associatedDiseases$rows[[22]]$disease$id
#> [1] "EFO_0004213"
#> 
#> [[1]]$associatedDiseases$rows[[22]]$disease$name
#> [1] "otosclerosis"
#> 
#> [[1]]$associatedDiseases$rows[[22]]$disease$description
#> [1] "Formation of spongy bone in the labyrinth capsule which can progress toward the stapes (stapedial fixation) or anteriorly toward the cochlea leading to conductive, sensorineural, or mixed hearing loss. Several genes are associated with familial otosclerosis with varied clinical signs."
#> 
#> [[1]]$associatedDiseases$rows[[22]]$disease$therapeuticAreas
#> [[1]]$associatedDiseases$rows[[22]]$disease$therapeuticAreas[[1]]
#> [[1]]$associatedDiseases$rows[[22]]$disease$therapeuticAreas[[1]]$name
#> [1] "disorder of ear"
#> 
#> 
#> [[1]]$associatedDiseases$rows[[22]]$disease$therapeuticAreas[[2]]
#> [[1]]$associatedDiseases$rows[[22]]$disease$therapeuticAreas[[2]]$name
#> [1] "nervous system disease"
#> 
#> 
#> [[1]]$associatedDiseases$rows[[22]]$disease$therapeuticAreas[[3]]
#> [[1]]$associatedDiseases$rows[[22]]$disease$therapeuticAreas[[3]]$name
#> [1] "genetic, familial or congenital disease"
#> 
#> 
#> 
#> 
#> 
#> [[1]]$associatedDiseases$rows[[23]]
#> [[1]]$associatedDiseases$rows[[23]]$score
#> [1] 0.06788977
#> 
#> [[1]]$associatedDiseases$rows[[23]]$datatypeScores
#> [[1]]$associatedDiseases$rows[[23]]$datatypeScores[[1]]
#> [[1]]$associatedDiseases$rows[[23]]$datatypeScores[[1]]$id
#> [1] "literature"
#> 
#> [[1]]$associatedDiseases$rows[[23]]$datatypeScores[[1]]$score
#> [1] 0.5583676
#> 
#> 
#> 
#> [[1]]$associatedDiseases$rows[[23]]$disease
#> [[1]]$associatedDiseases$rows[[23]]$disease$id
#> [1] "MONDO_0008903"
#> 
#> [[1]]$associatedDiseases$rows[[23]]$disease$name
#> [1] "lung cancer"
#> 
#> [[1]]$associatedDiseases$rows[[23]]$disease$description
#> [1] "A malignant neoplasm involving the lung."
#> 
#> [[1]]$associatedDiseases$rows[[23]]$disease$therapeuticAreas
#> [[1]]$associatedDiseases$rows[[23]]$disease$therapeuticAreas[[1]]
#> [[1]]$associatedDiseases$rows[[23]]$disease$therapeuticAreas[[1]]$name
#> [1] "respiratory or thoracic disease"
#> 
#> 
#> [[1]]$associatedDiseases$rows[[23]]$disease$therapeuticAreas[[2]]
#> [[1]]$associatedDiseases$rows[[23]]$disease$therapeuticAreas[[2]]$name
#> [1] "cancer or benign tumor"
#> 
#> 
#> 
#> 
#> 
#> [[1]]$associatedDiseases$rows[[24]]
#> [[1]]$associatedDiseases$rows[[24]]$score
#> [1] 0.06432784
#> 
#> [[1]]$associatedDiseases$rows[[24]]$datatypeScores
#> [[1]]$associatedDiseases$rows[[24]]$datatypeScores[[1]]
#> [[1]]$associatedDiseases$rows[[24]]$datatypeScores[[1]]$id
#> [1] "genetic_association"
#> 
#> [[1]]$associatedDiseases$rows[[24]]$datatypeScores[[1]]$score
#> [1] 0.1058144
#> 
#> 
#> 
#> [[1]]$associatedDiseases$rows[[24]]$disease
#> [[1]]$associatedDiseases$rows[[24]]$disease$id
#> [1] "EFO_0006335"
#> 
#> [[1]]$associatedDiseases$rows[[24]]$disease$name
#> [1] "systolic blood pressure"
#> 
#> [[1]]$associatedDiseases$rows[[24]]$disease$description
#> [1] "The blood pressure during the contraction of the left ventricle of the heart."
#> 
#> [[1]]$associatedDiseases$rows[[24]]$disease$therapeuticAreas
#> [[1]]$associatedDiseases$rows[[24]]$disease$therapeuticAreas[[1]]
#> [[1]]$associatedDiseases$rows[[24]]$disease$therapeuticAreas[[1]]$name
#> [1] "measurement"
#> 
#> 
#> 
#> 
#> 
#> [[1]]$associatedDiseases$rows[[25]]
#> [[1]]$associatedDiseases$rows[[25]]$score
#> [1] 0.05907112
#> 
#> [[1]]$associatedDiseases$rows[[25]]$datatypeScores
#> [[1]]$associatedDiseases$rows[[25]]$datatypeScores[[1]]
#> [[1]]$associatedDiseases$rows[[25]]$datatypeScores[[1]]$id
#> [1] "genetic_association"
#> 
#> [[1]]$associatedDiseases$rows[[25]]$datatypeScores[[1]]$score
#> [1] 0.0971675
#> 
#> 
#> 
#> [[1]]$associatedDiseases$rows[[25]]$disease
#> [[1]]$associatedDiseases$rows[[25]]$disease$id
#> [1] "EFO_0005763"
#> 
#> [[1]]$associatedDiseases$rows[[25]]$disease$name
#> [1] "pulse pressure measurement"
#> 
#> [[1]]$associatedDiseases$rows[[25]]$disease$description
#> [1] "quantification of the difference between systolic blood pressure and diastolic blood pressure. Higher PP is associated with left ventricle hypertrophy and the increased intimal thickness of the carotid artery, which represent early target organ damage in cardiovascular diseases"
#> 
#> [[1]]$associatedDiseases$rows[[25]]$disease$therapeuticAreas
#> [[1]]$associatedDiseases$rows[[25]]$disease$therapeuticAreas[[1]]
#> [[1]]$associatedDiseases$rows[[25]]$disease$therapeuticAreas[[1]]$name
#> [1] "measurement"
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
#> list()
#> 
#> [[1]]$knownDrugs
#> [[1]]$knownDrugs$count
#> [1] 0
#> 
#> [[1]]$knownDrugs$rows
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
#> [1] 477
#> 
#> [[2]]$associatedDiseases$rows
#> [[2]]$associatedDiseases$rows[[1]]
#> [[2]]$associatedDiseases$rows[[1]]$score
#> [1] 0.4802271
#> 
#> [[2]]$associatedDiseases$rows[[1]]$datatypeScores
#> [[2]]$associatedDiseases$rows[[1]]$datatypeScores[[1]]
#> [[2]]$associatedDiseases$rows[[1]]$datatypeScores[[1]]$id
#> [1] "literature"
#> 
#> [[2]]$associatedDiseases$rows[[1]]$datatypeScores[[1]]$score
#> [1] 0.2372838
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
#> [1] 0.7776787
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
#> 
#> 
#> 
#> [[2]]$associatedDiseases$rows[[2]]
#> [[2]]$associatedDiseases$rows[[2]]$score
#> [1] 0.4781718
#> 
#> [[2]]$associatedDiseases$rows[[2]]$datatypeScores
#> [[2]]$associatedDiseases$rows[[2]]$datatypeScores[[1]]
#> [[2]]$associatedDiseases$rows[[2]]$datatypeScores[[1]]$id
#> [1] "genetic_association"
#> 
#> [[2]]$associatedDiseases$rows[[2]]$datatypeScores[[1]]$score
#> [1] 0.7865563
#> 
#> 
#> 
#> [[2]]$associatedDiseases$rows[[2]]$disease
#> [[2]]$associatedDiseases$rows[[2]]$disease$id
#> [1] "EFO_0004842"
#> 
#> [[2]]$associatedDiseases$rows[[2]]$disease$name
#> [1] "eosinophil count"
#> 
#> [[2]]$associatedDiseases$rows[[2]]$disease$description
#> [1] "The number of granulocytes (polymorphonuclear leukocytes) in a specified volume of blood, usually 1 cubic millimeter."
#> 
#> [[2]]$associatedDiseases$rows[[2]]$disease$therapeuticAreas
#> [[2]]$associatedDiseases$rows[[2]]$disease$therapeuticAreas[[1]]
#> [[2]]$associatedDiseases$rows[[2]]$disease$therapeuticAreas[[1]]$name
#> [1] "measurement"
#> 
#> 
#> 
#> 
#> 
#> [[2]]$associatedDiseases$rows[[3]]
#> [[2]]$associatedDiseases$rows[[3]]$score
#> [1] 0.4637505
#> 
#> [[2]]$associatedDiseases$rows[[3]]$datatypeScores
#> [[2]]$associatedDiseases$rows[[3]]$datatypeScores[[1]]
#> [[2]]$associatedDiseases$rows[[3]]$datatypeScores[[1]]$id
#> [1] "literature"
#> 
#> [[2]]$associatedDiseases$rows[[3]]$datatypeScores[[1]]$score
#> [1] 0.1320306
#> 
#> 
#> [[2]]$associatedDiseases$rows[[3]]$datatypeScores[[2]]
#> [[2]]$associatedDiseases$rows[[3]]$datatypeScores[[2]]$id
#> [1] "genetic_association"
#> 
#> [[2]]$associatedDiseases$rows[[3]]$datatypeScores[[2]]$score
#> [1] 0.7562328
#> 
#> 
#> 
#> [[2]]$associatedDiseases$rows[[3]]$disease
#> [[2]]$associatedDiseases$rows[[3]]$disease$id
#> [1] "EFO_0000676"
#> 
#> [[2]]$associatedDiseases$rows[[3]]$disease$name
#> [1] "psoriasis"
#> 
#> [[2]]$associatedDiseases$rows[[3]]$disease$description
#> [1] "An autoimmune condition characterized by red, well-delineated plaques with silvery scales that are usually on the extensor surfaces and scalp. They can occasionally present with these manifestations: pustules; erythema and scaling in intertriginous areas, and erythroderma, that are often distributed on extensor surfaces and scalp."
#> 
#> [[2]]$associatedDiseases$rows[[3]]$disease$therapeuticAreas
#> [[2]]$associatedDiseases$rows[[3]]$disease$therapeuticAreas[[1]]
#> [[2]]$associatedDiseases$rows[[3]]$disease$therapeuticAreas[[1]]$name
#> [1] "immune system disease"
#> 
#> 
#> [[2]]$associatedDiseases$rows[[3]]$disease$therapeuticAreas[[2]]
#> [[2]]$associatedDiseases$rows[[3]]$disease$therapeuticAreas[[2]]$name
#> [1] "genetic, familial or congenital disease"
#> 
#> 
#> [[2]]$associatedDiseases$rows[[3]]$disease$therapeuticAreas[[3]]
#> [[2]]$associatedDiseases$rows[[3]]$disease$therapeuticAreas[[3]]$name
#> [1] "integumentary system disease"
#> 
#> 
#> 
#> 
#> 
#> [[2]]$associatedDiseases$rows[[4]]
#> [[2]]$associatedDiseases$rows[[4]]$score
#> [1] 0.4609477
#> 
#> [[2]]$associatedDiseases$rows[[4]]$datatypeScores
#> [[2]]$associatedDiseases$rows[[4]]$datatypeScores[[1]]
#> [[2]]$associatedDiseases$rows[[4]]$datatypeScores[[1]]$id
#> [1] "literature"
#> 
#> [[2]]$associatedDiseases$rows[[4]]$datatypeScores[[1]]$score
#> [1] 0.4626507
#> 
#> 
#> [[2]]$associatedDiseases$rows[[4]]$datatypeScores[[2]]
#> [[2]]$associatedDiseases$rows[[4]]$datatypeScores[[2]]$id
#> [1] "genetic_association"
#> 
#> [[2]]$associatedDiseases$rows[[4]]$datatypeScores[[2]]$score
#> [1] 0.7350914
#> 
#> 
#> 
#> [[2]]$associatedDiseases$rows[[4]]$disease
#> [[2]]$associatedDiseases$rows[[4]]$disease$id
#> [1] "MONDO_0005147"
#> 
#> [[2]]$associatedDiseases$rows[[4]]$disease$name
#> [1] "type 1 diabetes mellitus"
#> 
#> [[2]]$associatedDiseases$rows[[4]]$disease$description
#> [1] "A chronic condition characterized by minimal or absent production of insulin by the pancreas."
#> 
#> [[2]]$associatedDiseases$rows[[4]]$disease$therapeuticAreas
#> [[2]]$associatedDiseases$rows[[4]]$disease$therapeuticAreas[[1]]
#> [[2]]$associatedDiseases$rows[[4]]$disease$therapeuticAreas[[1]]$name
#> [1] "pancreas disease"
#> 
#> 
#> [[2]]$associatedDiseases$rows[[4]]$disease$therapeuticAreas[[2]]
#> [[2]]$associatedDiseases$rows[[4]]$disease$therapeuticAreas[[2]]$name
#> [1] "endocrine system disease"
#> 
#> 
#> [[2]]$associatedDiseases$rows[[4]]$disease$therapeuticAreas[[3]]
#> [[2]]$associatedDiseases$rows[[4]]$disease$therapeuticAreas[[3]]$name
#> [1] "gastrointestinal disease"
#> 
#> 
#> [[2]]$associatedDiseases$rows[[4]]$disease$therapeuticAreas[[4]]
#> [[2]]$associatedDiseases$rows[[4]]$disease$therapeuticAreas[[4]]$name
#> [1] "immune system disease"
#> 
#> 
#> [[2]]$associatedDiseases$rows[[4]]$disease$therapeuticAreas[[5]]
#> [[2]]$associatedDiseases$rows[[4]]$disease$therapeuticAreas[[5]]$name
#> [1] "nutritional or metabolic disease"
#> 
#> 
#> 
#> 
#> 
#> [[2]]$associatedDiseases$rows[[5]]
#> [[2]]$associatedDiseases$rows[[5]]$score
#> [1] 0.4472464
#> 
#> [[2]]$associatedDiseases$rows[[5]]$datatypeScores
#> [[2]]$associatedDiseases$rows[[5]]$datatypeScores[[1]]
#> [[2]]$associatedDiseases$rows[[5]]$datatypeScores[[1]]$id
#> [1] "literature"
#> 
#> [[2]]$associatedDiseases$rows[[5]]$datatypeScores[[1]]$score
#> [1] 0.236859
#> 
#> 
#> [[2]]$associatedDiseases$rows[[5]]$datatypeScores[[2]]
#> [[2]]$associatedDiseases$rows[[5]]$datatypeScores[[2]]$id
#> [1] "genetic_association"
#> 
#> [[2]]$associatedDiseases$rows[[5]]$datatypeScores[[2]]$score
#> [1] 0.7238434
#> 
#> 
#> 
#> [[2]]$associatedDiseases$rows[[5]]$disease
#> [[2]]$associatedDiseases$rows[[5]]$disease$id
#> [1] "EFO_0000384"
#> 
#> [[2]]$associatedDiseases$rows[[5]]$disease$name
#> [1] "Crohn's disease"
#> 
#> [[2]]$associatedDiseases$rows[[5]]$disease$description
#> [1] "A gastrointestinal disorder characterized by chronic inflammation involving all layers of the intestinal wall, noncaseating granulomas affecting the intestinal wall and regional lymph nodes, and transmural fibrosis. Crohn disease most commonly involves the terminal ileum; the colon is the second most common site of involvement."
#> 
#> [[2]]$associatedDiseases$rows[[5]]$disease$therapeuticAreas
#> [[2]]$associatedDiseases$rows[[5]]$disease$therapeuticAreas[[1]]
#> [[2]]$associatedDiseases$rows[[5]]$disease$therapeuticAreas[[1]]$name
#> [1] "genetic, familial or congenital disease"
#> 
#> 
#> [[2]]$associatedDiseases$rows[[5]]$disease$therapeuticAreas[[2]]
#> [[2]]$associatedDiseases$rows[[5]]$disease$therapeuticAreas[[2]]$name
#> [1] "immune system disease"
#> 
#> 
#> [[2]]$associatedDiseases$rows[[5]]$disease$therapeuticAreas[[3]]
#> [[2]]$associatedDiseases$rows[[5]]$disease$therapeuticAreas[[3]]$name
#> [1] "gastrointestinal disease"
#> 
#> 
#> 
#> 
#> 
#> [[2]]$associatedDiseases$rows[[6]]
#> [[2]]$associatedDiseases$rows[[6]]$score
#> [1] 0.4383893
#> 
#> [[2]]$associatedDiseases$rows[[6]]$datatypeScores
#> [[2]]$associatedDiseases$rows[[6]]$datatypeScores[[1]]
#> [[2]]$associatedDiseases$rows[[6]]$datatypeScores[[1]]$id
#> [1] "genetic_association"
#> 
#> [[2]]$associatedDiseases$rows[[6]]$datatypeScores[[1]]$score
#> [1] 0.7211171
#> 
#> 
#> 
#> [[2]]$associatedDiseases$rows[[6]]$disease
#> [[2]]$associatedDiseases$rows[[6]]$disease$id
#> [1] "EFO_0004509"
#> 
#> [[2]]$associatedDiseases$rows[[6]]$disease$name
#> [1] "hemoglobin measurement"
#> 
#> [[2]]$associatedDiseases$rows[[6]]$disease$description
#> [1] "Hemoglobin measurement is a measure of the quantity of the metallo protein hemoglobin in blood often used in the diagnosis of anaemia."
#> 
#> [[2]]$associatedDiseases$rows[[6]]$disease$therapeuticAreas
#> [[2]]$associatedDiseases$rows[[6]]$disease$therapeuticAreas[[1]]
#> [[2]]$associatedDiseases$rows[[6]]$disease$therapeuticAreas[[1]]$name
#> [1] "measurement"
#> 
#> 
#> 
#> 
#> 
#> [[2]]$associatedDiseases$rows[[7]]
#> [[2]]$associatedDiseases$rows[[7]]$score
#> [1] 0.4327832
#> 
#> [[2]]$associatedDiseases$rows[[7]]$datatypeScores
#> [[2]]$associatedDiseases$rows[[7]]$datatypeScores[[1]]
#> [[2]]$associatedDiseases$rows[[7]]$datatypeScores[[1]]$id
#> [1] "genetic_association"
#> 
#> [[2]]$associatedDiseases$rows[[7]]$datatypeScores[[1]]$score
#> [1] 0.5695164
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
#> [1] "musculoskeletal or connective tissue disease"
#> 
#> 
#> [[2]]$associatedDiseases$rows[[7]]$disease$therapeuticAreas[[2]]
#> [[2]]$associatedDiseases$rows[[7]]$disease$therapeuticAreas[[2]]$name
#> [1] "immune system disease"
#> 
#> 
#> [[2]]$associatedDiseases$rows[[7]]$disease$therapeuticAreas[[3]]
#> [[2]]$associatedDiseases$rows[[7]]$disease$therapeuticAreas[[3]]$name
#> [1] "urinary system disease"
#> 
#> 
#> 
#> 
#> 
#> [[2]]$associatedDiseases$rows[[8]]
#> [[2]]$associatedDiseases$rows[[8]]$score
#> [1] 0.4327665
#> 
#> [[2]]$associatedDiseases$rows[[8]]$datatypeScores
#> [[2]]$associatedDiseases$rows[[8]]$datatypeScores[[1]]
#> [[2]]$associatedDiseases$rows[[8]]$datatypeScores[[1]]$id
#> [1] "literature"
#> 
#> [[2]]$associatedDiseases$rows[[8]]$datatypeScores[[1]]$score
#> [1] 0.3482714
#> 
#> 
#> [[2]]$associatedDiseases$rows[[8]]$datatypeScores[[2]]
#> [[2]]$associatedDiseases$rows[[8]]$datatypeScores[[2]]$id
#> [1] "genetic_association"
#> 
#> [[2]]$associatedDiseases$rows[[8]]$datatypeScores[[2]]$score
#> [1] 0.6944544
#> 
#> 
#> 
#> [[2]]$associatedDiseases$rows[[8]]$disease
#> [[2]]$associatedDiseases$rows[[8]]$disease$id
#> [1] "EFO_0003767"
#> 
#> [[2]]$associatedDiseases$rows[[8]]$disease$name
#> [1] "inflammatory bowel disease"
#> 
#> [[2]]$associatedDiseases$rows[[8]]$disease$description
#> [1] "A spectrum of small and large bowel inflammatory diseases of unknown etiology. It includes Crohn's disease, ulcerative colitis, and colitis of indeterminate type."
#> 
#> [[2]]$associatedDiseases$rows[[8]]$disease$therapeuticAreas
#> [[2]]$associatedDiseases$rows[[8]]$disease$therapeuticAreas[[1]]
#> [[2]]$associatedDiseases$rows[[8]]$disease$therapeuticAreas[[1]]$name
#> [1] "genetic, familial or congenital disease"
#> 
#> 
#> [[2]]$associatedDiseases$rows[[8]]$disease$therapeuticAreas[[2]]
#> [[2]]$associatedDiseases$rows[[8]]$disease$therapeuticAreas[[2]]$name
#> [1] "immune system disease"
#> 
#> 
#> [[2]]$associatedDiseases$rows[[8]]$disease$therapeuticAreas[[3]]
#> [[2]]$associatedDiseases$rows[[8]]$disease$therapeuticAreas[[3]]$name
#> [1] "gastrointestinal disease"
#> 
#> 
#> 
#> 
#> 
#> [[2]]$associatedDiseases$rows[[9]]
#> [[2]]$associatedDiseases$rows[[9]]$score
#> [1] 0.4271177
#> 
#> [[2]]$associatedDiseases$rows[[9]]$datatypeScores
#> [[2]]$associatedDiseases$rows[[9]]$datatypeScores[[1]]
#> [[2]]$associatedDiseases$rows[[9]]$datatypeScores[[1]]$id
#> [1] "literature"
#> 
#> [[2]]$associatedDiseases$rows[[9]]$datatypeScores[[1]]$score
#> [1] 0.09330685
#> 
#> 
#> [[2]]$associatedDiseases$rows[[9]]$datatypeScores[[2]]
#> [[2]]$associatedDiseases$rows[[9]]$datatypeScores[[2]]$id
#> [1] "animal_model"
#> 
#> [[2]]$associatedDiseases$rows[[9]]$datatypeScores[[2]]$score
#> [1] 0.2926579
#> 
#> 
#> [[2]]$associatedDiseases$rows[[9]]$datatypeScores[[3]]
#> [[2]]$associatedDiseases$rows[[9]]$datatypeScores[[3]]$id
#> [1] "genetic_association"
#> 
#> [[2]]$associatedDiseases$rows[[9]]$datatypeScores[[3]]$score
#> [1] 0.6858699
#> 
#> 
#> 
#> [[2]]$associatedDiseases$rows[[9]]$disease
#> [[2]]$associatedDiseases$rows[[9]]$disease$id
#> [1] "EFO_0005140"
#> 
#> [[2]]$associatedDiseases$rows[[9]]$disease$name
#> [1] "autoimmune disease"
#> 
#> [[2]]$associatedDiseases$rows[[9]]$disease$description
#> [1] "Autoimmune disease or disorder is a disease characterized by an immune response of an organism against parts of itself causing pathology e.g. Graves' disease."
#> 
#> [[2]]$associatedDiseases$rows[[9]]$disease$therapeuticAreas
#> [[2]]$associatedDiseases$rows[[9]]$disease$therapeuticAreas[[1]]
#> [[2]]$associatedDiseases$rows[[9]]$disease$therapeuticAreas[[1]]$name
#> [1] "immune system disease"
#> 
#> 
#> 
#> 
#> 
#> [[2]]$associatedDiseases$rows[[10]]
#> [[2]]$associatedDiseases$rows[[10]]$score
#> [1] 0.4193997
#> 
#> [[2]]$associatedDiseases$rows[[10]]$datatypeScores
#> [[2]]$associatedDiseases$rows[[10]]$datatypeScores[[1]]
#> [[2]]$associatedDiseases$rows[[10]]$datatypeScores[[1]]$id
#> [1] "literature"
#> 
#> [[2]]$associatedDiseases$rows[[10]]$datatypeScores[[1]]$score
#> [1] 0.7055879
#> 
#> 
#> [[2]]$associatedDiseases$rows[[10]]$datatypeScores[[2]]
#> [[2]]$associatedDiseases$rows[[10]]$datatypeScores[[2]]$id
#> [1] "genetic_association"
#> 
#> [[2]]$associatedDiseases$rows[[10]]$datatypeScores[[2]]$score
#> [1] 0.6546013
#> 
#> 
#> 
#> [[2]]$associatedDiseases$rows[[10]]$disease
#> [[2]]$associatedDiseases$rows[[10]]$disease$id
#> [1] "EFO_0000729"
#> 
#> [[2]]$associatedDiseases$rows[[10]]$disease$name
#> [1] "ulcerative colitis"
#> 
#> [[2]]$associatedDiseases$rows[[10]]$disease$description
#> [1] "An inflammatory bowel disease involving the mucosal surface of the large intestine and rectum. It may present with an acute or slow onset and follows an intermittent or continuous course. Signs and symptoms include abdominal pain, diarrhea, fever, weight loss, and intestinal hemorrhage."
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
#> [1] 0.4158367
#> 
#> [[2]]$associatedDiseases$rows[[11]]$datatypeScores
#> [[2]]$associatedDiseases$rows[[11]]$datatypeScores[[1]]
#> [[2]]$associatedDiseases$rows[[11]]$datatypeScores[[1]]$id
#> [1] "genetic_association"
#> 
#> [[2]]$associatedDiseases$rows[[11]]$datatypeScores[[1]]$score
#> [1] 0.6840199
#> 
#> 
#> 
#> [[2]]$associatedDiseases$rows[[11]]$disease
#> [[2]]$associatedDiseases$rows[[11]]$disease$id
#> [1] "OBA_0003460"
#> 
#> [[2]]$associatedDiseases$rows[[11]]$disease$name
#> [1] "erythrocyte volume"
#> 
#> [[2]]$associatedDiseases$rows[[11]]$disease$description
#> [1] "The volume of a erythrocyte."
#> 
#> [[2]]$associatedDiseases$rows[[11]]$disease$therapeuticAreas
#> [[2]]$associatedDiseases$rows[[11]]$disease$therapeuticAreas[[1]]
#> [[2]]$associatedDiseases$rows[[11]]$disease$therapeuticAreas[[1]]$name
#> [1] "measurement"
#> 
#> 
#> 
#> 
#> 
#> [[2]]$associatedDiseases$rows[[12]]
#> [[2]]$associatedDiseases$rows[[12]]$score
#> [1] 0.4152994
#> 
#> [[2]]$associatedDiseases$rows[[12]]$datatypeScores
#> [[2]]$associatedDiseases$rows[[12]]$datatypeScores[[1]]
#> [[2]]$associatedDiseases$rows[[12]]$datatypeScores[[1]]$id
#> [1] "genetic_association"
#> 
#> [[2]]$associatedDiseases$rows[[12]]$datatypeScores[[1]]$score
#> [1] 0.683136
#> 
#> 
#> 
#> [[2]]$associatedDiseases$rows[[12]]$disease
#> [[2]]$associatedDiseases$rows[[12]]$disease$id
#> [1] "EFO_0004348"
#> 
#> [[2]]$associatedDiseases$rows[[12]]$disease$name
#> [1] "hematocrit"
#> 
#> [[2]]$associatedDiseases$rows[[12]]$disease$description
#> [1] "The volume of packed RED BLOOD CELLS in a blood specimen. The volume is measured by centrifugation in a tube with graduated markings, or with automated blood cell counters. It is an indicator of erythrocyte status in disease. For example, ANEMIA shows a low value; POLYCYTHEMIA, a high value."
#> 
#> [[2]]$associatedDiseases$rows[[12]]$disease$therapeuticAreas
#> [[2]]$associatedDiseases$rows[[12]]$disease$therapeuticAreas[[1]]
#> [[2]]$associatedDiseases$rows[[12]]$disease$therapeuticAreas[[1]]$name
#> [1] "measurement"
#> 
#> 
#> 
#> 
#> 
#> [[2]]$associatedDiseases$rows[[13]]
#> [[2]]$associatedDiseases$rows[[13]]$score
#> [1] 0.4135607
#> 
#> [[2]]$associatedDiseases$rows[[13]]$datatypeScores
#> [[2]]$associatedDiseases$rows[[13]]$datatypeScores[[1]]
#> [[2]]$associatedDiseases$rows[[13]]$datatypeScores[[1]]$id
#> [1] "genetic_association"
#> 
#> [[2]]$associatedDiseases$rows[[13]]$datatypeScores[[1]]$score
#> [1] 0.6802759
#> 
#> 
#> 
#> [[2]]$associatedDiseases$rows[[13]]$disease
#> [[2]]$associatedDiseases$rows[[13]]$disease$id
#> [1] "EFO_0004587"
#> 
#> [[2]]$associatedDiseases$rows[[13]]$disease$name
#> [1] "lymphocyte count"
#> 
#> [[2]]$associatedDiseases$rows[[13]]$disease$description
#> [1] "A quantification of lymphocytes in blood."
#> 
#> [[2]]$associatedDiseases$rows[[13]]$disease$therapeuticAreas
#> [[2]]$associatedDiseases$rows[[13]]$disease$therapeuticAreas[[1]]
#> [[2]]$associatedDiseases$rows[[13]]$disease$therapeuticAreas[[1]]$name
#> [1] "measurement"
#> 
#> 
#> 
#> 
#> 
#> [[2]]$associatedDiseases$rows[[14]]
#> [[2]]$associatedDiseases$rows[[14]]$score
#> [1] 0.4017471
#> 
#> [[2]]$associatedDiseases$rows[[14]]$datatypeScores
#> [[2]]$associatedDiseases$rows[[14]]$datatypeScores[[1]]
#> [[2]]$associatedDiseases$rows[[14]]$datatypeScores[[1]]$id
#> [1] "genetic_association"
#> 
#> [[2]]$associatedDiseases$rows[[14]]$datatypeScores[[1]]$score
#> [1] 0.6608435
#> 
#> 
#> 
#> [[2]]$associatedDiseases$rows[[14]]$disease
#> [[2]]$associatedDiseases$rows[[14]]$disease$id
#> [1] "EFO_0007837"
#> 
#> [[2]]$associatedDiseases$rows[[14]]$disease$name
#> [1] "anti-citrullinated protein antibody seropositivity"
#> 
#> [[2]]$associatedDiseases$rows[[14]]$disease$description
#> [1] "anti-citrullinated protein antibody seropositivity is the result of a measurement of circulating anti-cyclic citrullinated peptide antibodies"
#> 
#> [[2]]$associatedDiseases$rows[[14]]$disease$therapeuticAreas
#> [[2]]$associatedDiseases$rows[[14]]$disease$therapeuticAreas[[1]]
#> [[2]]$associatedDiseases$rows[[14]]$disease$therapeuticAreas[[1]]$name
#> [1] "measurement"
#> 
#> 
#> 
#> 
#> 
#> [[2]]$associatedDiseases$rows[[15]]
#> [[2]]$associatedDiseases$rows[[15]]$score
#> [1] 0.4017471
#> 
#> [[2]]$associatedDiseases$rows[[15]]$datatypeScores
#> [[2]]$associatedDiseases$rows[[15]]$datatypeScores[[1]]
#> [[2]]$associatedDiseases$rows[[15]]$datatypeScores[[1]]$id
#> [1] "genetic_association"
#> 
#> [[2]]$associatedDiseases$rows[[15]]$datatypeScores[[1]]$score
#> [1] 0.6608435
#> 
#> 
#> 
#> [[2]]$associatedDiseases$rows[[15]]$disease
#> [[2]]$associatedDiseases$rows[[15]]$disease$id
#> [1] "EFO_0007791"
#> 
#> [[2]]$associatedDiseases$rows[[15]]$disease$name
#> [1] "rheumatoid factor seropositivity measurement"
#> 
#> [[2]]$associatedDiseases$rows[[15]]$disease$description
#> [1] "rheumatoid factor seropositivity is the result of a measurement of circulating  autoantibodies called rheumatoid factors that contribute to a number of autoimmune diseases including rheumatoid arthritis"
#> 
#> [[2]]$associatedDiseases$rows[[15]]$disease$therapeuticAreas
#> [[2]]$associatedDiseases$rows[[15]]$disease$therapeuticAreas[[1]]
#> [[2]]$associatedDiseases$rows[[15]]$disease$therapeuticAreas[[1]]$name
#> [1] "measurement"
#> 
#> 
#> 
#> 
#> 
#> [[2]]$associatedDiseases$rows[[16]]
#> [[2]]$associatedDiseases$rows[[16]]$score
#> [1] 0.4000243
#> 
#> [[2]]$associatedDiseases$rows[[16]]$datatypeScores
#> [[2]]$associatedDiseases$rows[[16]]$datatypeScores[[1]]
#> [[2]]$associatedDiseases$rows[[16]]$datatypeScores[[1]]$id
#> [1] "genetic_association"
#> 
#> [[2]]$associatedDiseases$rows[[16]]$datatypeScores[[1]]$score
#> [1] 0.6580096
#> 
#> 
#> 
#> [[2]]$associatedDiseases$rows[[16]]$disease
#> [[2]]$associatedDiseases$rows[[16]]$disease$id
#> [1] "EFO_0004458"
#> 
#> [[2]]$associatedDiseases$rows[[16]]$disease$name
#> [1] "C-reactive protein measurement"
#> 
#> [[2]]$associatedDiseases$rows[[16]]$disease$description
#> [1] "C-reactive protein (CRP) measurement is a measurement of the level of C-reactive protein in the blood.  Levels are known to rise in response to inflammation, CRP is therefore used as a clinical measure of inflammation. The measurement is used in the process of clinical diagnosis as high levels of CRP are associated with cardiovascular disease, diabetes and hypertension and in some cancers."
#> 
#> [[2]]$associatedDiseases$rows[[16]]$disease$therapeuticAreas
#> [[2]]$associatedDiseases$rows[[16]]$disease$therapeuticAreas[[1]]
#> [[2]]$associatedDiseases$rows[[16]]$disease$therapeuticAreas[[1]]$name
#> [1] "measurement"
#> 
#> 
#> 
#> 
#> 
#> [[2]]$associatedDiseases$rows[[17]]
#> [[2]]$associatedDiseases$rows[[17]]$score
#> [1] 0.3880588
#> 
#> [[2]]$associatedDiseases$rows[[17]]$datatypeScores
#> [[2]]$associatedDiseases$rows[[17]]$datatypeScores[[1]]
#> [[2]]$associatedDiseases$rows[[17]]$datatypeScores[[1]]$id
#> [1] "literature"
#> 
#> [[2]]$associatedDiseases$rows[[17]]$datatypeScores[[1]]$score
#> [1] 0.6079308
#> 
#> 
#> [[2]]$associatedDiseases$rows[[17]]$datatypeScores[[2]]
#> [[2]]$associatedDiseases$rows[[17]]$datatypeScores[[2]]$id
#> [1] "genetic_association"
#> 
#> [[2]]$associatedDiseases$rows[[17]]$datatypeScores[[2]]$score
#> [1] 0.6079308
#> 
#> 
#> 
#> [[2]]$associatedDiseases$rows[[17]]$disease
#> [[2]]$associatedDiseases$rows[[17]]$disease$id
#> [1] "HP_0001369"
#> 
#> [[2]]$associatedDiseases$rows[[17]]$disease$name
#> [1] "Arthritis"
#> 
#> [[2]]$associatedDiseases$rows[[17]]$disease$description
#> [1] "Inflammation of a joint."
#> 
#> [[2]]$associatedDiseases$rows[[17]]$disease$therapeuticAreas
#> [[2]]$associatedDiseases$rows[[17]]$disease$therapeuticAreas[[1]]
#> [[2]]$associatedDiseases$rows[[17]]$disease$therapeuticAreas[[1]]$name
#> [1] "phenotype"
#> 
#> 
#> 
#> 
#> 
#> [[2]]$associatedDiseases$rows[[18]]
#> [[2]]$associatedDiseases$rows[[18]]$score
#> [1] 0.3783412
#> 
#> [[2]]$associatedDiseases$rows[[18]]$datatypeScores
#> [[2]]$associatedDiseases$rows[[18]]$datatypeScores[[1]]
#> [[2]]$associatedDiseases$rows[[18]]$datatypeScores[[1]]$id
#> [1] "literature"
#> 
#> [[2]]$associatedDiseases$rows[[18]]$datatypeScores[[1]]$score
#> [1] 0.03039654
#> 
#> 
#> [[2]]$associatedDiseases$rows[[18]]$datatypeScores[[2]]
#> [[2]]$associatedDiseases$rows[[18]]$datatypeScores[[2]]$id
#> [1] "animal_model"
#> 
#> [[2]]$associatedDiseases$rows[[18]]$datatypeScores[[2]]$score
#> [1] 0.2747239
#> 
#> 
#> [[2]]$associatedDiseases$rows[[18]]$datatypeScores[[3]]
#> [[2]]$associatedDiseases$rows[[18]]$datatypeScores[[3]]$id
#> [1] "genetic_association"
#> 
#> [[2]]$associatedDiseases$rows[[18]]$datatypeScores[[3]]$score
#> [1] 0.6079308
#> 
#> 
#> 
#> [[2]]$associatedDiseases$rows[[18]]$disease
#> [[2]]$associatedDiseases$rows[[18]]$disease$id
#> [1] "MONDO_0015517"
#> 
#> [[2]]$associatedDiseases$rows[[18]]$disease$name
#> [1] "common variable immunodeficiency"
#> 
#> [[2]]$associatedDiseases$rows[[18]]$disease$description
#> [1] "Common variable immunodeficiency (CVID) comprises a heterogeneous group of diseases characterized by a significant hypogammaglobulinemia of unknown cause, failure to produce specific antibodies after immunizations and susceptibility to bacterial infections, predominantly caused by encapsulated bacteria."
#> 
#> [[2]]$associatedDiseases$rows[[18]]$disease$therapeuticAreas
#> [[2]]$associatedDiseases$rows[[18]]$disease$therapeuticAreas[[1]]
#> [[2]]$associatedDiseases$rows[[18]]$disease$therapeuticAreas[[1]]$name
#> [1] "immune system disease"
#> 
#> 
#> [[2]]$associatedDiseases$rows[[18]]$disease$therapeuticAreas[[2]]
#> [[2]]$associatedDiseases$rows[[18]]$disease$therapeuticAreas[[2]]$name
#> [1] "cancer or benign tumor"
#> 
#> 
#> [[2]]$associatedDiseases$rows[[18]]$disease$therapeuticAreas[[3]]
#> [[2]]$associatedDiseases$rows[[18]]$disease$therapeuticAreas[[3]]$name
#> [1] "hematologic disease"
#> 
#> 
#> [[2]]$associatedDiseases$rows[[18]]$disease$therapeuticAreas[[4]]
#> [[2]]$associatedDiseases$rows[[18]]$disease$therapeuticAreas[[4]]$name
#> [1] "genetic, familial or congenital disease"
#> 
#> 
#> 
#> 
#> 
#> [[2]]$associatedDiseases$rows[[19]]
#> [[2]]$associatedDiseases$rows[[19]]$score
#> [1] 0.3745147
#> 
#> [[2]]$associatedDiseases$rows[[19]]$datatypeScores
#> [[2]]$associatedDiseases$rows[[19]]$datatypeScores[[1]]
#> [[2]]$associatedDiseases$rows[[19]]$datatypeScores[[1]]$id
#> [1] "genetic_association"
#> 
#> [[2]]$associatedDiseases$rows[[19]]$datatypeScores[[1]]$score
#> [1] 0.6160483
#> 
#> 
#> 
#> [[2]]$associatedDiseases$rows[[19]]$disease
#> [[2]]$associatedDiseases$rows[[19]]$disease$id
#> [1] "EFO_0803539"
#> 
#> [[2]]$associatedDiseases$rows[[19]]$disease$name
#> [1] "basophil measurement"
#> 
#> [[2]]$associatedDiseases$rows[[19]]$disease$description
#> [1] "Quantification of some aspect of basophils, such as function, quantity or composition."
#> 
#> [[2]]$associatedDiseases$rows[[19]]$disease$therapeuticAreas
#> [[2]]$associatedDiseases$rows[[19]]$disease$therapeuticAreas[[1]]
#> [[2]]$associatedDiseases$rows[[19]]$disease$therapeuticAreas[[1]]$name
#> [1] "measurement"
#> 
#> 
#> 
#> 
#> 
#> [[2]]$associatedDiseases$rows[[20]]
#> [[2]]$associatedDiseases$rows[[20]]$score
#> [1] 0.3710447
#> 
#> [[2]]$associatedDiseases$rows[[20]]$datatypeScores
#> [[2]]$associatedDiseases$rows[[20]]$datatypeScores[[1]]
#> [[2]]$associatedDiseases$rows[[20]]$datatypeScores[[1]]$id
#> [1] "genetic_association"
#> 
#> [[2]]$associatedDiseases$rows[[20]]$datatypeScores[[1]]$score
#> [1] 0.6103403
#> 
#> 
#> 
#> [[2]]$associatedDiseases$rows[[20]]$disease
#> [[2]]$associatedDiseases$rows[[20]]$disease$id
#> [1] "EFO_1001494"
#> 
#> [[2]]$associatedDiseases$rows[[20]]$disease$name
#> [1] "psoriasis vulgaris"
#> 
#> [[2]]$associatedDiseases$rows[[20]]$disease$description
#> [1] "Plaque psoriasis is the most common presentation of psoriasis. It presents as small to large, well demarcated, red, scaly and thickened areas of skin. It most likely to affect elbows, knees, and lower back but may arise on any part of the body."
#> 
#> [[2]]$associatedDiseases$rows[[20]]$disease$therapeuticAreas
#> [[2]]$associatedDiseases$rows[[20]]$disease$therapeuticAreas[[1]]
#> [[2]]$associatedDiseases$rows[[20]]$disease$therapeuticAreas[[1]]$name
#> [1] "integumentary system disease"
#> 
#> 
#> [[2]]$associatedDiseases$rows[[20]]$disease$therapeuticAreas[[2]]
#> [[2]]$associatedDiseases$rows[[20]]$disease$therapeuticAreas[[2]]$name
#> [1] "immune system disease"
#> 
#> 
#> [[2]]$associatedDiseases$rows[[20]]$disease$therapeuticAreas[[3]]
#> [[2]]$associatedDiseases$rows[[20]]$disease$therapeuticAreas[[3]]$name
#> [1] "genetic, familial or congenital disease"
#> 
#> 
#> 
#> 
#> 
#> [[2]]$associatedDiseases$rows[[21]]
#> [[2]]$associatedDiseases$rows[[21]]$score
#> [1] 0.3703172
#> 
#> [[2]]$associatedDiseases$rows[[21]]$datatypeScores
#> [[2]]$associatedDiseases$rows[[21]]$datatypeScores[[1]]
#> [[2]]$associatedDiseases$rows[[21]]$datatypeScores[[1]]$id
#> [1] "genetic_association"
#> 
#> [[2]]$associatedDiseases$rows[[21]]$datatypeScores[[1]]$score
#> [1] 0.6091437
#> 
#> 
#> 
#> [[2]]$associatedDiseases$rows[[21]]$disease
#> [[2]]$associatedDiseases$rows[[21]]$disease$id
#> [1] "EFO_0004305"
#> 
#> [[2]]$associatedDiseases$rows[[21]]$disease$name
#> [1] "erythrocyte count"
#> 
#> [[2]]$associatedDiseases$rows[[21]]$disease$description
#> [1] "The number of red blood cells per unit volume in a sample of venous blood."
#> 
#> [[2]]$associatedDiseases$rows[[21]]$disease$therapeuticAreas
#> [[2]]$associatedDiseases$rows[[21]]$disease$therapeuticAreas[[1]]
#> [[2]]$associatedDiseases$rows[[21]]$disease$therapeuticAreas[[1]]$name
#> [1] "measurement"
#> 
#> 
#> 
#> 
#> 
#> [[2]]$associatedDiseases$rows[[22]]
#> [[2]]$associatedDiseases$rows[[22]]$score
#> [1] 0.3695799
#> 
#> [[2]]$associatedDiseases$rows[[22]]$datatypeScores
#> [[2]]$associatedDiseases$rows[[22]]$datatypeScores[[1]]
#> [[2]]$associatedDiseases$rows[[22]]$datatypeScores[[1]]$id
#> [1] "genetic_association"
#> 
#> [[2]]$associatedDiseases$rows[[22]]$datatypeScores[[1]]$score
#> [1] 0.6079308
#> 
#> 
#> 
#> [[2]]$associatedDiseases$rows[[22]]$disease
#> [[2]]$associatedDiseases$rows[[22]]$disease$id
#> [1] "MONDO_0004670"
#> 
#> [[2]]$associatedDiseases$rows[[22]]$disease$name
#> [1] "lupus erythematosus"
#> 
#> [[2]]$associatedDiseases$rows[[22]]$disease$description
#> [1] "An autoimmune, connective tissue chronic inflammatory disorder affecting the skin, joints, kidneys, lungs, heart, and the peripheral blood cells. It is more commonly seen in women than men. Variants include discoid and systemic lupus erythematosus."
#> 
#> [[2]]$associatedDiseases$rows[[22]]$disease$therapeuticAreas
#> [[2]]$associatedDiseases$rows[[22]]$disease$therapeuticAreas[[1]]
#> [[2]]$associatedDiseases$rows[[22]]$disease$therapeuticAreas[[1]]$name
#> [1] "musculoskeletal or connective tissue disease"
#> 
#> 
#> [[2]]$associatedDiseases$rows[[22]]$disease$therapeuticAreas[[2]]
#> [[2]]$associatedDiseases$rows[[22]]$disease$therapeuticAreas[[2]]$name
#> [1] "immune system disease"
#> 
#> 
#> 
#> 
#> 
#> [[2]]$associatedDiseases$rows[[23]]
#> [[2]]$associatedDiseases$rows[[23]]$score
#> [1] 0.3695799
#> 
#> [[2]]$associatedDiseases$rows[[23]]$datatypeScores
#> [[2]]$associatedDiseases$rows[[23]]$datatypeScores[[1]]
#> [[2]]$associatedDiseases$rows[[23]]$datatypeScores[[1]]$id
#> [1] "genetic_association"
#> 
#> [[2]]$associatedDiseases$rows[[23]]$datatypeScores[[1]]$score
#> [1] 0.6079308
#> 
#> 
#> 
#> [[2]]$associatedDiseases$rows[[23]]$disease
#> [[2]]$associatedDiseases$rows[[23]]$disease$id
#> [1] "EFO_0005856"
#> 
#> [[2]]$associatedDiseases$rows[[23]]$disease$name
#> [1] "arthritis"
#> 
#> [[2]]$associatedDiseases$rows[[23]]$disease$description
#> [1] "Arthritis (from Greek arthro-, joint + -itis, inflammation; plural: arthritides) is a form of joint disorder that involves inflammation of one or more joints."
#> 
#> [[2]]$associatedDiseases$rows[[23]]$disease$therapeuticAreas
#> [[2]]$associatedDiseases$rows[[23]]$disease$therapeuticAreas[[1]]
#> [[2]]$associatedDiseases$rows[[23]]$disease$therapeuticAreas[[1]]$name
#> [1] "musculoskeletal or connective tissue disease"
#> 
#> 
#> 
#> 
#> 
#> [[2]]$associatedDiseases$rows[[24]]
#> [[2]]$associatedDiseases$rows[[24]]$score
#> [1] 0.361915
#> 
#> [[2]]$associatedDiseases$rows[[24]]$datatypeScores
#> [[2]]$associatedDiseases$rows[[24]]$datatypeScores[[1]]
#> [[2]]$associatedDiseases$rows[[24]]$datatypeScores[[1]]$id
#> [1] "literature"
#> 
#> [[2]]$associatedDiseases$rows[[24]]$datatypeScores[[1]]$score
#> [1] 0.09004494
#> 
#> 
#> [[2]]$associatedDiseases$rows[[24]]$datatypeScores[[2]]
#> [[2]]$associatedDiseases$rows[[24]]$datatypeScores[[2]]$id
#> [1] "genetic_association"
#> 
#> [[2]]$associatedDiseases$rows[[24]]$datatypeScores[[2]]$score
#> [1] 0.5908204
#> 
#> 
#> 
#> [[2]]$associatedDiseases$rows[[24]]$disease
#> [[2]]$associatedDiseases$rows[[24]]$disease$id
#> [1] "EFO_0001060"
#> 
#> [[2]]$associatedDiseases$rows[[24]]$disease$name
#> [1] "celiac disease"
#> 
#> [[2]]$associatedDiseases$rows[[24]]$disease$description
#> [1] "An autoimmune genetic disorder with an unknown pattern of inheritance that primarily affects the digestive tract. It is caused by intolerance to dietary gluten. Consumption of gluten protein triggers an immune response which damages small intestinal villi and prevents adequate absorption of nutrients. Clinical signs include abdominal cramping, diarrhea or constipation and weight loss. If untreated, the clinical course may progress to malnutrition, anemia, osteoporosis and an increased risk of intestinal malignancies. However, the prognosis is favorable with successful avoidance of gluten in the diet."
#> 
#> [[2]]$associatedDiseases$rows[[24]]$disease$therapeuticAreas
#> [[2]]$associatedDiseases$rows[[24]]$disease$therapeuticAreas[[1]]
#> [[2]]$associatedDiseases$rows[[24]]$disease$therapeuticAreas[[1]]$name
#> [1] "immune system disease"
#> 
#> 
#> [[2]]$associatedDiseases$rows[[24]]$disease$therapeuticAreas[[2]]
#> [[2]]$associatedDiseases$rows[[24]]$disease$therapeuticAreas[[2]]$name
#> [1] "gastrointestinal disease"
#> 
#> 
#> [[2]]$associatedDiseases$rows[[24]]$disease$therapeuticAreas[[3]]
#> [[2]]$associatedDiseases$rows[[24]]$disease$therapeuticAreas[[3]]$name
#> [1] "genetic, familial or congenital disease"
#> 
#> 
#> 
#> 
#> 
#> [[2]]$associatedDiseases$rows[[25]]
#> [[2]]$associatedDiseases$rows[[25]]$score
#> [1] 0.3377977
#> 
#> [[2]]$associatedDiseases$rows[[25]]$datatypeScores
#> [[2]]$associatedDiseases$rows[[25]]$datatypeScores[[1]]
#> [[2]]$associatedDiseases$rows[[25]]$datatypeScores[[1]]$id
#> [1] "genetic_association"
#> 
#> [[2]]$associatedDiseases$rows[[25]]$datatypeScores[[1]]$score
#> [1] 0.5556516
#> 
#> 
#> 
#> [[2]]$associatedDiseases$rows[[25]]$disease
#> [[2]]$associatedDiseases$rows[[25]]$disease$id
#> [1] "EFO_0004705"
#> 
#> [[2]]$associatedDiseases$rows[[25]]$disease$name
#> [1] "hypothyroidism"
#> 
#> [[2]]$associatedDiseases$rows[[25]]$disease$description
#> [1] "Abnormally low levels of thyroid hormone."
#> 
#> [[2]]$associatedDiseases$rows[[25]]$disease$therapeuticAreas
#> [[2]]$associatedDiseases$rows[[25]]$disease$therapeuticAreas[[1]]
#> [[2]]$associatedDiseases$rows[[25]]$disease$therapeuticAreas[[1]]$name
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
#> [[2]]$knownDrugs
#> [[2]]$knownDrugs$count
#> [1] 0
#> 
#> [[2]]$knownDrugs$rows
#> list()
#> 
#> 
#> 
```
