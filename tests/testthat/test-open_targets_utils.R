test_that("Test Open Targets GraphQL API", {
  # EIF2B2, PTPN2: no drug candidates (exercises the empty-drugs path)
  # EGFR: 79+ drug/clinical candidates (exercises the drug formatting path)
  target_ensembl_gene_ids <- c("ENSG00000119718", "ENSG00000175354", "ENSG00000146648")
  open_targets_data <- summarize_open_targets_targets(target_ensembl_gene_ids)

  expect_equal(nrow(open_targets_data$targets), 3)

  egfr_row <- dplyr::filter(open_targets_data$targets, approvedSymbol == "EGFR")
  expect_equal(nrow(egfr_row), 1)
  expect_false(is.na(egfr_row$drugs))
})
