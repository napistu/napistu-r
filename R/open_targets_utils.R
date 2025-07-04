# Utilities for interacting with Open Target's GraphQL API which don't depend on Napistu

#' Shiny Open Targets Test
#'
#' A stand-alone app to test querying open targets using ensembl IDs
shiny_opentargets_test <- function() {
    shiny::shinyApp(
        ui = shiny::fluidPage(
            shiny::textInput(
                "ensembl",
                "Provide an ensembl gene ID",
                value = "ENSG00000119718"
            )
        ),
        server = function(input, output, session) {
            ot_results <- shiny::reactive({
                req(input$ensembl)
                
                summarize_open_targets_targets(
                    input$ensembl
                )
            })
            
            observe({
                print(ot_results())
            })
        }
    )
}

#' Summarize Open Targets - Targets
#'
#' Using Open Target's GraphQL API to read and format indications and
#'   tractability information for a set of genes.
#'
#' @inheritParams query_open_targets_targets
#'
#' @returns a list containing:
#' \describe{
#'   \item{targets}{tibble summarizing each target and their known chemical
#'     modulators}
#'   \item{indications}{tibble summarizing disease/phenotypic associations
#'     scores related to each target. NULL if no scores are present}
#' }
#'
#' @examples
#' target_ensembl_gene_ids <- c(
#'   "ENSG00000999999",
#'   "ENSG00000119718",
#'   "ENSG00000175354",
#'   "ENSG00000091831"
#'   )
#' summarize_open_targets_targets(target_ensembl_gene_ids)
#' @export
summarize_open_targets_targets <- function(target_ensembl_gene_ids) {
    
    # query open target's graphQL API
    target_results <- query_open_targets_targets(
        target_ensembl_gene_ids
    )
    
    # check for ids with no open targets data
    if (length(query_open_targets_targets) == 0) {
        warning(
            "No results were found in Open Targets for the provided ensembl IDs:",
            paste(target_ensembl_gene_ids, collapse = ", ")
        )
        
        return(list(
            targets = tibble::tibble(),
            indications = tibble::tibble()
        ))
    }
    
    found_target_ensembl_gene_ids <- purrr::map_chr(
        target_results,
        function(x) {
            x$id
        }
    )
    
    if (length(found_target_ensembl_gene_ids) == 0) {
        warning(glue::glue(
            "No results were returned from Open Targets"
        ))
        
        return(list(
            targets = tibble::tibble(),
            indications = tibble::tibble()
        ))
    }
    
    missing_target_ensembl_gene_ids <- setdiff(
        target_ensembl_gene_ids,
        found_target_ensembl_gene_ids
    )
    
    if (length(missing_target_ensembl_gene_ids) != 0) {
        warning(glue::glue(
            "No results for {length(missing_target_ensembl_gene_ids)} ensembl IDs
      were found in Open Targets:
      {paste(missing_target_ensembl_gene_ids, collapse = ', ')}"
        ))
    }
    
    # format entries as one-row per target
    targets_spread <- tibble::tibble(
        ensembl_id = found_target_ensembl_gene_ids
    ) %>%
        dplyr::mutate(target_results = target_results) %>%
        tidyr::unnest_wider(target_results)
    
    # summarize how readily druggable each target seems to be
    
    if (!("tractability" %in% colnames(targets_spread))) {
        tractability <- tibble::tibble(
            approvedSymbol = NA_character_,
            ab_tractability = NA_character_,
            sm_tractability = NA_character_,
            n_sms = 0
        )
    } else {
        tractability <- targets_spread %>%
            dplyr::select(approvedSymbol, tractability) %>%
            dplyr::filter(!purrr::map_lgl(tractability, is.null))
        
        if (nrow(tractability) == 0) {
            tractability <- tibble::tibble(
                approvedSymbol = NA_character_,
                ab_tractability = NA_character_,
                sm_tractability = NA_character_,
                n_sms = 0
            )
        } else {
            tractability <- tractability %>%
                dplyr::mutate(tractability = purrr::map(
                    tractability,
                    format_open_targets_tractability
                )) %>%
                tidyr::unnest(tractability)
        }
    }
    
    # find known drugs and chemical probes for each target
    drugs <- format_open_targets_drug_list(targets_spread)
    probes <- format_open_targets_probe_list(targets_spread)
    conservation <- format_open_targets_genetic_conservation(targets_spread)
    
    # generate a final target-level summary
    target_summary <- targets_spread %>%
        dplyr::select(ensembl_id, approvedSymbol, approvedName) %>%
        dplyr::left_join(tractability, by = "approvedSymbol")
    
    if (!is.null(drugs)) {
        target_summary <- target_summary %>%
            dplyr::left_join(drugs, by = "approvedSymbol")
    }
    if (!is.null(probes)) {
        target_summary <- target_summary %>%
            dplyr::left_join(probes, by = "approvedSymbol")
    }
    if (!is.null(conservation)) {
        target_summary <- target_summary %>%
            dplyr::left_join(conservation, by = "approvedSymbol")
    }
    
    target_summary <- target_summary %>%
        dplyr::mutate_if(is.character, stringr::str_wrap, width = 40, exdent = 2)
    
    # find all top indications for each target (max 25 per target)
    indications <- targets_spread %>%
        dplyr::mutate(diseases = purrr::map(
            associatedDiseases,
            format_open_targets_diseases
        )) %>%
        dplyr::select(approvedSymbol, diseases) %>%
        tidyr::unnest(diseases)
    
    if (!is.null(indications)) {
        data_types_present <- setdiff(
            colnames(indications),
            c(
                "approvedSymbol",
                "disease_name",
                "score",
                "therapeutic_area",
                "description"
            )
        ) %>%
            sort()
        
        indications <- indications %>%
            dplyr::arrange(dplyr::desc(score)) %>%
            dplyr::mutate(description = stringr::str_wrap(description, exdent = 2)) %>%
            dplyr::select(
                approvedSymbol,
                disease_name,
                score,
                !!!rlang::syms(data_types_present),
                therapeutic_area,
                description
            ) %>%
            dplyr::arrange(dplyr::desc(score))
    } else {
        indications <- tibble::tibble()
    }
    
    output <- list(
        targets = target_summary,
        indications = indications
    )
    
    return(output)
}

#' Query Open Targets - Targets
#'
#' Using Open Target's GraphQL API return gene-metadata and disease/phenotype
#'   associations for a set of genes.
#'
#' @param target_ensembl_gene_ids a character vector of ensembl gene names
#' @param post_n_genes separate queries into posts of this number of genes
#'
#' @returns a list of target attributes
#'
#' @examples
#' target_ensembl_gene_ids <- c("ENSG00000119718", "ENSG00000175354")
#' query_open_targets_targets(target_ensembl_gene_ids)
#' @export
query_open_targets_targets <- function(
        target_ensembl_gene_ids,
        post_n_genes = 10
) {
    
    checkmate::assertCharacter(
        target_ensembl_gene_ids,
        pattern = "^ENSG[0-9]{11}$",
        min.len = 1,
        max.len = 1000
    )
    
    checkmate::assertNumber(post_n_genes)
    
    # split large queries into multiple posts
    max_posts <- ceiling(length(target_ensembl_gene_ids) / post_n_genes)
    
    post_sets <- tibble::tibble(
        gene = target_ensembl_gene_ids,
        set = ceiling(seq(
            0.0001,
            max_posts,
            length.out = length(target_ensembl_gene_ids)
        ))
    ) %>%
        tidyr::nest(genes = -set) %>%
        {
            purrr::map(.$genes, function(x) {
                x$gene
            })
        } %>%
        # post a query for each set of genes
        purrr::map(
            post_open_targets_query
        )
    
    post_sets <- do.call(c, post_sets)
    
    return(post_sets)
}

post_open_targets_query <- function(target_ensembl_gene_ids) {
    query_string <- "
    query targets(
    $ensemblIds: [String!]!,
    ){
    targets(ensemblIds: $ensemblIds){
        id
        approvedSymbol,
        approvedName,
        associatedDiseases {
          count,
          rows {
            score,
            datatypeScores {
              id,
              score
            },
            disease {
              id,
              name,
              description,
              therapeuticAreas {
                name
              }
            }
          }
        },
    		tractability {
        	label,
        	modality,
        	value
      	},
        geneticConstraint{
          constraintType
          exp
          obs
          score
          oe
          oeLower
          oeUpper
        },
        chemicalProbes {
          id,
          control,
          drugId,
          mechanismOfAction,
          isHighQuality,
          urls {
            url
          }
        },
        knownDrugs {
          count,
          rows {
            drugId,
            drug {
              name,
              tradeNames,
              drugType,
              yearOfFirstApproval
              }
            }
          }
        }
      }
  "
    
    base_url <- "https://api.platform.opentargets.org/api/v4/graphql"
    
    # Set variables object of arguments to be passed to endpoint
    variables <- list(
        "ensemblIds" = target_ensembl_gene_ids
    )
    # Construct POST request body object with query string and variables
    post_body <- list(query = query_string, variables = variables)
    
    # Perform POST request
    r <- httr::POST(url = base_url, body = post_body, encode = "json")
    validate_open_targets_post(r)
    target_results <- httr::content(r)$data$targets
    
    return(target_results)
}

format_open_targets_diseases <- function(diseases) {
    checkmate::assertList(diseases, len = 2, names = "unique")
    stopifnot(names(diseases) == c("count", "rows"))
    
    if (diseases$count == 0) {
        return(NULL)
    }
    
    indication_scores <- purrr::map(diseases$rows, function(x) {
        spread_datatype_scores <- purrr::map(
            x$datatypeScores,
            function(y) {
                tibble::tibble(id = y$id, score = y$score)
            }
        ) %>%
            dplyr::bind_rows() %>%
            tidyr::spread(id, score)
        
        # x$disease$therapeuticAreas
        
        tibble::tibble(
            score = x$score,
            disease_name = x$disease$name,
            disease_id = x$disease$id,
            therapeutic_area = ifelse(
                is.null(x$disease$therapeuticAreas),
                "",
                purrr::map_chr(x$disease$therapeuticAreas, function(x) {
                    x$name
                }) %>%
                    paste(collapse = ", + ")
            ),
            description = ifelse(
                is.null(x$disease$description),
                "",
                x$disease$description
            )
        ) %>%
            dplyr::bind_cols(spread_datatype_scores)
    }) %>%
        dplyr::bind_rows()
    
    # rearrange and round scores
    data_types_present <- setdiff(
        colnames(indication_scores),
        c("disease_id", "disease_name", "score", "description")
    ) %>%
        sort()
    
    reformated_scores <- indication_scores %>%
        dplyr::mutate_if(is.numeric, round, 2) %>%
        dplyr::select(disease_name, score, !!!rlang::syms(data_types_present), description) %>%
        dplyr::arrange(dplyr::desc(score))
    
    return(reformated_scores)
}

format_open_targets_probe_list <- function(targets_spread) {
    checkmate::assertDataFrame(targets_spread)
    
    if (!("chemicalProbes" %in% colnames(targets_spread)) || !inherits(targets_spread$chemicalProbes, "list")) {
        return(NULL)
    }
    
    valid_probes <- targets_spread %>%
        dplyr::select(approvedSymbol, chemicalProbes) %>%
        dplyr::filter(!purrr::map_lgl(chemicalProbes, is.null)) %>%
        tidyr::unnest(chemicalProbes) %>%
        dplyr::filter(purrr::map_int(chemicalProbes, length) != 0)
    
    if (nrow(valid_probes) == 0) {
        # some targets have a probes entry which doesn't include a named probe
        return(NULL)
    }
    
    out <- valid_probes %>%
        tidyr::unnest_wider(chemicalProbes) %>%
        dplyr::group_by(approvedSymbol) %>%
        dplyr::summarize(
            `high quality probes` = paste(id[isHighQuality], collapse = ", "),
            `N probes` = length(id)
        )
    
    return(out)
}

format_open_targets_tractability <- function(tractability) {
    checkmate::assertList(tractability)
    
    # combine tractability summaries
    tractability_df <- tibble::tibble(dat = tractability) %>%
        tidyr::unnest_wider(dat) %>%
        dplyr::filter(value == TRUE) %>%
        dplyr::group_by(modality) %>%
        dplyr::summarize(label = paste(unique(sort(label)), collapse = ", "))
    
    # add missing categories
    missing_tractability_df <- tibble::tibble(
        modality = c("AB", "PR", "SM"),
        label = NA_character_
    ) %>%
        dplyr::anti_join(tractability_df, by = "modality")
    
    out <- dplyr::bind_rows(tractability_df, missing_tractability_df) %>%
        dplyr::mutate(
            modality = dplyr::case_when(
                modality == "AB" ~ "antibody tractability",
                modality == "PR" ~ "protac tractability",
                modality == "SM" ~ "small molecule tractability"
            ),
            modality = factor(modality, levels = c(
                "small molecule tractability",
                "antibody tractability",
                "protac tractability"
            ))
        ) %>%
        tidyr::spread(modality, label)
    
    return(out)
}


format_open_targets_drug_list <- function(targets_spread) {
    checkmate::assertDataFrame(targets_spread)
    
    # if there are no records then knownDrugs will just be an NA
    
    if (!("knownDrugs" %in% colnames(targets_spread)) || !inherits(targets_spread$knownDrugs, "list")) {
        return(NULL)
    }
    
    valid_drugs <- targets_spread %>%
        dplyr::select(approvedSymbol, knownDrugs) %>%
        dplyr::filter(!purrr::map_lgl(knownDrugs, is.null)) %>%
        tidyr::unnest_wider(knownDrugs) %>%
        dplyr::filter(!is.na(rows))
    
    if (nrow(valid_drugs) == 0) {
        return(NULL)
    }
    
    valid_drugs <- valid_drugs %>%
        tidyr::unnest(rows) %>%
        tidyr::unnest_wider(rows) %>%
        tidyr::unnest_wider(drug)
    
    if (!("yearOfFirstApproval" %in% colnames(valid_drugs))) {
        valid_drugs <- valid_drugs %>%
            dplyr::mutate(yearOfFirstApproval = NA)
    }
    
    valid_drugs <- valid_drugs %>%
        dplyr::distinct(approvedSymbol, name, yearOfFirstApproval) %>%
        dplyr::mutate(
            name = stringr::str_to_title(name),
            label = dplyr::case_when(
                is.na(yearOfFirstApproval) ~ name,
                TRUE ~ as.character(glue::glue("{name} ({yearOfFirstApproval})"))
            )
        ) %>%
        dplyr::arrange(dplyr::desc(yearOfFirstApproval)) %>%
        dplyr::group_by(approvedSymbol) %>%
        dplyr::summarize(drugs = stringr::str_c(label, collapse = "; "))
    
    return(valid_drugs)
}


format_open_targets_genetic_conservation <- function(targets_spread) {
    checkmate::assertDataFrame(targets_spread)
    
    if (!("geneticConstraint" %in% colnames(targets_spread))) {
        return(NULL)
    }
    
    out <- targets_spread %>%
        dplyr::select(approvedSymbol, geneticConstraint) %>%
        dplyr::filter(!purrr::map_lgl(geneticConstraint, is.null)) %>%
        dplyr::mutate(dat = purrr::map(geneticConstraint, function(x) {
            tibble::tibble(x) %>%
                tidyr::unnest_wider(x)
        })) %>%
        dplyr::select(-geneticConstraint) %>%
        tidyr::unnest(dat) %>%
        dplyr::mutate(
            var = dplyr::case_when(
                constraintType == "syn" ~ "o/e synonymous",
                constraintType == "mis" ~ "o/e non-synonymous",
                constraintType == "lof" ~ "o/e loss-of-function"
            ),
            var = factor(
                var,
                levels = c(
                    "o/e synonymous",
                    "o/e non-synonymous",
                    "o/e loss-of-function"
                )
            )
        ) %>%
        dplyr::select(approvedSymbol, var, oe) %>%
        tidyr::spread(var, oe)
    
    return(out)
}

#' Query Open Targets - Indications
#'
#' Using Open Target's GraphQL API return indication score for a
#'   disease/phenotype of interest across a set of genes.
#'
#' @param target_ensembl_gene_ids a character vector of ensembl gene names
#' @param disease_id A disease to search from the EFO ontology
#'
#' @returns a list of target attributes
#'
#' @examples
#' target_ensembl_gene_ids <- c("ENSG00000090104", "ENSG00000163599")
#' disease_id <- "EFO_0001060" # celiac's disease
#' query_open_targets_indications(target_ensembl_gene_ids, disease_id)
#' @export
query_open_targets_indications <- function(
        target_ensembl_gene_ids,
        disease_id
) {
    
    checkmate::assertCharacter(
        target_ensembl_gene_ids,
        pattern = "^ENSG[0-9]{11}$",
        min.len = 1,
        max.len = 1000
    )
    checkmate::assertString(disease_id)
    
    query_string <- "
        query disease (
            $ensemblIds: [String!]!,
            $efoId: String!
        )
        {
        disease(efoId: $efoId){
            id,
            	name,
            	target_scores: associatedTargets (Bs: $ensemblIds) {
            	    count,
                rows {
                    score,
                    target {
                        id,
                        approvedSymbol,
                        approvedName
                        }
                    }
                }
            }
        }
    "
    
    base_url <- "https://api.platform.opentargets.org/api/v4/graphql"
    
    # Set variables object of arguments to be passed to endpoint
    variables <- list(
        "efoId" = disease_id,
        "ensemblIds" = target_ensembl_gene_ids
    )
    # Construct POST request body object with query string and variables
    post_body <- list(query = query_string, variables = variables)
    
    # Perform POST request
    r <- httr::POST(url = base_url, body = post_body, encode = "json")
    validate_open_targets_post(r)
    
    disease_results <- httr::content(r)$data$disease
    
    return(disease_results)
}

validate_open_targets_post <- function(r) {
    errors <- httr::content(r)$errors[[1]]
    if (!is.null(errors)) {
        stop(errors$message)
    }
    
    if (r$status_code %in% c(400, 502)) {
        html_error <- httr::content(r) %>%
            rvest::html_element("body") %>%
            rvest::html_element("p") %>%
            rvest::html_text() %>%
            stringr::str_trim()
        
        if (r$status_code == 400) {
            stop(
                "The GraphQL query failed with the following error message:\n",
                html_error
            )
        } else if (r$status_code == 502) {
            warning(
                "The GraphQL query failed with the following error message:\n",
                html_error
            )
        } else {
            stop("undefined status code")
        }
    }
    
    return(invisible(0))
}
