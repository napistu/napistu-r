# Utilities for interacting with Open Target's GraphQL API which don't depend on Napistu

OT_API_URL <- "https://api.platform.opentargets.org/api/v4/graphql"

# Fields we expect on each GraphQL type, keyed to the query sections in
# build_targets_query(). Used by check_open_targets_schema().
OT_EXPECTED_FIELDS <- list(
    Target = c(
        "id", "approvedSymbol", "approvedName",
        "associatedDiseases",      # → format_open_targets_diseases
        "tractability",            # → format_open_targets_tractability
        "geneticConstraint",       # → format_open_targets_genetic_conservation
        "chemicalProbes",          # → format_open_targets_probe_list
        "drugAndClinicalCandidates" # → format_open_targets_drug_list
    ),
    Drug = c("name", "drugType")
)

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
                summarize_open_targets_targets(input$ensembl)
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
summarize_open_targets_targets <- function(target_ensembl_gene_ids, verbose = FALSE) {

    target_results <- query_open_targets_targets(
        target_ensembl_gene_ids,
        verbose = verbose
    )

    if (length(query_open_targets_targets) == 0) {
        warning(
            "No results were found in Open Targets for the provided ensembl IDs:",
            paste(target_ensembl_gene_ids, collapse = ", ")
        )
        return(list(targets = tibble::tibble(), indications = tibble::tibble()))
    }

    found_target_ensembl_gene_ids <- purrr::map_chr(target_results, function(x) x$id)

    if (length(found_target_ensembl_gene_ids) == 0) {
        warning("No results were returned from Open Targets")
        return(list(targets = tibble::tibble(), indications = tibble::tibble()))
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

    targets_spread <- tibble::tibble(
        ensembl_id = found_target_ensembl_gene_ids
    ) %>%
        dplyr::mutate(target_results = target_results) %>%
        tidyr::unnest_wider(target_results)

    if (!("tractability" %in% colnames(targets_spread))) {
        tractability <- tibble::tibble(
            approvedSymbol  = NA_character_,
            ab_tractability = NA_character_,
            sm_tractability = NA_character_,
            n_sms           = 0
        )
    } else {
        tractability <- targets_spread %>%
            dplyr::select(approvedSymbol, tractability) %>%
            dplyr::filter(!purrr::map_lgl(tractability, is.null))

        if (nrow(tractability) == 0) {
            tractability <- tibble::tibble(
                approvedSymbol  = NA_character_,
                ab_tractability = NA_character_,
                sm_tractability = NA_character_,
                n_sms           = 0
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

    drugs        <- format_open_targets_drug_list(targets_spread)
    probes       <- format_open_targets_probe_list(targets_spread)
    conservation <- format_open_targets_genetic_conservation(targets_spread)

    target_summary <- targets_spread %>%
        dplyr::select(ensembl_id, approvedSymbol, approvedName) %>%
        dplyr::left_join(tractability, by = "approvedSymbol")

    if (!is.null(drugs))        target_summary <- dplyr::left_join(target_summary, drugs,        by = "approvedSymbol")
    if (!is.null(probes))       target_summary <- dplyr::left_join(target_summary, probes,       by = "approvedSymbol")
    if (!is.null(conservation)) target_summary <- dplyr::left_join(target_summary, conservation, by = "approvedSymbol")

    target_summary <- target_summary %>%
        dplyr::mutate_if(is.character, stringr::str_wrap, width = 40, exdent = 2)

    indications <- targets_spread %>%
        dplyr::mutate(diseases = purrr::map(associatedDiseases, format_open_targets_diseases)) %>%
        dplyr::select(approvedSymbol, diseases) %>%
        tidyr::unnest(diseases)

    if (!is.null(indications)) {
        data_types_present <- setdiff(
            colnames(indications),
            c("approvedSymbol", "disease_name", "score", "therapeutic_area", "description")
        ) %>%
            sort()

        indications <- indications %>%
            dplyr::arrange(dplyr::desc(score)) %>%
            dplyr::mutate(description = stringr::str_wrap(description, exdent = 2)) %>%
            dplyr::select(
                approvedSymbol, disease_name, score,
                !!!rlang::syms(data_types_present),
                therapeutic_area, description
            ) %>%
            dplyr::arrange(dplyr::desc(score))
    } else {
        indications <- tibble::tibble()
    }

    list(targets = target_summary, indications = indications)
}

#' Query Open Targets - Targets
#'
#' Using Open Target's GraphQL API return gene-metadata and disease/phenotype
#'   associations for a set of genes.
#'
#' @param target_ensembl_gene_ids a character vector of ensembl gene names
#' @param post_n_genes separate queries into posts of this number of genes
#' @param verbose print the request body and raw response for each batch
#'
#' @returns a list of target attributes
#'
#' @examples
#' target_ensembl_gene_ids <- c("ENSG00000119718", "ENSG00000175354")
#' query_open_targets_targets(target_ensembl_gene_ids)
#' @export
query_open_targets_targets <- function(
        target_ensembl_gene_ids,
        post_n_genes = 10,
        verbose = FALSE
) {
    checkmate::assertCharacter(
        target_ensembl_gene_ids,
        pattern = "^ENSG[0-9]{11}$",
        min.len  = 1,
        max.len  = 1000
    )
    checkmate::assertNumber(post_n_genes)

    max_posts <- ceiling(length(target_ensembl_gene_ids) / post_n_genes)

    post_sets <- tibble::tibble(
        gene = target_ensembl_gene_ids,
        set  = ceiling(seq(0.0001, max_posts, length.out = length(target_ensembl_gene_ids)))
    ) %>%
        tidyr::nest(genes = -set) %>%
        {
            purrr::map(.$genes, function(x) x$gene)
        } %>%
        purrr::map(\(genes) post_open_targets_query(genes, verbose = verbose))

    do.call(c, post_sets)
}

#' Build Open Targets GraphQL Query for Targets
#'
#' Returns the GraphQL query string used by [post_open_targets_query()]. Each
#' comment in the query body names the format function that processes that
#' section, making it easy to locate what needs updating when the Open Targets
#' API changes a field name.
#'
#' @returns a single character string containing the GraphQL query
#' @keywords internal
build_targets_query <- function() {
    'query getTargets($ensemblIds: [String!]!) {
  targets(ensemblIds: $ensemblIds) {
    id
    approvedSymbol
    approvedName

    # format_open_targets_diseases
    associatedDiseases {
      count
      rows {
        score
        datatypeScores { id score }
        disease { id name description therapeuticAreas { name } }
      }
    }

    # format_open_targets_tractability
    tractability { label modality value }

    # format_open_targets_genetic_conservation
    geneticConstraint { constraintType exp obs score oe oeLower oeUpper }

    # format_open_targets_probe_list
    chemicalProbes { id control drugId mechanismOfAction isHighQuality urls { url } }

    # format_open_targets_drug_list
    drugAndClinicalCandidates {
      rows {
        maxClinicalStage
        drug { name drugType }
      }
    }
  }
}'
}

#' Check Open Targets Schema Compatibility
#'
#' Runs a GraphQL introspection query and compares the fields we actually use
#' against what the API currently exposes. Call this when queries start failing
#' with unexpected errors — it will identify removed or renamed fields before
#' you have to dig through cryptic HTTP 400 messages.
#'
#' The expected fields are defined in [OT_EXPECTED_FIELDS] and correspond
#' directly to the sections of [build_targets_query()].
#'
#' @returns invisibly, a named list of logical vectors (one per type) indicating
#'   which expected fields are present
#' @keywords internal
check_open_targets_schema <- function() {
    results <- purrr::imap(OT_EXPECTED_FIELDS, function(fields, type_name) {
        query <- sprintf('{ __type(name: "%s") { fields { name } } }', type_name)

        resp <- httr2::request(OT_API_URL) |>
            httr2::req_body_json(list(query = query)) |>
            httr2::req_error(is_error = \(r) FALSE) |>
            httr2::req_perform()

        available <- purrr::map_chr(
            httr2::resp_body_json(resp)$data$`__type`$fields %||% list(),
            "name"
        )

        stats::setNames(fields %in% available, fields)
    })

    any_missing <- FALSE
    for (type_name in names(results)) {
        missing <- names(which(!results[[type_name]]))
        if (length(missing) > 0) {
            any_missing <- TRUE
            cli::cli_warn(
                "Open Targets {.cls {type_name}}: missing fields {.field {missing}}"
            )
        } else {
            cli::cli_inform(
                "Open Targets {.cls {type_name}}: all {length(results[[type_name]])} expected fields present"
            )
        }
    }

    if (!any_missing) {
        cli::cli_inform("Open Targets schema check passed.")
    }

    invisible(results)
}

post_open_targets_query <- function(target_ensembl_gene_ids, verbose = FALSE) {
    query_string <- build_targets_query()
    variables    <- list(ensemblIds = target_ensembl_gene_ids)

    if (verbose) {
        cli::cli_inform("POST {OT_API_URL}")
        message("Variables: ", jsonlite::toJSON(variables, auto_unbox = TRUE))
    }

    resp <- httr2::request(OT_API_URL) |>
        httr2::req_body_json(list(query = query_string, variables = variables)) |>
        httr2::req_error(is_error = \(r) FALSE) |>
        httr2::req_perform()

    if (verbose) {
        cli::cli_inform("Response HTTP {httr2::resp_status(resp)}")
        message(substr(httr2::resp_body_string(resp), 1, 500))
    }

    validate_open_targets_resp(resp)
    httr2::resp_body_json(resp)$data$targets
}

# ── Format helpers ────────────────────────────────────────────────────────────
# Each function takes `targets_spread` (the unnested API response tibble) and
# returns a per-symbol summary tibble, or NULL if that data type is absent.

format_open_targets_diseases <- function(diseases) {
    checkmate::assertList(diseases, len = 2, names = "unique")
    stopifnot(names(diseases) == c("count", "rows"))

    if (diseases$count == 0) {
        return(NULL)
    }

    indication_scores <- purrr::map(diseases$rows, function(x) {
        spread_datatype_scores <- purrr::map(
            x$datatypeScores,
            function(y) tibble::tibble(id = y$id, score = y$score)
        ) %>%
            dplyr::bind_rows() %>%
            tidyr::spread(id, score)

        tibble::tibble(
            score            = x$score,
            disease_name     = x$disease$name,
            disease_id       = x$disease$id,
            therapeutic_area = ifelse(
                is.null(x$disease$therapeuticAreas),
                "",
                purrr::map_chr(x$disease$therapeuticAreas, function(x) x$name) %>%
                    paste(collapse = ", + ")
            ),
            description = ifelse(
                is.null(x$disease$description), "", x$disease$description
            )
        ) %>%
            dplyr::bind_cols(spread_datatype_scores)
    }) %>%
        dplyr::bind_rows()

    data_types_present <- setdiff(
        colnames(indication_scores),
        c("disease_id", "disease_name", "score", "description")
    ) %>%
        sort()

    indication_scores %>%
        dplyr::mutate_if(is.numeric, round, 2) %>%
        dplyr::select(disease_name, score, !!!rlang::syms(data_types_present), description) %>%
        dplyr::arrange(dplyr::desc(score))
}

format_open_targets_probe_list <- function(targets_spread) {
    checkmate::assertDataFrame(targets_spread)

    if (!("chemicalProbes" %in% colnames(targets_spread)) ||
        !inherits(targets_spread$chemicalProbes, "list")) {
        return(NULL)
    }

    valid_probes <- targets_spread %>%
        dplyr::select(approvedSymbol, chemicalProbes) %>%
        dplyr::filter(!purrr::map_lgl(chemicalProbes, is.null)) %>%
        tidyr::unnest(chemicalProbes) %>%
        dplyr::filter(purrr::map_int(chemicalProbes, length) != 0)

    if (nrow(valid_probes) == 0) {
        return(NULL)
    }

    valid_probes %>%
        tidyr::unnest_wider(chemicalProbes) %>%
        dplyr::group_by(approvedSymbol) %>%
        dplyr::summarize(
            `high quality probes` = paste(id[isHighQuality], collapse = ", "),
            `N probes`            = length(id)
        )
}

format_open_targets_tractability <- function(tractability) {
    checkmate::assertList(tractability)

    tractability_df <- tibble::tibble(dat = tractability) %>%
        tidyr::unnest_wider(dat) %>%
        dplyr::filter(value == TRUE) %>%
        dplyr::group_by(modality) %>%
        dplyr::summarize(label = paste(unique(sort(label)), collapse = ", "))

    missing_tractability_df <- tibble::tibble(
        modality = c("AB", "PR", "SM"),
        label    = NA_character_
    ) %>%
        dplyr::anti_join(tractability_df, by = "modality")

    dplyr::bind_rows(tractability_df, missing_tractability_df) %>%
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
}

format_open_targets_drug_list <- function(targets_spread) {
    checkmate::assertDataFrame(targets_spread)

    if (!("drugAndClinicalCandidates" %in% colnames(targets_spread)) ||
        !inherits(targets_spread$drugAndClinicalCandidates, "list")) {
        return(NULL)
    }

    valid_drugs <- targets_spread %>%
        dplyr::select(approvedSymbol, drugAndClinicalCandidates) %>%
        dplyr::filter(!purrr::map_lgl(drugAndClinicalCandidates, is.null))

    if (nrow(valid_drugs) == 0) {
        return(NULL)
    }

    valid_drugs <- valid_drugs %>%
        dplyr::mutate(rows = purrr::map(drugAndClinicalCandidates, function(x) {
            if (is.null(x$rows) || length(x$rows) == 0) {
                return(tibble::tibble(
                    name             = character(),
                    drugType         = character(),
                    maxClinicalStage = character()
                ))
            }
            purrr::map_dfr(x$rows, function(row) {
                tibble::tibble(
                    name             = row$drug$name %||% NA_character_,
                    drugType         = row$drug$drugType %||% NA_character_,
                    maxClinicalStage = row$maxClinicalStage %||% NA_character_
                )
            })
        })) %>%
        dplyr::select(approvedSymbol, rows) %>%
        tidyr::unnest(rows) %>%
        dplyr::filter(!is.na(name))

    if (nrow(valid_drugs) == 0) {
        return(NULL)
    }

    valid_drugs %>%
        dplyr::distinct(approvedSymbol, name) %>%
        dplyr::mutate(
            name  = stringr::str_to_title(name),
            label = name
        ) %>%
        dplyr::arrange(name) %>%
        dplyr::group_by(approvedSymbol) %>%
        dplyr::summarize(drugs = stringr::str_c(label, collapse = "; "))
}

format_open_targets_genetic_conservation <- function(targets_spread) {
    checkmate::assertDataFrame(targets_spread)

    if (!("geneticConstraint" %in% colnames(targets_spread))) {
        return(NULL)
    }

    targets_spread %>%
        dplyr::select(approvedSymbol, geneticConstraint) %>%
        dplyr::filter(!purrr::map_lgl(geneticConstraint, is.null)) %>%
        dplyr::mutate(dat = purrr::map(geneticConstraint, function(x) {
            tibble::tibble(x) %>% tidyr::unnest_wider(x)
        })) %>%
        dplyr::select(-geneticConstraint) %>%
        tidyr::unnest(dat) %>%
        dplyr::mutate(
            var = dplyr::case_when(
                constraintType == "syn" ~ "o/e synonymous",
                constraintType == "mis" ~ "o/e non-synonymous",
                constraintType == "lof" ~ "o/e loss-of-function"
            ),
            var = factor(var, levels = c(
                "o/e synonymous", "o/e non-synonymous", "o/e loss-of-function"
            ))
        ) %>%
        dplyr::select(approvedSymbol, var, oe) %>%
        tidyr::spread(var, oe)
}

#' Query Open Targets - Indications
#'
#' Using Open Target's GraphQL API return indication score for a
#'   disease/phenotype of interest across a set of genes.
#'
#' @param target_ensembl_gene_ids a character vector of ensembl gene names
#' @param disease_id A disease to search from the EFO ontology
#' @param verbose print the request body and raw response
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
        disease_id,
        verbose = FALSE
) {
    checkmate::assertCharacter(
        target_ensembl_gene_ids,
        pattern = "^ENSG[0-9]{11}$",
        min.len  = 1,
        max.len  = 1000
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
    }"

    variables <- list(efoId = disease_id, ensemblIds = target_ensembl_gene_ids)

    if (verbose) {
        cli::cli_inform("POST {OT_API_URL}")
        message("Variables: ", jsonlite::toJSON(variables, auto_unbox = TRUE))
    }

    resp <- httr2::request(OT_API_URL) |>
        httr2::req_body_json(list(query = query_string, variables = variables)) |>
        httr2::req_error(is_error = \(r) FALSE) |>
        httr2::req_perform()

    if (verbose) {
        cli::cli_inform("Response HTTP {httr2::resp_status(resp)}")
        message(substr(httr2::resp_body_string(resp), 1, 500))
    }

    validate_open_targets_resp(resp)
    httr2::resp_body_json(resp)$data$disease
}

# ── HTTP helpers ──────────────────────────────────────────────────────────────

validate_open_targets_resp <- function(resp) {
    status <- httr2::resp_status(resp)
    body   <- tryCatch(httr2::resp_body_json(resp), error = \(e) NULL)

    # GraphQL-level errors come back as HTTP 200 with an `errors` field
    if (!is.null(body$errors[[1]])) {
        cli::cli_abort(body$errors[[1]]$message)
    }

    if (status == 400) {
        error_msg <- body$syntaxError %||% body$error %||% body$message %||%
            httr2::resp_body_string(resp)
        cli::cli_abort(c("GraphQL query failed (HTTP 400)", "x" = "{error_msg}"))
    }

    if (status == 502) {
        error_msg <- body$syntaxError %||% body$error %||% body$message %||%
            httr2::resp_body_string(resp)
        cli::cli_warn(c("GraphQL query failed (HTTP 502)", "!" = "{error_msg}"))
    }

    invisible(0)
}
