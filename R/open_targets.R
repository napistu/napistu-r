#' Summarize Diseases
#'
#' Using a molecular neighborhood, target and indication summaries for all
#'   genes from Open Targets.
#'
#' @inheritParams validate_napistu_list
#' @param neighborhood_summary_table tibble produced by
#'   \code{create_neighborhood_summary_table}
#'
#' @returns A list containing targets and indications
#'
#' @examples
#'
#' if (interactive()) {
#'     setup_napistu_list(create_napistu_config())
#'     species_id <- random_species(napistu_list)
#'
#'     neighborhood_summary_table <- create_neighborhood_table(
#'         napistu_list,
#'         species_id,
#'         max_steps = 7L
#'     ) %>%
#'         create_neighborhood_summary_table()
#'       
#'     summarize_diseases(
#'         napistu_list,
#'         neighborhood_summary_table
#'     )
#' }
#' @export
summarize_diseases <- function(
    napistu_list,
    neighborhood_summary_table
) {
    
    validate_napistu_list(napistu_list)
    sbml_dfs <- napistu_list$sbml_dfs
    species_identifiers <- napistu_list$species_identifiers
    checkmate::assertDataFrame(neighborhood_summary_table)
    
    # extract a tibble of ensembl IDs for neighbors
    neighborhood_ensembl_ids <- format_neighborhood_ensembl_ids(
        neighborhood_summary_table,
        species_identifiers
    )
    
    if (nrow(neighborhood_ensembl_ids) == 0) {
        return(list(
            targets = tibble(),
            indications = tibble()
        ))
    }
    
    open_targets_results <- summarize_open_targets_targets(
        neighborhood_ensembl_ids$identifier
    )
    
    if ("targets" %in% names(open_targets_results)) {
        open_targets_results$targets <- open_targets_results$targets %>%
            dplyr::left_join(
                neighborhood_ensembl_ids %>%
                    dplyr::select(ensembl_id = identifier, path_weight:sc_name),
                by = "ensembl_id"
            )
    }
    
    return(open_targets_results)
}

format_neighborhood_ensembl_ids <- function(
    neighborhood_summary_table,
    species_identifiers
) {
    checkmate::assertDataFrame(neighborhood_summary_table)
    
    if (!("ensembl_gene" %in% species_identifiers$ontology)) {
        cli::cli_abort(
            "no entries for \"ensembl_gene\" among identifiers.
            ensembl gene IDs are required for open targets's API"
        )
    }
    
    ensembl_ids <- species_identifiers$ontology_ids[species_identifiers$ontology == "ensembl_gene"][[1]]
    
    included_sids <- neighborhood_summary_table %>%
        dplyr::filter(node_type == "species")
    
    # filter ensembl IDs to entries in neighborhoods and
    # add path length / path weights
    neighborhood_ensembl_ids <- ensembl_ids %>%
        dplyr::inner_join(
            included_sids %>%
                dplyr::select(s_id, path_weight, path_length, sc_name),
            by = "s_id"
        ) %>%
        dplyr::arrange(path_weight, path_length) %>%
        dplyr::group_by(identifier) %>%
        dplyr::slice(1) %>%
        dplyr::ungroup()
    
    if (nrow(neighborhood_ensembl_ids) == 0) {
        message("No neighbors with ensembl IDs")
    } else {
        message(glue::glue("Query open targets with {nrow(neighborhood_ensembl_ids)} ensembl IDs from {nrow(included_sids)} species"))
    }
    
    return(neighborhood_ensembl_ids)
}

#' Summarize Indications
#'
#' Using a molecular neighborhood, find all Open Target scores for a specified disease.
#'
#' @inheritParams validate_napistu_list
#' @inheritParams query_open_targets_indications
#' @inheritParams summarize_diseases
#'
#' @returns A tibble of species names and disease scores
#'
#' @examples
#'
#' if (interactive()) {
#'     setup_napistu_list(create_napistu_config())
#'     species_id <- random_species(napistu_list)
#'
#'     neighborhood_summary_table <- create_neighborhood_table(
#'         napistu_list,
#'         species_id,
#'         max_steps = 7L
#'         ) %>%
#'         create_neighborhood_summary_table()
#'    
#'      disease_id <- "EFO_0000400" # diabetes
#'      summarize_indication(disease_id, sbml_dfs, neighborhood_summary_table, species_identifiers)
#' }
#' @export
summarize_indication <- function(
    napistu_list,
    disease_id,
    neighborhood_summary_table
) {
    
    validate_napistu_list(napistu_list)
    species_identifiers <- napistu_list$species_identifiers
    
    checkmate::assertString(disease_id)
    checkmate::assertDataFrame(neighborhood_summary_table)
    
    # extract a tibble of ensembl IDs for neighbors
    neighborhood_ensembl_ids <- format_neighborhood_ensembl_ids(
        neighborhood_summary_table = neighborhood_summary_table,
        species_identifiers = species_identifiers
    )
    
    if (nrow(neighborhood_ensembl_ids) == 0) {
        return(
            tibble::tibble(
                s_id = NA_character_,
                id = NA_character_,
                score = NA_real_,
                approvedSymbol = NA_character_,
                approvedName = NA_character_,
                url = NA_character_
            ) %>%
                dplyr::slice(-1)
        )
    }
    
    # call the graphQL API repeatedly since it often says
    # "try again in 30 seconds"
    max_retries <- 2
    query_tries <- 0
    valid_query <- FALSE
    while (query_tries < max_retries && !valid_query) {
        
        open_targets_results <- query_open_targets_indications(
            neighborhood_ensembl_ids$identifier,
            disease_id
        )
        
        if (!is.null(open_targets_results)) {
            valid_query <- TRUE
        }
        query_tries <- query_tries + 1
        Sys.sleep(30)
    }
    
    if (!valid_query) {
        error_message <- glue::glue("The Open Targets query failed {max_retries} times, try again later")
    }
    
    if (valid_query && open_targets_results$target_scores$count > 0) {
        # at least one neighbor has a score for disease
        indications_df <- tibble::tibble(dat = open_targets_results$target_scores$rows) %>%
            tidyr::unnest_wider(dat) %>%
            tidyr::unnest_wider(target) %>%
            dplyr::mutate(url = glue::glue("https://platform.opentargets.org/evidence/{id}/{disease_id}"))
    } else {
        out <- tibble::tibble(
            s_id = NA_character_,
            id = NA_character_,
            score = NA_real_,
            approvedSymbol = NA_character_,
            approvedName = NA_character_,
            url = NA_character_
        ) %>%
            dplyr::slice(-1)
        
        if (!valid_query) {
            # smuggle an error message out of the query
            attr(out, 'ot_error') <- error_message
        }
        
        return(out)
    }
    
    indication_scores <- neighborhood_ensembl_ids %>%
        dplyr::select(s_id, id = identifier) %>%
        dplyr::left_join(indications_df, by = "id") %>%
        dplyr::mutate(score = ifelse(is.na(score), 0, score)) %>%
        dplyr::group_by(s_id) %>%
        dplyr::arrange(dplyr::desc(score)) %>%
        # retain only the top score when multiple ensembl IDs are associated with the same node
        dplyr::slice(1) %>%
        dplyr::ungroup()
    
    return(indication_scores)
}
