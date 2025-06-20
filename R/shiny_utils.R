add_url_links <- function(df) {
    
    # convert url variable in df to html links
    
    checkmate::assertDataFrame(df)
    
    # find url variables
    
    url_variables <- df %>%
        dplyr::select_if(is.character) %>%
        purrr::map_lgl(~ any(stringr::str_detect(., "^http"), na.rm = TRUE)) %>%
        {
            names(.[.])
        }
    
    if (length(url_variables) == 0) {
        print("No urls found")
        return(df)
    }
    
    annotated_df <- df
    for (url_var in url_variables) {
        annotated_df <- annotated_df %>%
            # create a temporary variable containing the url of interest
            # (I couldn't get the tidy evaluation working with)
            dplyr::mutate(
                .tmp = !!rlang::sym(url_var),
                !!rlang::sym(url_var) := dplyr::case_when(
                    .tmp == "" ~ glue::glue(""),
                    is.na(.tmp) ~ glue::glue(""),
                    TRUE ~ glue::glue('<a href="{.tmp}" target="_blank" class="btn btn-primary">Link</a>')
                )
            ) %>%
            dplyr::select(-.tmp)
    }
    
    return(annotated_df)
}