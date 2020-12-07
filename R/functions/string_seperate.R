string_seperate <- function(df_input, col_name, split_pattern) {
  unique_strings <-
    df_input %>%
    dplyr::distinct({{ col_name }}) %>%
    unlist() %>%
    stringr::str_split({{ split_pattern }}, simplify = TRUE) %>%
    as.data.frame() %>%
    unlist() %>%
    stringi::stri_unique() %>%
    stringi::stri_remove_empty_na()

  var_names <-
    unique_strings %>%
    snakecase::to_snake_case()

  var_patterns <- stringr::regex(unique_strings, ignore_case = TRUE)

  df_strings <- purrr::map2_dfc(var_patterns, var_names, function(x, y) {
    output_var <-
      df_input %>%
      dplyr::select({{ col_name }}) %>%
      unlist() %>%
      stringr::str_detect(x) %>%
      tibble::as_tibble_col(y)
  })

  final_df <-
    cbind(df_input, df_strings) %>%
    dplyr::select(-{{ col_name }})

  return(final_df)
}
