gen_factor <- function(data_file, convert_numeric = FALSE) {
  data_temp <-
    data_file %>%
    dplyr::ungroup() %>%
    dplyr::relocate(dplyr::matches("\\d$"), .after = dplyr::last_col()) %>%
    dplyr::mutate(
      dplyr::across(.fns = type_convert_as_is)
    )

  var_names_num_factor <-
    data_temp %>%
    dplyr::select(where(is.numeric)) %>%
    dplyr::summarise(
      dplyr::across(
        .fns = function(x) {
          # X cannot contain NA values or everything it touches will turn into NA
          x <- x %>%
            stringi::stri_remove_empty_na() %>%
            type_convert_as_is()

          # First criterion: Values range between 0 and 20
          crit_1 <- all(dplyr::between(x, 0, 20))
          # Second criterion: only 12 unique values
          crit_2 <- length(unique(x)) <= 30

          result <- all(crit_1, crit_2)
        }
      )
    ) %>%
    unlist() %>%
    which() %>%
    names()

  var_names_char_factor <-
    data_temp %>%
    dplyr::select(where(is.character)) %>%
    dplyr::select(-dplyr::any_of(c("nga", "polity"))) %>%
    names()

  var_names_binary <-
    data_temp %>%
    dplyr::select(where(is.logical)) %>%
    names()



  data_output <-
    data_temp %>%
    dplyr::mutate(
      dplyr::across(
        .cols = dplyr::all_of(var_names_binary),
        .fns = function(x) {
          var_binary <- stringr::str_replace_all(
            as.character(x),
            c("TRUE" = "1", "FALSE" = "0")
          )
          var_result <- as.integer(var_binary)
          return(var_result)
        }
      )
    ) %>%
    dplyr::mutate(
      dplyr::across(
        .cols = dplyr::all_of(var_names_char_factor),
        .fns = function(x) {
          results <- forcats::as_factor(x)
          return(results)
        }
      )
    )

  if (convert_numeric == FALSE) {
    return(data_output)
  } else {
    data_output_mod <-
      data_output %>%
      dplyr::mutate(
        dplyr::across(
          .cols = dplyr::all_of(var_names_num_factor),
          .fns = function(x) factor(x, ordered = TRUE)
        )
      )
    return(data_output_mod)
  }
}
