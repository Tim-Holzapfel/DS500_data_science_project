date_extract <- function(value_from_df) {

  date_final <-
    value_from_df %>%
    dplyr::mutate(
      id_duration_first = dplyr::if_else(
        variable == "duration", 1, 2
      )
    ) %>%
    dplyr::arrange(nga, polity, id_duration_first) %>%
    dplyr::mutate(
      bce_true = stringr::str_detect(value_from, stringr::regex("bce", ignore_case = TRUE)),
      ce_true = stringr::str_detect(value_from, stringr::regex("(?<!b)ce", ignore_case = TRUE)),
      both_true = bce_true & ce_true,
      period_from = stringi::stri_extract_first_regex(value_from, "\\d+"),
      period_from = dplyr::if_else(
        bce_true == TRUE,
        paste0("-", period_from),
        period_from
      ),
      period_from = as.integer(period_from),
      period_to = stringi::stri_extract_last_regex(value_from, "\\d+"),
      period_to = dplyr::if_else(
        (bce_true == TRUE) & (both_true == FALSE),
        paste0("-", period_to),
        period_to
      ),
      period_to = as.integer(period_to)
    ) %>%
    dplyr::mutate(
      period_from = floor(period_from / 10) * 10,
      period_to = ceiling(period_to / 10) * 10,
      polity = stringr::str_remove(polity, "\\*+") %>%
        stringi::stri_trans_tolower(),
      period_from = dplyr::if_else(
        id_duration_first == 1,
        period_from,
        NA_real_
      ),
      period_to = dplyr::if_else(
        id_duration_first == 1,
        period_to,
        NA_real_
      )
    ) %>%
    tidyr::fill(period_from, period_to, .direction = "down") %>%
    dplyr::select(-c(bce_true, ce_true, both_true, id_duration_first))

  return(date_final)

}
