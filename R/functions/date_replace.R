date_replace <- function(data_import) {
  data_output <-
    data_import %>%
    dplyr::mutate(
      outside_interval_ind = dplyr::if_else(
        (date_from > period_to) | (period_from > date_to), 1, 0
      ) %>% tidyr::replace_na(0),
      period_from = dplyr::if_else(
        !is.na(date_from) & (date_from >= period_from) & (date_from <= period_to),
        floor(date_from / 10) * 10,
        period_from
      ),
      period_to = dplyr::if_else(
        !is.na(date_to) & (date_to <= period_to) & (date_to >= period_from),
        floor(date_to / 10) * 10,
        period_to
      )
    )

  return(data_output)
}
