replicate_rows <- function(data_import) {


  # Check to make sure that all necessary variables are part of the dataset
  pass_condition <-
    all(c("period_from", "period_to", "period_duration") %in% names(data_import))

  error_massage <-
    "Either period_from, period_to or period_duration is either not part of the
  dataframe or is called differently. Maybe forgot to rename date_from to perod_from?"

  if (pass_condition == FALSE) stop(error_massage)



  data_temp <-
    data_import %>%
    dplyr::group_by(period_from, period_to) %>%
    dplyr::mutate(
      id_group = dplyr::cur_group_id()
    ) %>%
    dplyr::arrange(id_group) %>%
    dplyr::mutate(
      id_rows = dplyr::cur_group_rows(),
      id_n = dplyr::n()
    )

  loop_id_group <- unique(data_temp$id_group)

  loop_data <-
    data_temp %>%
    dplyr::distinct(id_group, .keep_all = TRUE) %>%
    dplyr::select(period_from, period_to, period_duration, id_group, id_n) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      period_mult = period_duration * id_n
    )


  id_rows_rep <- rep(loop_data$period_duration, times = loop_data$id_n)

  rows_total_rep <- rep(data_temp$id_rows, times = id_rows_rep)

  rep_id_groups <- rep(loop_data$id_group, times = loop_data$period_duration)

  mult_period_from <- rep(loop_data$period_from, times = loop_data$id_n)

  mult_period_duration <- rep(loop_data$period_duration, times = loop_data$id_n)

  rep_seq_period_from <-
    sequence(mult_period_duration, from = mult_period_from, by = 10)

  rep_seq_period_to <- rep_seq_period_from + 10

  data_output <- data_temp[rows_total_rep, ]

  data_output$period_from <- rep_seq_period_from

  data_output$period_to <- rep_seq_period_to

  data_output_final <-
    data_output %>%
    dplyr::select(-c("id_group", "id_rows", "id_n")) %>%
    dplyr::ungroup() %>%
    as.data.frame()
}
