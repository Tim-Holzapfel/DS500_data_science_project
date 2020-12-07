fast_read <- function(csv_file, ...) {
  csv_file_mod <- paste0(csv_file, ".csv")

  found_file <- find_files(csv_file_mod)

  csv_table <-
    data.table::fread(
      file = found_file,
      encoding = "UTF-8",
      data.table = FALSE,
      logical01 = TRUE,
      strip.white = TRUE,
      na.strings = "",
      ...
    ) %>%
    dplyr::rename_with(.fn = snakecase::to_snake_case) %>%
    string_squish() %>%
    replace_empty() %>%
    dplyr::mutate(
      dplyr::across(.fns = type_convert_as_is)
    )
}
