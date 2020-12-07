replace_empty <- function(data_file) {
  return_file <-
    data_file %>%
    dplyr::mutate_all(list(~ dplyr::na_if(., "")))

  return(return_file)
}
