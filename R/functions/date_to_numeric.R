date_to_numeric <- function(date_char) {
  date_numeric <-
    dplyr::if_else(
    condition = stringr::str_detect(date_char, stringr::regex("bce", ignore_case = TRUE)),
    true = paste0("-", stringr::str_remove_all(date_char, "[A-z ]+")),
    false = stringr::str_remove_all(date_char, "[A-z ]+")
  )

  date_numeric <- as.numeric(date_numeric)

  return(date_numeric)

}
