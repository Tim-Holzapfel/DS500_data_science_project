dec_places <- function(x) {
  if (all(x %% 1 == 0)) {
    return(0)
  } else {
    stringr::str_extract(as.character(x), "(?<=\\.)\\d+") %>%
      stringi::stri_remove_empty_na() %>%
      nchar() %>%
      max(na.rm = TRUE)
  }
}
