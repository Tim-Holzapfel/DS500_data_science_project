sf_index_crosses_transform <- function(list_input) {
  class(list_input) <- "list"

  list_output <-
    purrr::map(list_input, function(x) {
      if (sjmisc::is_empty(x)) {
        return_x <- NA_character_
      } else {
        return_x <- paste0(x, collapse = ",")
      }
      return(return_x)
    }) %>% unlist()

  list_final <-
    list_output %>%
    stringr::str_split(",", simplify = TRUE) %>%
    type_convert_as_is()

  return(list_final)
}
