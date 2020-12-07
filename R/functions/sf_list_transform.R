sf_list_transform <- function(list_input) {
  class(list_input) <- "list"

  list_output <-
    purrr::map(list_input, function(x) {
      if (sjmisc::is_empty(x)) {
        return_x <- NA_real_
      } else {
        return_x <- x
      }
      return(return_x)
    }) %>% unlist()

  return(list_output)
}
