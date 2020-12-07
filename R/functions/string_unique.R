string_unique <- function(string_input) {
  inner_function <- function(string_input) {
    string_output <-
      string_input %>%
      stringr::str_split(pattern = "\\s", simplify = TRUE) %>%
      stringi::stri_unique() %>%
      paste0(collapse = " ")

    return(string_output)
  }

  environment(inner_function) <- rlang::global_env()

  string_final <- inner_function(string_input)

  return(string_final)
}
