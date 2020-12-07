site_pagination <- function() {

  pagination_fun <- function() {
    pagination_index_parent <- remDr$findElement("css", "div[class*=pagination-bottom-wrapper]")

    pagination_index_buttons <- pagination_index_parent$findChildElements("css", "a")

    pagination_buttons_values <-
      sapply(pagination_index_buttons, function(x) {
        x$getElementText()
      }) %>%
      unlist() %>%
      stringr::str_replace_all("\\D+|^$", "-99999") %>%
      as.integer()

    return(pagination_buttons_values)

  }

  environment(pagination_fun) <- rlang::global_env()

  pagination_buttons_values <- pagination_fun()

  return(pagination_buttons_values)

}
