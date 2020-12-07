retrieve_page_source <- function() {

  page_source_function <- function() {
    page_source <- NULL
    while (is.null(page_source)) {
      page_source <-
        tryCatch(
          {
            remDr$findElement("css", ":root")
          },
          error = function(e) NULL
        )
    }

    offer_page_source <- page_source$getElementText() %>% unlist()

    error_check <-
      offer_page_source %>%
      # It is necessary to remove all page breaks (\n) as . matches any character
      # EXCEPT page breaks. This can lead to function misbehavior in case the
      # title should span multiple rows.
      stringr::str_remove_all("\\\n") %>%
      stringr::str_extract("(?<=\\<title\\>).*(?=\\<\\/title\\>)")

    if (error_check == "\u00DCberpr\u00FCfung") stop("Website invoked bot check.")

    return(offer_page_source)
  }

  environment(page_source_function) <- rlang::caller_env()

  html_source_code <- page_source_function()

  return(html_source_code)
}
