# Waiting for page to be loaded
number_results_object <- NULL
while (is.null(number_results_object)) {
  number_results_object <-
    tryCatch(
      {
        # Page navigation buttons at the end of the site
        remDr$findElement("css", "h1.headline.headline-default")
      },
      error = function(e) {
        NULL
      }
    )
  # loop until element with name <value> is found in <webpage url>
}

number_results <-
  number_results_object$getElementText() %>%
  unlist() %>%
  stringr::str_extract("\\d+(?=\\sAngebot)") %>%
  as.integer()

if (number_results == 0) {
  write.table(
    city_norm,
    file = "R/empty_cities.txt",
    sep = ",",
    append = TRUE,
    quote = FALSE,
    col.names = FALSE,
    row.names = FALSE
  )
  next
}
