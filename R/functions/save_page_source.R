
save_page_source <- function() {
  page_source <- function() {

    page_loaded <- NULL
    while (is.null(page_loaded)) {
      page_loaded <-
        tryCatch(
          {
            remDr$findElements("css", ".wgg_card.offer_list_item")
          },
          error = function(e) {
            NULL
          }
        )
      # loop until element with name <value> is found in <webpage url>
    }

    page_source <-
      remDr$getPageSource() %>%
      unlist()

    error_check <- remDr$findElements("css", ".wgg_card.offer_list_item")

    if (sjmisc::is_empty(error_check)) stop("Website invoked a bot check.")

    # In case no page information was given, assume that it's the first page
    if (!exists("j")) j <- 1

    page_source_file_name <-
      paste(city_norm, "page", j, date_snakecase, sep = "_") %>%
      paste0(".html") %>%
      file.path(scrape_dir, .)

    save_source(page_source, page_source_file_name)
  }

  environment(page_source) <- rlang::global_env()

  page_source()
}
