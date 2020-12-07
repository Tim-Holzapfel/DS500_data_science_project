clear()

operation <- function() {
  crawl_rate <- 3

  existing_source_codes <-
    list.files(scrape_dir_single) %>%
    stringr::str_remove("\\.html") %>%
    tibble::as_tibble_col(column_name = "ID")

  wg_offers <-
    read_file("real_estates_overview_dataset") %>%
    dplyr::anti_join(existing_source_codes, by = "ID")

  remDr <- generate_tor_driver(headless = TRUE)

  withRestarts(
    tryCatch(
      {
        for (i in 1:nrow(wg_offers)) {
          print(i)

          wg_offer_url <-
            paste0("view-source:", wg_offers$offer_url[i])
          wg_offer_id <- wg_offers$ID[i]

          remDr$navigate(wg_offer_url)

          page_source <- retrieve_page_source()

          page_source_file_name <-
            paste0(wg_offer_id, ".html") %>%
            file.path(
              data_dir,
              "wg_gesucht/page_sources_real_estates", .
            )

          save_source(page_source, page_source_file_name)

          remDr$deleteAllCookies()

          Sys.sleep(crawl_rate)
        }
      },
      error = function(e) {
        invokeRestart("retry")
      }
    ),
    retry = function() {
      remDr$quit()
      message("Changing Proxy")
      operation()
    }
  )
}

operation()
