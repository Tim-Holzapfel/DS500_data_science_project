
# remDr$setTimeout(type = "page load", milliseconds = 5000)

n_wg_offers <- remDr$findElements("css", ".wgg_card.offer_list_item")


wg_offers_data_ids <- sapply(n_wg_offers, function(x) {
  x$getElementAttribute("data-id")
}) %>% unlist()

existings_wg_offers <- list.files(
  path = "data/inserate_source_code"
) %>%
  stringr::str_remove("\\.html")

index_non_existing_offers <- which(wg_offers_data_ids %notin% existings_wg_offers)

t <- 1

if (!sjmisc::is_empty(index_non_existing_offers)) {
  for (t in index_non_existing_offers) {
    print(paste0("Offer: ", t))

    # Waiting for page to be loaded
    wg_offers_list <- NULL
    while (is.null(wg_offers_list)) {
      wg_offers_list <-
        tryCatch(
          {
            remDr$findElements("css", ".wgg_card.offer_list_item")
          },
          error = function(e) {
            NULL
          }
        )
    }

    wg_offers_links <- sapply(wg_offers_list, function(x) {
      offer_link_raw <- x$findChildElements("css", "h3.truncate_title.noprint a")[[1]]
      # offer_link_raw <- x$findChildElements("css", "h3.truncate_title.noprint")
      # offer_link_raw$getElementAttribute("href")
    })

    wg_link_j <- wg_offers_links[[t]]

    wg_link_j$clickElement()

    Sys.sleep(5)

    complete_page <- NULL
    while (is.null(complete_page)) {
      complete_page <-
        tryCatch(
          {
            remDr$findElement("css", "html")
          },
          error = function(e) {
            NULL
          }
        )
      # loop until element with name <value> is found in <webpage url>
    }

    page_source_wg_offer <- remDr$getPageSource()[[1]]

    error_check <-
      paste0(page_source_wg_offer, collapse = "") %>%
      stringr::str_replace("\\\n|\\\r", "") %>%
      stringr::str_squish() %>%
      stringr::str_extract("(?<=\\<title\\>).*(?=\\<\\/title\\>)")

    if (error_check == "\u00DCberpr\u00FCfung") stop("Website invoked bot check.")


    page_source_file_name <-
      remDr$getCurrentUrl()[[1]] %>%
      stringr::str_extract("\\d+\\.html") %>%
      file.path("data/inserate_source_code", .)

    wg_offer_id <-
      remDr$getCurrentUrl()[[1]] %>%
      stringr::str_extract("\\d+(?=\\.html)") %>%
      paste0(., ".png") %>%
      file.path("data/screenshots", .)


    fileConn <- file(page_source_file_name)
    writeLines(page_source_wg_offer, fileConn)
    close(fileConn)

    remDr$screenshot(useViewer = FALSE, file = wg_offer_id)

    n_scroll <- 140
    skip_distance <- 20
    wait_time <- runif(n_scroll/skip_distance, 0.8, 1.2)
    skip_counter <- 1

    for (i in 1:n_scroll) {
      complete_page$sendKeysToElement(list(key = "down_arrow"))

      if (i %% skip_distance == 0) {
        Sys.sleep(wait_time[skip_counter])
        skip_counter <- skip_counter + 1
      }

    }

    Sys.sleep(1)

    remDr$goBack()
  }
}
