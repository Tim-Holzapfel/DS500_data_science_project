# Testing if there are already pages existing for the city in question and, if so,
# continue where we left off last time instead of starting all over again.
existing_pages_index <- which(existing_pages$city %in% city_norm)

if (!sjmisc::is_empty(existing_pages_index)) {
  last_page <- existing_pages$page[existing_pages_index]
  page_button_value <- 0L

  while (page_button_value != last_page) {

    # Waiting for page to be loaded
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

    n_scroll <- 100
    skip_distance <- 20
    wait_time <- runif(n_scroll / skip_distance, 0.8, 1.2)
    skip_counter <- 1

    for (i in 1:n_scroll) {
      complete_page$sendKeysToElement(list(key = "down_arrow"))

      if (i %% skip_distance == 0) {
        Sys.sleep(wait_time[skip_counter])
        skip_counter <- skip_counter + 1
      }
    }

    Sys.sleep(1)

    pagination_index_parent <- remDr$findElement("css", "div[class*=pagination-bottom-wrapper]")
    pagination_index_buttons <- pagination_index_parent$findChildElements("css", "a")
    pagination_buttons_values <-
      sapply(pagination_index_buttons, function(x) {
        x$getElementText()
      }) %>%
      unlist() %>%
      stringr::str_replace_all("\\D+|^$", "-99999") %>%
      as.integer()

    # The number of available pagination buttons is rather limited. It goes until
    # 11 and after that offers rather large intervals. We choose the available page button
    # that is closest to our last page
    closest_page_index <-
      DescTools::Closest(
        pagination_buttons_values,
        last_page,
        which = TRUE
      )

    page_button_value <-
      DescTools::Closest(
        pagination_buttons_values,
        last_page
      )

    page_button <- pagination_index_buttons[[closest_page_index]]
    page_button$clickElement()
    Sys.sleep(5)
  }

}

