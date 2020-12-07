
clear()

i <- 1

crawl_rate <- 4

operation <- function() {
  missing_cities <- data.table::fread(
    input = "R/empty_cities.txt",
    header = FALSE,
    sep = "\n",
    encoding = "UTF-8",
    col.names = "city_string"
  )

  complete_cities <- data.table::fread(
    input = "R/complete_cities.txt",
    header = FALSE,
    sep = "\n",
    encoding = "UTF-8",
    col.names = "city_string"
  )

  city_index <-
    read_file("city_index") %>%
    dplyr::anti_join(missing_cities, by = "city_string") %>%
    dplyr::anti_join(complete_cities, by = "city_string") %>%
    dplyr::arrange(city_number) %>%
    dplyr::mutate(
      city_n = dplyr::row_number()
    )

  i <- 1 # city index
  j <- 0 # page index

  remDr <- generate_tor_driver(headless = TRUE)
  remDr$open(silent = TRUE)

  withRestarts(
    tryCatch(
      {
        for (i in 1:nrow(city_index)) {
          wg_link_comp <-
            paste0(
              "view-source:https://www.wg-gesucht.de/1-zimmer-wohnungen-und-wohnungen-und-haeuser-in-",
              city_index$city_string[i],
              ".1+2+3.1.0.html"
            )

          remDr$navigate(wg_link_comp)

          complete_page <- NULL
          while (is.null(complete_page)) {
            complete_page <-
              tryCatch(
                {
                  remDr$findElement("css", "html")
                },
                error = function(e) NULL
              )
          }

          offer_page_source <- complete_page$getElementText() %>% unlist()

          error_check <-
            paste0(offer_page_source, collapse = "") %>%
            stringr::str_extract("(?<=\\<title\\>).*(?=\\<\\/title\\>)")

          if (error_check == "\u00DCberpr\u00FCfung") stop("Website invoked bot check.")

          city_i <- city_index$city[i]

          # Some cities contain parentheses, which need to be escaped as they parentheses would
          # otherwise be interpreted as capture groups.
          city_pattern <-
            city_index$city_orig[i] %>%
            stringr::str_replace_all(c("\\(" = "\\\\(", "\\)" = "\\\\)"))

          number_results_pattern <- paste0(
            "(?<=1-Zimmer-Wohnungen, Wohnungen, Häuser in ",
            city_pattern,
            "\\:\\s)\\d+"
          )

          number_results <-
            offer_page_source %>%
            paste0(collapse = "") %>%
            stringr::str_extract(number_results_pattern) %>%
            as.integer()

          print(paste0(city_index$city[i], ": ", number_results, " Angebote."))

          # If the city contains no results then write the name of the city to a
          # blacklist file and move on to the next city
          if (number_results == 0) {
            write.table(
              city_index$city_string[i],
              file = "R/empty_cities.txt",
              sep = ",",
              append = TRUE,
              quote = FALSE,
              col.names = FALSE,
              row.names = FALSE
            )
            Sys.sleep(crawl_rate)
            remDr$deleteAllCookies()
            next
          }

          # If the city contains a non-zero amount of offers, write the source code
          # to a file.
          page_source_file_name <-
            paste(city_index$city_string[i], "page_1", sep = "_") %>%
            paste0(".html") %>%
            file.path(scrape_dir, .)

          fileConn <- file(page_source_file_name)
          writeLines(offer_page_source, fileConn)
          close(fileConn)

          # Number of pages for the given city. Because every page contains 20 offers,
          # the number of pages is the total number of offers divided by 20.
          end_page <- ceiling(number_results / 20)

          # If the city contains only one page then skip to the next city.
          if (end_page == 1) {
            write.table(
              city_index$city_string[i],
              file = "R/complete_cities.txt",
              sep = ",",
              append = TRUE,
              quote = FALSE,
              col.names = FALSE,
              row.names = FALSE
            )
            Sys.sleep(crawl_rate)
            remDr$deleteAllCookies()
            next
          }

          # Test if there are already existing source code pages for the city in question in addition
          # to the one created before
          existing_pages <- list.files(
            path = "data/wg_gesucht_page_sources"
          ) %>%
            tibble::as_tibble_col("path") %>%
            dplyr::mutate(
              city = stringr::str_extract(path, "^[^\\.]*"),
              page = stringr::str_extract(path, "(?<=page\\_)\\d+(?=\\.html)") %>% as.integer()
            ) %>%
            dplyr::filter(
              city == city_i
            )

          last_page <- max(existing_pages$page)

          if (last_page >= end_page) {
            write.table(
              city_index$city_string[i],
              file = "R/complete_cities.txt",
              sep = ",",
              append = TRUE,
              quote = FALSE,
              col.names = FALSE,
              row.names = FALSE
            )
            next
          } else if (!sjmisc::is_empty(existing_pages)) {
            start_page <- last_page
          }

          end_page_sub <- end_page - 1

          for (j in start_page:end_page_sub) {
            print(j + 1)
            wg_link_comp <-
              paste0(
                "view-source:https://www.wg-gesucht.de/1-zimmer-wohnungen-und-wohnungen-und-haeuser-in-",
                city_index$city_string[i],
                ".1+2+3.1.",
                j,
                ".html"
              )

            remDr$navigate(wg_link_comp)

            complete_page <- NULL
            while (is.null(complete_page)) {
              complete_page <-
                tryCatch(
                  {
                    remDr$findElement("css", "html")
                  },
                  error = function(e) NULL
                )
            }

            offer_page_source <- complete_page$getElementText() %>% unlist()

            error_check <-
              paste0(offer_page_source, collapse = "") %>%
              stringr::str_extract("(?<=\\<title\\>).*(?=\\<\\/title\\>)")

            if (error_check == "\u00DCberpr\u00FCfung") stop("Website invoked bot check.")

            # j + 1 because wg-gesucht.de starts counting at 0.
            page_source_file_name <-
              paste(city_index$city_string[i], "page", (j + 1), sep = "_") %>%
              paste0(".html") %>%
              file.path(scrape_dir, .)

            fileConn <- file(page_source_file_name)
            writeLines(offer_page_source, fileConn)
            close(fileConn)

            if (j == end_page_sub) {
              write.table(
                city_index$city_string[i],
                file = "R/complete_cities.txt",
                sep = ",",
                append = TRUE,
                quote = FALSE,
                col.names = FALSE,
                row.names = FALSE
              )
            }

            Sys.sleep(crawl_rate)
            remDr$deleteAllCookies()
          }
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









#
#
#
#
#
# html_decimal_date <- lubridate::decimal_date(html_date_created)
#
# file_name <- paste(sep = "_", city_string, "page", current_page, html_decimal_date) %>%
#   paste0(".html") %>%
#   file.path(scrape_dir, .)
#
# file.rename(from = html_link, to = file_name)
#
# current_page <-
#   html_source %>%
#   rvest::html_text(trim = TRUE) %>%
#   stringr::str_extract("(?<=current_page\\s\\s\\:\\s\\')\\d+(?=\\')")
#
# #html_date_created <- lubridate::as_date(file.info(html_link)$mtime)
#
# html_date_created <- file.info(html_link)$mtime
