
clear()
library(xml2)
library(RSelenium)

i <- 1
geonames <- fst::read_fst(file.path(output_dir, "geonames_de.fst"))

# Check various conditions
source("R/scraper/01_verify_conditions.R")

# Load list of cities
source("R/scraper/02_city_table.R")





# remDr <- generate_tor_driver()
# remDr$open()
# remDr$deleteAllCookies()
# remDr$setWindowPosition(x = -1877, y = 28)
# remDr$maxWindowSize()
# remDr$setTimeout(type = "page load", milliseconds = 10000)
# remDr$setTimeout(type = "implicit", milliseconds = 10000)
# remDr$setTimeout(type = "script", milliseconds = 10000)
#
# n_cities <- nrow(cities)
#
# remDr$navigate("https://www.wg-gesucht.de/")
#
# accept_conditions <- remDr$findElement("css", "#cmpwelcomebtnyes")
# accept_conditions$clickElement()
#
# city <- cities$city[i]
#
# city_norm <- cities$city_norm[i]
#
# print(city)
#
#
#
#
#
# source("R/scraper/03_city_form_field.R")
# Sys.sleep(2)
# source("R/scraper/04_dropdown_categories.R")
#
#
# # start page search button field
# search_button <- remDr$findElement("id", "search_button")
# search_button$clickElement()
# Sys.sleep(5)
#
# # Testing if the city in question contains any results
# # source("R/scraper/05_verify_results.R")
#
# save_page_source()
#
# # Get page source of individual links
# source("R/scraper/wg_offers_loop.R")



operation <- function() {
  message(paste0("Starting Browser"))
  withRestarts(
    tryCatch(
      {

        remDr <- generate_tor_driver()
        remDr$open()
        remDr$deleteAllCookies()
        remDr$setWindowPosition(x = -1877, y = 28)
        remDr$maxWindowSize()
        remDr$setTimeout(type = "page load", milliseconds = 10000)
        remDr$setTimeout(type = "implicit", milliseconds = 10000)
        remDr$setTimeout(type = "script", milliseconds = 10000)

        n_cities <- nrow(cities)

        remDr$navigate("https://www.wg-gesucht.de/")

        accept_conditions <- remDr$findElement("css", "#cmpwelcomebtnyes")
        accept_conditions$clickElement()

        Sys.sleep(5)

        #   ____________________________________________________________________________
        #   Automated Browser                                                       ####

        for (i in seq_len(n_cities)) {

          city <- cities$city[i]

          city_norm <- cities$city_norm[i]

          print(city)

          source("R/scraper/03_city_form_field.R")
          Sys.sleep(2)
          source("R/scraper/04_dropdown_categories.R")

          # start page search button field
          search_button <- remDr$findElement("id", "search_button")
          search_button$clickElement()
          Sys.sleep(5)

          # Testing if the city in question contains any results
          # source("R/scraper/05_verify_results.R")

          save_page_source()

          # Get page source of individual links
          source("R/scraper/wg_offers_loop.R")

          # Number of pages / value of the last page
          end_page <- site_pagination() %>% max()

          end_page_sub <- end_page - 1

          # If there is only one page, looping through the pages obviously won't work
          if (sjmisc::is_empty(end_page)) {
            next
          }

          source("R/scraper/06_testing_for_existing_pages.R")

          if (!sjmisc::is_empty(existing_pages_index)) {
            start_page <- page_button_value
          } else {
            start_page <- 2
          }

          next_page_button <- remDr$findElement("css", "li.active + li a")
          next_page_button$clickElement()

          Sys.sleep(0.5)


          #   ____________________________________________________________________________
          #   Page loop                                                               ####

          # j <- 2

          if (end_page_sub > 200) end_page_sub <- 200

          for (j in start_page:end_page_sub) {
            print(paste0("Page: ", j))
            source("R/scraper/wg_offers_loop.R")

            save_page_source()

            next_page_button <- remDr$findElement("css", "li.active + li a")
            next_page_button$clickElement()
            Sys.sleep(5)
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































































































operation <- function() {
  message(paste0("Starting Browser"))
  withRestarts(
    tryCatch(
      {

        remDr <- generate_tor_driver()
        remDr$open()
        remDr$deleteAllCookies()
        remDr$setWindowPosition(x = -1877, y = 28)
        remDr$maxWindowSize()
        remDr$setTimeout(type = "page load", milliseconds = 10000)
        remDr$setTimeout(type = "implicit", milliseconds = 10000)
        remDr$setTimeout(type = "script", milliseconds = 10000)

        n_cities <- nrow(cities)

        remDr$navigate("https://www.wg-gesucht.de/")

        accept_conditions <- remDr$findElement("css", "#cmpwelcomebtnyes")
        accept_conditions$clickElement()

        Sys.sleep(5)

        #   ____________________________________________________________________________
        #   Automated Browser                                                       ####

        for (i in seq_len(n_cities)) {

          city <- cities$city[i]

          city_norm <- cities$city_norm[i]

          print(city)

          source("R/scraper/03_city_form_field.R")
          Sys.sleep(2)
          source("R/scraper/04_dropdown_categories.R")

          # start page search button field
          search_button <- remDr$findElement("id", "search_button")
          search_button$clickElement()
          Sys.sleep(5)

          # Testing if the city in question contains any results
          # source("R/scraper/05_verify_results.R")

          save_page_source()

          # Get page source of individual links
          source("R/scraper/wg_offers_loop.R")

          # Number of pages / value of the last page
          end_page <- site_pagination() %>% max()

          end_page_sub <- end_page - 1

          # If there is only one page, looping through the pages obviously won't work
          if (sjmisc::is_empty(end_page)) {
            next
          }

          source("R/scraper/06_testing_for_existing_pages.R")

          if (!sjmisc::is_empty(existing_pages_index)) {
            start_page <- page_button_value
          } else {
            start_page <- 2
          }

          next_page_button <- remDr$findElement("css", "li.active + li a")
          next_page_button$clickElement()

          Sys.sleep(0.5)


          #   ____________________________________________________________________________
          #   Page loop                                                               ####

          # j <- 2

          if (end_page_sub > 200) end_page_sub <- 200

          for (j in start_page:end_page_sub) {
            print(paste0("Page: ", j))
            source("R/scraper/wg_offers_loop.R")

            save_page_source()

            next_page_button <- remDr$findElement("css", "li.active + li a")
            next_page_button$clickElement()
            Sys.sleep(5)
          }
        }

      },
      error = function(e) {
        invokeRestart("retry")
      }
    ),
    retry = function() {
      Sys.sleep(4)

      remDr$quit()

      message("Changing Proxy")

      operation()
    }
  )
}

operation()
