
clear()
library(xml2)

library(RSelenium)

#   ____________________________________________________________________________
#   Preparations                                                            ####

geonames <- fst::read_fst(file.path(output_dir, "geonames_de.fst"))

# Check various conditions
source("R/scraper/01_verify_conditions.R")

# Load list of cities
source("R/scraper/02_city_table.R")

system("java_kill.bat")

fprof <- makeFirefoxProfile(list(
  network.proxy.socks = "127.0.0.1", # for proxy settings specify the proxy host IP
  network.proxy.socks_port = 9150L,
  network.proxy.type = 1L,
  network.proxy.socks_version = 5L,
  network.proxy.socks_remote_dns = TRUE
))

rD <- rsDriver(
  port = 4445L,
  browser = "firefox",
  version = "latest",
  geckover = "latest",
  iedrver = NULL,
  phantomver = "2.1.1",
  verbose = TRUE,
  check = TRUE,
  extraCapabilities = fprof
)
remDr <- rD[["client"]]
remDr <- rD$client
# remDr$navigate("https://check.torproject.org/") # should confirm tor is setup
# remDr$navigate("http://whatismyip.org/") # should confirm tor is setup

n_cities <- nrow(cities)

remDr$navigate("https://www.wg-gesucht.de/")

accept_conditions <- remDr$findElement("css", "#cmpwelcomebtnyes")
accept_conditions$clickElement()

#   ____________________________________________________________________________
#   Automated Browser                                                       ####

for (i in seq_len(n_cities)) {
  i <- 1

  city <- cities$city[i]

  city_norm <- cities$city_norm[i]

  print(city)

  source("R/scraper/03_city_form_field.R")

  source("R/scraper/04_dropdown_categories.R")

  # start page search button field
  search_button <- remDr$findElement("id", "search_button")
  search_button$clickElement()
  Sys.sleep(1)

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
