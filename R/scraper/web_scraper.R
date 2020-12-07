
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

remDr <- generate_driver(headless = TRUE)

n_cities <- nrow(cities)

remDr$open(silent = TRUE)
remDr$navigate("https://www.wg-gesucht.de/")


#   ____________________________________________________________________________
#   Automated Browser                                                       ####

# for (i in seq_len(n_cities)) {

i <- 1

city <- cities$city[i]

city_norm <- cities$city_norm[i]

print(city)

source("R/scraper/03_city_form_field.R")

source("R/scraper/04_dropdown_categories.R")

# start page search button field
search_button <- remDr$findElement("id", "search_button")
search_button$clickElement()
Sys.sleep(2)


# Testing if the city in question contains any results
source("R/scraper/05_verify_results.R")

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
remDr$mouseMoveToLocation(webElement = next_page_button)
Sys.sleep(0.5)
remDr$click()

#   ____________________________________________________________________________
#   Page loop                                                               ####

# j <- 2

if (end_page_sub > 200) end_page_sub <- 200

for (j in start_page:end_page_sub) {
  print(paste0("Page: ", j))
  source("R/scraper/wg_offers_loop.R")

  save_page_source()

  next_page_button <- remDr$findElement("css", "li.active + li a")
  remDr$mouseMoveToLocation(webElement = next_page_button)
  Sys.sleep(0.5)
  remDr$click()

  Sys.sleep(5)
}
























clear()

system("java_kill.bat")
# shell("java -jar selenium-server-standalone-4.0.0-alpha-2.jar", wait = F)



fprof <- makeFirefoxProfile(list(
  network.proxy.socks = "127.0.0.1", # for proxy settings specify the proxy host IP
  network.proxy.socks_port = 9150L, # proxy port. Last character "L" for specifying integer is very important and if not specified it will not have any impact
  network.proxy.type = 1L, # 1 for manual and 2 for automatic configuration script. here also "L" is important
  network.proxy.socks_version = 5L, # ditto
  network.proxy.socks_remote_dns = TRUE
))


rD <- rsDriver(
  port = 4445L, browser = "firefox", version = "latest", geckover = "latest", iedrver = NULL, phantomver = "2.1.1",
  verbose = TRUE, check = TRUE, extraCapabilities = fprof
) # works for selenium server: 3.3.1 and geckover: 0.15.0; Firefox: 52
remDr <- rD[["client"]]
remDr <- rD$client
remDr$navigate("https://check.torproject.org/") # should confirm tor is setup
remDr$navigate("http://whatismyip.org/") # should confirm tor is setup
