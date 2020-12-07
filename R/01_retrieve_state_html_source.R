
clear()

library(RSelenium)
library(rvest)
library(xml2)

#   ____________________________________________________________________________
#   Retrieving HTML links of federal states                                 ####

remDr <- generate_tor_driver(headless = FALSE)
remDr$navigate("view-source:https://www.wg-gesucht.de/")

page_source <- retrieve_page_source()

page_source_file_name <-
  file.path(
    data_dir,
    "wg_gesucht/page_sources_federal_states/federal_states_html_links_overview.html"
  )

save_source(page_source, page_source_file_name)

remDr$closeall()


#   ____________________________________________________________________________
#   Processing retrieved HTML links                                         ####

wg_index <- xml2::read_html(page_source_file_name)

buttom_links <- rvest::html_nodes(wg_index, css = ".ul-bottomlinks")

bundesland_index <-
  rvest::html_nodes(wg_index, css = ".ul-bottomlinks h4") %>%
  rvest::html_text() %>%
  stringr::str_which("Bundesländer|(^\\s$)")

bundesland_links <-
  buttom_links[bundesland_index] %>%
  rvest::html_nodes(css = "a") %>%
  rvest::html_attr("href") %>%
  paste0("view-source:", .)

bundesland_name <-
  bundesland_links %>%
  stringr::str_extract("(?<=staedte\\-in\\-)[^\\.]*") %>%
  snakecase::to_snake_case()

#   ____________________________________________________________________________
#   Looping through federal states                                          ####

remDr <- generate_tor_driver()

for (i in 1:16) {
  remDr$navigate(bundesland_links[i])

  page_source <- retrieve_page_source()

  page_source_file_name <-
    paste0(bundesland_name[i], ".html") %>%
    file.path(data_dir, "wg_gesucht/page_sources_federal_states", .)

  save_source(page_source, page_source_file_name)

  Sys.sleep(4)
}

remDr$closeall()
