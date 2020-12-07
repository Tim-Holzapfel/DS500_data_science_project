
clear()

bundesland_pages <- list.files(
  path = file.path(data_dir, "wg_gesucht/page_sources_federal_states"),
  pattern = "\\.html$",
  full.names = TRUE
) %>%
  stringr::str_subset("overview", negate = TRUE)

bundeland_complete <-
  purrr::map_dfr(bundesland_pages, function(x) {
    root_node <- xml2::read_html(x)
    grouping_links <-
      rvest::html_nodes(root_node, css = ".state_grouping_link") %>%
      rvest::html_attr("href") %>%
      tibble::as_tibble_col("path") %>%
      dplyr::mutate(
        city = stringr::str_extract(path, "(?<=anzeigen\\-in\\-)[^\\.]*"),
        city_number = stringr::str_extract(path, paste0("(?<=", city, "\\.)\\d+")) %>%
          as.integer(),
        city_string = paste0(city, ".", city_number),
        bundesland = stringr::str_remove(basename(x), "\\.html$")
      )
  })

# big cities are not part of list of other cities but are listed for each
# bundesland in a seperate list.
data_index <- xml2::read_html(bundesland_pages[1])

# Big cities are listed separately.
big_cities <- rvest::html_nodes(data_index, css = ".ul-bottomlinks a") %>%
  rvest::html_attr("href") %>%
  tibble::as_tibble_col("path") %>%
  dplyr::filter(
    stringr::str_detect(path, "zimmer")
  ) %>%
  dplyr::mutate(
    city = stringr::str_extract(path, "(?<=wg\\-zimmer\\-in\\-)[^\\.]*"),
    city_number = stringr::str_extract(path, paste0("(?<=", city, "\\.)\\d+")) %>%
      as.integer(),
    city_string = paste0(city, ".", city_number),
    bundesland = NA_character_
  ) %>%
  dplyr::distinct(city_number, .keep_all = TRUE)

bundeland_complete_ext <-
  bundeland_complete %>%
  rbind(big_cities)

save_file(bundeland_complete_ext, "city_index")
