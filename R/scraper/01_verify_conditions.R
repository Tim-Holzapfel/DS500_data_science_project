
# List of cities that could not be found on wg-gesucht.de
if (file.exists("R/scraper/missing_cities.txt")) {
  missing_cities <- data.table::fread(
    input = "R/scraper/missing_cities.txt",
    header = FALSE,
    sep = "\n",
    encoding = "UTF-8",
    col.names = "cities_missing"
  ) %>%
    dplyr::distinct()
  if (nrow(missing_cities) == 0) {
    missing_cities <- tibble::tibble(
      cities_missing = NA_character_
    )
  }
} else {
  missing_cities <- tibble::tibble(
    cities_missing = NA_character_
  )
}

# List of cities that are listed on wg-gesucht.de but which are empty (0 results)
if (file.exists("R/scraper/empty_cities.txt")) {
  empty_cities <- data.table::fread(
    input = "R/scraper/empty_cities.txt",
    header = FALSE,
    sep = "\n",
    encoding = "UTF-8",
    col.names = "cities_empty"
  ) %>%
    dplyr::distinct()
  if (nrow(empty_cities) == 0) {
    empty_cities <- tibble::tibble(
      cities_empty = NA_character_
    )
  }
} else {
  empty_cities <- tibble::tibble(
    cities_empty = NA_character_
  )
}

existing_pages <-
  list.files(
    path = scrape_dir,
    pattern = "\\.html$"
  ) %>%
  tibble::as_tibble_col(column_name = "page_path") %>%
  dplyr::mutate(
    city = stringr::str_remove(page_path, "\\_page\\_.*\\.html"),
    date = stringr::str_extract(page_path, "\\d{4}\\_\\d{2}\\_\\d{2}") %>%
      lubridate::as_date(format = "%Y_%m_%d"),
    page = stringr::str_extract(page_path, "(?<=\\_page\\_)\\d+(?=\\_)") %>% as.integer()
  ) %>%
  dplyr::group_by(city, date) %>%
  dplyr::slice_max(page) %>%
  dplyr::filter(date == lubridate::as_date(date_snakecase, format = "%Y_%m_%d"))
