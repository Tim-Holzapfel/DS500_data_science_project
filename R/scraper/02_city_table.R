cities <-
  sf::read_sf(find_files("gadm36_DEU_4.shp")) %>%
  as.data.frame() %>%
  rename_with_snakecase() %>%
  dplyr::select(-geometry, -matches("gid|varname|engtype|cc|name\\_0")) %>%
  dplyr::rename(
    bundesland = name_1, bundeskreis = name_2, verwaltungsgebiet = name_3,
    stadt = name_4, type = type_4
  ) %>%
  dplyr::mutate(
    city = paste0(stadt, " (", bundesland, ")"),
    city_norm = snakecase::to_snake_case(city, transliterations = "de-ASCII")
  ) %>%
  # There are a number of cities in in the same Bundesland with the same name.
  # Because wg-gesucht.de only distinguishes between Bundesland and city, but does
  # not account for the Verwaltungsgebiete, cities with the same name in the same
  # Bundesland are sliced and only the first is selected.
  dplyr::group_by(bundesland, stadt) %>%
  dplyr::arrange(bundesland, stadt) %>%
  dplyr::slice_head() %>%
  dplyr::ungroup() %>%
  dplyr::anti_join(missing_cities, by = c("city" = "cities_missing")) %>%
  dplyr::anti_join(empty_cities, by = c("city" = "cities_empty")) %>%
  dplyr::left_join(geonames, by = c("city" = "stadt_bundesland")) %>%
  dplyr::relocate(population) %>%
  dplyr::arrange(dplyr::desc(population), bundesland, stadt)
