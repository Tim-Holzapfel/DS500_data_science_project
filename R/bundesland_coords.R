local({
  bundesland_coords <-
    sf::read_sf(
      find_files("gadm36_DEU_1.shp")
    ) %>%
    dplyr::select(-matches("NAME\\_[0]|NL\\_NAME|GID|VARNAME|ENGTYPE|HASC")) %>%
    dplyr::rename(name = NAME_1, type = TYPE_1, cc = CC_1) %>%
    dplyr::mutate(
      bundes_ID = dplyr::row_number()
    )

  bundesland_centroids <-
    bundesland_coords %>%
    create_centroids()

  bundeskreise_coords <-
    sf::read_sf(
      find_files("gadm36_DEU_2.shp")
    ) %>%
    dplyr::select(-matches("NAME\\_[0|1]|NL\\_NAME|GID|VARNAME|ENGTYPE|HASC")) %>%
    dplyr::rename(name = NAME_2, type = TYPE_2, cc = CC_2)

  bundeskreise_centroids <-
    bundeskreise_coords %>%
    create_centroids()

  stadtkreise_coords <-
    sf::read_sf(
      find_files("gadm36_DEU_3.shp")
    ) %>%
    dplyr::select(-matches("NAME\\_[0|1|2]|NL\\_NAME|GID|VARNAME|ENGTYPE|HASC")) %>%
    dplyr::rename(name = NAME_3, type = TYPE_3, cc = CC_3)

  stadtkreise_centroids <-
    stadtkreise_coords %>%
    create_centroids()

  stadt_coords <-
    sf::read_sf(
      find_files("gadm36_DEU_4.shp")
    ) %>%
    dplyr::select(-matches("NAME\\_[0|1|2|3]|NL\\_NAME|GID|VARNAME|ENGTYPE|HASC")) %>%
    dplyr::rename(name = NAME_4, type = TYPE_4, cc = CC_4)

  stadt_centroids <-
    stadt_coords %>%
    create_centroids()

  big_cities <- c("Berlin", "Bielefeld", "Bochum", "Bonn", "Bremen", "Dortmund",
                  "Dresden", "Düsseldorf", "Duisburg", "Essen", "Frankfurt am Main",
                  "Hamburg", "Hannover", "Karlsruhe", "Köln", "Leipzig", "Mannheim",
                  "München", "Nürnberg", "Stuttgart", "Wuppertal")

  big_city_centroids <-
    stadt_centroids %>%
    dplyr::filter(name %in% big_cities)

}, envir = baseenv())

