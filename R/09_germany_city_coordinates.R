
clear()

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

saveRDS(bundesland_coords, "output/bundesland_coords.RDS")
saveRDS(bundesland_centroids, "output/bundesland_centroids.RDS")


bundeskreise_coords <-
  sf::read_sf(
    find_files("gadm36_DEU_2.shp")
  ) %>%
  dplyr::select(-matches("NAME\\_[0|1]|NL\\_NAME|GID|VARNAME|ENGTYPE|HASC")) %>%
  dplyr::rename(name = NAME_2, type = TYPE_2, cc = CC_2)

saveRDS(bundeskreise_coords, "output/bundeskreise_coords.RDS")

stadtkreise_coords <-
  sf::read_sf(
    find_files("gadm36_DEU_3.shp")
  ) %>%
  dplyr::select(-matches("NAME\\_[0|1|2]|NL\\_NAME|GID|VARNAME|ENGTYPE|HASC")) %>%
  dplyr::rename(name = NAME_3, type = TYPE_3, cc = CC_3)

saveRDS(stadtkreise_coords, "output/stadtkreise_coords.RDS")

stadt_coords <-
  sf::read_sf(
    find_files("gadm36_DEU_4.shp")
  ) %>%
  dplyr::select(-matches("NAME\\_[0|1|2|3]|NL\\_NAME|GID|VARNAME|ENGTYPE|HASC")) %>%
  dplyr::rename(name = NAME_4, type = TYPE_4, cc = CC_4)

saveRDS(stadt_coords, "output/stadt_coords.RDS")

germany_centroid <-
  sf::st_union(bundesland_coords) %>%
  sf::st_centroid() %>%
  sf::st_coordinates() %>%
  as.data.frame()

saveRDS(germany_centroid, "output/germany_centroid.RDS")


