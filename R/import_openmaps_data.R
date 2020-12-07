
clear()

plz_data <- data.table::fread(
  file = find_files("geonames_plz_de.txt"),
  encoding = "UTF-8",
  blank.lines.skip = TRUE,
  # Some PLZs have a leading zero
  keepLeadingZeros = TRUE,
  col.names = c(
    "Country",
    "plz",
    "city",
    "bundesland",
    "bundesland_abbr",
    "V6",
    "V7",
    "region",
    "V9",
    "latitude",
    "longitude",
    "V12"
  )
) %>%
  sfheaders::sf_point(x = "longitude", y = "latitude", keep = TRUE) %>%
  dplyr::select(plz, city, bundesland, region)

sf::st_crs(plz_data) <- TA

baden_w_full <-
  sf::read_sf(
    dsn = "E:/z_open_street_maps/baden-wuerttemberg/baden_wuerttemberg_lines/baden_wuerttemberg_lines_full.shp"
  )



baden_w_names <-
  baden_w_full %>%
  dplyr::filter(!is.na(name)) %>%
  dplyr::filter(highway %in%
                  c("primary", "secondary", "tertiary", "living_street", "pedestrian",
                    "residential", "unclassified")
  )


baden_w_names_cent <-
  baden_w_names

#sf::st_centroid()

baden_w_names_cent$city <- NA_character_
baden_w_names_cent$plz <- NA_character_

t1 <- sf::st_nearest_feature(baden_w_names_cent, plz_data)



baden_w_names_cent$city <- plz_data$city[t1]
baden_w_names_cent$plz <- plz_data$plz[t1]

baden_w_names_cent <-
  baden_w_names_cent %>%
  dplyr::relocate(city, plz)





openmaps_data_paths <-
  list.files(
    path = "E:/z_open_street_maps",
    recursive = TRUE,
    full.names = TRUE
  ) %>%
  stringr::str_subset("^((?!full).)*\\.shp$")

x <- openmaps_data_paths[1]



berlin_lines_full <-
  sf::read_sf(
    dsn = "E:/z_open_street_maps/berlin/berlin_lines/berlin_lines_full.shp"
  )


berlin_lines <-
  sf::read_sf(
    dsn = "E:/z_open_street_maps/berlin/berlin_lines/berlin_lines.shp"
  )



baden_w <-
  sf::read_sf(
    dsn = "E:/z_open_street_maps/baden-wuerttemberg/baden_wuerttemberg_lines/baden_wuerttemberg_lines.shp"
  )

baden_w_full_sub <-
  baden_w_full %>%
  dplyr::filter(!is.na(name) & !is.na(postal_cod))














openstreet_data <- sf::read_sf(
  dsn = x
) %>%
  dplyr::left_join(plz_data, by = c("postal_cod" = "plz")) %>%
  dplyr::relocate(city) %>%
  dplyr::filter(is.na(city))

openstreet_data <- purrr::map_dfr(openmaps_data_paths, function(x) {
  sf::read_sf(
    dsn = x
  )
})


saveRDS(openstreet_data, "output/openstreet_data.RDS")
