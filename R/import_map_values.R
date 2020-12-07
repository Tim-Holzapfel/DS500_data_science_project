
clear()



t1 <- sf::read_sf(
  dsn = "E:/z_open_street_maps/berlin-latest.osm/berlin_lines/berlin_lines.shp"
)

t1full <- sf::read_sf(
  dsn = "E:/z_open_street_maps/berlin-latest.osm/berlin_lines/berlin_lines_full.shp"
) %>%
  dplyr::filter(!is.na(name) & !is.na(postal_cod))












