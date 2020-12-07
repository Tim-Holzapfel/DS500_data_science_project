create_geometry <- function(coordinates) {
  library(magrittr)
  # Coordinates (longitude, latitude) in matrix format.

  n <- nrow(coordinates)

  loop_anchor <- sf::st_sfc(sf::st_point(coordinates[1, ]))

  for (i in 2:n) {
    loop_mid <- sf::st_sfc(sf::st_point(coordinates[i, ]))

    loop_anchor <- rbind(loop_anchor, loop_mid)
  }

  geometry_df <- sf::st_sfc(loop_anchor)

  return(geometry_df)
}
