create_centroids <- function(sf_dataframe, return_coords = FALSE) {
  sf_dataframe_temp <- sf_dataframe

  sf_dataframe_no_geom <- sf_dataframe_temp

  sf::st_geometry(sf_dataframe_no_geom) <- NULL

  if (return_coords == FALSE) {

    sf_dataframe_crs <- sf::st_crs(sf_dataframe_temp)

    sf_centroids <-
      sf_dataframe_temp %>%
      sf::as_Spatial() %>%
      geosphere::centroid() %>%
      sfheaders::sf_point() %>%
      cbind(sf_dataframe_no_geom)

    sf::st_crs(sf_centroids) <- sf_dataframe_crs

  }

  if (return_coords == TRUE) {

    sf_centroids <-
      sf_dataframe_temp %>%
      sf::as_Spatial() %>%
      geosphere::centroid() %>%
      as.data.frame() %>%
      dplyr::rename(lon = V1, lat = V2) %>%
      cbind(sf_dataframe_no_geom) %>%
      dplyr::relocate(lon, lat, .after = dplyr::last_col())

  }

  return(sf_centroids)
}
