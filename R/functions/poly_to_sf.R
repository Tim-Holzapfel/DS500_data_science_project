poly_to_sf <- function(map_poly) {
  nb_cen <- sf::st_centroid(map_poly)

  nb <- spdep::poly2nb(map_poly)

  index_not_null <- purrr::map(nb, function(x) {
    ifelse(length(x) == 1,
           x != 0,
           TRUE
    )
  }) %>%
    unlist() %>%
    which()

  nb_san <- nb[index_not_null]

  nb_filtered <- purrr::map(nb_san, function(x) {
    x %>%
      unlist() %>%
      as.vector() %>%
      nb_cen$geometry[.] %>%
      sf::st_coordinates()
  })

  # better solution to the above
  nb_cleared <- nb[nb %notin% c(0)]



  start_loop <- nb_filtered[[1]] %>% tibble::as_tibble()
  start_loop[, "id"] <- 1

  for (i in 2:length(nb_filtered)) {
    mid_loop <- nb_filtered[[i]] %>% tibble::as_tibble()
    mid_loop[, "id"] <- i

    start_loop <- rbind(start_loop, mid_loop)
  }

  nb_final <-
    sfheaders::sf_multilinestring(obj = start_loop, multilinestring_id = "id")

  crs_nb <- sf::st_crs(map_poly)$input

  sf::st_crs(nb_final) <- crs_nb

  return(nb_final)
}
