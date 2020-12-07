baden_wuerttemberg_polygons <-
  sf::read_sf(
    dsn = find_files("baden-wuerttemberg_polygons.shp"),
    query = "
    SELECT *
    FROM \"baden-wuerttemberg_polygons\"
    LIMIT 100 OFFSET 200
    "
  )
geom_vars <-
  c(
    "name", "amenity", "landuse", "building", "leisure", "office",
    "place", "shop", "aeroway", "admin_leve", "boundary", "other_tags"
  )

geom_vars_cat <-
  geom_vars %>%
  paste0(collapse = ", ")

geom_layer <-
  sf::st_layers(find_files("baden-wuerttemberg_polygons.shp"))

geom_dns <-
  geom_layer$name %>%
  paste0("\"", ., "\"")

geom_dsn_path <- find_files("baden-wuerttemberg_polygons.shp")

# number of rows contained in the source
geom_rows <-
  geom_layer$features

geom_data_final <-
  tibble::tibble(
    .rows = geom_rows
  )

geom_data_final[, geom_vars] <- NA_character_
geom_data_final[, c("lat", "lng")] <- NA_real_

geom_limit <- 100000

i <- 0
j_t_1 <- 1
j_t_2 <- j_t_1 + geom_limit - 1





geom_data_loop <-
  sf::read_sf(
    dsn = geom_dsn_path,
    query = paste0(
      "SELECT ", geom_vars_cat, " FROM ", geom_dns,
      "LIMIT ", geom_limit, " OFFSET ", i
    )
  )

geom_loop_coords <-
  sf::st_coordinates(geom_data_loop) %>%
  as.data.frame() %>%
  dplyr::select(lat = Y, lng = X)



geom_data_final[j_t_1:j_t_2, c("lat", "lng")] <- geom_loop_coords








tt3 <- j_t_1:j_t_2




for (i in seq.int(from = 0, to = geom_rows, by = geom_limit)) {

  print(i)

  geom_data_loop <-
    sf::read_sf(
      dsn = geom_dsn_path,
      query = paste0(
        "SELECT ", geom_vars_cat, " FROM ", geom_dns,
        "LIMIT ", geom_limit, " OFFSET ", i
      )
    )

  geom_data_final <- rbind(geom_data_loop, geom_data_final)


}



library(sf)
nrows <- 5000000
df <- st_sf(id = 1:nrows, geometry = st_sfc(lapply(1:nrows, function(x) st_geometrycollection())))







#

t2 <- purrr::map_dfr(loop_satz_paths, readRDS)


t1 <- readRDS(loop_satz_paths[1])












saveRDS(geom_data_loop, file = file.path(temp_dir, "loop_satz_1"))





test1[, "geometry"] <- t1

class(test1) <- t5

























loop_satz_paths <- list.files(
  path = temp_dir,
  pattern = "loop\\_satz\\d{1,2}",
  full.names = TRUE
)

satz_final <- NULL
for (i in seq_along(loop_satz_paths)) {
  print(i)
  satz_loop <- readRDS(loop_satz_paths[i])
  satz_final <- rbind(satz_loop, satz_final)
}


















loop_satz_paths <- list.files(
  path = "D:/uni_tuebingen/Master/Kurse/DS500_data_science_project/data/temp",
  pattern = "loop\\_satz\\d{1,2}",
  full.names = TRUE
)




