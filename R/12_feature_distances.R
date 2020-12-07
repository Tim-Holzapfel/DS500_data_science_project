
clear()

library(shiny)
library(leaflet)
library(shinyjs)

real_estate_objects <- readRDS(file.path(output_dir, "real_estate_objects.RDS"))
data_results <- readRDS(file.path(output_dir, "data_results.RDS"))

var_group_table <- readRDS(file.path(output_dir, "var_group_table.RDS"))


i <- 1
j <- 4
city_list <- unique(real_estate_objects$city)
group_list <- as.character(var_group_table$var_group)

#group_list <- group_list[1:2]

active_city_name <- city_list[i]
active_city <-
  data_results %>%
  dplyr::group_by(var_group)

active_group_name <- as.character(group_list[j])

cores <- parallelly::availableCores(which = "min")

plan(multisession, workers = cores)

feature_distance_table <- furrr::future_map_dfc(group_list, function(x) {

  active_city_group <-
    active_city %>%
    dplyr::filter(var_group == x)

  real_estate_active_city <-
    real_estate_objects %>%
    dplyr::filter(city == active_city_name)

  sf::st_crs(real_estate_active_city) <- TA


  index_feature <- sf::st_nearest_feature(real_estate_active_city, active_city_group)

  active_city_sorted <- active_city_group[index_feature, ]

  feature_distance <-
    sf::st_distance(real_estate_active_city, active_city_sorted, by_element = TRUE) %>%
    as.vector() %>%
    as.data.frame()

  names(feature_distance) <- paste0(x, "_distance")

  return(feature_distance)


})





