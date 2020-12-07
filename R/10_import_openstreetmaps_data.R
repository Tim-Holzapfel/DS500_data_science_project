clear()
options(enhancedView.pageLength = 1000)

point_paths <-
  list.files(
    path = "data/open_street_maps/points",
    pattern = "\\.shp$",
    full.names = TRUE
  )


point_geoms <- purrr::map_dfr(point_paths, function(x) {
  sf::read_sf(
    dsn = x
  ) %>%
    dplyr::rename(public_transport = public_tra) %>%
    dplyr::mutate(
      data_type = "points",
      federal_state = stringr::str_extract(x, federal_states_pattern)
    )
})

polygon_paths <-
  list.files(
    path = "data/open_street_maps/polygons",
    pattern = "\\.shp$",
    full.names = TRUE
  )

polygon_geom <- purrr::map_dfr(polygon_paths, function(x) {
  sf::read_sf(
    dsn = x
  ) %>%
    dplyr::mutate(
      data_type = "polygons",
      federal_state = stringr::str_extract(x, federal_states_pattern)
    )
})

names_add_polygons <- names(point_geoms)[which(names(point_geoms) %notin% names(polygon_geom))]

names_add_points <- names(polygon_geom)[which(names(polygon_geom) %notin% names(point_geoms))]

polygon_geom[, names_add_polygons] <- NA_character_

point_geoms[, names_add_points] <- NA_character_


geom_data <-
  rbind(polygon_geom, point_geoms) %>%
  dplyr::relocate(osm_id, osm_way_id, data_type) %>%
  dplyr::mutate(
    osm_id = dplyr::if_else(
      is.na(osm_id),
      osm_way_id,
      osm_id
    )
  ) %>%
  dplyr::select(-osm_way_id) %>%
  dplyr::relocate(amenity, leisure, tourism, shop)

#   ____________________________________________________________________________
#   Features                                                                ####

leisure <-
  geom_data %>%
  dplyr::filter(
    amenity %in% c("nightclub", "cinema", "swimming_pool", "shisha_lounge") |
      leisure %in% c("park", "playground", "sport_centre", "swimming_pool", "stadium", "ice_rink")
  ) %>%
  dplyr::mutate(
    var_group = dplyr::case_when(
      amenity %in% c("nightclub", "cinema", "swimming_pool", "shisha_lounge") ~ amenity,
      leisure %in% c("park", "playground", "sport_centre", "swimming_pool", "stadium", "ice_rink") ~ leisure,
      TRUE ~ NA_character_
    )
  )

culture <-
  geom_data %>%
  dplyr::filter(
    amenity %in% c("art_gallery", "arts_centre", "theatre")
  ) %>%
  dplyr::mutate(
    var_group = amenity
  )

catering <-
  geom_data %>%
  dplyr::filter(
    amenity %in% c("restaurant", "fast_food", "cafe", "pub", "bar")
  ) %>%
  dplyr::mutate(
    var_group = amenity
  )

accommodation <-
  geom_data %>%
  dplyr::filter(
    tourism %in% c("hotel", "motel", "bed_and_breakfast", "guest_house", "hostel", "camp_site")
  ) %>%
  dplyr::mutate(
    var_group = tourism
  )

shopping <-
  geom_data %>%
  dplyr::filter(
    shop %in% c("supermarket", "bakery", "mall", "department_store", "convenience") |
      amenity == "marketplace"
  ) %>%
  dplyr::mutate(
    var_group = dplyr::case_when(
      amenity == "marketplace" ~ amenity,
      shop %in% c("supermarket", "bakery", "mall", "department_store", "convenience") ~ shop,
      TRUE ~ NA_character_
    )
  )

education <-
  geom_data %>%
  dplyr::filter(
    amenity %in% c("university", "school", "kindergarten", "college")
  ) %>%
  dplyr::mutate(
    var_group = amenity
  )

health <-
  geom_data %>%
  dplyr::filter(
    amenity %in% c("pharmacy", "hospital", "doctors", "nursing_home")
  ) %>%
  dplyr::mutate(
    var_group = amenity
  )

public <-
  geom_data %>%
  dplyr::filter(
    amenity %in% c("police", "library", "prison", "graveyard") |
      landuse == "cemetery"
  ) %>%
  dplyr::mutate(
    var_group = dplyr::case_when(
      amenity %in% c("police", "library", "prison", "graveyard") ~ amenity,
      landuse == "cemetery" ~ landuse,
      TRUE ~ NA_character_
    )
  )

misc <-
  geom_data %>%
  dplyr::filter(
    amenity %in% c("brothel", "adult_gaming_centre", "gambling", "love_hotel", "stripclub")
  ) %>%
  dplyr::mutate(
    var_group = amenity
  )

faith <-
  geom_data %>%
  dplyr::filter(
    amenity %in% c("monastery", "place_of_worship")
  ) %>%
  dplyr::mutate(
    var_group = amenity
  )


#   ____________________________________________________________________________
#   Traffic Features                                                        ####

bus_stop <-
  geom_data %>%
  dplyr::filter(
    highway == "bus_stop" | (public_transport == "stop_position" & bus == "yes") | amenity == "bus_station"
  ) %>%
  dplyr::mutate(
    var_group = "bus_stop"
  )

tram_stop <-
  geom_data %>%
  dplyr::filter(
    railway == "tram_stop" | (public_transport == "stop_position" & tram == "yes")
  ) %>%
  dplyr::mutate(
    var_group = "tram_stop"
  )

train_station <-
  geom_data %>%
  dplyr::filter(
    railway == "station"
  ) %>%
  dplyr::mutate(
    var_group = "train_station"
  )

train_stop <-
  geom_data %>%
  dplyr::filter(
    railway == "halt" | (public_transport == "stop_position" & train == "yes")
  ) %>%
  dplyr::mutate(
    var_group = "train_stop"
  )

airport <-
  geom_data %>%
  dplyr::filter(
    amenity == "airport" | (aeroway == "aerodrome" & type != "yes")
  ) %>%
  dplyr::mutate(
    var_group = "airport"
  )

#   ____________________________________________________________________________
#   End of features                                                         ####

exclusion_list <-
  c(
    "exclusion_list", "geom_data", "lines_geom",
    "geom_coords", "icon_list", "data_list", "data_result", "names_add_points",
    "names_add_polygons", "point_geoms", "polygon_geom", "var_groups_list", "labs",
    "point_paths", "polygon_paths"
  ) %>%
  paste(collapse = "|")

data_list <- stringr::str_subset(ls(), paste0("^((?!", exclusion_list, ").)*$"))

z_data_result <- purrr::map_dfr(data_list, function(x) {
  result <- eval(parse(text = x)) %>%
    dplyr::mutate(
      var_category = dplyr::if_else(
        var_group %notin% c("airport", "train_stop", "train_station", "tram_stop", "bus_stop"),
        paste(x),
        "transportation"
      )
    )
}) %>%
  dplyr::mutate(
    address = paste(
      sep = "<br/>", name,
      paste0(addr_stree, addr_house),
      paste0(addr_city, addr_postc)
    ),
    var_group = factor(var_group),
    var_category = factor(var_category)
  ) %>%
  dplyr::relocate(var_group, var_category, address) %>%
  dplyr::arrange(name, var_category, var_group) %>%
  sf::st_centroid()

saveRDS(z_data_result, "output/feature_data.RDS")
