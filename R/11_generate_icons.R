clear()

#feature_data <- read_file("feature_data", convert_to_sf = TRUE)
feature_data <- readRDS(file.path(output_dir, "feature_data_city.RDS"))
plz_data <- readRDS(file.path(output_dir, "zip_codes_data.RDS"))




# map_bbox <- c(
#   xmin = input$map_bounds$west,
#   ymin = input$map_bounds$south,
#   xmax = input$map_bounds$east,
#   ymax = input$map_bounds$north
# )


map_bbox <- c(
  xmin = 11.01242,
  ymin = 49.38505,
  xmax = 11.95862,
  ymax = 50.09504
)
class(map_bbox) <- "bbox"

map_view_coords <-
  sf::st_crop(plz_data, map_bbox) %>%
  magrittr::extract2("id")








t1 <-
  map_bbox %>%
  as.data.frame() %>%
  sfheaders::sf_polygon()






sf::st_within(plz_data, map_bbox)



observeEvent(
  input$map_center, {
    map_center_sf <- center_coords()
    index_map <- suppressMessages({
      sf::st_nearest_feature(map_center_sf, plz_data) %>% unlist()
    })
    index_id <- plz_data$id[index_map]
    active_city$city_index <- index_id
  }, ignoreInit = TRUE
)

































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
  dplyr::group_by(plz) %>%
  dplyr::summarise(.groups = "keep") %>%
  dplyr::mutate(
    id = dplyr::cur_group_id()
  ) %>%
  dplyr::ungroup()

sf::st_crs(plz_data) <- TA


index_city <- sf::st_nearest_feature(feature_data, plz_data)

feature_data$id <- plz_data$id[index_city]

feature_data <-
  feature_data %>%
  dplyr::relocate(id)

saveRDS(feature_data, "output/feature_data_city.RDS")

#   ____________________________________________________________________________
#   Assigning ID to real estate objects                                     ####

real_estate_objects <-
  readRDS(file.path(output_dir, "real_estate_objects.RDS"))

sf::st_crs(real_estate_objects) <- TA

index_real_estate <- sf::st_nearest_feature(real_estate_objects, plz_data)

real_estate_objects$id <- plz_data$id[index_real_estate]

saveRDS(real_estate_objects, "output/real_estate_objects.RDS")

saveRDS(plz_data, "output/zip_codes_data.RDS")























unique(plz_data$plz)



plz_data <- readRDS("output/zip_codes_data.RDS")


plz_data2 <-
  plz_data %>%
  dplyr::group_by(plz) %>%
  dplyr::summarise(.groups = "drop")





plz_data <- readRDS("output/zip_codes_data.RDS")
feature_data <-
  readRDS(file.path(output_dir, "feature_data_city.RDS"))


map_center <-
  tibble::tibble(
    id_empty = "",
    x = 8.75061,
    y = 51.76614
  ) %>%
  sfheaders::sf_point(x = "x", y = "y")
sf::st_crs(map_center) <- TA


index_map <- sf::st_nearest_feature(map_center, plz_data) %>% unlist()
index_id <- plz_data$id[index_map]


plz_data$city[index_map]




feature_data_sub <-
  feature_data %>%
  dplyr::filter(id == index_id)







feature_data2 <-
  feature_data %>%
  data.table::as.data.table()

t6 <-
  feature_data2 %>%
  dplyr::slice(1:100)


leaflet() %>% addPolygons(data = t6)

stadt_coords <-
  sf::read_sf(
    find_files("gadm36_DEU_3.shp")
  )

index_stadt <- sf::st_nearest_feature(stadt_coords, plz_data)

stadt_coords$plz <- plz_data$plz[index_city2]
stadt_coords$city_plz <- plz_data$city[index_city2]

saveRDS(stadt_coords, "output/city_polygons.rDS")


























color_table <-
  c(
    "red" = "#FF0000",
    "darkred" = "#8B0000",
    "lightred" = "#ffcccb",
    "orange" = "#FFA500",
    "beige" = "#F5F5DC",
    "green" = "#008000",
    "darkgreen" = "#006400",
    "lightgreen" = "#90EE90",
    "blue" = "#0000FF",
    "darkblue" = "#00008B",
    "lightblue" = "#ADD8E6",
    "purple" = "#800080",
    "darkpurple" = "#301934",
    "pink" = "#FFC0CB",
    "cadetblue" = "#5F9EA0",
    "white" = "#FFFFFF",
    "gray" = "#808080",
    "lightgray" = "#D3D3D3",
    "black" = "#000000"
  )

var_group_table <-
  feature_data %>%
  as.data.frame() %>%
  dplyr::group_by(var_category) %>%
  dplyr::mutate(
    group_color = names(color_table)[cur_group_id()],
    group_color_rgb = unname(color_table)[cur_group_id()]
  ) %>%
  dplyr::group_by(var_category, group_color, group_color_rgb, var_group) %>%
  dplyr::summarise(.groups = "drop") %>%
  dplyr::arrange(var_category, var_group) %>%
  dplyr::mutate(
    icon_sym = dplyr::case_when(
      var_group == "adult_gaming_centre" ~ "dice",
      var_group == "airport" ~ "plane-departure",
      var_group == "art_gallery" ~ "image",
      var_group == "arts_centre" ~ "images",
      var_group == "bakery" ~ "bread-slice",
      var_group == "bar" ~ "beer",
      var_group == "brothel" ~ "cocktail",
      var_group == "bus_stop" ~ "bus",
      var_group == "cafe" ~ "coffee",
      var_group == "camp_site" ~ "campground",
      var_group == "cemetery" ~ "skull",
      var_group == "cinema" ~ "film",
      var_group == "college" ~ "graduation-cap",
      var_group == "convenience" ~ "cash-register",
      var_group == "department_store" ~ "cash-register",
      var_group == "doctors" ~ "user-md",
      var_group == "fast_food" ~ "hamburger",
      var_group == "gambling" ~ "dice",
      var_group == "guest_house" ~ "door-open",
      var_group == "hospital" ~ "hospital",
      var_group == "hostel" ~ "bed",
      var_group == "hotel" ~ "hotel",
      var_group == "ice_rink" ~ "skating",
      var_group == "kindergarten" ~ "child",
      var_group == "library" ~ "book-reader",
      var_group == "love_hotel" ~ "burn",
      var_group == "mall" ~ "store",
      var_group == "marketplace" ~ "store-alt",
      var_group == "monastery" ~ "cross",
      var_group == "motel" ~ "hotel",
      var_group == "nightclub" ~ "glass-cheers",
      var_group == "nursing_home" ~ "user-nurse",
      var_group == "park" ~ "leaf",
      var_group == "pharmacy" ~ "pills",
      var_group == "place_of_worship" ~ "place-of-worship",
      var_group == "playground" ~ "child",
      var_group == "police" ~ "user-shield",
      var_group == "prison" ~ "lock",
      var_group == "pub" ~ "glass-martini",
      var_group == "restaurant" ~ "utensils",
      var_group == "school" ~ "school",
      var_group == "shisha_lounge" ~ "smoking",
      var_group == "stadium" ~ "basketball-ball",
      var_group == "stripclub" ~ "cocktail",
      var_group == "supermarket" ~ "store-alt",
      var_group == "swimming_pool" ~ "swimmer",
      var_group == "theatre" ~ "theater-masks",
      var_group == "train_station" ~ "train",
      var_group == "train_stop" ~ "train",
      var_group == "tram_stop" ~ "tram",
      var_group == "university" ~ "graduation-cap",
      TRUE ~ NA_character_
    )
  ) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    legend_html = list(fontawesome::fa(icon_sym, fill = group_color_rgb)),
    legend_html_code = paste0(legend_html, " = ", snakecase::to_title_case(paste0(var_group))),
    legend_html_code = paste0(
      "<span class='ctr'><span class='lft'>",
      legend_html,
      "</span>=<span class='lft'>",
      snakecase::to_title_case(paste0(var_group)),
      "</span></span>"
    )
  ) %>%
  dplyr::ungroup()

icon_list <- purrr::map2(var_group_table$icon_sym, var_group_table$group_color, function(x, y) {
  make_awesome_icon(x, markerColor = y)
})

names(icon_list) <- var_group_table$var_group

class(icon_list) <- "leaflet_awesome_icon_set"

saveRDS(icon_list, "output/icon_list.RDS")
saveRDS(var_group_table, "output/var_group_table.RDS")
