
clear()
library(leaflet)
library(enhancedView)
library(htmltools)
library(fontawesome)
conflicted::conflict_prefer("View", "enhancedView", quiet = TRUE)
options(enhancedView.pageLength = 1000)

# TODO Control for the presence of a university within a city

mannheim_coords <-
  sf::read_sf(find_files("gadm36_DEU_2.shp")) %>%
  dplyr::filter(NAME_2 == "Mannheim") %>%
  sf::st_transform(TA)

mannheim_lines <- readRDS("output/mannheim_lines.RDS")

mannheim_points <-
  readRDS("output/mannheim_points.RDS") %>%
  dplyr::rename(public_transport = public_tra) %>%
  dplyr::mutate(
    data_type = "points"
  )

mannheim_polygons <-
  readRDS("output/mannheim_polygons2.RDS") %>%
  dplyr::mutate(
    data_type = "polygons"
  )

names_add_polygons <- names(mannheim_points)[which(names(mannheim_points) %notin% names(mannheim_polygons))]

names_add_points <- names(mannheim_polygons)[which(names(mannheim_polygons) %notin% names(mannheim_points))]

mannheim_polygons[, names_add_polygons] <- NA_character_

mannheim_points[, names_add_points] <- NA_character_


mannheim_data <-
  rbind(mannheim_polygons, mannheim_points) %>%
  dplyr::relocate(osm_id, osm_way_id, data_type)

rm(mannheim_polygons, mannheim_points, names_add_polygons, names_add_points)

#   ____________________________________________________________________________
#   Public Features                                                         ####


police <-
  mannheim_data %>%
  dplyr::filter(amenity == "police")

library <-
  mannheim_data %>%
  dplyr::filter(
    amenity == "library"
  )

prison <-
  mannheim_data %>%
  dplyr::filter(
    amenity == "prison"
  )

nursing_home <-
  mannheim_data %>%
  dplyr::filter(
    amenity == "nursing_home"
  )

cemetery <-
  mannheim_data %>%
  dplyr::filter(landuse == "cemetery" | amenity == "graveyard")

market_place <-
  mannheim_data %>%
  dplyr::filter(
    amenity == "market_place"
  )

university <-
  mannheim_data %>%
  dplyr::filter(
    amenity == "university"
  )

school <-
  mannheim_data %>%
  dplyr::filter(
    amenity == "school"
  )

college <-
  mannheim_data %>%
  dplyr::filter(
    amenity == "college"
  )

kindergarten <-
  mannheim_data %>%
  dplyr::filter(
    amenity == "kindergarten"
  )

brothel <-
  mannheim_data %>%
  dplyr::filter(amenity == "brothel")

#   ____________________________________________________________________________
#   Health Features                                                         ####

hospital <-
  mannheim_data %>%
  dplyr::filter(amenity == "hospital")

pharmacy <-
  mannheim_data %>%
  dplyr::filter(amenity == "pharmacy")

doctor <-
  mannheim_data %>%
  dplyr::filter(amenity == "doctors")


#   ____________________________________________________________________________
#   Leisure Feature                                                         ####

park <-
  mannheim_data %>%
  dplyr::filter(leisure == "park")

playground <-
  mannheim_data %>%
  dplyr::filter(leisure == "park")


#   ____________________________________________________________________________
#   Accomodation Feature                                                    ####

hotel <-
  mannheim_data %>%
  dplyr::filter(tourism == "hotel")

motel <-
  mannheim_data %>%
  dplyr::filter(tourism == "motel")

hostel <-
  mannheim_data %>%
  dplyr::filter(tourism == "hostel")


#   ____________________________________________________________________________
#   Shopping                                                                ####

supermarket <-
  mannheim_data %>%
  dplyr::filter(
    shop == "supermarket"
  )

bakery <-
  mannheim_data %>%
  dplyr::filter(
    shop == "bakery"
  )

mall <-
  mannheim_data %>%
  dplyr::filter(
    shop == "mall"
  )

department_store <-
  mannheim_data %>%
  dplyr::filter(
    shop == "department_store"
  )

#   ____________________________________________________________________________
#   Traffic Features                                                        ####

bus_stop <-
  mannheim_data %>%
  dplyr::filter(
    highway == "bus_stop" | (public_transport == "stop_position" & bus == "yes") | amenity == "bus_station"
  )

tram_stop <-
  mannheim_data %>%
  dplyr::filter(
    railway == "tram_stop" | (public_transport == "stop_position" & tram == "yes")
  )

train_station <-
  mannheim_data %>%
  dplyr::filter(
    railway == "station"
  )

train_stop <-
  mannheim_data %>%
  dplyr::filter(
    railway == "halt" | (public_transport == "stop_position" & train == "yes")
  )

airport <-
  mannheim_data %>%
  dplyr::filter(
    amenity == "airport" | (aeroway == "aerodrome" & type != "yes")
  )

#   ____________________________________________________________________________
#   End of features                                                         ####

exclusion_list <-
  c("exclusion_list", "mannheim_data", "mannheim_lines",
    "mannheim_coords", "icon_list", "data_list", "data_result") %>%
  paste(collapse = "|")

data_list <- stringr::str_subset(ls(), paste0("^((?!",exclusion_list  ,").)*$"))

z_data_result <- purrr::map_dfr(data_list, function(x) {
  result <- eval(parse(text = x)) %>%
    dplyr::mutate(variable_type = paste(x))
  }) %>%
  dplyr::mutate(
    osm_id = dplyr::if_else(
      is.na(osm_id),
      osm_way_id,
      osm_id
    ),
    address = paste(sep = '<br/>', addr_stree, addr_house)
  ) %>%
  dplyr::mutate(
    variable_type = factor(variable_type),
    variable_group = dplyr::case_when(
      variable_type %in% c("university", "school", "college", "kindergarten") ~ "Education",
      variable_type %in% c("police", "library", "prison", "cemetery") ~ "Public",
      variable_type %in%
        c("train_stop", "tram_stop", "train_station", "bus_stop", "airport") ~ "Public Transportation",
      variable_type %in% c("hotel", "motel", "hostel") ~ "Accommodation",
      variable_type %in% c("bakery", "supermarket", "department_store", "mall") ~ "Shopping",
      variable_type %in% c("brothel", "nursing_home") ~ "Miscellaneous",
      variable_type %in% c("doctor", "hospital", "pharmacy") ~ "Health",
      variable_type %in% c("playground", "park", "pharmacy") ~ "Leisure",
      TRUE ~ NA_character_
    )
  ) %>%
  dplyr::arrange(name, variable_type, osm_id) %>%
  dplyr::relocate(osm_id, data_type, variable_type) %>%
  dplyr::select(-osm_way_id) %>%
  sf::st_centroid()


labs <- as.list(z_data_result$address)







t3 <- aliases %>% as.data.frame()

icon_list <- awesomeIconList(
  airport = makeAwesomeIcon(text = fa("plane-departure"), markerColor = "green"),
  bakery = makeAwesomeIcon(text = fa("bread-slice"), markerColor = "lightblue"),
  brothel = makeAwesomeIcon(text = fa("cocktail"), markerColor = "black"),
  bus_stop = makeAwesomeIcon(text = fa("bus"), markerColor = "black"),
  cemetery = makeAwesomeIcon(text = fa("skull"), markerColor = "purple"),
  college = makeAwesomeIcon(text = fa("graduation-cap"), markerColor = "darkpurple"),
  department_store = makeAwesomeIcon(text = fa("cash-register"), markerColor = "darkpurple"),
  doctor = makeAwesomeIcon(text = fa("user-md"), markerColor = "red"),
  graveyard = makeAwesomeIcon(text = fa("skull"), markerColor = "beige"),
  hospital = makeAwesomeIcon(text = fa("hospital"), markerColor = "cadetblue"),
  hostel = makeAwesomeIcon(text = fa("bed"), markerColor = "darkpurple"),
  hotel = makeAwesomeIcon(text = fa("hotel", fill = "white"), squareMarker = TRUE),
  kindergarten = makeAwesomeIcon(text = fa("child"), markerColor = "orange"),
  library = makeAwesomeIcon(text = fa("book-reader"), markerColor = "blue"),
  mall = makeAwesomeIcon(text = fa("store"), markerColor = "darkpurple"),
  market_place = makeAwesomeIcon(text = fa("store"), markerColor = "darkblue"),
  motel = makeAwesomeIcon(text = fa("hotel"), markerColor = "darkpurple"),
  nursing_home = makeAwesomeIcon(text = fa("user-nurse"), markerColor = "darkpurple"),
  park = makeAwesomeIcon(text = fa("leaf"), markerColor = "darkpurple"),
  pharmacy = makeAwesomeIcon(text = fa("pills"), markerColor = "white"),
  playground = makeAwesomeIcon(text = fa("child", fill = "white"), squareMarker = TRUE),
  police = makeAwesomeIcon(text = fa("user-shield"), markerColor = "lightred"),
  prison = makeAwesomeIcon(text = fa("lock"), markerColor = "darkpurple"),
  school = makeAwesomeIcon(text = fa("school"), markerColor = "lightgray"),
  supermarket = makeAwesomeIcon(text = fa("store-alt"), markerColor = "darkpurple"),
  train_station = makeAwesomeIcon(text = fa("train", fill = "white"), squareMarker = TRUE),
  train_stop = makeAwesomeIcon(text = fa("train"), markerColor = "darkgreen"),
  tram_stop = makeAwesomeIcon(text = fa("tram"), markerColor = "darkpurple"),
  university = makeAwesomeIcon(text = fa("graduation-cap"), markerColor = "gray")
)

primary_roads <-
  mannheim_lines %>%
  dplyr::filter(highway == "primary") %>%
  dplyr::group_by(name, highway) %>%
  dplyr::summarise(.groups = "drop")

secondary_roads <-
  mannheim_lines %>%
  dplyr::filter(highway == "secondary") %>%
  dplyr::group_by(name, highway) %>%
  dplyr::summarise(.groups = "drop")

tertiary_roads <-
  mannheim_lines %>%
  dplyr::filter(highway == "tertiary") %>%
  dplyr::group_by(name, highway) %>%
  dplyr::summarise(.groups = "drop")


library(shiny)
library(leaflet)

labs <- as.list(z_data_result$address)

ui <- shinyUI(
  fluidPage(
    fluidRow(
      column(
        width = 12,
        leafletOutput(outputId = "map", height = 800)
      )
    )
  )
)

server <- function(input, output) {

  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addPolygons(
        data = mannheim_coords,
        fill = FALSE
      ) %>%
      addAwesomeMarkers(
        data = z_data_result,
        icon = ~icon_list[variable_type],
        group = ~variable_group,
        popup = lapply(labs, HTML),
        label = lapply(labs, HTML),
        options = markerOptions(riseOnHover = TRUE, title = "test2", alt = "test4")
      ) %>%
      addPolylines(data = primary_roads, color = "red") %>%
      addPolylines(data = secondary_roads, color = "yellow") %>%
      addPolylines(data = tertiary_roads, color = "orange") %>%
      addMarkers(lat = 49.4786346023694, lng = 8.49027924150479) %>%
      addLayersControl(
        data = z_data_result,
        overlayGroups = ~variable_group,
        options = layersControlOptions(
          collapsed = FALSE
        )
      )
  })
}

shinyApp(ui, server)



































# icon_colors <-
#   c(
#     , "darkred", , , , , ,
#     "lightgreen", , , , , ,
#     "pink", , , , ,
#   )


















unique(mannheim_polygons$amenity) %>% sort()



unique(mannheim_polygons$landuse) %>% sort()


police_station <-
  mannheim_polygons %>%
  dplyr::filter(
    amenity == "police"
  ) %>%
  dplyr::filter(!is.na(addr_stree)) %>%
  dplyr::mutate(
    address = paste(sep = '<br/>', addr_stree, addr_house),
    type = factor(x = "police_station")
  ) %>%
  sf::st_centroid()





admin_boundaries <-
  mannheim_polygons %>%
  dplyr::filter(boundary == "administrative") %>%
  dplyr::mutate(
    admin_leve = as.integer(admin_leve)
  ) %>%
  dplyr::arrange(admin_leve)






























marker_group <-
  rbind(police_station)
















# germany_map_tiles <- OpenStreetMap::openmap(
#   upperLeft = c(54.983104, 5.988658),
#   lowerRight = c(47.302488, 15.016996),
#   type = "esri"
# )



world_coords <- rnaturalearth::ne_countries(returnclass = "sf")

germany_coords <- rnaturalearth::ne_countries(returnclass = "sf", country = "Germany")


ggplot() + geom_sf(data = germamy_diff)


germamy_diff <- sf::st_difference(world_coords, germany_coords)

bounds <- matrix(c(54.983104, 5.988658, 47.302488, 15.016996), nrow = 2, byrow = T)


leaflet(
  options = leafletOptions(preferCanvas = FALSE)
) %>%
  addTiles(
    options = tileOptions(
      unloadInvisibleTiles = FALSE,
      bounds = bounds,
      maxZoom = 18)
    ) %>%
  rmapshaper::ms_clip(clip = germany_coords)






  setView(lat = 51.13372, lng = 10.28849, zoom = 6) %>%
  addPolygons(data = germamy_diff, fillColor = "white", fillOpacity = 1)




  addPolygons(data = mannheim_coords)



sf::st_centroid(germany_coords) %>% sf::st_coordinates()





   %>%
  setView(49.4786346023694, lng = 8.49027924150479, zoom = 11)


install.packages("geojsonlint")


library(rmapshaper)












t1 <-
  rbind(bus_stop, tram_stop, train_station, train_stop) %>%
  dplyr::mutate(
    type = factor(type)
  )




leaflet() %>%
  addAwesomeMarkers(
    data = t1,
    icon = ~icon_list[type]
  )

















z_tram_stop_point <-
  mannheim_points %>%
  dplyr::filter(
    railway == "tram_stop" | (public_transport == "stop_position" & tram == "yes")
  ) %>% as.data.frame()






t5 <- dplyr::inner_join(z_tram_stop_point, z_tram_stop_polygon, by = "osm_id")

unique(mannheim_polygons$landuse) %>% sort()








# school <-
#   mannheim_points %>%
#   dplyr::filter(
#     amenity == "school"
#   ) %>%
#   dplyr::relocate(osm_id) %>%
#   dplyr::arrange(osm_id)







replace_na()














# mannheim_polygons <-
#   readRDS("output/mannheim_polygons.RDS") %>%
#   dplyr::select(-c(
#     "osm_id", "osm_way_id", "geological", "craft",
#     "historic", "land_area", "man_made", "military", "barrier",
#     "natural", "sport", "tourism"
#   ))










start_data <-
  mannheim_lines %>%
  dplyr::filter(
    stringr::str_detect(other_tags, "start_date")
  )







