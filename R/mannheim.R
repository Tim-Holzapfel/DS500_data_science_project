
clear()
library(leaflet)
library(enhancedView)
library(htmltools)
conflicted::conflict_prefer("View", "enhancedView", quiet = TRUE)

mannheim_coords <-
  sf::read_sf(find_files("gadm36_DEU_2.shp")) %>%
  dplyr::filter(NAME_2 == "Mannheim") %>%
  sf::st_transform(TA)

#t1 <- sf::st_bbox(mannheim_coords)
# t3 <- c(t2$west, t2$south, t2$east, t2$north)
# names(t3) <- c("xmin", "ymin", "xmax", "ymax")




#   ____________________________________________________________________________
#   Point features                                                          ####

baden_wuerttemberg_points <-
  sf::read_sf(
    dsn = find_files("baden-wuerttemberg_points.shp")
  ) %>%
  sf::st_transform(TA)

mannheim_index <-
  sf::st_within(baden_wuerttemberg_points, mannheim_coords) %>%
  as.logical() %>%
  which()

mannheim_points <- baden_wuerttemberg_points[mannheim_index, ]

saveRDS(mannheim_points, "output/mannheim_points.RDS")

rm(baden_wuerttemberg_points)

gc()

#   ____________________________________________________________________________
#   Line Features                                                           ####

baden_wuerttemberg_lines <-
  sf::read_sf(
    dsn = find_files("baden-wuerttemberg_lines.shp")
  ) %>%
  sf::st_transform(TA)

mannheim_index_lines_within <-
  sf::st_within(baden_wuerttemberg_lines, mannheim_coords) %>%
  as.logical() %>%
  which()

mannheim_index_lines_crosses <-
  sf::st_crosses(baden_wuerttemberg_lines, mannheim_coords) %>%
  as.logical() %>%
  which()

mannheim_index_lines <-
  c(mannheim_index_lines_within, mannheim_index_lines_crosses) %>%
  sort() %>%
  unique()

mannheim_lines <-
  baden_wuerttemberg_lines[mannheim_index_lines, ] %>%
  dplyr::arrange(highway)

saveRDS(mannheim_lines, "output/mannheim_lines.RDS")

rm(baden_wuerttemberg_lines)

gc()

#   ____________________________________________________________________________
#   Polygon features                                                        ####

mannheim_polygons <-
  sf::read_sf(
    dsn = find_files("mannheim_polygons.shp"),
    query = "
    SELECT *
    FROM \"mannheim_polygons\"
    "
  ) %>%
  sf::st_transform(TA) %>%
  dplyr::relocate(name, amenity, landuse, building, leisure)


saveRDS(mannheim_polygons, "output/mannheim_polygons.RDS")

mannheim_polygons2 <-
  sf::read_sf(
    dsn = find_files("mannheim_polygons2.shp")
  ) %>%
  sf::st_transform(TA) %>%
  dplyr::relocate(name, amenity, landuse, building, leisure)

saveRDS(mannheim_polygons2, "output/mannheim_polygons2.RDS")








mannheim_polygons_no_geom <-
  mannheim_polygons %>%
  as.data.frame() %>%
  dplyr::select(-"_ogr_geometry_") %>%
  dplyr::select(-c(
    "osm_id", "osm_way_id", "geological", "craft",
    "historic", "land_area", "man_made", "military", "barrier",
    "type", "natural", "sport", "tourism"
  )) %>%
  dplyr::relocate(name, amenity, landuse, building, leisure, office, place, shop)


c(
  "name", "amenity", "landuse", "building", "leisure", "office",
  "place", "shop", "aeroway", "admin_leve", "boundary", "other_tags"
)


t1 <- names(mannheim_polygons_no_geom)

dput(t1)




mannheim_index_polygons <-
  sf::st_within(mannheim_polygons, mannheim_coords_sub) %>%
  as.logical() %>%
  which()




mannheim_postal_codes <-
  mannheim_polygons %>%
  dplyr::filter(boundary == "postal_code")

mannheim_postal_codes_labels <-
  mannheim_postal_codes %>%
  sf::st_centroid()


leaflet() %>%
  addPolygons(data = mannheim_postal_codes) %>%
  addPopups(data = mannheim_postal_codes_labels, popup = mannheim_postal_codes_labels$other_tags)



baden_wuerttemberg_polygons_no_geom <-
  baden_wuerttemberg_polygons %>%
  as.data.frame() %>%
  dplyr::select(-c("_ogr_geometry_", "osm_id", "osm_way_id", "type")) %>%
  dplyr::relocate(name, amenity, landuse, building, leisure)







sf::st_read()
baden_wuerttemberg_polygons <-
  sf::read_sf(
    dsn = find_files("baden-wuerttemberg_polygons.shp")
  ) %>%
  sf::st_transform(TA)




mannheim_index_lines_within <-
  sf::st_within(baden_wuerttemberg_lines, mannheim_coords) %>%
  as.logical() %>%
  which()

mannheim_index_lines_crosses <-
  sf::st_crosses(baden_wuerttemberg_lines, mannheim_coords) %>%
  as.logical() %>%
  which()

mannheim_index_lines <-
  c(mannheim_index_lines_within, mannheim_index_lines_crosses) %>%
  sort() %>%
  unique()

mannheim_lines <-
  baden_wuerttemberg_lines[mannheim_index_lines, ] %>%
  dplyr::arrange(highway)

rm(baden_wuerttemberg_lines)

gc()


#   ____________________________________________________________________________
#   End features                                                            ####




leaflet() %>%
  addPolygons(data = mannheim_coords_sub, color = "red") %>%
  addPolygons(data = mannheim_coords)




























#   ____________________________________________________________________________
#   End features                                                            ####


mannheim_points_details <-
  readRDS("output/mannheim_points.RDS") %>%
  dplyr::relocate(
    "name",
    "shop",
    "short_name",
    "bus",
    "amenity",
    "public_transport",
    "tram",
    "train",
    "network",
    "railway",
    "parking",
    "capacity",
    "fee",
    "layer",
    "operator",
    "leisure",
    "tourism"
  )

mannheim_bus <-
  mannheim_points %>%
  dplyr::filter(highway == "bus_stop")

mannheim_roads <-
  mannheim_lines %>%
  dplyr::filter(highway %in% c("primary", "secondary", "tertiary")) %>%
  dplyr::group_by(name, highway) %>%
  dplyr::summarise(.groups = "drop")

mannheimn_primary_roads <-
  mannheim_roads %>%
  dplyr::filter(highway == "primary")

mannheim_secondary_roads <-
  mannheim_roads %>%
  dplyr::filter(highway == "secondary")

mannheim_tertiary_roads <-
  mannheim_roads %>%
  dplyr::filter(highway == "tertiary")


mannheim_primary_roads_labels <- sf::st_centroid(mannheimn_primary_roads)




leaflet() %>%
  addPolygons(data = mannheim_coords) %>%
  addCircles(data = mannheim_bus) %>%
  addPolylines(data = mannheimn_primary_roads, color = "green") %>%
  addPolylines(data = mannheim_secondary_roads) %>%
  addPolylines(data = mannheim_tertiary_roads) %>%
  addMarkers(lat = 49.4786346023694, lng = 8.49027924150479)





real_estates <- readRDS("output/real_estates_details_dataset.RDS")



sum_mannheim <-
  mannheim_lines %>%
  as.data.frame() %>%
  dplyr::select(-geometry) %>%
  dplyr::group_by(highway) %>%
  dplyr::summarise(n1 = dplyr::n())
