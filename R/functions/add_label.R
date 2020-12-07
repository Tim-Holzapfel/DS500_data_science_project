add_label <- function(map, data, group) {
  leaflet::addLabelOnlyMarkers(
    map = map,
    data = data,
    group = group,
    label = ~name,
    labelOptions = leaflet::labelOptions(
      noHide = TRUE,
      direction = "center",
      textOnly = TRUE
    )
  )
}
