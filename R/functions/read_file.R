read_file <- function(input_file, convert_to_sf = FALSE, lng = "lng", lat = "lat") {
  input_file <- if (stringr::str_detect(input_file, "\\.feather$", negate = TRUE)) {
    input_file <- paste0(input_file, ".feather")
  }

  if (convert_to_sf == FALSE) {
    output_file <- feather::read_feather(file.path(output_dir, input_file))
  } else {
    output_file <-
      feather::read_feather(file.path(output_dir, input_file)) %>%
      sf::st_as_sf(agr = "constant", coords = c(lng, lat))
    sf::st_crs(output_file) <- 4326
  }
  return(output_file)
}
