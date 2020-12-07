save_file <- function(input_file, file_name) {
  file_name <- if (stringr::str_detect(file_name, "\\.feather$", negate = TRUE)) {
    file_name <- paste0(file_name, ".feather")
  }

  if (any(class(input_file) %in% "sf")) {
    input_file_coords <-
      input_file %>%
      sf::st_coordinates() %>%
      unname()

    input_file_df <-
      input_file %>%
      as.data.frame() %>%
      dplyr::select(-geometry) %>%
      dplyr::mutate(
        lng = feature_data_coords[, 1],
        lat = feature_data_coords[, 2]
      )

    feather::write_feather(input_file_df, file.path(output_dir, file_name))
  } else {
    feather::write_feather(input_file, file.path(output_dir, file_name))
  }
}
