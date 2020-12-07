
clear()

plan(multisession, workers = cores)

boundary_shapefiles <- list.files(
  path = file.path(data_dir, "open_street_maps/boundaries"),
  pattern = "\\.shp$",
  full.names = TRUE
) %>%
  tibble::as_tibble_col("boundaries") %>%
  dplyr::mutate(
    federal_state = stringr::str_extract(boundaries, federal_states_pattern)
  )

line_strings_shapefiles <- list.files(
  path = file.path(data_dir, "open_street_maps/lines"),
  pattern = "\\.shp$",
  full.names = TRUE
) %>%
  tibble::as_tibble_col("line_strings") %>%
  dplyr::mutate(
    federal_state = stringr::str_extract(line_strings, federal_states_pattern)
  )

file_list <-
  federal_states_mod %>%
  tibble::as_tibble_col("federal_state") %>%
  dplyr::left_join(boundary_shapefiles, by = "federal_state") %>%
  dplyr::left_join(line_strings_shapefiles, by = "federal_state")

i <- 7

federal_states_list <-
  federal_states_mod %>%
  sort()

#   ____________________________________________________________________________
#   Loop Start                                                              ####


# state <- federal_states_list[i]
state <- "berlin"
for (state in federal_states_list) {
  print(state)
  file_list_sub <-
    file_list %>%
    dplyr::filter(federal_state == state)

  boundary_path <- file_list_sub$boundaries

  lines_path <- file_list_sub$line_strings

  # name of the layer that is to be imported
  boundary_layer_name <- sf::st_layers(boundary_path)[["name"]]
  lines_layer_name <- sf::st_layers(lines_path)[["name"]]

  # Polygon describing the postal codes of the cities
  boundary_poly <-
    sf::read_sf(
      dsn = boundary_path,
      query = sprintf(
        "SELECT postal_cod AS postal_code FROM %s ORDER BY postal_cod",
        boundary_layer_name
      )
    ) %>%
    sf::st_sf(agr = "constant")

  # Setting CRS definition for polygon
  sf::st_crs(boundary_poly) <- TA

  # Changing the class from sf to data.frame makes it possible to remove the
  # geometry column which is otherwise "sticky" as the package author refers to
  # it.
  boundary_poly_df <-
    boundary_poly %>%
    as.data.frame()

  # Street coordinates
  line_strings <-
    sf::read_sf(
      dsn = lines_path
    ) %>%
    sf::st_sf(agr = "constant")

  sf::st_crs(line_strings) <- TA

  # Index of streets which are confined to the postal code polygon, thus not
  # crossing postal code lines; have only one postal code.
  index_within <- sf::st_within(line_strings, boundary_poly) %>% sf_list_transform()

  # Index of streets crossing postal codes and which have multiple postal codes.
  index_crosses <- sf::st_crosses(line_strings, boundary_poly) %>% sf_index_crosses_transform()

  # Names of variables for zip codes which are going to be assigned to streets
  # which cross multiple districts.
  var_names_zip <- paste0("postal_code_", 1:ncol(index_crosses))

  # Postal codes of street crossing multiple districts
  postal_codes_crosses <-
    boundary_poly_df[index_crosses, "postal_code"] %>%
    matrix(ncol = ncol(index_crosses)) %>%
    tibble::as_tibble(
      .name_repair = function(x) var_names_zip
    ) %>%
    replace_empty()

  # Postal codes of streets contained in a single district
  postal_codes_within <- boundary_poly_df$postal_code[index_within]

  postal_codes_complete <-
    cbind("postal_code_0" = postal_codes_within, postal_codes_crosses) %>%
    tidyr::unite("postal_code", c("postal_code_0", "postal_code_1"), na.rm = TRUE)

  #   ____________________________________________________________________________
  #   Second Step                                                             ####

  line_strings_bind <-
    cbind(line_strings, postal_codes_complete) %>%
    dplyr::relocate(postal_code, postal_code_2) %>%
    dplyr::rename(postal_code_1 = postal_code) %>%
    dplyr::filter(
      postal_code_1 != ""
    )

  var <-
    c("name", var_names_zip) %>%
    syms()

  # Subset of streets that have two or more postal codes, meaning streets which
  # cross postal code areas and therefore need to be split.
  line_strings_split <-
    line_strings_bind %>%
    dplyr::filter(
      !is.na(postal_code_2)
    ) %>%
    dplyr::group_by(!!!var) %>%
    dplyr::summarise(.groups = "drop")

  geom_final <- NULL

  # Subset of line_string_split that only contains postal_code variables
  index_postal <-
    line_strings_split %>%
    as.data.frame() %>%
    dplyr::select(dplyr::matches("postal\\_code\\_\\d+"))

  i <- 1

  total_rows <- nrow(line_strings_split)
  print(total_rows)
  #   ____________________________________________________________________________
  #   Second Loop start                                                       ####

  row_seq <- seq_len(nrow(line_strings_split))

  geom_final <-
    furrr::future_map_dfr(
      row_seq,
      function(i) {
        index_postal_loop <-
          index_postal %>%
          dplyr::slice(i) %>%
          unlist() %>%
          stringi::stri_remove_empty_na()

        boundary_poly_sub <-
          boundary_poly %>% dplyr::filter(postal_code %in% index_postal_loop)

        lines_string_sub <- line_strings_split[i, ]

        tryCatch(
          expr = {
            # Splitting the street into two or more parts, depending on who many
            # postal code regions the street passes through
            geom_loop <- lwgeom::st_split(lines_string_sub, boundary_poly_sub) %>%
              sf::st_collection_extract("LINESTRING")

            # To decide to which postal code the "splitted" streets are to be allocated,
            # the easiest way is to calculate the centroids of the splitted streets and
            # use a predicate function to see in which boundary polygon the centroids (points)
            # lie.
            geom_loop_centroids <-
              geom_loop %>%
              sf::st_centroid()

            index_centroids <-
              suppressMessages({
                sf::st_within(geom_loop_centroids, boundary_poly_sub) %>%
                  unlist()
              })


            geom_loop$zip_code <- boundary_poly_sub$postal_code[index_centroids]

            return(geom_loop)
          },
          error = function(e) {
            geom_empty <- st_sf(
              name = NA_character_,
              geometry = st_sfc(lapply(1, function(x) st_linestring()))
            )

            var_names <-
              names(lines_string_sub) %>%
              stringr::str_subset("geometry", negate = TRUE) %>%
              c("zip_code")

            geom_empty[, var_names] <- NA_character_

            return(geom_empty)
          }
        )
      }
    )

  geom_final_mod <-
    geom_final %>%
    dplyr::select(name, zip_code) %>%
    sf::st_sf(agr = "constant")

  geom_final_mod <-
    geom_final %>%
    dplyr::select(name, zip_code) %>%
    sf::st_sf(agr = "constant") %>%
    dplyr::group_by(name, zip_code) %>%
    dplyr::arrange(name, zip_code) %>%
    dplyr::summarise(.groups = "drop")

  line_strings_final <-
    line_strings_bind %>%
    dplyr::filter(is.na(postal_code_2)) %>%
    dplyr::mutate(
      zip_code = postal_code_1
    ) %>%
    dplyr::select(name, zip_code) %>%
    dplyr::arrange(name, zip_code) %>%
    as.data.frame() %>%
    dplyr::distinct(name, zip_code, .keep_all = TRUE) %>%
    sf::st_sf() %>%
    rbind(geom_final_mod)

  output_file_name <-
    paste0(state, "_streets.RDS") %>%
    file.path(output_dir, .)

  saveRDS(line_strings_final, output_file_name)
}
