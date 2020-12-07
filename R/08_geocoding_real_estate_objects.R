
clear()

options(enhancedView.pageLength = 1000, enhancedView.standard_view = FALSE)

real_estates <-
  readRDS("output/real_estate_offers.RDS") %>%
  dplyr::mutate(
    # Replace German specific characters with their ASCII equivalent
    street = stringi::stri_trans_general(street, "de-ASCII"),
    # All lower case for better handling of cases
    street = stringi::stri_trans_tolower(street),
    # The 'Am' of street names is often dropped in street names entered by users
    street = stringi::stri_replace_all_regex(street, "^am", ""),
    # Replace abbreviation of str. with strasse
    street = stringi::stri_replace_all_regex(street, "str\\.", "strasse"),
    # Remove all commas
    street = stringr::str_remove_all(street, "\\,"),
    # Replace abbreviation str even if it is not followed by a dot but only if
    # it appears at the end of the string
    street = stringr::str_replace(street, "str$", "strasse"),
    street = stringr::str_replace(street, "\\sstrasse", "strasse"),
    # The hyphen in street names is often ignored by users of wg-gesucht.de,
    # which is why it makes sense to remove it in the look-up table
    street = stringi::stri_replace_all_regex(street, "\\-", ""),
    # Remove all whitespaces
    street = stringi::stri_replace_all_regex(street, "\\s+", "")
  ) %>%
  string_squish() %>%
  tidyr::drop_na(zip_code)

real_estates_complete <-
  real_estates %>%
  tidyr::drop_na(latitude)

real_estates_missing <-
  real_estates %>%
  dplyr::filter(
    is.na(latitude)
  ) %>%
  # Removing coordinates as the coordinates for the missing cases are allocated
  # by the Openstreetmaps lines dataframe.
  dplyr::select(-c(latitude, longitude))

# street = stringi::stri_replace_all_regex(street, "^am", ""),
# street = stringi::stri_replace_all_regex(street, "\\-", ""),
# street = stringi::stri_replace_all_regex(street, "\\s+", ""),
# street = stringr::str_replace(street, "\\sstrasse", "strasse"),

street_names <-
  readRDS("output/berlin_streets.RDS") %>%
  dplyr::rename(street = name) %>%
  dplyr::mutate(
    street = stringi::stri_trans_general(street, "de-ASCII"),
    street = stringi::stri_trans_tolower(street),
    street = stringr::str_replace_all(
      street,
      c("\\sstrasse" = "strasse", "^am" = "", "\\-" = "",  "\\s+" = "")
    ),
    .after = 1
  ) %>%
  string_squish() %>%
  dplyr::group_by(street, zip_code) %>%
  dplyr::arrange(street, zip_code) %>%
  dplyr::summarise(.groups = "drop") %>%
  dplyr::mutate(
    id = dplyr::row_number()
  ) %>%
  sf::st_sf(agr = "constant") %>%
  sf::st_centroid()

# Extracting coordinates column and saving it as two columns "latitude" and
# "longitude".
street_coords <-
  sf::st_coordinates(street_names) %>%
  tibble::as_tibble(
    .name_repair = function(x) c("longitude", "latitude")
  )

# Adding the coordinates to the dataframe again, this time however not as a
# list of class "sf" but as two seperate columns.
street_names_df <-
  street_names %>%
  as.data.frame() %>%
  dplyr::select(-geometry) %>%
  cbind(street_coords)

real_estates_missing_join <-
  real_estates_missing %>%
  dplyr::left_join(street_names_df, by = c("street", "zip_code"))

# Objects which could be joined and do not need further processing
real_estates_missing_allocated <-
  real_estates_missing_join %>%
  tidyr::drop_na(id)

# Objects that could not be allocated by normal join and need to be processed further
real_estates_missing_remaining <-
  real_estates_missing_join %>%
  dplyr::filter(is.na(id))

real_estates_missing_remaining$street_match <- NA_character_
real_estates_missing_remaining$str_dist_match <- NA_real_

real_estates_missing_remaining <-
  real_estates_missing_remaining %>%
  dplyr::relocate(street, street_match, str_dist_match)

#   ____________________________________________________________________________
#   Loop start                                                              ####
i <- 1

for (i in 1:nrow(real_estates_missing_remaining)) {
  print(i)

  street_names_loop <-
    street_names_df %>%
    dplyr::filter(zip_code == real_estates_missing_remaining$zip_code[i])

  street_str_dist <- stringdist::stringdist(
    real_estates_missing_remaining$street[i],
    street_names_loop$street,
    method = "jw",
    p = 0.1
  ) %>%
    as.data.frame() %>%
    dplyr::rename(str_dist = ".") %>%
    cbind(street_names_loop) %>%
    dplyr::arrange(str_dist)

  # Skip to the next iteration if a match could not be found
  if (is.na(street_str_dist$str_dist[1])) next

  if (street_str_dist$str_dist[1] <= 0.1) {
    real_estates_missing_remaining$street_match[i] <- street_str_dist[1, "street"]
    real_estates_missing_remaining$str_dist_match[i] <- street_str_dist[1, "str_dist"]
    real_estates_missing_remaining$latitude[i] <- street_str_dist[1, "latitude"]
    real_estates_missing_remaining$longitude[i] <- street_str_dist[1, "longitude"]
  }
}

real_estates_missing_remaining_mod <-
  real_estates_missing_remaining %>%
  tidyr::drop_na(street_match) %>%
  dplyr::select(-c(street, str_dist_match)) %>%
  dplyr::rename(street = street_match)


real_estates_final <-
  rbind(real_estates_missing_allocated, real_estates_missing_remaining_mod) %>%
  dplyr::select(-id) %>%
  rbind(real_estates_complete) %>%
  dplyr::mutate(
    latitude = as.double(latitude),
    longitude = as.double(longitude)
  ) %>%
  sfheaders::sf_point(y = "latitude", x = "longitude", keep = TRUE)

saveRDS(real_estates_final, "output/real_estate_objects.RDS")
