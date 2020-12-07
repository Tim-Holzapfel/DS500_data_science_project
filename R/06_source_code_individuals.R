
clear()
# .options = furrr_options(seed = 42),
real_estates_overview_dataset <-
  read_file("real_estates_overview_dataset") %>%
  dplyr::select(ID, city, city_part, flat_type, date, company_offer)


wg_offers_table <- list.files(
  scrape_dir_single,
  pattern = "\\d+\\.html$",
  full.names = TRUE
) %>%
  tibble::as_tibble_col("path") %>%
  dplyr::mutate(
    ID = stringr::str_remove(basename(path), "\\.html") %>% as.integer(),
    N = dplyr::row_number()
  ) %>%
  dplyr::arrange(N)

plan(multisession, workers = cores)

# purrr::map2_dfr(
# furrr::future_map_dfr(

# x <- wg_offers_table$path[9]

real_estate_offers <-
  furrr::future_map_dfr(
    wg_offers_table$path,
    .options = furrr_options(seed = 42),
    function(x) {
      offer_source_code <- xml2::read_html(x)

      # Check if the offer has not been deleted meanwhile.
      error_check <-
        rvest::html_nodes(offer_source_code, css = "title") %>%
        rvest::html_text() %>%
        stringr::str_detect("Anzeige nicht vorhanden\\/gel\u00F6scht")

      if (error_check) {
        return()
      }

      # Whether the real estate object in question is a apartment, one-room
      # flat, house etc.
      real_estate_type <-
        offer_source_code %>%
        rvest::html_text(TRUE) %>%
        stringr::str_extract("(?<=var\\scustom_dimension_rubrik\\s\\=\\s)[^\\;]*") %>%
        stringr::str_remove_all("\\'")

      # Extracting user-id and asset-id
      identifiers <-
        rvest::html_node(
          offer_source_code,
          css = "script[src='/js/User/ContactedData.class.tjb1604366072.js'] + script"
        ) %>%
        rvest::html_text(trim = TRUE) %>%
        stringr::str_replace_all("\\\"", "") %>%
        stringr::str_squish() %>%
        stringr::str_extract_all("(?<=(user|asset)\\_id\\:\\s)\\d+", simplify = TRUE) %>%
        tibble::as_tibble_row(.name_repair = function(x) c("user_id", "asset_id"))

      address <-
        rvest::html_node(offer_source_code, css = "a[href^='#mapContainer']") %>%
        rvest::html_text(trim = TRUE) %>%
        stringr::str_replace_all("\\\r\\\n\\\r\\\n", "|") %>%
        stringr::str_squish() %>%
        stringr::str_split("\\|", n = 2, simplify = TRUE) %>%
        tibble::as_tibble_row(
          .name_repair = function(names) {
            if (length(names) == 2) {
              return(c("street", "city_and_postal_code"))
            } else if (length(names == 1)) {
              return(c("city_and_postal_code"))
            } else {
              return(vctrs::vec_as_names(names, repair = "universal"))
            }
          }
        ) %>%
        string_squish() %>%
        dplyr::mutate(
          zip_code = stringr::str_extract(city_and_postal_code, "^\\d+"),
          city_region = stringr::str_remove(city_and_postal_code, "\\d+") %>% string_unique()
        ) %>%
        dplyr::select(-city_and_postal_code)

      glyphs <-
        rvest::html_nodes(
          offer_source_code,
          css = "span[class^='glyphicons'][class*='noprint']"
        ) %>%
        rvest::html_attrs() %>%
        purrr::map(1) %>%
        unlist() %>%
        unname() %>%
        stringr::str_extract("(?<=glyphicons glyphicons\\-).*(?=noprint)") %>%
        snakecase::to_snake_case()

      main_information <-
        rvest::html_nodes(offer_source_code, css = "div.col-xs-6.col-sm-4") %>%
        rvest::html_text(trim = TRUE) %>%
        stringr::str_squish() %>%
        tibble::as_tibble_row(
          .name_repair = function(x) glyphs
        )

      latitude <-
        rvest::html_text(offer_source_code, TRUE) %>%
        stringr::str_extract("(?<=\\\"lat\\\"\\:)\\d+\\.\\d+") %>%
        as.double()

      longitude <-
        rvest::html_text(offer_source_code, TRUE) %>%
        stringr::str_extract("(?<=\\\"lng\\\"\\:)\\d+\\.\\d+") %>%
        as.double()

      basic_facts <-
        rvest::html_nodes(offer_source_code, css = "#basic_facts_wrapper label") %>%
        rvest::html_text(trim = TRUE) %>%
        matrix(nrow = 2, byrow = F) %>%
        as.data.frame() %>%
        janitor::row_to_names(row_number = 2, remove_rows_above = F) %>%
        rename_with_snakecase()

      z_wg_offer_final <-
        cbind(
          identifiers, real_estate_type, address,
          basic_facts, main_information, latitude, longitude
        )
    }
  )


z_real_estate_offers_final <-
  real_estate_offers %>%
  dplyr::left_join(real_estates_overview_dataset, by = c("asset_id" = "ID")) %>%
  tidyr::drop_na(date) %>%
  convert_to_ascii() %>%
  string_squish() %>%
  # replacing NA (missing entries) with an empty string makes it easier to manipulate
  # the data, otherwise data manipulation becomes increasingly difficult as
  # missing entries in filtering are often implicitly assumed to be FALSE.
  replace_na_with_empty("^n\\.a\\.$") %>%
  # The focus of this study is on real estates that are meant to be inhabited by
  # individuals or couples, not on shared accommodation. Therefore, if the ad
  # mentions a shared bathroom or a shared kitchen, it is to be assumed that the
  # real-estate in question is a shared flat.
  tidyr::unite(
    col = "property_size",
    c("zimmergroesse", "wohnung", "haus"),
    na.rm = TRUE,
    sep = ""
  ) %>%
  dplyr::filter(
    real_estate_type != "wg",
    stringr::str_detect(bath_bathtub, "Badmitbenutzung|(Nicht vorhanden)", negate = TRUE),
    stringr::str_detect(dining_set, "Kuechenmitbenutzung", negate = TRUE)
  ) %>%
  dplyr::mutate(
    # It is somewhat suspicious if the type of heating is not specified and
    # therefore assumingly somewhat inferior.
    heating = dplyr::case_when(
      stringr::str_detect(fire, "Gasheizung") ~ "gas",
      stringr::str_detect(fire, "Zentralheizung") ~ "central",
      stringr::str_detect(fire, "Fernwaerme") ~ "district",
      stringr::str_detect(fire, "Nachtspeicherofen") ~ "storage",
      stringr::str_detect(fire, "Ofenheizung") ~ "furnace",
      stringr::str_detect(fire, "Kohleofen") ~ "coal",
      TRUE ~ as.character("not stated")
    ) %>% factor(),
    building_type = dplyr::case_when(
      stringr::str_detect(mixed_buildings, "sanierter Altbau") ~ "renovated old building",
      stringr::str_detect(mixed_buildings, "Mehrfamilienhaus") ~ "apartment building",
      stringr::str_detect(mixed_buildings, "Altbau") ~ "old building",
      stringr::str_detect(mixed_buildings, "Neubau") ~ "new construction",
      stringr::str_detect(mixed_buildings, "Hochhaus") ~ "multistory building",
      stringr::str_detect(mixed_buildings, "Doppelhaus") ~ "semi-detached house",
      stringr::str_detect(mixed_buildings, "Reihenhaus") ~ "row house",
      stringr::str_detect(mixed_buildings, "Einfamilienhaus") ~ "single-family house",
      stringr::str_detect(mixed_buildings, "Plattenbau") ~ "prefabricated building",
      TRUE ~ as.character("")
    ) %>% factor(),
    # floor has to be converted to a factor as the are references to the attic
    # or top floor. As there are no information given about how many floors
    # there are, only that the attic is the highest of them, it is not possible
    # to assign an integer to attic references.
    story = dplyr::case_when(
      stringr::str_detect(building, "EG") ~ "first floor",
      stringr::str_detect(building, "1. OG") ~ "second floor",
      stringr::str_detect(building, "2. OG") ~ "third floor",
      stringr::str_detect(building, "3. OG") ~ "fourth floor",
      stringr::str_detect(building, "4. OG") ~ "fifth floor",
      stringr::str_detect(building, "5. OG") ~ "sixth floor",
      stringr::str_detect(building, "hoeher als 5. OG") ~ "above sixth floor",
      stringr::str_detect(building, "Dachgeschoss") ~ "top floor",
      stringr::str_detect(building, "Tiefparterre") ~ "lower ground floor",
      stringr::str_detect(building, "Hochparterre") ~ "raised ground floor",
      stringr::str_detect(building, "Keller") ~ "basement",
      TRUE ~ as.character("not stated")
    ) %>% factor(),
    # if there was no presence of a kitchen specified then it is assumed that
    # the kitchen is missing. The reasoning behind this is that, because the
    # existence of a kitchen is advantageous and enhances the property value,
    # the only reasoning for not declaring it's presence is the non-existence of
    # it.
    kitchen = dplyr::case_when(
      stringr::str_detect(dining_set, "Nicht vorhanden") ~ "nonexistent",
      stringr::str_detect(dining_set, "Kochnische") ~ "kitchenette",
      stringr::str_detect(dining_set, "Eigene Kueche") ~ "existent",
      TRUE ~ as.character("nonexistent")
    ) %>% factor(),
    television = dplyr::case_when(
      stringr::str_detect(display, "Satellit") ~ "satellite",
      stringr::str_detect(display, "Kabel") ~ "cable",
      stringr::str_detect(display, "Kabel, Satellit") ~ "satellite and cable",
      TRUE ~ as.character("cable")
    ) %>% factor(),
    parking = dplyr::case_when(
      stringr::str_detect(car, "Tiefgaragenstellplatz") ~ "underground parking space",
      stringr::str_detect(car, "gute Parkmoeglichkeiten") ~ "good parking possibilities",
      stringr::str_detect(car, "schlechte Parkmoeglichkeiten") ~ "bad parking possibilities",
      stringr::str_detect(car, "Bewohnerparken") ~ "resident parking",
      stringr::str_detect(car, "eigener Parkplatz") ~ "private parking space",
      TRUE ~ as.character("bad parking possibilities")
    ) %>% factor(),
    furnished = dplyr::case_when(
      stringr::str_detect(bed, "moebliert") ~ "fully furnished",
      stringr::str_detect(bed, "teilmoebliert") ~ "partly furnished",
      TRUE ~ as.character("unfurnished")
    ) %>% factor(),
    room_number = stringr::str_extract(flat_type, "^\\d+(?=\\-)") %>%
      as.integer(),
    property_type = dplyr::case_when(
      stringr::str_detect(flat_type, "Wohnung") ~ "apartment",
      stringr::str_detect(flat_type, "Haus") ~ "house",
      TRUE ~ as.character("")
    ) %>% factor(),
    # With regard to the type of flooring, the only distinction of real interest
    # is whether it is of higher quality like parquet flooring or floorboards or
    # something simpler like laminate or PVC.
    parquet = stringr::str_detect(fabric, "Parkett|Dielen"),
    underfloor_heating = stringr::str_detect(fabric, "Fussbodenheizung"),
    bathtub = stringr::str_detect(bath_bathtub, "Badewanne"),
    guest_bathroom = stringr::str_detect(bath_bathtub, "Gaeste WC"),
    property_size = stringr::str_extract(property_size, "\\d+") %>%
      as.double(),
    house_number = stringr::str_extract(street, "[A-z]?\\d+[A-z]?$"),
    street = stringr::str_remove_all(street, "\\d+"),
    ancillary_costs = stringr::str_extract(nebenkosten, "\\d+") %>%
      as.double(),
    other_costs = stringr::str_extract(sonstige, "\\d+") %>%
      as.double(),
    total_rent = stringr::str_extract(gesamtmiete, "\\d+") %>%
      as.double(),
    net_rent = stringr::str_extract(miete, "\\d+") %>%
      as.double(),
    deposit = stringr::str_extract(kaution, "\\d+") %>%
      as.double(),
    redemption_agreement = stringr::str_extract(abloesevereinbarung, "\\d+") %>%
      as.double(),
    green_electricity = stringr::str_detect(leaf, "Oekostrom"),
    # Whether or not pets are allowed is not really relevant as just because it
    # is not explicitly stated that pets are allowed does not necessarily mean
    # that pets are forbidden.
    folder_closed = stringr::str_remove(folder_closed, "\\,?\\s?Haustiere\\serlaubt"),
    folder_closed = stringr::str_replace_all(
      folder_closed,
      c(
        "Waschmaschine" = "washing_machine", "Terrasse" = "patio",
        "Spuelmaschine" = "dishwasher", "Gartenmitbenutzung" = "yard_sharing",
        "Garten" = "yard", "Aufzug" = "elevator",
        "Balkon" = "balcony", "Keller" = "basement",
        "Fahrradkeller" = "basement_bike_racks",
        "Dachboden" = "attic"
      )
    ),
    electricity = stringr::str_remove(electricity, "^.*noetig\\."),
    construction_year = stringr::str_extract(electricity, "(?<=Baujahr\\s)\\d+"),
    energy_consumption = stringr::str_extract(electricity, "(?<=V\\:\\s)\\d+(?=kW)"),
    energy_efficiency_class = stringr::str_extract(electricity, "(?<=Energieeffizienzklasse)(\\s\\w)|(\\+)"),
    energy_certificate = stringr::str_detect(
      electricity,
      "Verbrauchsausweis|Bedarfsausweis"
    ),
    electricity = stringr::str_remove_all(
      electricity,
      "(Baujahr\\s\\d+)|(V\\:\\s.*\\))|Verbrauchsausweis|Bedarfsausweis|(Energieeffizienzklasse\\s?\\+?\\w?\\+?)"
    ),
    electricity = stringr::str_remove_all(electricity, ","),
    electricity = stringr::str_squish(electricity),
    energy_type = dplyr::case_when(
      stringr::str_detect(electricity, "Gas") ~ "gas heating",
      stringr::str_detect(electricity, "Nahwaerme") ~ "local heating",
      stringr::str_detect(electricity, "Fernwaerme") ~ "district heating",
      stringr::str_detect(electricity, "Waermelieferung") ~ "supplied heat",
      stringr::str_detect(electricity, "Strom") ~ "electric heating",
      stringr::str_detect(electricity, "Oel") ~ "oil",
      stringr::str_detect(electricity, "Solar") ~ "solar",
      stringr::str_detect(electricity, "Holzpellets") ~ "wood pellets",
      stringr::str_detect(electricity, "Erdgas leicht") ~ "light natural gas",
      stringr::str_detect(electricity, "Erdgas schwer") ~ "heavy natural gas",
      stringr::str_detect(electricity, "Fluessiggas") ~ "liquid natural gas",
      stringr::str_detect(electricity, "Holz") ~ "wood",
      stringr::str_detect(electricity, "Erdwaerme") ~ "geothermal heating",
      stringr::str_detect(electricity, "Kohle/Koks") ~ "coal",
      TRUE ~ as.character("")
    )
  ) %>%
  string_seperate(folder_closed, ",\\s") %>%
  # Even though the accessibility is definitely important, whether or not the
  # building is accessible with a wheelchair is only specified in less than 1%
  # of the cases which is why it is not included in the regression. Group
  # specifies whether or not the real estate in question is suitable for a
  # shared living community. But this only means that it can be used for a
  # shared living community but it does not have to be. Wifi-alt, specifying the
  # quality of the internet connection is also only declared in less than 1% of
  # the cases.
  dplyr::select(
    -c(
      bath_bathtub, mixed_buildings, building, fire, fabric, dining_set,
      person_wheelchair, group, wifi_alt, display, parking, bed, car,
      real_estate_type, zimmer, flat_type, nebenkosten, sonstige, gesamtmiete,
      miete, kaution, abloesevereinbarung, leaf
    )
  ) %>%
  dplyr::relocate(user_id, asset_id, city, total_rent, property_size, story, elevator,
                  room_number, property_type, building_type, furnished, parquet, bathtub)

save_file(z_real_estate_offers_final, "real_estate_offers")



names(z_real_estate_offers_final)







