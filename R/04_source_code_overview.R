
# Extract individual IDs from the overview pages.

clear()

html_source_list <-
  list.files(
    path = scrape_dir,
    pattern = "\\.html$",
    full.names = TRUE
  ) %>% sort()

#i <- 1


plan(multisession, workers = cores)

wg_offers_final <-
  furrr::future_map_dfr(
    html_source_list,
    .options = furrr_options(seed = 42),
    function(x) {
      html_link <- x

      html_source <- xml2::read_html(html_link)

      # Name of the city.
      city_name <- rvest::html_node(html_source, css = "title") %>%
        rvest::html_text() %>%
        stringr::str_extract(
          "(?<=Angebote in\\s).*$"
        )

      # Source of the individual offers (without any ads)
      wg_offers <- rvest::html_nodes(html_source, css = ".wgg_card.offer_list_item")

      if (sjmisc::is_empty(wg_offers)) {
        return()
      }

      # Pattern to spot professional real estate offers.
      regex_company_pattern <-
        paste("hotel", "hostel", "gmbh", "ohg", "immobilien", sep = "|") %>%
        stringr::regex(ignore_case = TRUE)

      # Real estate offers by companies can be identified as they usually include the
      # name of the business in the alt-attribute of the displayed images. It seems
      # prudent to control for the
      company_offer <-
        wg_offers %>%
        rvest::html_nodes(css = "img") %>%
        rvest::html_attr("alt") %>%
        stringr::str_detect(regex_company_pattern) %>%
        stringi::stri_replace_na("FALSE") %>%
        as.logical()

      unique_ids <-
        rvest::html_attr(wg_offers, name = "data-id") %>%
        tibble::as_tibble_col(column_name = "ID") %>%
        dplyr::mutate(
          offer_url = paste0("https://www.wg-gesucht.de/", ID, ".html")
        )

      # Date when the html source was scraped from wg-gesucht.de, extracted from the
      # meta data of the html file. This approach is valid since the html files are
      # treated as read only and have not been modified in any way after the source
      # code from wg-gesucht.de has been extracted.
      html_date_created <- lubridate::as_date(file.info(html_link)$mtime)



      # Date when the offer was first published. Special care has to be taken
      # concerning publishing date of the ad as the field "Online:" can either
      # pinpoint the exact date when the offer was published or the minutes/hours that
      # have passed since the publication.
      online_since <-
        wg_offers %>%
        rvest::html_text() %>%
        stringr::str_extract(
          "(?<=Online\\:\\s)\\d{2}\\.\\d{2}\\.\\d{4}|\\d+\\s(Tag|Stunde|Minute)"
        ) %>%
        tibble::as_tibble_col("date_offer") %>%
        dplyr::mutate(
          date = dplyr::case_when(
            stringr::str_detect(date_offer, "Minute") ~
            (html_date_created - lubridate::minutes(stringr::str_extract(date_offer, "\\d+(?=\\sMinute)"))) %>%
              lubridate::as_date(),
            stringr::str_detect(date_offer, "Stunde") ~
            (html_date_created - lubridate::hours(stringr::str_extract(date_offer, "\\d+(?=\\sStunde)"))) %>%
              lubridate::as_date(),
            stringr::str_detect(date_offer, "Tag") ~
            html_date_created - lubridate::days(stringr::str_extract(date_offer, "\\d+(?=\\sTag)")),
            TRUE ~ lubridate::as_date(date_offer, format = "%d.%m.%Y")
          )
        ) %>%
        dplyr::select(-date_offer)

      # Some users have the unsavioury attitude to use the vertical seperator | in
      # their descriptions which makes it difficult to correctly seperate the
      # individual parts of the real estate offer. The title of the real estate offer
      # is therefore simply removed.
      ad_details <- rvest::html_nodes(wg_offers, css = ".printonly") %>%
        rvest::html_text() %>%
        stringr::str_squish() %>%
        stringr::str_remove(".*(?=\\s\\d+\\s\\u20AC)") %>%
        stringr::str_remove("\\sVerf\u00FCgbar.*") %>%
        stringr::str_split("\\|", simplify = TRUE) %>%
        tibble::as_tibble(
          .name_repair = function(x) {
            c("price", "flat_type_and_size", "city_and_city_part", "street")
          }
        ) %>%
        dplyr::mutate(
          price = stringr::str_extract(price, "\\d+(?=\\s\\u20AC)") %>% as.double(),
          flat_size_m2 = stringr::str_extract(flat_type_and_size, "\\d+(?=\\sm\u00B2)") %>% as.double(),
          flat_type = stringr::str_extract(flat_type_and_size, "(?<=\\d\\sm\u00B2\\s).*"),
          city = city_name,
          city_part = stringr::str_remove(city_and_city_part, city)
        ) %>%
        string_squish() %>%
        dplyr::select(-c("flat_type_and_size", "city_and_city_part"))

      z_wg_offer_final <-
        unique_ids %>%
        cbind(online_since, ad_details, company_offer) %>%
        dplyr::mutate(
          city_string = stringr::str_extract(basename(html_link), ".*(?=\\_page)"),
          overview_html = html_link
        )
    }
  )

wg_offers_final_unique <-
  wg_offers_final %>% dplyr::distinct(ID, .keep_all = TRUE)

save_file(wg_offers_final_unique, "real_estates_overview_dataset")
