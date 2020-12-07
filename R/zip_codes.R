
clear()

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
  sfheaders::sf_point(x = "longitude", y = "latitude", keep = TRUE)

sf::st_crs(plz_data) <- TA


saveRDS(plz_data, "output/plz_tabelle.RDS")






#
#
# zip_codes_list <- list.files(
#   path = "data/PLZ",
#   recursive = TRUE,
#   full.names = TRUE,
#   pattern = "\\.html$"
# )
#
# plz_tabelle_final <- purrr::map_dfr(zip_codes_list, function(x) {
#   html_source <- xml2::read_html(x, encoding = "UTF-8", options = "BIG_LINES")
#
#   zip_code_table <-
#     html_source %>%
#     rvest::html_node(
#       css = "#ms-maincontent > div:nth-child(1) > div:nth-child(2) > div:nth-child(1)"
#     ) %>%
#     xml2::xml_child(1) %>%
#     rvest::html_table(fill = TRUE, header = F) %>%
#     janitor::row_to_names(1) %>%
#     replace_empty()
#
#   zip_code_table_final <-
#     zip_code_table$PLZ %>%
#     stringr::str_replace_all("(\\d{5})", "\\1 ") %>%
#     stringr::str_split("\\s", simplify = TRUE) %>%
#     as.data.frame() %>%
#     replace_empty() %>%
#     cbind(zip_code_table) %>%
#     dplyr::select(-PLZ) %>%
#     tidyr::pivot_longer(
#       data = .,
#       cols = dplyr::starts_with("V"),
#       values_drop_na = TRUE
#     ) %>%
#     dplyr::select(-name) %>%
#     dplyr::rename(plz = value) %>%
#     dplyr::mutate(
#       plz = as.integer(plz)
#     )
# })
#
# saveRDS(plz_tabelle_final, "output/plz_tabelle_meinestadt.RDS")
