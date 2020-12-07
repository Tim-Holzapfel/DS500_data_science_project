sum_group <- function(input_data, type_name, type_label = type_name) {

  dataset_name <- deparse(substitute(input_data))

  dataset_result <-
    input_data %>%
    dplyr::filter(!is.na(name)) %>%
    dplyr::group_by(name) %>%
    dplyr::summarise(.groups = "drop") %>%
    sf::st_cast(to = "POINT") %>%
    dplyr::select(name) %>%
    dplyr::mutate(
      type = factor(x = type_name, labels = type_label)
    )

  return(dataset_result)
}
