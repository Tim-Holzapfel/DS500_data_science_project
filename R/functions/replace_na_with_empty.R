replace_na_with_empty <- function(input_data, na_interpretation = NULL) {

  if (!is.null(na_interpretation)) {
    input_data <- dplyr::mutate(
      input_data,
      dplyr::across(
        .fns = function(x) stringr::str_replace_all(x, na_interpretation, "")
      )
    )
  }

  output_data <-
    dplyr::mutate(
      input_data,
      dplyr::across(
        .fns = function(x) stringr::str_replace_na(x, "")
      )
    )

  return(output_data)
}





