convert_to_ascii <- function(input_data) {
  output_data <-
    dplyr::mutate(
      input_data,
      dplyr::across(
        .fns = function(x) stringi::stri_trans_general(x, "de-ASCII")
      )
    )
  return(output_data)
}
