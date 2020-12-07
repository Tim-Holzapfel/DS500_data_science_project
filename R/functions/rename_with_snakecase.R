rename_with_snakecase <- function(input_file) {
  output_file <-
    input_file %>%
    dplyr::rename_with(
      .fn = function(x) snakecase::to_snake_case(x, transliterations = "de-ASCII")
    )
  return(output_file)
}
