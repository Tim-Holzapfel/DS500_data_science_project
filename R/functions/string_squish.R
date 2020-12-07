string_squish <- function(dataframe_tb) {
  tb_return <-
    dataframe_tb %>%
    dplyr::mutate(dplyr::across(
      .cols = where(is.character),
      .fns = stringr::str_squish
    ))
  return(tb_return)
}
