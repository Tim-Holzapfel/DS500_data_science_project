count_missing <- function(x) {
  apply(x, 2, function(x) {
    1 - sum(is.na(x)) / length(x)
  }) %>%
    as.data.frame() %>%
    tibble::rownames_to_column(var = "variables") %>%
    dplyr::rename(percentage_complete = ".") %>%
    dplyr::arrange(dplyr::desc(percentage_complete))
}
