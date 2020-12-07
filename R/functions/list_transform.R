list_transform <- function(data_set) {
  library(magrittr)

  initial_list <-
    purrr::map(data_set, function(x) {
      x[(purrr::map(x, function(y) {
        class(y)
      }) != "list") & (lengths(x) == 1)]
    })

  stacked_list <- rlist::list.stack(initial_list, fill = TRUE)

  return(stacked_list)
}
