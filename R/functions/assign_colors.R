assign_colors <- function(input_icon_list) {

  n_list <- seq_along(input_icon_list)

  icon_colors <-
    c(
      "red", "darkred", "lightred", "orange", "beige", "green", "darkgreen",
      "lightgreen", "blue", "darkblue", "lightblue", "purple", "darkpurple",
      "pink", "cadetblue", "white", "gray", "lightgray", "black"
    )


  result_list <-
    purrr::map2(input_icon_list, n_list, function(x, y) x$iconColor <- icon_colors[y])
}
