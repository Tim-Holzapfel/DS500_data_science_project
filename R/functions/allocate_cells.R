allocate_cells <- function(data_input) {
  #stopifnot("data.frame" %in% class(data_input))

  data_input <- as.data.frame(data_input)

  var_names <-
    data_input %>%
    unlist() %>%
    stringr::str_extract("\\w+(?=(\\,\\w+\\:|\\:))") %>%
    unique() %>%
    stringi::stri_remove_na()

  data_results <-
    tibble::tibble(
      .rows = nrow(data_input)
    )

  data_results[, var_names] <- NA_character_

  for (i in 1:nrow(data_input)) {
    var_index <-
      plyr::colwise(.fun = function(x) {
        stringr::str_detect(x, paste0("^", var_names[i]))
      })(data_input) %>%
      as.matrix() %>%
      which(arr.ind = T)

    data_results[var_index[, "row"], var_names[i]] <-
      data_input[var_index] %>% stringr::str_remove("^.*\\:")
  }

  return(data_results)
}






# var_table <-
#   berlin_lines %>%
#   as.data.frame() %>%
#   magrittr::extract2("other_tags") %>%
#   unlist() %>%
#   stringr::str_remove_all("\\\"") %>%
#   stringr::str_replace_all("\\:", "_") %>%
#   stringr::str_split(",", simplify = TRUE) %>%
#   as.data.frame()
#
# # var_names <-
# #   var_table %>%
# #   unlist() %>%
# #   stringr::str_extract(".*(?=\\=\\>)") %>%
# #   unique() %>%
# #   stringi::stri_remove_na()
#
# var_names <- "postal_code"
#
# data_results <-
#   tibble::tibble(
#     .rows = nrow(var_table)
#   )
#
# data_results[, var_names] <- NA_character_
#
# #x <- var_table
# i <- 2
#
# var_index <-
#   plyr::colwise(.fun = function(x) {
#     stringr::str_detect(x, paste0("^", var_names[i], "\\=\\>"))
#   })(var_table) %>%
#   as.matrix() %>%
#   which(arr.ind = T)
#
# data_results[var_index[, "row"], var_names[i]] <-
#   var_table[var_index] %>% stringr::str_remove("^.*\\=\\>")
#
#
#
# for (i in 1:length(var_names)) {
#   print(i)
#   var_index <-
#     plyr::colwise(.fun = function(x) {
#       stringr::str_detect(x, paste0("^", var_names[i], "\\=\\>"))
#     })(var_table) %>%
#     as.matrix() %>%
#     which(arr.ind = T)
#
#   data_results[var_index[, "row"], var_names[i]] <-
#     var_table[var_index] %>% stringr::str_remove("^.*\\=\\>")
#
# }












