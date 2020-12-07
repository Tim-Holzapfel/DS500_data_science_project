list_remove <- function(list_input, remove_index) {
  list_remove_inner <- function(list_input, remove_index) {
    if (sjmisc::is_empty(remove_index)) {
      list_output <-
        list_input
    } else {
      list_output <-
        list_input %>%
        rlist::list.remove(remove_index)
    }
    return(list_output)
  }

  environment(list_remove_inner) <- rlang::global_env()


  list_output_final <- list_remove_inner(list_input, remove_index)

  return(list_output_final)
}
