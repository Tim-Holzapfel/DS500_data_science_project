find_files <- function(data_file) {
  found_file <- list.files(
    path = data_dir,
    pattern = paste0("^", data_file, "$"),
    recursive = TRUE,
    full.names = TRUE
  )

  if (length(found_file) > 1) {
    stop("Found more than one file.")
  }

  if (length(found_file) == 0) {
    stop("Didn't find any file. Maybe wrong file ending?")
  }

  return(found_file)
}
