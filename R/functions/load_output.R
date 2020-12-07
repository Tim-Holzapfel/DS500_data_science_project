load_output <- function(script_name) {
  index_output_file <- which(project_scripts$script_name == script_name)

  output_file_name <- project_scripts$output_file[index_output_file]

  if (length(output_file_name) == 0) stop(paste0("File ", script_name,  " does not exist."))

  file_ending <-
    stringr::str_extract(output_file_name, "(?<=\\.)(fst)|(RDS)")

  if (file_ending == "RDS") {
    readRDS(output_file_name)
  } else {
    fst::read_fst(output_file_name)
  }
}
