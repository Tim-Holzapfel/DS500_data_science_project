read_fst <- function(data_file) {
  file <- paste0(data_file, ".fst")

  output_file <- fst::read_fst(find_files(file))

  return(output_file)
}
