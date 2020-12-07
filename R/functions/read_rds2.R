read_rds2 <- function(data_file) {
  file <- paste0(data_file, ".RDS")

  output_file <- readRDS(find_files(file))

  return(output_file)
}
