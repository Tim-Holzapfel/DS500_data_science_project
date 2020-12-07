save_source <- function(input_file, file_name) {
  fileConn <- file(file_name, encoding = "UTF-8")
  writeLines(input_file, fileConn)
  close(fileConn)
}
