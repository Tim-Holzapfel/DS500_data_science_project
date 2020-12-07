file_path <- function(file_input) {
  # file_root <- dirname(rstudioapi::getSourceEditorContext()$path)
  #
  # return(file.path(file_root, file_append))

  file_path_output <- isoreader::iso_root_paths(file_input)$path

  return(file_path_output)

}
