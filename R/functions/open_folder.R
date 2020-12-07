open_folder <- function(file_dir) {

  path <- dirname(find_files(file_dir))

  path_paste <- paste0("start ", path)

  shell(path_paste, wait = FALSE, translate = TRUE, shell = "cmd")

}
