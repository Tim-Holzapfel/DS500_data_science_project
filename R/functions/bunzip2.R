
bunzip2 <- function(x) {
  x_sub <- stringr::str_extract(x, ".*\\.osm")

  output_path <-
    file.path(temp_dir, x_sub) %>%
    normalizePath(winslash = "/", mustWork = FALSE)

  R.utils::decompressFile(
    filename = find_files(x),
    destname = output_path,
    ext = "bz2",
    FUN = bzfile,
    remove = FALSE,
    skip = TRUE,
  )

  return(output_path)
}
