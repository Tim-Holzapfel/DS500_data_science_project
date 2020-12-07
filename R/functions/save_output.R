save_output <- function(output_file, output_name, output_path = output_dir) {
  base_env <- baseenv()

  script_name <- output_name

  if (any(class(output_file) %in% "sf") | "geometry" %in% names(output_file)) {
    datafile_path <- file.path(output_path, paste0(output_name, ".RDS"))

    saveRDS(output_file, datafile_path)
  } else {
    datafile_path <- file.path(output_path, paste0(output_name, ".fst"))

    fst::write_fst(output_file,
      path = datafile_path,
      compress = 0
    )
  }

  base_env$project_scripts <-
    base_env$project_scripts %>%
    tibble::add_row(
      script_name = script_name,
      output_file = datafile_path,
      date_created = paste0(lubridate::now())
    ) %>%
    dplyr::arrange(script_name, dplyr::desc(date_created)) %>%
    dplyr::distinct(script_name, .keep_all = TRUE) %>%
    dplyr::distinct(output_file, .keep_all = TRUE)

  # File with R scripts and the associated output files.

  fst::write_fst(project_scripts,
    path = file.path(aux_dir, "project_scripts.fst"),
    compress = 0
  )
}
