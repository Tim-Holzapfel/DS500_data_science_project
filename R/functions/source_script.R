source_script <- function(R_script) {

  found_script <- list.files(
    path = file.path(project_root, "R"),
    pattern = paste0(R_script, "\\.R$"),
    recursive = TRUE,
    full.names = TRUE
  )

  if (length(found_script) > 1) {
    stop("Found more than one R-Script. Unable to source.")
  }

  if (length(found_script) == 0) {
    stop("Didn't find any R-Script.")
  }

  suppressWarnings(source(
    file = found_script,
    local = FALSE,
    verbose = FALSE,
    chdir = TRUE
  ))
}
