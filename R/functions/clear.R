clear <- function() {

  pos <- -1
  envir <- as.environment(pos)
  options(warn = -1)
  withCallingHandlers(suppressWarnings(

    clear_fun <- function(envir) {
      cat("\f")

      grDevices::graphics.off()

      list <- ls(envir = globalenv())
      .Internal(remove(list, envir, FALSE))

      invisible(gc(verbose = FALSE, full = TRUE))
    }
  ))
    environment(clear_fun) <- rlang::global_env()

    clear_fun(envir)


}

