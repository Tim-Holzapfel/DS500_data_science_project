ggsave2 <- function(
  filename, pointsize = 14, width = NA,
  height = NA, scale = NA, units = "mm", ...
  ) {
  library(ggplot2)

  # ggsave uses (for some reason) different plot sizes depending on whether the
  # script is sourced or manually executed (run). This is probably due to
  # ggsave choosing the plot size based on the graphics device and that the
  # graphics device is depending on the execution mode.

  if (is.na(width) | is.na(height)) {

    if (is.na(scale)) scale <- 2

    ggsave(
      filename,
      device = cairo_pdf,
      path = figure_dir,
      dpi = 1000,
      scale = scale,
      bg = "transparent",
      symbolfamily = "Roboto Condensed",
      family = "Roboto Condensed",
      fallback_resolution = 1000,
      antialias = "default",
      units = units,
      pointsize = pointsize
    )
  } else {
    ggsave(
      filename,
      device = cairo_pdf,
      path = figure_dir,
      width = width,
      height = height,
      dpi = 1000,
      scale = 1,
      bg = "transparent",
      symbolfamily = "Roboto Condensed",
      family = "Roboto Condensed",
      fallback_resolution = 1000,
      antialias = "default",
      units = units,
      pointsize = pointsize
    )
  }
}
