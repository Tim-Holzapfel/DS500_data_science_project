make_awesome_icon <- function(text, markerColor) {
  requireNamespace("leaflet")

  marker_check <- tryCatch(
    {
      markerColor
    },
    error = function(e) {
      "blue"
    }
  )

  inner_fun <- function(icon = "home", library = "glyphicon",
                        markerColor = "blue",
                        iconColor = "white", spin = FALSE, extraClasses = NULL,
                        squareMarker = FALSE, iconRotate = 0, fontFamily = "monospace",
                        text = NULL) {
    if (!inherits(library, "formula")) {
      leaflet:::verifyIconLibrary(library)
    }
    icon <- filterNULL(list(
      icon = icon, library = library,
      markerColor = markerColor, iconColor = iconColor, spin = spin,
      extraClasses = extraClasses, squareMarker = squareMarker,
      iconRotate = iconRotate, font = fontFamily, text = text
    ))
    structure(icon, class = "leaflet_awesome_icon")
  }

  environment(inner_fun) <- rlang::pkg_env("leaflet")

  result_marker <- inner_fun(text = fontawesome::fa(text), markerColor = marker_check)

  return(result_marker)
}
