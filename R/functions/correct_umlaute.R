correct_umlaute <- function(x) {
  stringi::stri_replace_all_fixed(
    str = x,
    pattern = c("\u00C3\u0178", "\u00C3\u00B6", "\u00C3\u00BC", "\u00C3\u00A4", "\u00C3\u2013", "\u00C3\u201E"),
    replacement = c("\u00DF", "\u00F6", "\u00FC", "\u00E4", "\u00D6", "\u00C4"),
    vectorize_all = FALSE
  )
}
