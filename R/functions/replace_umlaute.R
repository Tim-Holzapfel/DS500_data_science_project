replace_umlaute <- function(string_with_umlaute) {
  stringi::stri_replace_all_fixed(
    string_with_umlaute,
    c("\u00E4", "\u00F6", "\u00FC"),
    c("ae", "oe", "ue"),
    vectorize_all = FALSE
  )
}
