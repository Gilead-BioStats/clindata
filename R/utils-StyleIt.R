StyleIt <- function() {
  myStyle <- styler::tidyverse_style()

  myStyle$indention$unindent_fun_dec <- NULL
  myStyle$indention$update_indention_ref_fun_dec <- NULL
  myStyle$line_break$remove_line_breaks_in_fun_dec <- NULL

  styler::style_pkg(transformers = myStyle)
}
