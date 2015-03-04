sentence_case <- function(x) {
  paste0(toupper(substring(x, 1, 1)), substring(x, 2))
}