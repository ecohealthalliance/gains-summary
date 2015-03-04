source_chunks <- function(file) {
  stopifnot(tolower(tools::file_ext(file)) %in% c("rnw", "rmd"))

  infile <- basename(file)
  path <- dirname(file)
  outfile <- paste0(tools::file_path_sans_ext(infile), ".r")

  knitr::purl(input = file.path(path, infile),
              output = file.path(path, outfile))
  source(file.path(path, outfile), echo = TRUE)
}