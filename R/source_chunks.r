source_chunks <- function(file) {
  infile <- basename(file)
  path <- dirname(file)
  outfile <- paste0(tools::file_path_sans_ext(infile), ".r")

  knitr::purl(input = file.path(path, infile),
              output = file.path(path, outfile))
  source(file.path(path, outfile), echo = TRUE)
}