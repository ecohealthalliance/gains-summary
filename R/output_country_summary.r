output_country_summary <- function(country, document = "inst/doc/generic summary/gains.rmd", output_dir = NULL, ...) {
  require(dplyr)
  require(rmarkdown)

  if(!is.null(output_dir)) {
    output_dir <- normalizePath(output_dir)
  }

  data(all_gains)
  country_gains <- filter(all_gains, Country == country)

  render(document, output_format = "pdf_document", output_file = paste0(country, ".pdf"), output_dir = output_dir, ...)
}