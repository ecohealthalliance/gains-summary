library(rmarkdown)
load_all()
countries <- unique(all_gains$Country)
for (country in countries) {
  cat(paste0(country, "\n"))
  output_country_summary(country, quiet = TRUE, output_dir = "inst/out/")
}