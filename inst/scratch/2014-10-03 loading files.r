load_all()

require(plyr)

china <- read.csv("inst/rawdata/CHINA.csv", as.is = TRUE)

str(china)

kevin <- read.csv("inst/rawdata/KevinP1rank.csv", as.is = TRUE)
str(kevin)
# kevin$vFamily <- factor(kevin$vFamily)
# kevin$vGenus <- factor(kevin$vGenus)

