load_all()
library(dplyr)

# Load China data
china_full <- read.csv("inst/rawdata/CHINA.csv", as.is = TRUE)
keeps <- c(1:4, 7, 8, 10:11, 14, 16:20, 23, 24, 28, 40, 49:52, 55, 56, 60, 137, 138, 139)
china <- china_full[, keeps]
names(china)

# Load Kevin's data
kevin <- read.csv("inst/rawdata/KevinP1rank.csv", as.is = TRUE)
names(kevin)

# Transform virus name variables so they're matchable.
library(stringr)
kevin$virusname <- str_replace_all(kevin$virusname, " ", "")

select_first <- function(x) {
    str_split(x, "\\|")[[1]][1]
}

china$VirusName <- sapply(china$VirusName, select_first, USE.NAMES = FALSE)

china$VirusName[china$VirusName == "theSARSRelatedCoronavirusHKU3"] <- "SARSRelatedCoronavirusHKU3"
china$VirusName[china$VirusName == ""] <- NA

# Merge the data frames
china <- left_join(china, kevin, by = c("VirusName" = "virusname"))
china$positive <- 0
china[!is.na(china$VirusName), "positive"] <- 1


names(china)

china$Risklevel[is.na(china$Risklevel)] <- 9

china_coords <- china %>%
  group_by(AnimalID..GAINS.) %>%
  summarize(RiskLevel = min(Risklevel),
            Longitude = mean(Longitude),
            Latitude = mean(Latitude))

write.csv(china_coords, file = "~/Desktop/china_coords.csv")