
## ----setup, include = FALSE----------------------------------------------
knitr::opts_knit$set(root.dir = "~/Dropbox (EHA)/repositories/gains-summary")
knitr::opts_chunk$set(echo = FALSE, message = FALSE, warning = FALSE)

# Load required packages
library(plyr)
library(dplyr)
library(magrittr)


## ----load_and_clean, include = FALSE-------------------------------------
# Load China data
china_full <- read.csv("inst/rawdata/CHINA.csv", as.is = TRUE)
keeps <- c(1:4, 7, 8, 14, 16:20, 23, 24, 28, 40, 49:52, 55,
    56, 60, 137, 138, 139)
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


## ----, results = "asis"--------------------------------------------------
viruses <- table(china$VirusName[!(china$VirusName == "")])
viruses <- data.frame(viruses)
viruses <- viruses[order(viruses$Freq, decreasing = TRUE), ]
names(viruses) <- c("VirusName", "freq")
viruses$VirusName <- as.character(viruses$VirusName)

knitr::kable(head(viruses, 15), col.names = c("Virus name", "Frequency"), caption = ("Top 15 viruses from GAINS in China"))

# For my own edification, another way of doing this.
# viruses <- group_by(china, VirusName)
# viruses <- summarize(viruses, freq = length(VirusName))
# viruses <- viruses[order(viruses$freq, decreasing = TRUE), ]
# knitr::kable(viruses) 


## ----, results = "asis"--------------------------------------------------
kable2 <- function(x, ...) knitr::kable(data.frame(table(x)), ...)
kable2(unique(china$VirusName) %in% kevin$virusname)


## ----, results = "asis"--------------------------------------------------
library(stringr)
kevin$virusname <- str_replace_all(kevin$virusname, " ", "")
kable2(unique(china$VirusName) %in% kevin$virusname)


## ----, results = "asis"--------------------------------------------------
select_first <- function(x) {
    str_split(x, "\\|")[[1]][1]
}

china$VirusName <- sapply(china$VirusName, select_first, USE.NAMES = FALSE)
kable2(unique(china$VirusName) %in% kevin$virusname)
# unique(china$VirusName)[!((unique(china$VirusName) %in% kevin$virusname))]


## ----, results = "asis"--------------------------------------------------
china$VirusName[china$VirusName == "theSARSRelatedCoronavirusHKU3"] <- "SARSRelatedCoronavirusHKU3"
china$VirusName[china$VirusName == ""] <- NA
kable2(unique(china$VirusName) %in% kevin$virusname)


## ----, results = "asis"--------------------------------------------------
china <- left_join(china, kevin, by = c("VirusName" = "virusname"))
china$positive <- 0
china[!is.na(china$VirusName), "positive"] <- 1

prev_by_risk <- china %>% # dplyr and magrittr
  group_by(Risklevel) %>%
  summarize(total = sum(positive),
            prevalence = (sum(positive) / nrow(china)) * 100)

prev_by_risk$prevalence <- round(prev_by_risk$prevalence, 2)

knitr::kable(prev_by_risk, col.names = c("Risk level", "Total", "Prevalence"), caption = "Sample prevalence of viruses by risk level")


## ----, results = "asis"--------------------------------------------------
prev_by_animal <- china %>%
  # filter(positive > 0) %>%
  group_by(AnimalID..GAINS.) %>%
  summarize(total = sum(positive)) %>%
  arrange(desc(total))

prev_by_animal <- data.frame(table(prev_by_animal$total))
prev_by_animal$Var1 <- as.numeric(as.character(prev_by_animal$Var1))
prev_by_animal$Perc <- round(prev_by_animal$Freq / sum(prev_by_animal$Freq) * 100, 2)

knitr::kable(prev_by_animal, col.names = c("Number of viruses", "Frequency in animals", "Percentage of animals"), caption = "Frequency of animals with 1, 2, and 3 matched viruses")


## ----, results = "asis"--------------------------------------------------
by_species <- china %>%
  group_by(SpeciesScientificName) %>%
  summarize(highest_risklevel = min(na.omit(Risklevel))) %>%
  arrange(highest_risklevel)

by_species$highest_risklevel[is.infinite(by_species$highest_risklevel)] <- NA

knitr::kable(by_species, col.namse = c("Species", "Any match", "Highest risk level of positive match"), caption = "Summaries of virus matches by species")


## ----, results = "asis"--------------------------------------------------
mean_viruses_by_species <- china %>%
  group_by(AnimalID..GAINS.) %>%
  summarize(num_species = length(unique(na.omit(VirusName))),
            SpeciesScientificName = SpeciesScientificName[1]) %>%
  group_by(SpeciesScientificName) %>%
  summarize(num_tested = length(AnimalID..GAINS.),
            mean_species = sum(num_species)/num_tested) %>%
  arrange(desc(mean_species)) %>%
  filter(mean_species > 0.0)

knitr::kable(mean_viruses_by_species, col.names = c("Species", "Number of animals tested", "Mean virus species"), caption = "Mean number of virus species found per individual for different species")


## ----, results = "asis"--------------------------------------------------
mean_viruses_by_species <- china %>%
  group_by(AnimalID..GAINS.) %>%
  summarize(num_species = length(unique(na.omit(VirusName))),
            SpeciesScientificName = SpeciesScientificName[1]) %>%
  group_by(SpeciesScientificName) %>%
  summarize(num_tested = length(AnimalID..GAINS.),
            mean_species = sum(num_species)/num_tested) %>%
  arrange(desc(mean_species)) %>%
  filter(mean_species > 0.0)

knitr::kable(mean_viruses_by_species, col.names = c("Species", "Number of animals tested", "Mean virus species"), caption = "Mean number of virus species found per individual for different species")

