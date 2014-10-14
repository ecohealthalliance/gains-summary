
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
viruses_per_animal <- china %>%
  group_by(AnimalID..GAINS.) %>%
  summarize(viruses_matched = length(unique(na.omit(VirusName)))) %>%
  group_by(viruses_matched) %>%
  summarize(individuals = length(viruses_matched))

viruses_per_animal$percent <- round(viruses_per_animal$individuals / sum(viruses_per_animal$individuals) * 100, 2)

knitr::kable(viruses_per_animal, col.names = c("Viruses per animal", "Number of animals", "Percentage"), caption = "Individual animals by sum of viruses")


## ----, include = FALSE---------------------------------------------------
total_tested <- length(unique(china$AnimalID..GAINS.))
total_positive <- length(unique(china$AnimalID..GAINS.[china$positive > 0]))
total_risk1 <- length(unique(china$AnimalID..GAINS.[china$Risklevel == 1]))


## ----, results = "asis"--------------------------------------------------
animals <- china %>%
  group_by(SpeciesScientificName) %>%
  summarize(SpeciesCommonNameEnglish = unique(SpeciesCommonNameEnglish),
            number = length(unique(AnimalID..GAINS.))) %>%
  arrange(desc(number))

names(animals) <- c("scientific", "common", "number")
animals$common <- sapply(animals$common, function(x) strsplit(x, " within")[[1]][1])

knitr::kable(animals, col.names = c("Scientific name", "Common name", "Number tested"), caption = "Number of individuals tested per animal species")


## ----, results = "asis"--------------------------------------------------
PIG <- china %>%
  group_by(PrimaryInterfaceGroup) %>%
  summarize(number = length(unique(AnimalID..GAINS.))) %>%
  arrange(desc(number))

PIG$percent <- round(PIG$number / sum(PIG$number) * 100, 2)

knitr::kable(PIG, col.names = c("Primary interface group", "Number of individuals", "Percentage"), caption = "Number of individuals tested per primary interface group")


## ----, results = "asis"--------------------------------------------------
SIG <- china %>%
  group_by(SecondaryInterfaceGroup) %>%
  summarize(number = length(unique(AnimalID..GAINS.))) %>%
  arrange(desc(number))

SIG$percent <- round(SIG$number / sum(SIG$number) * 100, 2)

knitr::kable(SIG, col.names = c("Secondary interface group", "Number of individuals", "Percentage"), caption = "Number of individuals tested per secondary interface group")


## ----, results = "asis"--------------------------------------------------
china$risk1 <- 0
china$risk1[china$Risklevel == 1] <- 1

risk1_crosstab <- china %>%
  group_by(AnimalID..GAINS.) %>%
  filter(positive == 1) %>%
  summarize(risk1 = ifelse(mean(risk1) > 0, 1, 0),
            viruses_matched = length(unique(na.omit(VirusName)))) %>%
  group_by(viruses_matched) %>%
  summarize(individuals = length(viruses_matched),
            risk1 = sum(risk1))

risk1_crosstab$percent <- round(risk1_crosstab$risk1/risk1_crosstab$individuals * 100, 2)

knitr::kable(risk1_crosstab, col.names = c("Viruses", "Individuals, total", "Individuals, 'high risk'", "Percentage"), caption = "Cross tabs of number of viruses in individuals by presence of risk level 1 virus")


## ----, include = FALSE---------------------------------------------------
riskfish <- risk1_crosstab[, 2:3]
riskfish$other = riskfish$individuals - riskfish$risk1
riskfish$individuals <- NULL
riskfishtest <- fisher.test(riskfish)


## ----, results = "asis"--------------------------------------------------
risk1_individual <- china %>%
  group_by(AnimalID..GAINS.) %>%
  summarize(scientific = unique(SpeciesScientificName),
            common = unique(SpeciesCommonNameEnglish),
            risk1 = ifelse(mean(risk1) > 0, 1, 0),
            PIG = unique(PrimaryInterfaceGroup),
            SIG = unique(SecondaryInterfaceGroup))

risk1_species <- risk1_individual %>%
  group_by(scientific) %>%
  summarize(common = unique(common),
            animals = length(unique(AnimalID..GAINS.)),
            risk1 = length(risk1[risk1 == 1]),
            percent = round(risk1 / animals * 100, 2)) %>%
  filter(risk1 > 0) %>%
  arrange(desc(percent))

risk1_species$common <- sapply(risk1_species$common, function(x) strsplit(x, " within")[[1]][1])

knitr::kable(risk1_species, col.names = c("Scientific name", "Common name", "Tested", "'High risk'", "%"), caption = "Number of animals with 'high risk' viruses per species")


## ----, results = "asis"--------------------------------------------------
risk1_PIG <- risk1_individual %>%
  group_by(PIG) %>%
  summarize(animals = length(unique(AnimalID..GAINS.)),
            risk1 = length(risk1[risk1 == 1]),
            percent = round(risk1 / animals * 100, 2)) %>%
  arrange(desc(percent))

knitr::kable(risk1_PIG, col.names = c("Primary interface group", "Tested", "'High risk'", "%"), caption = "Number of animals with 'high risk' viruses per primary interface group")


## ----, results = "asis"--------------------------------------------------
risk1_SIG <- risk1_individual %>%
filter(!(SIG == "")) %>%
  group_by(SIG) %>%
  summarize(animals = length(unique(AnimalID..GAINS.)),
            risk1 = length(risk1[risk1 == 1]),
            percent = round(risk1 / animals * 100, 2)) %>%
  arrange(desc(percent))

knitr::kable(risk1_SIG, col.names = c("Secondary interface group", "Tested", "'High risk'", "%"), caption = "Number of animals with 'high risk' viruses per secondary interface group")


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

