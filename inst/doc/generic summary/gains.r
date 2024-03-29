
## ----setup, include = FALSE----------------------------------------------
knitr::opts_knit$set(root.dir = "~/Dropbox (EHA)/repositories/gains-summary")
knitr::opts_chunk$set(echo = FALSE, message = FALSE, warning = FALSE)

# Load required packages
library(dplyr)


## ----, results = "asis"--------------------------------------------------
viruses_per_animal <- country_gains %>%
  group_by(AnimalID) %>%
  summarize(viruses_matched = length(unique(na.omit(VirusName)))) %>%
  group_by(viruses_matched) %>%
  summarize(individuals = length(viruses_matched))

viruses_per_animal$percent <- round(viruses_per_animal$individuals / sum(viruses_per_animal$individuals) * 100, 2)

knitr::kable(viruses_per_animal, col.names = c("Viruses per animal", "Number of animals", "Percentage"), caption = "Individual animals by sum of viruses")


## ----, include = FALSE---------------------------------------------------
total_tested <- length(unique(country_gains$AnimalID))
total_positive <- length(unique(country_gains$AnimalID[country_gains$positive > 0]))
total_risk1 <- length(na.omit(unique(country_gains$AnimalID[country_gains$RiskLevel == 1])))


## ----, results = "asis"--------------------------------------------------
animals <- country_gains %>%
  group_by(SpeciesScientificName) %>%
  summarize(SpeciesCommonNameEnglish = unique(SpeciesCommonNameEnglish),
            number = length(unique(AnimalID))) %>%
  arrange(desc(number))

names(animals) <- c("scientific", "common", "number")
animals$common <- sapply(animals$common, function(x) strsplit(x, " within")[[1]][1])

knitr::kable(animals, col.names = c("Scientific name", "Common name", "Number tested"), caption = "Number of individuals tested per animal species")


## ----, results = "asis"--------------------------------------------------
PIG <- country_gains %>%
  group_by(PrimaryInterfaceGroup) %>%
  summarize(number = length(unique(AnimalID))) %>%
  arrange(desc(number))

PIG$percent <- round(PIG$number / sum(PIG$number) * 100, 2)

knitr::kable(PIG, col.names = c("Primary interface group", "Number of individuals", "Percentage"), caption = "Number of individuals tested per primary interface group")


## ----, results = "asis"--------------------------------------------------
SIG <- country_gains %>%
  group_by(SecondaryInterfaceGroup) %>%
  summarize(number = length(unique(AnimalID))) %>%
  arrange(desc(number))

SIG$percent <- round(SIG$number / sum(SIG$number) * 100, 2)

knitr::kable(SIG, col.names = c("Secondary interface group", "Number of individuals", "Percentage"), caption = "Number of individuals tested per secondary interface group")


## ----, results = "asis"--------------------------------------------------
risk1_crosstab <- country_gains %>%
  group_by(AnimalID) %>%
  filter(Positive == 1) %>%
  summarize(risk1 = ifelse(summary(RiskLevel)["High"] > 0, 1, 0),
            viruses_matched = length(unique(na.omit(VirusName)))) %>%
  group_by(viruses_matched) %>%
  summarize(individuals = length(viruses_matched),
            risk1 = sum(risk1))

risk1_crosstab$percent <- round(risk1_crosstab$risk1/risk1_crosstab$individuals * 100, 2)

knitr::kable(risk1_crosstab, col.names = c("Viruses", "Individuals, total", "Individuals, 'high risk'", "Percentage"), caption = "Cross tabs of number of viruses in individuals by presence of risk level 1 virus")


## ----, results = "asis"--------------------------------------------------
risk1_viruses <- country_gains %>%
  filter(RiskLevel == "High") %>%
  group_by(VirusName) %>%
  summarize(animals = length(unique(na.omit(AnimalID)))) %>%
  select(VirusName, animals) %>%
  arrange(desc(animals))

knitr::kable(risk1_viruses, col.names = c("Virus name", "Number of animals"), caption = "'High risk' viruses")


## ----, results = "asis"--------------------------------------------------
risk1_individual <- country_gains %>%
  group_by(AnimalID) %>%
  summarize(scientific = unique(SpeciesScientificName),
            common = unique(SpeciesCommonNameEnglish),
            risk1 = ifelse(summary(RiskLevel)["High"] > 0, 1, 0),
            PIG = unique(PrimaryInterfaceGroup),
            SIG = unique(SecondaryInterfaceGroup))

risk1_species <- risk1_individual %>%
  group_by(scientific) %>%
  summarize(common = unique(common),
            animals = length(unique(AnimalID)),
            risk1 = length(risk1[risk1 == 1]),
            percent = round(risk1 / animals * 100, 2)) %>%
  filter(risk1 > 0) %>%
  arrange(desc(percent))

risk1_species$common <- sapply(risk1_species$common, function(x) strsplit(x, " within")[[1]][1])

knitr::kable(risk1_species, col.names = c("Scientific name", "Common name", "Tested", "'High risk'", "%"), caption = "Number of animals with 'high risk' viruses per species")


## ----, results = "asis"--------------------------------------------------
risk1_PIG <- risk1_individual %>%
  group_by(PIG) %>%
  summarize(animals = length(unique(AnimalID)),
            risk1 = length(risk1[risk1 == 1]),
            percent = round(risk1 / animals * 100, 2)) %>%
  arrange(desc(percent))

knitr::kable(risk1_PIG, col.names = c("Primary interface group", "Tested", "'High risk'", "%"), caption = "Number of animals with 'high risk' viruses per primary interface group")


## ----, results = "asis"--------------------------------------------------
risk1_SIG <- risk1_individual %>%
filter(!(SIG == "")) %>%
  group_by(SIG) %>%
  summarize(animals = length(unique(AnimalID)),
            risk1 = length(risk1[risk1 == 1]),
            percent = round(risk1 / animals * 100, 2)) %>%
  arrange(desc(percent))

knitr::kable(risk1_SIG, col.names = c("Secondary interface group", "Tested", "'High risk'", "%"), caption = "Number of animals with 'high risk' viruses per secondary interface group")

