load_all()

library(dplyr)
library(stringr)


# Define functions

fix_virus_names <- function(virus_names) {
  virus_names <- str_replace_all(virus_names, "_", " ")
  virus_names <- str_replace_all(virus_names, "strain of ", "")
  virus_names <- str_replace_all(virus_names, "^the ", "")
  return(virus_names)
}

#------------------------------#

# Load Kevin's virus risk assessment data frame
kevin <- read.csv("inst/rawdata/KevinP1rank.csv", as.is = TRUE)
kevin <- kevin %>%
  select(VirusFamily = vFamily,
         VirusName = virusname,
         RiskLevel = Risklevel,
         Novel = Novel,
         SimpleVirusName = SimpleVirusName,
         VirusGenus = vGenus,
         AveManualRL = AVE_Manual) %>%
  mutate(VirusName = fix_virus_names(VirusName)) # Fix virus names

#------------------------------#

# Load raw gains data
all_gains_raw <- read.csv("inst/rawdata/GAINS_Extract_11-11-2014 Virus.csv", as.is = TRUE)

all_gains <- all_gains_raw %>%
  select(SiteName, # Pick variables we're interested in.
         EventName,
         EventDate,
         DurationOfEventDays,
         Country,
         StateProv,
         Latitude,
         Longitude,
         HabitatType,
         PrimaryInterface,
         PrimaryInterfaceGroup,
         OtherPrimaryInterface,
         SecondaryInterface,
         SecondaryInterfaceGroup,
         LandscapeConversionGradient,
         AnthropogenicChange,
         DomesticAnimals,
         AnimalID = AnimalID..GAINS.,
         AnimalClassification,
         Taxagroup,
         SpeciesScientificName,
         SpeciesCommonNameEnglish,
         Sex,
         AgeClass,
         ConditionAtCapture,
         VirusName,
         Interpretation,
         TestStatus) %>%
  mutate(VirusName = fix_virus_names(VirusName)) %>% # Fix virus names
  left_join(kevin) %>% # Join kevin
  mutate(RiskLevelInt = RiskLevel, # Make RiskLevel a factor but have backup Int version
         RiskLevel = factor(RiskLevel,
                            levels = c(3, 2, 1),
                            labels = c("Low", "Medium", "High"),
                            ordered = TRUE),
         Positive = as.integer(!is.na(RiskLevel))) %>%
  mutate(SpeciesScientificName = sentence_case(SpeciesScientificName), # Fix species names
         SpeciesCommonNameEnglish = sentence_case(SpeciesCommonNameEnglish))

all_gains[all_gains$VirusName == "", "VirusName"] <- NA

save(all_gains, file = "data/all_gains.RData")