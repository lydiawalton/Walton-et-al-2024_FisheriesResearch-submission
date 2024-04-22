################################################################
# Title: Comparing otolith and genetic identification methods
# Author: Lydia Walton
# Last updated: 12-Apr-2024
################################################################

dev.off() #Shut down open graphics devices
rm(list = ls(all=T)) #Clear environment

#Setting your working directory
library(here) 
#Data manipulation and analysis
library(dplyr)
library(geomorph)
library(quadcleanR)
#Data visualization
library(kableExtra)

#Import csv files
genetics <- read.csv("Data/Myct_geneticIDs.csv") #Genetic identifications
visual <- read.csv("Data/Myct_otolithIDs_comparison.csv") #visual identifications (specimen + otolith morphology)


#----------Identification methods comparison

#Merge the two csv's together so you can compare the ID's (removes samples that don't have data for both ID methods)
mergedID <- merge(visual, genetics, by="Sample.ID")

#Remove the barcode non-compliant and flagged samples
mergedID.filtered <- mergedID %>% 
  filter(Remove.these.samples == "N")

#Now compare the Identification column (genetics) to the Otolith.ID column (otoliths)
mergedID.subset <- mergedID.filtered %>% 
  subset(select=c(Sample.ID, Otolith.ID, Collection.Date, Identification))

#Rename the genetics column
mergedID.subset <- mergedID.subset %>% 
  rename("Genetic.ID" = "Identification")

#Compare if the answers in the Otolith.ID column match the answers in the Genetic.ID column
mergedID.comparison <- mergedID.subset %>% 
  mutate(Final_ID = if_else(Otolith.ID == Genetic.ID, "Match", "Mismatch"))

#Create a table showing the ID comparison
(mergedID.table <- mergedID.comparison %>% 
    group_by(Final_ID) %>% 
    summarise("Otolith ID" = length(Final_ID)))

#We have 214 Visual IDs that match the Genetic IDs and 26 that do not (240 total)


#Make a table showing which species were mismatched
(Mismatch.table <- mergedID.comparison %>% 
    group_by(Otolith.ID, Genetic.ID, Final_ID) %>% 
    filter(Final_ID == "Mismatch") %>%   
    summarise("Number of IDs" = length(Final_ID)))

#Make a table with sample size
##Genetics ------------------------------------------------------------------
sample.size.table.genetics <- mergedID.comparison %>% 
   group_by(Genetic.ID) %>% 
   summarise("Sample size" = length(Sample.ID))

##Visual (used merged.ID dataframe to keep all possible otoliths (even ones flagged by DNA barcoding))
sample.size.table.oto <- mergedID.comparison %>% 
    group_by(Otolith.ID) %>% 
    summarise("Sample size" = length(Sample.ID))




