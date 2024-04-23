#Installing/invoking libraries
install.packages(tidyverse)
install.packages('dplyr')
library(tidyverse)
library(reshape)
library(dplyr)

#Run the "here" package to update your working directory (where all your data sets and R file are)
install.packages('here')
library(here)
here::here()

#Loading in the data sets
model_building <- read.csv(here('silctonBidiOut_Exp1.csv'))
pointing <- read.csv(here('wideform_pointing_Exp1.csv'))
condition <- read.csv(here('Participant_List.csv'))
qualtrics <- read.csv(here('Qualtrics_Exp1.csv'))

#Selecting columns from the model-building data
s_model_building <- subset(model_building, select=c("participant", "Overall_rsquared", "Overall_angle",
                                                         "Batty_rsquared", "Batty_angle", "Golledge_rsquared", "Golledge_angle"))

#Merging data sets into one data frame with model building and pointing tasks
pointing_model_building <- merge(pointing, s_model_building)

#Selecting columns from the participant condition
s_condition <- subset(condition, select=c("ID", "Compass_condition", "First_Building_in_model.building", "Pilot"))

#Renaming ID to participant
s_condition <- s_condition %>% rename(participant = ID)

#Selecting columns for sex and participant ID and age
s_qualtrics <- subset(qualtrics, select=c('Study_Participant_ID', 'Q8'))

#Renaming columns
s_qualtrics <- s_qualtrics %>% 
  rename(participant = Study_Participant_ID)

s_qualtrics <- s_qualtrics %>% 
  rename(sex = Q8)

#Getting rid of leading zeros in participant numbers in qualtrics data
s_qualtrics$participant <- as.numeric(s_qualtrics$participant)

#Merging all data sets into one data frame---------------------------------
merged_data <- merge(pointing_model_building, s_condition)
merged_data <- merge(merged_data, s_qualtrics)

#Excluding subjects, if needed
#merged_data_final_sex <- subset(merged_data_final_sex, participant != "replace with participant number")

#Save merged_data_final as csv 
write.csv(merged_data,(here('merged_data_Exp1.csv')), row.names = FALSE)

