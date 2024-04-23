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
model_building <- read.csv(here('silctonBidiOut_Exp2.csv'))
pointing <- read.csv(here('wideform_pointing_Exp2.csv'))
condition <- read.csv(here('Participant_List_Mountains.csv'))
qualtrics <- read.csv(here('Qualtrics_Exp2.csv'))

#Selecting columns from the model-building data
s_model_building <- subset(model_building, select=c("participant", "Overall_rsquared", "Overall_angle",
                                                         "Batty_rsquared", "Batty_angle", "Golledge_rsquared", "Golledge_angle"))

#Merging data sets into one data frame with model building and pointing tasks
pointing_model_building <- merge(pointing, s_model_building)

#Selecting columns from the participant condition
s_condition <- subset(condition, select=c("ID", "Condition", "First_Building_in_model.building", "Pilot"))

#Renaming ID to participant
s_condition <- s_condition %>% rename(participant = ID)

#Selecting columns for sex and participant ID and age
s_qualtrics <- subset(qualtrics, select=c('Study_Participant_ID', 'Q14'))

#Renaming columns
s_qualtrics <- s_qualtrics %>% 
  rename(participant = Study_Participant_ID)

s_qualtrics <- s_qualtrics %>% 
  rename(sex = Q14)

#Merging all data sets into one data frame---------------------------------
merged_data <- merge(pointing_model_building, s_condition)
merged_data <- merge(merged_data, s_qualtrics)

#Excluding subjects
#2050 ve 2051 equipment error
#2014, 2015, 2027, 2044, 2046, 2084 double data
merged_data <- subset(merged_data, participant != "2050")
merged_data <- subset(merged_data, participant != "2051")
merged_data <- subset(merged_data, participant != "2014")
merged_data <- subset(merged_data, participant != "2015")
merged_data <- subset(merged_data, participant != "2027")
merged_data <- subset(merged_data, participant != "2044")
merged_data <- subset(merged_data, participant != "2046")
merged_data <- subset(merged_data, participant != "2084")

#Save merged_data_final as csv 
write.csv(merged_data,(here('merged_data_Exp2.csv')), row.names = FALSE)

