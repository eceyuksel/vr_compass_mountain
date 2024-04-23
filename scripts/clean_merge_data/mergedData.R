#Reading the data frames

#Pointing data file
#Reading the pointing data file
longform <- read.csv('longform_pointing_study1_Steve.csv')
names(longform)

install.packages('tidyverse')
library(tidyverse)

#Adding a new variable column for within or between routes
longform <- longform %>%
  add_column(real_within_between = if_else(longform$within_between == 'between', 'between', 'within'))

#create a data frame using the subset command and column names with only 3 variables
subset_longform <- subset(longform, select=c("participant", "correct_absolute_error", "real_within_between"))

#getting the means for each participant within/between
means <- subset_longform %>% 
  group_by(real_within_between, participant) %>% 
  summarise(correct_absolute_error = mean(correct_absolute_error))

#Long format to wide format
library(reshape)
subset_wideform <- cast(means, participant ~ real_within_between)

#Merging other data sets
#----------------------------------

#Read the model-building data
model_building <- read.csv('silctonBidiOutFinal.csv')
names(model_building)

#Selecting columns from the model-building data
subset_model_building <- subset(model_building, select=c("participant", "Overall_rsquared", "Overall_angle"))

#Merging data sets into one data frame with model building and pointing tasks
merged_data <- merge(subset_wideform, subset_model_building)

#Read the compass condition data (participant list)
condition <- read.csv('Participant_List_csv.csv')
names(condition)

#Selecting columns from the participant condition
condition_subset <- subset(condition, select=c("ï..ID", "Compass_condition", "First_Building_in_model.building"))

install.packages('dplyr')
library(dplyr)

library(tidyverse)

#Renaming Sï..ID to participant
condition_subset_renamed <- condition_subset %>% 
  rename(participant = ï..ID)

#Merging all data sets into one dataframe
merged_data_final <- merge(merged_data, condition_subset_renamed)

#Adding sex, reading the data
sex <- read.csv('VR_Treadmill_Qualtrics.csv')

#Selecting columns
sex_subset <- subset(sex, select=c('participant', 'Sex'))

#Merging all data sets into one dataframe---------------------------------
merged_data_final_sex <- merge(merged_data_final, sex_subset)

#Adding overall pointing error
merged_data_final_sex$Overall_pointing <-apply(merged_data_final_sex[,2:3],1,mean)

#Save merged_data_final as csv to the directory
write.csv(merged_data_final_sex,
          "C:\\Users\\eceyu\\OneDrive\\Masaüstü\\UF_Spring_22\\01_SCANN_Lab\\VR_Project\\VR_Study_Data\\mergedCSV\\merged_data_final_sex.csv", row.names = FALSE)



