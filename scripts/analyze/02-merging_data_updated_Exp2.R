#Installing/invoking libraries
install.packages('tidyverse')
install.packages('dplyr')
library(tidyverse)
library(reshape)
library('dplyr')

#Run the "here" package to update your working directory (where all your data sets and R file are)
install.packages('here')
library(here)
here::here()

#Loading in the data sets
model_building <- read.csv(here('silctonBidiOut_Exp2.csv'))
pointing <- read.csv(here('wideform_pointing_Exp2.csv'))
condition <- read.csv(here('Participant_List_Mountains.csv'), fileEncoding="UTF-8-BOM")
qualtrics <- read.csv(here('Qualtrics_Exp2.csv'))

#Selecting columns from the model-building data
s_model_building <- subset(model_building, select=c("participant", "Overall_rsquared", "Overall_angle",
                                                         "Batty_rsquared", "Batty_angle", "Golledge_rsquared", "Golledge_angle"))

#Removing double model-building participants' data (due to experimenter error)
s_model_building <- subset(s_model_building, participant != "2014")
s_model_building <- subset(s_model_building, participant != "2015")
s_model_building <- subset(s_model_building, participant != "2027")
s_model_building <- subset(s_model_building, participant != "2044")
s_model_building <- subset(s_model_building, participant != "2046")
s_model_building <- subset(s_model_building, participant != "2084")

#calculate within-route rsquared
s_model_building$within_route_r2 <- (s_model_building$Batty_rsquared + s_model_building$Golledge_rsquared) / 2

#Merging data sets into one data frame with model building and pointing tasks
pointing_model_building <- merge(pointing, s_model_building)

#Selecting columns from the participant condition
s_condition <- subset(condition, select=c("ID", "Condition", "First_Building_in_model.building", "Pilot"))

#Renaming ID to participant
names(s_condition)[names(s_condition) == "ID"] = "participant"

#Selecting columns for sex and participant ID and age
s_qualtrics <- subset(qualtrics, select=c('Study_Participant_ID', 'Q8'))

#Renaming columns
names(s_qualtrics)[names(s_qualtrics) == "Study_Participant_ID"] = "participant"

names(s_qualtrics)[names(s_qualtrics) == "Q8"] = "sex"

#Getting double participants' pointing data (model-building experimenter error)
double_pointing <- subset(pointing, participant %in% c("2014", "2015", "2027", "2044", "2046", "2084"))

#Creating NA columns for the new data frame with pointing data
Overall_rsquared <- c(NA)
Overall_angle <- c(NA)
Batty_rsquared <- c(NA)
Batty_angle <- c(NA)
Golledge_rsquared <- c(NA)
Golledge_angle <- c(NA)
within_route_r2 <- c(NA)

#Adding the NA columns so that we can rbind the data frame at the end
double_pointing <- data.frame(double_pointing,Overall_rsquared, 
                              Overall_angle, Batty_rsquared, Batty_angle,
                              Golledge_rsquared, Golledge_angle, within_route_r2)


double_pointing <- NA

#Adding other columns from other data sets
double_pointing_merged <- merge(double_pointing,s_condition)
double_pointing_merged <- merge(double_pointing_merged, s_qualtrics)


#Merging all data sets into one data frame---------------------------------
merged_data <- merge(pointing_model_building, s_condition)
merged_data <- merge(merged_data, s_qualtrics)

#Excluding subjects
#2050 and 2051 equipment error
merged_data <- subset(merged_data, participant != "2050")
merged_data <- subset(merged_data, participant != "2051")

#Adding double participants' pointing data
merged_data2 <- rbind(merged_data, double_pointing_merged)

#Save merged_data_final as csv 
write.csv(merged_data2,(here('merged_data_Exp2.csv')), row.names = FALSE)

