#Install/invoke packages
install.packages('tidyverse')
library(tidyverse)
library(reshape)
library(dplyr)

#Reading the long form data file
longform <- read.csv('G:\\.shortcut-targets-by-id\\1iQQpKU1ZuIM9lonWw4rXjIcMOMTundDC\\SCANN Lab\\EceYuksel\\VR_CompassMountain\\Final_Data\\longform_pointing_Exp2.csv')

#Reading the pointing data file for mountain template
mountain_north_template <- read.csv('G:\\.shortcut-targets-by-id\\1iQQpKU1ZuIM9lonWw4rXjIcMOMTundDC\\SCANN Lab\\EceYuksel\\VR_CompassMountain\\Final_Data\\mountain_pointing_angle.csv', fileEncoding="UTF-8-BOM")

#Adding a new variable column for within or between routes and other
longform <- longform %>%
  mutate(within_between_only = case_when(
    str_detect(within_between, "within") ~ "within",
    str_detect(within_between, "between") ~ "between",
    str_detect(within_between, "other") ~ "other",
  ))

#Getting "other" pointing data error (mountain and compass north)
other <-longform %>% filter(within_between == 'other')

#Getting the actual directions from mountain_north_template
other_actual_directions <- other %>%
  mutate (actual_direction = case_when(
    str_detect(start_landmark,"Batty House") ~ mountain_north_template$actual_direction[mountain_north_template$start_landmark=="Batty House"],
    str_detect(start_landmark,"Lynch Station") ~ mountain_north_template$actual_direction[mountain_north_template$start_landmark=="Lynch Station"],
    str_detect(start_landmark,"Harris Hall") ~ mountain_north_template$actual_direction[mountain_north_template$start_landmark=="Harris Hall"],
    str_detect(start_landmark,"Harvey House") ~ mountain_north_template$actual_direction[mountain_north_template$start_landmark=="Harvey House"],
    str_detect(start_landmark,"Snow Church") ~ mountain_north_template$actual_direction[mountain_north_template$start_landmark=="Snow Church"],
    str_detect(start_landmark,"Sauer Center") ~ mountain_north_template$actual_direction[mountain_north_template$start_landmark=="Sauer Center"],
    str_detect(start_landmark,"Golledge Hall") ~ mountain_north_template$actual_direction[mountain_north_template$start_landmark=="Golledge Hall"],
    str_detect(start_landmark,"Tobler Museum") ~ mountain_north_template$actual_direction[mountain_north_template$start_landmark=="Tobler Museum"]
  ))

#Calculating absolute error for mountain/north responses
other_actual_directions$correct_absolute_error <- abs(other_actual_directions$participant_response - other_actual_directions$actual_direction)

#Correcting errors bigger than 180
corrected_other_actual_directions <- mutate(other_actual_directions, correct_absolute_error = ifelse(correct_absolute_error > 180, (360 - correct_absolute_error), correct_absolute_error))

#Getting the means for "other" pointing data
other_means <- corrected_other_actual_directions %>% 
  group_by(participant) %>% 
  summarise(correct_absolute_error_other = mean(correct_absolute_error))

#create a data frame using the subset command and column names with only 3 variables
subset_w_b <- subset(longform, select=c("participant", "correct_absolute_error", "within_between_only"))

#getting the means for each participant within/between
means_w_b <- subset_w_b %>% 
  group_by(within_between_only, participant) %>% 
  summarise(correct_absolute_error = mean(correct_absolute_error))

#Long format to wide format
subset_wideform <- cast(means_w_b, participant ~ within_between_only)

#Merge other and between within pointing error means
pointing_error <- merge(subset_wideform,other_means)

#Adding overall pointing error with weighted mean for between and within trials
pointing_error$overall_pointing <- with(pointing_error, (between * 4 + within * 3) / 7)

#Removing NA "other" column
pointing_error_final <- pointing_error[ , -which(names(pointing_error) %in% c("other"))]

#Save wide form as csv 
write.csv(pointing_error_final,
          "G:\\.shortcut-targets-by-id\\1iQQpKU1ZuIM9lonWw4rXjIcMOMTundDC\\SCANN Lab\\EceYuksel\\VR_CompassMountain\\Final_Data\\wideform_pointing_Exp2.csv", row.names = FALSE)




