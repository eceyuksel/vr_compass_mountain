#Install/invoke packages
install.packages('tidyverse')
library(tidyverse)
library(reshape)

#Reading the long form data file
longform <- read.csv('G:\\.shortcut-targets-by-id\\1iQQpKU1ZuIM9lonWw4rXjIcMOMTundDC\\SCANN Lab\\EceYuksel\\VR_CompassMountain\\Final_Data\\longform_pointing_Exp1.csv')

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
subset_wideform <- cast(means, participant ~ real_within_between)

#Adding overall pointing error with weighted mean for between and within trials
subset_wideform$overall_pointing <- with(subset_wideform, (between * 4 + within * 3) / 7)

#Save wide form as csv 
write.csv(subset_wideform,
          "G:\\.shortcut-targets-by-id\\1iQQpKU1ZuIM9lonWw4rXjIcMOMTundDC\\SCANN Lab\\EceYuksel\\VR_CompassMountain\\Final_Data\\wideform_pointing_Exp1.csv", row.names = FALSE)




