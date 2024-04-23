library(dplyr)

#Reading in the data frames
exp1data <- read.csv('corrected_angle_merged_data_Exp1.csv')
exp2data <- read.csv('corrected_angle_merged_data_Exp2.csv')

#Subsetting columns we do not need
exp2data <- subset(exp2data, select = -c(correct_absolute_error_other, First_Building_in_model.building) )
exp1data <- subset(exp1data, select = -c(Compass_condition, First_Building_in_model.building) )

#Merging data frames
merged_data_experiments <- rbind(exp1data,exp2data)

#Tagging exp number
merged_data_experiments <- merged_data_experiments %>% 
  mutate(exp= if_else(.$participant < 2008, 'Exp1', 'Exp2'))

#saving output
write.csv(merged_data_experiments,('merged_data_experiments.csv'), row.names = FALSE)
