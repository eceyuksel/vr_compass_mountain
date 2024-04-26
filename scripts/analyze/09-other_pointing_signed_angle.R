

#getting the "global angles"other"pointing trials to see if participants pointed to a consistent direction
#and reanalyzing the other_pointing error for each trial with signed
#error for Exp2

# Load the readr package
library(readr)
install.packages("CircStats")
install.packages("circular")

#read-in the longform pointing trials from exp1 and exp2
data1 <- read.csv("C:/Users/eceyu/Dropbox (UFL)/EceYuksel/VR_CompassMountain/raw_data/processedData_Exp1/pointingData/longform_pointing_Exp1.csv")
data1 <- read_csv("C:/Users/eceyuksel/Dropbox (UFL)/EceYuksel/VR_CompassMountain/raw_data/processedData_Exp1/pointingData/longform_pointing_Exp1.csv")


data2 <- read_csv("C:/Users/eceyu/Dropbox (UFL)/EceYuksel/VR_CompassMountain/raw_data/processedData_Exp2/pointingData/longform_pointing_Exp2.csv")
data2 <- read_csv("C:/Users/eceyuksel/Dropbox (UFL)/EceYuksel/VR_CompassMountain/raw_data/processedData_Exp2/pointingData/longform_pointing_Exp2.csv")

#merge data sets
data <- rbind(data1, data2)

#selecting other pointing trials
other_data <- subset(data, within_between == "other")

#read the actual direction data frame
actual_direction_other_orig <- read_csv('mountain_pointing_angle.csv')

actual_direction_other <- actual_direction_other_orig[c(1,5)]

#calculate the 360 degrees, not the signed angle
actual_direction_other$actual_360_direction <- NA
actual_direction_other$actual_360_direction <- ifelse(actual_direction_other$actual_direction < 0, 
                                                      (360 + actual_direction_other$actual_direction), 
                                                      actual_direction_other$actual_direction)

#calculate the 360 degrees for participant responses too, not the signed angle
other_data$participant_360_response <- ifelse(other_data$participant_response < 0, 
                                              (360 + other_data$participant_response), 
                                              other_data$participant_response)
#remove empty NA columns
other_data <- other_data[-c(1,7,8)]

#merge actual direction to the participant data frame by start landmark
merged <- merge(other_data, actual_direction_other, by = "start_landmark", all.x = TRUE)


#calculate participant's pointing angle relative to the mountains
merged$mountain_0_p_angle0 <- merged$participant_360_response - merged$actual_360_direction

#calculate the 360 mountain degrees, not signed
merged$mountain_0_p_angle360 <- ifelse(merged$mountain_0_p_angle0 < 0, 
                                      (360 + merged$mountain_0_p_angle0), 
                                      merged$mountain_0_p_angle0)


#add the pointing error for each trial
merged$error <- ifelse(abs(merged$participant_360_response - merged$actual_360_direction) > 180, 
                       abs(360 - abs(merged$participant_360_response - merged$actual_360_direction)),
                       abs(merged$participant_360_response - merged$actual_360_direction))

#just getting the 360 columns to sanity check
merged_360_only <- merged[c(1,2,3,7,9,10,11,12)]


write.csv(merged, "mountain_0_p_angles.csv")

# Calculate pointing mean by participant CIRCULAR MEAN -----------
library(CircStats)
library(circular)

#change angles to radians to calculate circular means
merged$radians <- merged$mountain_0_p_angle360 * (pi/180)

#calculate circular means grouped by participants
library(dplyr)
library(circular)
mean_by_participant <- merged %>%
  group_by(participant) %>%
  summarize(circular_mean_mountain_p_direction_radian = circ.mean(radians))

#these means are in radians so convert back to angles
mean_by_participant$circular_mean_mountain_p_direction_degrees <- mean_by_participant$circular_mean_mountain_p_direction_radian * (180/pi)

#add condition to the mean_by_participant 
part_cond <- merged[c("participant","target_landmark")]

#merge the condition and circular means
mean_by_participant2 <- part_cond %>%
  group_by(participant, target_landmark) %>%
  slice(1) %>%
  ungroup()

#add the condition to the mean_by_participant
mean_by_participant3 <- merge(mean_by_participant, mean_by_participant2, by = "participant")

write.csv(mean_by_participant3, "mean_p_mountain_angle.csv")



# Merge the mean values back into the original data frame
merged <- merge(merged, mean_by_participant)

#make the participants mean mountain pointing 360 degrees, getting rid of signed angle
merged$circular_mean_mountain_p_direction_360 <- NA
merged$circular_mean_mountain_p_direction_360 <- ifelse(merged$circular_mean_mountain_p_direction_degrees < 0, 
                                                      (360 + merged$circular_mean_mountain_p_direction_degrees), 
                                                      merged$circular_mean_mountain_p_direction_degrees)


#subtract the each trial from the mean to see the deviation from mean for each trial
merged$trial_deviation_from_p_mean <- merged$mountain_0_p_angle0 - merged$circular_mean_mountain_p_direction_360


merged$trial_deviation_from_p_mean2 <- ifelse((merged$mountain_0_p_angle0 - merged$circular_mean_mountain_p_direction_360) < -180,
                                              merged$mountain_0_p_angle0 - merged$circular_mean_mountain_p_direction_360 + 360, merged$trial_deviation_from_p_mean)

merged$trial_deviation_from_p_mean3 <- ifelse((merged$trial_deviation_from_p_mean2) > 180,
                                              360- merged$trial_deviation_from_p_mean2, merged$trial_deviation_from_p_mean2)

#just getting the 360 columns to sanity check
merged_360_only <- merged[c("participant","target_landmark","mountain_0_p_angle0"
                            ,"error","circular_mean_mountain_p_direction_360","trial_deviation_from_p_mean3")]

#write.csv(merged_360_only, "mountain_0_p_angles.csv")

#get the mean of the deviation to see the range of pointing for each participant, how far they pointed in general from their mountains, 
#the lower the range, the more consistent they were in pointing to the their mountains

#change angles to radians to calculate circular means
merged_360_only$trial_deviation_from_p_mean_radians <- merged$trial_deviation_from_p_mean3 * (pi/180)

#select only relevant columns
mean_deviation_radian <- merged_360_only[c(1,2,6,7)]

#calculate circular means grouped by participants
mean_deviation <- mean_deviation_radian %>%
  group_by(participant) %>%
  summarise(circ.range(trial_deviation_from_p_mean_radians))

#these means are in radians so convert back to angles
mean_deviation$degrees_range <- mean_deviation$range * (180/pi)


write.csv(mean_deviation, "mean_deviation.csv")

#create a mean of everything data frame and add error mean
participant_level_mean_other_pointing <- merge(mean_deviation, mean_by_participant3, by = "participant")
names(participant_level_mean_other_pointing)
participant_level_mean_other_pointing <- participant_level_mean_other_pointing[c("participant", "degrees_range","circular_mean_mountain_p_direction_degrees","target_landmark")]

#add error mean
mean_error <- merged %>%
  group_by(participant) %>%
  summarize(mean_other_pointing_error = mean(error))

#merge that to participant level data frame
participant_level_mean_other_pointing <- merge(mean_error, participant_level_mean_other_pointing, by = "participant")

#save the participant level data frame
write.csv(participant_level_mean_other_pointing, "participant_level_mean_other_pointing.csv", row.names = FALSE)


