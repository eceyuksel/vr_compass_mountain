#calculating mean ages
#read in the data

data1 <- read.csv("merged_data_Exp1.csv")
data2 <- read.csv("merged_data_Exp2.csv")

q2 <- read.csv("Qualtrics_Exp2.csv")

#Selecting columns for sex and participant ID and age
age2 <- subset(q2, select=c('Study_Participant_ID','What.is.your.age.'))

names(age2)[names(age2) == "Study_Participant_ID"] = "participant"


merged2 <- merge(data2, age2, by = "participant")

# Calculate mean and standard deviation
merged2$What.is.your.age. <- as.numeric(merged2$What.is.your.age.)

mean_value2 <- mean(merged2$What.is.your.age.)
sd_value2 <- sd(merged2$What.is.your.age.)
