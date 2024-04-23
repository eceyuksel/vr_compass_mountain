install.packages('here')
library(here)
here::here()
#let's get the exp1 data'--------------------
data1<-read.csv('merged_data_Exp1.csv')

#do NOT replace NAs to zeros, you will lose data

#add a column for first route
data1$first_route<-0

#manually edit the first route
data1[data1$participant == "4","first_route"] <- "Batty"

data1[data1$participant == '4', "first_route"] <- "Batty"
data1[data1$participant == '5', "first_route"] <- "Batty"
data1[data1$participant == '6', "first_route"] <- "Golledge"
data1[data1$participant == '7', "first_route"] <- "Batty"

data1[data1$participant == 13, "first_route"] <- "Batty"
data1[data1$participant == 14, "first_route"] <- "Batty"
data1[data1$participant == 15, "first_route"] <- "Batty"
data1[data1$participant == 16, "first_route"] <- "Batty"
data1[data1$participant == 17, "first_route"] <- "Golledge"
data1[data1$participant == 19, "first_route"] <- "Golledge"

data1[data1$participant == 20, "first_route"] <- "Golledge"
data1[data1$participant == 22, "first_route"] <- "Golledge"
data1[data1$participant == 23, "first_route"] <- "Golledge"
data1[data1$participant == 24, "first_route"] <- "Golledge"
data1[data1$participant == 26, "first_route"] <- "Golledge"
data1[data1$participant == 28, "first_route"] <- "Batty"
data1[data1$participant == 29, "first_route"] <- "Golledge"

data1[data1$participant == 30, "first_route"] <- "Batty"
data1[data1$participant == 31, "first_route"] <- "Batty"
data1[data1$participant == 32, "first_route"] <- "Batty"

data1[data1$participant == 33, "first_route"] <- "Golledge"
data1[data1$participant == 34, "first_route"] <- "Batty"
data1[data1$participant == 35, "first_route"] <- "Golledge"
data1[data1$participant == 39, "first_route"] <- "Golledge"


data1[data1$participant == 40, "first_route"] <- "Batty"
data1[data1$participant == 41, "first_route"] <- "Golledge"
data1[data1$participant == 43, "first_route"] <- "Batty"
data1[data1$participant == 44, "first_route"] <- "Golledge"

data1[data1$participant == 45, "first_route"] <- "Batty"
data1[data1$participant == 46, "first_route"] <- "Batty"
data1[data1$participant == 47, "first_route"] <- "Golledge"
data1[data1$participant == 48, "first_route"] <- "Batty"
data1[data1$participant == 49, "first_route"] <- "Batty"
data1[data1$participant == 50, "first_route"] <- "Golledge"

data1[data1$participant == 51, "first_route"] <- "Batty"
data1[data1$participant == 52, "first_route"] <- "Batty"
data1[data1$participant == 53, "first_route"] <- "Batty"
data1[data1$participant == 54, "first_route"] <- "Golledge"
data1[data1$participant == 55, "first_route"] <- "Batty"
data1[data1$participant == 56, "first_route"] <- "Batty"
data1[data1$participant == 57, "first_route"] <- "Golledge"
data1[data1$participant == 58, "first_route"] <- "Batty"
data1[data1$participant == 59, "first_route"] <- "Batty"

data1[data1$participant == 60, "first_route"] <- "Golledge"
data1[data1$participant == 61, "first_route"] <- "Golledge"
data1[data1$participant == 62, "first_route"] <- "Batty"
data1[data1$participant == 63, "first_route"] <- "Batty"
data1[data1$participant == 64, "first_route"] <- "Golledge"
data1[data1$participant == 65, "first_route"] <- "Golledge"
data1[data1$participant == 66, "first_route"] <- "Golledge"
data1[data1$participant == 67, "first_route"] <- "Batty"
data1[data1$participant == 68, "first_route"] <- "Batty"
data1[data1$participant == 69, "first_route"] <- "Batty"


data1[data1$participant == 71, "first_route"] <- "Batty"
data1[data1$participant == 72, "first_route"] <- "Batty"
data1[data1$participant == 73, "first_route"] <- "Golledge"


#adding coorected angles
data1$Overall_angle_corrected <- data1$Overall_angle
data1$Batty_angle_corrected <- data1$Batty_angle
data1$Golledge_angle_corrected <- data1$Golledge_angle

data1 <- na.omit(data1)

data1$Overall_angle_corrected[data1$Overall_angle_corrected < 0] <- 360 + data1$Overall_angle_corrected[data1$Overall_angle_corrected < 0]
data1$Batty_angle_corrected[data1$Batty_angle_corrected < 0] <- 360 + data1$Batty_angle_corrected[data1$Batty_angle_corrected < 0]
data1$Golledge_angle_corrected[data1$Golledge_angle_corrected < 0] <- 360 + data1$Golledge_angle_corrected[data1$Golledge_angle_corrected < 0]

#load in Kyle's hand-coded data to check how much it matches with my coding
kyle <- read.csv('first_route_kyle.csv')
kyle2 <- subset(kyle, select=c('participant','first_route'))

data2 <- subset(data1, select=c('participant','first_route'))

data_merge1 <- merge(kyle2, data2, by = c("participant"))  

#perfect macth!!!! :) 

#adding first connecting route 
connecting <- subset(kyle, select=c('participant','first_connecting_route'))

data_connecting <- merge(data1, connecting, by = c("participant"))  

write.csv(data_connecting,'exp1_final.csv', sep = os_sep)

#let's do the same for exp2 data----------------------------
data2<-read.csv('merged_data_Exp2.csv')

#add a column for first route
data2$first_route<-0

#manually edit the first route

data2[data2$participant == '2008', "first_route"] <- "Golledge"
data2[data2$participant == '2011', "first_route"] <- "Golledge"
data2[data2$participant == '2012', "first_route"] <- "Batty"
data2[data2$participant == '2013', "first_route"] <- "Golledge"

data2[data2$participant == 2016, "first_route"] <- "Golledge"
data2[data2$participant == 2017, "first_route"] <- "Golledge"
data2[data2$participant == 2018, "first_route"] <- "Golledge"
data2[data2$participant == 2019, "first_route"] <- "Golledge" 
#left here
data2[data2$participant == 2021, "first_route"] <- "Batty"
data2[data2$participant == 2022, "first_route"] <- "Golledge"

data2[data2$participant == 2023, "first_route"] <- "Golledge"
data2[data2$participant == 2024, "first_route"] <- "Golledge"
data2[data2$participant == 2025, "first_route"] <- "Batty"
data2[data2$participant == 2026, "first_route"] <- "Batty"
data2[data2$participant == 2028, "first_route"] <- "Golledge"
data2[data2$participant == 2029, "first_route"] <- "Batty"

data2[data2$participant == 2030, "first_route"] <- "Golledge"
data2[data2$participant == 2031, "first_route"] <- "Batty"


data2[data2$participant == 2033, "first_route"] <- "Golledge"
data2[data2$participant == 2034, "first_route"] <- "Batty"
data2[data2$participant == 2035, "first_route"] <- "Golledge"
data2[data2$participant == 2036, "first_route"] <- "Batty"
data2[data2$participant == 2037, "first_route"] <- "Batty"
data2[data2$participant == 2038, "first_route"] <- "Golledge"
data2[data2$participant == 2039, "first_route"] <- "Batty"


data2[data2$participant == 2040, "first_route"] <- "Batty"
data2[data2$participant == 2041, "first_route"] <- "Golledge"
data2[data2$participant == 2042, "first_route"] <- "Batty"
data2[data2$participant == 2043, "first_route"] <- "Golledge"


data2[data2$participant == 2045, "first_route"] <- 'Batty'
data2[data2$participant == 2047, "first_route"] <- "Batty"
data2[data2$participant == 2048, "first_route"] <- "Batty"
data2[data2$participant == 2049, "first_route"] <- "Batty"

data2[data2$participant == 2052, "first_route"] <- "Batty"
data2[data2$participant == 2053, "first_route"] <- "Batty"
data2[data2$participant == 2054, "first_route"] <- "Golledge"

data2[data2$participant == 2056, "first_route"] <- "Golledge"
data2[data2$participant == 2057, "first_route"] <- "Batty"
data2[data2$participant == 2058, "first_route"] <- "Batty"
data2[data2$participant == 2059, "first_route"] <- "Batty"

data2[data2$participant == 2060, "first_route"] <- "Batty"
data2[data2$participant == 2061, "first_route"] <- "Batty"
data2[data2$participant == 2062, "first_route"] <- "Golledge"
data2[data2$participant == 2063, "first_route"] <- "Batty"
data2[data2$participant == 2064, "first_route"] <- "Batty"
data2[data2$participant == 2065, "first_route"] <- "Batty"
data2[data2$participant == 2066, "first_route"] <- "Batty"
data2[data2$participant == 2067, "first_route"] <- "Batty"
data2[data2$participant == 2068, "first_route"] <- "Batty"
data2[data2$participant == 2069, "first_route"] <- "Batty"

data2[data2$participant == 2070, "first_route"] <- "Batty"
data2[data2$participant == 2072, "first_route"] <- "Golledge"
data2[data2$participant == 2073, "first_route"] <- "Batty"
data2[data2$participant == 2074, "first_route"] <- "Golledge"
data2[data2$participant == 2075, "first_route"] <- "Batty"
data2[data2$participant == 2076, "first_route"] <- "Batty"
data2[data2$participant == 2077, "first_route"] <- "Golledge"
data2[data2$participant == 2078, "first_route"] <- "Golledge"

data2[data2$participant == 2079, "first_route"] <- "Golledge"
data2[data2$participant == 2080, "first_route"] <- "Golledge"
data2[data2$participant == 2083, "first_route"] <- "Batty"



data2[data2$participant == 2014, "first_route"] <- "Batty"
data2[data2$participant == 2015, "first_route"] <- "Golledge"
data2[data2$participant == 2027, "first_route"] <- "Batty"
data2[data2$participant == 2044, "first_route"] <- "Batty"
data2[data2$participant == 2046, "first_route"] <- "Golledge"
data2[data2$participant == 2084, "first_route"] <- "Golledge"

#adding coorected angles
data2$Overall_angle_corrected <- data2$Overall_angle
data2$Batty_angle_corrected <- data2$Batty_angle
data2$Golledge_angle_corrected <- data2$Golledge_angle

data2 <- na.omit(data2)

data2$Overall_angle_corrected[data2$Overall_angle_corrected < 0] <- 360 + data2$Overall_angle_corrected[data2$Overall_angle_corrected < 0]
data2$Batty_angle_corrected[data2$Batty_angle_corrected < 0] <- 360 + data2$Batty_angle_corrected[data2$Batty_angle_corrected < 0]
data2$Golledge_angle_corrected[data2$Golledge_angle_corrected < 0] <- 360 + data2$Golledge_angle_corrected[data2$Golledge_angle_corrected < 0]

#check how much they match with Kyle's coding
kyle3 <- subset(kyle, select=c('participant','first_route'))

data3 <- subset(data2, select=c('participant','first_route'))

data_merge2 <- merge(kyle3, data3, by = c("participant"))  

#only participant 2019 is different, changing my coding as Kyle's look correct

#adding first connecting route 
connecting2 <- subset(kyle, select=c('participant','first_connecting_route'))

data_connecting2 <- merge(data2, connecting2, by = c("participant"))  


write.csv(data_connecting2,'exp2_final.csv', sep = os_sep)

#merge both experiements ------------------------------------
data1 <- read.csv('exp1_final.csv')
data2 <- read.csv('exp2_final.csv')

#adding a column to match column number to exp1 for cue direction pointing error
data1$correct_absolute_error_other<-NA

colnames(data1)[colnames(data1) == "Compass_condition"] ="Condition"



both_exp_final <- rbind(data1, data2)
both_exp_final$condition_all<-NA

both_exp_final[both_exp_final$Condition == 'Yes', "condition_all"] <- "Compass"
both_exp_final[both_exp_final$Condition == 'No', "condition_all"] <- "Control"
both_exp_final[both_exp_final$Condition == 'Mountains', "condition_all"] <- "Mountains"
both_exp_final[both_exp_final$Condition == 'Compass', "condition_all"] <- "Compass"


library(dplyr)

both_exp_final <- both_exp_final %>% 
  mutate(exp= if_else(.$participant < 2008, 'Exp1', 'Exp2'))

#saving output
write.csv(both_exp_final,'both_exp_final.csv', sep = os_sep)


