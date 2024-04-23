#Reading the Qualtrics Data
all <- read.csv('Mountain_VR_Treadmill.csv')
analysis <- read.csv("merged_data_final.csv")


install.packages('tidyverse')
library(tidyverse)

all <- all %>% 
  rename(participant = Study_Participant_ID)

all <- all %>% 
  rename(game = Q124)

#Selecting columns for age
age <- all[,c(18,24)]

#Rename the columns 
age <- age %>% 
  rename(age = Q9)


total <- merge(age,analysis,by="participant")

print(rowMeans(total$age))  

#Selecting SBSOD
SBSOD <- all[,c(18,35:49)]
names(SBSOD)

library(data.table)
long <- melt(setDT(wide), measure.vars = 3:7, variable.name = "participant")

install.packages('reshape2')
library(reshape2)
long <- melt(SBSOD, id.vars = c("participant"))

#Current video game play
game <- all[,c(18,70)]
total <- merge(game,analysis,by="participant")

#Save merged_data_final as csv 
write.csv(total,
          "C:\\Users\\eceyu\\Desktop\\SCANN_Lab\\02_VR_Project_Mountains\\mergedCSV\\merged_data_game.csv", row.names = FALSE)
