#@eceyuksel
#2023-06-07

#set up data directory
os_sep <- .Platform$file.sep
data_dir <- (paste('C:','Users','eceyuksel','Dropbox (UFL)','EceYuksel', 'VR_Compass Mountain', 'final_data', sep = os_sep))
setwd(data_dir)
getwd()

#Read the data------------------
data <- read.csv('both_exp_video.csv')

#get the video game play data
qual1 <- read.csv('Qualtrics_Exp1.csv')
qual2 <- read.csv('Qualtrics_Exp2.csv')

#select the SBSOD columns and participant ID
sub_qual1 <- qual1[-1,c(18,35:49)]
sub_qual2 <- qual2[-1,c(18,35:49)]

#rename the columns

#from here https://hegarty-lab.psych.ucsb.edu/node/226

colnames(sub_qual1) <- c('participant',1:15)
colnames(sub_qual2) <- c('participant',1:15)

#merge the data frames
SBSOD <- rbind(sub_qual1,sub_qual2)

# Define the mapping of Likert scale labels to numerical values
likert_mapping <- c("Strongly disagree" = 1,
                    "Disagree" = 2,
                    "Somewhat disagree" = 3,
                    "Neutral" = 4,
                    "Somewhat agree" = 5,
                    "Agree" = 6,
                    "Strongly agree" = 7)

# Function to convert Likert scale labels to numerical values
likert_to_numeric <- function(response) {
  likert_mapping[response]
}

# Specify the column to exclude from conversion
exclude_column <- "participant"

# Copy the original data frame
SBSOD_numeric <- SBSOD

# Apply the conversion to the relevant columns only
columns_to_convert <- setdiff(names(SBSOD_numeric), exclude_column)
SBSOD_numeric[columns_to_convert] <- lapply(SBSOD_numeric[columns_to_convert], likert_to_numeric)


#make data numeric
S <- c(2:16) 
SBSOD_numeric[ , S] <- apply(SBSOD_numeric[ , S], 2,            # Specify own function within apply
                     function(x) as.numeric(as.character(x)))
#reverse code values
SBSOD_numeric[,c(2, 4, 5, 6, 8, 10, 15)] <- 8-SBSOD_numeric[,c(2, 4, 5, 6, 8, 10, 15)]
#Then compute the average score across the 15 items (using the reverse scores for the positively stated items and the original scores for the other items). The score will be a number between 1 and 7 where 1 means a poor sense of direction and 7 means a good sense of direction. 
#Ethical (E) - Items 6,9,10,16,29,&30
SBSOD_numeric$SBSOD_avg<-rowMeans(SBSOD_numeric[,c(2:16)], na.rm=TRUE)
sub_SBSOD<-SBSOD_numeric[,c(1,17)]

#merge with the latest data set
data_sbsod <- merge(data, sub_SBSOD, by = c("participant"))  
write.csv(data_sbsod, 'data_final_sbsod.csv')

#NAV STRATEGIES QUESTIONNAIRE ----------------------------
#select the SBSOD columns and participant ID
sub_qual1_nav <- qual1[-1,c(18,36:49)]






#age means & ethnicity for Exp1 - only analysis participants
#select columns
data1<-read.csv('merged_data_Exp1.csv')
demog1 <- qual1[-1,c(18,24,33)]
colnames(demog1) <- c('participant','age','ethnicity')

demog11 <- merge(data1, demog1, by = c("participant"))  


library(psych)
install.packages('psych')
demog11$ethnicity<-as.factor(demog11$ethnicity)
demog11$age<-as.numeric(demog11$age)


describe(demog11$age)
summary(demog11$ethnicity)


#age means for all run participants
pilots1<-c(3,8,9,10,11,12)

pilots1 <- subset(demog1, participant != "3")
pilots1 <- subset(pilots1, participant != "8")
pilots1 <- subset(pilots1, participant != "9")
pilots1 <- subset(pilots1, participant != "10")
pilots1 <- subset(pilots1, participant != "11")
pilots1 <- subset(pilots1, participant != "12")
pilots1 <- subset(pilots1, participant != 74)
pilots1 <- subset(pilots1, participant != 75)
pilots1 <- subset(pilots1, participant != 76)
pilots1 <- subset(pilots1, participant != 77)

pilots1 <- subset(pilots1, participant != 0)
pilots1 <- subset(pilots1, participant != '')


pilots1 <- subset(pilots1, participant != 2001)
pilots1 <- subset(pilots1, participant != 2002)
pilots1 <- subset(pilots1, participant != 2003)
pilots1 <- subset(pilots1, participant != 2004)
pilots1 <- subset(pilots1, participant != 2005)
pilots1 <- subset(pilots1, participant != 2006)
pilots1 <- subset(pilots1, participant != 2007)

pilots1$age<-as.numeric(pilots1$age)

describe(pilots1$age)

pilots1$ethnicity<-as.factor(pilots1$ethnicity)

race1<-as.data.frame(summary(pilots1$ethnicity))
colnames(race1) <- c('race')

race1$percent_race<- (race1$race) * 100 / 65
sum(race1$percent_race)






