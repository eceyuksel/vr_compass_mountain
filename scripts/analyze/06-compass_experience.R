
#set up data directory
os_sep <- .Platform$file.sep
data_dir <- (paste('C:','Users','eceyuksel','Dropbox (UFL)','EceYuksel', 'VR_Compass Mountain', 'final_data', sep = os_sep))
setwd(data_dir)
getwd()


#Read the data------------------

#get the video game play data, merged
data <- read.csv('exp2_final.csv')
library(dplyr)

#add post-questionnaire qualtrics
compass <- read.csv('post_q_exp2.csv')

#selecting the compass questions
sub_compass <- subset(compass, select=c('Q17', 'Q26_2','Q26_1','Q26_4', 'Q26_3'))

#rename
sub_compass <- sub_compass %>% 
  rename("know_how_use" = "Q26_2",
         "experienced" = "Q26_1",
         "easy_use" = "Q26_4",
         "use_north_daily" = "Q26_3",
         'participant' = 'Q17')

#remove rows that are not numbers
sub_compass <- sub_compass[-c(1,2,3,9,76),]


# Convert list to numeric vector
sub_compass$know_how_use <- as.numeric(sub_compass$know_how_use)
sub_compass$experienced <- as.numeric(sub_compass$experienced)
sub_compass$easy_use <- as.numeric(sub_compass$easy_use)
sub_compass$use_north_daily <- as.numeric(sub_compass$use_north_daily)
sub_compass$participant <- as.numeric(sub_compass$participant)

# Calculate correlation
correlation <- cor(sub_compass)

# Save correlation matrix to a CSV file
write.csv(correlation, file = "correlation_compass.csv", row.names = TRUE)

#calculate the mean for the first three questions
sub_compass2 <- subset(sub_compass, select=c('participant','know_how_use','experienced','easy_use'))

library(dplyr)
sub_compass2 <- sub_compass2 %>% mutate(compass_use = (know_how_use + experienced + easy_use)/3)

#save!
write.csv(sub_compass2,'exp2_compass.csv')

#----------------------------------------------------------

#lets read the merged data set to do the correlations
data <- read.csv("both_exp_video_compass.csv")

# Compute Pearson correlation for pointing
pointing_compass_corr <- cor.test(data$overall_pointing, data$compass_use, method = "pearson")

# Print the correlation coefficient
print(pointing_compass_corr)


# Compute Pearson correlation for model-building
model_compass_corr <- cor.test(data$Overall_rsquared, data$compass_use, method = "pearson")

# Print the correlation coefficient
print(model_compass_corr)



#plot the correlations
# Assuming ggplot2 is installed, if not, install it using install.packages("ggplot2")
library(ggplot2)

# Create a scatter plot with the correlation line for Pointing and Compass Use
pointing_compass_plot <- ggplot(data, aes(x = compass_use, y = overall_pointing)) +
  geom_point(color = "darkgrey") +
  geom_smooth(method = "lm", se = TRUE, color = "darkblue", fill = "lightblue") +  # Add regression line with CI
  labs(
       x = "Compass Use Experience",
       y = "Overall Pointing Error")+
  theme_minimal()

# Print the plot
print(pointing_compass_plot)

# Create a scatter plot with the correlation line for Model-Building and Compass Use
model_compass_plot <- ggplot(data, aes(x = compass_use, y = Overall_rsquared)) +
  geom_point(color = "darkgrey") +
  geom_smooth(method = "lm", se = TRUE, color = "darkblue", fill = "lightblue") +  # Add regression line with CI
  labs(
       x = "Compass Use Experience",
       y = expression("Model-Building R"^2))+
  theme_minimal()

# Print the plot
print(model_compass_plot)


library(gridExtra)

grid_plots <- grid.arrange(pointing_compass_plot, model_compass_plot, ncol = 2, nrow = 1)
print(grid_plots)

ggsave("./output/plots/compass_correlation.jpg", plot = combined_plot, width = 10, height = 4, dpi = 300)

