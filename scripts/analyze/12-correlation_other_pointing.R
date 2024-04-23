#@eceyuksel
#2024-01-23

#set up data directory
os_sep <- .Platform$file.sep
data_dir <- (paste('C:','Users','eceyuksel','Dropbox (UFL)','EceYuksel', 'VR_Compass Mountain', 'final_data', sep = os_sep))
setwd(data_dir)
getwd()

#Read the data------------------

#get the video game play data, merged
data <- read.csv('exp2_final.csv')
library(dplyr)

# Compute Pearson correlation for pointing
other_pointing <- cor.test(data$overall_pointing, data$correct_absolute_error_other, method = "pearson")

# Print the correlation coefficient
print(other_pointing)


# Compute Pearson correlation for model-building
other_model <- cor.test(data$Overall_rsquared, data$correct_absolute_error_other, method = "pearson")

# Print the correlation coefficient
print(other_model)



library(ggplot2)

# Create a scatter plot with the correlation line for Pointing and Compass Use
pointing_other_plot <- ggplot(data, aes(x = correct_absolute_error_other, y = overall_pointing)) +
  geom_point(color = "darkgrey") +
  geom_smooth(method = "lm", se = TRUE, color = "darkgreen", fill = "lightgreen") +  # Add regression line with CI
  labs(
    x = "Cue Pointing Error",
    y = "Overall Pointing Error")+
  theme_minimal()

# Print the plot
print(pointing_other_plot)

# Create a scatter plot with the correlation line for Model-Building and Compass Use
model_other_plot <- ggplot(data, aes(x = correct_absolute_error_other, y = Overall_rsquared)) +
  geom_point(color = "darkgrey") +
  geom_smooth(method = "lm", se = TRUE, color = "darkgreen", fill = "lightgreen") +  # Add regression line with CI
  labs(
    x = "Cue Pointing Error",
    y = expression("Model-Building R"^2))+
  theme_minimal()

# Print the plot
print(model_other_plot)


library(gridExtra)

grid_plots <- grid.arrange(pointing_other_plot, model_other_plot, ncol = 2, nrow = 1)
print(grid_plots)

ggsave("./output/plots/other_pointing_correlation.jpg", plot = grid_plots, width = 10, height = 4, dpi = 300)
