#@eceyuksel
#2023-06-01

#set up data directory
os_sep <- .Platform$file.sep
data_dir <- (paste('C:','Users','eceyuksel','Dropbox (UFL)','EceYuksel', 'VR_Compass Mountain', 'final_data', sep = os_sep))
setwd(data_dir)
getwd()

#Read the data------------------

#get the video game play data, merged
data <- read.csv('both_exp_video.csv')

# Libraries
library(ggplot2)
library(dplyr)

#change the condition names to be consistent with the manuscript
data$condition_all_manu <- ifelse(data$condition_all == "Mountains", "Mountain Range",data$condition_all)

# Define the desired order of conditions
desired_order <- c("Control", "Compass", "Mountain Range")

# Reassign the factor levels
data$condition_all_manu <- factor(data$condition_all_manu, levels = desired_order)



#ponting error Plot
plot <- ggplot(data, aes(x = Video_Game_Play, y = overall_pointing, fill = condition_all_manu)) +
  geom_violin(trim = FALSE,width = 0.7, linetype = "11", color = "grey40", alpha = 0, position = position_dodge(width = 0.7)) +
  geom_boxplot(width = 0.43, color = "black", alpha = 0.88, position = position_dodge(width = 0.7)) +
  scale_fill_manual(values = c("#376092", "#ED7D31", "#95B3D7", 'white')) +
  labs(x = "Video Game Play",
       y = expression("Pointing Error"),
       fill = "Condition") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

plot <- plot + theme(axis.text.y = element_text(size = 15),
                     axis.title = element_text(size = 15),    
                     legend.position = c(1, 0.68),
                     legend.justification = c("right", "bottom"),
                     legend.box.just = "right",
                     legend.margin = margin(6, 6, 6, 6),
                     legend.background = element_rect(fill = "transparent", color = NA),  # Make the legend transparent
                     plot.title = element_text(size = 17)) +
  ylim(0, 100) 
  


plot
ggsave("./bar_plots_/video_condition_pointing4.jpg", plot = plot, width = 6, height = 4, dpi = 300)

#model building Plot
plot2 <- ggplot(data, aes(x = Video_Game_Play, y = Overall_rsquared, fill = condition_all_manu)) +
  geom_violin(trim = FALSE, width = 0.7, linetype = "11", color = "grey40", alpha = 0, position = position_dodge(width = 0.7)) +
  geom_boxplot(width = 0.43, color = "black", alpha = 0.88, position = position_dodge(width = 0.7)) +
  scale_fill_manual(values = c("#376092", "#ED7D31", "#95B3D7", 'white')) +
  labs(x = "Video Game Play",
       y = expression("Model-Building R"^2),
       fill = "Condition") +
  theme(legend.position = "left") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0, 1) 

plot2 <- plot2 + theme(axis.text.y = element_text(size = 15),
                       axis.title = element_text(size = 15),
                       plot.title = element_text(size = 17),
                       legend.position = "null") 

plot2




ggsave("./bar_plots_/video_condition_model.jpg", plot = plot2, width = 6, height = 4, dpi = 300)

# Create a grid of plots
library(gridExtra)
grid_plots <- grid.arrange(plot, plot2, ncol = 2, nrow = 1)

ggsave("./output/plots/video_condition.jpg", plot = grid_plots, width = 11, height = 4, dpi = 300)


