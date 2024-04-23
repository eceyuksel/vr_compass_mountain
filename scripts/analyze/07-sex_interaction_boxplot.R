##@eceyuksel
#2023-06-02

#set up data directory
os_sep <- .Platform$file.sep
data_dir <- (paste('C:','Users','eceyuksel','Dropbox (UFL)','EceYuksel', 'VR_Compass Mountain', 'final_data', sep = os_sep))
setwd(data_dir)
getwd()

#Read the data------------------

#get the video game play data, merged
data <- read.csv('both_exp_final.csv')

# Libraries
library(ggplot2)
library(dplyr)

#change the condition names to be consistent with the manuscript
data$condition_all_manu <- ifelse(data$condition_all == "Mountains", "Mountain Range",data$condition_all)

data$sex_manu <- ifelse(data$sex == "woman", "Female", "Male")

# Define the desired order of conditions
desired_order <- c("Control", "Compass", "Mountain Range")

# Reassign the factor levels
data$condition_all_manu <- factor(data$condition_all_manu, levels = desired_order)


#ponting error Plot
plot <- ggplot(data, aes(x = condition_all_manu, y = overall_pointing, fill = sex_manu)) +
  geom_violin(trim = FALSE,width = 0.7, linetype = "11", color = "grey40", alpha = 0, position = position_dodge(width = 0.7)) +
  geom_boxplot(width = 0.43, color = "black", alpha = 0.88, position = position_dodge(width = 0.7)) +
  scale_fill_manual(values = c("#8064A2", "#9BBB59", "#95B3D7", 'white')) +
  labs(x = "Condition",
       y = expression("Pointing Error"),
       fill = "Sex") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0, 100) 

plot <- plot + theme(axis.text.y = element_text(size = 15),
                     axis.title = element_text(size = 15),    
                     legend.position = c(1, .73),
                     legend.justification = c("right", "bottom"),
                     legend.box.just = "right",
                     legend.margin = margin(6, 6, 6, 6),
                     legend.background = element_rect(fill = "transparent", color = NA),  # Make the legend transparent
                     plot.title = element_text(size = 17)) 


plot

ggsave("./bar_plots_/sex_condition_pointing4.jpg", plot = plot, width = 6, height = 4, dpi = 300)


plot2 <- ggplot(data, aes(x = condition_all_manu, y = Overall_rsquared, fill = sex_manu)) +
  geom_violin(trim = FALSE,width = 0.7, linetype = "11", color = "grey40", alpha = 0, position = position_dodge(width = 0.7)) +
  geom_boxplot(width = 0.43, color = "black", alpha = 0.88, position = position_dodge(width = 0.7)) +
  scale_fill_manual(values = c("#8064A2", "#9BBB59", "#95B3D7", 'white')) +
  labs(x = "Condition",
       y = expression("Model-Building R"^2),
       fill = "Sex") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))+
  # Add the following line to set the y-axis limits
  ylim(0, 1) 

plot2 <- plot2 + theme(axis.text.y = element_text(size = 15),
                               axis.title = element_text(size = 15),
                               plot.title = element_text(size = 17),
                               legend.position = "null") 

plot2

ggsave("./bar_plots_/sex_condition_model.jpg", plot = plot2, width = 6, height = 4, dpi = 300)

# Create a grid of plots
library(gridExtra)
grid_plots <- grid.arrange(plot, plot2, ncol = 2, nrow = 1)

ggsave("./bar_plots_/sex_condition.jpg", plot = grid_plots, width = 10, height = 4, dpi = 300)



#FEMALES ONLY-----
female <- subset(data, data$sex == "woman")

#grouping based on mountains or not mountains
female$mountain_group <- ifelse(female$condition_all_manu == "Mountain Range", "Mountain Range", "No Mountains")

write.csv(female, "female_only.csv", row.names = FALSE)

