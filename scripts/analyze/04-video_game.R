

#set up data directory
os_sep <- .Platform$file.sep
data_dir <- (paste('C:','Users','eceyuksel','Dropbox (UFL)','EceYuksel', 'VR_Compass Mountain', 'final_data', sep = os_sep))
setwd(data_dir)
getwd()

#Read the data------------------

#get the video game play data
vid1 <- read.csv('Qualtrics_Exp1.csv')
vid2 <- read.csv('Qualtrics_Exp2.csv')

#select the "do you currently play video games?" column
sub_vid1 <- subset(vid1, select=c('Study_Participant_ID','Do.you.currently.play.video.games.'))
sub_vid2 <- subset(vid2, select=c('Study_Participant_ID','Do.you.currently.play.video.games.'))

#rename the columns
library(dplyr)
sub_vid1 <- sub_vid1 %>% 
  rename("participant" = 'Study_Participant_ID',
         "Video_Game_Play" = "Do.you.currently.play.video.games.")

sub_vid2 <- sub_vid2 %>% 
  rename("participant" = 'Study_Participant_ID',
         "Video_Game_Play" = "Do.you.currently.play.video.games.")

#non-existing answers to video game play question is NO - those participants reported they have NO video game experience to the first question.
sub_vid1$`Video_Game_Play`<-replace(sub_vid1$`Video_Game_Play`,sub_vid1$`Video_Game_Play`=="","No")
sub_vid2$`Video_Game_Play`<-replace(sub_vid2$`Video_Game_Play`,sub_vid2$`Video_Game_Play`=="","No")

#merge with the merged data frame
data <- read.csv('both_exp_final.csv')

video_game <- rbind(sub_vid1,sub_vid2)
data_video <- merge(data, video_game, by = c("participant"))  





#lets look if experiment or control groups differ
data_video[data_video$condition_all == "Compass","group"] <- "Experiment"
data_video[data_video$condition_all == "Mountains","group"] <- "Experiment"
data_video[data_video$condition_all == "Control","group"] <- "Control"


#saving output to merge with final data frame
write.csv(data_video,'both_exp_video.csv', sep = os_sep)


# Define the desired order of conditions
desired_order <- c("Control", "Compass", "Mountain Range")

# Reassign the factor levels
data$condition_all_manu <- factor(data$condition_all_manu, levels = desired_order)

library(ggplot2)
#ponting error Plot
plot <- ggplot(data_video, aes(x = Video_Game_Play, y = overall_pointing, fill = group)) +
  geom_violin(trim = FALSE,width = 0.7, linetype = "11", color = "grey40", alpha = 0, position = position_dodge(width = 0.7)) +
  geom_boxplot(width = 0.43, color = "black", alpha = 0.88, position = position_dodge(width = 0.7)) +
  scale_fill_manual(values = c("#FD872F", "#B3A2C7", "#95B3D7", 'white')) +
  labs(x = "Video Game Play",
       y = expression("Pointing Error"),
       fill = "Group") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0, 100) 

plot <- plot + theme(axis.text.y = element_text(size = 15),
                     axis.title = element_text(size = 15),    
                     legend.position = c(1, .76),
                     legend.justification = c("right", "bottom"),
                     legend.box.just = "right",
                     legend.margin = margin(6, 6, 6, 6),
                     legend.background = element_rect(fill = "transparent", color = NA),  # Make the legend transparent
                     plot.title = element_text(size = 17)) 


plot

ggsave("./bar_plots_/video_game_control_exp_pointing.jpg", plot = plot, width = 6, height = 4, dpi = 300)


#model-building plot
plot2 <- ggplot(data_video, aes(x = Video_Game_Play, y = Overall_rsquared, fill = group)) +
  geom_violin(trim = FALSE,width = 0.7, linetype = "11", color = "grey40", alpha = 0, position = position_dodge(width = 0.7)) +
  geom_boxplot(width = 0.43, color = "black", alpha = 0.88, position = position_dodge(width = 0.7)) +
  scale_fill_manual(values = c("#FD872F", "#B3A2C7", "#95B3D7", 'white')) +
  labs(x = "Video Game Play",
       y = expression("Model-Building R"^2),
       fill = "Group") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0, 1) 

plot2 <- plot2 + theme(axis.text.y = element_text(size = 15),
                     axis.title = element_text(size = 15),    
                     legend.position = "null",
                     legend.justification = c("right", "bottom"),
                     legend.box.just = "right",
                     legend.margin = margin(6, 6, 6, 6),
                     legend.background = element_rect(fill = "transparent", color = NA),  # Make the legend transparent
                     plot.title = element_text(size = 17)) 


plot2

ggsave("./bar_plots_/video_game_control_exp_model.jpg", plot = plot, width = 6, height = 4, dpi = 300)
# Create a grid of plots
library(gridExtra)
grid_plots <- grid.arrange(plot, plot2, ncol = 2, nrow = 1)

ggsave("./bar_plots_/video_game_control_exp_both.jpg", plot = grid_plots, width = 10, height = 4, dpi = 300)
 

