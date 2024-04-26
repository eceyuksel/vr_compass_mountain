

library(ggplot2)
library(circular)
install.packages("tidyr")
library(tidyr)
library(dplyr)
library(ggradar)
install.packages('ggradar')

install.packages("devtools")

devtools::install_github("ricardo-bion/ggradar", dependencies=TRUE)


#set up data directory
os_sep <- .Platform$file.sep
data_dir <- (paste('C:','Users','eceyuksel','Dropbox (UFL)','EceYuksel', 'VR_CompassMountain', sep = os_sep))
setwd(data_dir)
getwd()

#make a angles list to use later to sanity-check angle ranges
angles <- seq(0, 337.5, by = 22.5)
print(angles)

# Create a data frame with angles and cardinal directions
cardinal_directions <- c("N", "NNE", "NE", "ENE", "E", "ESE", "SE", "SSE",
                         "S", "SSW", "SW", "WSW", "W", "WNW", "NW", "NNW")

only_main_directions <- c('0/360', ' ',  ' ', ' ',  '90' ,' ' ,' ',' ' ,'180', ' ', ' ' ,' ' ,'270', ' ' ,' ', ' ')

#match the direction angles and the naming to see if they match correctly
angle_names <- data.frame(Angle = angles, Direction = cardinal_directions, only_main_directions = only_main_directions)

angle_names <- angle_names %>% mutate(range = Angle + 11.25)
print(angle_names$Angle)



#SKIP IF YOU HAVE THE DF BEFORE add the corected angles to the data set------------------
df <- read.csv('./final_data/merged_data_Exp2.csv')



# No Nans: 
df <- na.omit(df)

#let's change the condition names to be consistent

df[df$Condition == 'Mountains', "Condition"] <- "Mountain Range"


# Correct the angles for being between 0-360 (some are negative)
df$Overall_angle_corrected <- df$Overall_angle
df$Batty_angle_corrected <- df$Batty_angle
df$Golledge_angle_corrected <- df$Golledge_angle

df$Overall_angle_corrected[df$Overall_angle_corrected < 0] <- 360 + df$Overall_angle_corrected[df$Overall_angle_corrected < 0]
df$Batty_angle_corrected[df$Batty_angle_corrected < 0] <- 360 + df$Batty_angle_corrected[df$Batty_angle_corrected < 0]
df$Golledge_angle_corrected[df$Golledge_angle_corrected < 0] <- 360 + df$Golledge_angle_corrected[df$Golledge_angle_corrected < 0]

#Save df for future analysis (if needed)
write.csv(df,('corrected_angle_merged_data_Exp2.csv'), row.names = FALSE)

#or LOAD IN CORRECTED ANGLE DF directly ---------------
original <- read.csv('./corrected_angle_merged_data_Exp2.csv')

#make the data frames for each angle: overall, golledge and golledge so that we run the same spider plot code
#overall angle----------------------
overall_angle <- original[,c("Overall_angle_corrected","Condition","participant")]
colnames(overall_angle) <- c('angle','Condition',"id")

overall_angle <- overall_angle %>% mutate(angle_range =
                                            case_when(angle <= 11.25 ~ "0", 
                                                      angle <= 22.5+11.25 ~ "22.5",
                                                      angle <= 45+11.25 ~ "45",
                                                      angle <= 67.5+11.25 ~ "67.5",
                                                      angle <= 90+11.25 ~ "90",
                                                      angle <= 112.5+11.25 ~ "112.5",
                                                      angle <= 135+11.25 ~ "135",
                                                      angle <= 157.5+11.25 ~ "157.5",
                                                      angle <= 180+11.25 ~ "180", 
                                                      angle <= 202.5+11.25 ~ "202.5",
                                                      angle <= 225+11.25 ~ "225",
                                                      angle <= 247.5+11.25 ~ "247.5",
                                                      angle <= 270+11.25 ~ "270",
                                                      angle <= 292.5+11.25 ~ "292.5",
                                                      angle <= 315+11.25 ~ "315",
                                                      angle <= 337.5+11.25 ~ "337.5",
                                                      angle >= 337.6+11.25 ~ "0"))

# Create a data frame with angles as rows
overall_angle_range <- overall_angle %>% group_by(Condition) %>% 
  count(angle_range)

#get the observetion number for each angle and spot the NAs to add later to the spider plot as zeros
overall_angle_wide <- spread(overall_angle_range, angle_range, n)

overall_angle_wide[is.na(overall_angle_wide)] <- 0

# Create a vector of zeros with two rows
zeros <- rep(0, 2)

# add missing observation angles to the data frame to be plotted correctly
overall_angle_wide <- cbind(overall_angle_wide, '292.5' = zeros)


#reorder the angles to be plotted in the right order
overall_angle_wide = overall_angle_wide %>% select(Condition,'0', '22.5',  '45', '67.5',  '90' ,'112.5' ,'135','157.5' ,'180', '202.5', '225' ,'247.5' ,'270', '292.5' ,'315', '337.5')

#plot exp2 overall angle
exp2_overall <- ggradar(
  overall_angle_wide, 
  values.radar = c("0", "4", "8"),
  grid.min = 0, grid.mid = 4, grid.max = 8,
  axis.labels = angle_names$only_main_directions,
  plot.extent.x.sf = 1,
  plot.extent.y.sf = 1.15,
  # Polygons
  group.line.width = 1, 
  group.point.size = 1.75,
  group.colours = c("#376092", "#95B3D7"),
  grid.label.size = 7,  # Affects the grid annotations (0%, 50%, etc.)
  axis.label.size = 6, # Afftects the names of the variables
  axis.line.colour = 'grey',
  gridline.label.offset = 1,
  # Background and grid lines
  background.circle.colour = "gray95",
  gridline.min.colour = "gray60",
  gridline.mid.colour = "gray60",
  gridline.max.colour = "gray60",
  grid.line.width = 0.3,
  legend.position = "bottom",
  fill = TRUE,
  fill.alpha = 0.23,
  legend.text.size = 20,
)

exp2_overall <- exp2_overall + 
  labs(title = "Overall Angle") + 
  theme(
    plot.background = element_rect(fill = "white", color = "white"),
    panel.background = element_rect(fill = "white", color = "white"),
    plot.title = element_text(
      size = 18,
      face = "bold", 
      color = "black",
      hjust = 0.5  # Center the plot title horizontally
    ),
    legend.position = c(0.12,0.15),
    legend.text = element_text(size = 13),
    legend.background = element_blank())  # Remove the legend

exp2_overall

# save the plot as a high dpi jpeg file
ggsave("./plots/spider_plots/spider_exp2_overall.jpg", plot = exp2_overall, width = 8, height = 8, dpi = 300)



#batty angle----------------------
batty_angle <- original[,c("Batty_angle_corrected","Condition","participant")]
colnames(batty_angle) <- c('angle','Condition',"id")

batty_angle <- batty_angle %>% mutate(angle_range =
                                        case_when(angle <= 11.25 ~ "0", 
                                                  angle <= 22.5+11.25 ~ "22.5",
                                                  angle <= 45+11.25 ~ "45",
                                                  angle <= 67.5+11.25 ~ "67.5",
                                                  angle <= 90+11.25 ~ "90",
                                                  angle <= 112.5+11.25 ~ "112.5",
                                                  angle <= 135+11.25 ~ "135",
                                                  angle <= 157.5+11.25 ~ "157.5",
                                                  angle <= 180+11.25 ~ "180", 
                                                  angle <= 202.5+11.25 ~ "202.5",
                                                  angle <= 225+11.25 ~ "225",
                                                  angle <= 247.5+11.25 ~ "247.5",
                                                  angle <= 270+11.25 ~ "270",
                                                  angle <= 292.5+11.25 ~ "292.5",
                                                  angle <= 315+11.25 ~ "315",
                                                  angle <= 337.5+11.25 ~ "337.5",
                                                  angle >= 337.6+11.25 ~ "0"))

#make a angles list to use later to sanity-check angle ranges
angles <- seq(0, 337.5, by = 22.5)
print(angles)

# Create a data frame with angles as rows
batty_angle_range <- batty_angle %>% group_by(Condition) %>% 
  count(angle_range)

#get the observetion number for each angle and spot the NAs to add later to the spider plot as zeros
batty_angle_wide <- spread(batty_angle_range, angle_range, n)

batty_angle_wide[is.na(batty_angle_wide)] <- 0


#reorder the angles to be plotted in the right order
batty_angle_wide = batty_angle_wide %>% select(Condition,'0', '22.5',  '45', '67.5',  '90' ,'112.5' ,'135','157.5' ,'180', '202.5', '225' ,'247.5' ,'270', '292.5' ,'315', '337.5')

#plot exp2 batty angle
exp2_batty <- ggradar(
  batty_angle_wide, 
  values.radar = c("0", "4", "8"),
  grid.min = 0, grid.mid = 4, grid.max = 8,
  axis.labels = angle_names$only_main_directions,
  plot.extent.x.sf = 1,
  plot.extent.y.sf = 1.15,
  # Polygons
  group.line.width = 1, 
  group.point.size = 1.75,
  group.colours = c("#376092", "#95B3D7"),
  grid.label.size = 7,  # Affects the grid annotations (0%, 50%, etc.)
  axis.label.size = 6, # Afftects the names of the variables
  axis.line.colour = 'grey',
  gridline.label.offset = 1,
  # Background and grid lines
  background.circle.colour = "gray95",
  gridline.min.colour = "gray60",
  gridline.mid.colour = "gray60",
  gridline.max.colour = "gray60",
  grid.line.width = 0.3,
  legend.position = "bottom",
  fill = TRUE,
  fill.alpha = 0.23,
  legend.text.size = 20,
)

exp2_batty <- exp2_batty + 
  labs(title = "Route-A Angle") + 
  theme(
    plot.background = element_rect(fill = "white", color = "white"),
    panel.background = element_rect(fill = "white", color = "white"),
    plot.title = element_text(
      size = 18,
      face = "bold", 
      color = "black",
      hjust = 0.5  # Center the plot title horizontally
    ),legend.position = 'null'
  )

exp2_batty

# save the plot as a high dpi jpeg file
ggsave("./plots/spider_plots/spider_exp2_batty.jpg", plot = exp2_batty, width = 8, height = 8, dpi = 300)


#golledge_angle ------------------
golledge_angle <- original[,c("Golledge_angle_corrected","Condition","participant")]
colnames(golledge_angle) <- c('angle','Condition',"id")

golledge_angle <- golledge_angle %>% mutate(angle_range =
                                              case_when(angle <= 11.25 ~ "0", 
                                                        angle <= 22.5+11.25 ~ "22.5",
                                                        angle <= 45+11.25 ~ "45",
                                                        angle <= 67.5+11.25 ~ "67.5",
                                                        angle <= 90+11.25 ~ "90",
                                                        angle <= 112.5+11.25 ~ "112.5",
                                                        angle <= 135+11.25 ~ "135",
                                                        angle <= 157.5+11.25 ~ "157.5",
                                                        angle <= 180+11.25 ~ "180", 
                                                        angle <= 202.5+11.25 ~ "202.5",
                                                        angle <= 225+11.25 ~ "225",
                                                        angle <= 247.5+11.25 ~ "247.5",
                                                        angle <= 270+11.25 ~ "270",
                                                        angle <= 292.5+11.25 ~ "292.5",
                                                        angle <= 315+11.25 ~ "315",
                                                        angle <= 337.5+11.25 ~ "337.5",
                                                        angle >= 337.6+11.25 ~ "0"))

#make a angles list to use later to sanity-check angle ranges
angles <- seq(0, 337.5, by = 22.5)
print(angles)

# Create a data frame with angles as rows
golledge_angle_range <- golledge_angle %>% group_by(Condition) %>% 
  count(angle_range)

#get the observetion number for each angle and spot the NAs to add later to the spider plot as zeros
golledge_angle_wide <- spread(golledge_angle_range, angle_range, n)

golledge_angle_wide[is.na(golledge_angle_wide)] <- 0

# Create a vector of zeros with two rows
zeros <- rep(0, 2)

# add missing observation angles to the data frame to be plotted correctly
golledge_angle_wide <- cbind(golledge_angle_wide, '67.5' = zeros)

#reorder the angles to be plotted in the right order
golledge_angle_wide = golledge_angle_wide %>% select(Condition,'0', '22.5',  '45', '67.5',  '90' ,'112.5' ,'135','157.5' ,'180', '202.5', '225' ,'247.5' ,'270', '292.5' ,'315', '337.5')

#plot exp2 golledge angle
exp2_golledge <- ggradar(
  golledge_angle_wide, 
  values.radar = c("0", "4", "8"),
  grid.min = 0, grid.mid = 4, grid.max = 8,
  axis.labels = angle_names$only_main_directions,
  plot.extent.x.sf = 1,
  plot.extent.y.sf = 1.15,
  # Polygons
  group.line.width = 1, 
  group.point.size = 1.75,
  group.colours = c("#376092", "#95B3D7"),
  grid.label.size = 7,  # Affects the grid annotations (0%, 50%, etc.)
  axis.label.size = 6, # Afftects the names of the variables
  axis.line.colour = 'grey',
  gridline.label.offset = 1,
  # Background and grid lines
  background.circle.colour = "gray95",
  gridline.min.colour = "gray60",
  gridline.mid.colour = "gray60",
  gridline.max.colour = "gray60",
  grid.line.width = 0.3,
  legend.position = "bottom",
  fill = TRUE,
  fill.alpha = 0.23,
  legend.text.size = 20,
)

exp2_golledge <- exp2_golledge + 
  labs(title = "Route-B Angle") + 
  theme(
    plot.background = element_rect(fill = "white", color = "white"),
    panel.background = element_rect(fill = "white", color = "white"),
    plot.title = element_text(
      size = 18,
      face = "bold", 
      color = "black",
      hjust = 0.5  # Center the plot title horizontally
    ),
    legend.position = 'null',  # Remove the legend
  )

exp2_golledge

# save the plot as a high dpi jpeg file
ggsave("./plots/spider_plots/spider_exp2_golledge.jpg", plot = exp2_golledge, width = 8, height = 8, dpi = 300)



#plot those together! ------------------------
# Create a grid of plots
library(gridExtra)
spider_plots_exp2 <- grid.arrange(exp2_overall, exp2_batty, exp2_golledge, ncol = 3, nrow = 1)

ggsave("./plots/spider_plots/exp2.jpg", plot = spider_plots_exp2, width = 16, height = 6, dpi = 300)



#both experiments
spider_plots <- grid.arrange(exp1_overall, exp1_batty, exp1_golledge, exp2_overall, exp2_batty, exp2_golledge, ncol = 3, nrow = 2)

ggsave("./plots/spider_plots/all.jpg", plot = spider_plots, width = 20, height = 12, dpi = 300)


