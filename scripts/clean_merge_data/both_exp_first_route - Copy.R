#@eceyuksel
#2023-05-30

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

# Create a data frame with angles and cardinal directions
cardinal_directions <- c("N", "NNE", "NE", "ENE", "E", "ESE", "SE", "SSE",
                         "S", "SSW", "SW", "WSW", "W", "WNW", "NW", "NNW")

only_main_directions <- c('0/360', ' ',  ' ', ' ',  '90' ,' ' ,' ',' ' ,'180', ' ', ' ' ,' ' ,'270', ' ' ,' ', ' ')

#match the direction angles and the naming to see if they match correctly
angle_names <- data.frame(Angle = angles, Direction = cardinal_directions, only_main_directions = only_main_directions)

angle_names <- angle_names %>% mutate(range = Angle + 11.25)
print(angle_names$Angle)


#or LOAD IN CORRECTED ANGLE DF directly ---------------
original <- read.csv('both_exp_final.csv')
#original <- na.omit(original)
#make the data frames for each angle: overall, golledge and golledge so that we run the same spider plot code
#overall angle----------------------
overall_angle <- original[,c("Overall_angle_corrected","first_route","participant")]

#change the naming from Batty and Golledge to Route-A and Route-B
overall_angle$first_route2 <- ifelse(overall_angle$first_route == "Batty", "Route-A", "Route-B")

#rename the first_route to first_route to run the code
overall_angle <- subset(overall_angle, select = -first_route)


colnames(overall_angle) <- c('angle','id',"first_route")

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

#make a angles list to use later to sanity-check angle ranges
angles <- seq(0, 337.5, by = 22.5)
print(angles)

# Create a data frame with angles as rows
overall_angle_range <- overall_angle %>% group_by(first_route) %>% 
  count(angle_range)

#get the observetion number for each angle and spot the NAs to add later to the spider plot as zeros

overall_angle_wide <- spread(overall_angle_range, angle_range, n)

overall_angle_wide[is.na(overall_angle_wide)] <- 0


#reorder the angles to be plotted in the right order
overall_angle_wide = overall_angle_wide %>% select(first_route,'0', '22.5',  '45', '67.5',  '90' ,'112.5' ,'135','157.5' ,'180', '202.5', '225' ,'247.5' ,'270', '292.5' ,'315', '337.5')

#plot both overall angle
both_overall <- ggradar(
  overall_angle_wide, 
  values.radar = c("0", "7", "15"),
  grid.min = 0, grid.mid = 7, grid.max = 15,
  axis.labels = angle_names$only_main_directions,
  plot.extent.x.sf = 1,
  plot.extent.y.sf = 1.15,
  # Polygons
  group.line.width = 1, 
  group.point.size = 1.75,
  group.colours = c("#006400", "#32CD32"),
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

both_overall <- both_overall + 
  labs(title = "Model Angle") + 
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

both_overall

# save the plot as a high dpi jpeg file
ggsave("./plots/spider_plots/spider_first_route_overall.jpg", plot = both_overall, width = 8, height = 8, dpi = 300)



#batty angle----------------------
batty_angle <- original[,c("Batty_angle_corrected","first_route","participant")]
colnames(batty_angle) <- c('angle','first_route',"id")

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
batty_angle_range <- batty_angle %>% group_by(first_route) %>% 
  count(angle_range)

#get the observetion number for each angle and spot the NAs to add later to the spider plot as zeros
batty_angle_wide <- spread(batty_angle_range, angle_range, n)

batty_angle_wide[is.na(batty_angle_wide)] <- 0


#reorder the angles to be plotted in the right order
batty_angle_wide = batty_angle_wide %>% select(first_route,'0', '22.5',  '45', '67.5',  '90' ,'112.5' ,'135','157.5' ,'180', '202.5', '225' ,'247.5' ,'270', '292.5' ,'315', '337.5')

#plot both batty angle
both_batty <- ggradar(
  batty_angle_wide, 
  values.radar = c("0", "7", "15"),
  grid.min = 0, grid.mid = 7, grid.max = 15,
  axis.labels = angle_names$only_main_directions,
  plot.extent.x.sf = 1,
  plot.extent.y.sf = 1.15,
  # Polygons
  group.line.width = 1, 
  group.point.size = 1.75,
  group.colours = c("#006400", "#32CD32"),
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

both_batty <- both_batty + 
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

both_batty

# save the plot as a high dpi jpeg file
ggsave("./plots/spider_plots/spider_both_batty.jpg", plot = both_batty, width = 8, height = 8, dpi = 300)


#golledge_angle ------------------
golledge_angle <- original[,c("Golledge_angle_corrected","first_route","participant")]
colnames(golledge_angle) <- c('angle','first_route',"id")

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
golledge_angle_range <- golledge_angle %>% group_by(first_route) %>% 
  count(angle_range)

#get the observetion number for each angle and spot the NAs to add later to the spider plot as zeros
golledge_angle_wide <- spread(golledge_angle_range, angle_range, n)

golledge_angle_wide[is.na(golledge_angle_wide)] <- 0

# Create a vector of zeros with two rows
zeros <- rep(0, 2)

# add missing observation angles to the data frame to be plotted correctly
golledge_angle_wide <- cbind(golledge_angle_wide, '67.5' = zeros)


#reorder the angles to be plotted in the right order
golledge_angle_wide = golledge_angle_wide %>% select(first_route,'0', '22.5',  '45', '67.5',  '90' ,'112.5' ,'135','157.5' ,'180', '202.5', '225' ,'247.5' ,'270', '292.5' ,'315', '337.5')

#plot both golledge angle
both_golledge <- ggradar(
  golledge_angle_wide, 
  values.radar = c("0", "7", "15"),
  grid.min = 0, grid.mid = 7, grid.max = 15,
  axis.labels = angle_names$only_main_directions,
  plot.extent.x.sf = 1,
  plot.extent.y.sf = 1.15,
  # Polygons
  group.line.width = 1, 
  group.point.size = 1.75,
  group.colours = c("#006400", "#32CD32"),
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

both_golledge <- both_golledge + 
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

both_golledge

# save the plot as a high dpi jpeg file
ggsave("./plots/spider_plots/spider_both_golledge.jpg", plot = both_golledge, width = 8, height = 8, dpi = 300)



#plot those together! ------------------------
# Create a grid of plots
library(gridExtra)
spider_plots_both <- grid.arrange(both_overall, both_batty, both_golledge, ncol = 3, nrow = 1)

ggsave("./plots/spider_plots/both.jpg", plot = spider_plots_both, width = 16, height = 6, dpi = 300)




