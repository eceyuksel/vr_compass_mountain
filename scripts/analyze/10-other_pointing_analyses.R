#@eceyuksel 10/30/2023
#other pointing cue direction analyses

# Load the readr package
library(readr)

#Run the "here" package to update your working directory (where all your data sets and R file are)
install.packages('here')
library(here)
here::here()

#read in the data sets to merge
merged <- read_csv(here("exp2_with_first_route.csv"))

other_pointing <- read_csv(here('participant_level_mean_other_pointing.csv'))

#merge them
merged_other <- merge(merged, other_pointing, by = "participant")



#add first route

#save the csv--------------------------
write.csv(merged_other, "other_pointing_merged.csv", row.names = FALSE)

names(merged_other)

#if the model angle is 142 degrees, that means the participant aligned their map with the mountains (0 mountain angle)
merged_other$model_angle_moutains <- merged_other$Overall_angle - 142

merged_mountain0 <- merged_other[c(1,7,18,19,24)]

#change angles to radians to run a circular correlation
merged_other$circular_mean_mountain_p_direction_radians <- merged_other$circular_mean_mountain_p_direction_degrees * (pi/180)
merged_other$radian_range <- merged_other$degrees_range * (pi/180)
merged_other$model_angle_mountains_radian <- merged_other$model_angle_moutains * (pi/180)

library(circular)

cor.circular(merged_other$model_angle_mountains_radian, merged_other$circular_mean_mountain_p_direction_radians, test=TRUE)

cor.circular(merged_other$radian_range, merged_other$circular_mean_mountain_p_direction_radians, test=TRUE)

circ.cors1(angles1, angles2, rads = FALSE)

# Extract the circular variables
angles1 <- merged_other$model_angle_radian
angles2 <- merged_other$circular_mean_mountain_p_direction_radians

library(ggplot2)


# Create a circular plot with arrows and labels
ggplot() +
  geom_segment(aes(x = cos(angles1), y = sin(angles1),
                   xend = cos(angles2), yend = sin(angles2)),
               arrow = arrow(type = "closed", length = unit(0.2, "inches")),
               color = "blue", size = 0.5) +
  annotate("text", x = c(mean(cos(angles1)), mean(cos(angles2))), 
           y = c(mean(sin(angles1)), mean(sin(angles2))), 
           label = c("Model Angle", "Mean Other Direction"),
           color = "black", size =4, vjust = -0.5) +  # Add labels
  coord_fixed() +
  theme_void()


#plot the spider plots for mountaing 0 angle mean pointing -------------------------------------------
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


# Create a data frame with angles and cardinal directions
cardinal_directions <- c("N", "NNE", "NE", "ENE", "E", "ESE", "SE", "SSE",
                         "S", "SSW", "SW", "WSW", "W", "WNW", "NW", "NNW")

only_main_directions <- c('0/360 - Cue Direction (North/Mountain Range)', ' ',  ' ', ' ',  '90' ,' ' ,' ',' ' ,'180', ' ', ' ' ,' ' ,'270', ' ' ,' ', ' ')

angles <- seq(0, 337.5, by = 22.5)

#match the direction angles and the naming to see if they match correctly
angle_names <- data.frame(Angle = angles, Direction = cardinal_directions, only_main_directions = only_main_directions)

angle_names <- angle_names %>% mutate(range = Angle + 11.25)
print(angle_names$Angle)



#SKIP IF YOU HAVE THE DF BEFORE add the corected angles to the data set------------------
df <- merged_other



# No Nans: 
df <- na.omit(df)

#let's change the condition names to be consistent

df[df$Condition == 'Mountains', "Condition"] <- "Mountain Range"


# Correct the angles for being between 0-360 (some are negative)
df$circular_mean_mountain_p_direction_degrees_360 <- df$circular_mean_mountain_p_direction_degrees


df$circular_mean_mountain_p_direction_degrees_360[df$circular_mean_mountain_p_direction_degrees_360 < 0] <- 360 + df$circular_mean_mountain_p_direction_degrees_360[df$circular_mean_mountain_p_direction_degrees_360 < 0]



#make the data frames for each angle: overall, golledge and golledge so that we run the same spider plot code
#overall angle----------------------
circular_mean_mountain_p_direction <- df[,c("circular_mean_mountain_p_direction_degrees_360","Condition","participant")]
colnames(circular_mean_mountain_p_direction) <- c('angle','Condition',"id")

circular_mean_mountain_p_direction <- circular_mean_mountain_p_direction %>% mutate(angle_range =
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
overall_angle_range <- circular_mean_mountain_p_direction %>% group_by(Condition) %>% 
  count(angle_range)

#get the observetion number for each angle and spot the NAs to add later to the spider plot as zeros
overall_angle_wide <- spread(overall_angle_range, angle_range, n)

overall_angle_wide[is.na(overall_angle_wide)] <- 0

# Create a vector of zeros with two rows
zeros <- rep(0, 2)

# add missing observation angles to the data frame to be plotted correctly
overall_angle_wide <- cbind(overall_angle_wide, '45' = zeros)
overall_angle_wide <- cbind(overall_angle_wide, '67.5' = zeros)
overall_angle_wide <- cbind(overall_angle_wide, '90' = zeros)



#reorder the angles to be plotted in the right order
overall_angle_wide = overall_angle_wide %>% select(Condition,'0', '22.5',  '45', '67.5',  '90' ,'112.5' ,'135','157.5' ,'180', '202.5', '225' ,'247.5' ,'270', '292.5' ,'315', '337.5')

#plot exp2 overall angle
exp2_overall <- ggradar(
  overall_angle_wide, 
  values.radar = c("0", "4", "9"),
  grid.min = 0, grid.mid = 4, grid.max = 9,
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
  labs(title = "Participant Cue Direction Pointing") + 
  theme(
    plot.background = element_rect(fill = "white", color = "white"),
    panel.background = element_rect(fill = "white", color = "white"),
    plot.title = element_text(
      size = 18,
      face = "bold", 
      color = "black",
      hjust = 0.5  # Center the plot title horizontally
    ),
    legend.position = c(0.15,0.15),
    legend.text = element_text(size = 13),
    legend.background = element_blank())  # Remove the legend

exp2_overall

# save the plot as a high dpi jpeg file
ggsave("./plots/spider_plots/spider_exp2_overall.jpg", plot = exp2_overall, width = 8, height = 8, dpi = 300)




















