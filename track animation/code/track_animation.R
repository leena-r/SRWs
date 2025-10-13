### track animation ###


## test animation OZ 2022 tracks ##

##############################################################################

library(sp)
library(tidyverse)
library(gganimate)
library(ggmap)
library(magrittr)
library(here)
library(lubridate)
library(gifski)
library(png)
library(dplyr)
library(RColorBrewer) 

world_map <- map_data("world") %>%  
  mutate(long = ifelse(long <0, 360-long*-1, long))

##############################################################################

#instructions: https://jamesepaterson.github.io/jamespatersonblog/2020-01-02_animatingtrackingdata.html


# Read the csv file to animate tracks from
# use SSM version of OZ tracks  as they are tidier 
#file has been edited a bit, add column for PTT
OZ_2022_ssm <- read_csv(here::here('track animation', "data", "ssm_mpm_OZ_SRW_2022_20231116.csv"))
OZ_2022_ssm <- as.data.frame(OZ_2022_ssm)

## not included in updated file
# OZ_2022_ssm <- OZ_2022_ssm %>% #track too short so only adds confusion
#   filter(PTT != 235622)
# OZ_2022_ssm <- OZ_2022_ssm %>% ##kate requested dropping of this whale (Bibbul)
#   filter(PTT != 235411)

OZ_2022_ssm$PTT <- as.factor(OZ_2022_ssm$PTT)
OZ_2022_ssm <- OZ_2022_ssm %>% 
  mutate(date = as_date(date)) 
OZ_2022_ssm <- OZ_2022_ssm %>% 
  dplyr::rename(whale_ID = PTT) 

OZ_2022_ssm <- OZ_2022_ssm %>% mutate(
  whale_ID = case_when(
    whale_ID == 235405 ~ "Nebinyan",
    whale_ID == 235407 ~ "Yilberup",
    whale_ID == 235410 ~ "Augusta whale 1",
    whale_ID == 235413 ~ "Augusta whale 2",
    whale_ID == 235414 ~ "Wandinyil-mirnong",
    whale_ID == 235621 ~ "Busselton Whale 1"))




########## 2023 data ############
OZ_2023_ssm <- read_csv(here::here('track animation', "data", "ssm_mpm_OZ_SRW_2023_20231116.csv"))
OZ_2023_ssm <- as.data.frame(OZ_2023_ssm)

OZ_2023_ssm$PTT <- as.factor(OZ_2023_ssm$PTT)
OZ_2023_ssm <- OZ_2023_ssm %>% 
  mutate(date = as_date(date)) 
OZ_2023_ssm <- OZ_2023_ssm %>% 
  dplyr::rename(whale_ID = PTT) 

OZ_2023_ssm <- OZ_2023_ssm %>% mutate(
  whale_ID = case_when(
    whale_ID == 235408 ~ "Moolyup",
    whale_ID == 235409 ~ "Tyiurtj",
    whale_ID == 235411 ~ "Yookily",
    whale_ID == 235412 ~ "Twertup",
    whale_ID == 245751 ~ "Naaranyirup",
    whale_ID == 245752 ~ "Norngerin",
    whale_ID == 245754 ~ "Merningup"))


### join 2022 and 2023
OZ_2022_2023_ssm <- rbind(OZ_2022_ssm,OZ_2023_ssm)




## make whale ID levels so that 2022 data are first and then 2023 data

OZ_2022_2023_ssm <- OZ_2022_2023_ssm %>% mutate(
  whale_ID = factor(whale_ID, levels = c("Nebinyan", "Yilberup", "Augusta whale 1", "Augusta whale 2",
                               "Wandinyil-mirnong" , "Busselton Whale 1", "Moolyup", "Tyiurtj", "Yookily", 
                               "Twertup", "Naaranyirup", "Norngerin", "Merningup"))) 



# Plot study site 
mybasemap <- ggplot() +
  geom_polygon(data = world_map, aes(x=long, y=lat, group=group), fill="black") +
  coord_equal() + 
  coord_fixed(xlim=c(40,150), ylim=c(-70,-30)) +
  theme_bw()



# Plot static imagery + points + paths
mymap.paths <- ggplot() + 
  geom_polygon(data = world_map, aes(x=long, y=lat, group=group), fill="black") +
  coord_equal() + 
  coord_fixed(xlim=c(40,150), ylim=c(-70,-30)) +
  theme_bw() +
  geom_point(data = OZ_2022_2023_ssm, aes(x = lon, y = lat, colour = whale_ID), size=2) +
  geom_path(data = OZ_2022_2023_ssm, aes(x = lon, y = lat, colour = whale_ID, group = whale_ID), size=1.1) +
  guides(color = guide_legend(override.aes = list(size = 10)))+
  labs(x = "Longitude", y = "Latitude", 
       tag = "Mirnong Maat - southern right whale research",
       caption = "Animation by L. Riekkola") +
  #scale_colour_manual(name = "whale ID",
  #                    # Adjust the number of values for how many animals you have
  #                    values = c("red", "blue", "purple", "green", "orange"), 
  #                    # Enough breaks for every animal in the data set
  #                    breaks = unique(OZ_2022_ssm$PTT)) + 
  theme(legend.position = "bottom",
        legend.title=element_text(size=25),
        legend.text=element_text(size=25),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title = element_text(size = 20),
        plot.title = element_text(size = 25,margin = margin(10, 0, 0, 0)),
        plot.tag.position = "top",
        plot.tag = element_text(size = 35, vjust = 1),
        plot.caption = element_text(size = 10, face = "italic")
  )


# Static plot
mymap.paths



# Update plot to animate. 
#I used 'transition_reveal' so that the path builds from the beginning to the end. 
#Use 'transition_states' to show only one point at a time
path.animate.plot <- mymap.paths +
  transition_reveal(along = date) + #date_time
  labs(title = 'Date: {frame_along}', size=20)  # Add a label on top to say what date each frame is
##doesn't seem to plot every unique date and time combo... maybe try plotting by date only...?


# To display the animation, use `animate`.
# When using your own data, adjust frames per second (fps) to be as fast or slow as you like.
# Be patient at this stage! It will eventually render in your plotting window
animate(path.animate.plot,
        height = 800, width =1200,
        fps = 10, # frames per second #3
        nframes = 300,  # default is 100 frames
        renderer = gifski_renderer()) ##this is needed for the export to work
##using settings along = date_time, fps = 1, and nframes = 4176 causes rendering to take a really long time, also
#not useful as very little movement in 3h -- better to use day and not date time


anim_save("example.gif")


#save as images instead of gif
animate(path.animate.plot,
        height = 800, width =1200,
        fps = 1, # frames per second 
        nframes = 300)
#then try to use https://clideo.com/editor/video-maker to join pngs into mp4





##############################################################################

#NZ tracks 
#start with 2021

# Read the csv file to animate tracks from
# use SSM version as they are tidier 
#file has been edited a bit, add column for PTT
NZ_2021_ssm <- read_csv(here::here('track animation', "data", "NZ_2021_ssm_mp_normalised in each track_20230713.csv"))
NZ_2021_ssm <- as.data.frame(NZ_2021_ssm)

NZ_2021_ssm$PTT <- as.factor(NZ_2021_ssm$PTT)
NZ_2021_ssm <- NZ_2021_ssm %>% 
  mutate(date = as_date(date_time)) 
NZ_2021_ssm <- NZ_2021_ssm %>% 
  dplyr::rename(whale_ID = PTT) 

NZ_2021_ssm <- NZ_2021_ssm %>%  
  mutate(lon = ifelse(lon <0, 360-lon*-1, lon))

NZ_2021_ssm <- NZ_2021_ssm %>% mutate(
  whale_ID = case_when(
    whale_ID == 46633 ~ "Tekau mā ono",
    whale_ID == 46635 ~ "Tekau mā whā",
    whale_ID == 46950 ~ "Tekau mā rua",
    whale_ID == 46955 ~ "Tekau",
    whale_ID == 212499 ~ "Tekau mā toru",
    whale_ID == 212500 ~ "Tekau mā rima",
    whale_ID == 215258 ~ "Waru",
    whale_ID == 215259 ~ "Ono",
    whale_ID == 215261 ~ "Iwa",
    whale_ID == 215262 ~ "Whitu",
    whale_ID == 215263 ~ "Tekau mā tahi"))

NZ_2021_ssm$whale_ID <- factor(NZ_2021_ssm$whale_ID, 
                               levels=c('Ono', 'Whitu', 'Waru', 'Iwa', 'Tekau', 
                                        'Tekau mā tahi', 'Tekau mā rua', 'Tekau mā toru', 'Tekau mā whā', 
                                        'Tekau mā rima', 'Tekau mā ono'))


# Plot study site 
mybasemap <- ggplot() +
  geom_polygon(data = world_map, aes(x=long, y=lat, group=group), fill="black") +
  coord_equal() + 
  coord_fixed(xlim=c(80,200), ylim=c(-70,-30)) +
  theme_bw()



# Plot static imagery + points + paths
mymap.paths <- ggplot() + 
  geom_polygon(data = world_map, aes(x=long, y=lat, group=group), fill="black") +
  coord_equal() + 
  coord_fixed(xlim=c(80,200), ylim=c(-70,-30)) +
  theme_bw() +
  geom_point(data = NZ_2021_ssm, aes(x = lon, y = lat, colour = whale_ID), size=6) + 
  geom_path(data = NZ_2021_ssm, aes(x = lon, y = lat, colour = whale_ID, group = whale_ID), size=1.1) +
  guides(color = guide_legend(override.aes = list(size = 10)))+
  labs(x = "Longitude", y = "Latitude") +
  #scale_colour_manual(name = "whale ID",
  #                    # Adjust the number of values for how many animals you have
  #                    values = c("red", "blue", "purple", "green", "orange"), 
  #                    # Enough breaks for every animal in the data set
  #                    breaks = unique(OZ_2022_ssm$PTT)) + 
  theme(legend.position = "bottom",
        legend.title=element_text(size=25),
        legend.text=element_text(size=25),
        axis.title = element_text(size = 20),
        plot.title = element_text(size = 30)) 

# Static plot
mymap.paths


# Update plot to animate. 
#I used 'transition_reveal' so that the path builds from the beginning to the end. 
#Use 'transition_states' to show only one point at a time
path.animate.plot <- mymap.paths +
  transition_reveal(along = date) + #date_time
  labs(title = 'Date: {frame_along}', size=20)  # Add a label on top to say what date each frame is
##doesn't seem to plot every unique date and time combo... maybe try plotting by date only...?


# To display the animation, use `animate`.
# When using your own data, adjust frames per second (fps) to be as fast or slow as you like.
# Be patient at this stage! It will eventually render in your plotting window
animate(path.animate.plot,
        height = 800, width =1200,
        fps = 5, # frames per second #3
        nframes = 300,  # default is 100 frames
        renderer = gifski_renderer()) ##this is needed for the export to work
##using settings along = date_time, fps = 1, and nframes = 4176 causes rendering to take a really long time, also
#not useful as very little movement in 3h -- better to use day and not date time


anim_save("example.gif")


#save as images instead of gif
animate(path.animate.plot,
        height = 800, width =1200,
        fps = 1, # frames per second 
        nframes = 300,
        renderer = file_renderer(dir = "animation images",
                                 prefix = "plot2_"))
#if gifski is loaded then automatically renders a gif not pngs
#then try to use https://clideo.com/editor/video-maker to join pngs into mp4



##############################################################################


#NZ tracks 
#start with 2020

# Read the csv file to animate tracks from
# use SSM version as they are tidier 
#file has been edited a bit, add column for PTT
NZ_2020_ssm <- read_csv(here::here('track animation','data', "ssm_mpm_all_NZ_SRW_20231218.csv"))
NZ_2020_ssm <- as.data.frame(NZ_2020_ssm) %>% 
  filter(cohort == "NZ 2020")

NZ_2020_ssm$PTT <- as.factor(NZ_2020_ssm$PTT)
NZ_2020_ssm <- NZ_2020_ssm %>% 
  mutate(date = as_date(date)) 
NZ_2020_ssm <- NZ_2020_ssm %>% 
  dplyr::rename(whale_ID = PTT) 

NZ_2020_ssm <- NZ_2020_ssm %>%  
  mutate(lon = ifelse(lon <0, 360-lon*-1, lon))

NZ_2020_ssm <- NZ_2020_ssm %>% mutate(
  whale_ID = case_when(
    whale_ID == 203571 ~ "Rima",
    whale_ID == 203572 ~ "Toru",
    whale_ID == 203573 ~ "Rua",
    whale_ID == 203574 ~ "Wiremu (Bill)",
    whale_ID == 203575 ~ "Tahi",
    whale_ID == 205015 ~ "Whā"))

NZ_2020_ssm$whale_ID <- factor(NZ_2020_ssm$whale_ID, 
                               levels=c("Tahi", "Rua", "Toru", "Whā", "Rima", "Wiremu (Bill)"))


# Plot study site 
mybasemap <- ggplot() +
  geom_polygon(data = world_map, aes(x=long, y=lat, group=group), fill="black") +
  coord_equal() + 
  coord_fixed(xlim=c(80,200), ylim=c(-70,-30)) +
  theme_bw()



# Plot static imagery + points + paths
mymap.paths <- ggplot() + 
  geom_polygon(data = world_map, aes(x=long, y=lat, group=group), fill="black") +
  coord_equal() + 
  coord_fixed(xlim=c(75,180), ylim=c(-70,-30)) +
  theme_bw() +
  geom_point(data = NZ_2020_ssm, aes(x = lon, y = lat, colour = whale_ID), size=2) + 
  geom_path(data = NZ_2020_ssm, aes(x = lon, y = lat, colour = whale_ID, group = whale_ID), size=1.1) +
  guides(color = guide_legend(override.aes = list(size = 10)))+
  labs(x = "Longitude", y = "Latitude", 
       #tag = "Mirnong Maat - southern right whale research",
       caption = "Animation by L. Riekkola") +
  theme(legend.position = "bottom",
        legend.title=element_text(size=25),
        legend.text=element_text(size=25),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title = element_text(size = 20),
        plot.title = element_text(size = 25,margin = margin(10, 0, 0, 0)),
        plot.tag.position = "top",
        plot.tag = element_text(size = 35, vjust = 1),
        plot.caption = element_text(size = 10, face = "italic")
  ) 

# Static plot
mymap.paths



# Update plot to animate. 
#I used 'transition_reveal' so that the path builds from the beginning to the end. 
#Use 'transition_states' to show only one point at a time
path.animate.plot <- mymap.paths +
  transition_reveal(along = date) + #date_time
  labs(title = 'Date: {frame_along}', size=20)  # Add a label on top to say what date each frame is
##doesn't seem to plot every unique date and time combo... maybe try plotting by date only...?


# To display the animation, use `animate`.
# When using your own data, adjust frames per second (fps) to be as fast or slow as you like.
# Be patient at this stage! It will eventually render in your plotting window
animate(path.animate.plot,
        height = 800, width =1200,
        fps = 10, # frames per second 
        nframes = 300,  # default is 100 frames
        renderer = gifski_renderer()) ##this is needed for the export to work
##using settings along = date_time, fps = 1, and nframes = 4176 causes rendering to take a really long time, also
#not useful as very little movement in 3h -- better to use day and not date time


anim_save("example.gif")


#save as images instead of gif
animate(path.animate.plot,
        height = 800, width =1200,
        fps = 1, # frames per second 
        nframes = 300,
        renderer = file_renderer(dir = "animation images",
                                 prefix = "plot2_"))
#if gifski is loaded then automatically renders a gif not pngs
#then try to use https://clideo.com/editor/video-maker to join pngs into mp4




##############################################################################

#NZ tracks 
#start with 2022

# Read the csv file to animate tracks from
# use SSM version as they are tidier 
#file has been edited a bit, add column for PTT
NZ_2022_ssm <- read_csv(here::here('track animation','data', "NZ_2022_ssm_mp_normalised in each track_20230713.csv"))
NZ_2022_ssm <- as.data.frame(NZ_2022_ssm)

NZ_2022_ssm$PTT <- as.factor(NZ_2022_ssm$PTT)
NZ_2022_ssm <- NZ_2022_ssm %>% 
  mutate(date = as_date(date_time)) 
NZ_2022_ssm <- NZ_2022_ssm %>% 
  dplyr::rename(whale_ID = PTT) 

NZ_2022_ssm <- NZ_2022_ssm %>%  
  mutate(lon = ifelse(lon <0, 360-lon*-1, lon))

NZ_2022_ssm <- NZ_2022_ssm %>% mutate(
  whale_ID = case_when(
    whale_ID == 197853 ~ "Rua tekau mā rua",
    whale_ID == 208742 ~ "Rua tekau mā whā",
    whale_ID == 235399 ~ "Rua tekau mā toru",
    whale_ID == 235400 ~ "Tekau mā whitu (Muzza)",
    whale_ID == 235401 ~ "Rua tekau",
    whale_ID == 235402 ~ "Tekau mā iwa",
    whale_ID == 235403 ~ "Tekau mā waru",
    whale_ID == 235404 ~ "Rua tekau mā tahi"))

NZ_2022_ssm$whale_ID <- factor(NZ_2022_ssm$whale_ID, 
                               levels=c("Tekau mā whitu (Muzza)", "Tekau mā waru", "Tekau mā iwa", "Rua tekau",
                                        "Rua tekau mā tahi", "Rua tekau mā rua", "Rua tekau mā toru", "Rua tekau mā whā"))


# Plot study site 
mybasemap <- ggplot() +
  geom_polygon(data = world_map, aes(x=long, y=lat, group=group), fill="black") +
  coord_equal() + 
  coord_fixed(xlim=c(80,200), ylim=c(-70,-30)) +
  theme_bw()



# Plot static imagery + points + paths
mymap.paths <- ggplot() + 
  geom_polygon(data = world_map, aes(x=long, y=lat, group=group), fill="black") +
  coord_equal() + 
  coord_fixed(xlim=c(80,200), ylim=c(-70,-30)) +
  theme_bw() +
  geom_point(data = NZ_2022_ssm, aes(x = lon, y = lat, colour = whale_ID), size=6) + 
  geom_path(data = NZ_2022_ssm, aes(x = lon, y = lat, colour = whale_ID, group = whale_ID), size=1.1) +
  guides(color = guide_legend(override.aes = list(size = 10),nrow=3, byrow=TRUE))+
  labs(x = "Longitude", y = "Latitude") +
  #scale_colour_manual(name = "whale ID",
  #                    # Adjust the number of values for how many animals you have
  #                    values = c("red", "blue", "purple", "green", "orange"), 
  #                    # Enough breaks for every animal in the data set
  #                    breaks = unique(OZ_2022_ssm$PTT)) + 
  theme(legend.position = "bottom",
        legend.title=element_text(size=25),
        legend.text=element_text(size=25),
        axis.title = element_text(size = 20),
        plot.title = element_text(size = 30)) 

# Static plot
mymap.paths


# Update plot to animate. 
#I used 'transition_reveal' so that the path builds from the beginning to the end. 
#Use 'transition_states' to show only one point at a time
path.animate.plot <- mymap.paths +
  transition_reveal(along = date) + #date_time
  labs(title = 'Date: {frame_along}', size=20)  # Add a label on top to say what date each frame is
##doesn't seem to plot every unique date and time combo... maybe try plotting by date only...?


# To display the animation, use `animate`.
# When using your own data, adjust frames per second (fps) to be as fast or slow as you like.
# Be patient at this stage! It will eventually render in your plotting window
animate(path.animate.plot,
        height = 800, width =1200,
        fps = 5, # frames per second #3
        nframes = 300,  # default is 100 frames
        renderer = gifski_renderer()) ##this is needed for the export to work
##using settings along = date_time, fps = 1, and nframes = 4176 causes rendering to take a really long time, also
#not useful as very little movement in 3h -- better to use day and not date time


anim_save("example.gif")
#can use https://cloudconvert.com/gif-to-mp4 to convert gif to mp4

#save as images instead of gif
animate(path.animate.plot,
        height = 800, width =1200,
        fps = 1, # frames per second 
        nframes = 300,
        renderer = file_renderer(dir = "animation images",
                                 prefix = "plot2_"))
#if gifski is loaded then automatically renders a gif not pngs
#then try to use https://clideo.com/editor/video-maker to join pngs into mp4




#############################################################################################
#############################################################################################

#join all NZ data into one animation

all_NZ_ssm <- read_csv(here::here('track animation','data', "ssm_mpm_all_NZ_SRW_20231218.csv"))

all_NZ_ssm <- all_NZ_ssm %>%
  mutate(date = dmy(date), 
  date_num = as.numeric(date)) %>%  # numeric for animation
  arrange(date)

all_NZ_ssm <- all_NZ_ssm %>% mutate(
  whale_ID = case_when(
    PTT == 203571 ~ "Rima",
    PTT == 203572 ~ "Toru",
    PTT == 203573 ~ "Rua",
    PTT == 203574 ~ "Wiremu (Bill)",
    PTT == 203575 ~ "Tahi",
    PTT == 205015 ~ "Whā",
    PTT == 46633 ~ "Tekau mā ono",
    PTT == 46635 ~ "Tekau mā whā",
    PTT == 46950 ~ "Tekau mā rua",
    PTT == 46955 ~ "Tekau",
    PTT == 212499 ~ "Tekau mā toru",
    PTT == 212500 ~ "Tekau mā rima",
    PTT == 215258 ~ "Waru",
    PTT == 215259 ~ "Ono",
    PTT == 215261 ~ "Iwa",
    PTT == 215262 ~ "Whitu",
    PTT == 215263 ~ "Tekau mā tahi",
    PTT == 197853 ~ "Rua tekau mā rua",
    PTT == 208742 ~ "Rua tekau mā whā",
    PTT == 235399 ~ "Rua tekau mā toru",
    PTT == 235400 ~ "Tekau mā whitu (Muzza)",
    PTT == 235401 ~ "Rua tekau",
    PTT == 235402 ~ "Tekau mā iwa",
    PTT == 235403 ~ "Tekau mā waru",
    PTT == 235404 ~ "Rua tekau mā tahi"))


## make whale ID levels so that 2020 data are first and then 2021 and 2022 data
all_NZ_ssm <- all_NZ_ssm %>% mutate(
  whale_ID = factor(whale_ID, levels = c("Tahi", "Rua", "Toru", "Whā", "Rima", "Wiremu (Bill)",
                                         "Ono", "Whitu", "Waru", "Iwa", "Tekau", "Tekau mā tahi", "Tekau mā rua", "Tekau mā toru", "Tekau mā whā", "Tekau mā rima", "Tekau mā ono",
                                         "Tekau mā whitu (Muzza)", "Tekau mā waru","Tekau mā iwa", "Rua tekau", "Rua tekau mā tahi", "Rua tekau mā rua", "Rua tekau mā toru", "Rua tekau mā whā"))) 


all_NZ_ssm <- all_NZ_ssm %>%  
  mutate(lon = ifelse(lon <0, 360-lon*-1, lon))


# Static basemap layer
# world_map should already be loaded from e.g. map_data("world")
world_map <- map_data("world")

basemap <- geom_polygon(
  data = world_map,
  aes(x = long, y = lat, group = group),
  fill = "black"
)


# Define a vector of 25 bright, distinct colors
bright_colors <- c(
  "red", "blue", "green", "orange", "purple",
  "cyan", "magenta", "yellow", "black", "brown",
  "darkgreen", "darkblue", "darkred", "gold", "pink",
  "limegreen", "navy", "violet", "coral", "turquoise",
  "darkorange", "deeppink", "darkmagenta", "royalblue", "sienna"
)

# Make sure you have 25 colors
length(bright_colors)  # should be 25

# Shuffle colors to avoid similar ones next to each other
set.seed(42)  # for reproducibility
pal_shuffled <- sample(bright_colors)


###static map
legend_plot <- ggplot(all_NZ_ssm, aes(x = lon, y = lat, colour = whale_ID)) +
  geom_point() +
  scale_colour_manual(values = pal_shuffled) +   # Use shuffled colors
  guides(
    colour = guide_legend(
      override.aes = list(size = 3)#,
      #nrow = 3,
      #byrow = TRUE
    )
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 15),
    legend.text = element_text(size = 12)
  )
legend_plot


# ggsave(
#   filename = "all_NZ_tracks_static_for_legend.png",
#   plot = legend_plot,
#   width = 10, height = 6, dpi = 300
# )



# Assign colors manually in ggplot
p <- ggplot() +
  basemap +
  geom_path(data = all_NZ_ssm, aes(x = lon, y = lat, colour = whale_ID, group = whale_ID), linewidth = 1.1) +
  geom_point(data = all_NZ_ssm, aes(x = lon, y = lat, colour = whale_ID, group = whale_ID), size = 2) +
  coord_fixed(xlim = c(80, 200), ylim = c(-70, -30)) +
  scale_colour_manual(values = pal_shuffled) +   # Use shuffled colors
  theme_bw() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 25),
    legend.text = element_text(size = 25),
    axis.title = element_text(size = 20),
    plot.title = element_text(size = 30)
  ) +
  labs(
    x = "Longitude",
    y = "Latitude",
    title = "Date: {frame_along}",
    colour = "Whale ID"
  ) +
  transition_reveal(date)


# Render animation
anim <- animate(
  p,
  height = 800, width = 1200,
  fps = 5,
  nframes = length(unique(all_NZ_ssm$date)),
  renderer = gifski_renderer()
)

anim_save("whale_tracks_v17.gif", animation = anim)

##############################################################################3

## all ox tracks joined, same animation style as the nZ animation

#OZ_2022_2023_ssm  <- OZ_2022_2023_ssm  %>%
 # arrange(date)

# Static basemap layer
# world_map should already be loaded from e.g. map_data("world")
world_map <- map_data("world")

basemap <- geom_polygon(
  data = world_map,
  aes(x = long, y = lat, group = group),
  fill = "black"
)


# Define a vector of 13 bright, distinct colors
bright_colors <- c(
  "red", "blue", "green", "orange", "purple",
  "cyan", "magenta", "yellow", "black", "brown",
  "darkgreen", "darkblue", "darkred"
)

# Make sure you have 13 colors
length(bright_colors)  # should be 13

# Shuffle colors to avoid similar ones next to each other
set.seed(42)  # for reproducibility
pal_shuffled <- sample(bright_colors)


###static map
legend_plot_OZ <- ggplot(OZ_2022_2023_ssm, aes(x = lon, y = lat, colour = whale_ID)) +
  geom_point() +
  scale_colour_manual(values = pal_shuffled) +   # Use shuffled colors
  guides(
    colour = guide_legend(
      override.aes = list(size = 3)#,
      #nrow = 3,
      #byrow = TRUE
    )
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 15),
    legend.text = element_text(size = 12)
  )
legend_plot_OZ


# ggsave(
#   filename = "all_OZ_tracks_static_for_legend.png",
#   plot = legend_plot_OZ,
#   width = 10, height = 6, dpi = 300
# )



# Assign colors manually in ggplot
p <- ggplot() +
  basemap +
  geom_path(data = OZ_2022_2023_ssm, aes(x = lon, y = lat, colour = whale_ID, group = whale_ID), linewidth = 1.1) +
  geom_point(data = OZ_2022_2023_ssm, aes(x = lon, y = lat, colour = whale_ID, group = whale_ID), size = 2) +
  coord_fixed(xlim = c(40,150), ylim = c(-70, -30)) +
  scale_colour_manual(values = pal_shuffled) +   # Use shuffled colors
  theme_bw() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 25),
    legend.text = element_text(size = 25),
    axis.title = element_text(size = 20),
    plot.title = element_text(size = 30)
  ) +
  labs(
    x = "Longitude",
    y = "Latitude",
    title = "Date: {frame_along}",
    colour = "Whale ID"
  ) +
  transition_reveal(date)


# Render animation
anim <- animate(
  p,
  height = 800, width = 1200,
  fps = 5,
  nframes = length(unique(OZ_2022_2023_ssm$date)),
  renderer = gifski_renderer()
)

anim_save("whale_tracks_oZ_v1.gif", animation = anim)
























