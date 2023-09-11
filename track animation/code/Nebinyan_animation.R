##Nebinyan animation


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

library(plyr)
library(dplyr)
library(aniMotum)
library(lubridate)
library(argosfilter)
library(viridis)
library(sf)
library(rnaturalearth)
library(plotly)

world_map <- map_data("world") %>%  
  mutate(long = ifelse(long <0, 360-long*-1, long))

##############################################################################

Ptt235405_raw <- read_csv(here::here('tag data', 'OZ', '2022', 'Nebinyan data pull', '235405', "235405-Locations.csv"))

Ptt235405_raw <- Ptt235405_raw %>%
  select(DeployID, Ptt, Instr, Date, Type, Quality, Latitude, Longitude, `Error radius`, `Error Semi-major axis`, `Error Semi-minor axis`, `Error Ellipse orientation`)


Ptt235405_raw <- Ptt235405_raw %>%
  mutate(DateTime_UTC = Date) %>%
  mutate(DateTime_UTC=parse_date_time(DateTime_UTC, "HMS dby")) %>%
  select(-Date) %>%
  mutate(Date = as_date(DateTime_UTC))


#change column names to match Xuelei code
#also convert longitude from 0-180 to 0-360
Ptt235405_raw <- Ptt235405_raw %>% 
  dplyr::rename(id = Ptt,
                lat = Latitude,
                #lon = Longitude,
                lc = Quality,
                date = DateTime_UTC) %>% 
  mutate(lon = ifelse(Longitude <0, 360-Longitude*-1, Longitude))


#remove the poor quality locations -- doesn't really matter if do this here or later, but jsut do it here
Ptt235405_raw <- Ptt235405_raw %>% 
  filter (lc != "Z")



Nebinyan_time_diff_hours_df <- ddply(Ptt235405_raw, ~id, function(d){
  d$time_diff_hours <- NA
  for (i in 2:nrow(d)){
    d$time_diff_hours[i] = as.numeric(difftime(d$date[i], d$date[i-1], units = "hours"))}
  return(d)
})



Nebinyan_trackseg_argos_df <- ddply(Nebinyan_time_diff_hours_df, ~id, function(d){
  ind <- which(d$time_diff_hours > 36)
  d$mark <- 0
  d$mark[ind] <- 1
  d$track_seg <- cumsum(d$mark)
  return(d)
})

# Now create a new id based on track segment
Nebinyan_trackseg_argos_df$track_id <- paste(Nebinyan_trackseg_argos_df$id, "-", Nebinyan_trackseg_argos_df$track_seg, sep="")

table(Nebinyan_trackseg_argos_df$track_id)

length(unique(Nebinyan_trackseg_argos_df$track_id)) #42


# remove short track segs, test n <25 
min_obs <- 25 ## set the number of minimum obs acceptable
Nebinyan_trackseg_argos_df <- Nebinyan_trackseg_argos_df %>% group_by(track_id)
Nebinyan_trackseg_argos_df_filt <- filter(Nebinyan_trackseg_argos_df, n() >= min_obs)

table(Nebinyan_trackseg_argos_df_filt$track_id)
length(unique(Nebinyan_trackseg_argos_df_filt$track_id)) 




#SSM

Nebinyan_ssm_df <- Nebinyan_trackseg_argos_df_filt

#now structure the data frame so it matches the required structure for SSM
#keep but rename error ellipse variables
Nebinyan_ssm_df <- Nebinyan_ssm_df %>% 
  select(track_id, date, lc, lon, lat, `Error Semi-major axis`, `Error Semi-minor axis`, `Error Ellipse orientation`,  time_diff_hours) %>% 
  dplyr::rename(id = track_id, 
                smaj = `Error Semi-major axis`, 
                smin = `Error Semi-minor axis`, 
                eor = `Error Ellipse orientation`)


fit_ssm_12h_model_mp_Nebinyan <- fit_ssm(Nebinyan_ssm_df, vmax=5, model="mp", time.step=12, control = ssm_control(verbose=0))

fit_ssm_12h_model_mp_Nebinyan_p <-  fit_ssm_12h_model_mp_Nebinyan %>% grab(what="p")


fit_ssm_12h_model_mp_Nebinyan_p_v2 <- fit_ssm_12h_model_mp_Nebinyan_p %>% 
  mutate(PTT = id) %>% 
  separate(col=PTT, into=c('PTT', 'leftovers'), sep='-') %>% 
  select(-leftovers) %>% 
  mutate(Year = lubridate::year(date)) %>% 
  mutate(Month = month(date)) 




###animation


fit_ssm_12h_model_mp_Nebinyan_p_v2$PTT <- as.factor(fit_ssm_12h_model_mp_Nebinyan_p_v2$PTT)
fit_ssm_12h_model_mp_Nebinyan_p_v2 <- fit_ssm_12h_model_mp_Nebinyan_p_v2 %>% 
  mutate(date = as_date(date)) 
fit_ssm_12h_model_mp_Nebinyan_p_v2 <- fit_ssm_12h_model_mp_Nebinyan_p_v2 %>% 
  dplyr::rename(whale_ID = PTT) 

fit_ssm_12h_model_mp_Nebinyan_p_v2 <- fit_ssm_12h_model_mp_Nebinyan_p_v2 %>% mutate(
  whale_ID = case_when(
    whale_ID == 235405 ~ "Nebinyan"))



# Plot study site 
mybasemap <- ggplot() +
  geom_polygon(data = world_map, aes(x=long, y=lat, group=group), fill="black") +
  coord_equal() + 
  coord_fixed(xlim=c(90,150), ylim=c(-70,-30)) +
  theme_bw()



# Plot static imagery + points + paths
mymap.paths <- ggplot() + 
  geom_polygon(data = world_map, aes(x=long, y=lat, group=group), fill="black") +
  coord_equal() + 
  coord_fixed(xlim=c(90,150), ylim=c(-70,-30)) +
  theme_bw() +
  geom_point(data = fit_ssm_12h_model_mp_Nebinyan_p_v2, aes(x = lon, y = lat), size=2, colour = "magenta3") +
  geom_path(data = fit_ssm_12h_model_mp_Nebinyan_p_v2, aes(x = lon, y = lat), size=1.1, colour = "magenta3") +
  guides(color = guide_legend(override.aes = list(size = 10)))+
  labs(x = "Longitude", y = "Latitude") +
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
### if don't want date in the animation
#path.animate.plot <- mymap.paths +
#  transition_reveal(along = date) 


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
        nframes = 300)
#then try to use https://clideo.com/editor/video-maker to join pngs into mp4










