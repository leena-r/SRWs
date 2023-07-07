#run SSM on NZ tracks


################################

library(plyr)
library(aniMotum)
#library(ggplot2)
library(lubridate)
library(argosfilter)
library(tidyr)
#library(dplyr)
library(viridis)
library(sf)
library(tictoc)
library(rnaturalearth)
library(plotly)
library(tidyverse)

world_map <- map_data("world")

sc <- scale_colour_gradientn(colours = viridis(100), limits=c(0,1))


################################################################


#combine NZ data into one master file
#note that couple of the 2022 cohort are still transmitting

##NZ 2020
Ptt203571_raw <- read_csv(here::here('tag data', 'NZ', '2020', 'datapull 20230607', '203571', "203571-Locations.csv"))
Ptt203572_raw <- read_csv(here::here('tag data', 'NZ', '2020', 'datapull 20230607','203572', "203572-Locations.csv"))
Ptt203573_raw <- read_csv(here::here('tag data', 'NZ', '2020', 'datapull 20230607','203573', "203573-Locations.csv"))
Ptt203574_raw <- read_csv(here::here('tag data', 'NZ', '2020', 'datapull 20230607','203574', "203574-Locations.csv"))
Ptt203575_raw <- read_csv(here::here('tag data', 'NZ', '2020', 'datapull 20230607','203575', "203575-Locations.csv"))
Ptt205015_raw <- read_csv(here::here('tag data', 'NZ', '2020', 'datapull 20230607','205015', "205015-Locations.csv"))


all_ptt_2020 <- bind_rows(Ptt203571_raw, Ptt203572_raw, Ptt203573_raw, Ptt203574_raw, Ptt203575_raw, Ptt205015_raw)

#Only keep desired columns
all_ptt_2020 <- all_ptt_2020 %>% 
  select(DeployID, Ptt, Instr, Date, Type, Quality, Latitude, Longitude, `Error radius`, `Error Semi-major axis`, `Error Semi-minor axis`, `Error Ellipse orientation`)


all_ptt_2020 <- all_ptt_2020 %>% 
  mutate(DateTime_UTC = Date) %>%
  mutate(DateTime_UTC=parse_date_time(DateTime_UTC, "HMS dby")) %>%
  select(-Date) %>%
  mutate(Date = as_date(DateTime_UTC)) 

all_ptt_2020$cohort <- 2020



##NZ 2021
Ptt46633_raw <- read_csv(here::here('tag data', 'NZ', '2021', 'datapull 20230607','46633', "46633-Locations.csv"))
Ptt46635_raw <- read_csv(here::here('tag data', 'NZ', '2021', 'datapull 20230607','46635', "46635-Locations.csv"))
Ptt46950_raw <- read_csv(here::here('tag data', 'NZ', '2021', 'datapull 20230607','46950', "46950-Locations.csv"))
Ptt46955_raw <- read_csv(here::here('tag data', 'NZ', '2021', 'datapull 20230607','46955', "46955-Locations.csv"))
Ptt212499_raw <- read_csv(here::here('tag data', 'NZ', '2021', 'datapull 20230607','212499', "212499-Locations.csv"))
Ptt212500_raw <- read_csv(here::here('tag data', 'NZ', '2021', 'datapull 20230607','212500', "212500-Locations.csv"))
Ptt215258_raw <- read_csv(here::here('tag data', 'NZ', '2021', 'datapull 20230607','215258', "215258-Locations.csv"))
Ptt215259_raw <- read_csv(here::here('tag data', 'NZ', '2021', 'datapull 20230607','215259', "215259-Locations.csv"))
Ptt215261_raw <- read_csv(here::here('tag data', 'NZ', '2021', 'datapull 20230607','215261', "215261-Locations.csv"))
Ptt215262_raw <- read_csv(here::here('tag data', 'NZ', '2021', 'datapull 20230607','215262', "215262-Locations.csv"))
Ptt215263_raw <- read_csv(here::here('tag data', 'NZ', '2021', 'datapull 20230607','215263', "215263-Locations.csv"))

all_ptt_2021 <- bind_rows(Ptt46633_raw, Ptt46635_raw, Ptt46950_raw, Ptt46955_raw, Ptt212499_raw,
                          Ptt212500_raw, Ptt215258_raw, Ptt215259_raw, Ptt215261_raw, Ptt215262_raw, Ptt215263_raw)


#Only keep desired columns
all_ptt_2021 <- all_ptt_2021 %>% 
  select(DeployID, Ptt, Instr, Date, Type, Quality, Latitude, Longitude, `Error radius`, `Error Semi-major axis`, `Error Semi-minor axis`, `Error Ellipse orientation`)


all_ptt_2021 <- all_ptt_2021 %>% 
  mutate(DateTime_UTC = Date) %>%
  mutate(DateTime_UTC=parse_date_time(DateTime_UTC, "HMS dby")) %>%
  select(-Date) %>%
  mutate(Date = as_date(DateTime_UTC)) 

all_ptt_2021$cohort <- 2021



##NZ 2022  -- 3 still transmitting, as of July 2023
Ptt197853_raw <- read_csv(here::here('tag data', 'NZ', '2022', 'datapull 20230607', '197853', "197853-Locations.csv"))
Ptt208742_raw <- read_csv(here::here('tag data', 'NZ', '2022', 'datapull 20230607', '208742', "208742-Locations.csv"))
Ptt235399_raw <- read_csv(here::here('tag data', 'NZ', '2022', 'datapull 20230607', '235399', "235399-Locations.csv"))
Ptt235400_raw <- read_csv(here::here('tag data', 'NZ', '2022', 'datapull 20230607', '235400', "235400-Locations.csv"))
Ptt235401_raw <- read_csv(here::here('tag data', 'NZ', '2022', 'datapull 20230607', '235401', "235401-Locations.csv"))
Ptt235402_raw <- read_csv(here::here('tag data', 'NZ', '2022', 'datapull 20230607', '235402', "235402-Locations.csv"))
Ptt235403_raw <- read_csv(here::here('tag data', 'NZ', '2022', 'datapull 20230607', '235403', "235403-Locations.csv"))
Ptt235404_raw <- read_csv(here::here('tag data', 'NZ', '2022', 'datapull 20230607', '235404', "235404-Locations.csv"))


all_ptt_2022 <- bind_rows(Ptt197853_raw, Ptt208742_raw, Ptt235399_raw, Ptt235400_raw, 
                          Ptt235401_raw, Ptt235402_raw, Ptt235403_raw, Ptt235404_raw)



#Only keep desired columns
all_ptt_2022 <- all_ptt_2022 %>% 
  select(DeployID, Ptt, Instr, Date, Type, Quality, Latitude, Longitude, `Error radius`, `Error Semi-major axis`, `Error Semi-minor axis`, `Error Ellipse orientation`)


all_ptt_2022 <- all_ptt_2022 %>% 
  mutate(DateTime_UTC = Date) %>%
  mutate(DateTime_UTC=parse_date_time(DateTime_UTC, "HMS dby")) %>%
  select(-Date) %>%
  mutate(Date = as_date(DateTime_UTC)) 

all_ptt_2022$cohort <- 2022



raw_argos_df <- rbind(all_ptt_2020,all_ptt_2021,all_ptt_2022)


#safe combined NZ SRW data file - note that 3 2022 tags still going
#write_rds(raw_argos_df,here::here('SSM', 'data', 'NZ_SRW_2020_2021_2022_raw_argos_df_20230706.rds'))




################################################################


#load in master data file
raw_argos_df <- read_rds(here::here('SSM', 'data', 'NZ_SRW_2020_2021_2022_raw_argos_df_20230706.rds'))

#change column names to match Xuelei code
#also convert longitude from 0-180 to 0-360
raw_argos_df <- raw_argos_df %>% 
  dplyr::rename(id = Ptt,
                lat = Latitude,
                #lon = Longitude,
                lc = Quality,
                date = DateTime_UTC) %>% 
  mutate(lon = ifelse(Longitude <0, 360-Longitude*-1, Longitude))



# SDA filter - leave this to fit_ssm


################################################################

#remove duplicates - leave this to fit_ssm


################################################################

#Time difference between successive locations

time_diff_hours_df <- ddply(raw_argos_df, ~id, function(d){
  d$time_diff_hours <- NA
  for (i in 2:nrow(d)){
    d$time_diff_hours[i] = as.numeric(difftime(d$date[i], d$date[i-1], units = "hours"))}
  return(d)
})

# mean time difference between locations (in hours)

mts <- aggregate(time_diff_hours~id, time_diff_hours_df, mean)
mts #this is the mean time step

mxts <- aggregate(time_diff_hours~id, time_diff_hours_df, max)
mxts# this is the max time step

mnts <- aggregate(time_diff_hours~id, time_diff_hours_df, min)
mnts # this is the minimum time step

mets <- aggregate(time_diff_hours~id, time_diff_hours_df, median)
mets # this is the median time step


ggplot(time_diff_hours_df, aes(time_diff_hours)) + 
  geom_histogram(binwidth = 1, col ="white", na.rm = T) + 
  theme_bw() + xlim(c(0,100)) + 
  xlab("Time difference between successive locations")

ggplot(time_diff_hours_df, aes(time_diff_hours)) + 
  geom_histogram(binwidth = 1, col ="white", na.rm = T) + 
  theme_bw() + xlim(c(0,15)) + 
  xlab("Time difference between successive locations")


################################################################

#Segment tracks

trackseg_argos_df <- ddply(time_diff_hours_df, ~id, function(d){
  ind <- which(d$time_diff_hours > 24)
  d$mark <- 0
  d$mark[ind] <- 1
  d$track_seg <- cumsum(d$mark)
  return(d)
})

# Now create a new id based on track segment
trackseg_argos_df$track_id <- paste(trackseg_argos_df$id, "-", trackseg_argos_df$track_seg, sep="")

table(trackseg_argos_df$track_id)

length(unique(trackseg_argos_df$track_id)) #123


# remove short track segs, test n <15 
min_obs <- 15 ## set the number of minimum obs acceptable
trackseg_argos_df <- trackseg_argos_df %>% group_by(track_id)
trackseg_argos_df_filt <- filter(trackseg_argos_df, n() >= min_obs)

table(trackseg_argos_df_filt$track_id)
length(unique(trackseg_argos_df_filt$track_id)) # 65 



################################################################

#SSM


ssm_df <- trackseg_argos_df_filt

#remove the poor quality locations
ssm_df <- ssm_df %>% 
  filter (lc != "Z")


ssm_tdiff_hours_df <- ddply(ssm_df, ~track_id, function(d){
  d$time_diff_hours <- NA
  for (i in 2:nrow(d)){
    d$time_diff_hours[i] = as.numeric(difftime(d$date[i], d$date[i-1], units = "hours"))}
  return(d)
})

mts <- aggregate(time_diff_hours~ track_id, ssm_tdiff_hours_df, mean)
mts 

mean(mts$time_diff_hours) # 2.95 hrs for overall --- this is taking mean of a mean

#by annual cohort
time_diff_summary <- ssm_tdiff_hours_df %>% 
  group_by(cohort) %>% #, track_id
  summarise(first=quantile(time_diff_hours,probs=0.25, na.rm = TRUE),
            second=quantile(time_diff_hours,probs=0.5, na.rm = TRUE),
            third=quantile(time_diff_hours,probs=0.75, na.rm = TRUE),
            mean = mean(time_diff_hours, na.rm = TRUE))
#  cohort first second third  mean
#    2020 0.191  0.487  1.09  1.21
#    2021 0.202  0.550  1.64  1.71
#    2022 0.226  0.747  2.44  1.85


#now structure the data frame so it matches the required structure for SSM
#keep but rename error ellipse variables
ssm_df <- ssm_df %>% 
  select(track_id, date, lc, lon, lat, `Error Semi-major axis`, `Error Semi-minor axis`, `Error Ellipse orientation`, cohort, time_diff_hours) %>% 
  dplyr::rename(id = track_id, 
                smaj = `Error Semi-major axis`, 
                smin = `Error Semi-minor axis`, 
                eor = `Error Ellipse orientation`)

##haven't actually saved this version
#write_rds(ssm_df,here::here('SSM', 'data', 'xxx.rds'))


################################################################

#read in file ready for ssm
#ssm_df <- read_rds(here::here('SSM', 'data', 'xxx.rds'))



###2020
### try looping/function

ssm_2020 <- ssm_df %>% subset (id == "203571-0"|
                                 id == "203571-1"|
                                 
                                 id == "203572-0"|
                                 id == "203572-1"|
                                 
                                 id == "203573-0"|
                                 id == "203573-1"|
                                 
                                 id == "203574-0"|
                                 
                                 id == "203575-0"|
                                 id == "203575-2"|
                                 
                                 id == "205015-0")


#speed filter threshold (vmax) of 5 ms−1
fit_ssm_5h_NZ_2020<- fit_ssm(ssm_2020, vmax=5, model="mp", time.step=5, control = ssm_control(verbose=0))


NZ_2020_smm_mp <- purrr::map(unique(ssm_2020$id),function(x){
  ssm_2020 %>% 
    filter(id==x) %>% 
  fit_ssm(vmax=5, model="mp", time.step=5, control = ssm_control(verbose=0)) %>% 
    grab(what="p",normalise = TRUE, group = TRUE) %>% ##normalised within each trackid
    mutate(mode = case_when(g < quantile(g, probs = 0.25)   ~ "ARS",
                            g >= quantile(g, probs = 0.25)  ~ "transit")) %>% 
    unnest()
})


tibble_combined <- map_dfr(NZ_2020_smm_mp, bind_rows)  




ggplot(data.frame(tibble_combined),aes(lon, lat)) +
  geom_point(size=1, aes(col = g)) +
  geom_polygon(data = world_map, aes(x=long, y=lat, group=group), fill="black") +
  coord_equal() + 
  coord_fixed(xlim=c(80,180), ylim=c(-70,-40))+
  theme_bw()+
  theme(panel.grid=element_blank())+
  sc
ggplotly()





##the cutoff value would need to be calculated for each track individuallys
#quantile(tibble_combined$g, probs = 0.3) #0.8903892 
#quantile(tibble_combined$g, na.rm = TRUE)
#5h jmpm
# 0%        25%        50%        75%       100% 
# 0.3536222 0.8792230 0.9344770 0.9588796 0.9894788 



test <- tibble_combined %>% 
  mutate(mode = case_when(g < 0.8792230   ~ "ARS",
                          g >= 0.8792230  ~ "transit"))

ggplot(data.frame(tibble_combined),aes(lon, lat)) +
  geom_point(size=1, aes(col = mode)) +
  geom_polygon(data = world_map, aes(x=long, y=lat, group=group), fill="black") +
  coord_equal() + 
  coord_fixed(xlim=c(80,180), ylim=c(-70,-40))+
  theme_bw()+
  theme(panel.grid=element_blank())
ggplotly()


#test
#write_csv(tibble_combined,here::here('SSM', 'data', 'NZ_SRW_2020_5h_SSM_mp_dup filtered by fit_smm_normalised in each track.csv'))

############################


###2020
### try doing manually
# 
# ssm_2020 <- ssm_df %>% subset (id == "203571-0"|
#                                  id == "203571-1"|
#                                  
#                                  id == "203572-0"|
#                                  id == "203572-1"|
#                                  
#                                  id == "203573-0"|
#                                  id == "203573-1"|
#                                  
#                                  id == "203574-0"|
#                                  
#                                  id == "203575-0"|
#                                  id == "203575-2"|
#                                  
#                                  id == "205015-0")

track_id_203571_0 <- ssm_df %>% subset (id == "203571-0")

summary(track_id_203571_0$time_diff_hours)

#speed filter threshold (vmax) of 5 ms−1
fit_ssm_3h_track_id_203571_0 <- fit_ssm(track_id_203571_0, vmax=5, model="mp", time.step=3, control = ssm_control(verbose=0)) %>% 
    grab(what="p",normalise = TRUE) %>% ##normalised within each trackid
    mutate(mode = case_when(g < quantile(g, probs = 0.25)   ~ "ARS",
                            g >= quantile(g, probs = 0.25)  ~ "transit")) 

#quantile(fit_ssm_3h_track_id_203571_0$g, na.rm = TRUE)

ggplot(data.frame(fit_ssm_3h_track_id_203571_0),aes(lon, lat)) +
  geom_point(size=1, aes(col = g)) +
  geom_polygon(data = world_map, aes(x=long, y=lat, group=group), fill="black") +
  coord_equal() + 
  coord_fixed(xlim=c(80,180), ylim=c(-70,-40))+
  theme_bw()+
  theme(panel.grid=element_blank())+
  sc
ggplotly()



ggplot(data.frame(fit_ssm_3h_track_id_203571_0),aes(lon, lat)) +
  geom_point(size=1, aes(col = mode)) +
  geom_polygon(data = world_map, aes(x=long, y=lat, group=group), fill="black") +
  coord_equal() + 
  coord_fixed(xlim=c(80,180), ylim=c(-70,-40))+
  theme_bw()+
  theme(panel.grid=element_blank())
ggplotly()





track_id_203571_1 <- ssm_df %>% subset (id == "203571-1")

summary(track_id_203571_1$time_diff_hours)

#speed filter threshold (vmax) of 5 ms−1
fit_ssm_3h_track_id_203571_1 <- fit_ssm(track_id_203571_1, vmax=5, model="mp", time.step=3, control = ssm_control(verbose=0)) %>% 
  grab(what="p",normalise = TRUE) %>% ##normalised within each trackid
  mutate(mode = case_when(g < quantile(g, probs = 0.25)   ~ "ARS",
                          g >= quantile(g, probs = 0.25)  ~ "transit")) 

#quantile(fit_ssm_3h_track_id_203571_1$g, na.rm = TRUE)

ggplot(data.frame(fit_ssm_3h_track_id_203571_1),aes(lon, lat)) +
  geom_point(size=1, aes(col = g)) +
  geom_polygon(data = world_map, aes(x=long, y=lat, group=group), fill="black") +
  coord_equal() + 
  coord_fixed(xlim=c(80,180), ylim=c(-70,-40))+
  theme_bw()+
  theme(panel.grid=element_blank())+
  sc
ggplotly()



ggplot(data.frame(fit_ssm_3h_track_id_203571_1),aes(lon, lat)) +
  geom_point(size=1, aes(col = mode)) +
  geom_polygon(data = world_map, aes(x=long, y=lat, group=group), fill="black") +
  coord_equal() + 
  coord_fixed(xlim=c(80,180), ylim=c(-70,-40))+
  theme_bw()+
  theme(panel.grid=element_blank())
ggplotly()




track_id_203572_0 <- ssm_df %>% subset (id == "203572-0")

summary(track_id_203572_0$time_diff_hours)

#speed filter threshold (vmax) of 5 ms−1
fit_ssm_3h_track_id_203572_0 <- fit_ssm(track_id_203572_0, vmax=5, model="mp", time.step=3, control = ssm_control(verbose=0)) %>% 
  grab(what="p",normalise = TRUE) %>% ##normalised within each trackid
  mutate(mode = case_when(g < quantile(g, probs = 0.25)   ~ "ARS",
                          g >= quantile(g, probs = 0.25)  ~ "transit")) 

#quantile(fit_ssm_3h_track_id_203572_0$g, na.rm = TRUE)

ggplot(data.frame(fit_ssm_3h_track_id_203572_0),aes(lon, lat)) +
  geom_point(size=1, aes(col = g)) +
  geom_polygon(data = world_map, aes(x=long, y=lat, group=group), fill="black") +
  coord_equal() + 
  coord_fixed(xlim=c(80,180), ylim=c(-70,-40))+
  theme_bw()+
  theme(panel.grid=element_blank())+
  sc
ggplotly()



ggplot(data.frame(fit_ssm_3h_track_id_203572_0),aes(lon, lat)) +
  geom_point(size=1, aes(col = mode)) +
  geom_polygon(data = world_map, aes(x=long, y=lat, group=group), fill="black") +
  coord_equal() + 
  coord_fixed(xlim=c(80,180), ylim=c(-70,-40))+
  theme_bw()+
  theme(panel.grid=element_blank())
ggplotly()






track_id_203572_1 <- ssm_df %>% subset (id == "203572-1")

summary(track_id_203572_1$time_diff_hours)

#speed filter threshold (vmax) of 5 ms−1
fit_ssm_5h_track_id_203572_1 <- fit_ssm(track_id_203572_1, vmax=5, model="mp", time.step=3, control = ssm_control(verbose=0)) %>% 
  grab(what="p",normalise = TRUE) %>% ##normalised within each trackid
  mutate(mode = case_when(g < quantile(g, probs = 0.25)   ~ "ARS",
                          g >= quantile(g, probs = 0.25)  ~ "transit")) 

#quantile(fit_ssm_5h_track_id_203572_1$g, na.rm = TRUE)

ggplot(data.frame(fit_ssm_5h_track_id_203572_1),aes(lon, lat)) +
  geom_point(size=1, aes(col = g)) +
  geom_polygon(data = world_map, aes(x=long, y=lat, group=group), fill="black") +
  coord_equal() + 
  coord_fixed(xlim=c(80,180), ylim=c(-70,-40))+
  theme_bw()+
  theme(panel.grid=element_blank())+
  sc
ggplotly()



ggplot(data.frame(fit_ssm_5h_track_id_203572_1),aes(lon, lat)) +
  geom_point(size=1, aes(col = mode)) +
  geom_polygon(data = world_map, aes(x=long, y=lat, group=group), fill="black") +
  coord_equal() + 
  coord_fixed(xlim=c(80,180), ylim=c(-70,-40))+
  theme_bw()+
  theme(panel.grid=element_blank())
ggplotly()







track_id_203573_0 <- ssm_df %>% subset (id == "203573-0")

summary(track_id_203573_0$time_diff_hours)

#speed filter threshold (vmax) of 5 ms−1
fit_ssm_3h_track_id_203573_0 <- fit_ssm(track_id_203573_0, vmax=5, model="mp", time.step=3, control = ssm_control(verbose=0)) %>% 
  grab(what="p",normalise = TRUE) %>% ##normalised within each trackid
  mutate(mode = case_when(g < quantile(g, probs = 0.25)   ~ "ARS",
                          g >= quantile(g, probs = 0.25)  ~ "transit")) 

#quantile(fit_ssm_3h_track_id_203573_0$g, na.rm = TRUE)

ggplot(data.frame(fit_ssm_3h_track_id_203573_0),aes(lon, lat)) +
  geom_point(size=1, aes(col = g)) +
  geom_polygon(data = world_map, aes(x=long, y=lat, group=group), fill="black") +
  coord_equal() + 
  coord_fixed(xlim=c(80,180), ylim=c(-70,-40))+
  theme_bw()+
  theme(panel.grid=element_blank())+
  sc
ggplotly()



ggplot(data.frame(fit_ssm_3h_track_id_203573_0),aes(lon, lat)) +
  geom_point(size=1, aes(col = mode)) +
  geom_polygon(data = world_map, aes(x=long, y=lat, group=group), fill="black") +
  coord_equal() + 
  coord_fixed(xlim=c(80,180), ylim=c(-70,-40))+
  theme_bw()+
  theme(panel.grid=element_blank())
ggplotly()





track_id_203574_0 <- ssm_df %>% subset (id == "203574-0")

summary(track_id_203574_0$time_diff_hours)

#speed filter threshold (vmax) of 5 ms−1
fit_ssm_3h_track_id_203574_0 <- fit_ssm(track_id_203574_0, vmax=5, model="mp", time.step=3, control = ssm_control(verbose=0)) %>% 
  grab(what="p",normalise = TRUE) %>% ##normalised within each trackid
  mutate(mode = case_when(g < quantile(g, probs = 0.25)   ~ "ARS",
                          g >= quantile(g, probs = 0.25)  ~ "transit")) 

#quantile(fit_ssm_3h_track_id_203574_0$g, na.rm = TRUE)

ggplot(data.frame(fit_ssm_3h_track_id_203574_0),aes(lon, lat)) +
  geom_point(size=1, aes(col = g)) +
  geom_polygon(data = world_map, aes(x=long, y=lat, group=group), fill="black") +
  coord_equal() + 
  coord_fixed(xlim=c(80,180), ylim=c(-70,-40))+
  theme_bw()+
  theme(panel.grid=element_blank())+
  sc
ggplotly()



ggplot(data.frame(fit_ssm_3h_track_id_203574_0),aes(lon, lat)) +
  geom_point(size=1, aes(col = mode)) +
  geom_polygon(data = world_map, aes(x=long, y=lat, group=group), fill="black") +
  coord_equal() + 
  coord_fixed(xlim=c(80,180), ylim=c(-70,-40))+
  theme_bw()+
  theme(panel.grid=element_blank())
ggplotly()






track_id_203575_0 <- ssm_df %>% subset (id == "203575-0")

summary(track_id_203575_0$time_diff_hours)

#speed filter threshold (vmax) of 5 ms−1
fit_ssm_3h_track_id_203575_0 <- fit_ssm(track_id_203575_0, vmax=5, model="mp", time.step=3, control = ssm_control(verbose=0)) %>% 
  grab(what="p",normalise = TRUE) %>% ##normalised within each trackid
  mutate(mode = case_when(g < quantile(g, probs = 0.25)   ~ "ARS",
                          g >= quantile(g, probs = 0.25)  ~ "transit")) 

#quantile(fit_ssm_3h_track_id_203575_0$g, na.rm = TRUE)

ggplot(data.frame(fit_ssm_3h_track_id_203575_0),aes(lon, lat)) +
  geom_point(size=1, aes(col = g)) +
  geom_polygon(data = world_map, aes(x=long, y=lat, group=group), fill="black") +
  coord_equal() + 
  coord_fixed(xlim=c(80,180), ylim=c(-70,-40))+
  theme_bw()+
  theme(panel.grid=element_blank())+
  sc
ggplotly()



ggplot(data.frame(fit_ssm_3h_track_id_203575_0),aes(lon, lat)) +
  geom_point(size=1, aes(col = mode)) +
  geom_polygon(data = world_map, aes(x=long, y=lat, group=group), fill="black") +
  coord_equal() + 
  coord_fixed(xlim=c(80,180), ylim=c(-70,-40))+
  theme_bw()+
  theme(panel.grid=element_blank())
ggplotly()






track_id_203575_2 <- ssm_df %>% subset (id == "203575-2")

summary(track_id_203575_2$time_diff_hours)

#speed filter threshold (vmax) of 5 ms−1
fit_ssm_3h_track_id_203575_2 <- fit_ssm(track_id_203575_2, vmax=5, model="mp", time.step=3, control = ssm_control(verbose=0)) %>% 
  grab(what="p",normalise = TRUE) %>% ##normalised within each trackid
  mutate(mode = case_when(g < quantile(g, probs = 0.25)   ~ "ARS",
                          g >= quantile(g, probs = 0.25)  ~ "transit")) 

#quantile(fit_ssm_3h_track_id_203575_2$g, na.rm = TRUE)

ggplot(data.frame(fit_ssm_3h_track_id_203575_2),aes(lon, lat)) +
  geom_point(size=1, aes(col = g)) +
  geom_polygon(data = world_map, aes(x=long, y=lat, group=group), fill="black") +
  coord_equal() + 
  coord_fixed(xlim=c(80,180), ylim=c(-70,-40))+
  theme_bw()+
  theme(panel.grid=element_blank())+
  sc
ggplotly()



ggplot(data.frame(fit_ssm_3h_track_id_203575_2),aes(lon, lat)) +
  geom_point(size=1, aes(col = mode)) +
  geom_polygon(data = world_map, aes(x=long, y=lat, group=group), fill="black") +
  coord_equal() + 
  coord_fixed(xlim=c(80,180), ylim=c(-70,-40))+
  theme_bw()+
  theme(panel.grid=element_blank())
ggplotly()





track_id_205015_0 <- ssm_df %>% subset (id == "205015-0")

summary(track_id_205015_0$time_diff_hours)

#speed filter threshold (vmax) of 5 ms−1
fit_ssm_3h_track_id_205015_0 <- fit_ssm(track_id_205015_0, vmax=5, model="mp", time.step=3, control = ssm_control(verbose=0)) %>% 
  grab(what="p",normalise = TRUE) %>% ##normalised within each trackid
  mutate(mode = case_when(g < quantile(g, probs = 0.25)   ~ "ARS",
                          g >= quantile(g, probs = 0.25)  ~ "transit")) 

#quantile(fit_ssm_3h_track_id_203575_0$g, na.rm = TRUE)

ggplot(data.frame(fit_ssm_3h_track_id_205015_0),aes(lon, lat)) +
  geom_point(size=1, aes(col = g)) +
  geom_polygon(data = world_map, aes(x=long, y=lat, group=group), fill="black") +
  coord_equal() + 
  coord_fixed(xlim=c(80,180), ylim=c(-70,-40))+
  theme_bw()+
  theme(panel.grid=element_blank())+
  sc
ggplotly()



ggplot(data.frame(fit_ssm_3h_track_id_205015_0),aes(lon, lat)) +
  geom_point(size=1, aes(col = mode)) +
  geom_polygon(data = world_map, aes(x=long, y=lat, group=group), fill="black") +
  coord_equal() + 
  coord_fixed(xlim=c(80,180), ylim=c(-70,-40))+
  theme_bw()+
  theme(panel.grid=element_blank())
ggplotly()



NZ_2020_smm_mp <- rbind(fit_ssm_3h_track_id_203571_0,
                        fit_ssm_3h_track_id_203571_1,
                        fit_ssm_3h_track_id_203572_0,
                        fit_ssm_3h_track_id_203572_1,
                        fit_ssm_3h_track_id_203573_0,
                        fit_ssm_3h_track_id_203574_0,
                        fit_ssm_3h_track_id_203575_0,
                        fit_ssm_3h_track_id_203575_2,
                        fit_ssm_3h_track_id_205015_0)


#write_csv(NZ_2020_smm_mp,here::here('SSM', 'data', 'NZ_2020_smm_mp_normalised in each track_20230707.csv'))

