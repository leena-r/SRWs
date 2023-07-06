#run SSM on NZ tracks


################################

#use Xuelei's SSM code

################################

#aniMotum_1.1-05
# install.packages("aniMotum", 
#                  repos = c("https://cloud.r-project.org",
#                            "https://ianjonsen.r-universe.dev"),
#                  dependencies = TRUE)
# or download a binary version of the {aniMotum} package 
#here https://ianjonsen.r-universe.dev/ui#package:aniMotum. 
#If you have downloaded the file correctly, its extension will be .tgz, not .tar
#You can use the following command in the R console to install the file you’ve just downloaded 
#(where path_to_file is wherever you saved the download):
#install.packages("path_to_file\aniMotum_1.1-02.zip", 
#                 repos=NULL, type="win.binary", dependencies = TRUE)

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



# SDA filter 
# vmax in m/s
#Xuelei used 25, but use 5m/s as per Reisinger et al., 2021
raw_argos_df <- ddply(raw_argos_df, ~id, function(d){
  d$argosfilter <- sdafilter(lat = d$lat, 
                             lon = d$lon, 
                             lc = d$lc, 
                             dtime = d$date, vmax = 5)
  return(d)
})

glimpse(raw_argos_df)


# visualize filter 
with(raw_argos_df, plot(lon,lat,col="lightgrey",type="p",xlab="Longitude",ylab="Latitude", pch=19, cex=0.8)) #all data , xlim=c(55,170), ylim=c(-65, -35)
with(raw_argos_df, points(lon[which(argosfilter=="not")],lat[which(argosfilter=="not")],col="blue", pch=19, cex=0.8)) #all data to be retained
with(raw_argos_df, points(lon[which(argosfilter=="removed")],lat[which(argosfilter=="removed")],col="red", pch=19, cex=0.5)) #removed locations


# exclude errorneous locs 
filtered_argos_df <- raw_argos_df %>%
  filter(argosfilter != "removed") %>%
  dplyr::select(-argosfilter)




tab_1 <- raw_argos_df %>% 
  group_by(id) %>% 
  dplyr::summarize(nb_locations = n())

tab_2 <- filtered_argos_df %>% 
  group_by(id) %>% 
  dplyr::summarize(nb_locations = n())

tab  <- plyr::join(data.frame(tab_1), data.frame(tab_2), by="id")
colnames(tab) <- c("id", "raw_locs", "filt_locs")
tab



################################################################

#remove duplicates


pre_dup <- nrow(filtered_argos_df) # to get the current number of data points

# create dummy variable
filtered_argos_df$index <- c(1:nrow(filtered_argos_df))

# run each tag in a loop to check for duplicates
# if there is a time duplicate, select the best quality position or simply the first position
filtered_argos_df <- ddply(filtered_argos_df, ~id, function(d){
  toremove <- c()
  for (i in c(2:nrow(d))) {
    if (d$date[i] == d$date[i-1]) {
      dd <- d[(i-1):i,]
      r <- dd[dd$lc == ave(dd$lc, FUN = min), "index"] # find the lowest quality
      toremove <- c(toremove, r[1]) #select first element of r in case both locations have the same lq
    }
  }
  if (length(toremove) > 0){d <- d[!(d$index %in% toremove), ]}
  return(d)
})
# remove dummy variable
filtered_argos_df$index <- NULL
pre_dup - nrow(filtered_argos_df) #1492



################################################################

#Time difference between successive locations

time_diff_hours_df <- ddply(filtered_argos_df, ~id, function(d){
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


# remove short track segs n <10 
min_obs <- 10 ## set the number of minimum obs acceptable
trackseg_argos_df <- trackseg_argos_df %>% group_by(track_id)
trackseg_argos_df_filt <- filter(trackseg_argos_df, n() >= min_obs)

table(trackseg_argos_df_filt$track_id)
length(unique(trackseg_argos_df_filt$track_id)) # 77 

# 48791 locs left in df


################################################################

#SSM

#df is slightly different to Xuelei's, who here selected columns like this
#ssm_df <- trackseg_argos_df_filt[,c(2:5,9)]
#will do that later, so can use 'cohort' column when looking at time diff between steps

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

mean(mts$time_diff_hours) # 3.67 hrs for overall --- this is taking mean of a mean

#by annual cohort
time_diff_summary <- ssm_tdiff_hours_df %>% 
  group_by(cohort) %>% #, track_id
  summarise(first=quantile(time_diff_hours,probs=0.25, na.rm = TRUE),
            second=quantile(time_diff_hours,probs=0.5, na.rm = TRUE),
            third=quantile(time_diff_hours,probs=0.75, na.rm = TRUE),
            mean = mean(time_diff_hours, na.rm = TRUE))
#  cohort first second third  mean
#   2020  0.266  0.603  1.31  1.45
#   2021  0.271  0.682  2.38  1.98
#   2022  0.317  0.89   2.98  2.16


#now structure the data frame so it matches the required structure for SSM
ssm_df <- ssm_df %>% 
  select(track_id, date, lc, lon, lat) %>% 
  dplyr::rename(id = track_id)


#write_rds(ssm_df,here::here('SSM', 'data', 'NZ_SRW_2020_2021_2022_ssm_df_20230706.rds'))


################################################################

#read in file ready for ssm
ssm_df <- read_rds(here::here('SSM', 'data', 'NZ_SRW_2020_2021_2022_ssm_df_20230706.rds'))



###2020

# 16121 lcs

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
# average time difference = 1.45

table(ssm_2020$id)

#speed filter threshold (vmax) of 5 ms−1
fit_ssm_5h_NZ_2020<- fit_ssm(ssm_2020, vmax=5, model="crw", time.step=5, control = ssm_control(verbose=0))

fit_ssm_5h_NZ_2020

#plot(fit_ssm_5h_NZ_2020,ask=F,type=2,alpha=0.1,what="p")


ssm_2020_df <- grab(fit_ssm_5h_NZ_2020,what="p") # 

table(ssm_2020_df$id)

#ssm_2020_df <- ssm_2020_df %>% filter (id != "203573-1")

test <- fit_ssm_5h_NZ_2020 %>% filter (id != "203573-1")
mpm_NZ_2020<- fit_mpm(fit_ssm_5h_NZ_2020, model="jmpm", control = mpm_control(verbose = 0)) #model="jmpm"
#Error in nlminb(obj$par, ifelse(control$verbose == 1, myfn, obj$fn), obj$gr,  : 
#                  NA/NaN gradient evaluation
#https://github.com/ianjonsen/aniMotum/issues/29 :
#"In the joint move persistence model jmpm, the variance parameter for the random walk on γt is shared among individuals, 
#which can lead to optimizer errors (like the one you've encountered) if the tracks being fit 
#represent very different movement patterns. I don't think the different ranges in longitude should matter though.
#You could explore this by iteratively removing individual tracks and re-fitting the jmpm"
##---------- works if drop 203573-1

#3h jmpm no errors
#3h mpm no errors
#5h jmpm no errors

mpm_NZ_2020

#plot(mpm_NZ_2020,ask=F)


mpm_NZ_2020_df <- grab(x=mpm_NZ_2020,what = "f") 

ssm_mpm_NZ_2020<- dplyr::full_join(x=ssm_2020_df,y=mpm_NZ_2020_df)

ggplot(data.frame(ssm_mpm_NZ_2020),aes(lon, lat)) +
  geom_point(size=1, aes(col = g)) +
  geom_polygon(data = world_map, aes(x=long, y=lat, group=group), fill="black") +
  coord_equal() + 
  coord_fixed(xlim=c(80,180), ylim=c(-70,-40))+
  theme_bw()+
  theme(panel.grid=element_blank())+
  sc
ggplotly()

mean(ssm_mpm_NZ_2020$g)

# fit_mpm jmpm mean g= 0.61 ts=6h 3754 locs 
# Xuelei comment: mpm did not fit when ts = 4h 

##which one is better mpm or jmpm??


##Xuelei comments:
# ts = 5h 4503 locs; NaNs produced "203573-1 didnot converge when used mpm 
# ts = 5h jmpm mean=0.66
# 5h seems to be better lowe CI move persistence lines seem to be smoother 


quantile(ssm_mpm_NZ_2020$g, probs = 0.3)
quantile(ssm_mpm_NZ_2020$g, na.rm = TRUE)
#5h jmpm
# 0%        25%        50%        75%       100% 
# 0.02303193 0.43859461 0.72624702 0.84551633 0.93622054 

test <- ssm_mpm_NZ_2020 %>% 
  mutate(mode = case_when(g < 0.5024051   ~ "ARS",
                          g >= 0.5024051  ~ "transit"))

ggplot(data.frame(test),aes(lon, lat)) +
  geom_point(size=1, aes(col = mode)) +
  geom_polygon(data = world_map, aes(x=long, y=lat, group=group), fill="black") +
  coord_equal() + 
  coord_fixed(xlim=c(80,180), ylim=c(-70,-40))+
  theme_bw()+
  theme(panel.grid=element_blank())
ggplotly()

################################################################




################################################################
























































########delete the following:

# SDA filter 

all_ptt_2020_sda_filt <- ddply(all_ptt_2020, ~Ptt, function(d){
  d$argosfilter <- sdafilter(lat = d$Latitude, 
                             lon = d$Longitude, 
                             lc = d$Quality, 
                             dtime = d$DateTime_UTC, vmax = 25) #should dtime be Date or DateTime?? #can vary vmax, have also used 5 here
  return(d)
})

glimpse(all_ptt_2020_sda_filt)


# visualize filter 

with(all_ptt_2020_sda_filt, plot(Longitude,Latitude,col="lightgrey",type="p",xlab="Longitude",ylab="Latitude", pch=19, cex=0.8)) #all data , xlim=c(55,170), ylim=c(-65, -35)
with(all_ptt_2020_sda_filt, points(Longitude[which(argosfilter=="not")],Latitude[which(argosfilter=="not")],col="blue", pch=19, cex=0.8)) #all data to be retained
with(all_ptt_2020_sda_filt, points(Longitude[which(argosfilter=="removed")],Latitude[which(argosfilter=="removed")],col="red", pch=19, cex=0.5)) #removed locations


# exclude errorneous locs 
filtered_all_ptt_2020_df <- all_ptt_2020_sda_filt %>%
  filter(argosfilter != "removed") %>%
  dplyr::select(-argosfilter)



tab_1 <- all_ptt_2020_sda_filt %>% 
  group_by(Ptt) %>% 
  dplyr::summarize(nb_locations = n())

tab_2 <- filtered_all_ptt_2020_df %>% 
  group_by(Ptt) %>% 
  dplyr::summarize(nb_locations = n())

tab  <- plyr::join(data.frame(tab_1), data.frame(tab_2), by="Ptt")
colnames(tab) <- c("Ptt", "raw_locs", "filt_locs")
tab
#  Ptt    raw_locs  filt_locs
# 203571     1850      1557
# 203572     3560      3138
# 203573      243       215
# 203574     8359      7562
# 203575     2969      2627
# 205015     1660      1512


##########


pre_dup <- nrow(filtered_all_ptt_2020_df) # to get the current number of data points
#16611

# create dummy variable
filtered_all_ptt_2020_df$index <- c(1:nrow(filtered_all_ptt_2020_df))

# run each tag in a loop to check for duplicates
# if there is a time duplicate, select the best quality position or simply the first position
filtered_all_ptt_2020_df <- ddply(filtered_all_ptt_2020_df, ~Ptt, function(d){
  toremove <- c()
  for (i in c(2:nrow(d))) {
    if (d$DateTime_UTC[i] == d$DateTime_UTC[i-1]) {
      dd <- d[(i-1):i,]
      r <- dd[dd$Quality == ave(dd$Quality, FUN = min), "index"] # find the lowest quality
      toremove <- c(toremove, r[1]) #select first element of r in case both locations have the same lq
    }
  }
  if (length(toremove) > 0){d <- d[!(d$index %in% toremove), ]}
  return(d)
})

# remove dummy variable
filtered_all_ptt_2020_df <- filtered_all_ptt_2020_df %>% 
  select(-index)
pre_dup - nrow(filtered_all_ptt_2020_df)
#477 removed


##########


time_diff_hours_df <- ddply(filtered_all_ptt_2020_df, ~Ptt, function(d){
  d$time_diff_hours <- NA
  for (i in 2:nrow(d)){
    d$time_diff_hours[i] = as.numeric(difftime(d$DateTime_UTC[i], d$DateTime_UTC[i-1], units = "hours"))}
  return(d)
})

# mean time difference between locations (in hours)

mts <- aggregate(time_diff_hours~Ptt, time_diff_hours_df, mean)
mts #this is the mean time step
#  Ptt      time_diff_hours
# 203571       1.1981382
# 203572       1.3250110
# 203573       4.6430823
# 203574       1.2450874
# 203575       3.0365373
# 205015       0.9905809

mxts <- aggregate(time_diff_hours~Ptt, time_diff_hours_df, max)
mxts# this is the max time step
#  Ptt      time_diff_hours
# 203571        73.75361
# 203572        61.70472
# 203573       571.50056
# 203574        23.38111
# 203575      1679.74806
# 205015        10.33222

mnts <- aggregate(time_diff_hours~Ptt, time_diff_hours_df, min)
mnts # this is the minimum time step

mets <- aggregate(time_diff_hours~Ptt, time_diff_hours_df, median)
mets # this is the median time step
#  Ptt      time_diff_hours
# 203571       0.5705556
# 203572       0.5173611
# 203573       0.9433333
# 203574       0.5152778
# 203575       0.9138889
# 205015       0.5075000

ggplot(time_diff_hours_df, aes(time_diff_hours)) + 
  geom_histogram(binwidth = 1, col ="white", na.rm = T) + 
  theme_bw() + 
  xlim(c(0,100)) + 
  xlab("Time difference between successive locations")

ggplot(time_diff_hours_df, aes(time_diff_hours)) + 
  geom_histogram(binwidth = 1, col ="white", na.rm = T) + 
  theme_bw() + xlim(c(0,15)) + 
  xlab("Time difference between successive locations")


##########


trackseg_argos_df <- ddply(time_diff_hours_df, ~Ptt, function(d){
  ind <- which(d$time_diff_hours > 24)
  d$mark <- 0
  d$mark[ind] <- 1
  d$track_seg <- cumsum(d$mark)
  return(d)
})

# Now create a new id based on track segment
trackseg_argos_df$track_id <- paste(trackseg_argos_df$Ptt, "-", trackseg_argos_df$track_seg, sep="")

table(trackseg_argos_df$track_id)

length(unique(trackseg_argos_df$track_id)) #14 (NZ 2020 cohort only)

# remove short track segs n <10 

min_obs <- 10 ## set the number of minimum obs acceptable
trackseg_argos_df <- trackseg_argos_df %>% group_by(track_id)
trackseg_argos_df_filt <- filter(trackseg_argos_df, n() >= min_obs)

table(trackseg_argos_df_filt$track_id)
length(unique(trackseg_argos_df_filt$track_id)) # 10 (NZ 2020 cohort only) 

# 16122 locs left in df


##########


ssm_df_NZ_2020 <- trackseg_argos_df_filt[,c(2:5,9)] #what columns are being selected here??

ssm_df_NZ_2020 <- ssm_df_NZ_2020 %>% filter (Quality != "Z")

ssm_tdiff_hours_df <- ddply(ssm_df_NZ_2020, ~track_id, function(d){
  d$time_diff_hours <- NA
  for (i in 2:nrow(d)){
    d$time_diff_hours[i] = as.numeric(difftime(d$DateTime_UTC[i], d$DateTime_UTC[i-1], units = "hours"))}
  return(d)
})

mts <- aggregate(time_diff_hours~ track_id, ssm_tdiff_hours_df, mean)
mts 
# track_id    time_diff_hours
# 203571-0       1.0469361
# 203571-1       2.4814730
# 203572-0       1.2397938
# 203572-1       3.3546032
# 203573-0       1.8545615
# 203573-1       1.6263580
# 203574-0       1.2450874
# 203575-0       2.3849057
# 203575-2       1.6730601
# 205015-0       0.9905809

mean(mts$time_diff_hours) # 1.789736 hrs for overall, NZ 2020 cohort

ssm_df_NZ_2020 <- ssm_df_NZ_2020[,c(5,1:4)] #structure the data frame so it matches the required structure
ssm_df_NZ_2020x <- ssm_df_NZ_2020 %>% 
  #ungroup() %>% 
  select(Ptt, DateTime_UTC, Quality, Longitude, Latitude) %>% 
  dplyr::rename(id = track_id, #works with Ptt, not so much with track_id
                date = DateTime_UTC,
                lc = Quality,
                lon = Longitude,
                lat = Latitude)

colnames(ssm_df_NZ_2020)[1] <- "id"


#prob don't need these
ssm_df_NZ_2020 <- data.frame(ssm_df_NZ_2020)
#save(ssm_df_NZ_2020,file="D:/auckland/nzsrw/maps/maps/analysis/data processing/new/ssm_df_3.8.Rdata")


##########


# ssm_22 <- ssm_df %>% filter(date > "2022-07-01")
# 
# ssm_22<- ssm_22  %>% subset (id != "215262-18"&
#                                id != "215262-20"&
#                                id!="215262-22"&
#                                id != "215258-9")
# 
# table(ssm_22$id)
# length(unique(ssm_22$id))


fit_ssm_6_NZ_2020 <- fit_ssm(ssm_df_NZ_2020x, vmax = 25, model="crw", time.step = 6, control = ssm_control(verbose=0))

print(fit_ssm_6_NZ_2020)

ssm_NZ_2020_df <- grab(x=fit_ssm_6_NZ_2020,what = "p")

#aniMotum::map(fit_ssm_6_NZ_2020,what = "predicted",by.date=F,aes = aes_lst(mp=FALSE,fill=c("dodgerblue", "dodgerblue", NA, "orange", "grey60", NA),conf = F,date_pal = hcl.colors(100, palette = "Viridis", rev = FALSE)))


# move persistence model 

mpm_NZ_2020 <- fit_mpm(fit_ssm_6_NZ_2020,model="mpm",control = mpm_control(verbose = 0))

mpm_NZ_2020

mpm_NZ_2020_df <- grab(x=mpm_NZ_2020,what = "f") 

#plot(mpm_NZ_2020, control = mpm_control(verbose=0),ask=F)



ssm_mpm_NZ_2020 <- aniMotum::join(fit_ssm_6_NZ_2020, mpm_NZ_2020, what.ssm="predicted", as_sf=FALSE)%>%
  as.data.frame()




ggplot(ssm_mpm_NZ_2020, aes(lon, lat)) +
  geom_point(size=1, aes(col = g)) +
  geom_polygon(data = world_map, aes(x=long, y=lat, group=group), fill="black") +
  coord_equal() + 
  coord_fixed(xlim=c(80,180), ylim=c(-70,-40))+
  theme_bw()+
  theme(panel.grid=element_blank())+
  sc
ggplotly()

mean(ssm_mpm_NZ_2020$g)
#save(ssm_mpm_22_df,file="G:/auckland/nz_right_whale/maps/analysis/data processing/new/2022/ssm_mpm_NZ_2020.Rdata")
#save(ssm_mpm_NZ_2020,file="D:/auckland/nzsrw/maps/maps/analysis/data processing/new/2022/ssm_mpm_NZ_2020.Rdata")

#write_csv(ssm_mpm_NZ_2020,here::here('SSM', 'data', 'test_NZ_2020_SSM_6h.csv'))


#######################################################################################

#try NZ 2020 tracks with Solene code


# load raw data - NZ 2020 cohort

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

# filter z positions
all_ptt_2020 <- all_ptt_2020 %>% filter(Quality != 'Z')

# transform Date into POSIX format
all_ptt_2020 <- all_ptt_2020 %>% 
  mutate(Date = as.POSIXct(Date, format = "%H:%M:%S %d-%b-%Y"),
         month = as.numeric(strftime(Date, format = "%m")))

# prefilter positions with the speed filter (see Reisinger et al. 2021 who did that before actually fitting the ssm)
# vmax in meters / s
all_ptt_2020 <- ddply(all_ptt_2020, ~DeployID, function(d){
  d$argosfilter <- sdafilter(lat = d$Latitude, 
                             lon = d$Longitude, 
                             lc = d$Quality, 
                             dtime = d$Date, vmax = 5)
  return(d)
})
all_ptt_2020 <- all_ptt_2020 %>% 
  filter(argosfilter != "removed") %>% 
  dplyr::select(-argosfilter)

# who is left? how many positions?
all_ptt_2020 %>% 
  group_by(DeployID) %>% 
  dplyr::summarize(nb_locations = n())



####################
#Segmentate when transmissions interrupted
#Assess mean time step between locations

timelaps_df <- ddply(all_ptt_2020, ~DeployID, function(d){
  d$timelaps_hours <- NA
  for (i in 2:nrow(d)){
    d$timelaps_hours[i] = difftime(d$Date[i], d$Date[i-1], units = "hours")}
  return(d)
})

# mean time step between locations (in hours)
a <- aggregate(timelaps_hours~DeployID, timelaps_df, mean)
a

mean(a$timelaps_hours)


ggplot(timelaps_df, aes(timelaps_hours)) + 
  geom_histogram(binwidth = 5, col ="white", na.rm = T) + 
  theme_bw() + 
  facet_wrap(~DeployID, scales = "free") +
  xlab("Time laps between successive locations (in hours)")



### some Solene code I don't have
# source("Fun_TrackInterruption_SRW.R")
# 
# # need a time column to run
# raw_tracks$time <- raw_tracks$Date
# # run the custom function to split tarcks interrupted for more than 144 hours = 6 days
# raw_tracks <- Fun_TrackInterruption(dataframe = raw_tracks, hours_gap = 144, id = "DeployID")

# # set first timelaps of each segment to NA
# raw_tracks <- ddply(raw_tracks, ~segmentid, function(d){
#   d$timelaps[1] <- NA
#   return(d)
# })



##XUELEI:
trackseg_argos_df <- ddply(timelaps_df, ~DeployID, function(d){
  ind <- which(d$timelaps_hours > 24)
  d$mark <- 0
  d$mark[ind] <- 1
  d$track_seg <- cumsum(d$mark)
  return(d)
})

# Now create a new id based on track segment
trackseg_argos_df$track_id <- paste(trackseg_argos_df$Ptt, "-", trackseg_argos_df$track_seg, sep="")

raw_tracks <- trackseg_argos_df %>% 
  dplyr::rename(segmentid = track_id)

#back to SOlene code
# print number of locations per segments
raw_tracks %>% 
  group_by(segmentid) %>% 
  dplyr::summarize(nb_locations = n()) %>% 
  print(n = Inf)



# remove segments with less than 15 locations - Xuelei used 10
toremove <- raw_tracks %>% 
  group_by(segmentid) %>% 
  dplyr::summarize(nb_locations = n()) %>% 
  filter(nb_locations <= 10)

raw_tracks <- raw_tracks %>% filter(!(segmentid %in% toremove$segmentid))

# print number of locations per segments
raw_tracks %>% 
  group_by(segmentid) %>% 
  dplyr::summarize(nb_locations = n()) %>% 
  print(n = Inf)


# now what's the average time between locations?
a <- aggregate(timelaps_hours~segmentid, raw_tracks, mean)
a
#segment 203573-1 very high 53h

mean(a$timelaps_hours)

# what's the maximum time between locations?
a <- aggregate(timelaps_hours~segmentid, raw_tracks, max)
a

####################
#SSM

raw_tracks$time <- raw_tracks$Date

data_ssm <- raw_tracks %>% 
  dplyr::select(segmentid, time, Quality, Longitude, Latitude, `Error Semi-major axis`, `Error Semi-minor axis`, `Error Ellipse orientation`) %>% 
  dplyr::rename(id = segmentid, date = time, lc = Quality, lon = Longitude, lat = Latitude, smaj = `Error Semi-major axis`, smin = `Error Semi-minor axis`, eor = `Error Ellipse orientation`)
data_ssm$geometry <- NULL

str(data_ssm)


data_ssm$smaj <- as.integer(data_ssm$smaj)
data_ssm$smin <- as.integer(data_ssm$smin)
data_ssm$eor <- as.integer(data_ssm$eor)

str(data_ssm)


####################
#Fit model with two different time steps

#THIS DOESN'T WORK
nz_fit_ssm <- tibble(time_step = c(3, 6)) %>% # create a tibble with the two time step conditions in one column
  group_by(time_step) %>% # 
  nest() %>% 
  mutate(data = list(data_ssm, data_ssm), # store the same dataframe in the two rows
         fit = map2(time_step, data, function(x, y){
           fit_ssm(y,
                   spdf = F, # we already run the filter so no need to do again
                   pf = F,
                   model = "rw",
                   min.dt = 48,
                   time.step = x,
                   control = ssm_control(verbose = 0),
                   map = list(psi = factor(NA)))}))


#Xuelei:
fit_ssm_6_22 <- fit_ssm(ssm_22,vmax = 25,model="crw",time.step = 6,control = ssm_control(verbose=0))


nz_fit_ssm <- fit_ssm(data_ssm,
                      spdf = F, # we already run the filter so no need to do again
                      pf = F,
                      model = "crw",
                      min.dt = 48,
                      time.step = 6,
                      control = ssm_control(verbose = 0),
                      map = list(psi = factor(NA)))


#Xuelei
nz_fit_ssm_df <- grab(x=nz_fit_ssm,what = "p")



####################
#ARS
#Assess movement persistence along tracks

nz_fit_ssm <- nz_fit_ssm %>% 
  mutate(fmp = map(nz_fit_ssm, function(a){
    fit_mpm(a, what = "predicted", model = "mpm", control = mpm_control(verbose = 0))
  }))


#xuelei
fmp <- fit_mpm(nz_fit_ssm,model="mpm",control = mpm_control(verbose = 0))

fmp_df <- grab(x=fmp,what = "f") 

ssm_fmp <- aniMotum::join(nz_fit_ssm,fmp,what.ssm="predicted",as_sf=FALSE)%>%
  as.data.frame()

plot(fmp, type = 3, pages = 1, ncol = 2)


ggplot(ssm_fmp, aes(lon, lat)) +
  geom_point(size=1, aes(col = g)) +
  geom_polygon(data = world_map, aes(x=long, y=lat, group=group), fill="black") +
  coord_equal() + 
  coord_fixed(xlim=c(70,180), ylim=c(-70,-40))+
  theme_bw()+
  theme(panel.grid=element_blank())+
  sc
ggplotly()


summary(ssm_fmp$g)
####################





####################





####################














