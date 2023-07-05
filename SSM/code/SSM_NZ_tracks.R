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

################################


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


all_ptt_2020 <- all_ptt_2020 %>% 
  mutate(DateTime_UTC = Date) %>%
  mutate(DateTime_UTC=parse_date_time(DateTime_UTC, "HMS dby")) %>%
  select(-Date) %>%
  mutate(Date = as_date(DateTime_UTC)) 

#Add extra column for month
all_ptt_2020 <- all_ptt_2020 %>% 
  mutate(Month = month(DateTime_UTC))
all_ptt_2020$cohort <- 2020



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



























