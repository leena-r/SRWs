#run SSM on NZ tracks -- all tracks in one go, 12h time step


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
library(here)

world_map <- map_data("world")

sc <- scale_colour_gradientn(colours = viridis(100), limits=c(0,1))


################################################################

#combine NZ data into one master file
#note that couple of the 2022 cohort are still transmitting

# ##NZ 2020
# Ptt203571_raw <- read_csv(here::here('tag data', 'NZ', '2020', 'datapull 20230607', '203571', "203571-Locations.csv"))
# Ptt203572_raw <- read_csv(here::here('tag data', 'NZ', '2020', 'datapull 20230607','203572', "203572-Locations.csv"))
# Ptt203573_raw <- read_csv(here::here('tag data', 'NZ', '2020', 'datapull 20230607','203573', "203573-Locations.csv"))
# Ptt203574_raw <- read_csv(here::here('tag data', 'NZ', '2020', 'datapull 20230607','203574', "203574-Locations.csv"))
# Ptt203575_raw <- read_csv(here::here('tag data', 'NZ', '2020', 'datapull 20230607','203575', "203575-Locations.csv"))
# Ptt205015_raw <- read_csv(here::here('tag data', 'NZ', '2020', 'datapull 20230607','205015', "205015-Locations.csv"))
# 
# 
# all_ptt_2020 <- bind_rows(Ptt203571_raw, Ptt203572_raw, Ptt203573_raw, Ptt203574_raw, Ptt203575_raw, Ptt205015_raw)
# 
# #Only keep desired columns
# all_ptt_2020 <- all_ptt_2020 %>% 
#   select(DeployID, Ptt, Instr, Date, Type, Quality, Latitude, Longitude, `Error radius`, `Error Semi-major axis`, `Error Semi-minor axis`, `Error Ellipse orientation`)
# 
# 
# all_ptt_2020 <- all_ptt_2020 %>% 
#   mutate(DateTime_UTC = Date) %>%
#   mutate(DateTime_UTC=parse_date_time(DateTime_UTC, "HMS dby")) %>%
#   select(-Date) %>%
#   mutate(Date = as_date(DateTime_UTC)) 
# 
# all_ptt_2020$cohort <- 2020
# 
# 
# 
# ##NZ 2021
# Ptt46633_raw <- read_csv(here::here('tag data', 'NZ', '2021', 'datapull 20230607','46633', "46633-Locations.csv"))
# Ptt46635_raw <- read_csv(here::here('tag data', 'NZ', '2021', 'datapull 20230607','46635', "46635-Locations.csv"))
# Ptt46950_raw <- read_csv(here::here('tag data', 'NZ', '2021', 'datapull 20230607','46950', "46950-Locations.csv"))
# Ptt46955_raw <- read_csv(here::here('tag data', 'NZ', '2021', 'datapull 20230607','46955', "46955-Locations.csv"))
# Ptt212499_raw <- read_csv(here::here('tag data', 'NZ', '2021', 'datapull 20230607','212499', "212499-Locations.csv"))
# Ptt212500_raw <- read_csv(here::here('tag data', 'NZ', '2021', 'datapull 20230607','212500', "212500-Locations.csv"))
# Ptt215258_raw <- read_csv(here::here('tag data', 'NZ', '2021', 'datapull 20230607','215258', "215258-Locations.csv"))
# Ptt215259_raw <- read_csv(here::here('tag data', 'NZ', '2021', 'datapull 20230607','215259', "215259-Locations.csv"))
# Ptt215261_raw <- read_csv(here::here('tag data', 'NZ', '2021', 'datapull 20230607','215261', "215261-Locations.csv"))
# Ptt215262_raw <- read_csv(here::here('tag data', 'NZ', '2021', 'datapull 20230607','215262', "215262-Locations.csv"))
# Ptt215263_raw <- read_csv(here::here('tag data', 'NZ', '2021', 'datapull 20230607','215263', "215263-Locations.csv"))
# 
# all_ptt_2021 <- bind_rows(Ptt46633_raw, Ptt46635_raw, Ptt46950_raw, Ptt46955_raw, Ptt212499_raw,
#                           Ptt212500_raw, Ptt215258_raw, Ptt215259_raw, Ptt215261_raw, Ptt215262_raw, Ptt215263_raw)
# 
# 
# #Only keep desired columns
# all_ptt_2021 <- all_ptt_2021 %>% 
#   select(DeployID, Ptt, Instr, Date, Type, Quality, Latitude, Longitude, `Error radius`, `Error Semi-major axis`, `Error Semi-minor axis`, `Error Ellipse orientation`)
# 
# 
# all_ptt_2021 <- all_ptt_2021 %>% 
#   mutate(DateTime_UTC = Date) %>%
#   mutate(DateTime_UTC=parse_date_time(DateTime_UTC, "HMS dby")) %>%
#   select(-Date) %>%
#   mutate(Date = as_date(DateTime_UTC)) 
# 
# all_ptt_2021$cohort <- 2021
# 
# 
# 
# ##NZ 2022  -- 1 still transmitting, as of 30 Aug 2023
# Ptt197853_raw <- read_csv(here::here('tag data', 'NZ', '2022', 'datapull 20230830', '197853', "197853-Locations.csv"))
# Ptt208742_raw <- read_csv(here::here('tag data', 'NZ', '2022', 'datapull 20230830', '208742', "208742-Locations.csv"))
# Ptt235399_raw <- read_csv(here::here('tag data', 'NZ', '2022', 'datapull 20230830', '235399', "235399-Locations.csv"))
# Ptt235400_raw <- read_csv(here::here('tag data', 'NZ', '2022', 'datapull 20230830', '235400', "235400-Locations.csv"))
# Ptt235401_raw <- read_csv(here::here('tag data', 'NZ', '2022', 'datapull 20230830', '235401', "235401-Locations.csv"))
# Ptt235402_raw <- read_csv(here::here('tag data', 'NZ', '2022', 'datapull 20230830', '235402', "235402-Locations.csv"))
# Ptt235403_raw <- read_csv(here::here('tag data', 'NZ', '2022', 'datapull 20230830', '235403', "235403-Locations.csv"))
# Ptt235404_raw <- read_csv(here::here('tag data', 'NZ', '2022', 'datapull 20230830', '235404', "235404-Locations.csv"))
# 
# 
# all_ptt_2022 <- bind_rows(Ptt197853_raw, Ptt208742_raw, Ptt235399_raw, Ptt235400_raw, 
#                           Ptt235401_raw, Ptt235402_raw, Ptt235403_raw, Ptt235404_raw)
# 
# 
# 
# #Only keep desired columns
# all_ptt_2022 <- all_ptt_2022 %>% 
#   select(DeployID, Ptt, Instr, Date, Type, Quality, Latitude, Longitude, `Error radius`, `Error Semi-major axis`, `Error Semi-minor axis`, `Error Ellipse orientation`)
# 
# 
# all_ptt_2022 <- all_ptt_2022 %>% 
#   mutate(DateTime_UTC = Date) %>%
#   mutate(DateTime_UTC=parse_date_time(DateTime_UTC, "HMS dby")) %>%
#   select(-Date) %>%
#   mutate(Date = as_date(DateTime_UTC)) 
# 
# all_ptt_2022$cohort <- 2022
# 
# 
# 
# raw_argos_df <- rbind(all_ptt_2020,all_ptt_2021,all_ptt_2022)
# 
# 
# #save combined NZ SRW data file - note that 3 2022 tags still going
# #write_rds(raw_argos_df,here::here('SSM', 'data', 'NZ_SRW_2020_2021_2022_raw_argos_df_20230830.rds'))



################################################################


#load in master data file
raw_argos_df <- read_rds(here::here('SSM', 'data', 'NZ_SRW_2020_2021_2022_raw_argos_df_20230830.rds'))

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
  ind <- which(d$time_diff_hours > 36) ##test 24h, 36h...?
  d$mark <- 0
  d$mark[ind] <- 1
  d$track_seg <- cumsum(d$mark)
  return(d)
})

# Now create a new id based on track segment
trackseg_argos_df$track_id <- paste(trackseg_argos_df$id, "-", trackseg_argos_df$track_seg, sep="")

table(trackseg_argos_df$track_id)

length(unique(trackseg_argos_df$track_id)) #123


# remove short track segs, test n <20 
min_obs <- 20 ## set the number of minimum obs acceptable
trackseg_argos_df <- trackseg_argos_df %>% group_by(track_id)
trackseg_argos_df_filt <- filter(trackseg_argos_df, n() >= min_obs)

table(trackseg_argos_df_filt$track_id)
length(unique(trackseg_argos_df_filt$track_id)) # depends on time step and length chosen



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

mean(mts$time_diff_hours) # this is taking mean of a mean, value depnds on time step and short track length chosen

#by annual cohort
time_diff_summary <- ssm_tdiff_hours_df %>% 
  group_by(cohort) %>% #, track_id
  summarise(first=quantile(time_diff_hours,probs=0.25, na.rm = TRUE),
            second=quantile(time_diff_hours,probs=0.5, na.rm = TRUE),
            third=quantile(time_diff_hours,probs=0.75, na.rm = TRUE),
            mean = mean(time_diff_hours, na.rm = TRUE))

#now structure the data frame so it matches the required structure for SSM
#keep but rename error ellipse variables
ssm_df <- ssm_df %>% 
  select(track_id, date, lc, lon, lat, `Error Semi-major axis`, `Error Semi-minor axis`, `Error Ellipse orientation`, cohort, time_diff_hours) %>% 
  dplyr::rename(id = track_id, 
                smaj = `Error Semi-major axis`, 
                smin = `Error Semi-minor axis`, 
                eor = `Error Ellipse orientation`)

#write_rds(ssm_df,here::here('SSM', 'data', 'NZ_SRW_2020_2021_2022_ssm_df_20230830.rds'))


################################################################

#read in file ready for ssm
#ssm_df <- read_rds(here::here('SSM', 'data', 'NZ_SRW_2020_2021_2022_ssm_df_20230711.rds'))

#speed filter threshold (vmax) of 5 ms−1
fit_ssm_12h_model_mp_NZ_all<- fit_ssm(ssm_df, vmax=5, model="mp", time.step=12, control = ssm_control(verbose=0))
## actually this shouldn't work as model=mp should be only for running one track at a time?
#Gin says that it works
#if try to run on NZ 2020, 2021 and 2022 data, 36h gap, 20 locs is short, 12h ssm time step: has warning messages.
#try to identify which track causes the fail? 
  # View(fit_ssm_12h_model_mp_NZ_all)
  #those that have converged == FALSE: 215262-1, 215262-14, 235399-4, 46635-1
  # pdHess == FALSE: 215262-14, 46635-1 --- both are from 2021 cohort
fit_ssm_12h_model_mp_NZ_all_p <-  fit_ssm_12h_model_mp_NZ_all %>%  grab(what="p") 
  # --> logit_g.se == NA: 235399-4
## test mapping this particular out in QGIS
#write_csv(fit_ssm_12h_model_mp_NZ_all_p,here::here('SSM', 'data', 'ssm_mpm_together_all_NZ_gap36h_short20loc_20230904.csv'))
fit_ssm_12h_model_mp_NZ_all_p_groupnormalised <-  fit_ssm_12h_model_mp_NZ_all %>% grab(what="p",normalise = TRUE, group = TRUE)
#write_csv(fit_ssm_12h_model_mp_NZ_all_p_groupnormalised,here::here('SSM', 'data', 'ssm_mpm_together_all_NZ_gap36h_short20loc_GROUPNORMALISED_20230904.csv'))
#######here should first add other columsn: PTT, year, month...

##would there be lots more warnings if used 6hr ssm?
fit_ssm_6h_model_mp_NZ_all<- fit_ssm(ssm_df, vmax=5, model="mp", time.step=6, control = ssm_control(verbose=0))
#runs but has some warnings
#View(fit_ssm_6h_model_mp_NZ_all)
#those that have converged == FALSE: 215259-1, 215262-1, 215262-11, 215262-14, 235399-4, 46635-1 -- mostly the same, 215259 is new
#pdHess == FALSE: 215259-1, 215262-11, 215262-14 -some chanh=ges
fit_ssm_6h_model_mp_NZ_all_p <-  fit_ssm_6h_model_mp_NZ_all %>%  grab(what="p") 
# --> logit_g.se == NA: 215262-1, 235399-4, 235400-0 -- more so 6hr not great


# Xuelei's method steps
fit_ssm_NZ_all_12h <- fit_ssm(ssm_df,vmax = 25,model="crw",time.step = 12,control = ssm_control(verbose=0))
print(fit_ssm_NZ_all_12h)
ssm_NZ_all_12h_df <- grab(x=fit_ssm_NZ_all_12h,what = "p")
#aniMotum::map(fit_ssm_NZ_all_12h,what = "predicted",by.date=F,aes = aes_lst(mp=FALSE,fill=c("dodgerblue", "dodgerblue", NA, "orange", "grey60", NA),conf = F,date_pal = hcl.colors(100, palette = "Viridis", rev = FALSE)))


# move persistence model 
mpm_NZ_all_12h <- fit_mpm(fit_ssm_NZ_all_12h,model="mpm",control = mpm_control(verbose = 0))
mpm_NZ_all_12h
mpm_NZ_all_12h_df <- grab(x=mpm_NZ_all_12h,what = "f") 
#plot(mpm_NZ_all_12h, control = mpm_control(verbose=0),ask=F)


ssm_mpm_NZ_all_12h <- left_join(ssm_NZ_all_12h_df, mpm_NZ_all_12h_df, by=c('id','date'))

ggplot(ssm_mpm_NZ_all_12h, aes(lon, lat)) +
  geom_point(size=1, aes(col = g)) +
  geom_polygon(data = world_map, aes(x=long, y=lat, group=group), fill="black") +
  coord_equal() + 
  coord_fixed(xlim=c(80,200), ylim=c(-70,-40))+
  theme_bw()+
  theme(panel.grid=element_blank())+
  sc
ggplotly()



## unsure if the full dataset work together
#try 2021 separately

#read in file ready for ssm
#ssm_df <- read_rds(here::here('SSM', 'data', 'NZ_SRW_2020_2021_2022_ssm_df_20230711.rds'))
ssm_2021_df <- ssm_df %>% filter(cohort==2021)


# Xuelei's method steps
fit_ssm_NZ_2021_12h <- fit_ssm(ssm_2021_df, vmax=25, model="crw", time.step=12, control = ssm_control(verbose=0),map = list(psi = factor(NA))) 
#no complaints
print(fit_ssm_NZ_2021_12h)
ssm_NZ_2021_12h_df <- grab(x=fit_ssm_NZ_2021_12h,what = "p")
#aniMotum::map(fit_ssm_NZ_2021_12h,what = "predicted",by.date=F,aes = aes_lst(mp=FALSE,fill=c("dodgerblue", "dodgerblue", NA, "orange", "grey60", NA),conf = F,date_pal = hcl.colors(100, palette = "Viridis", rev = FALSE)))


# move persistence model 
mpm_NZ_2021_12h <- fit_mpm(fit_ssm_NZ_2021_12h,model="mpm",control = mpm_control(verbose = 0))
#some complaints 
# Warning messages:
# 1: In sqrt(diag(object$cov.fixed)) : NaNs produced
# 2: In sqrt(diag(object$cov.fixed)) : NaNs produced
mpm_NZ_2021_12h
mpm_NZ_2021_12h_df <- grab(x=mpm_NZ_2021_12h,what = "f") 
#plot(mpm_NZ_2021_12h, control = mpm_control(verbose=0),ask=F)


ssm_mpm_NZ_2021_12h <- left_join(ssm_NZ_2021_12h_df, mpm_NZ_2021_12h_df, by=c('id','date'))

ggplot(ssm_mpm_NZ_2021_12h, aes(lon, lat)) +
  geom_point(size=1, aes(col = g)) +
  geom_polygon(data = world_map, aes(x=long, y=lat, group=group), fill="black") +
  coord_equal() + 
  coord_fixed(xlim=c(80,200), ylim=c(-70,-40))+
  theme_bw()+
  theme(panel.grid=element_blank())+
  sc
ggplotly()




### the one that went east
#comparing sunning ssm and mp separately vs together

id_46950_0 <- ssm_df %>% filter(id == "46950-0")

# Xuelei's method steps
fit_ssm_46950_12h <- fit_ssm(id_46950_0, vmax=25, model="crw", time.step=12, control = ssm_control(verbose=0),map = list(psi = factor(NA))) 
#no complaints
print(fit_ssm_46950_12h)
ssm_fit_ssm_46950_12h_df <- grab(x=fit_ssm_46950_12h,what = "p")
#aniMotum::map(fit_ssm_46950_12h,what = "predicted",by.date=F,aes = aes_lst(mp=FALSE,fill=c("dodgerblue", "dodgerblue", NA, "orange", "grey60", NA),conf = F,date_pal = hcl.colors(100, palette = "Viridis", rev = FALSE)))


# move persistence model 
mpm_46950_12h <- fit_mpm(fit_ssm_46950_12h,model="mpm",control = mpm_control(verbose = 0))
mpm_46950_12h
mpm_46950_12h_df <- grab(x=mpm_46950_12h,what = "f") 
#plot(mpm_46950_12h, control = mpm_control(verbose=0),ask=F)


ssm_mpm_separate_46950_12h <- left_join(ssm_fit_ssm_46950_12h_df, mpm_46950_12h_df, by=c('id','date'))

ssm_mpm_separate_46950_12h <- ssm_mpm_separate_46950_12h %>% rename(g_separate = g) %>% 
  mutate(mode = case_when(g_separate < quantile(g_separate, probs = 0.25)   ~ "ARS",
                          g_separate >= quantile(g_separate, probs = 0.25)  ~ "transit")) 
#write_csv(ssm_mpm_separate_46950_12h,here::here('SSM', 'data', 'ssm_mpm_separate_46950_12h.csv'))


##mp as part of ssm

#speed filter threshold (vmax) of 5 ms−1
fit_ssm_with_model_mp_46950_12h <- fit_ssm(id_46950_0, vmax=25, model="mp", time.step=12, control = ssm_control(verbose=0)) %>% 
  grab(what="p") 
#plot(fit_ssm_with_model_mp_46950_12h, what = "predicted", type = 3, normalise = FALSE)
ggplot(data.frame(fit_ssm_with_model_mp_46950_12h),aes(lon, lat)) +
  geom_point(size=1, aes(col = g)) +
  geom_polygon(data = world_map, aes(x=long, y=lat, group=group), fill="black") +
  coord_equal() + 
  coord_fixed(xlim=c(80,180), ylim=c(-70,-40))+
  theme_bw()+
  theme(panel.grid=element_blank())+
  sc
ggplotly()


#write_csv(fit_ssm_with_model_mp_46950_12h,here::here('SSM', 'data', 'fit_ssm_with_model_mp_46950_12h.csv'))
fit_ssm_with_model_mp_46950_12h <- fit_ssm_with_model_mp_46950_12h %>% rename(g_ssm_mp_together = g) %>% 
  mutate(mode = case_when(g_ssm_mp_together < quantile(g_ssm_mp_together, probs = 0.25)   ~ "ARS",
                          g_ssm_mp_together >= quantile(g_ssm_mp_together, probs = 0.25)  ~ "transit"))

test_join <- left_join(ssm_mpm_separate_46950_12h, fit_ssm_with_model_mp_46950_12h, by=c('id','date'))











