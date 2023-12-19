#run SSM on NZ tracks -- all tracks in one go using fit_smm(model=mp), 12h time step


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


######################################################################################
######################################################################################
########################### NZ TRACKS  #######################################
######################################################################################
######################################################################################

#combine NZ data into one master file
#all tags done transmittiing

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
# ##NZ 2022  
# Ptt197853_raw <- read_csv(here::here('tag data', 'NZ', '2022', 'datapull 20231006', '197853', "197853-Locations.csv"))
# Ptt208742_raw <- read_csv(here::here('tag data', 'NZ', '2022', 'datapull 20231006', '208742', "208742-Locations.csv"))
# Ptt235399_raw <- read_csv(here::here('tag data', 'NZ', '2022', 'datapull 20231006', '235399', "235399-Locations.csv"))
# Ptt235400_raw <- read_csv(here::here('tag data', 'NZ', '2022', 'datapull 20231006', '235400', "235400-Locations.csv"))
# Ptt235401_raw <- read_csv(here::here('tag data', 'NZ', '2022', 'datapull 20231006', '235401', "235401-Locations.csv"))
# Ptt235402_raw <- read_csv(here::here('tag data', 'NZ', '2022', 'datapull 20231006', '235402', "235402-Locations.csv"))
# Ptt235403_raw <- read_csv(here::here('tag data', 'NZ', '2022', 'datapull 20231006', '235403', "235403-Locations.csv"))
# Ptt235404_raw <- read_csv(here::here('tag data', 'NZ', '2022', 'datapull 20231006', '235404', "235404-Locations.csv"))
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
# #save combined NZ SRW data file - all tags done transmitting
# #write_rds(raw_argos_df,here::here('SSM', 'data', 'NZ_SRW_2020_2021_2022_raw_argos_df_20231218.rds'))



################################################################


#load in master data file
raw_argos_df <- read_rds(here::here('SSM', 'data', 'NZ_SRW_2020_2021_2022_raw_argos_df_20231218.rds'))

#change column names to match Xuelei code
#also convert longitude from 0-180 to 0-360
raw_argos_df <- raw_argos_df %>% 
  dplyr::rename(id = Ptt,
                lat = Latitude,
                #lon = Longitude,
                lc = Quality,
                date = DateTime_UTC) %>% 
  mutate(lon = ifelse(Longitude <0, 360-Longitude*-1, Longitude))


#remove the poor quality locations -- doesn't really matter if do this here or later, but jsut do it here
raw_argos_df <- raw_argos_df %>% 
  filter (lc != "Z")


################################################################

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
  ind <- which(d$time_diff_hours > 36) 
  d$mark <- 0
  d$mark[ind] <- 1
  d$track_seg <- cumsum(d$mark)
  return(d)
})

# Now create a new id based on track segment
trackseg_argos_df$track_id <- paste(trackseg_argos_df$id, "-", trackseg_argos_df$track_seg, sep="")

table(trackseg_argos_df$track_id)

length(unique(trackseg_argos_df$track_id)) 


# remove short track segs, test n < 25 
min_obs <- 25 ## set the number of minimum obs acceptable
trackseg_argos_df <- trackseg_argos_df %>% group_by(track_id)
trackseg_argos_df_filt <- filter(trackseg_argos_df, n() >= min_obs)

table(trackseg_argos_df_filt$track_id)
length(unique(trackseg_argos_df_filt$track_id)) # depends on time step and length chosen



################################################################

#SSM


ssm_df <- trackseg_argos_df_filt


#now structure the data frame so it matches the required structure for SSM
#keep but rename error ellipse variables
ssm_df <- ssm_df %>% 
  select(track_id, date, lc, lon, lat, `Error Semi-major axis`, `Error Semi-minor axis`, `Error Ellipse orientation`, cohort, time_diff_hours) %>% 
  dplyr::rename(id = track_id, 
                smaj = `Error Semi-major axis`, 
                smin = `Error Semi-minor axis`, 
                eor = `Error Ellipse orientation`)



################################################################


#NZ 2020, 2021 and 2022 data, 36h gap, 25 locs is short track, 12h ssm time step: 

#speed filter threshold (vmax) of 5 ms−1
fit_ssm_12h_model_mp_NZ_all<- fit_ssm(ssm_df, vmax=5, model="mp", time.step=12, control = ssm_control(verbose=0))
## based on animotum documentation this shouldn't work as model=mp should be only for running one track at a time
#but it does work on grouped data
  ##when short track is < 25
  #write_rds(fit_ssm_12h_model_mp_NZ_all,here::here('SSM', 'data', 'fit_ssm_12h_model_mp_NZ_all_20231218.rds')) ##short track <25
  ##when short track is < 20
  #write_rds(fit_ssm_12h_model_mp_NZ_all,here::here('SSM', 'data', 'fit_ssm_12h_model_mp_NZ_all_20230912.rds')) ##short track <20

  ##when short track is < 25
  ##couple warnings:
  #  Warning messages:
  # 1: Hessian was not positive-definite so some standard errors could not be calculated. Try simplifying the model by adding the following argument:
  #   `map = list(psi = factor(NA))` 
  # 2: Hessian was not positive-definite so some standard errors could not be calculated. Try simplifying the model by adding the following argument:
  #   `map = list(psi = factor(NA))` 
  # 3: The optimiser failed. Try simplifying the model with the following argument: 
  #   `map = list(psi = factor(NA))` 
  # 4: Hessian was not positive-definite so some standard errors could not be calculated. Try simplifying the model by adding the following argument:
  #   `map = list(psi = factor(NA))` 
  # 5: In sqrt(as.numeric(object$diag.cov.random)) : NaNs produced
  # 6: Hessian was not positive-definite so some standard errors could not be calculated. Try simplifying the model by adding the following argument:
  #   `map = list(psi = factor(NA))` 
    # View(fit_ssm_12h_model_mp_NZ_all)
    #those that have converged == FALSE: 215262-1, 215262-14
    # pdHess == FALSE: 215262-14

  ##when short track is < 20
  # Warning messages:
  #   1: Hessian was not positive-definite so some standard errors could not be calculated. Try simplifying the model by adding the following argument:
  #   `map = list(psi = factor(NA))` 
  # 2: Hessian was not positive-definite so some standard errors could not be calculated. Try simplifying the model by adding the following argument:
  #   `map = list(psi = factor(NA))` 
  # 3: The optimiser failed. Try simplifying the model with the following argument: 
  #   `map = list(psi = factor(NA))` 
  # 4: In sqrt(as.numeric(object$diag.cov.random)) : NaNs produced
  # 5: Hessian was not positive-definite so some standard errors could not be calculated. Try simplifying the model by adding the following argument:
  #   `map = list(psi = factor(NA))` 
  # 6: Hessian was not positive-definite so some standard errors could not be calculated. Try simplifying the model by adding the following argument:
  #   `map = list(psi = factor(NA))` 
  # 7: Hessian was not positive-definite so some standard errors could not be calculated. Try simplifying the model by adding the following argument:
  #   `map = list(psi = factor(NA))` 
  # 8: In sqrt(as.numeric(object$diag.cov.random)) : NaNs produced
  # 9: Hessian was not positive-definite so some standard errors could not be calculated. Try simplifying the model by adding the following argument:
  #   `map = list(psi = factor(NA))` 
  # 10: The optimiser failed. Try simplifying the model with the following argument: 
  #   `map = list(psi = factor(NA))` 
    # View(fit_ssm_12h_model_mp_NZ_all)
    #those that have converged == FALSE: 215262-1, 215262-14, 235399-4, 46635-1
    # pdHess == FALSE: 215262-14, 46635-1


# fit_ssm_12h_model_mp_NZ_all_p_groupnormalised <-  fit_ssm_12h_model_mp_NZ_all %>% grab(what="p",normalise = TRUE, group = TRUE)
# # --> logit_g.se == NA: NONE
# 
# 
# #add other columns to data: PTT, year, month, tagging cohort...
# fit_ssm_12h_model_mp_NZ_all_p_groupnormalised_v2 <- fit_ssm_12h_model_mp_NZ_all_p_groupnormalised %>% 
#   mutate(PTT = id) %>% 
#   separate(col=PTT, into=c('PTT', 'leftovers'), sep='-') %>% 
#   select(-leftovers) %>% 
#   mutate(Year = lubridate::year(date)) %>% 
#   mutate(Month = month(date)) %>% 
#   mutate(cohort = case_when(PTT %in%  c("203571", "203572", "203573", "203574", "203575", "205015")  ~ "NZ 2020",
#                             PTT %in%  c("46633", "46635", "46950", "46955", "212499", "212500", 
#                                       "215258" , "215259" , "215261" , "215262", "215263")  ~ "NZ 2021",
#                             PTT %in%  c("197853", "208742", "235399", "235400", 
#                                       "235401" , "235402" , "235403" , "235404")  ~ "NZ 2022"))   

## save and map in QGIS
#write_csv(fit_ssm_12h_model_mp_NZ_all_p_groupnormalised_v2,here::here('SSM', 'data', 'ssm_mpm_all_NZ_SRW_normalised_20230906.csv'))


#(fit_ssm_12h_model_mp_NZ_all_p_groupnormalised_v2$g)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.0000  0.7906  0.8885  0.8519  0.9492  1.0000 


### 
### same model but don't normalise ### 
fit_ssm_12h_model_mp_NZ_all_p <-  fit_ssm_12h_model_mp_NZ_all %>% grab(what="p")
  ##when short track is < 25
  # --> logit_g.se == NA: NONE
  ##when short track is < 20
  # --> logit_g.se == NA: 235399-4
#add other columns to data: PTT, year, month, tagging cohort...
fit_ssm_12h_model_mp_NZ_all_p_v2 <- fit_ssm_12h_model_mp_NZ_all_p %>% 
  mutate(PTT = id) %>% 
  separate(col=PTT, into=c('PTT', 'leftovers'), sep='-') %>% 
  select(-leftovers) %>% 
  mutate(Year = lubridate::year(date)) %>% 
  mutate(Month = month(date)) %>% 
  mutate(cohort = case_when(PTT %in%  c("203571", "203572", "203573", "203574", "203575", "205015")  ~ "NZ 2020",
                            PTT %in%  c("46633", "46635", "46950", "46955", "212499", "212500", 
                                        "215258" , "215259" , "215261" , "215262", "215263")  ~ "NZ 2021",
                            PTT %in%  c("197853", "208742", "235399", "235400", 
                                        "235401" , "235402" , "235403" , "235404")  ~ "NZ 2022"))   
## save and map in QGIS
  ##when short track is < 25
  #write_csv(fit_ssm_12h_model_mp_NZ_all_p_v2,here::here('SSM', 'data', 'ssm_mpm_all_NZ_SRW_20231218.csv'))
summary(fit_ssm_12h_model_mp_NZ_all_p_v2$g)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.0109  0.7947  0.8920  0.8549  0.9504  1.0000 
  ##when short track is < 20
  #write_csv(fit_ssm_12h_model_mp_NZ_all_p_v2,here::here('SSM', 'data', 'ssm_mpm_all_NZ_SRW_20230912.csv'))
summary(fit_ssm_12h_model_mp_NZ_all_p_v2$g)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.0109  0.7932  0.8897  0.8537  0.9498  1.0000 







##would there be lots more warnings if used 6hr ssm? (gap 36h, short <25)s
fit_ssm_6h_model_mp_NZ_all<- fit_ssm(ssm_df, vmax=5, model="mp", time.step=6, control = ssm_control(verbose=0))
#runs but has some warnings
# Warning messages:
# 1: The optimiser failed. Try simplifying the model with the following argument: 
#   `map = list(psi = factor(NA))` 
# 2: Hessian was not positive-definite so some standard errors could not be calculated. Try simplifying the model by adding the following argument:
#   `map = list(psi = factor(NA))` 
# 3: The optimiser failed. Try simplifying the model with the following argument: 
#   `map = list(psi = factor(NA))` 
# 4: The optimiser failed. Try simplifying the model with the following argument: 
#   `map = list(psi = factor(NA))` 
# 5: In sqrt(as.numeric(object$diag.cov.random)) : NaNs produced
# 6: Hessian was not positive-definite so some standard errors could not be calculated. Try simplifying the model by adding the following argument:
#   `map = list(psi = factor(NA))` 
#View(fit_ssm_6h_model_mp_NZ_all)
#those that have converged == FALSE: 215259-1, 215262-1, 215262-11, 215262-14
#pdHess == FALSE: 215259-1, 215262-11 
fit_ssm_6h_model_mp_NZ_all_p <-  fit_ssm_6h_model_mp_NZ_all %>%  grab(what="p") 
# --> logit_g.se == NA: 215262-1, 235400-0 -- more warnings, so 6hr not great



###################
##as a test comparison, run few tracks individually with fit_ssm(model=mp) and compare
#compare using the non-normalised data
test_203571_0 <- ssm_df %>% filter(id == "203571-0")
fit_ssm_12h_model_mp_test_203571_0 <- fit_ssm(test_203571_0, vmax=5, model="mp", time.step=12, control = ssm_control(verbose=0))
fit_ssm_12h_model_mp_test_203571_0_p <-  fit_ssm_12h_model_mp_test_203571_0 %>% grab(what="p")
#write_csv(fit_ssm_12h_model_mp_test_203571_0_p,here::here('SSM', 'data', 'ssm_mpm_203571_0.csv'))
#-- g values are same as in group run

test_203571_1 <- ssm_df %>% filter(id == "203571-1")
fit_ssm_12h_model_mp_test_203571_1 <- fit_ssm(test_203571_1, vmax=5, model="mp", time.step=12, control = ssm_control(verbose=0))
fit_ssm_12h_model_mp_test_203571_1_p <-  fit_ssm_12h_model_mp_test_203571_1 %>% grab(what="p")
#write_csv(fit_ssm_12h_model_mp_test_203571_1_p,here::here('SSM', 'data', 'ssm_mpm_203571_1.csv'))


test_215262_14 <- ssm_df %>% filter(id == "215262-14")
fit_ssm_12h_model_mp_test_215262_14 <- fit_ssm(test_215262_14, vmax=5, model="mp", time.step=12, control = ssm_control(verbose=0))
# Warning message:
#   The optimiser failed. Try simplifying the model with the following argument: 
#   `map = list(psi = factor(NA))` 
fit_ssm_12h_model_mp_test_215262_14_p <-  fit_ssm_12h_model_mp_test_215262_14 %>% grab(what="p")
#Error in x$ssm[[1]] : subscript out of bounds
#doesn't run on its own, has issues --> it actually doesn't run in the group one either

test_215262_10 <- ssm_df %>% filter(id == "215262-10")
fit_ssm_12h_model_mp_test_215262_10 <- fit_ssm(test_215262_10, vmax=5, model="mp", time.step=12, control = ssm_control(verbose=0))
fit_ssm_12h_model_mp_test_215262_10_p <-  fit_ssm_12h_model_mp_test_215262_10 %>% grab(what="p")
#write_csv(fit_ssm_12h_model_mp_test_215262_10_p,here::here('SSM', 'data', 'ssm_mpm_215262_10.csv'))
#-- g values are same as in group run

######################################################################################
######################################################################################





######################################################################################
######################################################################################
########################### OZ TRACKS  #######################################
######################################################################################
######################################################################################
#########################################################################################
##run model on OZ tracks 

###########
#combine OZ data into one master file
#note that couple of the tags are still transmitting

# ##OZ 2022
# Ptt235405_raw <- read_csv(here::here('tag data', 'OZ', '2022', 'datapull 20231006', '235405', "235405-Locations.csv"))
# Ptt235407_raw <- read_csv(here::here('tag data', 'OZ', '2022', 'datapull 20231006', '235407', "235407-Locations.csv"))
# Ptt235410_raw <- read_csv(here::here('tag data', 'OZ', '2022', 'datapull 20231006', '235410', "235410-Locations.csv"))
# Ptt235413_raw <- read_csv(here::here('tag data', 'OZ', '2022', 'datapull 20231006', '235413', "235413-Locations.csv"))
# Ptt235414_raw <- read_csv(here::here('tag data', 'OZ', '2022', 'datapull 20231006', '235414', "235414-Locations.csv"))
# Ptt235621_raw <- read_csv(here::here('tag data', 'OZ', '2022', 'datapull 20231006', '235621', "235621-Locations.csv"))
# 
# 
# all_ptt_OZ_2022 <- bind_rows(Ptt235405_raw, Ptt235407_raw, Ptt235410_raw,
#                              Ptt235413_raw, Ptt235414_raw, Ptt235621_raw)
# 
# #Only keep desired columns
# all_ptt_OZ_2022 <- all_ptt_OZ_2022 %>% 
#   select(DeployID, Ptt, Instr, Date, Type, Quality, Latitude, Longitude, `Error radius`, `Error Semi-major axis`, `Error Semi-minor axis`, `Error Ellipse orientation`)
# 
# 
# all_ptt_OZ_2022 <- all_ptt_OZ_2022 %>% 
#   mutate(DateTime_UTC = Date) %>%
#   mutate(DateTime_UTC=parse_date_time(DateTime_UTC, "HMS dby")) %>%
#   select(-Date) %>%
#   mutate(Date = as_date(DateTime_UTC)) 
# 
# all_ptt_OZ_2022$cohort <- 2022
# 
# 
# #save combined OZ SRW data file - note that some tags still going
# #write_rds(all_ptt_OZ_2022,here::here('SSM', 'data', 'OZ_SRW_2022_raw_argos_df_20231116.rds'))


# ##OZ 2023 --Norngerin still transmitting
# Ptt235408_raw <- read_csv(here::here('tag data', 'OZ', '2023', 'datapull 20231220', '235408', "235408-Locations.csv"))
# Ptt235409_raw <- read_csv(here::here('tag data', 'OZ', '2023', 'datapull 20231220', '235409', "235409-Locations.csv"))
# Ptt235411_raw <- read_csv(here::here('tag data', 'OZ', '2023', 'datapull 20231220', '235411', "235411-Locations.csv"))
# Ptt235412_raw <- read_csv(here::here('tag data', 'OZ', '2023', 'datapull 20231220', '235412', "235412-Locations.csv"))
# Ptt245751_raw <- read_csv(here::here('tag data', 'OZ', '2023', 'datapull 20231220', '245751', "245751-Locations.csv"))
# Ptt245752_raw <- read_csv(here::here('tag data', 'OZ', '2023', 'datapull 20231220', '245752', "245752-Locations.csv"))
# Ptt245754_raw <- read_csv(here::here('tag data', 'OZ', '2023', 'datapull 20231220', '245754', "245754-Locations.csv"))

#  
# all_ptt_OZ_2023 <- bind_rows(Ptt235408_raw, Ptt235409_raw, Ptt235411_raw,
#                              Ptt235412_raw, Ptt245751_raw, Ptt245752_raw, Ptt245754_raw)
# 
# #Only keep desired columns
# all_ptt_OZ_2023 <- all_ptt_OZ_2023 %>% 
#   select(DeployID, Ptt, Instr, Date, Type, Quality, Latitude, Longitude, `Error radius`, `Error Semi-major axis`, `Error Semi-minor axis`, `Error Ellipse orientation`)
# 
# 
# all_ptt_OZ_2023 <- all_ptt_OZ_2023 %>% 
#   mutate(DateTime_UTC = Date) %>%
#   mutate(DateTime_UTC=parse_date_time(DateTime_UTC, "HMS dby")) %>%
#   select(-Date) %>%
#   mutate(Date = as_date(DateTime_UTC)) 
# 
# all_ptt_OZ_2023$cohort <- 2023
# 
# 
# #save combined OZ SRW data file - note that some tags still going
# #write_rds(all_ptt_OZ_2023,here::here('SSM', 'data', 'OZ_SRW_2023_raw_argos_df_20231220.rds'))


#raw_argos_df <- rbind(all_ptt_OZ_2022,all_ptt_OZ_2023)
# 
# 
# #save combined NZ SRW data file - all tags done transmitting
# #write_rds(raw_argos_df,here::here('SSM', 'data', 'OZ_SRW_2022_2023_raw_argos_df_20231220.rds'))



################################################################


#load in master data file
raw_OZ_argos_df <- read_rds(here::here('SSM', 'data', 'OZ_SRW_2022_2023_raw_argos_df_20231220.rds'))

#change column names to match Xuelei code
#also convert longitude from 0-180 to 0-360
raw_OZ_argos_df <- raw_OZ_argos_df %>% 
  dplyr::rename(id = Ptt,
                lat = Latitude,
                #lon = Longitude,
                lc = Quality,
                date = DateTime_UTC) %>% 
  mutate(lon = ifelse(Longitude <0, 360-Longitude*-1, Longitude))


#remove the poor quality locations -- doesn't really matter if do this here or later, but jsut do it here
raw_OZ_argos_df <- raw_OZ_argos_df %>% 
  filter (lc != "Z")


################################################################


# SDA filter - leave this to fit_ssm


################################################################

#remove duplicates - leave this to fit_ssm


################################################################


#Time difference between successive locations

OZ_time_diff_hours_df <- ddply(raw_OZ_argos_df, ~id, function(d){
  d$time_diff_hours <- NA
  for (i in 2:nrow(d)){
    d$time_diff_hours[i] = as.numeric(difftime(d$date[i], d$date[i-1], units = "hours"))}
  return(d)
})


ggplot(OZ_time_diff_hours_df, aes(time_diff_hours)) + 
  geom_histogram(binwidth = 1, col ="white", na.rm = T) + 
  theme_bw() + xlim(c(0,100)) + 
  xlab("Time difference between successive locations")

ggplot(OZ_time_diff_hours_df, aes(time_diff_hours)) + 
  geom_histogram(binwidth = 1, col ="white", na.rm = T) + 
  theme_bw() + xlim(c(0,15)) + 
  xlab("Time difference between successive locations")



################################################################

#Segment tracks

OZ_trackseg_argos_df <- ddply(OZ_time_diff_hours_df, ~id, function(d){
  ind <- which(d$time_diff_hours > 36)
  d$mark <- 0
  d$mark[ind] <- 1
  d$track_seg <- cumsum(d$mark)
  return(d)
})

# Now create a new id based on track segment
OZ_trackseg_argos_df$track_id <- paste(OZ_trackseg_argos_df$id, "-", OZ_trackseg_argos_df$track_seg, sep="")

table(OZ_trackseg_argos_df$track_id)

length(unique(OZ_trackseg_argos_df$track_id)) 


# remove short track segs, test n <25 
min_obs <- 25 ## set the number of minimum obs acceptable
OZ_trackseg_argos_df <- OZ_trackseg_argos_df %>% group_by(track_id)
OZ_trackseg_argos_df_filt <- filter(OZ_trackseg_argos_df, n() >= min_obs)

table(OZ_trackseg_argos_df_filt$track_id)
length(unique(OZ_trackseg_argos_df_filt$track_id)) 


################################################################

#SSM


OZ_ssm_df <- OZ_trackseg_argos_df_filt


#now structure the data frame so it matches the required structure for SSM
#keep but rename error ellipse variables
OZ_ssm_df <- OZ_ssm_df %>% 
  select(track_id, date, lc, lon, lat, `Error Semi-major axis`, `Error Semi-minor axis`, `Error Ellipse orientation`, cohort, time_diff_hours) %>% 
  dplyr::rename(id = track_id, 
                smaj = `Error Semi-major axis`, 
                smin = `Error Semi-minor axis`, 
                eor = `Error Ellipse orientation`)



################################################################
####           OZ 2022 & 2023 joined run         ##############
################################################################

#OZ 2022 and 2023 data, 36h gap, 25 locs is short track, 12h ssm time step: 

#speed filter threshold (vmax) of 5 ms−1
fit_ssm_12h_model_mp_OZ <- fit_ssm(OZ_ssm_df, vmax=5, model="mp", time.step=12, control = ssm_control(verbose=0))
## based on animotum documentation this shouldn't work as model=mp should be only for running one track at a time
#but it does work on grouped data

# Warning message:
#   Hessian was not positive-definite so some standard errors could not be calculated. Try simplifying the model by adding the following argument:
#   `map = list(psi = factor(NA))`

# View(fit_ssm_12h_model_mp_OZ)
#those that have converged == FALSE: 245754-1
# pdHess == FALSE: NONE



# fit_ssm_12h_model_mp_OZ_p_groupnormalised <-  fit_ssm_12h_model_mp_OZ %>% grab(what="p",normalise = TRUE, group = TRUE)
# # --> logit_g.se == NA: NONE
# 
# 
# #add other columns to data: PTT, year, month, tagging cohort...
# fit_ssm_12h_model_mp_OZ_p_groupnormalised_v2 <- fit_ssm_12h_model_mp_OZ_p_groupnormalised %>% 
#   mutate(PTT = id) %>% 
#   separate(col=PTT, into=c('PTT', 'leftovers'), sep='-') %>% 
#   select(-leftovers) %>% 
#   mutate(Year = lubridate::year(date)) %>% 
#   mutate(Month = month(date)) 
# fit_ssm_12h_model_mp_OZ_p_groupnormalised_v2$cohort <- 2022
# 
# ## save and map in QGIS --- this has not been updated as dont think should use normalised data
# #write_csv(fit_ssm_12h_model_mp_OZ_p_groupnormalised_v2,here::here('SSM', 'data', 'ssm_mpm_OZ_SRW_normalised_20230906.csv'))
# 
# 
# summary(fit_ssm_12h_model_mp_OZ_p_groupnormalised_v2$g)
# # Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# #0.0000  0.7220  0.8440  0.8102  0.9230  1.0000 


### 
### same model but don't normalise ### 
fit_ssm_12h_model_mp_OZ_p <-  fit_ssm_12h_model_mp_OZ %>% grab(what="p")
# --> logit_g.se == NA: NONE
#add other columns to data: PTT, year, month, tagging cohort...
fit_ssm_12h_model_mp_OZ_p_v2 <- fit_ssm_12h_model_mp_OZ_p %>% 
  mutate(PTT = id) %>% 
  separate(col=PTT, into=c('PTT', 'leftovers'), sep='-') %>% 
  select(-leftovers) %>% 
  mutate(Year = lubridate::year(date)) %>% 
  mutate(Month = month(date)) %>% 
  mutate(cohort = case_when(PTT %in%  c("235405", "235407",  "235410", 
                                        "235413", "235414", "235621" )  ~ "OZ 2022",
                            PTT %in%  c("235408", "235409", "235411", "235412", 
                                        "245751" , "245752" , "245754" )  ~ "OZ 2023"))

## save and map in QGIS
#write_csv(fit_ssm_12h_model_mp_OZ_p_v2,here::here('SSM', 'data', 'ssm_mpm_all_OZ_SRW_20231220.csv'))
summary(fit_ssm_12h_model_mp_OZ_p_v2$g)
# Min.   1st Qu.  Median  Mean 3rd Qu.    Max. 
#0.1181  0.7773  0.8743  0.8441  0.9361  0.9921  








######################################################################################
######################################################################################
######################################################################################
######################################################################################
######################################################################################
######################################################################################







################################################################
################################################################

##load in Mackay et al 2020 datasets


### HoB ######

load("C:/Users/lrie0/Documents/SRW projects/SRWs/tag data/Mackay et al 2020 data/raw_argos_df_HoB.RData")
glimpse(raw_argos_df_HoB)
#write_csv(raw_argos_df_HoB,here::here('SSM', 'data', 'raw_argos_df_HoB.csv'))

#column names are already what they should be, and data does not cross 180 meridian

#remove the poor quality locations -- doesn't really matter if do this here or later, but jsut do it here
raw_argos_df_HoB <- raw_argos_df_HoB %>% 
  filter (lc != "Z")


################################################################

# SDA filter - leave this to fit_ssm


################################################################

#remove duplicates - leave this to fit_ssm


################################################################


#Time difference between successive locations

HoB_time_diff_hours_df <- ddply(raw_argos_df_HoB, ~id, function(d){
  d$time_diff_hours <- NA
  for (i in 2:nrow(d)){
    d$time_diff_hours[i] = as.numeric(difftime(d$date[i], d$date[i-1], units = "hours"))}
  return(d)
})



ggplot(HoB_time_diff_hours_df, aes(time_diff_hours)) + 
  geom_histogram(binwidth = 1, col ="white", na.rm = T) + 
  theme_bw() + xlim(c(0,100)) + 
  xlab("Time difference between successive locations")

ggplot(HoB_time_diff_hours_df, aes(time_diff_hours)) + 
  geom_histogram(binwidth = 1, col ="white", na.rm = T) + 
  theme_bw() + xlim(c(0,15)) + 
  xlab("Time difference between successive locations")


################################################################

#Segment tracks

HoB_trackseg_argos_df <- ddply(HoB_time_diff_hours_df, ~id, function(d){
  ind <- which(d$time_diff_hours > 36) 
  d$mark <- 0
  d$mark[ind] <- 1
  d$track_seg <- cumsum(d$mark)
  return(d)
})

# Now create a new id based on track segment
HoB_trackseg_argos_df$track_id <- paste(HoB_trackseg_argos_df$id, "-", HoB_trackseg_argos_df$track_seg, sep="")

table(HoB_trackseg_argos_df$track_id)

length(unique(HoB_trackseg_argos_df$track_id)) 


# remove short track segs, test n <25 
min_obs <- 25 ## set the number of minimum obs acceptable
HoB_trackseg_argos_df <- HoB_trackseg_argos_df %>% group_by(track_id)
HoB_trackseg_argos_df_filt <- filter(HoB_trackseg_argos_df, n() >= min_obs)

table(HoB_trackseg_argos_df_filt$track_id)
length(unique(HoB_trackseg_argos_df_filt$track_id)) # depends on time step and length chosen



################################################################

#SSM


HoB_ssm_df <- HoB_trackseg_argos_df_filt


#now structure the data frame so it matches the required structure for SSM
#this data did not have error ellipse variables
HoB_ssm_df <- HoB_ssm_df %>% 
  select(track_id, date, lc, lon, lat, time_diff_hours) %>% 
  dplyr::rename(id = track_id)



################################################################


#HoB data, 36h gap, 25 locs is short track, 12h ssm time step: 

#speed filter threshold (vmax) of 5 ms−1
fit_ssm_12h_model_mp_HoB<- fit_ssm(HoB_ssm_df, vmax=5, model="mp", time.step=12, control = ssm_control(verbose=0))
#write_rds(fit_ssm_12h_model_mp_HoB,here::here('SSM', 'data', 'fit_ssm_12h_model_mp_HoB_20230911.rds'))

##couple warnings:
# Warning messages:
# 1: Hessian was not positive-definite so some standard errors could not be calculated. 
# 2: Hessian was not positive-definite so some standard errors could not be calculated. 
# 3: In sqrt(as.numeric(object$diag.cov.random)) : NaNs produced
# 4: Hessian was not positive-definite so some standard errors could not be calculated.

# View(fit_ssm_12h_model_mp_HoB)
#those that have converged == FALSE: 120944-1, 120949-1
# pdHess == FALSE: NONE


###  don't normalise ### 
fit_ssm_12h_model_mp_HoB_p <-  fit_ssm_12h_model_mp_HoB %>% grab(what="p")
# --> logit_g.se == NA: 120944-1
#add other columns to data: PTT, year, month, tagging cohort...
fit_ssm_12h_model_mp_HoB_p_v2 <- fit_ssm_12h_model_mp_HoB_p %>% 
  mutate(PTT = id) %>% 
  separate(col=PTT, into=c('PTT', 'leftovers'), sep='-') %>% 
  select(-leftovers) %>% 
  mutate(Year = lubridate::year(date)) %>% 
  mutate(Month = month(date)) %>% 
  mutate(cohort = "HoB 2014")   

## save and map in QGIS
#write_csv(fit_ssm_12h_model_mp_HoB_p_v2,here::here('SSM', 'data', 'ssm_mpm_HoB2014_SRW_20230911.csv'))
summary(fit_ssm_12h_model_mp_HoB_p_v2$g)
#   Min.  1st Qu.  Median   Mean 3rd Qu.    Max. 
# 0.1059  0.6793  0.7910  0.7688  0.8984  1.0000 






### TAS ######

load("C:/Users/lrie0/Documents/SRW projects/SRWs/tag data/Mackay et al 2020 data/raw_argos_df_Tas.RData")
glimpse(raw_argos_df_Tas)
#write_csv(raw_argos_df_Tas,here::here('SSM', 'data', 'raw_argos_df_Tas.csv'))
raw_argos_df_Tas <- read_csv(here::here('tag data', 'Mackay et al 2020 data', 'All_tracks_SRW.csv')) %>% 
  filter(Location == "TAS")


#column names are already what they should be, and data does not cross 180 meridian

#remove the poor quality locations -- doesn't really matter if do this here or later, but jsut do it here
raw_argos_df_Tas <- raw_argos_df_Tas %>% 
  filter (lc != "Z")


################################################################

# SDA filter - leave this to fit_ssm


################################################################

#remove duplicates - leave this to fit_ssm


################################################################


#Time difference between successive locations

TAS_time_diff_hours_df <- ddply(raw_argos_df_Tas, ~id, function(d){
  d$time_diff_hours <- NA
  for (i in 2:nrow(d)){
    d$time_diff_hours[i] = as.numeric(difftime(d$date[i], d$date[i-1], units = "hours"))}
  return(d)
})



ggplot(TAS_time_diff_hours_df, aes(time_diff_hours)) + 
  geom_histogram(binwidth = 1, col ="white", na.rm = T) + 
  theme_bw() + xlim(c(0,100)) + 
  xlab("Time difference between successive locations")

ggplot(TAS_time_diff_hours_df, aes(time_diff_hours)) + 
  geom_histogram(binwidth = 1, col ="white", na.rm = T) + 
  theme_bw() + xlim(c(0,15)) + 
  xlab("Time difference between successive locations")


################################################################

#Segment tracks

TAS_trackseg_argos_df <- ddply(TAS_time_diff_hours_df, ~id, function(d){
  ind <- which(d$time_diff_hours > 36) 
  d$mark <- 0
  d$mark[ind] <- 1
  d$track_seg <- cumsum(d$mark)
  return(d)
})

# Now create a new id based on track segment
TAS_trackseg_argos_df$track_id <- paste(TAS_trackseg_argos_df$id, "-", TAS_trackseg_argos_df$track_seg, sep="")

table(TAS_trackseg_argos_df$track_id)

length(unique(TAS_trackseg_argos_df$track_id)) 


# remove short track segs, test n <25 
min_obs <- 25 ## set the number of minimum obs acceptable
TAS_trackseg_argos_df <- TAS_trackseg_argos_df %>% group_by(track_id)
TAS_trackseg_argos_df_filt <- filter(TAS_trackseg_argos_df, n() >= min_obs)

table(TAS_trackseg_argos_df_filt$track_id)
length(unique(TAS_trackseg_argos_df_filt$track_id)) # depends on time step and length chosen



################################################################

#SSM


TAS_ssm_df <- TAS_trackseg_argos_df_filt


#now structure the data frame so it matches the required structure for SSM
#this data did not have error ellipse variables
TAS_ssm_df <- TAS_ssm_df %>% 
  select(track_id, date, lc, lon, lat, time_diff_hours) %>% 
  dplyr::rename(id = track_id)



################################################################


#TAS data, 36h gap, 25 locs is short track, 12h ssm time step: 

#speed filter threshold (vmax) of 5 ms−1
fit_ssm_12h_model_mp_TAS <- fit_ssm(TAS_ssm_df, vmax=5, model="mp", time.step=12, control = ssm_control(verbose=0))
#write_rds(fit_ssm_12h_model_mp_TAS,here::here('SSM', 'data', 'fit_ssm_12h_model_mp_TAS_20230911.rds'))

##couple warnings:
# Warning messages:
# Hessian was not positive-definite so some standard errors could not be calculated. 

# View(fit_ssm_12h_model_mp_TAS)
#those that have converged == FALSE: 98103-4
# pdHess == FALSE: NONE


###  don't normalise ### 
fit_ssm_12h_model_mp_TAS_p <-  fit_ssm_12h_model_mp_TAS %>% grab(what="p")
# --> logit_g.se == NA: 98103-4
#add other columns to data: PTT, year, month, tagging cohort...
fit_ssm_12h_model_mp_TAS_p_v2 <- fit_ssm_12h_model_mp_TAS_p %>% 
  mutate(PTT = id) %>% 
  separate(col=PTT, into=c('PTT', 'leftovers'), sep='-') %>% 
  select(-leftovers) %>% 
  mutate(Year = lubridate::year(date)) %>% 
  mutate(Month = month(date)) %>% 
  mutate(cohort = "TAS 2010")   

## save and map in QGIS
#write_csv(fit_ssm_12h_model_mp_TAS_p_v2,here::here('SSM', 'data', 'ssm_mpm_TAS2010_SRW_20230911.csv'))
summary(fit_ssm_12h_model_mp_TAS_p_v2$g)
#   Min.  1st Qu.  Median   Mean 3rd Qu.    Max. 
# 0.2009  0.9709  0.9808  0.9510  0.9923  1.0000  





### NZ 2009 ######

load("C:/Users/lrie0/Documents/SRW projects/SRWs/tag data/Mackay et al 2020 data/raw_argos_df_NZ_2009.RData")
glimpse(raw_argos_df_NZ_2009)
#write_csv(raw_argos_df_NZ_2009,here::here('SSM', 'data', 'raw_argos_df_NZ_2009.csv'))

#column names are already what they should be, and data does not cross 180 meridian

#remove the poor quality locations -- doesn't really matter if do this here or later, but jsut do it here
raw_argos_df_NZ_2009 <- raw_argos_df_NZ_2009 %>% 
  filter (lc != "Z")


################################################################

# SDA filter - leave this to fit_ssm


################################################################

#remove duplicates - leave this to fit_ssm


################################################################


#Time difference between successive locations

NZ2009_time_diff_hours_df <- ddply(raw_argos_df_NZ_2009, ~id, function(d){
  d$time_diff_hours <- NA
  for (i in 2:nrow(d)){
    d$time_diff_hours[i] = as.numeric(difftime(d$date[i], d$date[i-1], units = "hours"))}
  return(d)
})



ggplot(NZ2009_time_diff_hours_df, aes(time_diff_hours)) + 
  geom_histogram(binwidth = 1, col ="white", na.rm = T) + 
  theme_bw() + xlim(c(0,100)) + 
  xlab("Time difference between successive locations")

ggplot(NZ2009_time_diff_hours_df, aes(time_diff_hours)) + 
  geom_histogram(binwidth = 1, col ="white", na.rm = T) + 
  theme_bw() + xlim(c(0,15)) + 
  xlab("Time difference between successive locations")


################################################################

#Segment tracks

NZ2009_trackseg_argos_df <- ddply(NZ2009_time_diff_hours_df, ~id, function(d){
  ind <- which(d$time_diff_hours > 36) 
  d$mark <- 0
  d$mark[ind] <- 1
  d$track_seg <- cumsum(d$mark)
  return(d)
})

# Now create a new id based on track segment
NZ2009_trackseg_argos_df$track_id <- paste(NZ2009_trackseg_argos_df$id, "-", NZ2009_trackseg_argos_df$track_seg, sep="")

table(NZ2009_trackseg_argos_df$track_id)

length(unique(NZ2009_trackseg_argos_df$track_id)) 


# remove short track segs, test n <25 
min_obs <- 20 ## set the number of minimum obs acceptable
NZ2009_trackseg_argos_df <- NZ2009_trackseg_argos_df %>% group_by(track_id)
NZ2009_trackseg_argos_df_filt <- filter(NZ2009_trackseg_argos_df, n() >= min_obs)

table(NZ2009_trackseg_argos_df_filt$track_id)
length(unique(NZ2009_trackseg_argos_df_filt$track_id)) # depends on time step and length chosen



################################################################

#SSM

NZ2009_ssm_df <- NZ2009_trackseg_argos_df_filt


#now structure the data frame so it matches the required structure for SSM
#this data did not have error ellipse variables
NZ2009_ssm_df <- NZ2009_ssm_df %>% 
  select(track_id, date, lc, lon, lat, time_diff_hours) %>% 
  dplyr::rename(id = track_id)



################################################################


#TAS data, 36h gap, 25 locs is short track, 12h ssm time step: 

#speed filter threshold (vmax) of 5 ms−1
fit_ssm_12h_model_mp_NZ2009 <- fit_ssm(NZ2009_ssm_df, vmax=5, model="mp", time.step=12, control = ssm_control(verbose=0))
#write_rds(fit_ssm_12h_model_mp_NZ2009,here::here('SSM', 'data', 'fit_ssm_12h_model_mp_NZ2009_20230911.rds'))

##couple warnings:
# Warning messages:
# 1: Hessian was not positive-definite so some standard errors could not be calculated. 
# 2: Hessian was not positive-definite so some standard errors could not be calculated. 
# 3: In sqrt(as.numeric(object$diag.cov.random)) : NaNs produced
# 4: Hessian was not positive-definite so some standard errors could not be calculated.

# View(fit_ssm_12h_model_mp_NZ2009)
#those that have converged == FALSE: 96373-5, 96374-0
# pdHess == FALSE: NONE


###  don't normalise ### 
fit_ssm_12h_model_mp_NZ2009_p <-  fit_ssm_12h_model_mp_NZ2009 %>% grab(what="p")
# --> logit_g.se == NA: 96373-5, 96378-0
#add other columns to data: PTT, year, month, tagging cohort...
fit_ssm_12h_model_mp_NZ2009_p_v2 <- fit_ssm_12h_model_mp_NZ2009_p %>% 
  mutate(PTT = id) %>% 
  separate(col=PTT, into=c('PTT', 'leftovers'), sep='-') %>% 
  select(-leftovers) %>% 
  mutate(Year = lubridate::year(date)) %>% 
  mutate(Month = month(date)) %>% 
  mutate(cohort = "NZ 2009")   

## save and map in QGIS
#write_csv(fit_ssm_12h_model_mp_NZ2009_p_v2,here::here('SSM', 'data', 'ssm_mpm_NZ2009_SRW_20230911.csv'))
summary(fit_ssm_12h_model_mp_NZ2009_p_v2$g)
#   Min.  1st Qu.  Median   Mean 3rd Qu.    Max. 
# 0.0518  0.7479  0.8292  0.8078  0.9135  0.9961  







