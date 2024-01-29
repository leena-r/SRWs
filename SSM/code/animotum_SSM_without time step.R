## SSM without time step for current corrected SSM & MP


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


######################################################################################

#load in master data file
raw_argos_df <- read_rds(here::here('SSM', 'data', 'NZ_SRW_2020_2021_2022_raw_argos_df_20231218.rds'))
## this was created in code script: animotum_SSM_all_NZ_tracks_grouped_12h
# reads in data from datapulls for each NZ tag year when all tags had stopped transmitting

######################################################################################
## do all the same steps as basic SSM run, up until we'd be calling the SSM function
######################################################################################

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


#for this SSM run without a time step, use all NZ data, and most things are same as in the 'basic' SSM run,
#36h gap, 25 locs is short track. but just don't define time step
#https://ianjonsen.github.io/aniMotum/reference/fit_ssm.html
#also don't use model = mp
#speed filter threshold (vmax) of 5 ms−1 - same as before
fit_ssm_NZ_all_no_timestep <- fit_ssm(ssm_df, vmax=5, model="crw", time.step=NA)

fit_ssm_NZ_all_no_timestep_p <-  fit_ssm_NZ_all_no_timestep %>% grab(what="fitted")

#write_csv(ssm_df,here::here('SSM', 'data', 'test_for_plotting_preSSM.csv'))
#write_csv(fit_ssm_NZ_all_no_timestep_p,here::here('SSM', 'data', 'fit_ssm_NZ_all_no_timestep_20240111.csv'))


################################################################
##get gamma values for the locations

##run mpm on the 'original' locations

NZ_original <- read_csv(here::here('SSM', 'data', 'fit_ssm_NZ_all_no_timestep_20240111_with_current_correction.csv'))
NZ_original <- NZ_original %>% 
  select(id, date, lon, lat) 

nrow(NZ_original) #42947 

tic()
fmp_original <- fit_mpm(NZ_original, 
               what = "fitted", ##?fit_mpm() : 'what' gets ignored if x is a data frame
               model = "mpm",
               control = mpm_control(verbose = 0))
toc()
##no errors/FALSE when run fit_mpm on 'original' lat and lon

##save mpm results using the 'original' lat and lon from CRW
fit_mpm_NZ_no_time_step_SSM_but_original_lat_lon <-  fmp_original %>% grab(what="fitted")
nrow(fit_mpm_NZ_no_time_step_SSM_but_original_lat_lon) #42947 
##change some column names if want to join with current corrected data, to avoid duplicated column names
fit_mpm_NZ_no_time_step_SSM_but_original_lat_lon <- fit_mpm_NZ_no_time_step_SSM_but_original_lat_lon %>% 
  dplyr::rename(logit_g_orig = logit_g,
                logit_g.se_orig = logit_g.se,
                g_orig = g)

hist(fit_mpm_NZ_no_time_step_SSM_but_original_lat_lon$g_orig)
summary(fit_mpm_NZ_no_time_step_SSM_but_original_lat_lon$g_orig)

##the fit_mpm object doesn't have lat and lon columns, join from pre fit_mpm data
fit_mpm_NZ_no_time_step_SSM_but_original_lat_lon_v2 <- fit_mpm_NZ_no_time_step_SSM_but_original_lat_lon %>% 
  left_join(NZ_original) %>% 
  select(id, date,lon,lat,logit_g_orig,logit_g.se_orig,g_orig )

#write_csv(fit_mpm_NZ_no_time_step_SSM_but_original_lat_lon,here::here('SSM', 'data', 'fit_mpm_NZ_no_time_step_SSM_but_original_lat_lon.csv'))



##run mpm on the current corrected locations

NZ_corrected <- read_csv(here::here('SSM', 'data', 'fit_ssm_NZ_all_no_timestep_20240111_with_current_correction.csv'))
ssm_df_NZ_corrected <- NZ_corrected %>% 
  select(id, date, lon_correct, lat_correct) %>% 
  dplyr::rename(lon = lon_correct, 
                lat = lat_correct)

nrow(ssm_df_NZ_corrected) #42947 has about 3 times more data than the OZ no time step df, but refuses to run mp on this...
#problem was using fit_ssm(model="mp"), need to use fit_mpm(model = "mpm")
tic()
#fit_mp_12h_NZ_all_current_corrected <- fit_ssm(ssm_df_NZ_corrected, model="mp", control = ssm_control(verbose=0),map = list(psi = factor(NA)))
#Guessing that all observations are GPS locations. 
fmp <- fit_mpm(ssm_df_NZ_corrected, 
               what = "fitted", 
               model = "mpm",
               control = mpm_control(verbose = 0))
toc()
# Error in nlminb(obj$par, ifelse(control$verbose == 1, myfn, obj$fn), obj$gr,  : 
#                   NA/NaN gradient evaluation
#                 In addition: Warning messages:
#                   1: In sqrt(diag(object$cov.fixed)) : NaNs produced
#                 2: In sqrt(diag(object$cov.fixed)) : NaNs produced
#                 3: In sqrt(diag(object$cov.fixed)) : NaNs produced
#                 4: In sqrt(diag(object$cov.fixed)) : NaNs produced
#View(fmp)
#converged == FALSE: 215262-10, 235399-2, 235403-0, 46633-0 ##Error in opt[["par"]] : subscript out of bounds

##save mpm results using the current corrected lat and lon
fit_mpm_NZ_no_time_step_SSM_but_current_corrected <-  fmp %>% grab(what="fitted")
nrow(fit_mpm_NZ_no_time_step_SSM_but_current_corrected) #37922 -- #seems to be missing those that didn't converge

hist(fit_mpm_NZ_no_time_step_SSM_but_current_corrected$g) ##looks bit odd
summary(fit_mpm_NZ_no_time_step_SSM_but_current_corrected$g)
#write_csv(fit_mpm_NZ_no_time_step_SSM_but_current_corrected,here::here('SSM', 'data', 'fit_mpm_NZ_no_time_step_SSM_but_current_corrected.csv'))




test <- fit_mpm_NZ_no_time_step_SSM_but_original_lat_lon_v2 %>% left_join(fit_mpm_NZ_no_time_step_SSM_but_current_corrected)
test_no_NA <- test %>% filter(!is.na(g))
#write_csv(test,here::here('SSM', 'data', 'test.csv'))
test_v2 <- test %>% 
  mutate(g_diff = g_orig-g)

hist(test_v2$g_diff)




######################################################################################
############## OZ
######################################################################################

#load in master data file
raw_argos_df <- read_rds(here::here('SSM', 'data', 'OZ_SRW_2022_2023_raw_argos_df_20231220.rds'))
## this was created in code script: animotum_SSM_all_NZ_tracks_grouped_12h
# read in from datapull when all but 1 tag had stopped, the final tag stopped 2 days after this datapull

######################################################################################
## do all the same steps as basic SSM run, up until we'd be calling the SSM function
######################################################################################

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


#for this SSM run without a time step, use all OZ data, and most things are same as in the 'basic' SSM run,
#36h gap, 25 locs is short track. but just don't define time step
#https://ianjonsen.github.io/aniMotum/reference/fit_ssm.html
#also don't use model = mp
#speed filter threshold (vmax) of 5 ms−1 - same as before
fit_ssm_OZ_all_no_timestep <- fit_ssm(ssm_df, vmax=5, model="crw", time.step=NA,map = list(psi = factor(NA)))
# Warning messages:
#   1: In sqrt(as.numeric(object$diag.cov.random)) : NaNs produced
# 2: Hessian was not positive-definite so some standard errors could not be calculated. Try simplifying the model by adding the following argument:
#   map = list(psi = factor(NA)) 
# However fit_ssm_OZ_all_no_timestep shows that all "converged" and "pdHess" are TRUE...
#no warning messages if add the extra argument

fit_ssm_OZ_all_no_timestep_p <-  fit_ssm_OZ_all_no_timestep %>% grab(what="fitted")

#write_csv(ssm_df,here::here('SSM', 'data', 'test_for_plotting_preSSM.csv'))
#write_csv(fit_ssm_OZ_all_no_timestep_p,here::here('SSM', 'data', 'fit_ssm_OZ_all_no_timestep_20240124.csv'))


##run mp on non time step, CRW, locations
testdf <- read_csv(here::here('SSM', 'data', 'fit_ssm_OZ_all_no_timestep_20240124.csv'))
testdf2 <- testdf %>% select(id, date, lon, lat)
nrow(testdf2) #13343
tic()
fit_mpm <- fit_ssm(testdf2, model="mp", time.step=12, control = ssm_control(verbose=0),map = list(psi = factor(NA)))
toc()
#Guessing that all observations are GPS locations.
##2 min run
fit_mpm_OZ_mp_after_no_time_step_SSM <-  fit_mpm %>% grab(what="fitted")
#now will have g for each time stamp, no 12h step





