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
library(Matrix)


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
nrow(NZ_original) #42947 

## what if at this point remove NA cases, to keep things the same?
##removing these cause the additional 235403-0 to not converge, on top of 235399-7
NZ_original <- NZ_original[!is.na(NZ_original$lon_correct),]
nrow(NZ_original) #42911 

NZ_original <- NZ_original %>% 
  select(id, date, lon, lat) 
nrow(NZ_original)

##the LC column doesn't matter too much. if don't create it, mp assumes GPS (which is same as creating a lc = G column)
## lc == GL doesn't work
NZ_original$lc <- "G"


##make sure ordered by id and date
NZ_original <- NZ_original[order(NZ_original$id, NZ_original$date),]

##id 235399-7 has one odd location, remove it and see if that helps -- no it did not
NZ_original <- NZ_original %>% filter(!lon == 146.6288000000011)


#NZ_original <- NZ_original %>% filter(!id %in% c("215262-0", "215262-1",  "215262-10", "215262-11", "215262-14", "215262-8"))


# tic()
# fmp_original <- fit_mpm(NZ_original, 
#                #what = "fitted", ##?fit_mpm() : 'what' gets ignored if x is a data frame
#                model = "mpm",
#                control = mpm_control(verbose = 0))
# toc()
##no errors/FALSE when run fit_mpm on 'original' lat and lon




##Gin suggestion, run fit_smm model=mp
tic()
fmp_original <- fit_ssm(NZ_original, model="mp", time.step=18, control = ssm_control(verbose=0), map = list(psi = factor(NA))) ##
toc()
#"Guessing that all observations are GPS locations" - if no lc column was created
#without a time step: some warnings, but 235399-7 converged = FALSE - and NAs for logit_g.se
#and if at the start removed NA locs of corrected alt and lon also 235403-0 didn't converge - and NAs for logit_g.se

##if use 6h time step - 7 segments that don't converge, and NAs for logit.se
##if use 12h time step: converged = FALSE for: 208742-2, 215258-0, 215259-1, 215261-2, 215262-11, 235401-2, and NAs for logit.se
## same if use 12h time step an no map=list argument
#if use 24h time step: converged = FALSE for: 208742-2, 215261-2, 215262-11, 235401-2, 235402-0 - and NAs for logit_g.se
### BEST TIME STEP: if use 18h time step: converged = FALSE for: 215258-14 [short], 215262-11 [short] - no NAs for logit_g.se
# if use 18h time step and map = list(rho_o = factor(NA)): converged = FALSE for: 215258-14 [short], 215262-11 [short] - but has NAs for logit_g.se
#if use 18h time step and drop PTT 215262: converged = FALSE for: 215258-14 [short] - no NAs for logit_g.se

# id_235399_7 <- NZ_original %>% filter(id == "235399-7")
# fmp_id_235399_7 <- fit_ssm(id_235399_7, model="mp", control = ssm_control(verbose=0), map = list(psi = factor(NA)))
# ##235399-7: The optimiser failed. Try simplifying the model with the following argument: `map = list(rho_o = factor(NA))`
# id_235403_0 <- NZ_original %>% filter(id == "235403-0")
# fmp_id_235399_7 <- fit_ssm(id_235403_0, model="mp", control = ssm_control(verbose=0), map = list(rho_o = factor(NA)))
# ##The optimiser failed. Try simplifying the model with the following argument: `map = list(rho_o = factor(NA))`


##save mpm results using the 'original' lat and lon from CRW
fit_mpm_NZ_no_time_step_SSM_but_original_lat_lon <-  fmp_original %>% grab(what="fitted")
nrow(fit_mpm_NZ_no_time_step_SSM_but_original_lat_lon) #42947 
##40829 when try Gin suggestion of fit_smm(model=mp) and only 235399-7 converged = FALSE 
##39507 when try Gin suggestion of fit_smm(model=mp) and 235399-7 and 235403-0 converged = FALSE 
##change some column names if want to join with current corrected data, to avoid duplicated column names
fit_mpm_NZ_no_time_step_SSM_but_original_lat_lon <- fit_mpm_NZ_no_time_step_SSM_but_original_lat_lon %>% 
  dplyr::rename(logit_g_orig = logit_g,
                logit_g.se_orig = logit_g.se,
                g_orig = g)

hist(fit_mpm_NZ_no_time_step_SSM_but_original_lat_lon$g_orig)
summary(fit_mpm_NZ_no_time_step_SSM_but_original_lat_lon$g_orig)
#   sMin. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.00738 0.73699 0.88641 0.81413 0.95290 0.99918 
#values when try Gin suggestion fit_ssm(model=mp)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.0704  0.7811  0.9061  0.8453  0.9563  0.9991 

##the fit_mpm object doesn't have lat and lon columns, join from pre fit_mpm data
fit_mpm_NZ_no_time_step_SSM_but_original_lat_lon_v2 <- fit_mpm_NZ_no_time_step_SSM_but_original_lat_lon %>% 
  left_join(NZ_original) %>% 
  select(id, date,lon,lat,logit_g_orig,logit_g.se_orig,g_orig )

#write_csv(fit_mpm_NZ_no_time_step_SSM_but_original_lat_lon_v2,here::here('SSM', 'data', 'test_fit_mpm_NZ_no_time_step_SSM_but_mp_on_original_lat_lon.csv'))

##does already have lat lon if run fit_smm(model_mp)
#write_csv(fit_mpm_NZ_no_time_step_SSM_but_original_lat_lon,here::here('SSM', 'data', 'test_fit_mpm_NZ_no_time_step_original_lat_lon_Gin_fix.csv'))


###########################################
##run mpm on the current corrected locations

NZ_corrected <- read_csv(here::here('SSM', 'data', 'fit_ssm_NZ_all_no_timestep_20240111_with_current_correction.csv'))
ssm_df_NZ_corrected <- NZ_corrected %>% 
  select(id, date, lon_correct, lat_correct) %>% 
  dplyr::rename(lon = lon_correct, 
                lat = lat_correct)
nrow(ssm_df_NZ_corrected) #42947

##few instances of NA for current corrected lat and lon
ssm_df_NZ_corrected <- ssm_df_NZ_corrected[!is.na(ssm_df_NZ_corrected$lon),]
nrow(ssm_df_NZ_corrected) #42911

ssm_df_NZ_corrected$lc <- "G"

##make sure ordered by id and date
ssm_df_NZ_corrected <- ssm_df_NZ_corrected[order(ssm_df_NZ_corrected$id, ssm_df_NZ_corrected$date),]

### one of the issues might be that the lon_corrected is in 0-360 degree format instead of -180 to 180
ssm_df_NZ_corrected <- ssm_df_NZ_corrected %>% mutate(lon = if_else(lon > 180, lon-360, lon))


##PTT215262 seems to be the main problem, specifically "215262-11", "215262-14"
#ssm_df_NZ_corrected <- ssm_df_NZ_corrected %>% filter(!id %in% c("215262-0", "215262-1",  "215262-10", "215262-11", "215262-14", "215262-8"))


#problem was using fit_ssm(model="mp"), need to use fit_mpm(model = "mpm")
# tic()
# #fit_mp_12h_NZ_all_current_corrected <- fit_ssm(ssm_df_NZ_corrected, model="mp", control = ssm_control(verbose=0),map = list(psi = factor(NA)))
# #Guessing that all observations are GPS locations. 
# fmp <- fit_mpm(ssm_df_NZ_corrected, 
#                #what = "fitted", 
#                model = "mpm",
#                control = mpm_control(verbose = 0))
# toc()
##after Gin's fix of removing NAs
#                 1: In sqrt(diag(object$cov.fixed)) : NaNs produced
#                 2: In sqrt(diag(object$cov.fixed)) : NaNs produced
#                 3: In sqrt(diag(object$cov.fixed)) : NaNs produced
#                 4: In sqrt(diag(object$cov.fixed)) : NaNs produced
#View(fmp)
#but converged == FALSE: NONE


##Gin suggestion, run fit_smm model=mp
tic()
fmp <- fit_ssm(ssm_df_NZ_corrected, model="mp", time.step=18, control = ssm_control(verbose=0), map = list(psi = factor(NA))) ##
toc()
#Guessing that all observations are GPS locations. -- if didn't create a lc = G column earlier
#some warnings,  converged=FALSE for 197853-2, 215261-2, 215262-14, 235399-7, 235402-1, and NAs for logit_g.se
##same converged=FALSE if create a lc=G column before running fit_smm
##all fail to converge if create a lc=GL column before running fit_smm
##excluding the vmax=5 argument doesn't change outcome
## same converged=FALSE if run without 'map' argument
###IF FIX lon to be -180 to 180 (instead of 0-360):
#still the same ones have issues
##If add 6h time steP: 215258-14 [bit short], 215259-2 [bit short, but not THAT short] don't converge (logit.se no NAs)
##If add 12h time steP: 215259-2 [bit short, but not THAT short], 215261-2 [short], 215262-14 [short] don't converge
##If add 18h time steP: 215262-11 [bit short], 215262-14 [bit short] don't converge (some NAs for logit.se)
##########if use 18h time step but drop all segments for PTT 215262 - all converge, but NAs for logit_g.se

#if drop PTT215262 and run without time step: 197853-2, 215261-2, 235399-7, 235402-1 don't converge, and NAs for logit_g.se

# PTT215262 <- ssm_df_NZ_corrected %>% filter(id %in% c("215262-0", "215262-1",  "215262-10", "215262-11", "215262-14", "215262-8"))
# fmp_PTT215262 <- fit_ssm(PTT215262, model="mp", time.step=18, control = ssm_control(verbose=0), map = list(psi = factor(NA)))
##segments 11 and 14 don't converge

##save mpm results using the current corrected lat and lon
fit_mpm_NZ_no_time_step_SSM_but_current_corrected <-  fmp %>% grab(what="fitted")
nrow(fit_mpm_NZ_no_time_step_SSM_but_current_corrected) #42911  ##34210 after Gin suggestion fit_smm(model_mp)

hist(fit_mpm_NZ_no_time_step_SSM_but_current_corrected$g) ##looks bit odd still -- looks better with fit_smm(model_mp)
summary(fit_mpm_NZ_no_time_step_SSM_but_current_corrected$g)
# after Gins fix of removing nAs etc
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.0000  0.1828  0.4116  0.4267  0.6606  0.9799 
#values when try Gin suggestion fit_ssm(model=mp)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.0000  0.7793  0.8882  0.8385  0.9395  0.9939 

##the fit_mpm object doesn't have lat and lon columns, join from pre fit_mpm data
fit_mpm_NZ_no_time_step_SSM_but_current_corrected_v2 <- fit_mpm_NZ_no_time_step_SSM_but_current_corrected %>% 
  left_join(NZ_original) %>% 
  select(id, date,lon,lat,logit_g,logit_g.se,g)

#write_csv(fit_mpm_NZ_no_time_step_SSM_but_current_corrected_v2,here::here('SSM', 'data', 'test_fit_mpm_NZ_no_time_step_SSM_but_mp_on_current_corrected.csv'))

##does already have lat lon if run fit_smm(model_mp)
#write_csv(fit_mpm_NZ_no_time_step_SSM_but_current_corrected,here::here('SSM', 'data', 'test_fit_mpm_NZ_no_time_step_current_corr_lat_lon_Gin_fix.csv'))




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

##########################################################################
##run mp on non time step, CRW, locations
testdf <- read_csv(here::here('SSM', 'data', 'fit_ssm_OZ_all_no_timestep_20240124.csv'))
testdf2 <- testdf %>% select(id, date, lon, lat)
nrow(testdf2) #13343

testdf2$lc <- "G"
##make sure ordered by id and date
testdf2 <- testdf2[order(testdf2$id, testdf2$date),]

# tic()
# fit_mpm <- fit_mpm(testdf2, 
#                #what = "fitted", 
#                model = "mpm",
#                control = mpm_control(verbose = 0))
# toc()
# 

##Gin suggestion, run fit_smm model=mp
fmp_original_OZ <- fit_ssm(testdf2, model="mp", control = ssm_control(verbose=0), map = list(psi = factor(NA)))
#had some warnings. track id 235411-1 converged == FALSE

##save mpm results using the 'original' lat and lon from CRW
fit_mpm_OZ_no_time_step_SSM_but_original_lat_lon <-  fmp_original_OZ %>% grab(what="fitted")
#now will have g for each time stamp, no 12h step
nrow(fit_mpm_OZ_no_time_step_SSM_but_original_lat_lon) ##12773 missing data for track id 235411-1 converged == FALSE


##change some column names if want to join with current corrected data, to avoid duplicated column names
fit_mpm_OZ_no_time_step_SSM_but_original_lat_lon <- fit_mpm_OZ_no_time_step_SSM_but_original_lat_lon %>% 
  dplyr::rename(logit_g_orig = logit_g,
                logit_g.se_orig = logit_g.se,
                g_orig = g)

hist(fit_mpm_OZ_no_time_step_SSM_but_original_lat_lon$g_orig) ##looks bit off
summary(fit_mpm_OZ_no_time_step_SSM_but_original_lat_lon$g_orig)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.03305 0.51439 0.65135 0.64579 0.77984 0.98404

##does already have lat lon if run fit_smm(model_mp)
#write_csv(fit_mpm_OZ_no_time_step_SSM_but_original_lat_lon,here::here('SSM', 'data', 'test_fit_mpm_OZ_no_time_step_original_lat_lon_Gin_fix.csv'))



