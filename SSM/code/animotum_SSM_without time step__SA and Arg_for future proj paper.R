## SSM without time step
##for SA and Argentinian data
## NZ and OZ data were processed in animotum_SSM_without time step__for SRWs and ocean features paper.R
#which created final SSM/MP dataset for Ali to extract env variables, for Gin to extract ice dist - for SRW and fronts paper

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
library(argosfilter)

######################################################################################
############## SA
######################################################################################

#load in master data file
raw_argos_df <- read_rds(here::here('SSM', 'data', 'SA_SRW_2021_2022_2023_raw_argos_df_20240515.rds'))
## this was created in code script: animotum_SSM_all_NZ_tracks_grouped_12h
# read in from datapull when all 2021 and 2022 tags had stopped - 2023 tags still going but they can be deleted after SSM

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

raw_argos_df <- raw_argos_df[order(raw_argos_df$id, raw_argos_df$date),]


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


# remove short track segs, test n < 50 
##testing done in script 'animotum_SSM_without time step' for testing current corrections
##revealed that OZ tracks perform better if min obs <50
min_obs <- 50 ## set the number of minimum obs acceptable
trackseg_argos_df <- trackseg_argos_df %>% group_by(track_id)
trackseg_argos_df_filt <- filter(trackseg_argos_df, n() >= min_obs)

table(trackseg_argos_df_filt$track_id)
length(unique(trackseg_argos_df_filt$track_id)) # depends on time step and length chosen




################################################################

#SSM

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


#for this SSM run without a time step, use all SA data
#36h gap, 50 locs is short track. but just don't define time step
#https://ianjonsen.github.io/aniMotum/reference/fit_ssm.html
#also use model = mp at the same time 
###testing done in script 'animotum_SSM_without time step' for testing current corrections
##first ran without mp, did current correction, then did mp --- now jsut do mp
#speed filter threshold (vmax) of 5 ms−1 - same as before
##Warning message: The optimiser failed. You could try using a different time.step or turn off prefiltering with `spdf = FALSE` 
##but after added duplicate removal and speed filtering steps then SSM/MP converges
tic()
fit_ssm_mp_SA_all_no_timestep <- fit_ssm(ssm_df, vmax=5, model="mp", time.step=NA, control = ssm_control(verbose=0), map = list(psi = factor(NA))) ##, map = list(psi = factor(NA))
toc()
##3min to run SA data

fit_ssm_mp_SA_all_no_timestep_mp <-  fit_ssm_mp_SA_all_no_timestep %>% grab(what="fitted")
##no NAs for logit_g.se

nrow(fit_ssm_mp_SA_all_no_timestep_mp) #49068

### few outliers left in data
#225990-0 2 locs 16/10/2022
#236908-2 2 locs 3/3/2023
##THESE POINTS REMOVED FROM THE BELOW CSV MANUALLY AFTER SAVING IT HERE
#fit_ssm_mp_SA_all_no_timestep_mp <- read_csv(here::here('SSM', 'data', 'FINAL_fit_mpm_SA_no_time_step_SSM_mp_1step__20240515.csv'))



hist(fit_ssm_mp_SA_all_no_timestep_mp$g)
##looks ok, but not great


summary(fit_ssm_mp_SA_all_no_timestep_mp$g)
#  Min.  1st Qu.   Median   Mean 3rd Qu.    Max. 
#0.01117 0.44599 0.61418 0.59489 0.76156 0.97463 #these after 4 outliers removed 


#write_csv(fit_ssm_mp_SA_all_no_timestep_mp,here::here('SSM', 'data', 'FINAL_fit_mpm_SA_no_time_step_SSM_mp_1step__20240515.csv'))




# require(patchwork)
# # calculate & plot residuals
# tic
# res.rw <- osar(fit_ssm_mp_SA_all_no_timestep) ##h
# toc

####write_rds(res.rw,here::here('SSM', 'data', 'FINAL_fit_SA_no_time_step_SSM_mp_36h_50loc_1step__20240515_RESIDUALS.rds'))


ids <- unique(res.rw$id)

plot_list = list()
for (i in ids) {
  test <- res.rw %>% filter(id == i)
  p1 <- (plot(test, type = "ts") | plot(test, type = "qq")) / 
    (plot(test, type = "acf") | plot_spacer())
  plot_list[[i]] = p1
}

for (i in ids) {
  file_name = paste("plot_", i, ".tiff", sep="")
  tiff(file_name, units="in", width=7, height=5, res=300)
  print(plot_list[[i]])
  dev.off()
}

pdf("plots.pdf")
for (i in ids) {
  print(plot_list[[i]])
}



######################################################################################
############## ARG
######################################################################################

#load in master data file
raw_argos_df <- read_rds(here::here('SSM', 'data', 'SA_ARG_2014-2023_raw_argos_df_20240515.rds'))
## this was created in code script: animotum_SSM_all_NZ_tracks_grouped_12h
# read in from datapull when all 2021 and 2022 tags had stopped - 2023 tags still going but they can be deleted after SSM

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

raw_argos_df <- raw_argos_df[order(raw_argos_df$id, raw_argos_df$date),]


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


# remove short track segs, test n < 50 
##testing done in script 'animotum_SSM_without time step' for testing current corrections
##revealed that OZ tracks perform better if min obs <50
min_obs <- 50 ## set the number of minimum obs acceptable
trackseg_argos_df <- trackseg_argos_df %>% group_by(track_id)
trackseg_argos_df_filt <- filter(trackseg_argos_df, n() >= min_obs)

table(trackseg_argos_df_filt$track_id)
length(unique(trackseg_argos_df_filt$track_id)) # depends on time step and length chosen




################################################################

#SSM

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


#for this SSM run without a time step, use all SA data
#36h gap, 50 locs is short track. but just don't define time step
#https://ianjonsen.github.io/aniMotum/reference/fit_ssm.html
#also use model = mp at the same time 
###testing done in script 'animotum_SSM_without time step' for testing current corrections
##first ran without mp, did current correction, then did mp --- now jsut do mp
#speed filter threshold (vmax) of 5 ms−1 - same as before
##Warning message: The optimiser failed. You could try using a different time.step or turn off prefiltering with `spdf = FALSE` 
##but after added duplicate removal and speed filtering steps then SSM/MP converges
tic()
fit_ssm_mp_ARG_all_no_timestep <- fit_ssm(ssm_df, vmax=5, model="mp", time.step=NA, control = ssm_control(verbose=0), map = list(psi = factor(NA))) ##, map = list(psi = factor(NA))
toc()
##6min to run ARG data
#when 2023 tags included few warnings
# Warning messages:
# 1: In sqrt(as.numeric(object$diag.cov.random)) : NaNs produced
# 2: Hessian was not positive-definite so some standard errors could not be calculated. 
# 3: In sqrt(as.numeric(object$diag.cov.random)) : NaNs produced
# 4: Hessian was not positive-definite so some standard errors could not be calculated. 
# 5: In sqrt(as.numeric(object$diag.cov.random)) : NaNs produced
# 6: Hessian was not positive-definite so some standard errors could not be calculated. 

fit_ssm_mp_ARG_all_no_timestep_mp <-  fit_ssm_mp_ARG_all_no_timestep %>% grab(what="fitted")
##some NAs for 2 segments in logit_g.se

nrow(fit_ssm_mp_ARG_all_no_timestep_mp) #142019 with 2023 deployments (that left BG)

### few outliers left in data
#236903-1 2 locs 24/10/2022
#194579-0 1 locs 23/9/2019
#197859-1 1 locs 15/9/2023
#197855-2 1 locs 15/11/2022
##THESE POINTS REMOVED FROM THE BELOW CSV MANUALLY AFTER SAVING IT HERE
#fit_ssm_mp_ARG_all_no_timestep_mp <- read_csv(here::here('SSM', 'data', 'FINAL_fit_mpm_ARG_no_time_step_SSM_mp_1step__20240515.csv'))



hist(fit_ssm_mp_ARG_all_no_timestep_mp$g)
##looks ok, but not great


summary(fit_ssm_mp_ARG_all_no_timestep_mp$g)
#  Min.   1st Qu.   Median   Mean    3rd Qu.    Max. 
#0.002955 0.424333 0.564139 0.561022 0.709506 0.984704  #these after outliers removed 


#write_csv(fit_ssm_mp_ARG_all_no_timestep_mp,here::here('SSM', 'data', 'FINAL_fit_mpm_ARG_no_time_step_SSM_mp_1step__20240515.csv'))




# require(patchwork)
# # calculate & plot residuals
# tic
# res.rw <- osar(fit_ssm_mp_ARG_all_no_timestep) ##h
# toc

####write_rds(res.rw,here::here('SSM', 'data', 'FINAL_fit_ARG_no_time_step_SSM_mp_36h_50loc_1step__20240515_RESIDUALS.rds'))


ids <- unique(res.rw$id)

plot_list = list()
for (i in ids) {
  test <- res.rw %>% filter(id == i)
  p1 <- (plot(test, type = "ts") | plot(test, type = "qq")) / 
    (plot(test, type = "acf") | plot_spacer())
  plot_list[[i]] = p1
}

for (i in ids) {
  file_name = paste("plot_", i, ".tiff", sep="")
  tiff(file_name, units="in", width=7, height=5, res=300)
  print(plot_list[[i]])
  dev.off()
}

pdf("plots.pdf")
for (i in ids) {
  print(plot_list[[i]])
}


