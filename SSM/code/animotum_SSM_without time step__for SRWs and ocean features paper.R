## SSM without time step for SRWs and ocean features paper
#creating final SSM/MP dataset for Ali to extract env variables, for Gin to extract ice dist
#and to be used for final analyses for the paper


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


#remove the poor quality locations -- doesn't really matter if do this here or later, but just do it here
raw_argos_df <- raw_argos_df %>% 
  filter (lc != "Z")


raw_argos_df <- raw_argos_df[order(raw_argos_df$id, raw_argos_df$date),]

################################################################

#remove duplicates - leave this to fit_ssm

# pre_dup <- nrow(raw_argos_df) # to get the current number of data points
# 
# # create dummy variable
# raw_argos_df$index <- c(1:nrow(raw_argos_df))
# 
# # run each tag in a loop check to check for duplicates
# # if there is a time duplicate, select the best quality position or simply the first position
# raw_argos_df <- ddply(raw_argos_df, ~id, function(d){
#   toremove <- c()
#   for (i in c(2:nrow(d))) {
#     if (d$date[i] == d$date[i-1]) {
#       dd <- d[(i-1):i,]
#       r <- dd[dd$lc == ave(dd$lc, FUN = min), "index"] # find the lowest quality
#       toremove <- c(toremove, r[1]) #select first element of r in case both locations have the same lq
#     }
#   }
#   if (length(toremove) > 0){d <- d[!(d$index %in% toremove), ]}
#   return(d)
# })
# # remove dummy variable
# raw_argos_df$index <- NULL
# pre_dup - nrow(raw_argos_df) # to get an understanding of how many duplicates were removed; n=2396

################################################################


# SDA filter - leave this to fit_ssm

# raw_argos_df <- ddply(raw_argos_df, ~id, function(d){
#   d$argosfilter <- sdafilter(lat = d$lat, 
#                              lon = d$lon, 
#                              lc = d$lc, 
#                              dtime = d$date, vmax = 5)
#   return(d)
# })
# 
# 
# # exclude errorneous locs 
# filtered_argos_df <- raw_argos_df %>%
#   filter(argosfilter != "removed") %>%
#   dplyr::select(-argosfilter)
# 
# 
# 
# filtered_argos_df %>% 
#   group_by(id) %>% 
#   dplyr::summarize(nb_locations = n())
# 
# tab_1 <- raw_argos_df %>% 
#   group_by(id) %>% 
#   dplyr::summarize(nb_locations = n())
# 
# tab_2 <- filtered_argos_df %>% 
#   group_by(id) %>% 
#   dplyr::summarize(nb_locations = n())
# 
# tab  <- plyr::join(data.frame(tab_1), data.frame(tab_2), by="id")
# colnames(tab) <- c("id", "raw_locs", "filt_locs")
# tab

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
#36h gap, 50 locs is short track. but just don't define time step
#https://ianjonsen.github.io/aniMotum/reference/fit_ssm.html
#also use model = mp at the same time 
###testing done in script 'animotum_SSM_without time step' for testing current corrections
##first ran without mp, did current correction, then did mp --- now just do mp at the same time
#speed filter threshold (vmax) of 5 ms−1 - same as before

##if 36h and <50locs
##Warning message: The optimiser failed. You could try using a different time.step or turn off prefiltering with `spdf = FALSE` 
##but after added duplicate removal and speed filtering steps then SSM/MP converges
##actually removed those steps and added speed filter to fit_smm, then it worked? -- as long as remove map argument

##if 36h and <50locs, drop 46635-0, with map argument -- all converge -- but doesnt look good in QGIS - lots of long ARS sections south of Akl Is
##if 36h and <50locs, drop 46635-0, without map argument -- all converge -- looks ok in QGIS - 1 long ARS sections south of Akl Is


########if 24h and <50 loc and no map argument, some issues and non convergence. 
#if add map argument 46635-0 doesn't converge
##if drop 46635-0 some warnings but all converge
### the 1-step approach

ssm_df <- ssm_df %>% filter(id != "46635-0")

tic()
fit_ssm_mp_NZ_all_no_timestep <- fit_ssm(ssm_df, vmax=5, model="mp", time.step=NA, control = ssm_control(verbose=0)) ##, map = list(psi = factor(NA))
toc()
##vmax=5, 

#res.rw <- osar(fit_ssm_mp_NZ_all_no_timestep) ##2h42min
# Warning message:
#   In asMethod(object) :
#   sparse->dense coercion: allocating vector of size 1.0 GiB
####write_rds(res.rw,here::here('SSM', 'data', 'res_rw_1stepapproach.rds'))
# # use patchwork package to arrange plot.osar options
# require(patchwork)
# # calculate & plot residuals
# res.rw <- osar(fit.rw)
# test <- res.rw %>% filter(id == "215261-0")
# (plot(test, type = "ts") | plot(test, type = "qq")) / 
#   (plot(test, type = "acf") | plot_spacer())



fit_ssm_mp_NZ_all_no_timestep_mp <-  fit_ssm_mp_NZ_all_no_timestep %>% grab(what="fitted")
##no NAs for se

nrow(fit_ssm_mp_NZ_all_no_timestep_mp) #42764



hist(fit_ssm_mp_NZ_all_no_timestep_mp$g)
##not perfect looking but ok
##if 36h, <50locs, drop 46635-0 and no map argument, then looks good


summary(fit_ssm_mp_NZ_all_no_timestep_mp$g)
##36h <50loc
#   Min.  1st Qu.   Median    Mean 3rd Qu.    Max. 
# 0.04657 0.49008 0.64925 0.62877 0.78876 0.96255
##if 36h, <50locs, drop 46635-0 and no map argument
#   Min.  1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.01052 0.75508 0.87344 0.82387 0.93712 0.99495 


#write_csv(fit_ssm_mp_NZ_all_no_timestep_mp,here::here('SSM', 'data', 'test_fit_mpm_NZ_no_time_step_SSM_mp_36h_50loc_1segmentremoved__20240328.csv'))



##the 2-step approach
##no segments dropped yet
fit_ssm_NZ_all_no_timestep <- fit_ssm(ssm_df, vmax=5, model="crw", time.step=NA)
fit_ssm_NZ_all_no_timestep_p <-  fit_ssm_NZ_all_no_timestep %>% grab(what="fitted")

fit_ssm_NZ_all_no_timestep_p <- fit_ssm_NZ_all_no_timestep_p %>% 
  select(id, date, lon, lat) 

fit_ssm_NZ_all_no_timestep_p <- fit_ssm_NZ_all_no_timestep_p[order(fit_ssm_NZ_all_no_timestep_p$id, fit_ssm_NZ_all_no_timestep_p$date),]

fit_ssm_NZ_all_no_timestep_p <- fit_ssm_NZ_all_no_timestep_p %>% filter(id != "235399-7")

# without time step: if drop 235399-7 segment, then all converge -- and no NAs
tic()
fit_ssm_mp_NZ_all_no_timestep_2step <- fit_ssm(fit_ssm_NZ_all_no_timestep_p, model="mp", time.step=NA, control = ssm_control(verbose=0), map = list(psi = factor(NA))) ##
toc()

fit_ssm_mp_NZ_all_no_timestep_MP_2step <-  fit_ssm_mp_NZ_all_no_timestep_2step %>% grab(what="fitted")
hist(fit_ssm_mp_NZ_all_no_timestep_MP_2step$g) ##looks good
summary(fit_ssm_mp_NZ_all_no_timestep_MP_2step$g)
#  Min.   1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.07041 0.78113 0.90579 0.84524 0.95632 0.99912 

#write_csv(fit_ssm_mp_NZ_all_no_timestep_MP_2step,here::here('SSM', 'data', 'FINAL_fit_NZ_no_time_step_SSM_mp_36h_50loc_1segmentremoved_2step__20240403.csv'))



# require(patchwork)
# # calculate & plot residuals
#tic
#res.rw <- osar(fit_ssm_mp_NZ_all_no_timestep_2step) ##~3h
#toc
####write_rds(res.rw,here::here('SSM', 'data', 'FINAL_fit_NZ_no_time_step_SSM_mp_36h_50loc_1segmentremoved_2step__20240403_RESIDUALS.rds'))
# test <- res.rw %>% filter(id == "197853-1")
# (plot(test, type = "ts") | plot(test, type = "qq")) / 
#   (plot(test, type = "acf") | plot_spacer())



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
############## OZ
######################################################################################

#load in master data file
raw_argos_df <- read_rds(here::here('SSM', 'data', 'OZ_SRW_2022_2023_raw_argos_df_20240125.rds'))
## this was created in code script: animotum_SSM_all_NZ_tracks_grouped_12h
# read in from datapull when all tags had stopped

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


#for this SSM run without a time step, use all NZ data, and most things are same as in the 'basic' SSM run,
#36h gap, 50 locs is short track. but just don't define time step
#https://ianjonsen.github.io/aniMotum/reference/fit_ssm.html
#also use model = mp at the same time 
###testing done in script 'animotum_SSM_without time step' for testing current corrections
##first ran without mp, did current correction, then did mp --- now jsut do mp
#speed filter threshold (vmax) of 5 ms−1 - same as before
##Warning message: The optimiser failed. You could try using a different time.step or turn off prefiltering with `spdf = FALSE` 
##but after added duplicate removal and speed filtering steps then SSM/MP converges
tic()
fit_ssm_mp_OZ_all_no_timestep <- fit_ssm(ssm_df, vmax=5, model="mp", time.step=NA, control = ssm_control(verbose=0), map = list(psi = factor(NA))) ##, map = list(psi = factor(NA))
toc()


fit_ssm_mp_OZ_all_no_timestep_mp <-  fit_ssm_mp_OZ_all_no_timestep %>% grab(what="fitted")
##no NAs for se

nrow(fit_ssm_mp_OZ_all_no_timestep_mp) #13288



hist(fit_ssm_mp_OZ_all_no_timestep_mp$g)
##not perfect looking but ok


summary(fit_ssm_mp_OZ_all_no_timestep_mp$g)
#  Min.  1st Qu.   Median    Mean 3rd Qu.    Max. 
# 0.01414 0.44608 0.63810 0.60539 0.78629 0.98866



#write_csv(fit_ssm_mp_OZ_all_no_timestep_mp,here::here('SSM', 'data', 'FINAL_fit_mpm_OZ_no_time_step_SSM_mp_1step__20240403.csv'))




# require(patchwork)
# # calculate & plot residuals
#tic
#res.rw <- osar(fit_ssm_mp_OZ_all_no_timestep) ##~1.5h
#toc

####write_rds(res.rw,here::here('SSM', 'data', 'FINAL_fit_OZ_no_time_step_SSM_mp_36h_50loc_1step__20240403_RESIDUALS.rds'))


# test <- res.rw %>% filter(id == "197853-1")
# (plot(test, type = "ts") | plot(test, type = "qq")) / 
#   (plot(test, type = "acf") | plot_spacer())


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


















##the 2-step approach
##no segments dropped yet
fit_ssm_OZ_all_no_timestep <- fit_ssm(ssm_df, vmax=5, model="crw", time.step=NA)
fit_ssm_OZ_all_no_timestep_p <-  fit_ssm_OZ_all_no_timestep %>% grab(what="fitted")

fit_ssm_OZ_all_no_timestep_p <- fit_ssm_OZ_all_no_timestep_p %>% 
  select(id, date, lon, lat) 

fit_ssm_OZ_all_no_timestep_p <- fit_ssm_OZ_all_no_timestep_p[order(fit_ssm_OZ_all_no_timestep_p$id, fit_ssm_OZ_all_no_timestep_p$date),]

# without time step:  all converge -- and no NAs
tic()
fit_ssm_mp_OZ_all_no_timestep_2step <- fit_ssm(fit_ssm_OZ_all_no_timestep_p, model="mp", time.step=NA, control = ssm_control(verbose=0), map = list(psi = factor(NA))) ##
toc()

fit_ssm_mp_OZ_all_no_timestep_MP_2step <-  fit_ssm_mp_OZ_all_no_timestep_2step %>% grab(what="fitted")
hist(fit_ssm_mp_OZ_all_no_timestep_MP_2step$g) ##looks good
summary(fit_ssm_mp_OZ_all_no_timestep_MP_2step$g)
#  Min.   1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.1012  0.7323  0.8630  0.8169  0.9379  0.9927 

#write_csv(fit_ssm_mp_OZ_all_no_timestep_MP_2step,here::here('SSM', 'data', 'FINAL_fit_OZ_no_time_step_SSM_mp_36h_50loc_1segmentremoved_2step__20240403.csv'))



# require(patchwork)
# # calculate & plot residuals
#tic
#res.rw <- osar(fit_ssm_mp_OZ_all_no_timestep_2step) ##~1.5h
#toc

####write_rds(res.rw,here::here('SSM', 'data', 'FINAL_fit_OZ_no_time_step_SSM_mp_36h_50loc_1segmentremoved_2step__20240403_RESIDUALS.rds'))


# test <- res.rw %>% filter(id == "197853-1")
# (plot(test, type = "ts") | plot(test, type = "qq")) / 
#   (plot(test, type = "acf") | plot_spacer())


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

