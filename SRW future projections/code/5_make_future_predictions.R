### SRW future projections

# script 5:
# This script makes monthly circumpolar future predictions of SRW foraging habitat suitability 
# using population-level Random Forest (RF) models [script 1] and environmental covariates.


######################################################################################
## load packages
######################################################################################

library(tidyverse)
library(terra)
library(tidyterra)
library(tictoc)
library(viridis)
library(here)

######################################################################################

#define scenario
this.scenario <- "ssp126" ## options: ssp585 or ssp126

######################################################################################
## Read in monthly variables 
######################################################################################

#read in monthly dynamic variables
#chl and wind have different extent to the other layers
chl <- rast(here("data", "deltas", "chlos", "satellite_data", paste0("monthly_transformed_", this.scenario, ".nc")))
wind <- rast(here("data", "deltas", "wind", "satellite_data", paste0("monthly_transformed_", this.scenario, ".nc")))
e <- ext(-180, 180, -80, -25)
chl <- crop(chl, e)
wind <- crop(wind, e)


sst <- rast(here("data", "deltas", "tos", "satellite_data", paste0("monthly_transformed_", this.scenario, "_updated.nc")))
ssh <- rast(here("data", "deltas", "zos", "satellite_data", paste0("monthly_transformed_", this.scenario, "_updated.nc")))
sal <- rast(here("data", "deltas", "sos", "satellite_data", paste0("monthly_transformed_", this.scenario, "_updated.nc")))
mld <- rast(here("data", "deltas", "mlotst", "satellite_data", paste0("monthly_transformed_", this.scenario, "_updated.nc")))
sic <- rast(here("data", "deltas", "siconc", "satellite_data", paste0("monthly_transformed_", this.scenario, "_updated.nc")))

#create stack
dynamic <- c(chl, sst, ssh, sal, mld, sic, wind)

#limit to target years and months
min_year <- 2014
max_year <- 2023
months <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
dynamic <- dynamic[[month(time(dynamic)) %in% months & 
                      year(time(dynamic)) >= min_year & 
                      year(time(dynamic)) <= max_year]]

#cleanup
rm(chl, sst, ssh, sal, mld, sic, wind)

#read in static variables
depth <- rast(here("data", "monthly", "depth_resampled_1deg.nc"))
dshelf <- rast(here("data", "monthly", "dshelf_resampled_1deg.nc"))
slope <- rast(here("data", "monthly", "slope_resampled_1deg.nc"))


#create stack
static <- c(depth, dshelf, slope)
names(static) <- c("depth", "dshelf", "slope")

#cleanup
rm(depth, dshelf, slope)

#resample static variables to dynamic resolution, crs, and extent
static <- resample(static, dynamic, method = "bilinear", threads = T)

#crop to target area
e <- ext(-180, 180, -80, -30)
static <- crop(static, e)
dynamic <- crop(dynamic, e)

#cleanup
rm(min_year, max_year, months)



######################################################################################
## Make circumpolar prediction map for each month
######################################################################################

#read in random forests for each population and the circumpolar model
arg_rf <- readRDS(here("output", "RF_ARG_data_v1_no_currents_foraging_20241203.RDS"))
sa_rf <- readRDS(here("output", "RF_SA_data_v1_no_currents_foraging_20241203.RDS"))
nz_rf <- readRDS(here("output", "RF_NZ_data_v1_no_currents_foraging_20241203.RDS"))
oz_rf <- readRDS(here("output", "RF_OZ_data_v1_no_currents_foraging_20241203.RDS"))

circumpolar_rf <- readRDS(here("output", "RF_joined_ARS_model_no_currents_20241205.RDS"))


#extract list of every foraging month from 2001-2020
nmonths <- n_distinct(time(dynamic))
months <- time(dynamic)[1:nmonths] 
months <- as_date(months)

#loop over each month
for(i in months){
  this.month <- as_date(i)
  
  #extract dynamic variables for this month
  stack <- dynamic[[time(dynamic) == this.month]]
  names(stack) <- c("chl", "sst", "ssh", "sal", "mld", "sic", "wind")
  
  
  #change sic NAs to 0s (to allow prediction outside sea ice area)
  stack$sic[is.na(stack$sic)] <- 0
  
  #crop to target area
  stack <- crop(stack, e)
  
  
  #create season raster layer
    season_selection <- 
    case_when(month(this.month) %in% c(12, 1, 2) ~ "summer",
              month(this.month) %in% c(3, 4, 5) ~ "autumn",
              month(this.month) %in% c(6, 7, 8) ~ "winter",
              month(this.month) %in% c(9, 10, 11) ~ "spring")
  season_raster <- rast(ext=ext(stack), res=res(stack), crs=crs(stack))
  values(season_raster) <- paste0(this.month)
  names(season_raster) <- "season1"
  
  
  #integrate static variables and month with raster stack
  stack <- c(stack, static, season_raster)
  
  
  #predict each model to raster stack
  arg <- predict(stack, arg_rf, type="prob", na.rm = T)$presence
  names(arg) <- "ARG_pred"
  
  sa <- predict(stack, sa_rf, type="prob", na.rm = T)$presence
  names(sa) <- "SA_pred"
  
  nz <- predict(stack, nz_rf, type="prob", na.rm = T)$presence
  names(nz) <- "NZ_pred"
  
  oz <- predict(stack, oz_rf, type="prob", na.rm = T)$presence
  names(oz) <- "OZ_pred"
  
  
  
  #combine predictions with raster stack
  stack <- c(stack, arg, sa, nz, oz)
  #names(stack)
  rm(arg, sa, nz, oz)
  
  #predict circumpolar habitat
  circumpolar <- predict(stack, circumpolar_rf, type="prob", na.rm = T)$presence
  
  #assign time
  time(circumpolar) <- this.month
  
  #join circumpolar prediction to all other predictions
  if(i == months[1]){
    predictions <- circumpolar
  } else {
    predictions <- c(predictions, circumpolar)
  }
  
  #project to visualise
  ant_view <- project(circumpolar, "epsg:6932")
  g1 <- ggplot() + geom_spatraster(data=ant_view) + theme_void() + 
    scale_fill_viridis_c(na.value="white", name = "Habitat Suitability") + 
    ggtitle(paste0(month(this.month, label=T), " ", year(this.month))) +
    theme(plot.title = element_text(hjust = 0.5))
  
  #save plot
  # ggsave(plot = g1, width = 10, height = 8,
  #        filename = paste0("output/future projections/", this.scenario, "/", month(this.month, label=T), "_", year(this.month), ".jpeg"))
  # 
  #print completion
  print(paste0(month(this.month, label=T), " ", year(this.month), " completed"))
}

#export
writeCDF(
  predictions,
  here("output", "future projections", this.scenario, paste0("projections_", this.scenario, ".nc"))
)


######################################################################################
## create monthly average predictions 
######################################################################################

#define scenario
this.scenario <- "ssp126" ## options: ssp585 or ssp126

#load all predictions
preds <- rast(
  here("output", "future projections", this.scenario, paste0("projections_", this.scenario, ".nc"))
)

#list all months 
months <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)

#for each month
tic()
for(i in months){
  
  #define month
  this.month <- i
  
  #subset predictions to this month
  monthly_preds <- preds[[month(time(preds)) == this.month]]
  
  #calculate monthly average
  monthly_avg <- mean(monthly_preds, na.rm = TRUE)
  
  #extract written month name
  longname <- as.character(month(time(monthly_preds[[1]]), label=T, abbr=F))
  
  #export
  writeCDF(
    monthly_avg,
    filename = here("output", "future projections", this.scenario, paste0("monthly_avg_", longname, "_", this.scenario, ".nc"))
  )
  
  #reproject
  monthly_avg <- project(monthly_avg, "epsg:6932")
  g1 <- ggplot() + geom_spatraster(data=monthly_avg) + theme_void() +
    scale_fill_viridis_c(na.value="white", name = "Habitat Suitability") +
    ggtitle(paste0(longname, " Average")) +
    theme(plot.title = element_text(hjust = 0.5))
  print(g1)

  #export
  ggsave(
    here("output", "future projections", this.scenario, paste0("monthly_avg_", longname, ".png")),
    g1, width = 10, height = 8
  )
}
toc()



