### SRW future projections

# script 4:
# This script predicts monthly SRW foraging habitat suitability (over 10 years: 2014–2023)
# using global Random Forest (RF) models [script 3] and environmental covariates.


######################################################################################
## load packages
######################################################################################


library(terra)
library(tidyverse)
library(tidyterra)
library(tictoc)
library(viridis)
library(here)


######################################################################################

## when originally ran, some env data was at 0.083 grid resolution
## re-did global present day predictions using GLORYS data resampled to a 1 degree resolution 
## to be consistent with future CMIP projection resolution

######################################################################################
## Prep environmental data layers
######################################################################################

## read in env data
##some env data was processed in script 2 (predict_population_level_RFs)

##chl goes to April 2024 but other data don't, grab up to Dec 2023
chl <- rast(here("data", "deltas", "chlos", "satellite_data", "monthly_original.nc"))[[1:120]]

##most data start at 2000, only grab 2014 onwards
mld <- rast(here("data", "deltas", "mlotst", "satellite_data", "monthly_original_updated.nc"))[[169:288]]
sal <- rast(here("data", "deltas", "sos", "satellite_data", "monthly_original_updated.nc"))[[169:288]]
sic <- rast(here("data", "deltas", "siconc", "satellite_data", "monthly_original_updated.nc"))[[169:288]]
  # Replace NA sea-ice concentration values with 0
  sic <- sic %>% replace(is.na(.), 0)
ssh <- rast(here("data", "deltas", "zos", "satellite_data", "monthly_original_updated.nc"))[[169:288]]
sst <- rast(here("data", "deltas", "tos", "satellite_data", "monthly_original_updated.nc"))[[169:288]]
wind <- rast(here("data", "deltas", "wind", "satellite_data", "monthly_original.nc"))[[174:293]]

depth <- rast(here("data", "monthly", "depth_resampled_1deg.nc"))
dshelf <- rast(here("data", "monthly", "dshelf_resampled_1deg.nc"))
slope <- rast(here("data", "monthly", "slope_resampled_1deg.nc"))



## Crop layers for consistent extent
e <- ext(sst)
wind_cropped <- crop(wind, e)
chl_cropped <- crop(chl, e)
depth_cropped <- crop(depth, e)
dshelf_cropped <- crop(dshelf, e)
slope_cropped <- crop(slope, e)



# Combine dynamic and static stacks
env_dynamic <- c(chl_cropped, mld, sal, sst, ssh, sic, wind_cropped) #, curr - drop currents as not available in CMIP
env_static <- c(depth_cropped, dshelf_cropped, slope_cropped)



######################################################################################

##                Global habitat model monthly predictions

######################################################################################
## Prediction settings
######################################################################################
years <- 2014:2023
months <- 1:12


######################################################################################
## Run predictions 
######################################################################################
# # Set working directory for reading/writing files
# setwd("D:/UoA postdoc/Rutherford SRW - important bits as hard drive full/Paper - Future projections/output/global predictions/model without currents/1deg_resolution")

# Load the global RF model
path_data <- here("output", "RF_joined_ARS_model_no_currents_20241205.rds")
m1 <- readRDS(path_data)


# Loop through each month
for (month_selection in months) {
  
  # Determine season based on the selected month
  season_selection <- case_when(
    month_selection %in% c(12, 1, 2) ~ "summer",
    month_selection %in% c(3, 4, 5) ~ "autumn",
    month_selection %in% c(6, 7, 8) ~ "winter",
    month_selection %in% c(9, 10, 11) ~ "spring"
  )
  
  # Initialize an empty list to store predicted rasters for each year
  raster_stack <- list()
  
  # Loop through each year
  for (year_selection in years) {
    
    ## Select year
    var_year_selection <- env_dynamic[[year(time(env_dynamic)) == year_selection]]
    
    ## Check if year data is valid
    if (is.null(var_year_selection)) {
      warning(paste("No data for year:", year_selection))
      next  # Skip if no data
    }
    
    ## Select month within that year
    var_month_year_selection <- var_year_selection[[month(time(var_year_selection)) == month_selection]]
    
    ## Check if month data is valid
    if (is.null(var_month_year_selection)) {
      warning(paste("No data for month:", month_selection, "in year:", year_selection))
      next  # Skip if no data
    }
    
    ## Create a season raster
    season_raster <- rast(ext = ext(sst), res = res(sst), crs = crs(sst))
    values(season_raster) <- season_selection
    
    ## Read in the raster for each population for the selected month
    OZ_mean <- rast(here("output", "global predictions", "model without currents", "1deg_resolution", 
                         paste0("OZ_mean_", month_selection, "_foraging_presence_1deg.nc")))
    OZ_mean_crop <- crop(OZ_mean, e)
    
    NZ_mean <- rast(here("output", "global predictions", "model without currents", "1deg_resolution", 
                         paste0("NZ_mean_", month_selection, "_foraging_presence_1deg.nc")))
    NZ_mean_crop <- crop(NZ_mean, e)
    
    
    SA_mean <- rast(here("output", "global predictions", "model without currents", "1deg_resolution", 
                         paste0("SA_mean_", month_selection, "_foraging_presence_1deg.nc")))
    SA_mean_crop <- crop(SA_mean, e)
    
    ARG_mean <- rast(here("output", "global predictions", "model without currents", "1deg_resolution", 
                         paste0("ARG_mean_", month_selection, "_foraging_presence_1deg.nc")))
    ARG_mean_crop <- crop(ARG_mean, e)
    
    ## Combine covariates
    var_month_year_selection <- c(var_month_year_selection, env_static, season_raster,
                                  OZ_mean_crop, NZ_mean_crop, SA_mean_crop, ARG_mean_crop)
    
    names(var_month_year_selection) <- c("chl", "mld", "sal", "sst", "ssh", "sic", "wind", 
                                         "depth", "dshelf", "slope", "season1", 
                                         "ARG_pred", "OZ_pred", "SA_pred", "NZ_pred")
    
    ## Predict presence probability
    tic()
    pred_m1_month_year_selection <- terra::predict(var_month_year_selection, m1, na.rm = TRUE, type = "prob")
    toc()
    
    
    
    # Store the presence raster for averaging
    if (!is.null(pred_m1_month_year_selection$presence)) {
      raster_stack[[length(raster_stack) + 1]] <- pred_m1_month_year_selection$presence
    }
    
    # Optional: Plot each year's prediction
    plot(pred_m1_month_year_selection$presence, 
         plg = list(title = paste0(year_selection, "_", month_selection)),
         col = viridis_pal(begin = 0, end = 1, option = "D")(200)) 
  }
  
  # Check if raster stack has valid data before averaging
  if (length(raster_stack) > 0) {
    raster_stack_month <- do.call(c, raster_stack)  # Convert list to a raster stack
    mean_raster <- app(raster_stack_month, mean, na.rm = TRUE)
    
    # Export the mean presence raster
    output_filename <- here("output", "global predictions", "model without currents", 
                            "1deg_resolution", paste0("all_mean_", month_selection, "_foraging_presence_1deg_20251013.nc"))
    writeCDF(mean_raster, filename = output_filename, 
             varname = "mean_presence", 
             longname = "mean presence_foraging RF",
             unit = " ")
    
  } else {
    message("No valid data for ", pop, " in month ", month_selection)
  }
}



###########################################################################
## make maps for figure 2
###########################################################################

# # List all months
# months <- 1:12
# 
# # Loop over months
# tic()
# for(i in months){
#   
#   # Define month
#   this.month <- i
#   
#   # Load present day predictions using here()
#   preds <- rast(here("output", "global predictions", "model without currents", 
#                      "1deg_resolution", paste0("all_mean_", this.month, "_foraging_presence_1deg.nc")))
#   
#   # Reproject
#   preds <- project(preds, "epsg:6932")
#   
#   # Plot
#   g1 <- ggplot() + 
#     geom_spatraster(data = preds) + 
#     theme_void() + 
#     scale_fill_viridis_c(
#       na.value = "white", 
#       name = "Habitat Suitability",
#       limits = c(0, 0.8), 
#       oob = scales::squish
#     ) + 
#     ggtitle(paste0(this.month, " present day")) +
#     theme(plot.title = element_text(hjust = 0.5))
#   
#   print(g1)
#   
#   # Export using here()
#   ggsave(
#     here("output", "global predictions", "model without currents", "1deg_resolution", 
#          paste0(this.month, "_avg_present_day_scaled_1deg.png")), 
#     g1, width = 10, height = 8
#   )
# }
# toc()
# 



