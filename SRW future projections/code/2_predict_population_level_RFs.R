### SRW future projections

# script 2:
# This script predicts monthly SRW foraging habitat suitability (over 10 years: 2014–2023)
# using population-level Random Forest (RF) models [script 1] and environmental covariates.


######################################################################################
## load packages
######################################################################################

library(terra)
library(tidyverse)
library(viridis)
library(here)

######################################################################################

## when originally ran, some env data was at 0.083 grid resolution
## re-did each population's present day predictions using GLORYS data resampled to a 1 degree resolution 
## to be consistent with future CMIP projection resolution

######################################################################################
## Prep environmental data layers
######################################################################################

## read in env data

##chl goes to April 2024 but other data don't, grab up to Dec 2023
chl <- rast(here("data", "deltas", "chlos", "satellite_data", "monthly_original.nc"))[[1:120]]

##most data start at 2000, only grab 2014 onwards
mld <- rast(here("data", "deltas", "mlotst", "satellite_data", "monthly_original_updated.nc"))[[169:288]]
sal <- rast(here("data", "deltas", "sos", "satellite_data", "monthly_original_updated.nc"))[[169:288]]
sic1 <- rast(here("data", "deltas", "siconc", "satellite_data", "monthly_original_updated.nc"))[[169:288]]
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
env_dynamic <- c(chl_cropped, mld, sal, sst, ssh, sic, wind_cropped) 
env_static <- c(depth_cropped, dshelf_cropped, slope_cropped)


######################################################################################


##                population monthly predictions

######################################################################################
## Prediction settings
######################################################################################
populations <- c("NZ", "OZ", "SA", "ARG")
years <- 2014:2023
months <- 1:12


######################################################################################
## Run predictions 
######################################################################################
# Loop over populations
for (pop in populations) {
  
  # Set path for the corresponding population RF model
  path_data <- here("output", paste0("RF_", pop, "_data_v1_no_currents_foraging_20241203.rds"))
  
  # Read the model
  m1 <- readRDS(path_data)
  
  # Loop over months
  for (month_selection in months) {
    
    # Determine season based on the month
    season_selection <- case_when(
      month_selection %in% c(12, 1, 2) ~ "summer",
      month_selection %in% c(3, 4, 5) ~ "autumn",
      month_selection %in% c(6, 7, 8) ~ "winter",
      month_selection %in% c(9, 10, 11) ~ "spring"
    )
    
    # Initialize raster stack for averaging
    raster_stack <- list()
    
    # Loop over years
    for (year_selection in years) {
      
      # Select dynamic env data for the selected year
      var_year_selection <- env_dynamic[[year(time(env_dynamic)) == year_selection]]
      
      # Check if year data is valid
      if (is.null(var_year_selection)) next  # Skip if no data
      
      # Select dynamic env data for the selected month within that year
      var_month_year_selection <- var_year_selection[[month(time(var_year_selection)) == month_selection]]
      
      # Check if month data is valid
      if (is.null(var_month_year_selection)) next  # Skip if no data
      
      # Create a season raster
      season_raster <- rast(ext = ext(sst), res = res(sst), crs = crs(sst))
      values(season_raster) <- season_selection
      
      # Ensure variable names match RF model covariates
      var_month_year_selection <- c(var_month_year_selection, env_static, season_raster)
      names(var_month_year_selection) <- c("chl", "mld", "sal", "sst", "ssh", "sic", 'wind', 
                                           'depth', 'dshelf', 'slope', "season1")
      
      # Run model prediction
      pred_m1_month_year_selection <- terra::predict(var_month_year_selection, m1, na.rm = TRUE, type = "prob")
      
      # Store the presence raster for averaging
      if (!is.null(pred_m1_month_year_selection$presence)) {
        raster_stack[[length(raster_stack) + 1]] <- pred_m1_month_year_selection$presence
      }
      
      # Optional: Plot each year's prediction
      plot(pred_m1_month_year_selection$presence, 
           plg = list(title = paste0(year_selection, "_", month_selection)),
           col = viridis_pal(begin = 0, end = 1, option = "D")(200)) 
    }
    
    # Check if raster stack has valid data before averaging predictions across years
    if (length(raster_stack) > 0) {
      raster_stack_month <- do.call(c, raster_stack)  # Convert list to a raster stack
      mean_raster <- app(raster_stack_month, mean, na.rm = TRUE)
      
      # Export the mean presence raster
      output_filename <- here(
        "output", "global predictions", "model without currents", "1deg_resolution", 
        paste0(pop, "_mean_", month_selection, "_foraging_presence_1deg.nc")
      )
      writeCDF(mean_raster, filename = output_filename, 
               varname = "mean presence", 
               longname = "mean presence_foraging RF",
               unit = " ")
    } else {
      message("No valid data for ", pop, " in month ", month_selection)
    }
  }
}


