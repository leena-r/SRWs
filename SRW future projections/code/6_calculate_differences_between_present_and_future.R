### SRW future projections

# script 6:
# This script calculates differences in monthly circumpolar habitat suitability predictions 
# between present day and future scenarios 


######################################################################################
## load packages
######################################################################################

library(tidyverse)
library(terra)
library(tidyterra)
library(here)
library(scico)
library(tictoc)

######################################################################################

rm(list=ls())

#loop over each month
for(i in c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)){
  
  #define month
  this.month <- i
  
  #define longname for month for reading files
  longname <- as.character(month(this.month, label=T, abbr=F))
  
  shortname <- 
    case_when(longname == "January" ~ "jan",
              longname == "February" ~ "feb",
              longname == "March" ~ "mar",
              longname == "April" ~ "apr",
              longname == "May" ~ "may",
              longname == "June" ~ "jun",
              longname == "July" ~ "jul",
              longname == "August" ~ "aug",
              longname == "September" ~ "sep",
              longname == "October" ~ "oct",
              longname == "November" ~ "nov",
              longname == "December" ~ "dec",
    )
  
  #read in monthly predictions and projections
  modern <- rast(
    here("output", "global predictions", "model without currents", "1deg_resolution",
         paste0("all_mean_", this.month, "_foraging_presence_1deg.nc")))
  ssp126 <- rast(
    here("output", "future projections", "ssp126",
         paste0("monthly_avg_", longname, "_ssp126_updated.nc")))
  ssp585 <- rast(
    here("output", "future projections", "ssp585",
         paste0("monthly_avg_", longname, "_ssp585_updated.nc")))
  
  
  #resample ssp scenarios to modern grid
  ssp126 <- resample(ssp126, modern, method = "bilinear", threads = T)
  ssp585 <- resample(ssp585, modern, method = "bilinear", threads = T)
  
  
  #calculate differences
  diff126 <- ssp126 - modern
  diff585 <- ssp585 - modern
  
  
  #plot differences
  diff126_ant <- project(diff126, "epsg:6932")
  p1 <- ggplot() + geom_spatraster(data = diff126_ant) +
    scale_fill_gradient2(limits = c(-0.5, 0.5), 
                         oob = scales::squish) + 
    theme_void() +
    ggtitle(paste0(longname, " Average")) +
    theme(plot.title = element_text(hjust = 0.5))
  print(p1)
  
  diff585_ant <- project(diff585, "epsg:6932")
  p2 <- ggplot() + geom_spatraster(data = diff585_ant) +
    scale_fill_gradient2(limits = c(-0.5, 0.5), 
                         oob = scales::squish) + 
    theme_void() +
    ggtitle(paste0(longname, " Average")) +
    theme(plot.title = element_text(hjust = 0.5))
  print(p2)
  
  #export plots
  # ggsave(
  #   here("output", "differences", "ssp126", paste0(longname, "_ssp126_updated.png")),
  #   p1, width = 6, height = 6, dpi = 300)
  # ggsave(
  #   here("output", "differences", "ssp585", paste0(longname, "_ssp585_updated.png")),
  #   p2, width = 6, height = 6, dpi = 300)
  
  #join rasters to other months
  if(!exists("ssp126_diffs")){
    ssp126_diffs <- diff126
    ssp585_diffs <- diff585
  } else {
    ssp126_diffs <- c(ssp126_diffs, diff126)
    ssp585_diffs <- c(ssp585_diffs, diff585)
  }
  
}

#create time information
for(i in c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)){
  
  #define month
  this.month <- i
  
  #create date
  date <- as_date(paste0("2023-", this.month, "-01"))
  
  #join to all dates 
  if(!exists("all_dates")){
    all_dates <- date
  } else {
    all_dates <- c(all_dates, date)
  }
}

#assign times
time(ssp126_diffs) <- all_dates
time(ssp585_diffs) <- all_dates

##select specific month
#test <- ssp585_diffs[[(time(ssp585_diffs)) == "2023-12-01"]]
#time(test)

#export difference rasters
writeCDF(
  ssp126_diffs,
  here("output", "differences", "ssp126", "ssp126_diffs_updated_1deg.nc"),
  overwrite = TRUE)
writeCDF(
  ssp585_diffs,
  here("output", "differences", "ssp585", "ssp585_diffs_updated_1deg.nc"),
  overwrite = TRUE)


######################################################################################
## make scaled difference maps for figure 2
######################################################################################

#list all months 
months <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)

#define scenario
this.scenario <- "ssp126" ## options: ssp585 or ssp126

#for each month
tic()
for(i in months){
  
  #define month
  this.month <- i
  
  #load difference nc
  diffs <- rast(
    here("output", "differences", this.scenario, paste0(this.scenario, "_diffs_updated_1deg.nc"))
  )
  
  #subset predictions to this month
  monthly_diffs <- diffs[[month(time(diffs)) == this.month]]
  
  #extract written month name
  longname <- as.character(month(time(monthly_diffs[[1]]), label=T, abbr=F))
  
  #reproject
  monthly_diffs <- project(monthly_diffs, "epsg:6932")
  g1 <- ggplot() + geom_spatraster(data=monthly_diffs) + 
    theme_void() + 
    scale_fill_scico(palette = "vik", na.value = "white", name = "Difference in habitat Suitability",
                     limits = c(-0.7, 0.7), oob = scales::squish) + 
    ggtitle(paste0(longname, " difference")) +
    theme(plot.title = element_text(hjust = 0.5))
  print(g1)
  
  #export
  ggsave(
    here("output", "differences", this.scenario,
         paste0(longname, "_", this.scenario, "_difference_updated_scaled_1deg.png")),
    g1,
    width = 10,
    height = 8
  )
}
toc()





