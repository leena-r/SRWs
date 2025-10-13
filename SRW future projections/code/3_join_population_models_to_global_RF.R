### SRW future projections

# script 3:
# This script joins SRW population level RFs [script 1] into a global RF model of foraging habitat suitability
# by using population level model predictions [script 2] as added covariates. 

######################################################################################
## load packages
######################################################################################

library(lubridate)
library(tidyr)
library(viridis)
library(tidyverse)
library(here)
library(tictoc)
library(ranger)
library(caret)
library(miceRanger)
library(terra)


######################################################################################
## read in presence-absence data  for each population
######################################################################################
# where the data was filtered for ARS locations only

df_OZ_data_pa_ARS <- read.csv(here("data", "df_OZ_data_pa_ARS.csv")) %>% 
  mutate(population = "OZ")
df_NZ_data_pa_ARS <- read.csv(here("data", "df_NZ_data_pa_ARS.csv")) %>% 
  mutate(population = "NZ")
df_SA_data_pa_ARS <- read.csv(here("data", "df_SA_data_pa_ARS.csv")) %>% 
  mutate(population = "SA")
df_ARG_data_pa_ARS <- read.csv(here("data", "df_ARG_data_pa_ARS.csv")) %>% 
  mutate(population = "ARG")


## join data
df_all <- rbind(df_OZ_data_pa_ARS, df_NZ_data_pa_ARS, 
                df_SA_data_pa_ARS, df_ARG_data_pa_ARS)
glimpse(df_all)


######################################################################################
## Extract population-level predictions for each data point
######################################################################################

# Define extent
e <- ext(-180, 180, -80, -30)

# Create a lookup for month names
month_lookup <- c("jan", "feb", "mar", "apr", "may", "jun",
                  "jul", "aug", "sep", "oct", "nov", "dec")

# Create an empty list to store each month's data
list_month_df_all_crop <- list()

# Loop through months 1 to 12
for (i in 1:12) {
  
  # Get month name
  month_name <- month_lookup[i]
  
  # Read rasters (use the finer resolution predictions)
  OZ_mean <- rast(here("output","global predictions", "model without currents", "montly population level_0.083grid",
                       paste0("OZ_mean ", month_name, "_foraging_presence.nc")))
  NZ_mean <- rast(here("output","global predictions", "model without currents", "montly population level_0.083grid",
                       paste0("NZ_mean ", month_name, "_foraging_presence.nc")))
  SA_mean <- rast(here("output","global predictions", "model without currents", "montly population level_0.083grid",
                       paste0("SA_mean ", month_name, "_foraging_presence.nc")))
  ARG_mean <- rast(here("output","global predictions", "model without currents", "montly population level_0.083grid",
                       paste0("ARG_mean ", month_name, "_foraging_presence.nc")))

  # Crop rasters
  OZ_mean_crop <- crop(OZ_mean, e)
  NZ_mean_crop <- crop(NZ_mean, e)
  SA_mean_crop <- crop(SA_mean, e)
  ARG_mean_crop <- crop(ARG_mean, e)
  
  # Subset df_all for the month
  month_df_all <- df_all %>%
    filter(month == i)
  
  # Convert to spatial object
  month_df_all_vect <- vect(month_df_all,
                            geom = c("x", "y"),
                            crs = crs(OZ_mean)) 
  
  # Crop to extent
  month_df_all_crop <- crop(month_df_all_vect, e)
  
  # Extract predictions
  month_df_all_crop$OZ_pred <- extract(OZ_mean_crop, month_df_all_crop, ID = FALSE)
  month_df_all_crop$NZ_pred <- extract(NZ_mean_crop, month_df_all_crop, ID = FALSE)
  month_df_all_crop$SA_pred <- extract(SA_mean_crop, month_df_all_crop, ID = FALSE)
  month_df_all_crop$ARG_pred <- extract(ARG_mean_crop, month_df_all_crop, ID = FALSE)
  
  # Convert to dataframe
  month_df_all_crop_df <- as.data.frame(month_df_all_crop, geom = "XY")
  
  # Save into list
  list_month_df_all_crop[[i]] <- month_df_all_crop_df
}

# Combine all months together
df_all_for_RF <- bind_rows(list_month_df_all_crop)


######################################################################################
## Prepare RF
######################################################################################

df_all_for_RF$trackID <- df_all_for_RF$id
df_all_for_RF <- df_all_for_RF %>% separate_wider_delim(id, delim = "-", names = c("whaleID", "segmentID"))

df_all_for_RF <- df_all_for_RF %>% select(-segmentID, trackID)

#create parameter grid to vary mtry between 2, 3, and 4
####mtry usually square root of the number of explanatory variables
## sqrt(14) = 3.7
param_grid <- expand.grid(mtry=2:4, splitrule = "gini", min.node.size=1)


folds <- groupKFold(group=df_all_for_RF$whaleID, k=10)

#setup 5-fold cross-validation
cv_scheme <- trainControl(method = "cv", number = length(folds), verboseIter = FALSE,
                          summaryFunction = twoClassSummary, classProbs = TRUE, index=folds)


#########################
#check for NA - less than 10% of training data okay for imputing

## all individual covariates <10% NAs

df_all_for_RF <- df_all_for_RF %>%  select(-X,-date, -uo, -vo, -wind_east, -wind_north, -x, -y)

#if(sum(is.na(df_all_for_RF)) < 0.101*nrow(df_all_for_RF) & sum(is.na(df_all_for_RF)) > 0){
df_all_for_RF_mice <- miceRanger(df_all_for_RF, m=1)
df_all_for_RF <- completeData(df_all_for_RF_mice)[[1]]
#}

##save df as it takes long time to impute values -- df_all_for_RF - all data, with NAs imputed 
# df_file_out <- here("data", "df_all_for_RF_without_currents_20241205.csv")
# write_csv(df_all_for_RF, file = df_file_out)

# path_data <- here("data", "df_all_for_RF_without_currents_20241205.csv")
# df_all_for_RF <- read.csv(path_data)


#########################
##add a season column, and use that as variable
###needs to be after imputation step or the imputation doesn't work

df_all_for_RF <- df_all_for_RF %>% 
  mutate(
    season1 = case_when(
      month %in% c(12,1,2) ~ "summer",
      month %in% c(3,4,5) ~ "autumn",
      month %in% c(6,7,8) ~ "winter",
      month %in% c(9,10,11) ~ "spring"
    )
  ) 


######################################################################################
## Run RF
######################################################################################
#perform tuning search
##explanatory variables
X <- df_all_for_RF %>% select(-pa,-trackID,-whaleID, -month, -population, -curr) 
Y <- as.factor(df_all_for_RF$pa)
## export X and Y for pdp later
# file.out <- here("data", "X_for_pdp.rds")
# saveRDS(X, file = file.out)
# file.out <- here("data", "Y_for_pdp.rds")
# saveRDS(Y, file = file.out)

tic()
m1 <- train(x = X, y = Y, method = "ranger", metric = "ROC", trControl = cv_scheme, 
            tuneGrid = param_grid, num.trees = 1000, importance = "impurity")
toc()
##foraging RF, backgrounds, no currents ~2h

varImp(m1)
m1$results
m1$bestTune

######################################################################################
#save model

## RF_joined_ARS_model.rds = all ind pop RFs joined into one RF
# file.out <- here("output", "RF_joined_ARS_model_no_currents_20241205.rds")
# saveRDS(m1, file = file.out)

##read in saved RF model built with joined RFs from all pops
path_data <- here("output", "RF_joined_ARS_model_no_currents_20241205.rds")
m1 <- readRDS(path_data)
varImp(m1)


######################################################################################
## Partial Dependency Plots
######################################################################################

# if need be read in X_for pdp and Y_for pdp RDS files
file_X <- here("data", "X_for_pdp.rds")
file_Y <- here("data", "Y_for_pdp.rds")
# Read the data
X <- readRDS(file_X)
Y <- readRDS(file_Y)


library(plotmo)  

file.out <- "C:/Users/lrie0/OneDrive/Rutherford SRW/Paper - Future projections/writing/Supplementary Materials/partial_dependence_plot_circumpolar.png"
png(file.out, width = 3000, height = 2000, res = 300)  # start the graphics device

plotmo(m1,
       type = "prob",
       nresponse = 2,
       degree1 = TRUE,
       varmod.method = "lm",
       varmod.show = TRUE,
       vars = c("ARG_pred", "OZ_pred", "SA_pred", "NZ_pred", "dshelf", "depth", "ssh",
                "sst", "sal", "chl",  "wind", "mld", "slope", "season1", "sic"),
       ylim = NA)   # This allows each plot its own y-axis

dev.off() 



