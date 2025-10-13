### SRW future projections

# script 1:
# This script builds population-level random forest models predicting
# foraging habitat (presence/absence) for southern right whales across four populations:
# Australia (OZ), New Zealand (NZ), South Africa (SA), and Argentina (ARG).

##uses SRW state-space modelled data, to which environmental variables have been extracted 

######################################################################################
## load packages
######################################################################################

library(lubridate)
library(tidyr)
library(tidyverse)
library(here)
library(tictoc)
library(ranger)
library(caret)
library(miceRanger)
library(terra)


######################################################################################
## OZ
######################################################################################
## read in SRW tracking file with env var extractions

path_data <- here("data", "OZ_presences_extracted.csv")
data_raw_OZ <- read.csv(path_data)

##create a presence/absence column - all tracking data points are presences
data_raw_OZ$pa <- "presence"

##has repeated X/row id column
data_raw_OZ <- data_raw_OZ %>% select(-X.1)

data_raw_OZ <- data_raw_OZ %>% 
mutate(month = str_sub(date,6,7))

glimpse(data_raw_OZ)


#############################
## read in absences

path_data_abs <- here("data", "OZ_background_extracted.csv")
background_OZ <- read.csv(path_data_abs)

##create a presence/absence column - buffer points are absences
background_OZ$pa <- "absence"

background_OZ <- background_OZ %>% 
  mutate(month = str_sub(date,6,7))

glimpse(background_OZ)


#############################
## make one df
####keep ID column so we can use it in cross validation step

#remove day column from buffer file
background_OZ <- background_OZ %>% select(-day)

df <- rbind(data_raw_OZ,background_OZ)


#############################
## files have dropped behavioural mode (g) column during env extraction
##want to use it to filter data for foraging locations only
##read in raw ssm output to which env data were extracted 
path_data_ssm <- here("data", "OZ_SRW_SSM_track_data.csv")
data_raw_ssm_OZ <- read.csv(path_data_ssm)

## select columns
data_raw_ssm_OZ <- data_raw_ssm_OZ %>% select(id, date,g)

data_raw_ssm_OZ <- data_raw_ssm_OZ %>% 
  mutate(day = str_sub(date,1,10),
         time = str_sub(date,12,19)) %>% 
  select(-date) %>% 
  mutate(
    date = paste(day, time, sep = " ")) %>% 
  select(-day, -time)

glimpse(data_raw_ssm_OZ)

##join g to data
df <- left_join(df, data_raw_ssm_OZ)
##spot checked, looks good, correct g value joined


#############################
## filter data based on behaviour (g value)
## OZ pop mean g was 0.60539

df <- df %>% filter(g < 0.60539) %>% 
  ##but drop g as don't want to use it as a covariate
  select(-g)

# df_file.out <- here("data", "df_OZ_data_pa_ARS.csv")
# write_csv(df, file = df_file.out)


#############################
## RF settings

df$trackID <- df$id
df <- df %>% separate_wider_delim(id, delim = "-", names = c("whaleID", "segmentID"))

df <- df %>% select(-segmentID, trackID)

#create parameter grid to vary mtry between 2, 3, and 4
####mtry usually square root of the number of explanatory variables
## sqrt(11) = 3.3
param_grid <- expand.grid(mtry=2:4, splitrule = "gini", min.node.size=1)

folds <- groupKFold(group=df$whaleID, k=10)

#setup 5-fold cross-validation
cv_scheme <- trainControl(method = "cv", number = length(folds), verboseIter = FALSE,
                          summaryFunction = twoClassSummary, classProbs = TRUE, index=folds)


#############################
#check for NA - less than 10% of training data okay for imputing

##chl only variable with NAs (OZ data)
## sum(is.na(df$chl)) / nrow(df) *100 == 8.38%
##some NAs for SSH in background data

df <- df %>%  select(-X,-date, -uo, -vo, -wind_east, -wind_north, -x, -y)

if(sum(is.na(df)) < 0.101*nrow(df) & sum(is.na(df)) > 0){
  df_mice <- miceRanger(df, m=1)
  df <- completeData(df_mice)[[1]]
}


#############################
##add a season column, and use that as variable
###needs to be after imputation step or the imputation doesn't work

## tested out season1 (spring, summer, autumn, winter) as well as season2 (spring-summer, autumn-witner)
## Season1 deemed better (higher variable importance score) and used in final model

df <- df %>% 
  mutate(
    season1 = case_when(
      month %in% c("12","01","02") ~ "summer",
      month %in% c("03","04","05") ~ "autumn",
      month %in% c("06","07","08") ~ "winter",
      month %in% c("09","10","11") ~ "spring"
    )
  ) %>% 
  select(-month) %>% 
  mutate(
    season2 = case_when(
      season1 %in% c("spring","summer") ~ "spring-summer",
      season1 %in% c("autumn","winter") ~ "autumn-winter")
  )

#############################
#perform tuning search
##explanatory variables
X <- df %>% select(-pa,-trackID,-season2,-whaleID, -curr) ##drop currents as not available in CMIP
Y <- as.factor(df$pa)
tic()
m1 <- train(x = X, y = Y, method = "ranger", metric = "ROC", trControl = cv_scheme, 
                 tuneGrid = param_grid, num.trees = 1000, importance = "impurity")
toc()
##~1min for foraging locations only RF (without currents)

varImp(m1)
m1$results
m1$bestTune

#############################
#save model

# file.out <- here("output", "RF_OZ_data_v1_no_currents_foraging_20241203.rds")
# saveRDS(m1, file = file.out)




######################################################################################
## NZ
######################################################################################
## read in SRW tracking file with env var extractions

path_data <- here("data", "NZ_presences_extracted.csv")
data_raw_NZ <- read.csv(path_data)

##create a presence/absence column - all tracking data points are presences
data_raw_NZ$pa <- "presence"

##has repeated X/row id column
data_raw_NZ <- data_raw_NZ %>% select(-X.1)

data_raw_NZ <- data_raw_NZ %>% 
  mutate(month = str_sub(date,6,7))

glimpse(data_raw_NZ)

#############################
## read in absences

path_data_abs <- here("data", "NZ_background_extracted.csv")
background_NZ <- read.csv(path_data_abs)

##create a presence/absence column - buffer points are absences
background_NZ$pa <- "absence"

background_NZ <- background_NZ %>% 
  mutate(month = str_sub(date,6,7))

glimpse(background_NZ)


#############################
## make one df
####keep ID column so we can use it in cross validation step

#remove day column from buffer file
background_NZ <- background_NZ %>% select(-day)

df <- rbind(data_raw_NZ,background_NZ)


#############################
## files have dropped behavioural mode (g) column during env extraction
##want to use it to filter data for foraging locations only
##read in raw ssm output that was sent to Josh for env var extraction
path_data_ssm <- here("data", "NZ_SRW_SSM_track_data.csv")
data_raw_ssm_NZ <- read.csv(path_data_ssm)
## select columns
data_raw_ssm_NZ <- data_raw_ssm_NZ %>% select(id, date,g)

data_raw_ssm_NZ <- data_raw_ssm_NZ %>% 
  mutate(day = str_sub(date,1,10),
         time = str_sub(date,12,19)) %>% 
  select(-date) %>% 
  mutate(
    date = paste(day, time, sep = " ")) %>% 
  select(-day, -time)

glimpse(data_raw_ssm_NZ)


##join g to data
df <- left_join(df, data_raw_ssm_NZ)
##spot checked, looks good, correct g value joined


#############################
## filter data based on behaviour (g value)
##NZ pop mean g was 0.82387

df <- df %>% filter(g < 0.82387) %>% 
  ##but drop g as don't want to use it as a covariate
  select(-g)

# df_file.out <- here("data", "df_NZ_data_pa_ARS.csv")
# write_csv(df, file = df_file.out)


#############################
## RF settings
df$trackID <- df$id
df <- df %>% separate_wider_delim(id, delim = "-", names = c("whaleID", "segmentID"))

df <- df %>% select(-segmentID, trackID)

#create parameter grid to vary mtry between 2, 3, and 4
####mtry usually square root of the number of explanatory variables
## sqrt(11) = 3.3
param_grid <- expand.grid(mtry=2:4, splitrule = "gini", min.node.size=1)

folds <- groupKFold(group=df$whaleID, k=10)

#setup 5-fold cross-validation
cv_scheme <- trainControl(method = "cv", number = length(folds), verboseIter = FALSE,
                          summaryFunction = twoClassSummary, classProbs = TRUE, index=folds)


#############################
#check for NA - less than 10% of training data okay for imputing

##NZ data has various variables with NAs 
##slope: 2 rows NA
##dshelf: 2 rows NA 
##mld: 1 row NA
##chl: 2150 NAs -- sum(is.na(df$chl)) / nrow(df) *100 == 6.79% 

df <- df %>%  select(-X,-date, -uo, -vo, -wind_east, -wind_north, -x, -y)

if(sum(is.na(df)) < 0.101*nrow(df) & sum(is.na(df)) > 0){
  df_mice <- miceRanger(df, m=1)
  df <- completeData(df_mice)[[1]]
}
##runs RF to figure dist of the variable, takes into account other data
##randomly distributes it


#############################
#add a season column

df <- df %>% 
  mutate(
    season1 = case_when(
      month %in% c("12","01","02") ~ "summer",
      month %in% c("03","04","05") ~ "autumn",
      month %in% c("06","07","08") ~ "winter",
      month %in% c("09","10","11") ~ "spring"
    )
  ) %>% 
  select(-month) %>% 
  mutate(
    season2 = case_when(
      season1 %in% c("spring","summer") ~ "spring-summer",
      season1 %in% c("autumn","winter") ~ "autumn-winter")
  )


#############################
#perform tuning search
##explanatory variables
X <- df %>% select(-pa,-trackID,-season2,-whaleID, -curr) ##drop currents as not available in CMIP
Y <- as.factor(df$pa)
tic()
m1 <- train(x = X, y = Y, method = "ranger", metric = "ROC", trControl = cv_scheme, 
            tuneGrid = param_grid, num.trees = 1000, importance = "impurity")
toc()
##foraging RF, background absences, foraging model: ~4min

varImp(m1)
m1$results
m1$bestTune


#############################
#save model

# file.out <- here("output", "RF_NZ_data_v1_no_currents_foraging_20241203.rds")
# saveRDS(m1, file = file.out)

######################################################################################




######################################################################################
## SA
######################################################################################
## read in SRW tracking file with env var extractions

path_data <- here("data", "SA_presences_extracted.csv")
data_raw_SA <- read.csv(path_data)

##create a presence/absence column - all tracking data points are presences
data_raw_SA$pa <- "presence"

data_raw_SA <- data_raw_SA %>% 
  mutate(month = str_sub(date,6,7))

glimpse(data_raw_SA)


#############################
## read in background data - absences

path_data_abs <- here("data", "SA_background_extracted.csv")
background_SA <- read.csv(path_data_abs)

##create a presence/absence column - backround points are absences
background_SA$pa <- "absence"

background_SA <- background_SA %>% 
  select(-month) %>% ##already had a month column but was NAs. has unnecessary day column
  mutate(month = str_sub(date,6,7))

glimpse(background_SA)


#############################
## make one df
####keep ID column so we can use it in cross validation step

#remove day column from background file
background_SA <- background_SA %>% select(-day)

df <- rbind(data_raw_SA,background_SA)


#############################
## files have dropped behavioural mode (g) column during env extraction
##want to use it to filter data for foraging locations only
##read in raw ssm output that was sent to Josh for env var extraction
path_data_ssm <- here("data", "SA_SRW_SSM_track_data.csv")
data_raw_ssm_SA <- read.csv(path_data_ssm)
## select columns
data_raw_ssm_SA <- data_raw_ssm_SA %>% select(id, date,g)

data_raw_ssm_SA <- data_raw_ssm_SA %>% 
  mutate(day = str_sub(date,1,10),
         time = str_sub(date,12,19)) %>% 
  select(-date) %>% 
  mutate(
    date = paste(day, time, sep = " ")) %>% 
  select(-day, -time)

glimpse(data_raw_ssm_SA)

##join g to data
df <- left_join(df, data_raw_ssm_SA)
##spot checked, looks good, correct g value joined


#############################
##  filter data based on behaviour (g value)
##NZ pop mean g was 0.59489

df <- df %>% filter(g < 0.59489) %>% 
  ##but drop g as don't want to use it as a covariate
  select(-g)

# df_file.out <- here("data", "df_SA_data_pa_ARS.csv")
# write_csv(df, file = df_file.out)


#############################
## RF settings
df$trackID <- df$id
df <- df %>% separate_wider_delim(id, delim = "-", names = c("whaleID", "segmentID"))

df <- df %>% select(-segmentID, trackID)

#create parameter grid to vary mtry between 2, 3, and 4
####mtry usually square root of the number of explanatory variables
## sqrt(11) = 3.3
param_grid <- expand.grid(mtry=2:4, splitrule = "gini", min.node.size=1)

folds <- groupKFold(group=df$whaleID, k=10)

#setup 5-fold cross-validation
cv_scheme <- trainControl(method = "cv", number = length(folds), verboseIter = FALSE,
                          summaryFunction = twoClassSummary, classProbs = TRUE, index=folds)


#############################
#check for NA - less than 10% of training data okay for imputing

##NZ data has various variables with NAs 
##dshelf: 788rows ==  1.9%
##sst/mld/sal/ssh/curr: all 4 NA rows  
##chl: sum(is.na(df$chl)) / nrow(df) *100 == 3852rows == 9.3%
##wind sum(is.na(df$wind)) / nrow(df) *100 == 3248rows == 7.9%
### total is 19% of full df -- sum(is.na(df)) / nrow(df) *100, but individual variable NAs <10%

df <- df %>%  select(-X,-date, -uo, -vo, -wind_east, -wind_north, -x, -y)

#if(sum(is.na(df)) < 0.101*nrow(df) & sum(is.na(df)) > 0){
  df_mice <- miceRanger(df, m=1)
  df <- completeData(df_mice)[[1]]
#}
##runs RF to figure dist of the variable, takes into account other data
##randomly distributes it

  
#############################
##add a season column

df <- df %>% 
  mutate(
    season1 = case_when(
      month %in% c("12","01","02") ~ "summer",
      month %in% c("03","04","05") ~ "autumn",
      month %in% c("06","07","08") ~ "winter",
      month %in% c("09","10","11") ~ "spring"
    )
  ) %>% 
  select(-month) %>% 
  mutate(
    season2 = case_when(
      season1 %in% c("spring","summer") ~ "spring-summer",
      season1 %in% c("autumn","winter") ~ "autumn-winter")
  )


#############################
#perform tuning search
##explanatory variables
X <- df %>% select(-pa,-trackID,-season2,-whaleID, -curr) ##drop currents as not available in CMIP
Y <- as.factor(df$pa)
tic()
m1 <- train(x = X, y = Y, method = "ranger", metric = "ROC", trControl = cv_scheme, 
            tuneGrid = param_grid, num.trees = 1000, importance = "impurity")
toc()
##foraging RF, backgrounds ~8min without currents

varImp(m1)
m1$results
m1$bestTune


#############################
#save model

# file.out <- here("output", "RF_SA_data_v1_no_currents_foraging_20241203.rds")
# saveRDS(m1, file = file.out)

######################################################################################




######################################################################################
## ARG
######################################################################################
## read in SRW tracking file with env var extractions

path_data <- here("data", "ARG_presences_extracted.csv")
data_raw_ARG <- read.csv(path_data)

##create a presence/absence column - all tracking data points are presences
data_raw_ARG$pa <- "presence"

data_raw_ARG <- data_raw_ARG %>% 
  mutate(month = str_sub(date,6,7))

glimpse(data_raw_ARG)


#############################
## read in background data - absences

path_data_abs <- here("data", "ARG_background_extracted.csv")
background_ARG <- read.csv(path_data_abs)

##create a presence/absence column - backround points are absences
background_ARG$pa <- "absence"

background_ARG <- background_ARG %>% 
  select(-month) %>% ##already had a month column but was NAs. has unnecesARGry day column
  mutate(month = str_sub(date,6,7))

glimpse(background_ARG)


#############################
## make one df
####keep ID column so we can use it in cross validation step

#remove day column from background file
background_ARG <- background_ARG %>% select(-day)

df <- rbind(data_raw_ARG,background_ARG)


#############################
## files have dropped behavioural mode (g) column during env extraction
##want to use it to filter data for foraging locations only
##read in raw ssm output that was sent to Josh for env var extraction
path_data_ssm <- here("data", "ARG_SRW_SSM_track_data.csv")
data_raw_ssm_ARG <- read.csv(path_data_ssm)
## select columns
data_raw_ssm_ARG <- data_raw_ssm_ARG %>% select(id, date,g)

data_raw_ssm_ARG <- data_raw_ssm_ARG %>% 
  mutate(day = str_sub(date,1,10),
         time = str_sub(date,12,19)) %>% 
  select(-date) %>% 
  mutate(
    date = paste(day, time, sep = " ")) %>% 
  select(-day, -time)

glimpse(data_raw_ssm_ARG)

##join g to data
df <- left_join(df, data_raw_ssm_ARG)
##spot checked, looks good, correct g value joined


#############################
## filter data based on behaviour (g value)
##NZ pop mean g was 0.561022

df <- df %>% filter(g < 0.561022) %>% 
  ##but drop g as don't want to use it as a covariate
  select(-g)

# df_file.out <- here("data", "df_ARG_data_pa_ARS.csv")
# write_csv(df, file = df_file.out)


#############################
## RF settings
df$trackID <- df$id
df <- df %>% separate_wider_delim(id, delim = "-", names = c("whaleID", "segmentID"))

df <- df %>% select(-segmentID, trackID)

#create parameter grid to vary mtry between 2, 3, and 4
####mtry usually square root of the number of explanatory variables
## sqrt(11) = 3.3
param_grid <- expand.grid(mtry=2:4, splitrule = "gini", min.node.size=1)

folds <- groupKFold(group=df$whaleID, k=10)

#setup 5-fold cross-validation
cv_scheme <- trainControl(method = "cv", number = length(folds), verboseIter = FALSE,
                          summaryFunction = twoClassSummary, classProbs = TRUE, index=folds)


#############################
#check for NA - less than 10% of training data okay for imputing

##NZ data has various variables with NAs 
##dshelf: sum(is.na(df$dshelf)) / nrow(df) *100 == 2627 rows == 2.02%
##sst/mld/sal: all 2 NA rows 
##ssh: 206 NAs
##chl: sum(is.na(df$chl)) / nrow(df) *100 == 6212rows == 4.8%
##wind E/N/wind sum(is.na(df$wind)) / nrow(df) *100 == 6751 rows == 5.18%
### total is 22.5% of full df -- sum(is.na(df)) / nrow(df) *100, but each individual var is <10% NAs

df <- df %>%  select(-X,-date, -uo, -vo, -wind_east, -wind_north, -x, -y)

#if(sum(is.na(df)) < 0.101*nrow(df) & sum(is.na(df)) > 0){
df_mice <- miceRanger(df, m=1)
df <- completeData(df_mice)[[1]]
#}
##runs RF to figure dist of the variable, takes into account other data
##randomly distributes it


#############################
##add a season column

df <- df %>% 
  mutate(
    season1 = case_when(
      month %in% c("12","01","02") ~ "summer",
      month %in% c("03","04","05") ~ "autumn",
      month %in% c("06","07","08") ~ "winter",
      month %in% c("09","10","11") ~ "spring"
    )
  ) %>% 
  select(-month) %>% 
  mutate(
    season2 = case_when(
      season1 %in% c("spring","summer") ~ "spring-summer",
      season1 %in% c("autumn","winter") ~ "autumn-winter")
  )


#############################
#perform tuning search
##explanatory variables
X <- df %>% select(-pa,-trackID,-season2,-whaleID, -curr) ##drop currents as not available in CMIP
Y <- as.factor(df$pa)
tic()
m1 <- train(x = X, y = Y, method = "ranger", metric = "ROC", trControl = cv_scheme, 
            tuneGrid = param_grid, num.trees = 1000, importance = "impurity")
toc()
##foraging RF, background absences ~50min without currents

varImp(m1)
m1$results
m1$bestTune


#############################
#save model

# file.out <- here("output", "RF_ARG_data_v1_no_currents_foraging_20241203.rds")
# saveRDS(m1, file = file.out)

######################################################################################

