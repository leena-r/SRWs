### SRW future projections

# script 7:
# This script calculates area of core habitat (90th percentile habitat suitability predictions)
# both circumpolarly and within each populations spatial extent
# as well as the overlap of core habitat with different protected areas


######################################################################################
## load packages
######################################################################################

library(tidyverse)
library(terra)
library(tictoc)
library(dplyr)
library(sf)
library(here)

##############################################################################


## calculate core habitat area (90th percentile habitat suitability value from circumpolar RF model)
## across entire SH, not broken down by population extents


# Define the list of months and scenarios
#months <- c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")
months <- (1:12)
long_months <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
scenarios <- c("ssp126", "ssp585")

# Define an empty data frame to store the results
area_results <- data.frame(
  Month = character(),
  Scenario = character(),
  Area_90th = numeric(),
  stringsAsFactors = FALSE
)

# Loop through each month of global habitat suitability prediction rasters  -- this will take some time to run
tic() # takes about 25s to run
for (i in seq_along(months)) {
  this.month <- months[i]
  this.long_month <- long_months[i]  # Get the corresponding long month name
  
  # Load the raster data for present day habitat suitability predictions 
  preds <- rast(here("output", "global predictions", "model without currents", "1deg_resolution", paste0("all_mean_", this.month, "_foraging_presence_1deg.nc")))
  
  # Define equal-area CRS and project the raster
  equal_area_crs <- "epsg:6932"
  proj_preds <- project(preds, equal_area_crs)
  
  # Calculate derired percentiles on the projected raster - only use 90th percentile here
  raster_values <- values(proj_preds, na.rm = TRUE)
  threshold_90th <- quantile(raster_values, probs = 0.90, na.rm = TRUE)
  
  # Calculate the cell area (in km2)
  cell_size <- res(proj_preds)[1] * res(proj_preds)[2] / 1e6
  
  # Create masks for present-day selected cells
  mask_90th <- proj_preds > threshold_90th
  plot(mask_90th)
  
  # Calculate the total number of cells that meet the criteria
  count_cells_90th <- sum(values(mask_90th), na.rm = TRUE)  
  
  # Calculate total area (in km2) for present day
  total_area_90th <- count_cells_90th * cell_size
  
  # Store results for present day
  area_results <- rbind(
    area_results,
    data.frame(
      Month = this.month,
      Scenario = "present_day",
      Area_90th = total_area_90th,
      stringsAsFactors = FALSE
    )
  )
  
  # Loop through each scenario for future projections
  for (this.scenario in scenarios) {
    # Load the raster data for the future scenario
    future_preds <- rast(here("output", "future projections", this.scenario, paste0("monthly_avg_", this.long_month, "_", this.scenario, "_updated.nc")))
    
    # Define equal-area CRS and project the raster
    proj_future_preds <- project(future_preds, equal_area_crs)
    
    # Calculate the cell area (in km2)
    cell_size_future_preds <- res(proj_future_preds)[1] * res(proj_future_preds)[2] / 1e6  # Convert m2 to km2
    
    # Create masks for future selected cells
    mask_90th_future_preds <- proj_future_preds > threshold_90th
    plot(mask_90th_future_preds)
    
    # Calculate the total number of cells that meet the criteria
    count_cells_90th_future_preds <- sum(values(mask_90th_future_preds), na.rm = TRUE)
    
    # Calculate total area (in km2) for future scenarios
    total_area_90th_future_preds <- count_cells_90th_future_preds * cell_size_future_preds
    
    
    # Store results for future scenarios
    area_results <- rbind(
      area_results,
      data.frame(
        Month = this.month,
        Scenario = this.scenario,
        Area_90th = total_area_90th_future_preds,
        stringsAsFactors = FALSE
      )
    )
  }
}
toc() ##25s
# Output the results
print(area_results)

#write_csv(area_results, here("output","core_habitat_area_results_circumpolar_all_pops_1deg.csv"))




    #### Extended Data Figure 1 #### 

#plot area (km2) change in circumpolar core habitat (90th percentile habitat sutiability)

area_results_selected <- area_results %>%
  dplyr::select(Month, Scenario , Area_90th)


# Step 1: Calculate area changes for each month - convert to millions of km2
area_changes <- area_results_selected %>%
  pivot_wider(names_from = Scenario, values_from = Area_90th) %>%
  mutate(
    change_ssp126 = (ssp126 - present_day) / 1e6,  # convert to million km2 directly
    change_ssp585 = (ssp585 - present_day) / 1e6
  )

# Step 2: Reshape to long format for ggplot
area_changes_long <- area_changes %>%
  dplyr::select(Month, change_ssp126, change_ssp585) %>%
  pivot_longer(
    cols = starts_with("change_"),
    names_to = "Scenario",
    values_to = "Change_million_km2"
  ) %>%
  mutate(
    Scenario = recode(Scenario,
                      "change_ssp126" = "ssp126",
                      "change_ssp585" = "ssp585")
  )

# Step 3: Define the months to plot (Oct-Apr)
month_map <- c(
  "1" = "jan", "2" = "feb", "3" = "mar", "4" = "apr",
  "5" = "may", "6" = "jun", "7" = "jul", "8" = "aug",
  "9" = "sep", "10" = "oct", "11" = "nov", "12" = "dec"
)
month_order <- c("oct", "nov", "dec", "jan", "feb", "mar", "apr")


# Step 4: Filter, reorder, and prepare for plotting
df_filtered <- area_changes_long %>%
  mutate(Month_label = month_map[as.character(Month)]) %>%
  filter(Month_label %in% month_order) %>%
  mutate(Month_label = factor(Month_label, levels = month_order))

# Step 5: Plot
p1 <- ggplot(df_filtered, aes(x = Month_label, y = Change_million_km2, 
                              color = Scenario, group = Scenario)) +
  geom_line(linewidth = 1.3) +
  geom_point(size = 3) +
  scale_x_discrete(labels = str_to_title) +
  scale_color_manual(
    values = c("ssp126" = "#1b9e77", "ssp585" = "#d95f02"),
    labels = c("ssp126" = "SSP1-2.6", "ssp585" = "SSP5-8.5")
  ) +
  labs(
    x = "Month",
    y = "Change in core habitat area (million km2)",
    color = "Scenario"
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 15),
    legend.text = element_text(size = 13),
    axis.title.x = element_text(size = 16, color = "black"),
    axis.title.y = element_text(size = 14, color = "black"),
    axis.text.x = element_text(size = 12, color = "black"),
    axis.text.y = element_text(size = 12, color = "black"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank()
  ) +
  guides(color = guide_legend(override.aes = list(size = 5, linewidth = 2.3)))

p1


# ggsave(
#   here("writing", "Supplementary Materials", "Circumpolar_area_km2_change.png"),
#   plot = p1,
#   dpi = 300,
#   width = 10,
#   height = 6,
#   units = "in"
# )

##############################################################################
##############################################################################


#calculate absolute area (km2) change in core habitat (90th percentile habitat suitability) BY POPULATION
# also separated by high or mid latitudes


## creating extent shapefiles for each pop
##OZ
# Define the boundary coordinates
lat_min <- -80
lat_max <- -30
lon_min <- 45
lon_max <- 133
split_lat <- -55

##SA
# Define the boundary coordinates
lat_min <- -80
lat_max <- -30
lon_min <- -64
lon_max <- 56
split_lat <- -55

##ARG
# Define the boundary coordinates
lat_min <- -80
lat_max <- -30
lon_min <- -70
lon_max <- -18
split_lat <- -55

##NZ
# Define the boundary coordinates
lat_min <- -80
lat_max <- -30
lon_min <- 79
lon_max <- 190
split_lat <- -55


# Generate a sequence of points at 1-degree intervals
lons <- seq(lon_min, lon_max, by = 1)

# Create the polygon following the globe's contours
# Top boundary (lat_max)
top_boundary <- data.frame(lon = lons, lat = lat_max)

# Bottom boundary (lat_min)
bottom_boundary <- data.frame(lon = rev(lons), lat = lat_min)

# Combine the boundaries into a full polygon loop
polygon_coords <- rbind(
  top_boundary,
  bottom_boundary,
  top_boundary[1, ]  # Close the polygon
)

# Create the polygon object
polygon_sf <- st_sfc(
  st_polygon(list(as.matrix(polygon_coords))),
  crs = 4326
)
polygon_sf <- st_sf(geometry = polygon_sf, name = "Full Polygon")

# Create the split polygons at split_lat
# Top polygon: lat_max to split_lat
top_split_coords <- rbind(
  data.frame(lon = lons, lat = lat_max),
  data.frame(lon = rev(lons), lat = split_lat),
  data.frame(lon = lons[1], lat = lat_max)
)

# Bottom polygon: split_lat to lat_min
bottom_split_coords <- rbind(
  data.frame(lon = lons, lat = split_lat),
  data.frame(lon = rev(lons), lat = lat_min),
  data.frame(lon = lons[1], lat = split_lat)
)

# Create the split polygon objects
top_split_sf <- st_sfc(
  st_polygon(list(as.matrix(top_split_coords))),
  crs = 4326
)
bottom_split_sf <- st_sfc(
  st_polygon(list(as.matrix(bottom_split_coords))),
  crs = 4326
)

# Combine into an sf object
split_polygons_sf <- st_sf(
  geometry = c(top_split_sf, bottom_split_sf),
  name = c("Top Split", "Bottom Split")
)

# Write the shapefiles one at a time
##full popualtion extent
# st_write(
#   polygon_sf,
#   here("data", "for mapping", "extent shapefiles", "NZ_full_polygon.shp"),
#   delete_layer = TRUE
# )
## broken down by high an dmid lats
# st_write(
#   split_polygons_sf,
#   here("data", "for mapping", "extent shapefiles", "NZ_split_polygons.shp"),
#   delete_layer = TRUE
# )

# Optional: View the shapefiles
# plot(st_geometry(polygon_sf), col = "lightblue", main = "Full Polygon")
# plot(st_geometry(split_polygons_sf), col = c("lightgreen", "lightcoral"), main = "Split Polygons", add = TRUE)





#90th percentile suitable habitat area (km2) in each month and each populations extent
##need to project shapefiles, and specify that in split shapefile use the "name" feature
#[to separate population shapefile for mid and high latitude half of the extent]


# Define the list of months and scenarios
months <- c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")
long_months <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
months_number <- c(1,2,3,4,5,6,7,8,9,10,11,12)
scenarios <- c("ssp126", "ssp585")
regions <- c("ARG", "NZ", "OZ", "SA")

# Path to the shapefiles
shapefile_path <- here("data", "for mapping", "extent shapefiles")

# Define an empty data frame to store the results - here do only 90th percentile habitat suitability
area_results <- data.frame(
  Region = character(),
  Polygon = character(),
  Month = character(),
  Scenario = character(),
  Area_90th = numeric(),
  stringsAsFactors = FALSE
)

# Loop through each region
tic() #takes about 3min to run
for (region in regions) {
  # Load the full and split shapefiles for the region
  full_shapefile <- vect(file.path(shapefile_path, paste0(region, "_full_polygon.shp")))
  split_shapefile <- vect(file.path(shapefile_path, paste0(region, "_split_polygon.shp")))
  
  # Define equal-area CRS and project the shapefiles
  equal_area_crs <- "epsg:6932"
  proj_full_shapefile <- project(full_shapefile, equal_area_crs)
  proj_split_shapefile <- project(split_shapefile, equal_area_crs)
  
  # Ensure the "name" attribute is retained after projection
  proj_split_shapefile$name <- split_shapefile$name
  
  # Combine the shapefiles into a list for easier iteration
  shapefiles <- list(full = proj_full_shapefile, split = proj_split_shapefile)
  
  # Loop through the shapefiles
  for (polygon_type in names(shapefiles)) {
    current_shapefile <- shapefiles[[polygon_type]]
    
    # Handle split shapefile separately by its "name" attribute
    if (polygon_type == "split") {
      split_names <- unique(current_shapefile$name)  # Get unique names (e.g., "Top Split", "Bottom Split")
    } else {
      split_names <- c(NA)  # Only one polygon for full shapefile
    }
    
    # Loop through each split (or full)
    for (split_name in split_names) {
      if (!is.na(split_name)) {
        subset_shapefile <- current_shapefile[current_shapefile$name == split_name, ]
      } else {
        subset_shapefile <- current_shapefile  # Use full shapefile
      }
      
      # Loop through each month
      for (i in seq_along(months)) {
        this.month <- months[i]
        this.long_month <- long_months[i]
        this.month_number <- months_number[i]
        
        # Load the raster data for present day
        preds <- rast(here("output", "global predictions", "model without currents", "1deg_resolution",
                           paste0("all_mean_", this.month_number, "_foraging_presence_1deg.nc")))
        
        # Define equal-area CRS and project the raster
        proj_preds <- project(preds, equal_area_crs)
        
        # Calculate percentiles on the projected raster
        raster_values <- values(proj_preds, na.rm = TRUE)
        threshold_90th <- quantile(raster_values, probs = 0.90, na.rm = TRUE)
        
        # Create masks for selected cells
        mask_90th <- proj_preds > threshold_90th
        
        # Clip the masks to the subset shapefile extent
        mask_90th_clipped <- mask(mask_90th, subset_shapefile)
        
        # Calculate cell area (in km2)
        cell_size <- res(mask_90th_clipped)[1] * res(mask_90th_clipped)[2] / 1e6  # Convert m2 to km2
        
        # Calculate total area within the shapefile
        count_cells_90th <- sum(values(mask_90th_clipped), na.rm = TRUE)
        
        total_area_90th <- count_cells_90th * cell_size
        
        
        # Store results for present day
        area_results <- rbind(
          area_results,
          data.frame(
            Region = region,
            Polygon = ifelse(is.na(split_name), polygon_type, split_name),
            Month = this.month,
            Scenario = "present_day",
            Area_90th = total_area_90th,
            stringsAsFactors = FALSE
          )
        )
        
        # Loop through each future scenario
        for (this.scenario in scenarios) {
          # Load the future raster
          future_preds <- rast(here("output", "future projections", this.scenario,
                                    paste0("monthly_avg_", this.long_month, "_", this.scenario, "_updated.nc")))
          
          # Project to equal-area CRS
          proj_future_preds <- project(future_preds, equal_area_crs)
          
          # Create masks for future scenario
          mask_90th_future <- mask(proj_future_preds > threshold_90th, subset_shapefile)
          
          # Calculate total area for future scenarios
          count_cells_90th_future <- sum(values(mask_90th_future), na.rm = TRUE)
          
          cell_size_future_preds <- res(mask_90th_future)[1] * res(mask_90th_future)[2] / 1e6  # Convert m2 to km2
          
          total_area_90th_future <- count_cells_90th_future * cell_size_future_preds
          
          
          # Store results for the future scenario
          area_results <- rbind(
            area_results,
            data.frame(
              Region = region,
              Polygon = ifelse(is.na(split_name), polygon_type, split_name),
              Month = this.month,
              Scenario = this.scenario,
              Area_90th = total_area_90th_future,
              stringsAsFactors = FALSE
            )
          )
        }
      }
    }
  }
}
toc() #3min
# Output the results
print(area_results)


#write_csv(area_results, here("output", "core_habitat_area_results_1deg.csv"))




      #### FIGURE 3 #####  

area_results_v2 <- area_results %>%
  mutate(
    Polygon = recode(Polygon,
                     "Top Split" = "mid latitudes",
                     "Bottom Split" = "high latitudes")
  )

high_latitudes <- area_results_v2 %>%
  filter(Polygon == "high latitudes")
  

# Calculate area changes for each month by rown
high_latitudes_changes <- high_latitudes %>%
  # Reshape so each scenario becomes a column
  pivot_wider(
    names_from = Scenario,
    values_from = Area_90th
  ) %>%
  # Calculate changes
  mutate(
    ssp126 = ssp126 - present_day,
    ssp585 = ssp585 - present_day
  ) %>%
  # Convert back to long format
  pivot_longer(
    cols = c(ssp126, ssp585),
    names_to = "Scenario",
    values_to = "change_Area_90th"
  ) %>%
  dplyr::select(Region, Month, Scenario, change_Area_90th)



high_latitudes_changes$Month <- str_to_title(high_latitudes_changes$Month)

# Define the months of interest
months_of_interest <- c("Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr")

# Convert Month to a factor to ensure correct order
high_latitudes_changes$Month <- factor(high_latitudes_changes$Month, levels = months_of_interest)


# Ensure Region is a factor with the specified order
high_latitudes_changes$Region <- factor(high_latitudes_changes$Region, levels = c("ARG", "SA", "OZ", "NZ"))

# Define region-specific colors
region_colors <- c(
  "ARG" = "#1d5355",#"#C99B38",
  "SA"  = "#f48f4d", 
  "OZ"  = "#40b7d4",#"#EDDCA5",
  "NZ"  = "#b72c26"#"#96DCF8"
)


p <- ggplot(high_latitudes_changes %>% filter(!is.na(Month)), aes(x = Month, y = change_Area_90th, 
                              color = Region, group = interaction(Region, Scenario), 
                              linetype = Scenario)) +
  geom_line(size = 1) +
  geom_point(aes(fill = Region), shape = 21, size = 3, stroke = 1.2) +
  geom_hline(yintercept = 0, color = "black", linetype = "dotted", linewidth = 0.8) +
  scale_color_manual(values = region_colors) +
  scale_fill_manual(values = region_colors) +
  scale_linetype_manual(values = c("ssp126" = "solid", "ssp585" = "dashed")) +
  scale_y_continuous(
    labels = function(x) x / 1000000,  # Convert to millions of km2
    breaks = scales::pretty_breaks(n = 6)  # Adjust number of ticks
  ) +
  labs(title = "High latitudes (55-80S)",
       x = "Month", 
       y = expression("Change in core habitat (mill. km"^2*")")) +  # y-axis label
  theme_bw() +
  theme(
    legend.position = "none",  # <--- Hides the legend
    #legend.title = element_blank(),
    axis.title.x = element_text(size = 16, color = "black"),
    axis.title.y = element_text(size = 14, color = "black"),
    axis.text.x = element_text(size = 12, color = "black"),
    axis.text.y = element_text(size = 12, color = "black"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank()
  )

print(p)
# ggsave(
#   filename = here("writing", "Figures", "Figure3", "Fig3_high_lats__new colours.png"),
#   plot = p,
#   dpi = 300,
#   width = 10,
#   height = 6,
#   units = "in"
# )




##mid lats
mid_latitudes <- area_results_v2 %>%
  filter(Polygon == "mid latitudes")


# Calculate area changes for each month by rown
mid_latitudes_changes <- mid_latitudes %>%
  # Reshape so each scenario becomes a column
  pivot_wider(
    names_from = Scenario,
    values_from = Area_90th
  ) %>%
  # Calculate changes
  mutate(
    ssp126 = ssp126 - present_day,
    ssp585 = ssp585 - present_day
  ) %>%
  # Convert back to long format
  pivot_longer(
    cols = c(ssp126, ssp585),
    names_to = "Scenario",
    values_to = "change_Area_90th"
  ) %>%
  dplyr::select(Region, Month, Scenario, change_Area_90th)



mid_latitudes_changes$Month <- str_to_title(mid_latitudes_changes$Month)

# Define the months of interest
months_of_interest <- c("Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr")

# Convert Month to a factor to ensure correct order
mid_latitudes_changes$Month <- factor(mid_latitudes_changes$Month, levels = months_of_interest)


# Ensure Region is a factor with the specified order
mid_latitudes_changes$Region <- factor(mid_latitudes_changes$Region, levels = c("ARG", "SA", "OZ", "NZ"))

# Define region-specific colors
region_colors <- c(
  "ARG" = "#1d5355",#"#C99B38",
  "SA"  = "#f48f4d", 
  "OZ"  = "#40b7d4",#"#EDDCA5",
  "NZ"  = "#b72c26"#"#96DCF8"
)


p <- ggplot(mid_latitudes_changes %>% filter(!is.na(Month)), aes(x = Month, y = change_Area_90th, 
                                                                  color = Region, group = interaction(Region, Scenario), 
                                                                  linetype = Scenario)) +
  geom_line(size = 1) +
  geom_point(aes(fill = Region), shape = 21, size = 3, stroke = 1.2) +
  geom_hline(yintercept = 0, color = "black", linetype = "dotted", linewidth = 0.8) +
  scale_color_manual(values = region_colors) +
  scale_fill_manual(values = region_colors) +
  scale_linetype_manual(values = c("ssp126" = "solid", "ssp585" = "dashed")) +
  scale_y_continuous(
    labels = function(x) x / 1000000,  # Convert to millions of km2
    breaks = scales::pretty_breaks(n = 6)  # Adjust number of ticks
  ) +
  labs(title = "Mid latitudes (30-55S)",
       x = "Month", 
       y = expression("Change in core habitat (mill. km"^2*")")) +  # y-axis label
  theme_bw() +
  theme(
    legend.position = "none",  # <--- Hides the legend
    #legend.title = element_blank(),
    axis.title.x = element_text(size = 16, color = "black"),
    axis.title.y = element_text(size = 14, color = "black"),
    axis.text.x = element_text(size = 12, color = "black"),
    axis.text.y = element_text(size = 12, color = "black"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank()
  )

print(p)
# ggsave(
#   filename = here("writing", "Figures", "Figure3", "Fig3_mid_lats__new colours.png"),
#   plot = p,
#   dpi = 300,
#   width = 10,
#   height = 6,
#   units = "in"
# )

##############################################################################
##############################################################################


  ##### MPA shapefiles ##### 

### do commented out sections just ones. otherwise, just read in the final product (not commented out)

    ##### Load 3 MPA shapefiles sourced from WDPA
# mpa1 <- vect(here("MPA data", "WDPA_Dec2024_Public_shp", "WDPA_Dec2024_Public_shp-polygons.shp"))
# 
# # Clip to south of 30S
# # First, define a rectangle that covers the southern hemisphere south of 30S
# clip_extent <- ext(-180, 180, -90, -30)
# clip_polygon <- as.polygons(clip_extent, crs = crs(mpa1))
# mpa1_clipped  <- crop(mpa1, clip_polygon)
# 
# 
# 
# mpa2 <- vect(here("MPA data", "WDPA_Dec2024_Public_shp", "WDPA_Dec2024_Public_shp-polygons_1.shp"))
# # Clip to south of 30S
# mpa2_clipped  <- crop(mpa2, clip_polygon)
# 
# 
# 
# mpa3 <- vect(here("MPA data", "WDPA_Dec2024_Public_shp", "WDPA_Dec2024_Public_shp-polygons_2.shp"))
# # Clip to south of 30S
# mpa3_clipped  <- crop(mpa3, clip_polygon)
# 
# 
# 
# # Combine into one SpatVector
# mpas_combined_clipped_30 <- rbind(mpa1_clipped, mpa2_clipped, mpa3_clipped)
# 
# # Save the clipped version to avoid rerunning
# mpa_clipped_file <- here("MPA data", "MPAs_south_30S.shp")
# terra::writeVector(mpas_combined_clipped_30, mpa_clipped_file, overwrite = TRUE)

## also tidied the shp file in QGIS a bit. few terresrial areas that are not relevant

## also ended up saving as designated_MPAs_south_30S.shp to only include those labelled as "designated"


designated_mpas_clipped_30 <- vect(here("MPA data", "designated_MPAs_south_30S.shp"))
## already projected to epsg:6932


# # Project to equal-area CRS (same as habitat & population shapefiles)
# proj_mpas <- project(mpas_combined_clipped_30, "epsg:6932")
# #plot(proj_mpas)



    ##### read in additional MPA data from mpatlas_export_geo_202503

## read in implemented MPAs
mpatlas_implemented <- vect(here("MPA data", "mpatlas_implemented_MPAs.shp"))


all_designated_implemented_mpas <- rbind(designated_mpas_clipped_30, mpatlas_implemented)


    ##### read in and join proposed MPAs and iucn-immas

proposed_mpas <- vect(here("MPA data", "mpatlas_proposed_MPAs.shp"))

iucn_immas <- vect(here("MPA data", "iucn_imma_south_30S.shp"))
iucn_immas <- project(iucn_immas, "epsg:6932")
#plot(iucn_immas)

proposed_mpas_and_immas <- rbind(iucn_immas, proposed_mpas)


#######################################################################################################


    #### TOP PORTION OF TABLE 1 ####

# how much MPA coverage is there in each population's extent?

# Load designated MPAs
designated_mpas_clipped_30 <- vect(here("MPA data", "designated_MPAs_south_30S.shp"))
mpatlas_implemented <- vect(here("MPA data", "mpatlas_implemented_MPAs.shp"))
all_designated_implemented_mpas <- rbind(designated_mpas_clipped_30, mpatlas_implemented)

# Fix geometry
all_designated_implemented_mpas <- makeValid(all_designated_implemented_mpas)


# Load proposed MPAs
proposed_mpas <- vect(here("MPA data", "mpatlas_proposed_MPAs.shp"))
proposed_mpas <- makeValid(proposed_mpas)

iucn_immas <- vect(here("MPA data", "iucn_imma_south_30S.shp"))
iucn_immas <- project(iucn_immas, "epsg:6932")
iucn_immas <- makeValid(iucn_immas)

proposed_mpas_and_immas <- rbind(iucn_immas, proposed_mpas)
# Fix geometry
proposed_mpas_and_immas <- makeValid(proposed_mpas_and_immas)


# join designated + proposed
all_mpas_combined <- rbind(all_designated_implemented_mpas, proposed_mpas_and_immas)
all_mpas_combined <- makeValid(all_mpas_combined)


# Load required population extent shapefiles
shapefile_path <- here("data", "for mapping", "extent shapefiles")
equal_area_crs <- "epsg:6932"
regions <- c("ARG", "NZ", "OZ", "SA")

# Prepare output dataframe
mpa_area_results <- data.frame(
  Region = character(),
  Polygon = character(),
  Area_Designated_MPA = numeric(),
  Area_Proposed_MPA = numeric(),
  Area_IMMAs = numeric(),
  Area_Any_MPA = numeric(),
  stringsAsFactors = FALSE
)

# Loop through regions
tic() #takes about 2min to run
for (region in regions) {
  full_shapefile <- vect(paste0(shapefile_path, region, "_full_polygon.shp"))
  split_shapefile <- vect(paste0(shapefile_path, region, "_split_polygons.shp"))
  
  proj_full_shapefile <- project(full_shapefile, equal_area_crs)
  proj_split_shapefile <- project(split_shapefile, equal_area_crs)
  proj_split_shapefile$name <- split_shapefile$name
  
  shapefiles <- list(full = proj_full_shapefile, split = proj_split_shapefile)
  
  for (polygon_type in names(shapefiles)) {
    current_shapefile <- shapefiles[[polygon_type]]
    split_names <- if (polygon_type == "split") unique(current_shapefile$name) else c(NA)
    
    for (split_name in split_names) {
      subset_shapefile <- if (!is.na(split_name)) current_shapefile[current_shapefile$name == split_name, ] else current_shapefile
      
      # Intersections
      designated_in_region <- intersect(subset_shapefile, all_designated_implemented_mpas)
      proposed_in_region <- intersect(subset_shapefile, proposed_mpas)
      IMMAs_in_region <- intersect(subset_shapefile, iucn_immas)
      any_mpa_in_region <- intersect(subset_shapefile, all_mpas_combined)
      
      # Calculate areas (convert from m2 to km2)
      area_designated <- if (!is.null(designated_in_region)) sum(expanse(designated_in_region, unit = "km")) else 0
      area_proposed <- if (!is.null(proposed_in_region)) sum(expanse(proposed_in_region, unit = "km")) else 0
      area_IMMAs <- if (!is.null(IMMAs_in_region)) sum(expanse(IMMAs_in_region, unit = "km")) else 0
      area_any <- if (!is.null(any_mpa_in_region)) sum(expanse(any_mpa_in_region, unit = "km")) else 0
      
      mpa_area_results <- rbind(mpa_area_results, data.frame(
        Region = region,
        Polygon = ifelse(is.na(split_name), polygon_type, split_name),
        Area_Designated_MPA = area_designated,
        Area_Proposed_MPA = area_proposed,
        Area_IMMAs = area_IMMAs,
        Area_Any_MPA = area_any,
        stringsAsFactors = FALSE
      ))
    }
  }
}
toc() #2min
mpa_area_results
## Note: top split == mid latitudes, bottom split == high latitudes

# write.csv(
#   mpa_area_results, 
#   here("output", "how_much_mpa_coverage_in_population_extents_20250724.csv"), 
#   row.names = FALSE
# )

## this provides the absolute km2 area coverage in each population's extent that is covered with a different type of MPA used in Table 1
## to calculate the % of population extent that is covered, we also need km2 value of each extent area
## originally calculated that in R, but that did not adjust for land
## so km2 area of population extent was calculated in QGIS instead
# Region   Polygon        km2 area of population extent__adjusted for land in QGIS
# ARG       full            15,444,323 
# ARG       Top Split       9,791,549
# ARG       Bottom Split    5,652,774 
# NZ        full            32,293,625
# NZ        Top Split       23,149,428 
# NZ        Bottom Split    9,144,197 
# OZ        full            25,787,701
# OZ        Top Split       19,397,579
# OZ        Bottom Split    6,390,122 
# SA        full            37,023,296 
# SA        Top Split       25,617,442
# SA        Bottom Split    11,405,854 


#######################################################################################################

## calculate core habitat area (km2) in each population's extent, 
# and then core habitat area that's either within designated or proposed MPAs

# Load designated MPAs
designated_mpas_clipped_30 <- vect(here("MPA data", "designated_MPAs_south_30S.shp"))
mpatlas_implemented <- vect(here("MPA data", "mpatlas_implemented_MPAs.shp"))
all_designated_implemented_mpas <- rbind(designated_mpas_clipped_30, mpatlas_implemented)

# Fix geometry
all_designated_implemented_mpas <- makeValid(all_designated_implemented_mpas)


# Load proposed MPAs
proposed_mpas <- vect(here("MPA data", "mpatlas_proposed_MPAs.shp"))
proposed_mpas <- makeValid(proposed_mpas)

iucn_immas <- vect(here("MPA data", "iucn_imma_south_30S.shp"))
iucn_immas <- project(iucn_immas, "epsg:6932")
iucn_immas <- makeValid(iucn_immas)

proposed_mpas_and_immas <- rbind(iucn_immas, proposed_mpas)
# Fix geometry
proposed_mpas_and_immas <- makeValid(proposed_mpas_and_immas)



# join designated + proposed
all_mpas_combined <- rbind(all_designated_implemented_mpas, proposed_mpas_and_immas)
all_mpas_combined <- makeValid(all_mpas_combined)


# Parameters
months <- c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")
long_months <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
months_number <- 1:12
scenarios <- c("ssp126", "ssp585")
regions <- c("ARG", "NZ", "OZ", "SA")

shapefile_path <- here("data", "for mapping", "extent shapefiles")
equal_area_crs <- "epsg:6932"

# Empty results dataframe
area_results <- data.frame(
  Region = character(),
  Polygon = character(),
  Month = character(),
  Scenario = character(),
  Area_90th = numeric(),
  Area_90th_Designated_MPA = numeric(),
  Area_90th_Proposed_MPA = numeric(),
  Area_90th_IMMAs = numeric(),
  Area_90th_Proposed_MPA_or_IMMA = numeric(),
  Area_90th_any_MPA_IMMA = numeric(),
  stringsAsFactors = FALSE
)

# Main loop
tic() #takes about 70 min to run
for (region in regions) {
  full_shapefile <- vect(paste0(shapefile_path, region, "_full_polygon.shp"))
  split_shapefile <- vect(paste0(shapefile_path, region, "_split_polygons.shp"))
  
  proj_full_shapefile <- project(full_shapefile, equal_area_crs)
  proj_split_shapefile <- project(split_shapefile, equal_area_crs)
  proj_split_shapefile$name <- split_shapefile$name
  
  shapefiles <- list(full = proj_full_shapefile, split = proj_split_shapefile)
  
  for (polygon_type in names(shapefiles)) {
    current_shapefile <- shapefiles[[polygon_type]]
    split_names <- if (polygon_type == "split") unique(current_shapefile$name) else c(NA)
    
    for (split_name in split_names) {
      subset_shapefile <- if (!is.na(split_name)) current_shapefile[current_shapefile$name == split_name, ] else current_shapefile
      
      for (i in seq_along(months)) {
        this.month <- months[i]
        this.long_month <- long_months[i]
        this.month_number <- months_number[i]
        
        preds <- rast(here("output", "global predictions", "model without currents", "1deg_resolution",
            paste0("all_mean_", this.month_number, "_foraging_presence_1deg.nc")
          )
        )
        proj_preds <- project(preds, equal_area_crs)
        
        threshold_90th <- quantile(values(proj_preds, na.rm = TRUE), probs = 0.90, na.rm = TRUE)
        mask_90th <- proj_preds > threshold_90th
        mask_90th_clipped <- mask(mask_90th, subset_shapefile)
        cell_size <- res(mask_90th_clipped)[1] * res(mask_90th_clipped)[2] / 1e6
        count_cells_90th <- sum(values(mask_90th_clipped), na.rm = TRUE)
        total_area_90th <- count_cells_90th * cell_size
        
        designated_in_region <- intersect(subset_shapefile, all_designated_implemented_mpas)
        proposed_in_region <- intersect(subset_shapefile, proposed_mpas)
        IMMA_in_region <- intersect(subset_shapefile, iucn_immas)
        proposed_or_IMMA_in_region <- intersect(subset_shapefile, proposed_mpas_and_immas)
        any_mpa_in_region <- intersect(subset_shapefile, all_mpas_combined)
        
        # Overlap: Designated
        mask_designated <- if (!is.null(designated_in_region)) mask(mask_90th, designated_in_region) else NA
        area_90th_designated <- if (!is.null(mask_designated)) sum(values(mask_designated), na.rm = TRUE) * cell_size else 0
        
        # Overlap: Proposed
        mask_proposed <- if (!is.null(proposed_in_region)) mask(mask_90th, proposed_in_region) else NA
        area_90th_proposed <- if (!is.null(mask_proposed)) sum(values(mask_proposed), na.rm = TRUE) * cell_size else 0
        
        # Overlap: IMMA
        mask_IMMA <- if (!is.null(IMMA_in_region)) mask(mask_90th, IMMA_in_region) else NA
        area_90th_IMMA <- if (!is.null(mask_IMMA)) sum(values(mask_IMMA), na.rm = TRUE) * cell_size else 0
        
        # Overlap: Proposed or IMMA
        mask_proposed_or_IMMA <- if (!is.null(proposed_or_IMMA_in_region)) mask(mask_90th, proposed_or_IMMA_in_region) else NA
        area_90th_proposed_or_IMMA <- if (!is.null(mask_proposed_or_IMMA)) sum(values(mask_proposed_or_IMMA), na.rm = TRUE) * cell_size else 0
        
        # Overlap: Any
        mask_any <- if (!is.null(any_mpa_in_region)) mask(mask_90th, any_mpa_in_region) else NA
        area_90th_any_MPA_IMMA <- if (!is.null(mask_any)) sum(values(mask_any), na.rm = TRUE) * cell_size else 0
        
        area_results <- rbind(area_results, data.frame(
          Region = region,
          Polygon = ifelse(is.na(split_name), polygon_type, split_name),
          Month = this.month,
          Scenario = "present_day",
          Area_90th = total_area_90th,
          Area_90th_Designated_MPA = area_90th_designated,
          Area_90th_Proposed_MPA = area_90th_proposed,
          Area_90th_IMMAs = area_90th_IMMA,
          Area_90th_Proposed_MPA_or_IMMA = area_90th_proposed_or_IMMA,
          Area_90th_any_MPA_IMMA = area_90th_any_MPA_IMMA,
          stringsAsFactors = FALSE
        ))
        
        # Future scenarios
        for (this.scenario in scenarios) {
          future_preds <- rast(here("output", "future projections", this.scenario,
                                    paste0("monthly_avg_", this.long_month, "_", this.scenario, "_updated.nc")))
          proj_future_preds <- project(future_preds, equal_area_crs)
          
          mask_90th_future <- mask(proj_future_preds > threshold_90th, subset_shapefile)
          cell_size_future <- res(mask_90th_future)[1] * res(mask_90th_future)[2] / 1e6
          area_90th_future <- sum(values(mask_90th_future), na.rm = TRUE) * cell_size_future
          
          mask_future_designated <- if (!is.null(designated_in_region)) mask(mask_90th_future, designated_in_region) else NA
          area_90th_future_designated <- if (!is.null(mask_future_designated)) sum(values(mask_future_designated), na.rm = TRUE) * cell_size_future else 0
          
          mask_future_proposed <- if (!is.null(proposed_in_region)) mask(mask_90th_future, proposed_in_region) else NA
          area_90th_future_proposed <- if (!is.null(mask_future_proposed)) sum(values(mask_future_proposed), na.rm = TRUE) * cell_size_future else 0
          
          mask_future_IMMA <- if (!is.null(IMMA_in_region)) mask(mask_90th_future, IMMA_in_region) else NA
          area_90th_future_IMMA <- if (!is.null(mask_future_IMMA)) sum(values(mask_future_IMMA), na.rm = TRUE) * cell_size_future else 0
          
          mask_future_proposed_or_IMMA <- if (!is.null(proposed_or_IMMA_in_region)) mask(mask_90th_future, proposed_or_IMMA_in_region) else NA
          area_90th_future_proposed_or_IMMA <- if (!is.null(mask_future_proposed_or_IMMA)) sum(values(mask_future_proposed_or_IMMA), na.rm = TRUE) * cell_size_future else 0
          
          mask_future_any <- if (!is.null(any_mpa_in_region)) mask(mask_90th_future, any_mpa_in_region) else NA
          area_90th_future_any_MPA_IMMA <- if (!is.null(mask_future_any)) sum(values(mask_future_any), na.rm = TRUE) * cell_size_future else 0
          
          area_results <- rbind(area_results, data.frame(
            Region = region,
            Polygon = ifelse(is.na(split_name), polygon_type, split_name),
            Month = this.month,
            Scenario = this.scenario,
            Area_90th = area_90th_future,
            Area_90th_Designated_MPA = area_90th_future_designated,
            Area_90th_Proposed_MPA = area_90th_future_proposed,
            Area_90th_IMMAs = area_90th_future_IMMA,
            Area_90th_Proposed_MPA_or_IMMA = area_90th_future_proposed_or_IMMA,
            Area_90th_any_MPA_IMMA = area_90th_future_any_MPA_IMMA,
            stringsAsFactors = FALSE
          ))
        }
      }
    }
  }
}
toc() #70 minutes
# Save the results
write.csv(
  area_results,
  here(
    "output",
    "core_habitat_area_results_designated_vs_proposed_and_joined_MPAs_20250723.csv"
  ),
  row.names = FALSE
)

## read in the data
area_results <- read.csv(
  here(
    "output",
    "core_habitat_area_results_designated_vs_proposed_and_joined_MPAs_20250723.csv"
  ),
  row.names = NULL
)



      #### BOTTOM PORTION OF TABLE 1 ####

  ## established MPAs

# Define months to keep
months_to_keep <- c("oct", "nov", "dec", "jan", "feb", "mar", "apr")

# Calculate % protected and summarise
summary_stats_all <- area_results %>%
  filter(
    Polygon == "full",
    Scenario %in% c("present_day", "ssp126", "ssp585"),
    Month %in% months_to_keep
  ) %>%
  mutate(
    percent_protected = (Area_90th_Designated_MPA / Area_90th) * 100
  ) %>%
  group_by(Region, Scenario) %>%
  summarise(
    mean_area_million_km2 = mean(Area_90th_Designated_MPA, na.rm = TRUE) / 1e6,
    min_area_million_km2  = min(Area_90th_Designated_MPA, na.rm = TRUE) / 1e6,
    max_area_million_km2  = max(Area_90th_Designated_MPA, na.rm = TRUE) / 1e6,
    
    mean_percent_protected = mean(percent_protected, na.rm = TRUE),
    min_percent_protected  = min(percent_protected, na.rm = TRUE),
    max_percent_protected  = max(percent_protected, na.rm = TRUE),
    
    .groups = "drop"
  )
summary_stats_all



  ## proposed MPAs

# Define months to keep
months_to_keep <- c("oct", "nov", "dec", "jan", "feb", "mar", "apr")

# Calculate % protected and summarise
summary_stats_all <- area_results %>%
  filter(
    Polygon == "full",
    Scenario %in% c("present_day", "ssp126", "ssp585"),
    Month %in% months_to_keep
  ) %>%
  mutate(
    percent_protected = (Area_90th_Proposed_MPA / Area_90th) * 100
  ) %>%
  group_by(Region, Scenario) %>%
  summarise(
    mean_area_million_km2 = mean(Area_90th_Proposed_MPA, na.rm = TRUE) / 1e6,
    min_area_million_km2  = min(Area_90th_Proposed_MPA, na.rm = TRUE) / 1e6,
    max_area_million_km2  = max(Area_90th_Proposed_MPA, na.rm = TRUE) / 1e6,
    
    mean_percent_protected = mean(percent_protected, na.rm = TRUE),
    min_percent_protected  = min(percent_protected, na.rm = TRUE),
    max_percent_protected  = max(percent_protected, na.rm = TRUE),
    
    .groups = "drop"
  )
summary_stats_all



  ## IMMAs

# Define months to keep
months_to_keep <- c("oct", "nov", "dec", "jan", "feb", "mar", "apr")

# Calculate % protected and summarise
summary_stats_all <- area_results %>%
  filter(
    Polygon == "full",
    Scenario %in% c("present_day", "ssp126", "ssp585"),
    Month %in% months_to_keep
  ) %>%
  mutate(
    percent_protected = (Area_90th_IMMAs / Area_90th) * 100
  ) %>%
  group_by(Region, Scenario) %>%
  summarise(
    mean_area_million_km2 = mean(Area_90th_IMMAs, na.rm = TRUE) / 1e6,
    min_area_million_km2  = min(Area_90th_IMMAs, na.rm = TRUE) / 1e6,
    max_area_million_km2  = max(Area_90th_IMMAs, na.rm = TRUE) / 1e6,
    
    mean_percent_protected = mean(percent_protected, na.rm = TRUE),
    min_percent_protected  = min(percent_protected, na.rm = TRUE),
    max_percent_protected  = max(percent_protected, na.rm = TRUE),
    
    .groups = "drop"
  )
summary_stats_all




  ## Any MPA or IMMA

# Define months to keep
months_to_keep <- c("oct", "nov", "dec", "jan", "feb", "mar", "apr")

# Calculate % protected and summarise
summary_stats_all <- area_results %>%
  filter(
    Polygon == "full",
    Scenario %in% c("present_day", "ssp126", "ssp585"),
    Month %in% months_to_keep
  ) %>%
  mutate(
    percent_protected = (Area_90th_any_MPA_IMMA / Area_90th) * 100
  ) %>%
  group_by(Region, Scenario) %>%
  summarise(
    mean_area_million_km2 = mean(Area_90th_any_MPA_IMMA, na.rm = TRUE) / 1e6,
    min_area_million_km2  = min(Area_90th_any_MPA_IMMA, na.rm = TRUE) / 1e6,
    max_area_million_km2  = max(Area_90th_any_MPA_IMMA, na.rm = TRUE) / 1e6,
    
    mean_percent_protected = mean(percent_protected, na.rm = TRUE),
    min_percent_protected  = min(percent_protected, na.rm = TRUE),
    max_percent_protected  = max(percent_protected, na.rm = TRUE),
    
    .groups = "drop"
  )
summary_stats_all




