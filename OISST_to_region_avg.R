library(tidyverse)
library(lubridate)
library(foreach)

OISST_data <- raster::brick("Data/OISST_data.grd")

# GoM Regions
GoM_regions <- raster::shapefile("Data/Shapefiles/GoM regions/PhysioRegions_WGS84.shp")

numCores <- parallel::detectCores()
doParallel::registerDoParallel(numCores)
GoM_region_temps <-  foreach (i=1:length(GoM_regions$Region), .combine = rbind, .errorhandling = "remove") %dopar% {
  name <- GoM_regions@data[["Region"]][i]
  values <- raster::extract(OISST_data, GoM_regions[i,], fun = mean, na.rm = TRUE, df = TRUE) %>% 
    pivot_longer(., cols = -ID, names_to = "Date", values_to = "Temp")
  values$name <- name
  values <- values %>% mutate(Date = as.Date(str_sub(Date, start = 2, end = 11), format = "%Y.%m.%d")) %>% dplyr::select(-ID)
  values
}
doParallel::stopImplicitCluster()

# Calculate a GoM avg temp

GoM_regions_area <- foreign::read.dbf("Data/Shapefiles/GoM regions/PhysioRegions_WGS84.dbf") %>% 
  ungroup %>% select(Region, Area_km)

GoM_temp <- GoM_region_temps %>% left_join(., GoM_regions_area, by = c("name" = "Region")) %>% 
  filter(name %in% c("Central Gulf of Maine","Northern Coastal Shelf","Southern Coastal Shelf",
                     "Wikinson Basin","Jordan Basin","Bay of Fundy","Georges Basin","Browns Bank",
                     "Eastern Coastal Shelf")) %>% group_by(Date) %>%
  summarise(Temp = weighted.mean(Temp, w = Area_km, na.rm = TRUE)) %>% mutate(name = paste("Gulf of Maine"))
  



# Lobster Zones
lob_zones <- raster::shapefile("Data/Shapefiles/Lobster_zones/DMR_Lobster_Zones.shp")

numCores <- parallel::detectCores()
doParallel::registerDoParallel(numCores)
lob_zone_temps <-  foreach (i=1:length(lob_zones$ZONEID), .combine = rbind, .errorhandling = "remove") %dopar% {
  name <- lob_zones@data[["ZONEID"]][i]
  values <- raster::extract(OISST_data, lob_zones[i,], fun = mean, na.rm = TRUE, df = TRUE) %>% 
    pivot_longer(., cols = -ID, names_to = "Date", values_to = "Temp")
  values$name <- name
  values <- values %>% mutate(Date = as.Date(str_sub(Date, start = 2, end = 11), format = "%Y.%m.%d"), name = paste("Lobster Zone", name)) %>% dplyr::select(-ID)
  values
}
doParallel::stopImplicitCluster()


# Lobster Zones combined
lob_cbind <- raster::shapefile("Data/Shapefiles/Lobster_zones/Lob_zones_combined.shp")

lob_zone_cbind_temps <- raster::extract(OISST_data, lob_cbind, fun = mean, na.rm = TRUE, df = TRUE) %>% 
    pivot_longer(., cols = -ID, names_to = "Date", values_to = "Temp") %>% 
  mutate(Date = as.Date(str_sub(Date, start = 2, end = 11), format = "%Y.%m.%d")) %>% dplyr::select(-ID)
lob_zone_cbind_temps$name <- "Combined Lobster Zones"


# Combine into one df

Gulf_of_Maine <- bind_rows(GoM_region_temps, GoM_temp, lob_zone_temps, lob_zone_cbind_temps)

# Write to Data file

write_csv(Gulf_of_Maine, "Data/GoM_regions_OISST.csv")
