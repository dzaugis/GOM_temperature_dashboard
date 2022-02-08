library(tidyverse)
library(lubridate)
library(foreach)

# Raster Data
FVCOM_raster <- raster::brick("Data/FVCOM_raster.grd")
OISST_raster <- raster::brick("Data/OISST_data.grd")

# Shapefiles
GoM_regions <- raster::shapefile("Data/Shapefiles/GoM regions/PhysioRegions_WGS84.shp")
GoM_regions_area <- foreign::read.dbf("Data/Shapefiles/GoM regions/PhysioRegions_WGS84.dbf") %>% 
  ungroup %>% select(Region, Area_km)
lob_zones <- raster::shapefile("Data/Shapefiles/Lobster_zones/DMR_Lobster_Zones.shp")
lob_cbind <- raster::shapefile("Data/Shapefiles/Lobster_zones/Lob_zones_combined.shp")

# Required Functions
source("raster_extract_funs.R")

# List Rasters
ls <- list("OISST" = OISST_raster, "FVCOM" = FVCOM_raster)

# Update_Indicators Function
Update_Indicators <- function(ls){
  numCores <- parallel::detectCores()
  doParallel::registerDoParallel(numCores)
  region_temp <-  foreach (i=1:length(ls), .combine = rbind, .errorhandling = "remove") %dopar% {
    name <- names(ls)[[i]]
    GoM <- GoM_Regions_extract(df = ls[[i]])
    lob_c <- lob_cbind_extract(df = ls[[i]])
    lob_z <- lob_zones_extract(df = ls[[i]])
    df <- bind_rows(GoM, lob_c, lob_z)
    df$Source <- name
    df
  }
  doParallel::stopImplicitCluster()
  
  GoM_regions_temp <- region_temp %>% mutate(dayz = day(Date), mon = month(Date)) %>%
    group_by(dayz, mon, name, Source) %>% mutate(clim = mean(Temp, na.rm = TRUE)) %>% ungroup() %>%
    mutate(Anomaly = Temp - clim) %>% ungroup %>%
    dplyr::select(-dayz, -mon, -clim) %>% 
    pivot_longer(., cols = c(Temp, Anomaly), names_to = "Type", values_to = "Temperature")
  
  return(GoM_regions_temp)
}

# Run function. Takes 15 minutes
GoM_regions_temp <- Update_Indicators(ls)

# Write csv
data.table::fwrite(GoM_regions_temp, "Data/GoM_regions_temp.csv")
