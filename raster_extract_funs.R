
# GoM_Regions_extract
GoM_Regions_extract <- function(df){
  numCores <- parallel::detectCores()
  doParallel::registerDoParallel(numCores)
  GoM_region_temps <-  foreach (i=1:length(GoM_regions$Region), .combine = rbind, .errorhandling = "remove") %dopar% {
    name <- GoM_regions@data[["Region"]][i]
    values <- raster::extract(df, GoM_regions[i,], fun = mean, na.rm = TRUE, df = TRUE) %>% 
      pivot_longer(., cols = -ID, names_to = "Date", values_to = "Temp")
    values$name <- name
    values <- values %>% mutate(Date = as.Date(str_sub(Date, start = 2, end = 11), format = "%Y.%m.%d")) %>% dplyr::select(-ID)
    values
  }
  doParallel::stopImplicitCluster()
  
  GoM_temp <- GoM_region_temps %>% left_join(., GoM_regions_area, by = c("name" = "Region")) %>% 
    filter(name %in% c("Central Gulf of Maine","Northern Coastal Shelf","Southern Coastal Shelf",
                       "Wikinson Basin","Jordan Basin","Georges Basin","Browns Bank",
                       "Eastern Coastal Shelf")) %>% group_by(Date) %>%
    summarise(Temp = weighted.mean(Temp, w = Area_km, na.rm = TRUE)) %>% mutate(name = paste("Gulf of Maine"))
  
  GoM_temp <- bind_rows(GoM_region_temps, GoM_temp)
  return(GoM_temp)
}

# Lobster Zones extract
lob_zones_extract <- function(df){
  numCores <- parallel::detectCores()
  doParallel::registerDoParallel(numCores)
  lob_zone_temps <-  foreach (i=1:length(lob_zones$ZONEID), .combine = rbind, .errorhandling = "remove") %dopar% {
    name <- lob_zones@data[["ZONEID"]][i]
    values <- raster::extract(df, lob_zones[i,], fun = mean, na.rm = TRUE, df = TRUE) %>% 
      pivot_longer(., cols = -ID, names_to = "Date", values_to = "Temp")
    values$name <- name
    values <- values %>% mutate(Date = as.Date(str_sub(Date, start = 2, end = 11), format = "%Y.%m.%d"), name = paste("Lobster Zone", name)) %>% dplyr::select(-ID)
    values
  }
  doParallel::stopImplicitCluster()
  return(lob_zone_temps)
}

# Lob_cbind_extract
lob_cbind_extract <- function(df){
  lob_zone_cbind_temps <- raster::extract(df, lob_cbind, fun = mean, na.rm = TRUE, df = TRUE) %>% 
    pivot_longer(., cols = -ID, names_to = "Date", values_to = "Temp") %>% 
    mutate(Date = as.Date(str_sub(Date, start = 2, end = 11), format = "%Y.%m.%d")) %>% dplyr::select(-ID)
  lob_zone_cbind_temps$name <- "Combined Lobster Zones"
  return(lob_zone_cbind_temps)
}



