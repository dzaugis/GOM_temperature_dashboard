
### Days above n degrees 

deg_days <- function(df, num, region, Sources){
  
  df %>% filter(Type == "Temp", 
                              Temperature >= num, 
                              name == region, 
                              Source == Sources) %>%
    mutate(mon = month(Date), yr = year(Date)) %>%
    group_by(name, Source, yr) %>% tally() %>% 
    ggplot() + geom_col(aes(yr, n)) + theme_bw() +
    labs(y = "Days", x = "Year", 
         title = paste("Days above", num, "degrees C")) + 
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank())
  
}

#deg_days(15, "Gulf of Maine", "OISST")


### first date of x deg water

date_above <- function(df, num, region, Sources){
  
  yrs <- df %>% filter(Source == Sources) %>% 
    mutate(yr = year(Date)) %>% distinct(yr) %>%
    dplyr::select(yr)
  
  df <- df %>% filter(Type == "Temp", 
                              Temperature > num, 
                              name == region, 
                              Source == Sources) %>%
    mutate(yr = year(Date), yrday = yday(Date), daymon = format(Date, "%d-%m")) %>%
    group_by(name, Source, yr) %>% arrange(Date) %>% slice(1) %>% right_join(., yrs, by = "yr")
  
  d_seq <- data.frame("yrday" = seq(min(sort(df$yrday)), max(sort(df$yrday)), 8))%>% 
    mutate(labs = format(as.Date(yrday, origin = "2000-01-01"), "%d-%m"))
  
  ggplot(df) + geom_line(aes(yr, yrday)) + theme_bw() +
    labs(y = "Day", x = "Year", 
         title = paste("First day above", num, "degrees C")) + 
    scale_y_continuous(breaks = d_seq$yrday,
                       labels = d_seq$labs) +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank())
  
}

#date_above(15, "Gulf of Maine", "OISST")

# End of Summer

end_of_sum <- function(df, num, region, Sources){
  
  yrs <- df %>% filter(Source == Sources) %>% 
    mutate(yr = year(Date)) %>% distinct(yr) %>%
    dplyr::select(yr)
  
  df <- df %>% filter(Type == "Temp", 
                                    Temperature > num, 
                                    name == region, 
                                    Source == Sources) %>%
    mutate(yr = year(Date), yrday = yday(Date), daymon = format(Date, "%d-%m")) %>%
    group_by(name, Source, yr) %>% arrange(desc(Date)) %>% slice(1) %>% right_join(., yrs, by = "yr")
  
  d_seq <- data.frame("yrday" = seq(min(sort(df$yrday)), max(sort(df$yrday)), 8))%>% 
    mutate(labs = format(as.Date(yrday, origin = "2000-01-01"), "%d-%m"))
  
  ggplot(df) + geom_line(aes(yr, yrday)) + theme_bw() +
    labs(y = "Day", x = "Year", 
         title = paste("Last day above", num, "degrees C")) + 
    scale_y_continuous(breaks = d_seq$yrday,
                       labels = d_seq$labs) +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank())
  
}

#end_of_sum(15, "Gulf of Maine", "OISST")

# temp indicator ts

temp_indicator_ts <- function(df, type, region, Sources){
  df %>% filter(name == region, Type == type, Source == Sources) %>% mutate(Year = year(Date)) %>%
    group_by(name, Year, Source)  %>%  
    dplyr::summarise(Temperature = mean(Temperature, na.rm = TRUE)) %>% ggplot() + 
    geom_line(aes(Year, Temperature)) + theme_bw() + 
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank())+
    labs(y = "Temperature Anomaly (deg C)", title = "Sea Surface Temperature")
}

# temp_indicator_ts(GoM_regions_temp, type = "Anomaly", region = "Gulf of Maine", Sources = "OISST")

Yrmon_clim <- function(df, type, region, Sources){
  df %>% filter(name == region, Type == type, Source == Sources) %>%
    mutate(mon = month(Date), yr = year(Date)) %>% 
    group_by(name, mon, yr, Source)  %>%  
    dplyr::summarise(Temperature = mean(Temperature, na.rm = TRUE)) %>% ggplot() +
    geom_line(aes(x = mon, y = Temperature, col = as.factor(yr)))+ theme_bw() + 
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank()) + 
    labs(y = if_else(type =="Anomaly", "Temperature Anomaly (deg C)", "Temperature (deg C)"), 
         title = if_else(type =="Anomaly", "Compare Years", "Seasonal Changes"),
         x = "Month") + 
    scale_color_viridis_d(name = "Year")  + 
    scale_x_continuous(breaks = seq(1,12,1), 
                       labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                                  "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) 
}

#plotly::ggplotly(Yrmon_clim(GoM_regions_temp, type = "Anomaly", region = "Gulf of Maine", Sources = "OISST"))

# Salinity indicator ts

sal_indicator_ts <- function(df, depth, type, variable){
  Buoy_data %>% filter(Depth == 1, Type == "Anomaly", Variable == "sal") %>% mutate(Year = year(Date)) %>%
    group_by(Year)  %>%  
    dplyr::summarise(Values = mean(Values, na.rm = TRUE)) %>% ggplot() + 
    geom_line(aes(Year, Values)) + theme_bw() + 
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank()) + 
    labs(y = "Salinity Anomaly (psu)", title = "Coastal Salinity")
}

#sal_indicator_ts(depth = 1, type = "Anomaly", variable = "sal")


