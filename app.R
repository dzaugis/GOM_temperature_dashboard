library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(ggplot2)
library(lubridate)
library(tidyverse)
library(leaflet)
library(shinyBS)
library(rgdal)

# Indicator Data
GoM_regions_temp <- data.table::fread("Data/GoM_regions_temp.csv") %>% mutate(Date = as.Date(Date))
Buoy_data <- data.table::fread("Data/buoys.csv")%>% mutate(Date = as.Date(Date), Depth = as.double(Depth))

# Raster Data
FVCOM_raster <- raster::brick("Data/FVCOM_raster_cropped.grd")
OISST_raster <- raster::brick("Data/OISST_data.grd")

# Shapefiles list
GoM_regions <- sf::st_read("Data/Shapefiles/GoM regions/PhysioRegions_WGS84.shp", stringsAsFactors = FALSE) 
lob_zones<- sf::st_read("Data/Shapefiles/Lobster_zones/Lob_zone_simple.shp", stringsAsFactors = FALSE)  %>% 
  sf::st_transform(., crs = '+proj=longlat +datum=WGS84') %>% dplyr::rename(., "Region" = ZONEID)
Gulf_of_Maine <- sf::st_read("Data/Shapefiles/GoM regions/GoM_combined_sp.shp", stringsAsFactors = FALSE) 

buoy_coords <- read_csv("Data/Buoy_locations.csv") %>% dplyr::rename(., "Region" = name) 

shapefiles_ls <- list("Gulf_of_Maine" = Gulf_of_Maine, "GoM_regions" = GoM_regions, "lob_zones" = lob_zones)

# Create selection index
Select_index <- unique(GoM_regions_temp$name)

#read in functions
source("indicator_funs.R")

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Indicators", tabName = "indicators", icon = icon("chart-line")),
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Compare-Contrast", tabName = "compare", icon = icon("sort-amount-up")),
    menuItem("Data Explorer", tabName = "temperature", icon = icon("globe-americas")),
    menuItem("GMRI", icon = icon("home"),
             href = "https://gmri.org")
  )
)

# HTML TAGS UI 
body <- dashboardBody(
  tags$head(tags$style(HTML('
        .skin-blue .main-header .logo {
          background-color: #00608A;
        }
        .skin-blue .main-header .logo:hover {
          background-color: #00608A;
        }
        .skin-blue .main-header .navbar {
          background-color: #00608A;
        }
        .main-header .logo {
          font-weight: bold;
          font-size: 24px;
        }
        .skin-blue .content-wrapper .tab-content .box-header {
          background-color: #00608A;
        }
      '))),
  
  # INFORMATION UI 
  tabItems(
    tabItem(tabName = "indicators",
            
            fluidRow(
              box(
                width = 12,
                title = "Gulf of Maine Temperature Indicators",
                solidHeader = TRUE, status = "primary",
                collapsible = FALSE,
                fluidRow(
                    box(
                      width = 12,
                      "Water temperature is connected to a number of biological events in the Gulf of Maine. 
                      
                      The plots below are different visulizations of sea surface temperature over time averaged
                      over the entire Gulf of Maine. 
                    
                      Note: I plan to follow the methods in Thomas et al 2017 to calculte the SST phenology metrics. 
                      What I show here is a rough cut using the first day the location passed a threshold. 
                      To accuratly account for short term bumps, I need to calculate 15 day rolling means for each grid cell 
                      and select the start of summer when the SST passed the threshold for 8 sequential days. I just haven't
                      processed the OISST or FVCOM data in that way yet.
                      "
                    )),
                fluidRow(
                    column(
                      width = 6,
                      plotly::plotlyOutput(outputId = "temp_indicator_ts"),
                      "This is a time series of yearly averaged temperature anomalies of the Gulf of Maine. 
                      Time runs along the bottom and the temperature difference from average, or anomaly, 
                      is on the vertical axis. Points above 0 indicate that the 
                      yearly averaged temperature was greater than normal and points below 0 show that 
                      the temperature was lower than normal. In this case 'normal' 
                      is the monthly average from 1982-2019."),
                    column(
                      width = 6,
                      plotly::plotlyOutput(outputId = "deg_days"),
                      "This plot shows the number of days each year where the daily averaged temperature 
                      in the Gulf of Maine was above 15 degrees celsius. This number of days has
                      has been a steady increasing over the time period where there is data.")),
                fluidRow(
                  column(
                    width = 6,
                    plotly::plotlyOutput(outputId = "date_above"),
                    "This plot shows the first date each year that experienced a daily average temperature above 
                    15 degrees celsius. For the average temperature of the Gulf of Maine, 15 degrees can be thought of as the
                    start of summer. This start of summer temperature varies by location, but overall they show an earlier start 
                    through time."
                  ),
                  column(
                    width = 6,
                    plotly::plotlyOutput(outputId = "end_of_sum"),
                    "This plot shows the last date each year that experienced a daily average temperature above
                    15 degrees celsius. This date can be thought of as the end of summer. Similar to the start of summer,
                    the temperature threshold for the start of summer changes by location."
                  )
                ),
                fluidRow(
                  column(
                    width = 6,
                    plotly::plotlyOutput(outputId = "yrday_clim"),
                    "This plot shows monthly averaged temperature plottled over the course of a year. 
                    You can see the seasonal cycle of the region clearly. This plot is interactive. 
                    You can double click (very quick double click) on a line to isolate that year. 
                    Single click to either add or remove a year."
                  ),
                  column(
                    width = 6,
                    plotly::plotlyOutput(outputId = "yrday_anom"),
                    "This plot shows monthly averaged temperature anomalies plottled over the course of a year. 
                    With this plot you can see differences in years. This plot is interactive. 
                    You can double click (very quick double click) on a line to isolate that year. 
                    Single click to either add or remove a year."
                  )
                )
              )
            )),
    tabItem(tabName = "dashboard",
            fluidRow(
              box(
                width = 12,
                title = "Your Dashboard",
                solidHeader = TRUE,
                status = "primary",
                collapsible = FALSE,
                fluidRow(
                  column(
                    width = 12,
                    "You can use the built in functions to make your own Gulf of Maine indicators dashboard. 
                    We provide suggested threshold temperature values the start of summer but you can 
                    explore how different temperatures change the patterns.
                    
                    \n 
                    
                    This is a bit buggy, I didn't extract a large enough area to cover all of these regions 
                    so some regions return an error. I probably to add some constrains to the input values 
                    and add some error-return text"
                  )
                ),
                fluidRow(
                  column(
                    width = 4,
                    selectInput(
                      inputId = "loc_select",
                      label = "Select Region",
                      choices = Select_index,
                      selected = "Gulf of Maine")),
                  column(
                    width = 4,
                    radioButtons(inputId = "top_bot",
                                 label = "Surface or Bottom",
                                 choices = c("Surface" = "OISST",
                                             "Bottom" = "FVCOM"),
                                 selected = "OISST")),
                  column(
                    width = 4,
                    numericInput(inputId = "temp_val",
                                 label = "Input Temperature",
                                 value = 15))),
                fluidRow(
                  column(
                    width = 6,
                    plotly::plotlyOutput(outputId = "dash_temp_indicator_ts")),
                  column(
                    width = 6,
                    plotly::plotlyOutput(outputId = "dash_deg_days"))),
                fluidRow(
                  column(
                    width = 6,
                    plotly::plotlyOutput(outputId = "dash_date_above")
                  ),
                  column(
                    width = 6,
                    plotly::plotlyOutput(outputId = "dash_end_of_sum")
                  )
                ),
                fluidRow(
                  column(
                    width = 6,
                    plotly::plotlyOutput(outputId = "dash_yrday_clim")
                  ),
                  column(
                    width = 6,
                    plotly::plotlyOutput(outputId = "dash_yrday_anom")
                  )
                )
              )
            )),
    tabItem(tabName = "compare",
            fluidRow(
              box(
                width = 12,
                title = "Compare Locations and Years",
                solidHeader = TRUE,
                status = "primary",
                collapsible = FALSE,
                column(
                  width = 12,
                  "Some introductory text..."
                ),
                fluidRow(
                  column(
                    width = 3,
                    selectInput(inputId = "comp_region", label = "Location",
                                choices = Select_index,
                                selected = "Gulf_of_Maine",
                                multiple = TRUE)
                    ),
                  column(
                    width = 3,
                    radioButtons(inputId = "comp_top_bot",
                                 label = "Surface or Bottom",
                                 choices = c("Surface" = "OISST",
                                             "Bottom" = "FVCOM"),
                                 selected = "OISST")
                  ),
                  column(
                    width = 3,
                    radioButtons(inputId = "comp_vis", label = "Data format",
                                 choices = c("Observed" = "Temp", "Anomalies" = "Anomaly"),
                                 selected = "Temp")
                  ),
                  column(
                    width = 3,
                    radioButtons(inputId = "comp_avg_over", label = "Average over:",
                                 choices = c("Day" = "Date", "Month" = "Month", "Year" = "Year"))
                  )
                  ),
                fluidRow(
                  column(
                    width = 12,
                    plotly::plotlyOutput(outputId = "comp_plot")
                  )
                )
                  
                )
              )
            
            ),
    tabItem(tabName = "temperature",
            fluidRow(
              box(width = 12, 
                  title = "Explore Gulf of Maine Temperature",
                  solidHeader = TRUE, status = "primary",
                  collapsible = TRUE,
                  fluidRow(
                    column(
                      width = 12,
                      "This page contains two spatial maps. The first, titled 'Regional Changes' shows the 
                      regions used to calculate the temperature indicators. You can select 
                      the Gulf of Maine, Gulf of Maine subregions, lobster zones, or a buoy to view the temperature 
                      time series of the surface or bottom at that location. 
                      \n 
                      The second tab is a high resolution spatial map of ocean temperature at a date of
                      your choosing. The map shows either the raw sea surface temperatue or bottom temperature 
                      and when you click on a grid cell, you can extract the time series of that point. You can also 
                      enter a boundig box and extract that time series of each grid cell within the bounding box.
                      Note: 
                      at some point I will include the option to view either the raw temps or anomalies. I just haven't
                      created a raster of the anomalies yet. It is also possible to add an 'upload shapefile' button
                      we you can upload a shapefile and extract the data from within that shpae."
                    )
                    )
                  )
              ),
            
            fluidRow(
              box(
                width = 12,
                title = "Regional Changes",
                solidHeader = TRUE, status = "primary",
                collapsible = TRUE,
                collapsed = TRUE,
                  fluidRow(
                    column(
                      width = 3,
                      selectInput(inputId = "region", label = "Location",
                                  choices = c("Gulf of Maine" = "Gulf_of_Maine", "Gulf of Maine subregions" = "GoM_regions", "Lobster Zones" = "lob_zones"),
                                  selected = "Gulf_of_Maine"),
                      bsTooltip("region", title= "Select region to show on map",
                                placement = "bottom", trigger = "hover")),
                    column(
                      width = 2,
                      radioButtons(inputId = "sur_bot", label = "Surface or Bottom",
                                   choices = c("Surface" = "OISST", "Bottom" = "FVCOM"), 
                                   selected = "OISST"),
                      bsTooltip("sur_bot", title= "Surface is OISST date, Bottom is FVCOM",
                                placement = "bottom", trigger = "hover")),
                    column(
                      width = 2,
                      radioButtons(inputId = "vis", label = "Data format",
                                   choices = c("Observed" = "Temp", "Anomalies" = "Anomaly"),
                                   selected = "Temp")),
                    column(
                      width = 2,
                      radioButtons(inputId = "avg_over", label = "Average over:",
                                   choices = c("Day" = "Date", "Month" = "Month", "Year" = "Year"))),
                    column(
                      width = 2,
                      downloadButton("download_ts", "Download Data"),
                      bsTooltip("download_ts", title= "Download a csv",
                                placement = "bottom", trigger = "hover"))),
                
                  fluidRow(
                    column(
                      width = 6,
                      leafletOutput("shp_map")),
                    column(
                        width = 6,
                        plotly::plotlyOutput(outputId = "indicators_main"))))),
            fluidRow(
              box(
                width = 12,
                title = "Spatial Map",
                solidHeader = TRUE,
                status = "primary",
                collapsible = TRUE,
                collapsed = TRUE,
                fluidRow(
                  column(
                    width = 5,
                    sliderInput("time_period", "Date",
                                min = min(as.Date("1995-01-01")),
                                max = max(as.Date("2019-04-30")),
                                value = as.Date("2018-11-24")),
                    bsTooltip("time_period", title = "Change the date to view temperature on map"),
                    downloadButton("downloadSpatial", "Download Data"),
                    checkboxInput("legend", "Show legend", TRUE)),
                  column(
                    width = 2,
                    radioButtons(
                      inputId = "sst_fv",
                      label = "Select Surface of Bottom",
                      choices = c("Surface" = "OISST_raster", "Bottom" = "FVCOM_raster"),
                      selected = "OISST_raster")),
                  column(
                    width = 5,
                    numericRangeInput("lon_range", "Longitude Range",
                                      value = c(-68,-65)),
                    numericRangeInput("lat_range", "Latitude Range",
                                      value = c(42.5,44)),
                    fluidRow(
                      column(width = 6,
                             actionButton(inputId = "box_subset", label = "Box Subset"),
                             downloadButton("downloadSubset", "Download Subset"),
                             bsTooltip("downloadSubset", "This may take awhile..."))))),
                fluidRow(
                  column(
                    width = 12,
                    leafletOutput("spatial_map"))),
                fluidRow(
                  column(
                    width = 12,
                    plotly::plotlyOutput(outputId = "ts_plot1")))
                )
              )
            )
    )
)




############################## UI ####
ui <- dashboardPage(
  dashboardHeader(title = "Gulf of Maine Indicators",
                  titleWidth = 350,
                  dropdownMenuOutput("notificationMenu")),
  sidebar,
  body
)


############################## SERVER ####

server <- function(input, output, session) { 
  
  # Temperature indicator 
  
  output$date_above <- plotly::renderPlotly({
    date_above(GoM_regions_temp, 15, "Gulf of Maine", "OISST")
  })
  
  output$deg_days <- plotly::renderPlotly({
    deg_days(GoM_regions_temp, 15, "Gulf of Maine", "OISST")
  })
  
  output$end_of_sum <- plotly::renderPlotly({
    end_of_sum(GoM_regions_temp, 15, "Gulf of Maine", "OISST")
  })
  
  output$temp_indicator_ts <- plotly::renderPlotly({
    temp_indicator_ts(GoM_regions_temp, type = "Anomaly", region = "Gulf of Maine", Sources = "OISST")
  })
  
  output$yrday_clim <- plotly::renderPlotly({
    Yrmon_clim(GoM_regions_temp, type = "Temp", region = input$loc_select, Sources = input$top_bot)
  })
  
  output$yrday_anom <- plotly::renderPlotly({
    Yrmon_clim(GoM_regions_temp, type = "Anomaly", region = input$loc_select, Sources = input$top_bot)
  })
  
  ## Dashboard
  
  observeEvent(input$loc_select, {
    # We'll use the input$controller variable multiple times, so save it as x
    # for convenience.
    
    tt <- GoM_regions_temp %>% mutate(yr = year(Date)) %>% 
      filter(Type == "Temp", Source == input$top_bot, name == input$loc_select) %>% 
      group_by(yr, name, Source)  %>%
      summarise(tmax = max(Temperature, na.rm = TRUE)) %>% ungroup() %>% dplyr::select(tmax) %>%
      summarise(tmin = min(tmax, na.rm = TRUE)) %>% mutate(tmin = round(tmin, 2)) %>% unlist()
    
    updateNumericInput(session, "temp_val", value = tt[[1]]-0.5)

  })
  
  output$dash_date_above <- plotly::renderPlotly({
    date_above(GoM_regions_temp, num = input$temp_val, region = input$loc_select, Sources = input$top_bot)
  })
  
  output$dash_deg_days <- plotly::renderPlotly({
    deg_days(GoM_regions_temp, num = input$temp_val, region = input$loc_select, Sources = input$top_bot)
  })
  
  output$dash_end_of_sum <- plotly::renderPlotly({
    end_of_sum(GoM_regions_temp, num = input$temp_val, region = input$loc_select, Sources = input$top_bot)
  })
  
  output$dash_temp_indicator_ts <- plotly::renderPlotly({
    temp_indicator_ts(GoM_regions_temp, type = "Anomaly", region = input$loc_select, Sources = input$top_bot)
  })
  
  output$dash_yrday_clim <- plotly::renderPlotly({
    Yrmon_clim(GoM_regions_temp, type = "Temp", region = input$loc_select, Sources = input$top_bot)
  })
  
  output$dash_yrday_anom <- plotly::renderPlotly({
    Yrmon_clim(GoM_regions_temp, type = "Anomaly", region = input$loc_select, Sources = input$top_bot)
  })
  
  
  # Compare and contrast
  
  output$comp_plot <- plotly::renderPlotly({
    req(input$comp_region)
    
    GoM_regions_temp %>% filter(Source == input$comp_top_bot, 
                                Type == input$comp_vis) %>% 
      mutate(Month = zoo::as.yearmon(format(as.Date(Date), "%Y-%m")), Year = year(Date)) %>%
      group_by(name, get(input$comp_avg_over))  %>%  dplyr::rename("x_axis" = `get(input$comp_avg_over)`) %>%
      dplyr::summarise(Temperature = mean(Temperature, na.rm = TRUE)) %>%
      filter(name %in% input$comp_region) %>%
      ggplot() + geom_line(aes(x = x_axis, y = Temperature, col = name)) + theme_bw() + 
      labs(y = "Temperature (deg C)", x = input$comp_avg_over)
  })
  
  ### Leaflet maps
  
  ### static backround map
  output$shp_map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>% 
      fitBounds(-70.5, 42.5, -66, 45)
  })  

  # regional reactive map
  observe({
    leafletProxy("shp_map", data = shapefiles_ls[[input$region]]) %>% clearShapes() %>%
      addPolygons(layerId = ~Region, opacity = 1, fill = TRUE, weight = 3, color = "gray", label = ~Region,
                  highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE)) %>%
      addMarkers(data = buoy_coords, lng = ~lon, lat = ~lat, label = ~Region, layerId = ~Region)
  })
  
  GoM_reactive <- reactive({
    GoM_regions_temp %>% filter(Source == input$sur_bot, 
                                Type == input$vis) %>% 
      mutate(Month = zoo::as.yearmon(format(as.Date(Date), "%Y-%m")), Year = year(Date)) %>%
      group_by(name, get(input$avg_over))  %>%  dplyr::rename("x_axis" = `get(input$avg_over)`) %>%
      dplyr::summarise(Temperature = mean(Temperature, na.rm = TRUE))
  })
  
  Buoy_reactive <- reactive({
    Buoy_data %>% filter(Depth == if_else(input$sur_bot == "OISST", 1, max(Depth)), 
                         Variable == "temp", Type == if_else(input$vis == "Temp", "raw", "Anomaly")) %>% 
      mutate(Month = zoo::as.yearmon(format(as.Date(Date), "%Y-%m")), Year = year(Date)) %>%
      group_by(name, get(input$avg_over))  %>%  dplyr::rename("x_axis" = `get(input$avg_over)`) %>%
      dplyr::summarise(Values = mean(Values, na.rm = TRUE))
  })

  
  #Observer to show Popups on click
  observe({
    map_click <- input$shp_map_shape_click
    if (is.null(map_click)) 
      return()
    
    output$indicators_main <- plotly::renderPlotly({
      GoM_reactive() %>% filter(name == if_else(input$region == "lob_zones", 
                                                paste("Lobster Zone", map_click$id), map_click$id)) %>%
        ggplot() + geom_line(aes(x = x_axis, y = Temperature)) + theme_bw() + 
        labs(title = if_else(input$region == "lob_zones", 
                             paste("Lobster Zone", map_click$id), map_click$id), 
             y = "Temperature (deg C)", x = input$avg_over)
    })
    
    
    output$download_ts <- downloadHandler(
      
      filename = function () {
        paste(if_else(input$region == "lob_zones", 
                      paste("Lobster Zone", map_click$id), map_click$id), input$sur_bot, ".csv", sep="_")}, 
      
      content = function(file) {
        write_csv(GoM_reactive(), file)})
  })
  
  observe({
    buoy_click <- input$shp_map_marker_click
    if (is.null(buoy_click))
      return()
 
    output$indicators_main <- plotly::renderPlotly({
      Buoy_reactive() %>% filter(name == buoy_click$id) %>% 
        ggplot() + geom_line(aes(x = x_axis, y = Values)) + theme_bw() + 
        labs(title = buoy_click$id, y = "Temperature (deg C)", x = input$avg_over)
    })
    
    output$download_ts <- downloadHandler(
      
      filename = function () {
        paste(buoy_click$id, ".csv", sep="_")}, 
      
      content = function(file) {
        write_csv(Buoy_reactive(), file)})

  })
  
  ## SPATIAL map Server Logic
  tmin_react <- reactive({
    tmin <- min(get(input$sst_fv)@data@min)
  })
  
  tmax_react <- reactive({
    tmax <- max(get(input$sst_fv)@data@max)
  })
  
  raster_pal <- reactive({
    tmax <- max(get(input$sst_fv)@data@max)
    tmin <- min(get(input$sst_fv)@data@min)
    raster_pal <- colorNumeric(
      palette = "viridis",
      domain = c(tmin,tmax),
      na.color = "transparent")
    return(raster_pal)
  })

  ## OISST or FVCOM
  reactive_raster <- reactive({
    if(input$sst_fv == "OISST_raster"){
      yr <- lubridate::year(input$time_period)
      mon <- lubridate::month(input$time_period)
      mon1 <- if_else(mon <= 9, paste("0",mon, sep = ""), paste(mon))
      dayz <- lubridate::day(input$time_period)
      dayz1 <- if_else(dayz <= 9, paste("0",dayz, sep=""), paste(dayz))
      r_layer <- paste("X",yr,".",mon1,".",dayz1, sep = "")
      
      return(OISST_raster[[r_layer]])}
    
    if(input$sst_fv == "FVCOM_raster"){
      fvcom_date_min <- str_extract(min(FVCOM_raster@data@names), "[0-9]{4}[:punct:][0-9]{2}[:punct:][0-9]{2}")
      fvcom_date_max <- str_extract(max(FVCOM_raster@data@names), "[0-9]{4}[:punct:][0-9]{2}[:punct:][0-9]{2}")
      layer_num <- format(seq(as.Date(fvcom_date_min, format = "%Y.%m.%d"), 
                              as.Date(fvcom_date_max, format = "%Y.%m.%d"), by = "month"), "%b-%Y")
      yr <- lubridate::year(input$time_period)
      mon <- lubridate::month(input$time_period)
      times <- format(as.Date(paste(yr,mon,"01",sep="-")), "%b-%Y")
      
      return(FVCOM_raster[[which(layer_num == times)]])}
    
  })
  
  # static backround map
  output$spatial_map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>% 
      fitBounds(-70.5, 42.5, -66, 45)
  })  
  
  # reactive map
  observe({ 
    leafletProxy("spatial_map") %>% clearImages() %>%
      addRasterImage(reactive_raster(), colors = raster_pal(), opacity = 0.9)
  })
  
  observe({
    
    proxy <- leafletProxy("spatial_map")
    proxy %>% clearControls()
    
    if (input$legend) {
      proxy %>% addLegend(position = "topleft",
                          pal = raster_pal(), 
                          title = "Sea Surface Temp",
                          values = c(tmin_react(), tmax_react()), opacity = .75, 
                          labels = raster_pal())}
  })
  
  # bounding box subset
  
  observe({
    
    req(input$box_subset)
    
    xmin <- min(input$lon_range)
    xmax <- max(input$lon_range)
    ymin <- min(input$lat_range)
    ymax <- max(input$lat_range)
    
    bbox <- raster::extent(xmin, xmax, ymin, ymax)
    
    subsetted <- raster::extract(get(input$sst_fv), bbox, df = TRUE, cellnumbers = TRUE)
    
    new_df <- cbind(raster::coordinates(get(input$sst_fv))[subsetted[,1],], subsetted) %>% 
      dplyr::select(-cell, -ID) %>% 
      pivot_longer(., cols = c(-x, -y), names_to = "Date", values_to = "Temp") %>%
      mutate(Date = as.Date(str_sub(Date, 2, 11), format = "%Y.%m.%d"))
    
    output$downloadSubset <- downloadHandler(
      
      filename = function () {
        paste(input$sst_fv, xmin, xmax, ymin, ymax, ".csv", sep="_")}, 
      
      content = function(file) {
        write_csv(new_df, file)})
  })
  
  
  observe({#Observer to show Popups on click
    click <- input$spatial_map_click
    if (is.null(click)) 
      return()
    x<-click$lng 
    y<-click$lat
    
    xy <- cbind(x,y)
    sp <- sp::SpatialPoints(xy)
    cell <- raster::cellFromXY(get(input$sst_fv), xy = sp)
    temp_ras <- raster::extract(get(input$sst_fv), sp)
    temp_ras <- data.frame(t(temp_ras)) %>% rownames_to_column() %>% 
      rename("Date" = rowname, "temp"= t.temp_ras.) %>% 
      mutate(yr = str_sub(Date, 2,5),
             mon = str_sub(Date,7,8),
             dayz = str_sub(Date,10,11),
             Date = as.Date(paste(yr,mon,dayz,sep="-")), 
             yrday = yday(Date)) %>% 
      group_by(yrday) %>% mutate(clim = mean(temp)) %>%
      ungroup() %>% mutate(anom = temp - clim) %>% ungroup() %>%
      dplyr::select(Date, temp, "anomaly" = anom)
    
    
    if(input$sst_fv == "OISST_raster")
      temp <- temp_ras %>% filter(Date == as.Date(input$time_period)) %>% 
        dplyr::select(temp) %>% round(digits = 2)
     else temp <- temp_ras %>% mutate(yr = year(Date), mon = month(Date)) %>% 
      filter(yr == year(as.Date(input$time_period)), mon == month(as.Date(input$time_period))) %>% 
              dplyr::select(temp) %>% round(digits = 2)
    
    proxy <- leafletProxy("spatial_map")
    #add Popup
    proxy %>% clearPopups() %>% addPopups(x, y, popup = paste(temp))
    # add temppoint
    # add anomalies to plot
    
    output$ts_plot1 <- plotly::renderPlotly({
      ggplot(temp_ras) + geom_line(aes(Date, anomaly)) + 
        theme_bw() + labs(x = "Date", y = "Anomaly (C)") +
        geom_vline(xintercept = as.Date(input$time_period), color = "red")
    })
    
    output$downloadSpatial <- downloadHandler(
      
      filename = function () {
        paste(input$sst_fv, input$time_period, round(x,2),round(y,2), ".csv", sep="_")}, 
      
      content = function(file) {
        write_csv(temp_ras, file)})
  
    })
  
}

shinyApp(ui, server)













