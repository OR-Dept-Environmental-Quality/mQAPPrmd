# Pop-up tables ----
popupTable.temp <- function(station_name = NULL){
  
  if(station_name %in% unique(sort(temp_stations$Station))){
    
    mapTempTbl <- temp.data.sample.count %>% 
      dplyr::filter(Station == station_name) %>% 
      dplyr::select(-c(`Station ID`, Station))
    
    table <- knitr::kable(mapTempTbl,
                          format = "html", row.names = FALSE,
                          caption = tags$h5("Count of days per month/year with available data:")) %>% 
      kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed","responsive"),
                                full_width = TRUE, font_size = 12,
                                position = "left")
    return(table)
    
  } else {
    
    print("")
    
  }
  
}

popupTable.flow <- function(station_name = NULL){
  
  if(station_name %in% unique(sort(flow_stations$Station))){
    
    mapFlowTbl <- flow.data.sample.count %>% 
      dplyr::filter(Station == station_name) %>% 
      dplyr::select(-c(`Station ID`, Station))
    
    table <- knitr::kable(mapFlowTbl,
                          format = "html", row.names = FALSE,
                          caption = tags$h5("Count of days per month/year with available data:")) %>% 
      kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed","responsive"),
                                full_width = TRUE, font_size = 12,
                                position = "left")
    return(table)
    
  } else {
    
    print("")
    
  }
  
}

popupTable.gageHeight <- function(station_name = NULL){
  
  if(station_name %in% unique(sort(gage_height_stations_map$Station))){
    
    mapGageHeightTbl <- gh.data.sample.count %>% 
      dplyr::filter(Station == station_name) %>% 
      dplyr::select(-c(`Station ID`, Station))
    
    table <- knitr::kable(mapGageHeightTbl,
                          format = "html", row.names = FALSE,
                          caption = tags$h5("Count of days per month/year with available data:")) %>% 
      kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed","responsive"),
                                full_width = TRUE, font_size = 12,
                                position = "left")
    return(table)
    
  } else {
    
    print("")
    
  }
  
}

# Markers ----
tempStation.markers <- function(map, data) {
  
  # data = temp_stations
  leaflet::addMarkers(
    map,
    data = data,
    group = "Stream Temperature Stations",
    options = leaflet::leafletOptions(pane="marker"),
    #clusterOptions = markerClusterOptions(),
    label = paste0(data$Organization, ": ", data$Station, " (", data$`Station ID`,")"),
    labelOptions = labelOptions(textsize = "15px"),
    popup = ~paste0("<b>", 
                    data$Organization," Station Name: ",
                    data$Station,"<br>",
                    "Station ID: ", data$`Station ID`,
                    sapply(data$Station, 
                           popupTable.temp, USE.NAMES = FALSE)),
    popupOptions = leaflet::popupOptions(maxWidth = 650, maxHeight = 300)
    )
  
}

tempCalibration.markers <- function(map, data) {
  
  # data = temp_cal_sites
  leaflet::addMarkers(
    map,
    data = data,
    group = "Stream Temperature Calibration Sites",
    options = leaflet::leafletOptions(pane="marker"),
    #clusterOptions = markerClusterOptions(),
    label = ifelse(is.na(data$`Station ID`),data$`Model Location Name`,paste0(data$`Model Location Name`," (", data$`Station ID`,")")), 
    labelOptions = labelOptions(textsize = "15px"),
    popup = ~paste0("Station: ", `Model Location Name`,
                    '<br>', "Station ID: ", `Station ID`,
                    '<br>', "Model Location: ", ifelse(`Model Location`== "NA", NA, paste0(round(as.numeric(`Model Location`),1), " ", `Location Units`)),
                    '<br>', "Calibration Parameter: ", Parameter,
                    '<br>', "Data Source: ", `Data Source`),
    popupOptions = leaflet::popupOptions(maxWidth = 650, maxHeight = 300)
    )
  
}

tempBoundaryTributary.markers <- function(map, data) {
  
  # data = temp_model_bc_tri
  leaflet::addMarkers(
    map,
    data = data,
    group = "Stream Temperature Model Boundary Conditions and Tributary Inputs",
    options = leaflet::leafletOptions(pane="marker"),
    #clusterOptions = markerClusterOptions(),
    label = ifelse(is.na(data$`Station ID`),data$`Model Location Name`,paste0(data$`Model Location Name`," (", data$`Station ID`,")")), 
    labelOptions = labelOptions(textsize = "15px"),
    popup = ~paste0("Station: ", `Model Location Name`,
                    '<br>', "Station ID: ", `Station ID`,
                    '<br>', "Model Location: ", ifelse(`Model Location`== "NA", NA, paste0(round(as.numeric(`Model Location`),1), " ", `Location Units`)),
                    '<br>', "Model Input Type: ", `Model Location Type`,
                    '<br>', "Model Input Parameter: ", Parameter,
                    '<br>', "Data Source: ", `Data Source`),
    popupOptions = leaflet::popupOptions(maxWidth = 650, maxHeight = 300)
    )
  
}

flowStation.markers <- function(map, data) {
  
  # data = flow_stations
  leaflet::addMarkers(
    map,
    data = data,
    group = "Stream Flow Stations",
    options = leaflet::leafletOptions(pane="marker"),
    #clusterOptions = markerClusterOptions(),
    label = paste0(data$`Data Source`, ": ", data$Station, " (", data$`Station ID`,")"),
    labelOptions = labelOptions(textsize = "15px"),
    popup = ~paste0("<b>", 
                    data$`Data Source`," Station Name: ",
                    data$Station,"<br>",
                    "Station ID: ", data$`Station ID`,
                    sapply(data$Station, 
                           popupTable.flow, USE.NAMES = FALSE)),
    popupOptions = leaflet::popupOptions(maxWidth = 650, maxHeight = 300)
    )
  
}

flowBoundaryTributary.markers <- function(map, data) {
  
  # data = flow_model_bc_tri
  leaflet::addMarkers(
    map,
    data = data,
    group = "Stream Flow Model Boundary Conditions and Tributary Inputs",
    options = leaflet::leafletOptions(pane="marker"),
    #clusterOptions = markerClusterOptions(),
    label = ifelse(is.na(data$`Station ID`),data$`Model Location Name`,paste0(data$`Model Location Name`," (", data$`Station ID`,")")), 
    labelOptions = labelOptions(textsize = "15px"),
    popup = ~paste0("Station: ", `Model Location Name`,
                    '<br>', "Station ID: ", `Station ID`,
                    '<br>', "Model Location: ", ifelse(`Model Location`== "NA", NA, paste0(round(as.numeric(`Model Location`),1), " ", `Location Units`)),
                    '<br>', "Model Input Type: ", `Model Location Type`,
                    '<br>', "Model Input Parameter: ", Parameter,
                    '<br>', "Data Source: ", `Data Source`),
    popupOptions = leaflet::popupOptions(maxWidth = 650, maxHeight = 300)
  )
  
}

metStation.markders <- function(map, data) {
  
  # data = met_stations
  leaflet::addMarkers(
    map,
    data = data,
    group = "Meteorological Stations",
    options = leaflet::leafletOptions(pane="marker"),
    #clusterOptions = markerClusterOptions(),
    label = paste0(data$tbl, ": ", data$Station, " (", data$`Station ID`, ")"),
    labelOptions = labelOptions(textsize = "15px")) 
  
}

indPS.markers <- function(map, data) {
  
  # data = ind_ps
  leaflet::addMarkers(
    map,
    data = data,
    group = "Individual NPDES Point Sources",
    options = leaflet::leafletOptions(pane="marker"),
    #clusterOptions = markerClusterOptions(),
    label = paste0(data$`Facility Name (Facility Number)`),
    labelOptions = labelOptions(textsize = "15px"),
    popup = ~paste0("Facility Name (Facility Number): ", data$`Facility Name (Facility Number)`,
                    "<br>", 
                    "Permit Type and Description: ", data$`Permit Type and Description`,
                    "<br>",
                    "Stream/River Mile: ", data$`Stream/River Mile`),
    popupOptions = leaflet::popupOptions(maxWidth = 650, maxHeight = 300)
  )
  
}

genPS.markers <- function(map, data) {
  
  # data = gen_ps
  leaflet::addMarkers(
    map,
    data = data,
    group = "General NPDES Point Sources (GEN01, GEN03, GEN04, GEN05, GEN19, or GEN40)",
    options = leaflet::leafletOptions(pane="marker"),
    #clusterOptions = markerClusterOptions(),
    label = paste0(data$`Facility Name (Facility Number)`),
    labelOptions = labelOptions(textsize = "15px"),
    popup = ~paste0("Facility Name (Facility Number): ", data$`Facility Name (Facility Number)`,
                    "<br>", 
                    "Permit Type and Description: ", data$`Permit Type and Description`,
                    "<br>",
                    "Stream/River Mile: ", data$`Stream/River Mile`),
    popupOptions = leaflet::popupOptions(maxWidth = 650, maxHeight = 300)
    )
  
}

gageHeight.markers <- function(map, data) {
  
  # data = gage_height_stations_map
  leaflet::addMarkers(
    map,
    data = data,
    group = "Gage Height Stations",
    options = leaflet::leafletOptions(pane="marker"),
    #clusterOptions = markerClusterOptions(),
    label = paste0("USGS: ", data$Station, " (", data$`Station ID`,")"),
    labelOptions = labelOptions(textsize = "15px"),
    popup = ~paste0("<b>", 
                    "USGS Station Name: ",
                    data$Station,"<br>",
                    "Station ID: ", data$`Station ID`,
                    sapply(data$Station, 
                           popupTable.gageHeight, USE.NAMES = FALSE)),
    popupOptions = leaflet::popupOptions(maxWidth = 650, maxHeight = 300)
    )
  
}

# Models ----
hsTempModel <- function(map, data) {
  
  # data = hs_temp_model_extent
  leaflet::addPolylines(
    map,
    data = data,
    group = "Heat Source Temperature Model Extent",
    options = leaflet::leafletOptions(pane="mod"),
    label = ~Stream,
    labelOptions = labelOptions(style = list("color" = "black",
                                             "font-size" = "20px")),
    color = "#045a8d",
    opacity = 1,
    weight = 4,
    fill=FALSE
    )
  
}

hsSolarModel <- function(map, data) {
  
  # data = hs_solar_model_extent
  leaflet::addPolylines(
    map,
    data = data,
    group = "Heat Source Solar Model Extent",
    options = leaflet::leafletOptions(pane="mod"),
    label = ~Stream,
    labelOptions = labelOptions(style = list("color" = "black",
                                             "font-size" = "20px")),
    color = "#3690c0",
    opacity = 1,
    weight = 4,
    fill=FALSE
    )
  
}

hsSolarArea <- function(map, data) {
  
  # data = hs_solar_model_area
  leaflet::addPolygons(
    map,
    data = data,
    group = "Heat Source Solar Model Area",
    options = leaflet::leafletOptions(pane="mod"),
    label = ~Name,
    labelOptions = labelOptions(style = list("color" = "black",
                                             "font-size" = "20px")),
    fillColor = "#3690c0",
    fillOpacity = 0.8,
    weight = 3,
    color = "#3690c0",
    opacity = 1) 
  
}

ceModel <- function(map, data) {
  
  # data = ce_model_extent
  leaflet::addPolylines(
    map,
    data = data,
    group = "CE-QUAL-W2 Temperature Model Extent",
    options = leaflet::leafletOptions(pane="mod"),
    label = ~Stream,
    labelOptions = labelOptions(style = list("color" = "black",
                                             "font-size" = "20px")),
    color = "#8c510a",
    opacity = 1,
    weight = 4,
    fill=FALSE
    ) 
  
}

shModel <- function(map, data) {
  
  # data = sh_model_extent
  leaflet::addPolylines(
    map,
    data = data,
    group = "SHADOW Model Extent",
    options = leaflet::leafletOptions(pane="mod"),
    label = ~Stname,
    labelOptions = labelOptions(style = list("color" = "black",
                                             "font-size" = "20px")),
    color = "#8c510a",
    opacity = 1,
    weight = 4,
    fill=FALSE
  ) 
  
}

# Color factor ----
pal <- colorFactor(palette = c("blue","green","orange","pink"),
                   levels = c("12", # Bull Trout Spawning and Juvenile Rearing
                              "16", # Core Cold Water Habitat
                              "18", # Salmon and Trout Rearing and Migration
                              "20"  # Salmon and Streelhead Migration Corridors
                   )
)



