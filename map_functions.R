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
    popup = ~paste0("<b>Station:</b> ", `Model Location Name`,
                    '<br>', "<b>Station ID:</b> ", `Station ID`,
                    '<br>', "<b>Model Location:</b> ", ifelse(`Model Location`== "NA", NA, paste0(round(as.numeric(`Model Location`),1), " ", `Location Units`)),
                    '<br>', "<b>Calibration Parameter:</b> ", Parameter,
                    '<br>', "<b>Data Source:</b> ", `Data Source`),
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
    popup = ~paste0("<b>Station:</b> ", `Model Location Name`,
                    '<br>', "<b>Station ID:</b> ", `Station ID`,
                    '<br>', "<b>Model Location:</b> ", ifelse(`Model Location`== "NA", NA, paste0(round(as.numeric(`Model Location`),1), " ", `Location Units`)),
                    '<br>', "<b>Model Input Type:</b> ", `Model Location Type`,
                    '<br>', "<b>Model Input Parameter:</b> ", Parameter,
                    '<br>', "<b>Data Source:</b> ", `Data Source`),
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
    popup = ~paste0("<b>Station:</b> ", `Model Location Name`,
                    '<br>', "<b>Station ID:</b> ", `Station ID`,
                    '<br>', "<b>Model Location:</b> ", ifelse(`Model Location`== "NA", NA, paste0(round(as.numeric(`Model Location`),1), " ", `Location Units`)),
                    '<br>', "<b>Model Input Type:</b> ", `Model Location Type`,
                    '<br>', "<b>Model Input Parameter:</b> ", Parameter,
                    '<br>', "<b>Data Source:</b> ", `Data Source`),
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
    popup = ~paste0("<b>Facility Name (Facility Number):</b> ", data$`Facility Name (Facility Number)`,
                    "<br>", 
                    "<b>Permit Type and Description:</b> ", data$`Permit Type and Description`,
                    "<br>",
                    "<b>Stream River Mile:</b> ", data$`Stream River Mile`),
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
    popup = ~paste0("<b>Facility Name (Facility Number):</b> ", data$`Facility Name (Facility Number)`,
                    "<br>", 
                    "<b>Permit Type and Description:</b> ", data$`Permit Type and Description`,
                    "<br>",
                    "<b>Stream River Mile:</b> ", data$`Stream River Mile`),
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

effectiveShade.markers <- function(map, data) {
  
  # data = shade
  leaflet::addMarkers(
    map,
    data = data,
    group = "Effective Shade Measurement Sites",
    options = leaflet::leafletOptions(pane="marker"),
    #clusterOptions = markerClusterOptions(),
    label = paste0(data$Station),
    labelOptions = labelOptions(textsize = "15px"),
    popup = ~paste0("<b>Station:</b> ", data$Station,
                    "<br>", 
                    "<b>Effective shade measurement time:</b> ", t57_date,
                    "<br>",
                    "<b>Effective shade measurement value:</b> ", data$`Effective Shade`),
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

# Legends ----
# stroke color for temp non spawning lines. Same as fish use maps
tempWQScolor <- htmlwidgets::JS("function tempWQScolor(feature) {
    var colorToUse;
    var zdadm = feature.properties.Temperature_Criterion_7dADM_C;
                      
                      if (zdadm === \"12\") colorToUse = \"blue\";  
                      else if (zdadm === \"16\") colorToUse = \"darkgreen\"; 
                      else if (zdadm === \"18\") colorToUse = \"orange\";
                      else if (zdadm === \"20\") colorToUse = \"deeppink\";
                      else colorToUse = \"darkgrey\";
                      
    return {color: colorToUse};
    }")

# stroke color for spawning lines
# Hex colors generated using viridis::plasma(24)
tempSpawncolor <- htmlwidgets::JS("function tempSpawncolor(feature) {
    var colorToUse;
    var code = feature.properties.SpawnCode;
    
                      if (code === 0) colorToUse = \"darkgrey\";
                      else if (code === 11) colorToUse = \"darkgrey\";
                      else if (code === 12) colorToUse = \"#0D0887FF\";
                      else if (code === 13) colorToUse = \"#280592FF\";
                      else if (code === 14) colorToUse = \"#3C049BFF\";
                      else if (code === 15) colorToUse = \"#4F02A2FF\";
                      else if (code === 16) colorToUse = \"#F39C12FF\";
                      else if (code === 17) colorToUse = \"#6E2C00FF\";
                      else if (code === 18) colorToUse = \"#A04000FF\";
                      else if (code === 19) colorToUse = \"#D35400FF\";
                      else if (code === 20) colorToUse = \"#AF601AFF\";
                      else if (code === 21) colorToUse = \"#AE2892FF\";
                      else if (code === 22) colorToUse = \"#BB3488FF\";
                      else if (code === 23) colorToUse = \"#C6417DFF\";
                      else if (code === 24) colorToUse = \"#D04D73FF\";
                      else if (code === 25) colorToUse = \"#DA5A6AFF\";
                      else if (code === 26) colorToUse = \"#1C8200FF\";
                      else if (code === 27) colorToUse = \"#2E86C1FF\";
                      else if (code === 28) colorToUse = \"#85C1E9FF\";
                      else if (code === 29) colorToUse = \"#3498DBFF\";
                      else if (code === 30) colorToUse = \"#FBA039FF\";
                      else if (code === 31) colorToUse = \"#FDB030FF\";
                      else if (code === 32) colorToUse = \"#FDC229FF\";
                      else if (code === 33) colorToUse = \"#FBD324FF\";
                      else if (code === 34) colorToUse = \"#F6E626FF\";
                      else if (code === 35) colorToUse = \"#F0F921FF\";
                      else if (code === 99) colorToUse = \"darkgrey\";
                      else colorToUse = \"darkgrey\";
                      
    return {color: colorToUse};
    }")
