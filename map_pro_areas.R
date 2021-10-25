library(tidyverse)
library(stringr)
library(leaflet)
library(leaflet.extras)
library(leaflet.esri)
library(sf)
library(tigris)
options(tigris_use_cache = TRUE)
library(htmltools)
library(htmlwidgets)
library(knitr)
library(kableExtra)
library(rmarkdown)
library(httr)
library(geojsonsf)

data.dir <- "//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/model_QAPPs/R/data/"
load(paste0("./data/lookup.RData"))

map.dir <-  "//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/model_QAPPs/R/map/area_maps/"

tag.map.title <- tags$style(HTML("
  .leaflet-control.map-title { 
    transform: translate(-50%,20%);
    position: fixed !important;
    left: 40%;
    text-align: left;
    padding-left: 10px; 
    padding-right: 10px; 
    background: rgba(255,255,255,0.75);
    font-weight: bold;
    font-size: 28px;
  }
"))

popupTable.temp <- function(station_name = NULL){
  
  # test: station_name <- "Little Sandy River Near Bull Run, OR"
  if(station_name %in% unique(sort(temp_stations$Station))){
    
    #station_name <- gsub(pattern=",[[:space:]]*OR$", replacement="", x=station_name, ignore.case = TRUE)
    
    mapTempTbl <- temp.data.sample.count %>% 
      dplyr::filter(Station == station_name) %>% 
      #dplyr::filter(grepl(paste0(station_name,"?"), Station, ignore.case = TRUE)) %>% 
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
    
    #station_name <- gsub(pattern=",[[:space:]]*OR$", replacement="", x=station_name, ignore.case = TRUE)
    
    mapFlowTbl <- flow.data.sample.count %>% 
      dplyr::filter(Station == station_name) %>% 
      #dplyr::filter(grepl(paste0(station_name,"?"), Station, ignore.case = TRUE)) %>% 
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
    
    # station_name <- gsub(pattern=",[[:space:]]*OR$", replacement="", x=station_name, ignore.case = TRUE)
    
    # test: station_name <- "Willamette River At Portland, OR"
    
    mapGageHeightTbl <- gh.data.sample.count %>% 
      dplyr::filter(Station == station_name) %>% 
      #dplyr::filter(grepl(paste0(station_name,"?"), Station, ignore.case = TRUE)) %>% 
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

# PROJECT AREA MAPS ----
## for test:
# qapp_project_area = "John Day River Basin"
# qapp_project_area = "Lower Grande Ronde, Imnaha, and Wallowa Subbasins"
# qapp_project_area = "Lower Willamette and Clackamas Subbasins"
# qapp_project_area = "Malheur River Subbasins"
# qapp_project_area = "Middle Willamette Subbasins"
# qapp_project_area = "Middle Columbia-Hood, Miles Creeks"
# qapp_project_area = "North Umpqua Subbasin"
# qapp_project_area = "Rogue River Basin"
qapp_project_area = "Sandy Subbasin"
# qapp_project_area = "South Umpqua and Umpqua Subbasins"
# qapp_project_area = "Southern Willamette Subbasins"
# qapp_project_area = "Walla Walla Subbasin"
# qapp_project_area = "Willamette River Mainstem and Major Tributaries"
# qapp_project_area = "Willow Creek Subbasin"

#for (qapp_project_area in project.areas$areas) {

  map.file.name <- paste0("map_", project.areas[which(project.areas$areas == qapp_project_area),]$file.name)
  load(paste0("./data/",map.file.name,".RData")) # data.R
  load(paste0("./data/",map.file.name,"_qapp.RData")) # model_QAPP.Rmd
  pro.area.extent <- unlist(strsplit(project.areas[which(project.areas$areas == qapp_project_area),]$huc8.extent, split = ","))
  subbasin_huc8 <- unique(lookup.huc[which(lookup.huc$QAPP_Project_Area == qapp_project_area),]$HUC_8)
  subbasin_huc10 <- unique(lookup.huc[which(lookup.huc$QAPP_Project_Area == qapp_project_area),]$HUC10)
  subbasin_huc12 <- unique(lookup.huc[which(lookup.huc$QAPP_Project_Area == qapp_project_area),]$HUC12)
  
  # where clause used in querying the feature layers from the REST Server
  where_huc8 <- ""
  where_huc10 <- ""
  where_huc12 <- ""
  reachcode <- ""
  for(huc_8 in sort(subbasin_huc8)){
    query_min <- paste0(huc_8,"000000")
    query_max <- paste0(huc_8,"999999")
    if(huc_8 == last(sort(subbasin_huc8))){
      where_last_8 <- paste0("HUC8 LIKE '", huc_8, "'")
      where_huc8 <- paste0(where_huc8,where_last_8)
      where_last_10 <- paste0("HUC10 LIKE '", huc_8, "%'")
      where_huc10 <- paste0(where_huc10,where_last_10)
      where_last_12 <- paste0("HUC12 LIKE '", huc_8, "%'")
      where_huc12 <- paste0(where_huc12,where_last_12)
      reachcode_last <- paste0("(ReachCode >= ", query_min, " AND ReachCode <= ", query_max, ")")
      reachcode <- paste0(reachcode,reachcode_last)
    } else {
      where_next_8 <- paste0("HUC8 LIKE '", huc_8, "' OR ")
      where_huc8 <- paste0(where_huc8, where_next_8)
      where_next_10 <- paste0("HUC10 LIKE '", huc_8, "%' OR ")
      where_huc10 <- paste0(where_huc10, where_next_10)
      where_next_12 <- paste0("HUC12 LIKE '", huc_8, "%' OR ")
      where_huc12 <- paste0(where_huc12, where_next_12)
      reachcode_next <- paste0("(ReachCode >= ", query_min, " AND ReachCode <= ", query_max, ") OR ")
      reachcode <- paste0(reachcode,reachcode_next)
    }
  }
  
  dta.check.area <- data.frame("Project Area"=qapp_project_area,
                               "hs_temp_model_extent" = nrow(hs_temp_model_extent),
                               "hs_solar_model_extent" = nrow(hs_solar_model_extent),
                               "hs_solar_model_area" = nrow(hs_solar_model_area),
                               "ce_model_extent" = nrow(ce_model_extent),
                               "sh_model_extent" = nrow(sh_model_extent),
                               "temp_stations" = nrow(temp_stations),
                               "temp_cal_sites" = nrow(temp_cal_sites),
                               "temp_model_bc_tri" = nrow(temp_model_bc_tri),
                               "flow_model_bc_tri" = nrow(flow_model_bc_tri),
                               "flow_stations" = nrow (flow_stations),
                               "met_stations" = nrow(met_stations),
                               "ind_ps" = nrow(ind_ps),
                               "gen_ps" = nrow(gen_ps))
  
  dta.stations.mod <- dta.check.area %>% 
    tidyr::pivot_longer(!Project.Area, names_to= "data", values_to = "nrow") %>% 
    dplyr::filter(!nrow == 0) %>% 
    pull(data)
  
  pal <- colorFactor(palette = c("blue","green","orange","pink"),
                     levels = c("12", # Bull Trout Spawning and Juvenile Rearing
                                "16", # Core Cold Water Habitat
                                "18", # Salmon and Trout Rearing and Migration
                                "20"  # Salmon and Streelhead Migration Corridors
                     )
  )
  
  print(qapp_project_area)
  
  # Basic layers ----
  map.title <- tags$div(tag.map.title, HTML(paste0(qapp_project_area)))
  map_basic <- leaflet::leaflet() %>%
    leaflet::addControl(map.title, position = "topleft", className="map-title")%>% 
    leaflet::addMiniMap(position = "bottomright",
                        width = 200,
                        height = 150,
                        zoomLevelFixed = 5,
                        toggleDisplay = TRUE,
                        minimized = TRUE) %>% 
    leaflet.extras::addResetMapButton() %>% 
    leaflet::fitBounds(lng1 = pro.area.extent[2], lat1 = pro.area.extent[1],
                       lng2 = pro.area.extent[4], lat2 = pro.area.extent[3]) %>%
    leaflet::addMapPane("OpenStreetMap", zIndex = -2000) %>% 
    leaflet::addMapPane("aerial", zIndex = -1100) %>% 
    leaflet::addMapPane("area", zIndex = -1000) %>%
    leaflet::addMapPane("huc8", zIndex = -900) %>%
    leaflet::addMapPane("huc10", zIndex = -800) %>%
    leaflet::addMapPane("huc12", zIndex = -700) %>%
    leaflet::addMapPane("wqs1", zIndex = -600) %>%
    leaflet::addMapPane("wqs2", zIndex = -500) %>%
    leaflet::addMapPane("ir", zIndex = -400) %>%
    leaflet::addMapPane("mod", zIndex = -300) %>%
    leaflet::addMapPane("modbes", zIndex = -350) %>%
    leaflet::addMapPane("mod2016", zIndex = -200) %>%
    leaflet::addMapPane("mod2009", zIndex = -200) %>%
    leaflet::addMapPane("node", zIndex = -100) %>%
    leaflet::addMapPane("marker", zIndex = 100) %>%
    leaflet::addProviderTiles("OpenStreetMap",group = "OpenStreetMap",
                              options = pathOptions(pane = "OpenStreetMap")) %>% 
    # __ Oregon Imagery ----
  leaflet.esri::addEsriImageMapLayer(url="https://imagery.oregonexplorer.info/arcgis/rest/services/OSIP_2018/OSIP_2018_WM/ImageServer",
                                     group = "Oregon Imagery",
                                     options = leaflet::leafletOptions(pane="aerial")) %>%
    leaflet.esri::addEsriImageMapLayer(url="https://imagery.oregonexplorer.info/arcgis/rest/services/OSIP_2017/OSIP_2017_WM/ImageServer",
                                       group = "Oregon Imagery",
                                       options = leaflet::leafletOptions(pane="aerial")) %>%
    # __ Project area outline ----
  leaflet::addPolygons(data = pro_area,
                       options = leaflet::leafletOptions(pane="area"),
                       fillColor = "transparent",
                       fillOpacity = 0,
                       weight = 3,
                       color = "black",
                       opacity = 1) %>% 
    # __ HUCs ----
  leaflet.esri::addEsriFeatureLayer(url="https://arcgis.deq.state.or.us/arcgis/rest/services/WQ/WBD/MapServer/1",
                                    options = leaflet.esri::featureLayerOptions(where = where_huc8),
                                    group = "HUC8",
                                    pathOptions = leaflet::pathOptions(pane="huc8"),
                                    labelProperty = "Name",
                                    labelOptions = leaflet::labelOptions(style = list("color" = "red",
                                                                                      "font-size" = "20px")),
                                    weight = 3,
                                    color = "red",
                                    opacity = 3,
                                    fillColor = "transparent",
                                    fillOpacity = 0) %>% 
    leaflet.esri::addEsriFeatureLayer(url="https://arcgis.deq.state.or.us/arcgis/rest/services/WQ/WBD/MapServer/2",
                                      options = leaflet.esri::featureLayerOptions(where = where_huc10),
                                      group = "HUC10",
                                      pathOptions = leaflet::pathOptions(pane="huc10"),
                                      labelProperty = "Name",
                                      labelOptions = leaflet::labelOptions(style = list("color" = "orange",
                                                                                        "font-size" = "20px")),
                                      weight = 2,
                                      color = "orange",
                                      opacity = 1,
                                      fillColor = "transparent",
                                      fillOpacity = 0) %>% 
    leaflet.esri::addEsriFeatureLayer(url="https://arcgis.deq.state.or.us/arcgis/rest/services/WQ/WBD/MapServer/3",
                                      options = leaflet.esri::featureLayerOptions(where = where_huc12),
                                      group = "HUC12",
                                      pathOptions = leaflet::pathOptions(pane="huc12"),
                                      labelProperty = "Name",
                                      labelOptions = leaflet::labelOptions(style = list("color" = "grey",
                                                                                        "font-size" = "20px")),
                                      weight = 1,
                                      color = "grey",
                                      opacity = 1,
                                      fillColor = "transparent",
                                      fillOpacity = 0) %>% 
    # __ WQS ----
  leaflet.esri::addEsriFeatureLayer(url="https://arcgis.deq.state.or.us/arcgis/rest/services/WQ/WQStandards_WM/MapServer/0",
                                    options = leaflet.esri::featureLayerOptions(where = reachcode),
                                    useServiceSymbology = TRUE,
                                    group = "Fish Use Designations",
                                    pathOptions = leaflet::pathOptions(pane="wqs1"),
                                    weight = 1,
                                    opacity = 1,
                                    fill=FALSE,
                                    highlightOptions = leaflet::highlightOptions(color="black",
                                                                                 weight = 4,
                                                                                 fillOpacity = 0.8,
                                                                                 bringToFront = TRUE,
                                                                                 sendToBack = TRUE),
                                    labelOptions = leaflet::labelOptions(offset = c(0,0),
                                                                         opacity = 0.9,
                                                                         textsize = "14px",
                                                                         sticky = FALSE),
                                    popupOptions = leaflet::popupOptions(maxWidth = 600, maxHeight = 500),
                                    labelProperty = htmlwidgets::JS("function(feature){var props = feature.properties; return props.GNIS_Name+\" \"}"),
                                    popupProperty = htmlwidgets::JS(paste0('function(feature){var props = feature.properties; return \"',
                                                                           '<b>Waterbody.Name:</b> \"+props.GNIS_Name+\"',
                                                                           '<br><b>Temperature.Criteria:</b> \"+props.Temperature_Criteria+\"',
                                                                           '<br><b>Temperature.Criterion.7dADM.deg-C:</b> \"+props.Temperature_Criterion_7dADM_C+\"',
                                                                           '<br><b>Temperature.Spawn.Dates:</b> \"+props.Temperature_Spawn_dates+\"',
                                                                           ' \"}'))
  ) %>% 
    leaflet.esri::addEsriFeatureLayer(url="https://arcgis.deq.state.or.us/arcgis/rest/services/WQ/WQStandards_WM/MapServer/0",
                                      options = leaflet.esri::featureLayerOptions(where = reachcode),
                                      useServiceSymbology = TRUE,
                                      group = "Salmon and Steelhead Spawning Use Designations",
                                      pathOptions = leaflet::pathOptions(pane="wqs2"),
                                      weight = 1,
                                      opacity = 1,
                                      fill=FALSE) %>% 
    # __ IR ----
  leaflet.esri::addEsriFeatureLayer(url="https://arcgis.deq.state.or.us/arcgis/rest/services/WQ/WQ_Assessment_2018_2020/MapServer/3",
                                    options = leaflet.esri::featureLayerOptions(where = where_huc12),
                                    useServiceSymbology = TRUE,
                                    group = "2018/2020 IR Temperature Status - Streams",
                                    pathOptions = leaflet::pathOptions(pane="ir"),
                                    weight = 1,
                                    opacity = 1,
                                    fill=FALSE) %>% 
    leaflet.esri::addEsriFeatureLayer(url="https://arcgis.deq.state.or.us/arcgis/rest/services/WQ/WQ_Assessment_2018_2020/MapServer/2",
                                      options = leaflet.esri::featureLayerOptions(where = where_huc12),
                                      useServiceSymbology = TRUE,
                                      group = "2018/2020 IR Temperature Status - Waterbodies",
                                      pathOptions = leaflet::pathOptions(pane="ir"),
                                      weight = 3,
                                      opacity = 1,
                                      fill=FALSE) %>% 
    leaflet.esri::addEsriFeatureLayer(url="https://arcgis.deq.state.or.us/arcgis/rest/services/WQ/WQ_Assessment_2018_2020/MapServer/4",
                                      options = leaflet.esri::featureLayerOptions(where = where_huc12),
                                      useServiceSymbology = TRUE,
                                      group = "2018/2020 IR Temperature Status - Watershed",
                                      pathOptions = leaflet::pathOptions(pane="ir"),
                                      weight = 1,
                                      opacity = 1,
                                      fill=FALSE) %>% 
    # __ Meteorological Stations ----
  leaflet::addMarkers(data = met_stations,
                      group = "Meteorological Stations",
                      options = leaflet::leafletOptions(pane="marker"),
                      #clusterOptions = markerClusterOptions(),
                      label = paste0(met_stations$tbl, ": ", met_stations$Station, " (", met_stations$`Station ID`, ")"),
                      labelOptions = labelOptions(textsize = "15px")) %>% 
    # __ Temp Stations ----
  leaflet::addMarkers(data = temp_stations,
                      group = "Stream Temperature Stations",
                      options = leaflet::leafletOptions(pane="marker"),
                      #clusterOptions = markerClusterOptions(),
                      label = paste0(temp_stations$Organization, ": ", temp_stations$Station, " (", temp_stations$`Station ID`,")"),
                      labelOptions = labelOptions(textsize = "15px"),
                      popup = ~paste0("<b>", 
                                      temp_stations$Organization," Station Name: ",
                                      temp_stations$Station,"<br>",
                                      "Station ID: ", temp_stations$`Station ID`,
                                      #"<br>",
                                      #"<br>",
                                      sapply(temp_stations$Station, 
                                             popupTable.temp, USE.NAMES = FALSE)),
                      popupOptions = leaflet::popupOptions(maxWidth = 650, maxHeight = 300)) %>% 
    # __ Flow Stations ----
  leaflet::addMarkers(data = flow_stations,
                      group = "Stream Flow Stations",
                      options = leaflet::leafletOptions(pane="marker"),
                      #clusterOptions = markerClusterOptions(),
                      label = paste0(flow_stations$`Data Source`, ": ", flow_stations$Station, " (", flow_stations$`Station ID`,")"),
                      labelOptions = labelOptions(textsize = "15px"),
                      popup = ~paste0("<b>", 
                                      flow_stations$`Data Source`," Station Name: ",
                                      flow_stations$Station,"<br>",
                                      "Station ID: ", flow_stations$`Station ID`,
                                      #"<br>",
                                      #"<br>",
                                      sapply(flow_stations$Station, 
                                             popupTable.flow, USE.NAMES = FALSE)),
                      popupOptions = leaflet::popupOptions(maxWidth = 650, maxHeight = 300)) %>% 
    # __ Search temp and flow stations ----
  leaflet.extras::addSearchFeatures(targetGroups = c("Stream Temperature Stations","Stream Flow Stations"),
                                    options = searchFeaturesOptions(openPopup = TRUE)) %>% 
    htmlwidgets::onRender(jsCode = "function(el, x){
    var elements = document.getElementsByClassName('Station');
    var index;
    elements = elements.length ? elements : [elements];
  for (index = 0; index < elements.length; index++) {
    element = elements[index];
    if (isElementHidden(element)) {
      element.style.display = '';
      // If the element is still hidden after removing the inline display
      if (isElementHidden(element)) {
        element.style.display = 'block';
      }
    } else {
      element.style.display = 'none';
    }
  }
  function isElementHidden (element) {
    return window.getComputedStyle(element, null).getPropertyValue('display') === 'none';
  }
  var layerToggle = document.getElementsByClassName('leaflet-bar easy-button-container leaflet-control')[4];
  layerToggle.style.float = 'none';
  layerToggle.style.backgroundColor = 'white';
               }")
  
  # John Day River Basin ----
  if(qapp_project_area == "John Day River Basin") {
    map_area <- map_basic %>% 
      # __ hs_temp_model_extent ----
    leaflet::addPolylines(data = hs_temp_model_extent,
                          group = "Heat Source Temperature Model Extent",
                          options = leaflet::leafletOptions(pane="mod"),
                          label = ~Stream,
                          labelOptions = labelOptions(style = list("color" = "black",
                                                                   "font-size" = "20px")),
                          color = "#045a8d",
                          opacity = 1,
                          weight = 4) %>% 
      # __ Temp Calibration Sites ----
    leaflet::addMarkers(data = temp_cal_sites,
                        group = "Stream Temperature Calibration Sites",
                        options = leaflet::leafletOptions(pane="marker"),
                        #clusterOptions = markerClusterOptions(),
                        label = ifelse(is.na(temp_cal_sites$`Station ID`),temp_cal_sites$`Model Location Name`,paste0(temp_cal_sites$`Model Location Name`," (", temp_cal_sites$`Station ID`,")")), 
                        labelOptions = labelOptions(textsize = "15px"),
                        popup = ~paste0("Station: ", `Model Location Name`,
                                        '<br>', "Station ID: ", `Station ID`,
                                        '<br>', "Model Location: ", ifelse(`Model Location`== "NA", NA, paste0(round(`Model Location`,1), " ", `Location Units`)),
                                        '<br>', "Calibration Parameter: ", Parameter,
                                        '<br>', "Data Source: ", `Data Source`),
                        popupOptions = leaflet::popupOptions(maxWidth = 650, maxHeight = 300)) %>% 
      # __ Temp Model Boundary Conditions and Tributary Inputs ----
    leaflet::addMarkers(data = temp_model_bc_tri,
                        group = "Stream Temperature Model Boundary Conditions and Tributary Inputs",
                        options = leaflet::leafletOptions(pane="marker"),
                        #clusterOptions = markerClusterOptions(),
                        label = ifelse(is.na(temp_model_bc_tri$`Station ID`),temp_model_bc_tri$`Model Location Name`,paste0(temp_model_bc_tri$`Model Location Name`," (", temp_model_bc_tri$`Station ID`,")")), 
                        labelOptions = labelOptions(textsize = "15px"),
                        popup = ~paste0("Station: ", `Model Location Name`,
                                        '<br>', "Station ID: ", `Station ID`,
                                        '<br>', "Model Location: ", ifelse(`Model Location`== "NA", NA, paste0(round(`Model Location`,1), " ", `Location Units`)),
                                        '<br>', "Model Input Type: ", `Model Location Type`,
                                        '<br>', "Model Input Parameter: ", Parameter,
                                        '<br>', "Data Source: ", `Data Source`),
                        popupOptions = leaflet::popupOptions(maxWidth = 650, maxHeight = 300)) %>% 
      # __ Flow Model Boundary Conditions and Tributary Inputs ----
    leaflet::addMarkers(data = flow_model_bc_tri,
                        group = "Stream Flow Model Boundary Conditions and Tributary Inputs",
                        options = leaflet::leafletOptions(pane="marker"),
                        #clusterOptions = markerClusterOptions(),
                        label = ifelse(is.na(flow_model_bc_tri$`Station ID`),flow_model_bc_tri$`Model Location Name`,paste0(flow_model_bc_tri$`Model Location Name`," (", flow_model_bc_tri$`Station ID`,")")), 
                        labelOptions = labelOptions(textsize = "15px"),
                        popup = ~paste0("Station: ", `Model Location Name`,
                                        '<br>', "Station ID: ", `Station ID`,
                                        '<br>', "Model Location: ", ifelse(`Model Location`== "NA", NA, paste0(round(`Model Location`,1), " ", `Location Units`)),
                                        '<br>', "Model Input Type: ", `Model Location Type`,
                                        '<br>', "Model Input Parameter: ", Parameter,
                                        '<br>', "Data Source: ", `Data Source`),
                        popupOptions = leaflet::popupOptions(maxWidth = 650, maxHeight = 300)) %>% 
      # __ Ind NPDES PS ----
    leaflet::addMarkers(data = ind_ps,
                        group = "Individual NPDES Point Sources",
                        options = leaflet::leafletOptions(pane="marker"),
                        #clusterOptions = markerClusterOptions(),
                        label = paste0(ind_ps$`Facility Name (Facility Number)`),
                        labelOptions = labelOptions(textsize = "15px"),
                        popup = ~paste0("Facility Name (Facility Number): ", ind_ps$`Facility Name (Facility Number)`,
                                        "<br>", 
                                        "Permit Type and Description: ", ind_ps$`Permit Type and Description`,
                                        "<br>",
                                        "Stream/River Mile: ", ind_ps$`Stream/River Mile`),
                        popupOptions = leaflet::popupOptions(maxWidth = 650, maxHeight = 300)) %>% 
      # __ Gen NPDES PS (GEN01, GEN03, GEN04, GEN05) ----
    leaflet::addMarkers(data = gen_ps,
                        group = "General NPDES Point Sources (GEN01, GEN03, GEN04, GEN05, GEN19, or GEN40)",
                        options = leaflet::leafletOptions(pane="marker"),
                        #clusterOptions = markerClusterOptions(),
                        label = paste0(gen_ps$`Facility Name (Facility Number)`),
                        labelOptions = labelOptions(textsize = "15px"),
                        popup = ~paste0("Facility Name (Facility Number): ", gen_ps$`Facility Name (Facility Number)`,
                                        "<br>", 
                                        "Permit Type and Description: ", gen_ps$`Permit Type and Description`,
                                        "<br>",
                                        "Stream/River Mile: ", gen_ps$`Stream/River Mile`),
                        popupOptions = leaflet::popupOptions(maxWidth = 650, maxHeight = 300)) %>% 
      leaflet::addLayersControl(overlayGroups = c("Heat Source Temperature Model Extent",
                                                  "Stream Temperature Stations",
                                                  "Stream Temperature Calibration Sites",
                                                  "Stream Temperature Model Boundary Conditions and Tributary Inputs",
                                                  "Stream Flow Stations",
                                                  "Stream Flow Model Boundary Conditions and Tributary Inputs",
                                                  "Meteorological Stations",
                                                  "Individual NPDES Point Sources",
                                                  "General NPDES Point Sources (GEN01, GEN03, GEN04, GEN05, GEN19, or GEN40)",
                                                  "HUC8","HUC10","HUC12",
                                                  "2018/2020 IR Temperature Status - Streams",
                                                  "2018/2020 IR Temperature Status - Waterbodies",
                                                  "2018/2020 IR Temperature Status - Watershed",
                                                  "Fish Use Designations",
                                                  "Salmon and Steelhead Spawning Use Designations",
                                                  "Oregon Imagery"),
                                options = leaflet::layersControlOptions(collapsed = FALSE, autoZIndex = TRUE)) %>% 
      leaflet::hideGroup(c("Stream Temperature Stations",
                           "Stream Temperature Calibration Sites",
                           "Stream Temperature Model Boundary Conditions and Tributary Inputs",
                           "Stream Flow Stations",
                           "Stream Flow Model Boundary Conditions and Tributary Inputs",
                           "Meteorological Stations",
                           "Individual NPDES Point Sources",
                           "General NPDES Point Sources (GEN01, GEN03, GEN04, GEN05, GEN19, or GEN40)",
                           "HUC8","HUC10","HUC12",
                           "2018/2020 IR Temperature Status - Streams",
                           "2018/2020 IR Temperature Status - Waterbodies",
                           "2018/2020 IR Temperature Status - Watershed",
                           "Fish Use Designations",
                           "Salmon and Steelhead Spawning Use Designations",
                           "Oregon Imagery"))
    
  }
  
  # Lower Grande Ronde, Imnaha, and Wallowa Subbasins ----
  if(qapp_project_area == "Lower Grande Ronde, Imnaha, and Wallowa Subbasins") {
    map_area <- map_basic %>% 
      # __ hs_temp_model_extent ----
    leaflet::addPolylines(data = hs_temp_model_extent,
                          group = "Heat Source Temperature Model Extent",
                          options = leaflet::leafletOptions(pane="mod"),
                          label = ~Stream,
                          labelOptions = labelOptions(style = list("color" = "black",
                                                                   "font-size" = "20px")),
                          color = "#045a8d",
                          opacity = 1,
                          weight = 4) %>% 
      # __ hs_solar_model_extent ----
    leaflet::addPolylines(data = hs_solar_model_extent,
                          group = "Heat Source Solar Model Extent",
                          options = leaflet::leafletOptions(pane="mod"),
                          label = ~Stream,
                          labelOptions = labelOptions(style = list("color" = "black",
                                                                   "font-size" = "20px")),
                          color = "#3690c0",
                          opacity = 1,
                          weight = 4) %>% 
      # __ Temp Calibration Sites ----
    leaflet::addMarkers(data = temp_cal_sites,
                        group = "Stream Temperature Calibration Sites",
                        options = leaflet::leafletOptions(pane="marker"),
                        #clusterOptions = markerClusterOptions(),
                        label = ifelse(is.na(temp_cal_sites$`Station ID`),temp_cal_sites$`Model Location Name`,paste0(temp_cal_sites$`Model Location Name`," (", temp_cal_sites$`Station ID`,")")), 
                        labelOptions = labelOptions(textsize = "15px"),
                        popup = ~paste0("Station: ", `Model Location Name`,
                                        '<br>', "Station ID: ", `Station ID`,
                                        '<br>', "Model Location: ", ifelse(`Model Location`== "NA", NA, paste0(round(`Model Location`,1), " ", `Location Units`)),
                                        '<br>', "Calibration Parameter: ", Parameter,
                                        '<br>', "Data Source: ", `Data Source`),
                        popupOptions = leaflet::popupOptions(maxWidth = 650, maxHeight = 300)) %>% 
      # __ Temp Model Boundary Conditions and Tributary Inputs ----
    leaflet::addMarkers(data = temp_model_bc_tri,
                        group = "Stream Temperature Model Boundary Conditions and Tributary Inputs",
                        options = leaflet::leafletOptions(pane="marker"),
                        #clusterOptions = markerClusterOptions(),
                        label = ifelse(is.na(temp_model_bc_tri$`Station ID`),temp_model_bc_tri$`Model Location Name`,paste0(temp_model_bc_tri$`Model Location Name`," (", temp_model_bc_tri$`Station ID`,")")), 
                        labelOptions = labelOptions(textsize = "15px"),
                        popup = ~paste0("Station: ", `Model Location Name`,
                                        '<br>', "Station ID: ", `Station ID`,
                                        '<br>', "Model Location: ", ifelse(`Model Location`== "NA", NA, paste0(round(`Model Location`,1), " ", `Location Units`)),
                                        '<br>', "Model Input Type: ", `Model Location Type`,
                                        '<br>', "Model Input Parameter: ", Parameter,
                                        '<br>', "Data Source: ", `Data Source`),
                        popupOptions = leaflet::popupOptions(maxWidth = 650, maxHeight = 300)) %>% 
      # __ Ind NPDES PS ----
    leaflet::addMarkers(data = ind_ps,
                        group = "Individual NPDES Point Sources",
                        options = leaflet::leafletOptions(pane="marker"),
                        #clusterOptions = markerClusterOptions(),
                        label = paste0(ind_ps$`Facility Name (Facility Number)`),
                        labelOptions = labelOptions(textsize = "15px"),
                        popup = ~paste0("Facility Name (Facility Number): ", ind_ps$`Facility Name (Facility Number)`,
                                        "<br>", 
                                        "Permit Type and Description: ", ind_ps$`Permit Type and Description`,
                                        "<br>",
                                        "Stream/River Mile: ", ind_ps$`Stream/River Mile`),
                        popupOptions = leaflet::popupOptions(maxWidth = 650, maxHeight = 300)) %>% 
      # __ Gen NPDES PS (GEN01, GEN03, GEN04, GEN05) ----
    leaflet::addMarkers(data = gen_ps,
                        group = "General NPDES Point Sources (GEN01, GEN03, GEN04, GEN05, GEN19, or GEN40)",
                        options = leaflet::leafletOptions(pane="marker"),
                        #clusterOptions = markerClusterOptions(),
                        label = paste0(gen_ps$`Facility Name (Facility Number)`),
                        labelOptions = labelOptions(textsize = "15px"),
                        popup = ~paste0("Facility Name (Facility Number): ", gen_ps$`Facility Name (Facility Number)`,
                                        "<br>", 
                                        "Permit Type and Description: ", gen_ps$`Permit Type and Description`,
                                        "<br>",
                                        "Stream/River Mile: ", gen_ps$`Stream/River Mile`),
                        popupOptions = leaflet::popupOptions(maxWidth = 650, maxHeight = 300)) %>% 
      leaflet::addLayersControl(overlayGroups = c("Heat Source Temperature Model Extent",
                                                  "Heat Source Solar Model Extent",
                                                  "Stream Temperature Stations",
                                                  "Stream Temperature Calibration Sites",
                                                  "Stream Temperature Model Boundary Conditions and Tributary Inputs",
                                                  "Stream Flow Stations",
                                                  "Meteorological Stations",
                                                  "Individual NPDES Point Sources",
                                                  "General NPDES Point Sources (GEN01, GEN03, GEN04, GEN05, GEN19, or GEN40)",
                                                  "HUC8","HUC10","HUC12",
                                                  "2018/2020 IR Temperature Status - Streams",
                                                  "2018/2020 IR Temperature Status - Waterbodies",
                                                  "2018/2020 IR Temperature Status - Watershed",
                                                  "Fish Use Designations",
                                                  "Salmon and Steelhead Spawning Use Designations",
                                                  "Oregon Imagery"),
                                options = leaflet::layersControlOptions(collapsed = FALSE, autoZIndex = TRUE)) %>% 
      leaflet::hideGroup(c("Heat Source Solar Model Extent",
                           "Stream Temperature Stations",
                           "Stream Temperature Calibration Sites",
                           "Stream Temperature Model Boundary Conditions and Tributary Inputs",
                           "Stream Flow Stations",
                           "Meteorological Stations",
                           "Individual NPDES Point Sources",
                           "General NPDES Point Sources (GEN01, GEN03, GEN04, GEN05, GEN19, or GEN40)",
                           "HUC8","HUC10","HUC12",
                           "2018/2020 IR Temperature Status - Streams",
                           "2018/2020 IR Temperature Status - Waterbodies",
                           "2018/2020 IR Temperature Status - Watershed",
                           "Fish Use Designations",
                           "Salmon and Steelhead Spawning Use Designations",
                           "Oregon Imagery"))
    
  }
  
  # Lower Willamette and Clackamas Subbasins ----
  if(qapp_project_area == "Lower Willamette and Clackamas Subbasins") {
    
    # ____ New models: BES 2019 ----
    bes_model_extent <- sf::st_read(dsn = "//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/model_QAPPs/R/data/gis/bes_pro_reaches.shp",
                                    layer = "bes_pro_reaches")  %>% 
      sf::st_transform(4326) %>% 
      sf::st_zm() %>% 
      dplyr::rename(Stream = NAME)
    
    map_area <- map_basic %>% 
      # __ hs_temp_model_extent ----
    leaflet::addPolylines(data = hs_temp_model_extent,
                          group = "Heat Source Temperature Model Extent (Existing Models)",
                          options = leaflet::leafletOptions(pane="mod"),
                          label = ~Stream,
                          labelOptions = labelOptions(style = list("color" = "black",
                                                                   "font-size" = "20px")),
                          color = "#045a8d",
                          opacity = 1,
                          weight = 4,
                          fill=FALSE) %>% 
      # __ ce_model_extent ----
    leaflet::addPolylines(data = ce_model_extent,
                          group = "CE-QUAL-W2 Temperature Model Extent",
                          options = leaflet::leafletOptions(pane="mod"),
                          label = ~Stream,
                          labelOptions = labelOptions(style = list("color" = "black",
                                                                   "font-size" = "20px")),
                          color = "#8c510a",
                          opacity = 1,
                          weight = 4,
                          fill=FALSE) %>% 
      # __ BES (2019) ----
    leaflet::addPolylines(data = bes_model_extent,
                          group = "Heat Source Shade Model Extent (New Models)",
                          options = leaflet::leafletOptions(pane="modbes"),
                          label = ~Stream,
                          labelOptions = labelOptions(style = list("color" = "black",
                                                                   "font-size" = "20px")),
                          color = "#3690c0",
                          opacity = 1,
                          weight = 5,
                          fill=FALSE) %>% 
      # __ Temp Calibration Sites ----
    leaflet::addMarkers(data = temp_cal_sites,
                        group = "Stream Temperature Calibration Sites",
                        options = leaflet::leafletOptions(pane="marker"),
                        #clusterOptions = markerClusterOptions(),
                        label = ifelse(is.na(temp_cal_sites$`Station ID`),temp_cal_sites$`Model Location Name`,paste0(temp_cal_sites$`Model Location Name`," (", temp_cal_sites$`Station ID`,")")), 
                        labelOptions = labelOptions(textsize = "15px"),
                        popup = ~paste0("Station: ", `Model Location Name`,
                                        '<br>', "Station ID: ", `Station ID`,
                                        '<br>', "Model Location: ", ifelse(`Model Location`== "NA", NA, paste0(round(`Model Location`,1), " ", `Location Units`)),
                                        '<br>', "Calibration Parameter: ", Parameter,
                                        '<br>', "Data Source: ", `Data Source`),
                        popupOptions = leaflet::popupOptions(maxWidth = 650, maxHeight = 300)) %>% 
      # __ Temp Model Boundary Conditions and Tributary Inputs ----
    leaflet::addMarkers(data = temp_model_bc_tri,
                        group = "Stream Temperature Model Boundary Conditions and Tributary Inputs",
                        options = leaflet::leafletOptions(pane="marker"),
                        #clusterOptions = markerClusterOptions(),
                        label = ifelse(is.na(temp_model_bc_tri$`Station ID`),temp_model_bc_tri$`Model Location Name`,paste0(temp_model_bc_tri$`Model Location Name`," (", temp_model_bc_tri$`Station ID`,")")), 
                        labelOptions = labelOptions(textsize = "15px"),
                        popup = ~paste0("Station: ", `Model Location Name`,
                                        '<br>', "Station ID: ", `Station ID`,
                                        '<br>', "Model Location: ", ifelse(`Model Location`== "NA", NA, paste0(round(`Model Location`,1), " ", `Location Units`)),
                                        '<br>', "Model Input Type: ", `Model Location Type`,
                                        '<br>', "Model Input Parameter: ", Parameter,
                                        '<br>', "Data Source: ", `Data Source`),
                        popupOptions = leaflet::popupOptions(maxWidth = 650, maxHeight = 300)) %>% 
      # __ Flow Model Boundary Conditions and Tributary Inputs ----
    leaflet::addMarkers(data = flow_model_bc_tri,
                        group = "Stream Flow Model Boundary Conditions and Tributary Inputs",
                        options = leaflet::leafletOptions(pane="marker"),
                        #clusterOptions = markerClusterOptions(),
                        label = ifelse(is.na(flow_model_bc_tri$`Station ID`),flow_model_bc_tri$`Model Location Name`,paste0(flow_model_bc_tri$`Model Location Name`," (", flow_model_bc_tri$`Station ID`,")")), 
                        labelOptions = labelOptions(textsize = "15px"),
                        popup = ~paste0("Station: ", `Model Location Name`,
                                        '<br>', "Station ID: ", `Station ID`,
                                        '<br>', "Model Location: ", ifelse(`Model Location`== "NA", NA, paste0(round(`Model Location`,1), " ", `Location Units`)),
                                        '<br>', "Model Input Type: ", `Model Location Type`,
                                        '<br>', "Model Input Parameter: ", Parameter,
                                        '<br>', "Data Source: ", `Data Source`),
                        popupOptions = leaflet::popupOptions(maxWidth = 650, maxHeight = 300)) %>% 
      # __ Ind NPDES PS ----
    leaflet::addMarkers(data = ind_ps,
                        group = "Individual NPDES Point Sources",
                        options = leaflet::leafletOptions(pane="marker"),
                        #clusterOptions = markerClusterOptions(),
                        label = paste0(ind_ps$`Facility Name (Facility Number)`),
                        labelOptions = labelOptions(textsize = "15px"),
                        popup = ~paste0("Facility Name (Facility Number): ", ind_ps$`Facility Name (Facility Number)`,
                                        "<br>", 
                                        "Permit Type and Description: ", ind_ps$`Permit Type and Description`,
                                        "<br>",
                                        "Stream/River Mile: ", ind_ps$`Stream/River Mile`),
                        popupOptions = leaflet::popupOptions(maxWidth = 650, maxHeight = 300)) %>% 
      # __ Gen NPDES PS (GEN01, GEN03, GEN04, GEN05) ----
    leaflet::addMarkers(data = gen_ps,
                        group = "General NPDES Point Sources (GEN01, GEN03, GEN04, GEN05, GEN19, or GEN40)",
                        options = leaflet::leafletOptions(pane="marker"),
                        #clusterOptions = markerClusterOptions(),
                        label = paste0(gen_ps$`Facility Name (Facility Number)`),
                        labelOptions = labelOptions(textsize = "15px"),
                        popup = ~paste0("Facility Name (Facility Number): ", gen_ps$`Facility Name (Facility Number)`,
                                        "<br>", 
                                        "Permit Type and Description: ", gen_ps$`Permit Type and Description`,
                                        "<br>",
                                        "Stream/River Mile: ", gen_ps$`Stream/River Mile`),
                        popupOptions = leaflet::popupOptions(maxWidth = 650, maxHeight = 300)) %>% 
      leaflet::addLayersControl(overlayGroups = c("Heat Source Temperature Model Extent (Existing Models)",
                                                  "Heat Source Shade Model Extent (New Models)",
                                                  "CE-QUAL-W2 Temperature Model Extent",
                                                  "Stream Temperature Stations",
                                                  "Stream Temperature Calibration Sites",
                                                  "Stream Temperature Model Boundary Conditions and Tributary Inputs",
                                                  "Stream Flow Stations",
                                                  "Stream Flow Model Boundary Conditions and Tributary Inputs",
                                                  "Meteorological Stations",
                                                  "Individual NPDES Point Sources",
                                                  "General NPDES Point Sources (GEN01, GEN03, GEN04, GEN05, GEN19, or GEN40)",
                                                  "HUC8","HUC10","HUC12",
                                                  "2018/2020 IR Temperature Status - Streams",
                                                  "2018/2020 IR Temperature Status - Waterbodies",
                                                  "2018/2020 IR Temperature Status - Watershed",
                                                  "Fish Use Designations",
                                                  "Salmon and Steelhead Spawning Use Designations",
                                                  "Oregon Imagery"),
                                options = leaflet::layersControlOptions(collapsed = FALSE, autoZIndex = TRUE)) %>% 
      leaflet::hideGroup(c("Heat Source Shade Model Extent (New Models)",
                           "CE-QUAL-W2 Temperature Model Extent",
                           "Stream Temperature Stations",
                           "Stream Temperature Calibration Sites",
                           "Stream Temperature Model Boundary Conditions and Tributary Inputs",
                           "Stream Flow Stations",
                           "Stream Flow Model Boundary Conditions and Tributary Inputs",
                           "Meteorological Stations",
                           "Individual NPDES Point Sources",
                           "General NPDES Point Sources (GEN01, GEN03, GEN04, GEN05, GEN19, or GEN40)",
                           "HUC8","HUC10","HUC12",
                           "2018/2020 IR Temperature Status - Streams",
                           "2018/2020 IR Temperature Status - Waterbodies",
                           "2018/2020 IR Temperature Status - Watershed",
                           "Fish Use Designations",
                           "Salmon and Steelhead Spawning Use Designations",
                           "Oregon Imagery"))
    
  }
  
  # Malheur River Subbasins ----
  if(qapp_project_area == "Malheur River Subbasins") {
    map_area <- map_basic %>% 
      # __ hs_solar_model_extent ----
    leaflet::addPolylines(data = hs_solar_model_extent,
                          group = "Heat Source Solar Model Extent",
                          options = leaflet::leafletOptions(pane="mod"),
                          label = ~Stream,
                          labelOptions = labelOptions(style = list("color" = "black",
                                                                   "font-size" = "20px")),
                          color = "#3690c0",
                          opacity = 1,
                          weight = 4) %>% 
      # __ Temp Calibration Sites ----
    leaflet::addMarkers(data = temp_cal_sites,
                        group = "Stream Temperature Calibration Sites",
                        options = leaflet::leafletOptions(pane="marker"),
                        #clusterOptions = markerClusterOptions(),
                        label = ifelse(is.na(temp_cal_sites$`Station ID`),temp_cal_sites$`Model Location Name`,paste0(temp_cal_sites$`Model Location Name`," (", temp_cal_sites$`Station ID`,")")), 
                        labelOptions = labelOptions(textsize = "15px"),
                        popup = ~paste0("Station: ", `Model Location Name`,
                                        '<br>', "Station ID: ", `Station ID`,
                                        '<br>', "Model Location: ", ifelse(`Model Location`== "NA", NA, paste0(round(`Model Location`,1), " ", `Location Units`)),
                                        '<br>', "Calibration Parameter: ", Parameter,
                                        '<br>', "Data Source: ", `Data Source`),
                        popupOptions = leaflet::popupOptions(maxWidth = 650, maxHeight = 300)) %>% 
      # __ Ind NPDES PS ----
    leaflet::addMarkers(data = ind_ps,
                        group = "Individual NPDES Point Sources",
                        options = leaflet::leafletOptions(pane="marker"),
                        #clusterOptions = markerClusterOptions(),
                        label = paste0(ind_ps$`Facility Name (Facility Number)`),
                        labelOptions = labelOptions(textsize = "15px"),
                        popup = ~paste0("Facility Name (Facility Number): ", ind_ps$`Facility Name (Facility Number)`,
                                        "<br>", 
                                        "Permit Type and Description: ", ind_ps$`Permit Type and Description`,
                                        "<br>",
                                        "Stream/River Mile: ", ind_ps$`Stream/River Mile`),
                        popupOptions = leaflet::popupOptions(maxWidth = 650, maxHeight = 300)) %>% 
      leaflet::addLayersControl(overlayGroups = c("Heat Source Solar Model Extent",
                                                  "Stream Temperature Stations",
                                                  "Stream Temperature Calibration Sites",
                                                  "Stream Flow Stations",
                                                  "Meteorological Stations",
                                                  "Individual NPDES Point Sources",
                                                  "HUC8","HUC10","HUC12",
                                                  "2018/2020 IR Temperature Status - Streams",
                                                  "2018/2020 IR Temperature Status - Waterbodies",
                                                  "2018/2020 IR Temperature Status - Watershed",
                                                  "Fish Use Designations",
                                                  "Salmon and Steelhead Spawning Use Designations",
                                                  "Oregon Imagery"),
                                options = leaflet::layersControlOptions(collapsed = FALSE, autoZIndex = TRUE)) %>% 
      leaflet::hideGroup(c("Stream Temperature Stations",
                           "Stream Temperature Calibration Sites",
                           "Stream Flow Stations",
                           "Meteorological Stations",
                           "Individual NPDES Point Sources",
                           "HUC8","HUC10","HUC12",
                           "2018/2020 IR Temperature Status - Streams",
                           "2018/2020 IR Temperature Status - Waterbodies",
                           "2018/2020 IR Temperature Status - Watershed",
                           "Fish Use Designations",
                           "Salmon and Steelhead Spawning Use Designations",
                           "Oregon Imagery"))
    
  }
  
  # Middle Willamette Subbasins ----
  if(qapp_project_area == "Middle Willamette Subbasins") {
    map_area <- map_basic %>% 
      # __ hs_temp_model_extent ----
    leaflet::addPolylines(data = hs_temp_model_extent,
                          group = "Heat Source Temperature Model Extent",
                          options = leaflet::leafletOptions(pane="mod"),
                          label = ~Stream,
                          labelOptions = labelOptions(style = list("color" = "black",
                                                                   "font-size" = "20px")),
                          color = "#045a8d",
                          opacity = 1,
                          weight = 4) %>% 
      # __ Temp Calibration Sites ----
    leaflet::addMarkers(data = temp_cal_sites,
                        group = "Stream Temperature Calibration Sites",
                        options = leaflet::leafletOptions(pane="marker"),
                        #clusterOptions = markerClusterOptions(),
                        label = ifelse(is.na(temp_cal_sites$`Station ID`),temp_cal_sites$`Model Location Name`,paste0(temp_cal_sites$`Model Location Name`," (", temp_cal_sites$`Station ID`,")")), 
                        labelOptions = labelOptions(textsize = "15px"),
                        popup = ~paste0("Station: ", `Model Location Name`,
                                        '<br>', "Station ID: ", `Station ID`,
                                        '<br>', "Model Location: ", ifelse(`Model Location`== "NA", NA, paste0(round(`Model Location`,1), " ", `Location Units`)),
                                        '<br>', "Calibration Parameter: ", Parameter,
                                        '<br>', "Data Source: ", `Data Source`),
                        popupOptions = leaflet::popupOptions(maxWidth = 650, maxHeight = 300)) %>% 
      # __ Temp Model Boundary Conditions and Tributary Inputs ----
    leaflet::addMarkers(data = temp_model_bc_tri,
                        group = "Stream Temperature Model Boundary Conditions and Tributary Inputs",
                        options = leaflet::leafletOptions(pane="marker"),
                        #clusterOptions = markerClusterOptions(),
                        label = ifelse(is.na(temp_model_bc_tri$`Station ID`),temp_model_bc_tri$`Model Location Name`,paste0(temp_model_bc_tri$`Model Location Name`," (", temp_model_bc_tri$`Station ID`,")")), 
                        labelOptions = labelOptions(textsize = "15px"),
                        popup = ~paste0("Station: ", `Model Location Name`,
                                        '<br>', "Station ID: ", `Station ID`,
                                        '<br>', "Model Location: ", ifelse(`Model Location`== "NA", NA, paste0(round(`Model Location`,1), " ", `Location Units`)),
                                        '<br>', "Model Input Type: ", `Model Location Type`,
                                        '<br>', "Model Input Parameter: ", Parameter,
                                        '<br>', "Data Source: ", `Data Source`),
                        popupOptions = leaflet::popupOptions(maxWidth = 650, maxHeight = 300)) %>% 
      # __ Flow Model Boundary Conditions and Tributary Inputs ----
    leaflet::addMarkers(data = flow_model_bc_tri,
                        group = "Stream Flow Model Boundary Conditions and Tributary Inputs",
                        options = leaflet::leafletOptions(pane="marker"),
                        #clusterOptions = markerClusterOptions(),
                        label = ifelse(is.na(flow_model_bc_tri$`Station ID`),flow_model_bc_tri$`Model Location Name`,paste0(flow_model_bc_tri$`Model Location Name`," (", flow_model_bc_tri$`Station ID`,")")), 
                        labelOptions = labelOptions(textsize = "15px"),
                        popup = ~paste0("Station: ", `Model Location Name`,
                                        '<br>', "Station ID: ", `Station ID`,
                                        '<br>', "Model Location: ", ifelse(`Model Location`== "NA", NA, paste0(round(`Model Location`,1), " ", `Location Units`)),
                                        '<br>', "Model Input Type: ", `Model Location Type`,
                                        '<br>', "Model Input Parameter: ", Parameter,
                                        '<br>', "Data Source: ", `Data Source`),
                        popupOptions = leaflet::popupOptions(maxWidth = 650, maxHeight = 300)) %>% 
      # __ Ind NPDES PS ----
    leaflet::addMarkers(data = ind_ps,
                        group = "Individual NPDES Point Sources",
                        options = leaflet::leafletOptions(pane="marker"),
                        #clusterOptions = markerClusterOptions(),
                        label = paste0(ind_ps$`Facility Name (Facility Number)`),
                        labelOptions = labelOptions(textsize = "15px"),
                        popup = ~paste0("Facility Name (Facility Number): ", ind_ps$`Facility Name (Facility Number)`,
                                        "<br>", 
                                        "Permit Type and Description: ", ind_ps$`Permit Type and Description`,
                                        "<br>",
                                        "Stream/River Mile: ", ind_ps$`Stream/River Mile`),
                        popupOptions = leaflet::popupOptions(maxWidth = 650, maxHeight = 300)) %>% 
      # __ Gen NPDES PS (GEN01, GEN03, GEN04, GEN05) ----
    leaflet::addMarkers(data = gen_ps,
                        group = "General NPDES Point Sources (GEN01, GEN03, GEN04, GEN05, GEN19, or GEN40)",
                        options = leaflet::leafletOptions(pane="marker"),
                        #clusterOptions = markerClusterOptions(),
                        label = paste0(gen_ps$`Facility Name (Facility Number)`),
                        labelOptions = labelOptions(textsize = "15px"),
                        popup = ~paste0("Facility Name (Facility Number): ", gen_ps$`Facility Name (Facility Number)`,
                                        "<br>", 
                                        "Permit Type and Description: ", gen_ps$`Permit Type and Description`,
                                        "<br>",
                                        "Stream/River Mile: ", gen_ps$`Stream/River Mile`),
                        popupOptions = leaflet::popupOptions(maxWidth = 650, maxHeight = 300)) %>% 
      leaflet::addLayersControl(overlayGroups = c("Heat Source Temperature Model Extent",
                                                  "Stream Temperature Stations",
                                                  "Stream Temperature Calibration Sites",
                                                  "Stream Temperature Model Boundary Conditions and Tributary Inputs",
                                                  "Stream Flow Stations",
                                                  "Stream Flow Model Boundary Conditions and Tributary Inputs",
                                                  "Meteorological Stations",
                                                  "Individual NPDES Point Sources",
                                                  "General NPDES Point Sources (GEN01, GEN03, GEN04, GEN05, GEN19, or GEN40)",
                                                  "HUC8","HUC10","HUC12",
                                                  "2018/2020 IR Temperature Status - Streams",
                                                  "2018/2020 IR Temperature Status - Waterbodies",
                                                  "2018/2020 IR Temperature Status - Watershed",
                                                  "Fish Use Designations",
                                                  "Salmon and Steelhead Spawning Use Designations",
                                                  "Oregon Imagery"),
                                options = leaflet::layersControlOptions(collapsed = FALSE, autoZIndex = TRUE)) %>% 
      leaflet::hideGroup(c("Stream Temperature Stations",
                           "Stream Temperature Calibration Sites",
                           "Stream Temperature Model Boundary Conditions and Tributary Inputs",
                           "Stream Flow Stations",
                           "Stream Flow Model Boundary Conditions and Tributary Inputs",
                           "Meteorological Stations",
                           "Individual NPDES Point Sources",
                           "General NPDES Point Sources (GEN01, GEN03, GEN04, GEN05, GEN19, or GEN40)",
                           "HUC8","HUC10","HUC12",
                           "2018/2020 IR Temperature Status - Streams",
                           "2018/2020 IR Temperature Status - Waterbodies",
                           "2018/2020 IR Temperature Status - Watershed",
                           "Fish Use Designations",
                           "Salmon and Steelhead Spawning Use Designations",
                           "Oregon Imagery"))
    
  }
  
  # Middle Columbia-Hood, Miles Creeks ----
  if(qapp_project_area == "Middle Columbia-Hood, Miles Creeks") {
    map_area <- map_basic %>% 
      # __ hs_temp_model_extent ----
    leaflet::addPolylines(data = hs_temp_model_extent,
                          group = "Heat Source Temperature Model Extent",
                          options = leaflet::leafletOptions(pane="mod"),
                          label = ~Stream,
                          labelOptions = labelOptions(style = list("color" = "black",
                                                                   "font-size" = "20px")),
                          color = "#045a8d",
                          opacity = 1,
                          weight = 4) %>% 
      # __ hs_solar_model_extent ----
    leaflet::addPolylines(data = hs_solar_model_extent,
                          group = "Heat Source Solar Model Extent",
                          options = leaflet::leafletOptions(pane="mod"),
                          label = ~Stream,
                          labelOptions = labelOptions(style = list("color" = "black",
                                                                   "font-size" = "20px")),
                          color = "#3690c0",
                          opacity = 1,
                          weight = 4) %>% 
      # __ Temp Calibration Sites ----
    leaflet::addMarkers(data = temp_cal_sites,
                        group = "Stream Temperature Calibration Sites",
                        options = leaflet::leafletOptions(pane="marker"),
                        #clusterOptions = markerClusterOptions(),
                        label = ifelse(is.na(temp_cal_sites$`Station ID`),temp_cal_sites$`Model Location Name`,paste0(temp_cal_sites$`Model Location Name`," (", temp_cal_sites$`Station ID`,")")), 
                        labelOptions = labelOptions(textsize = "15px"),
                        popup = ~paste0("Station: ", `Model Location Name`,
                                        '<br>', "Station ID: ", `Station ID`,
                                        '<br>', "Model Location: ", ifelse(`Model Location`== "NA", NA, paste0(round(`Model Location`,1), " ", `Location Units`)),
                                        '<br>', "Calibration Parameter: ", Parameter,
                                        '<br>', "Data Source: ", `Data Source`),
                        popupOptions = leaflet::popupOptions(maxWidth = 650, maxHeight = 300)) %>% 
      # __ Temp Model Boundary Conditions and Tributary Inputs ----
    leaflet::addMarkers(data = temp_model_bc_tri,
                        group = "Stream Temperature Model Boundary Conditions and Tributary Inputs",
                        options = leaflet::leafletOptions(pane="marker"),
                        #clusterOptions = markerClusterOptions(),
                        label = ifelse(is.na(temp_model_bc_tri$`Station ID`),temp_model_bc_tri$`Model Location Name`,paste0(temp_model_bc_tri$`Model Location Name`," (", temp_model_bc_tri$`Station ID`,")")), 
                        labelOptions = labelOptions(textsize = "15px"),
                        popup = ~paste0("Station: ", `Model Location Name`,
                                        '<br>', "Station ID: ", `Station ID`,
                                        '<br>', "Model Location: ", ifelse(`Model Location`== "NA", NA, paste0(round(`Model Location`,1), " ", `Location Units`)),
                                        '<br>', "Model Input Type: ", `Model Location Type`,
                                        '<br>', "Model Input Parameter: ", Parameter,
                                        '<br>', "Data Source: ", `Data Source`),
                        popupOptions = leaflet::popupOptions(maxWidth = 650, maxHeight = 300)) %>% 
      # __ Ind NPDES PS ----
    leaflet::addMarkers(data = ind_ps,
                        group = "Individual NPDES Point Sources",
                        options = leaflet::leafletOptions(pane="marker"),
                        #clusterOptions = markerClusterOptions(),
                        label = paste0(ind_ps$`Facility Name (Facility Number)`),
                        labelOptions = labelOptions(textsize = "15px"),
                        popup = ~paste0("Facility Name (Facility Number): ", ind_ps$`Facility Name (Facility Number)`,
                                        "<br>", 
                                        "Permit Type and Description: ", ind_ps$`Permit Type and Description`,
                                        "<br>",
                                        "Stream/River Mile: ", ind_ps$`Stream/River Mile`),
                        popupOptions = leaflet::popupOptions(maxWidth = 650, maxHeight = 300)) %>% 
      leaflet::addLayersControl(overlayGroups = c("Heat Source Temperature Model Extent",
                                                  "Heat Source Solar Model Extent",
                                                  "Stream Temperature Stations",
                                                  "Stream Temperature Calibration Sites",
                                                  "Stream Temperature Model Boundary Conditions and Tributary Inputs",
                                                  "Stream Flow Stations",
                                                  "Meteorological Stations",
                                                  "Individual NPDES Point Sources",
                                                  "HUC8","HUC10","HUC12",
                                                  "2018/2020 IR Temperature Status - Streams",
                                                  "2018/2020 IR Temperature Status - Waterbodies",
                                                  "2018/2020 IR Temperature Status - Watershed",
                                                  "Fish Use Designations",
                                                  "Salmon and Steelhead Spawning Use Designations",
                                                  "Oregon Imagery"),
                                options = leaflet::layersControlOptions(collapsed = FALSE, autoZIndex = TRUE)) %>% 
      leaflet::hideGroup(c("Heat Source Solar Model Extent",
                           "Stream Temperature Stations",
                           "Stream Temperature Calibration Sites",
                           "Stream Temperature Model Boundary Conditions and Tributary Inputs",
                           "Stream Flow Stations",
                           "Meteorological Stations",
                           "Individual NPDES Point Sources",
                           "HUC8","HUC10","HUC12",
                           "2018/2020 IR Temperature Status - Streams",
                           "2018/2020 IR Temperature Status - Waterbodies",
                           "2018/2020 IR Temperature Status - Watershed",
                           "Fish Use Designations",
                           "Salmon and Steelhead Spawning Use Designations",
                           "Oregon Imagery"))
    
  }
  
  # North Umpqua Subbasin ----
  if(qapp_project_area == "North Umpqua Subbasin") {
    
    # ____ New models: Fish Creek 2009 ----
    fish_creek_2009 <- sf::st_read(dsn = "//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/model_QAPPs/R/data/gis/fish_creek_2009.shp",
                                   layer = "fish_creek_2009") %>% 
      sf::st_transform(4326) %>% 
      sf::st_zm()
    
    # North Umpqua River model nodes
    n_umpqua_model_nodes <- sf::st_read(dsn = "//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/model_QAPPs/R/data/gis/n_umpqua_river_model_nodes.shp",
                                        layer = "n_umpqua_river_model_nodes") %>% 
      
      sf::st_transform(4326) %>%
      sf::st_zm()
    
    map_area <- map_basic %>% 
      # __ hs_temp_model_extent ----
    leaflet::addPolylines(data = hs_temp_model_extent,
                          group = "Heat Source Temperature Model Extent (Existing Models)",
                          options = leaflet::leafletOptions(pane="mod"),
                          label = ~Stream,
                          labelOptions = labelOptions(style = list("color" = "black",
                                                                   "font-size" = "20px")),
                          color = "#045a8d",
                          opacity = 1,
                          weight = 4) %>% 
      # __ Fish Creek (2009) ----
    leaflet::addPolylines(data = fish_creek_2009,
                          group = "Heat Source Temperature Model Extent (New Models)",
                          options = leaflet::leafletOptions(pane="mod2009"),
                          label = ~Stream,
                          labelOptions = labelOptions(style = list("color" = "black",
                                                                   "font-size" = "20px")),
                          color = "#993404",
                          opacity = 0.5,
                          weight = 10)%>% 
      leaflet::addCircleMarkers(data = n_umpqua_model_nodes,
                                group = "North Umpqua River Model Nodes",
                                options = leaflet::leafletOptions(pane="node"),
                                color = "#e34a33", #red orange
                                stroke = FALSE, 
                                fillOpacity = 0.5,
                                label = ~Location,
                                labelOptions = labelOptions(textsize = "15px")) %>% 
      # __ Temp Calibration Sites ----
    leaflet::addMarkers(data = temp_cal_sites,
                        group = "Stream Temperature Calibration Sites",
                        options = leaflet::leafletOptions(pane="marker"),
                        #clusterOptions = markerClusterOptions(),
                        label = ifelse(is.na(temp_cal_sites$`Station ID`),temp_cal_sites$`Model Location Name`,paste0(temp_cal_sites$`Model Location Name`," (", temp_cal_sites$`Station ID`,")")), 
                        labelOptions = labelOptions(textsize = "15px"),
                        popup = ~paste0("Station: ", `Model Location Name`,
                                        '<br>', "Station ID: ", `Station ID`,
                                        '<br>', "Model Location: ", ifelse(`Model Location`== "NA", NA, paste0(round(`Model Location`,1), " ", `Location Units`)),
                                        '<br>', "Calibration Parameter: ", Parameter,
                                        '<br>', "Data Source: ", `Data Source`),
                        popupOptions = leaflet::popupOptions(maxWidth = 650, maxHeight = 300)) %>% 
      # __ Temp Model Boundary Conditions and Tributary Inputs ----
    leaflet::addMarkers(data = temp_model_bc_tri,
                        group = "Stream Temperature Model Boundary Conditions and Tributary Inputs",
                        options = leaflet::leafletOptions(pane="marker"),
                        #clusterOptions = markerClusterOptions(),
                        label = ifelse(is.na(temp_model_bc_tri$`Station ID`),temp_model_bc_tri$`Model Location Name`,paste0(temp_model_bc_tri$`Model Location Name`," (", temp_model_bc_tri$`Station ID`,")")), 
                        labelOptions = labelOptions(textsize = "15px"),
                        popup = ~paste0("Station: ", `Model Location Name`,
                                        '<br>', "Station ID: ", `Station ID`,
                                        '<br>', "Model Location: ", ifelse(`Model Location`== "NA", NA, paste0(round(`Model Location`,1), " ", `Location Units`)),
                                        '<br>', "Model Input Type: ", `Model Location Type`,
                                        '<br>', "Model Input Parameter: ", Parameter,
                                        '<br>', "Data Source: ", `Data Source`),
                        popupOptions = leaflet::popupOptions(maxWidth = 650, maxHeight = 300)) %>% 
      # __ Ind NPDES PS ----
    leaflet::addMarkers(data = ind_ps,
                        group = "Individual NPDES Point Sources",
                        options = leaflet::leafletOptions(pane="marker"),
                        #clusterOptions = markerClusterOptions(),
                        label = paste0(ind_ps$`Facility Name (Facility Number)`),
                        labelOptions = labelOptions(textsize = "15px"),
                        popup = ~paste0("Facility Name (Facility Number): ", ind_ps$`Facility Name (Facility Number)`,
                                        "<br>", 
                                        "Permit Type and Description: ", ind_ps$`Permit Type and Description`,
                                        "<br>",
                                        "Stream/River Mile: ", ind_ps$`Stream/River Mile`),
                        popupOptions = leaflet::popupOptions(maxWidth = 650, maxHeight = 300)) %>% 
      # __ Gen NPDES PS (GEN01, GEN03, GEN04, GEN05) ----
    leaflet::addMarkers(data = gen_ps,
                        group = "General NPDES Point Sources (GEN01, GEN03, GEN04, GEN05, GEN19, or GEN40)",
                        options = leaflet::leafletOptions(pane="marker"),
                        #clusterOptions = markerClusterOptions(),
                        label = paste0(gen_ps$`Facility Name (Facility Number)`),
                        labelOptions = labelOptions(textsize = "15px"),
                        popup = ~paste0("Facility Name (Facility Number): ", gen_ps$`Facility Name (Facility Number)`,
                                        "<br>", 
                                        "Permit Type and Description: ", gen_ps$`Permit Type and Description`,
                                        "<br>",
                                        "Stream/River Mile: ", gen_ps$`Stream/River Mile`),
                        popupOptions = leaflet::popupOptions(maxWidth = 650, maxHeight = 300)) %>% 
      leaflet::addLayersControl(overlayGroups = c("Heat Source Temperature Model Extent (Existing Models)",
                                                  "Heat Source Temperature Model Extent (New Models)",
                                                  "North Umpqua River Model Nodes",
                                                  "Stream Temperature Stations",
                                                  "Stream Temperature Calibration Sites",
                                                  "Stream Temperature Model Boundary Conditions and Tributary Inputs",
                                                  "Stream Flow Stations",
                                                  "Meteorological Stations",
                                                  "Individual NPDES Point Sources",
                                                  "General NPDES Point Sources (GEN01, GEN03, GEN04, GEN05, GEN19, or GEN40)",
                                                  "HUC8","HUC10","HUC12",
                                                  "2018/2020 IR Temperature Status - Streams",
                                                  "2018/2020 IR Temperature Status - Waterbodies",
                                                  "2018/2020 IR Temperature Status - Watershed",
                                                  "Fish Use Designations",
                                                  "Salmon and Steelhead Spawning Use Designations",
                                                  "Oregon Imagery"),
                                options = leaflet::layersControlOptions(collapsed = FALSE, autoZIndex = TRUE)) %>% 
      leaflet::hideGroup(c("Heat Source Temperature Model Extent (New Models)",
                           "North Umpqua River Model Nodes",
                           "Stream Temperature Stations",
                           "Stream Temperature Calibration Sites",
                           "Stream Temperature Model Boundary Conditions and Tributary Inputs",
                           "Stream Flow Stations",
                           "Meteorological Stations",
                           "Individual NPDES Point Sources",
                           "General NPDES Point Sources (GEN01, GEN03, GEN04, GEN05, GEN19, or GEN40)",
                           "HUC8","HUC10","HUC12",
                           "2018/2020 IR Temperature Status - Streams",
                           "2018/2020 IR Temperature Status - Waterbodies",
                           "2018/2020 IR Temperature Status - Watershed",
                           "Fish Use Designations",
                           "Salmon and Steelhead Spawning Use Designations",
                           "Oregon Imagery"))
    
  }
  
  # Rogue River Basin ----
  if(qapp_project_area == "Rogue River Basin") {
    map_area <- map_basic %>% 
      # __ hs_temp_model_extent ----
    leaflet::addPolylines(data = hs_temp_model_extent,
                          group = "Heat Source Temperature Model Extent",
                          options = leaflet::leafletOptions(pane="mod"),
                          label = ~Stream,
                          labelOptions = labelOptions(style = list("color" = "black",
                                                                   "font-size" = "20px")),
                          color = "#045a8d",
                          opacity = 1,
                          weight = 4) %>% 
      # __ sh_model_extent ----
    leaflet::addPolylines(data = sh_model_extent,
                          group = "SHADOW Model Extent",
                          options = leaflet::leafletOptions(pane="mod"),
                          label = ~Stname,
                          labelOptions = labelOptions(style = list("color" = "black",
                                                                   "font-size" = "20px")),
                          color = "#8c510a",
                          opacity = 1,
                          weight = 4) %>%
      # __ Temp Calibration Sites ----
    leaflet::addMarkers(data = temp_cal_sites,
                        group = "Stream Temperature Calibration Sites",
                        options = leaflet::leafletOptions(pane="marker"),
                        #clusterOptions = markerClusterOptions(),
                        label = ifelse(is.na(temp_cal_sites$`Station ID`),temp_cal_sites$`Model Location Name`,paste0(temp_cal_sites$`Model Location Name`," (", temp_cal_sites$`Station ID`,")")), 
                        labelOptions = labelOptions(textsize = "15px"),
                        popup = ~paste0("Station: ", `Model Location Name`,
                                        '<br>', "Station ID: ", `Station ID`,
                                        '<br>', "Model Location: ", ifelse(`Model Location`== "NA", NA, paste0(round(`Model Location`,1), " ", `Location Units`)),
                                        '<br>', "Calibration Parameter: ", Parameter,
                                        '<br>', "Data Source: ", `Data Source`),
                        popupOptions = leaflet::popupOptions(maxWidth = 650, maxHeight = 300)) %>% 
      # __ Temp Model Boundary Conditions and Tributary Inputs ----
    leaflet::addMarkers(data = temp_model_bc_tri,
                        group = "Stream Temperature Model Boundary Conditions and Tributary Inputs",
                        options = leaflet::leafletOptions(pane="marker"),
                        #clusterOptions = markerClusterOptions(),
                        label = ifelse(is.na(temp_model_bc_tri$`Station ID`),temp_model_bc_tri$`Model Location Name`,paste0(temp_model_bc_tri$`Model Location Name`," (", temp_model_bc_tri$`Station ID`,")")), 
                        labelOptions = labelOptions(textsize = "15px"),
                        popup = ~paste0("Station: ", `Model Location Name`,
                                        '<br>', "Station ID: ", `Station ID`,
                                        '<br>', "Model Location: ", ifelse(`Model Location`== "NA", NA, paste0(round(`Model Location`,1), " ", `Location Units`)),
                                        '<br>', "Model Input Type: ", `Model Location Type`,
                                        '<br>', "Model Input Parameter: ", Parameter,
                                        '<br>', "Data Source: ", `Data Source`),
                        popupOptions = leaflet::popupOptions(maxWidth = 650, maxHeight = 300)) %>% 
      # __ Flow Model Boundary Conditions and Tributary Inputs ----
    leaflet::addMarkers(data = flow_model_bc_tri,
                        group = "Stream Flow Model Boundary Conditions and Tributary Inputs",
                        options = leaflet::leafletOptions(pane="marker"),
                        #clusterOptions = markerClusterOptions(),
                        label = ifelse(is.na(flow_model_bc_tri$`Station ID`),flow_model_bc_tri$`Model Location Name`,paste0(flow_model_bc_tri$`Model Location Name`," (", flow_model_bc_tri$`Station ID`,")")), 
                        labelOptions = labelOptions(textsize = "15px"),
                        popup = ~paste0("Station: ", `Model Location Name`,
                                        '<br>', "Station ID: ", `Station ID`,
                                        '<br>', "Model Location: ", ifelse(`Model Location`== "NA", NA, paste0(round(`Model Location`,1), " ", `Location Units`)),
                                        '<br>', "Model Input Type: ", `Model Location Type`,
                                        '<br>', "Model Input Parameter: ", Parameter,
                                        '<br>', "Data Source: ", `Data Source`),
                        popupOptions = leaflet::popupOptions(maxWidth = 650, maxHeight = 300)) %>% 
      # __ Ind NPDES PS ----
    leaflet::addMarkers(data = ind_ps,
                        group = "Individual NPDES Point Sources",
                        options = leaflet::leafletOptions(pane="marker"),
                        #clusterOptions = markerClusterOptions(),
                        label = paste0(ind_ps$`Facility Name (Facility Number)`),
                        labelOptions = labelOptions(textsize = "15px"),
                        popup = ~paste0("Facility Name (Facility Number): ", ind_ps$`Facility Name (Facility Number)`,
                                        "<br>", 
                                        "Permit Type and Description: ", ind_ps$`Permit Type and Description`,
                                        "<br>",
                                        "Stream/River Mile: ", ind_ps$`Stream/River Mile`),
                        popupOptions = leaflet::popupOptions(maxWidth = 650, maxHeight = 300)) %>% 
      # __ Gen NPDES PS (GEN01, GEN03, GEN04, GEN05) ----
    leaflet::addMarkers(data = gen_ps,
                        group = "General NPDES Point Sources (GEN01, GEN03, GEN04, GEN05, GEN19, or GEN40)",
                        options = leaflet::leafletOptions(pane="marker"),
                        #clusterOptions = markerClusterOptions(),
                        label = paste0(gen_ps$`Facility Name (Facility Number)`),
                        labelOptions = labelOptions(textsize = "15px"),
                        popup = ~paste0("Facility Name (Facility Number): ", gen_ps$`Facility Name (Facility Number)`,
                                        "<br>", 
                                        "Permit Type and Description: ", gen_ps$`Permit Type and Description`,
                                        "<br>",
                                        "Stream/River Mile: ", gen_ps$`Stream/River Mile`),
                        popupOptions = leaflet::popupOptions(maxWidth = 650, maxHeight = 300)) %>% 
      leaflet::addLayersControl(overlayGroups = c("Heat Source Temperature Model Extent",
                                                  "SHADOW Model Extent",
                                                  "Stream Temperature Stations",
                                                  "Stream Temperature Calibration Sites",
                                                  "Stream Temperature Model Boundary Conditions and Tributary Inputs",
                                                  "Stream Flow Stations",
                                                  "Stream Flow Model Boundary Conditions and Tributary Inputs",
                                                  "Meteorological Stations",
                                                  "Individual NPDES Point Sources",
                                                  "General NPDES Point Sources (GEN01, GEN03, GEN04, GEN05, GEN19, or GEN40)",
                                                  "HUC8","HUC10","HUC12",
                                                  "2018/2020 IR Temperature Status - Streams",
                                                  "2018/2020 IR Temperature Status - Waterbodies",
                                                  "2018/2020 IR Temperature Status - Watershed",
                                                  "Fish Use Designations",
                                                  "Salmon and Steelhead Spawning Use Designations",
                                                  "Oregon Imagery"),
                                options = leaflet::layersControlOptions(collapsed = FALSE, autoZIndex = TRUE)) %>% 
      leaflet::hideGroup(c("SHADOW Model Extent",
                           "Stream Temperature Stations",
                           "Stream Temperature Calibration Sites",
                           "Stream Temperature Model Boundary Conditions and Tributary Inputs",
                           "Stream Flow Stations",
                           "Stream Flow Model Boundary Conditions and Tributary Inputs",
                           "Meteorological Stations",
                           "Individual NPDES Point Sources",
                           "General NPDES Point Sources (GEN01, GEN03, GEN04, GEN05, GEN19, or GEN40)",
                           "HUC8","HUC10","HUC12",
                           "2018/2020 IR Temperature Status - Streams",
                           "2018/2020 IR Temperature Status - Waterbodies",
                           "2018/2020 IR Temperature Status - Watershed",
                           "Fish Use Designations",
                           "Salmon and Steelhead Spawning Use Designations",
                           "Oregon Imagery"))
    
  }
  
  # Sandy Subbasin ----
  if(qapp_project_area == "Sandy Subbasin") {
    
    # ____ New models: Sandy 2016 ----
    sandy_2016 <- sf::st_read(dsn = "//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/model_QAPPs/R/data/gis/sandy_2016.shp",
                              layer = "sandy_2016") %>% 
      sf::st_transform(4326) %>% 
      sf::st_zm()
    
    map_area <- map_basic %>% 
      # __ hs_temp_model_extent ----
    leaflet::addPolylines(data = hs_temp_model_extent,
                          group = "Heat Source Temperature Model Extent (Existing Models)",
                          options = leaflet::leafletOptions(pane="mod"),
                          label = ~Stream,
                          labelOptions = labelOptions(style = list("color" = "black",
                                                                   "font-size" = "20px")),
                          color = "#045a8d",
                          opacity = 1,
                          weight = 4) %>% 
      # __ Bull Run River, Salmon River and Sandy Rivers (2016) ----
    leaflet::addPolylines(data = sandy_2016,
                          group = "Heat Source Temperature Model Extent (New Models)",
                          options = leaflet::leafletOptions(pane="mod2016"),
                          label = ~Name,
                          labelOptions = labelOptions(style = list("color" = "black",
                                                                   "font-size" = "20px")),
                          color = "#993404",
                          opacity = 0.5,
                          weight = 10) %>% 
      # __ Temp Calibration Sites ----
    leaflet::addMarkers(data = temp_cal_sites,
                        group = "Stream Temperature Calibration Sites",
                        options = leaflet::leafletOptions(pane="marker"),
                        #clusterOptions = markerClusterOptions(),
                        label = ifelse(is.na(temp_cal_sites$`Station ID`),temp_cal_sites$`Model Location Name`,paste0(temp_cal_sites$`Model Location Name`," (", temp_cal_sites$`Station ID`,")")), 
                        labelOptions = labelOptions(textsize = "15px"),
                        popup = ~paste0("Station: ", `Model Location Name`,
                                        '<br>', "Station ID: ", `Station ID`,
                                        '<br>', "Model Location: ", ifelse(`Model Location`== "NA", NA, paste0(round(`Model Location`,1), " ", `Location Units`)),
                                        '<br>', "Calibration Parameter: ", Parameter,
                                        '<br>', "Data Source: ", `Data Source`),
                        popupOptions = leaflet::popupOptions(maxWidth = 650, maxHeight = 300)) %>% 
      # __ Temp Model Boundary Conditions and Tributary Inputs ----
    leaflet::addMarkers(data = temp_model_bc_tri,
                        group = "Stream Temperature Model Boundary Conditions and Tributary Inputs",
                        options = leaflet::leafletOptions(pane="marker"),
                        #clusterOptions = markerClusterOptions(),
                        label = ifelse(is.na(temp_model_bc_tri$`Station ID`),temp_model_bc_tri$`Model Location Name`,paste0(temp_model_bc_tri$`Model Location Name`," (", temp_model_bc_tri$`Station ID`,")")), 
                        labelOptions = labelOptions(textsize = "15px"),
                        popup = ~paste0("Station: ", `Model Location Name`,
                                        '<br>', "Station ID: ", `Station ID`,
                                        '<br>', "Model Location: ", ifelse(`Model Location`== "NA", NA, paste0(round(`Model Location`,1), " ", `Location Units`)),
                                        '<br>', "Model Input Type: ", `Model Location Type`,
                                        '<br>', "Model Input Parameter: ", Parameter,
                                        '<br>', "Data Source: ", `Data Source`),
                        popupOptions = leaflet::popupOptions(maxWidth = 650, maxHeight = 300)) %>% 
      # __ Flow Model Boundary Conditions and Tributary Inputs ----
    leaflet::addMarkers(data = flow_model_bc_tri,
                        group = "Stream Flow Model Boundary Conditions and Tributary Inputs",
                        options = leaflet::leafletOptions(pane="marker"),
                        #clusterOptions = markerClusterOptions(),
                        label = ifelse(is.na(flow_model_bc_tri$`Station ID`),flow_model_bc_tri$`Model Location Name`,paste0(flow_model_bc_tri$`Model Location Name`," (", flow_model_bc_tri$`Station ID`,")")), 
                        labelOptions = labelOptions(textsize = "15px"),
                        popup = ~paste0("Station: ", `Model Location Name`,
                                        '<br>', "Station ID: ", `Station ID`,
                                        '<br>', "Model Location: ", ifelse(`Model Location`== "NA", NA, paste0(round(`Model Location`,1), " ", `Location Units`)),
                                        '<br>', "Model Input Type: ", `Model Location Type`,
                                        '<br>', "Model Input Parameter: ", Parameter,
                                        '<br>', "Data Source: ", `Data Source`),
                        popupOptions = leaflet::popupOptions(maxWidth = 650, maxHeight = 300)) %>% 
      # __ Ind NPDES PS ----
    leaflet::addMarkers(data = ind_ps,
                        group = "Individual NPDES Point Sources",
                        options = leaflet::leafletOptions(pane="marker"),
                        #clusterOptions = markerClusterOptions(),
                        label = paste0(ind_ps$`Facility Name (Facility Number)`),
                        labelOptions = labelOptions(textsize = "15px"),
                        popup = ~paste0("Facility Name (Facility Number): ", ind_ps$`Facility Name (Facility Number)`,
                                        "<br>", 
                                        "Permit Type and Description: ", ind_ps$`Permit Type and Description`,
                                        "<br>",
                                        "Stream/River Mile: ", ind_ps$`Stream/River Mile`),
                        popupOptions = leaflet::popupOptions(maxWidth = 650, maxHeight = 300)) %>% 
      # __ Gen NPDES PS (GEN01, GEN03, GEN04, GEN05) ----
    leaflet::addMarkers(data = gen_ps,
                        group = "General NPDES Point Sources (GEN01, GEN03, GEN04, GEN05, GEN19, or GEN40)",
                        options = leaflet::leafletOptions(pane="marker"),
                        #clusterOptions = markerClusterOptions(),
                        label = paste0(gen_ps$`Facility Name (Facility Number)`),
                        labelOptions = labelOptions(textsize = "15px"),
                        popup = ~paste0("Facility Name (Facility Number): ", gen_ps$`Facility Name (Facility Number)`,
                                        "<br>", 
                                        "Permit Type and Description: ", gen_ps$`Permit Type and Description`,
                                        "<br>",
                                        "Stream/River Mile: ", gen_ps$`Stream/River Mile`),
                        popupOptions = leaflet::popupOptions(maxWidth = 650, maxHeight = 300)) %>% 
      leaflet::addLayersControl(overlayGroups = c("Heat Source Temperature Model Extent (Existing Models)",
                                                  "Heat Source Temperature Model Extent (New Models)",
                                                  "Stream Temperature Stations",
                                                  "Stream Temperature Calibration Sites",
                                                  "Stream Temperature Model Boundary Conditions and Tributary Inputs",
                                                  "Stream Flow Stations",
                                                  "Stream Flow Model Boundary Conditions and Tributary Inputs",
                                                  "Meteorological Stations",
                                                  "Individual NPDES Point Sources",
                                                  "General NPDES Point Sources (GEN01, GEN03, GEN04, GEN05, GEN19, or GEN40)",
                                                  "HUC8","HUC10","HUC12",
                                                  "2018/2020 IR Temperature Status - Streams",
                                                  "2018/2020 IR Temperature Status - Waterbodies",
                                                  "2018/2020 IR Temperature Status - Watershed",
                                                  "Fish Use Designations",
                                                  "Salmon and Steelhead Spawning Use Designations",
                                                  "Oregon Imagery"),
                                options = leaflet::layersControlOptions(collapsed = FALSE, autoZIndex = TRUE)) %>% 
      leaflet::hideGroup(c("Heat Source Temperature Model Extent (New Models)",
                           "Stream Temperature Stations",
                           "Stream Temperature Calibration Sites",
                           "Stream Temperature Model Boundary Conditions and Tributary Inputs",
                           "Stream Flow Stations",
                           "Stream Flow Model Boundary Conditions and Tributary Inputs",
                           "Meteorological Stations",
                           "Individual NPDES Point Sources",
                           "General NPDES Point Sources (GEN01, GEN03, GEN04, GEN05, GEN19, or GEN40)",
                           "HUC8","HUC10","HUC12",
                           "2018/2020 IR Temperature Status - Streams",
                           "2018/2020 IR Temperature Status - Waterbodies",
                           "2018/2020 IR Temperature Status - Watershed",
                           "Fish Use Designations",
                           "Salmon and Steelhead Spawning Use Designations",
                           "Oregon Imagery"))
    
  }
  
  # South Umpqua and Umpqua Subbasins ----
  if(qapp_project_area == "South Umpqua and Umpqua Subbasins") {
    map_area <- map_basic %>% 
      # __ hs_temp_model_extent ----
    leaflet::addPolylines(data = hs_temp_model_extent,
                          group = "Heat Source Temperature Model Extent",
                          options = leaflet::leafletOptions(pane="mod"),
                          label = ~Stream,
                          labelOptions = labelOptions(style = list("color" = "black",
                                                                   "font-size" = "20px")),
                          color = "#045a8d",
                          opacity = 1,
                          weight = 4) %>% 
      # __ Temp Calibration Sites ----
    leaflet::addMarkers(data = temp_cal_sites,
                        group = "Stream Temperature Calibration Sites",
                        options = leaflet::leafletOptions(pane="marker"),
                        #clusterOptions = markerClusterOptions(),
                        label = ifelse(is.na(temp_cal_sites$`Station ID`),temp_cal_sites$`Model Location Name`,paste0(temp_cal_sites$`Model Location Name`," (", temp_cal_sites$`Station ID`,")")), 
                        labelOptions = labelOptions(textsize = "15px"),
                        popup = ~paste0("Station: ", `Model Location Name`,
                                        '<br>', "Station ID: ", `Station ID`,
                                        '<br>', "Model Location: ", ifelse(`Model Location`== "NA", NA, paste0(round(`Model Location`,1), " ", `Location Units`)),
                                        '<br>', "Calibration Parameter: ", Parameter,
                                        '<br>', "Data Source: ", `Data Source`),
                        popupOptions = leaflet::popupOptions(maxWidth = 650, maxHeight = 300)) %>% 
      # __ Temp Model Boundary Conditions and Tributary Inputs ----
    leaflet::addMarkers(data = temp_model_bc_tri,
                        group = "Stream Temperature Model Boundary Conditions and Tributary Inputs",
                        options = leaflet::leafletOptions(pane="marker"),
                        #clusterOptions = markerClusterOptions(),
                        label = ifelse(is.na(temp_model_bc_tri$`Station ID`),temp_model_bc_tri$`Model Location Name`,paste0(temp_model_bc_tri$`Model Location Name`," (", temp_model_bc_tri$`Station ID`,")")), 
                        labelOptions = labelOptions(textsize = "15px"),
                        popup = ~paste0("Station: ", `Model Location Name`,
                                        '<br>', "Station ID: ", `Station ID`,
                                        '<br>', "Model Location: ", ifelse(`Model Location`== "NA", NA, paste0(round(`Model Location`,1), " ", `Location Units`)),
                                        '<br>', "Model Input Type: ", `Model Location Type`,
                                        '<br>', "Model Input Parameter: ", Parameter,
                                        '<br>', "Data Source: ", `Data Source`),
                        popupOptions = leaflet::popupOptions(maxWidth = 650, maxHeight = 300)) %>% 
      # __ Flow Model Boundary Conditions and Tributary Inputs ----
    leaflet::addMarkers(data = flow_model_bc_tri,
                        group = "Stream Flow Model Boundary Conditions and Tributary Inputs",
                        options = leaflet::leafletOptions(pane="marker"),
                        #clusterOptions = markerClusterOptions(),
                        label = ifelse(is.na(flow_model_bc_tri$`Station ID`),flow_model_bc_tri$`Model Location Name`,paste0(flow_model_bc_tri$`Model Location Name`," (", flow_model_bc_tri$`Station ID`,")")), 
                        labelOptions = labelOptions(textsize = "15px"),
                        popup = ~paste0("Station: ", `Model Location Name`,
                                        '<br>', "Station ID: ", `Station ID`,
                                        '<br>', "Model Location: ", ifelse(`Model Location`== "NA", NA, paste0(round(`Model Location`,1), " ", `Location Units`)),
                                        '<br>', "Model Input Type: ", `Model Location Type`,
                                        '<br>', "Model Input Parameter: ", Parameter,
                                        '<br>', "Data Source: ", `Data Source`),
                        popupOptions = leaflet::popupOptions(maxWidth = 650, maxHeight = 300)) %>% 
      # __ Ind NPDES PS ----
    leaflet::addMarkers(data = ind_ps,
                        group = "Individual NPDES Point Sources",
                        options = leaflet::leafletOptions(pane="marker"),
                        #clusterOptions = markerClusterOptions(),
                        label = paste0(ind_ps$`Facility Name (Facility Number)`),
                        labelOptions = labelOptions(textsize = "15px"),
                        popup = ~paste0("Facility Name (Facility Number): ", ind_ps$`Facility Name (Facility Number)`,
                                        "<br>", 
                                        "Permit Type and Description: ", ind_ps$`Permit Type and Description`,
                                        "<br>",
                                        "Stream/River Mile: ", ind_ps$`Stream/River Mile`),
                        popupOptions = leaflet::popupOptions(maxWidth = 650, maxHeight = 300)) %>% 
      # __ Gen NPDES PS (GEN01, GEN03, GEN04, GEN05) ----
    leaflet::addMarkers(data = gen_ps,
                        group = "General NPDES Point Sources (GEN01, GEN03, GEN04, GEN05, GEN19, or GEN40)",
                        options = leaflet::leafletOptions(pane="marker"),
                        #clusterOptions = markerClusterOptions(),
                        label = paste0(gen_ps$`Facility Name (Facility Number)`),
                        labelOptions = labelOptions(textsize = "15px"),
                        popup = ~paste0("Facility Name (Facility Number): ", gen_ps$`Facility Name (Facility Number)`,
                                        "<br>", 
                                        "Permit Type and Description: ", gen_ps$`Permit Type and Description`,
                                        "<br>",
                                        "Stream/River Mile: ", gen_ps$`Stream/River Mile`),
                        popupOptions = leaflet::popupOptions(maxWidth = 650, maxHeight = 300)) %>% 
      leaflet::addLayersControl(overlayGroups = c("Heat Source Temperature Model Extent",
                                                  "Stream Temperature Stations",
                                                  "Stream Temperature Calibration Sites",
                                                  "Stream Temperature Model Boundary Conditions and Tributary Inputs",
                                                  "Stream Flow Stations",
                                                  "Stream Flow Model Boundary Conditions and Tributary Inputs",
                                                  "Meteorological Stations",
                                                  "Individual NPDES Point Sources",
                                                  "General NPDES Point Sources (GEN01, GEN03, GEN04, GEN05, GEN19, or GEN40)",
                                                  "HUC8","HUC10","HUC12",
                                                  "2018/2020 IR Temperature Status - Streams",
                                                  "2018/2020 IR Temperature Status - Waterbodies",
                                                  "2018/2020 IR Temperature Status - Watershed",
                                                  "Fish Use Designations",
                                                  "Salmon and Steelhead Spawning Use Designations",
                                                  "Oregon Imagery"),
                                options = leaflet::layersControlOptions(collapsed = FALSE, autoZIndex = TRUE)) %>% 
      leaflet::hideGroup(c("Stream Temperature Stations",
                           "Stream Temperature Calibration Sites",
                           "Stream Temperature Model Boundary Conditions and Tributary Inputs",
                           "Stream Flow Stations",
                           "Stream Flow Model Boundary Conditions and Tributary Inputs",
                           "Meteorological Stations",
                           "Individual NPDES Point Sources",
                           "General NPDES Point Sources (GEN01, GEN03, GEN04, GEN05, GEN19, or GEN40)",
                           "HUC8","HUC10","HUC12",
                           "2018/2020 IR Temperature Status - Streams",
                           "2018/2020 IR Temperature Status - Waterbodies",
                           "2018/2020 IR Temperature Status - Watershed",
                           "Fish Use Designations",
                           "Salmon and Steelhead Spawning Use Designations",
                           "Oregon Imagery"))
    
  }
  
  # Southern Willamette Subbasins ----
  if(qapp_project_area == "Southern Willamette Subbasins") {
    map_area <- map_basic %>% 
      # __ hs_temp_model_extent ----
    leaflet::addPolylines(data = hs_temp_model_extent,
                          group = "Heat Source Temperature Model Extent",
                          options = leaflet::leafletOptions(pane="mod"),
                          label = ~Stream,
                          labelOptions = labelOptions(style = list("color" = "black",
                                                                   "font-size" = "20px")),
                          color = "#045a8d",
                          opacity = 1,
                          weight = 4) %>% 
      # __ hs_solar_model_area ----
    leaflet::addPolygons(data = hs_solar_model_area,
                         group = "Heat Source Solar Model Extent",
                         options = leaflet::leafletOptions(pane="mod"),
                         label = ~Name,
                         labelOptions = labelOptions(style = list("color" = "black",
                                                                  "font-size" = "20px")),
                         fillColor = "#3690c0",
                         fillOpacity = 0.8,
                         weight = 3,
                         color = "#3690c0",
                         opacity = 1) %>% 
      # __ Temp Calibration Sites ----
    leaflet::addMarkers(data = temp_cal_sites,
                        group = "Stream Temperature Calibration Sites",
                        options = leaflet::leafletOptions(pane="marker"),
                        #clusterOptions = markerClusterOptions(),
                        label = ifelse(is.na(temp_cal_sites$`Station ID`),temp_cal_sites$`Model Location Name`,paste0(temp_cal_sites$`Model Location Name`," (", temp_cal_sites$`Station ID`,")")), 
                        labelOptions = labelOptions(textsize = "15px"),
                        popup = ~paste0("Station: ", `Model Location Name`,
                                        '<br>', "Station ID: ", `Station ID`,
                                        '<br>', "Model Location: ", ifelse(`Model Location`== "NA", NA, paste0(round(`Model Location`,1), " ", `Location Units`)),
                                        '<br>', "Calibration Parameter: ", Parameter,
                                        '<br>', "Data Source: ", `Data Source`),
                        popupOptions = leaflet::popupOptions(maxWidth = 650, maxHeight = 300)) %>% 
      # __ Temp Model Boundary Conditions and Tributary Inputs ----
    leaflet::addMarkers(data = temp_model_bc_tri,
                        group = "Stream Temperature Model Boundary Conditions and Tributary Inputs",
                        options = leaflet::leafletOptions(pane="marker"),
                        #clusterOptions = markerClusterOptions(),
                        label = ifelse(is.na(temp_model_bc_tri$`Station ID`),temp_model_bc_tri$`Model Location Name`,paste0(temp_model_bc_tri$`Model Location Name`," (", temp_model_bc_tri$`Station ID`,")")), 
                        labelOptions = labelOptions(textsize = "15px"),
                        popup = ~paste0("Station: ", `Model Location Name`,
                                        '<br>', "Station ID: ", `Station ID`,
                                        '<br>', "Model Location: ", ifelse(`Model Location`== "NA", NA, paste0(round(`Model Location`,1), " ", `Location Units`)),
                                        '<br>', "Model Input Type: ", `Model Location Type`,
                                        '<br>', "Model Input Parameter: ", Parameter,
                                        '<br>', "Data Source: ", `Data Source`),
                        popupOptions = leaflet::popupOptions(maxWidth = 650, maxHeight = 300)) %>% 
      # __ Flow Model Boundary Conditions and Tributary Inputs ----
    leaflet::addMarkers(data = flow_model_bc_tri,
                        group = "Stream Flow Model Boundary Conditions and Tributary Inputs",
                        options = leaflet::leafletOptions(pane="marker"),
                        #clusterOptions = markerClusterOptions(),
                        label = ifelse(is.na(flow_model_bc_tri$`Station ID`),flow_model_bc_tri$`Model Location Name`,paste0(flow_model_bc_tri$`Model Location Name`," (", flow_model_bc_tri$`Station ID`,")")), 
                        labelOptions = labelOptions(textsize = "15px"),
                        popup = ~paste0("Station: ", `Model Location Name`,
                                        '<br>', "Station ID: ", `Station ID`,
                                        '<br>', "Model Location: ", ifelse(`Model Location`== "NA", NA, paste0(round(`Model Location`,1), " ", `Location Units`)),
                                        '<br>', "Model Input Type: ", `Model Location Type`,
                                        '<br>', "Model Input Parameter: ", Parameter,
                                        '<br>', "Data Source: ", `Data Source`),
                        popupOptions = leaflet::popupOptions(maxWidth = 650, maxHeight = 300)) %>% 
      # __ Ind NPDES PS ----
    leaflet::addMarkers(data = ind_ps,
                        group = "Individual NPDES Point Sources",
                        options = leaflet::leafletOptions(pane="marker"),
                        #clusterOptions = markerClusterOptions(),
                        label = paste0(ind_ps$`Facility Name (Facility Number)`),
                        labelOptions = labelOptions(textsize = "15px"),
                        popup = ~paste0("Facility Name (Facility Number): ", ind_ps$`Facility Name (Facility Number)`,
                                        "<br>", 
                                        "Permit Type and Description: ", ind_ps$`Permit Type and Description`,
                                        "<br>",
                                        "Stream/River Mile: ", ind_ps$`Stream/River Mile`),
                        popupOptions = leaflet::popupOptions(maxWidth = 650, maxHeight = 300)) %>% 
      # __ Gen NPDES PS (GEN01, GEN03, GEN04, GEN05) ----
    leaflet::addMarkers(data = gen_ps,
                        group = "General NPDES Point Sources (GEN01, GEN03, GEN04, GEN05, GEN19, or GEN40)",
                        options = leaflet::leafletOptions(pane="marker"),
                        #clusterOptions = markerClusterOptions(),
                        label = paste0(gen_ps$`Facility Name (Facility Number)`),
                        labelOptions = labelOptions(textsize = "15px"),
                        popup = ~paste0("Facility Name (Facility Number): ", gen_ps$`Facility Name (Facility Number)`,
                                        "<br>", 
                                        "Permit Type and Description: ", gen_ps$`Permit Type and Description`,
                                        "<br>",
                                        "Stream/River Mile: ", gen_ps$`Stream/River Mile`),
                        popupOptions = leaflet::popupOptions(maxWidth = 650, maxHeight = 300)) %>% 
      leaflet::addLayersControl(overlayGroups = c("Heat Source Temperature Model Extent",
                                                  "Heat Source Solar Model Extent",
                                                  "Stream Temperature Stations",
                                                  "Stream Temperature Calibration Sites",
                                                  "Stream Temperature Model Boundary Conditions and Tributary Inputs",
                                                  "Stream Flow Stations",
                                                  "Stream Flow Model Boundary Conditions and Tributary Inputs",
                                                  "Meteorological Stations",
                                                  "Individual NPDES Point Sources",
                                                  "General NPDES Point Sources (GEN01, GEN03, GEN04, GEN05, GEN19, or GEN40)",
                                                  "HUC8","HUC10","HUC12",
                                                  "2018/2020 IR Temperature Status - Streams",
                                                  "2018/2020 IR Temperature Status - Waterbodies",
                                                  "2018/2020 IR Temperature Status - Watershed",
                                                  "Fish Use Designations",
                                                  "Salmon and Steelhead Spawning Use Designations",
                                                  "Oregon Imagery"),
                                options = leaflet::layersControlOptions(collapsed = FALSE, autoZIndex = TRUE)) %>% 
      leaflet::hideGroup(c("Heat Source Solar Model Extent",
                           "Stream Temperature Stations",
                           "Stream Temperature Calibration Sites",
                           "Stream Temperature Model Boundary Conditions and Tributary Inputs",
                           "Stream Flow Stations",
                           "Stream Flow Model Boundary Conditions and Tributary Inputs",
                           "Meteorological Stations",
                           "Individual NPDES Point Sources",
                           "General NPDES Point Sources (GEN01, GEN03, GEN04, GEN05, GEN19, or GEN40)",
                           "HUC8","HUC10","HUC12",
                           "2018/2020 IR Temperature Status - Streams",
                           "2018/2020 IR Temperature Status - Waterbodies",
                           "2018/2020 IR Temperature Status - Watershed",
                           "Fish Use Designations",
                           "Salmon and Steelhead Spawning Use Designations",
                           "Oregon Imagery"))
    
  }
  
  # Walla Walla Subbasin ----
  if(qapp_project_area == "Walla Walla Subbasin") {
    map_area <- map_basic %>% 
      # __ hs_temp_model_extent ----
    leaflet::addPolylines(data = hs_temp_model_extent,
                          group = "Heat Source Temperature Model Extent",
                          options = leaflet::leafletOptions(pane="mod"),
                          label = ~Stream,
                          labelOptions = labelOptions(style = list("color" = "black",
                                                                   "font-size" = "20px")),
                          color = "#045a8d",
                          opacity = 1,
                          weight = 4) %>% 
      # __ hs_solar_model_extent ----
    leaflet::addPolylines(data = hs_solar_model_extent,
                          group = "Heat Source Solar Model Extent",
                          options = leaflet::leafletOptions(pane="mod"),
                          label = ~Stream,
                          labelOptions = labelOptions(style = list("color" = "black",
                                                                   "font-size" = "20px")),
                          color = "#3690c0",
                          opacity = 1,
                          weight = 4) %>% 
      # __ Temp Calibration Sites ----
    leaflet::addMarkers(data = temp_cal_sites,
                        group = "Stream Temperature Calibration Sites",
                        options = leaflet::leafletOptions(pane="marker"),
                        #clusterOptions = markerClusterOptions(),
                        label = ifelse(is.na(temp_cal_sites$`Station ID`),temp_cal_sites$`Model Location Name`,paste0(temp_cal_sites$`Model Location Name`," (", temp_cal_sites$`Station ID`,")")), 
                        labelOptions = labelOptions(textsize = "15px"),
                        popup = ~paste0("Station: ", `Model Location Name`,
                                        '<br>', "Station ID: ", `Station ID`,
                                        '<br>', "Model Location: ", ifelse(`Model Location`== "NA", NA, paste0(round(`Model Location`,1), " ", `Location Units`)),
                                        '<br>', "Calibration Parameter: ", Parameter,
                                        '<br>', "Data Source: ", `Data Source`),
                        popupOptions = leaflet::popupOptions(maxWidth = 650, maxHeight = 300)) %>% 
      leaflet::addLayersControl(overlayGroups = c("Heat Source Temperature Model Extent",
                                                  "Heat Source Solar Model Extent",
                                                  "Stream Temperature Stations",
                                                  "Stream Temperature Calibration Sites",
                                                  "Stream Flow Stations",
                                                  "Meteorological Stations",
                                                  "HUC8","HUC10","HUC12",
                                                  "2018/2020 IR Temperature Status - Streams",
                                                  "2018/2020 IR Temperature Status - Waterbodies",
                                                  "2018/2020 IR Temperature Status - Watershed",
                                                  "Fish Use Designations",
                                                  "Salmon and Steelhead Spawning Use Designations",
                                                  "Oregon Imagery"),
                                options = leaflet::layersControlOptions(collapsed = FALSE, autoZIndex = TRUE)) %>% 
      leaflet::hideGroup(c("Heat Source Solar Model Extent",
                           "Stream Temperature Stations",
                           "Stream Temperature Calibration Sites",
                           "Stream Flow Stations",
                           "Meteorological Stations",
                           "HUC8","HUC10","HUC12",
                           "2018/2020 IR Temperature Status - Streams",
                           "2018/2020 IR Temperature Status - Waterbodies",
                           "2018/2020 IR Temperature Status - Watershed",
                           "Fish Use Designations",
                           "Salmon and Steelhead Spawning Use Designations",
                           "Oregon Imagery"))
    
  }
  
  # Willamette River Mainstem and Major Tributaries ----
  if(qapp_project_area == "Willamette River Mainstem and Major Tributaries") {
    map_area <- map_basic %>% 
      # __ ce_model_extent ----
    leaflet::addPolylines(data = ce_model_extent,
                          group = "CE-QUAL-W2 Temperature Model Extent",
                          options = leaflet::leafletOptions(pane="mod"),
                          label = ~Stream,
                          labelOptions = labelOptions(style = list("color" = "black",
                                                                   "font-size" = "20px")),
                          color = "#8c510a",
                          opacity = 1,
                          weight = 4,
                          fill=FALSE) %>% 
      # __ Gage Height Stations----
    leaflet::addMarkers(data = gage_height_stations_map,
                        group = "Gage Height Stations",
                        options = leaflet::leafletOptions(pane="marker"),
                        #clusterOptions = markerClusterOptions(),
                        label = paste0("USGS: ", gage_height_stations_map$Station, " (", gage_height_stations_map$`Station ID`,")"),
                        labelOptions = labelOptions(textsize = "15px"),
                        popup = ~paste0("<b>", 
                                        "USGS Station Name: ",
                                        gage_height_stations_map$Station,"<br>",
                                        "Station ID: ", gage_height_stations_map$`Station ID`,
                                        #"<br>",
                                        #"<br>",
                                        sapply(gage_height_stations_map$Station, 
                                               popupTable.gageHeight, USE.NAMES = FALSE)),
                        popupOptions = leaflet::popupOptions(maxWidth = 650, maxHeight = 300)) %>% 
      # __ Ind NPDES PS ----
    leaflet::addMarkers(data = ind_ps,
                        group = "Individual NPDES Point Sources",
                        options = leaflet::leafletOptions(pane="marker"),
                        #clusterOptions = markerClusterOptions(),
                        label = paste0(ind_ps$`Facility Name and Number`),
                        labelOptions = labelOptions(textsize = "15px"),
                        popup = ~paste0("Facility Name and Number: ", ind_ps$`Facility Name and Number`,
                                        "<br>", 
                                        "Permit Type and Description: ", ind_ps$`Permit Type and Description`,
                                        "<br>",
                                        "Stream River Mile: ", ind_ps$`Stream River Mile`),
                        popupOptions = leaflet::popupOptions(maxWidth = 650, maxHeight = 300)) %>% 
      # __ Gen NPDES PS (GEN01, GEN03, GEN04, GEN05) ----
    leaflet::addMarkers(data = gen_ps,
                        group = "General NPDES Point Sources (GEN01, GEN03, GEN04, GEN05, GEN19, or GEN40)",
                        options = leaflet::leafletOptions(pane="marker"),
                        #clusterOptions = markerClusterOptions(),
                        label = paste0(gen_ps$`Facility Name and Number`),
                        labelOptions = labelOptions(textsize = "15px"),
                        popup = ~paste0("Facility Name and Number: ", gen_ps$`Facility Name and Number`,
                                        "<br>", 
                                        "Permit Type and Description: ", gen_ps$`Permit Type and Description`,
                                        "<br>",
                                        "Stream River Mile: ", gen_ps$`Stream River Mile`),
                        popupOptions = leaflet::popupOptions(maxWidth = 650, maxHeight = 300)) %>% 
      leaflet::addLayersControl(overlayGroups = c("CE-QUAL-W2 Temperature Model Extent",
                                                  "Stream Temperature Stations",
                                                  "Stream Flow Stations",
                                                  "Gage Height Stations",
                                                  "Meteorological Stations",
                                                  "Individual NPDES Point Sources",
                                                  "General NPDES Point Sources (GEN01, GEN03, GEN04, GEN05, GEN19, or GEN40)",
                                                  "HUC8","HUC10","HUC12",
                                                  "2018/2020 IR Temperature Status - Streams",
                                                  "2018/2020 IR Temperature Status - Waterbodies",
                                                  "2018/2020 IR Temperature Status - Watershed",
                                                  "Fish Use Designations",
                                                  "Salmon and Steelhead Spawning Use Designations",
                                                  "Oregon Imagery"),
                                options = leaflet::layersControlOptions(collapsed = FALSE, autoZIndex = TRUE)) %>% 
      leaflet::hideGroup(c("Heat Source Solar Model Extent",
                           "Stream Temperature Stations",
                           "Stream Flow Stations",
                           "Gage Height Stations",
                           "Meteorological Stations",
                           "Individual NPDES Point Sources",
                           "General NPDES Point Sources (GEN01, GEN03, GEN04, GEN05, GEN19, or GEN40)",
                           "HUC8","HUC10","HUC12",
                           "2018/2020 IR Temperature Status - Streams",
                           "2018/2020 IR Temperature Status - Waterbodies",
                           "2018/2020 IR Temperature Status - Watershed",
                           "Fish Use Designations",
                           "Salmon and Steelhead Spawning Use Designations",
                           "Oregon Imagery"))
    
  }
  
  # Willow Creek Subbasin ----
  if(qapp_project_area == "Willow Creek Subbasin") {
    map_area <- map_basic %>% 
      # __ hs_temp_model_extent ----
    leaflet::addPolylines(data = hs_temp_model_extent,
                          group = "Heat Source Temperature Model Extent",
                          options = leaflet::leafletOptions(pane="mod"),
                          label = ~Stream,
                          labelOptions = labelOptions(style = list("color" = "black",
                                                                   "font-size" = "20px")),
                          color = "#045a8d",
                          opacity = 1,
                          weight = 4) %>% 
      # __ Temp Calibration Sites ----
    leaflet::addMarkers(data = temp_cal_sites,
                        group = "Stream Temperature Calibration Sites",
                        options = leaflet::leafletOptions(pane="marker"),
                        #clusterOptions = markerClusterOptions(),
                        label = ifelse(is.na(temp_cal_sites$`Station ID`),temp_cal_sites$`Model Location Name`,paste0(temp_cal_sites$`Model Location Name`," (", temp_cal_sites$`Station ID`,")")), 
                        labelOptions = labelOptions(textsize = "15px"),
                        popup = ~paste0("Station: ", `Model Location Name`,
                                        '<br>', "Station ID: ", `Station ID`,
                                        '<br>', "Model Location: ", ifelse(`Model Location`== "NA", NA, paste0(round(`Model Location`,1), " ", `Location Units`)),
                                        '<br>', "Calibration Parameter: ", Parameter,
                                        '<br>', "Data Source: ", `Data Source`),
                        popupOptions = leaflet::popupOptions(maxWidth = 650, maxHeight = 300)) %>% 
      # __ Temp Model Boundary Conditions and Tributary Inputs ----
    leaflet::addMarkers(data = temp_model_bc_tri,
                        group = "Stream Temperature Model Boundary Conditions and Tributary Inputs",
                        options = leaflet::leafletOptions(pane="marker"),
                        #clusterOptions = markerClusterOptions(),
                        label = ifelse(is.na(temp_model_bc_tri$`Station ID`),temp_model_bc_tri$`Model Location Name`,paste0(temp_model_bc_tri$`Model Location Name`," (", temp_model_bc_tri$`Station ID`,")")), 
                        labelOptions = labelOptions(textsize = "15px"),
                        popup = ~paste0("Station: ", `Model Location Name`,
                                        '<br>', "Station ID: ", `Station ID`,
                                        '<br>', "Model Location: ", ifelse(`Model Location`== "NA", NA, paste0(round(`Model Location`,1), " ", `Location Units`)),
                                        '<br>', "Model Input Type: ", `Model Location Type`,
                                        '<br>', "Model Input Parameter: ", Parameter,
                                        '<br>', "Data Source: ", `Data Source`),
                        popupOptions = leaflet::popupOptions(maxWidth = 650, maxHeight = 300)) %>% 
      # __ Ind NPDES PS ----
    leaflet::addMarkers(data = ind_ps,
                        group = "Individual NPDES Point Sources",
                        options = leaflet::leafletOptions(pane="marker"),
                        #clusterOptions = markerClusterOptions(),
                        label = paste0(ind_ps$`Facility Name (Facility Number)`),
                        labelOptions = labelOptions(textsize = "15px"),
                        popup = ~paste0("Facility Name (Facility Number): ", ind_ps$`Facility Name (Facility Number)`,
                                        "<br>", 
                                        "Permit Type and Description: ", ind_ps$`Permit Type and Description`,
                                        "<br>",
                                        "Stream/River Mile: ", ind_ps$`Stream/River Mile`),
                        popupOptions = leaflet::popupOptions(maxWidth = 650, maxHeight = 300)) %>% 
      leaflet::addLayersControl(overlayGroups = c("Heat Source Temperature Model Extent",
                                                  "Stream Temperature Stations",
                                                  "Stream Temperature Calibration Sites",
                                                  "Stream Temperature Model Boundary Conditions and Tributary Inputs",
                                                  "Stream Flow Stations",
                                                  "Meteorological Stations",
                                                  "Individual NPDES Point Sources",
                                                  "HUC8","HUC10","HUC12",
                                                  "2018/2020 IR Temperature Status - Streams",
                                                  "2018/2020 IR Temperature Status - Waterbodies",
                                                  "2018/2020 IR Temperature Status - Watershed",
                                                  "Fish Use Designations",
                                                  "Salmon and Steelhead Spawning Use Designations",
                                                  "Oregon Imagery"),
                                options = leaflet::layersControlOptions(collapsed = FALSE, autoZIndex = TRUE)) %>% 
      leaflet::hideGroup(c("Stream Temperature Stations",
                           "Stream Temperature Calibration Sites",
                           "Stream Temperature Model Boundary Conditions and Tributary Inputs",
                           "Stream Flow Stations",
                           "Meteorological Stations",
                           "Individual NPDES Point Sources",
                           "HUC8","HUC10","HUC12",
                           "2018/2020 IR Temperature Status - Streams",
                           "2018/2020 IR Temperature Status - Waterbodies",
                           "2018/2020 IR Temperature Status - Watershed",
                           "Fish Use Designations",
                           "Salmon and Steelhead Spawning Use Designations",
                           "Oregon Imagery"))
    
  }
  
  map_area <- map_area %>% 
    leaflet::addEasyButton(leaflet::easyButton(
      position = "topright",
      icon = "fa-align-justify",
      title = "Toggle Layers Control",
      id = 'layerToggle',
      onClick = JS("function(btn, map){
    var elements = document.getElementsByClassName('leaflet-control-layers leaflet-control-layers-expanded leaflet-control');
    var index;
    elements = elements.length ? elements : [elements];
  for (index = 0; index < elements.length; index++) {
    element = elements[index];
    if (isElementHidden(element)) {
      element.style.display = '';
      // If the element is still hidden after removing the inline display
      if (isElementHidden(element)) {
        element.style.display = 'block';
      }
    } else {
      element.style.display = 'none';
    }
  }
  function isElementHidden (element) {
    return window.getComputedStyle(element, null).getPropertyValue('display') === 'none';
  }
               }"
      )
    ))
  
  print(dta.stations.mod)
  
  # SAVE DATA ----
  print(paste0(qapp_project_area,"...Save the map"))
  htmlwidgets::saveWidget(map_area, paste0(map.dir,map.file.name,".html"), 
                          background = "grey", selfcontained = TRUE)
  
#}

# *************** -----
# DO NOT RUN ONLY WHEN CHECKING DATASETS FOR ALL PROJECT AREAS ----
data.dir <- "//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/model_QAPPs/R/data/"
load(paste0("./data/lookup.RData")) #lookup.huc; project.areas

dta.check <- NULL
for (qapp_project_area in project.areas$areas) {
  #qapp_project_area = "Southern Willamette Subbasins"
  
  map.file.name <- paste0("map_", project.areas[which(project.areas$areas == qapp_project_area),]$file.name)
  load(paste0("./data/",map.file.name,".RData")) # data.R
  load(paste0("./data/",map.file.name,"_qapp.RData")) # model_QAPP.Rmd
  
  dta.check.area <- data.frame("Project Area"=qapp_project_area,
                               "hs_temp_model_extent" = nrow(hs_temp_model_extent),
                               "hs_solar_model_extent" = nrow(hs_solar_model_extent),
                               "hs_solar_model_area" = nrow(hs_solar_model_area),
                               "ce_model_extent" = nrow(ce_model_extent),
                               "sh_model_extent" = nrow(sh_model_extent),
                               "temp_stations" = nrow(temp_stations),
                               "temp_cal_sites" = nrow(temp_cal_sites),
                               "temp_model_bc_tri" = nrow(temp_model_bc_tri),
                               "flow_model_bc_tri" = nrow(flow_model_bc_tri),
                               "flow_stations" = nrow (flow_stations),
                               "met_stations" = nrow(met_stations),
                               "ind_ps" = nrow(ind_ps),
                               "gen_ps" = nrow(gen_ps))
  
  dta.stations.mod <- dta.check.area %>% 
    tidyr::pivot_longer(!Project.Area, names_to= "data", values_to = "nrow") %>% 
    dplyr::filter(!nrow == 0)
  
  dta.check <- dplyr::bind_rows(dta.check,dta.check.area)
  
}

temp.dir <- "E:/PROJECTS/20200810_RyanMichie_TempTMDLReplacement/R/temp/"
write.csv(dta.check,paste0(temp.dir,"dta.check.csv"))
