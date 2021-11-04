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

map.dir <-  "//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/model_QAPPs/R/map/area_maps/"

source("map_functions.R")
load(paste0("./data/lookup.RData"))

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

# PROJECT AREA MAPS ----
## for test:
# qapp_project_area = "John Day River Basin"
# qapp_project_area = "Lower Grande Ronde, Imnaha, and Wallowa Subbasins"
qapp_project_area = "Lower Willamette and Clackamas Subbasins"
# qapp_project_area = "Malheur River Subbasins"
# qapp_project_area = "Middle Willamette Subbasins"
# qapp_project_area = "Middle Columbia-Hood, Miles Creeks"
# qapp_project_area = "North Umpqua Subbasin"
# qapp_project_area = "Rogue River Basin"
# qapp_project_area = "Sandy Subbasin"
# qapp_project_area = "South Umpqua and Umpqua Subbasins"
# qapp_project_area = "Southern Willamette Subbasins"
# qapp_project_area = "Walla Walla Subbasin"
# qapp_project_area = "Willamette River Mainstem and Major Tributaries"
# qapp_project_area = "Willow Creek Subbasin"

#for (qapp_project_area in project.areas[which(!project.areas$areas == "Willamette River Mainstem and Major Tributaries"),]$areas) {
  
  map.file.name <- paste0("map_", project.areas[which(project.areas$areas == qapp_project_area),]$file.name)
  load(paste0("./data/",map.file.name,".RData")) # data.R
  load(paste0("./data/",map.file.name,"_qapp.RData")) # model_QAPP.Rmd
  pro.area.extent <- unlist(strsplit(project.areas[which(project.areas$areas == qapp_project_area),]$huc8.extent, split = ","))
  subbasin_huc8 <- sort(unique(lookup.huc[which(lookup.huc$QAPP_Project_Area == qapp_project_area),]$HUC_8))
  subbasin_huc10 <- sort(unique(lookup.huc[which(lookup.huc$QAPP_Project_Area == qapp_project_area),]$HUC10))
  subbasin_huc12 <- sort(unique(lookup.huc[which(lookup.huc$QAPP_Project_Area == qapp_project_area),]$HUC12))
  
  # where clause ----
  ## used in querying the feature layers from the REST Server
  where_huc8 <- ""
  where_huc10 <- ""
  where_huc12 <- ""
  
  for(x in 1:(length(subbasin_huc8)-1)){
    where_next_8 <- paste0("HUC8 = '", subbasin_huc8[x], "' OR ")
    where_huc8 <- paste0(where_huc8, where_next_8)}
  
  for(y in 1:(length(subbasin_huc10)-1)){
    where_next_10 <- paste0("HUC10 = '", subbasin_huc10[y], "' OR ")
    where_huc10 <- paste0(where_huc10, where_next_10)}
  
  for(z in 1:(length(subbasin_huc12)-1)){
    where_next_12 <- paste0("HUC12 = '", subbasin_huc12[z], "' OR ")
    where_huc12 <- paste0(where_huc12, where_next_12)}
  
  where_last_8 <- paste0("HUC8 = '", last(subbasin_huc8), "'")
  where_huc8 <- paste0(where_huc8,where_last_8)
  where_last_10 <- paste0("HUC10 = '", last(subbasin_huc10), "'")
  where_huc10 <- paste0(where_huc10,where_last_10)
  where_last_12 <- paste0("HUC12 = '", last(subbasin_huc12), "'")
  where_huc12 <- paste0(where_huc12,where_last_12)
  
  #Use this line to check between the REST map and the QAPP table; if both are matched, use QAPP IR table to pull data to the map
  #IR_where <- paste0("(Char_Name = 'Temperature' AND IR_category = 'Category 5') AND (", where_huc12, ")") 
  
  where_au <- ""
  for(i in 1:(length(pro.cat.45.tbl$AU_ID)-1)){
    where_au_next <- paste0("AU_ID = '", pro.cat.45.tbl$AU_ID[i], "' OR ")
    where_au <- paste0(where_au, where_au_next)}
  where_au_last <- paste0("AU_ID = '", last(pro.cat.45.tbl$AU_ID), "'")
  where_au <- paste0(where_au,where_au_last)
  IR_where <- paste0("(Char_Name = 'Temperature' AND IR_category = 'Category 5') AND (", where_au, ")")
  
  reachcode <- ""
  for(huc_8 in subbasin_huc8){
    query_min <- paste0(huc_8,"000000")
    query_max <- paste0(huc_8,"999999")
    reachcode_next <- paste0("(ReachCode >= ", query_min, " AND ReachCode <= ", query_max, ") OR ")
    reachcode <- paste0(reachcode,reachcode_next)
  }
  reachcode_last <- paste0("(ReachCode >= ", query_min, " AND ReachCode <= ", query_max, ")")
  reachcode <- paste0(reachcode,reachcode_last)
  WQS_reachcode <- paste0("(Temperature_Spawn_dates NOT LIKE '%No Spawning%') AND (",reachcode, ")")
  
  # group names ----
  dta.stations.mod <- data.frame(project_area = qapp_project_area,
                                 data = c("hs_temp_model_extent",
                                          "hs_solar_model_extent",
                                          "hs_solar_model_area",
                                          "ce_model_extent",
                                          "sh_model_extent",
                                          "temp_stations",
                                          "temp_cal_sites",
                                          "temp_model_bc_tri",
                                          "flow_stations",
                                          "flow_model_bc_tri",
                                          "gage_height_stations_map",
                                          "met_stations",
                                          "ind_ps",
                                          "gen_ps"),
                                 NROW = c(nrow(hs_temp_model_extent),
                                          nrow(hs_solar_model_extent),
                                          nrow(hs_solar_model_area),
                                          nrow(ce_model_extent),
                                          nrow(sh_model_extent),
                                          nrow(temp_stations),
                                          nrow(temp_cal_sites),
                                          nrow(temp_model_bc_tri),
                                          nrow(flow_stations),
                                          nrow(flow_model_bc_tri),
                                          nrow(gage_height_stations_map),
                                          nrow(met_stations),
                                          nrow(ind_ps),
                                          nrow(gen_ps)),
                                 group_name = c("Heat Source Temperature Model Extent",
                                                "Heat Source Solar Model Extent",
                                                "Heat Source Solar Model Area",
                                                "CE-QUAL-W2 Temperature Model Extent",
                                                "SHADOW Model Extent",
                                                "Stream Temperature Stations",
                                                "Stream Temperature Calibration Sites",
                                                "Stream Temperature Model Boundary Conditions and Tributary Inputs",
                                                "Stream Flow Stations",
                                                "Stream Flow Model Boundary Conditions and Tributary Inputs",
                                                "Gage Height Stations",
                                                "Meteorological Stations",
                                                "Individual NPDES Point Sources",
                                                "General NPDES Point Sources (GEN01, GEN03, GEN04, GEN05, GEN19, or GEN40)")) %>% 
    dplyr::filter(!NROW == 0)
  
  group.names <- c(dta.stations.mod %>% dplyr::pull(group_name),
                   "HUC8","HUC10","HUC12",
                   "2018/2020 IR Temperature Status - Streams",
                   "2018/2020 IR Temperature Status - Waterbodies",
                   "2018/2020 IR Temperature Status - Watershed",
                   "Fish Use Designations",
                   "Salmon and Steelhead Spawning Use Designations",
                   "Oregon Imagery")
  
  group.names.hide <- c(dta.stations.mod[-1,] %>% dplyr::pull(group_name),
                        "HUC8","HUC10","HUC12",
                        "2018/2020 IR Temperature Status - Streams",
                        "2018/2020 IR Temperature Status - Waterbodies",
                        "2018/2020 IR Temperature Status - Watershed",
                        "Fish Use Designations",
                        "Salmon and Steelhead Spawning Use Designations",
                        "Oregon Imagery")
  
  print(qapp_project_area)
  
  # Basic layers ----
  map.title <- tags$div(tag.map.title, HTML(paste0(qapp_project_area)))
  map_basic <- leaflet::leaflet() %>%
    leaflet::addControl(map.title, position = "topleft", className="map-title") %>% 
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
                                    weight = 3,
                                    color = "red",
                                    opacity = 3,
                                    fillColor = "transparent",
                                    fillOpacity = 0,
                                    labelProperty = htmlwidgets::JS("function(feature){var props = feature.properties; return props.Name+\" \"}"),
                                    labelOptions = leaflet::labelOptions(style = list("color" = "red","font-size" = "20px")),
                                    popupProperty = htmlwidgets::JS(paste0('function(feature){var props = feature.properties; return \"',
                                                                           '<b>Name:</b> \"+props.Name+\"',
                                                                           '<br><b>HUC8:</b> \"+props.HUC8+\"',
                                                                           ' \"}'))) %>% 
    leaflet.esri::addEsriFeatureLayer(url="https://arcgis.deq.state.or.us/arcgis/rest/services/WQ/WBD/MapServer/2",
                                      options = leaflet.esri::featureLayerOptions(where = where_huc10),
                                      group = "HUC10",
                                      pathOptions = leaflet::pathOptions(pane="huc10"),
                                      weight = 2,
                                      color = "orange",
                                      opacity = 1,
                                      fillColor = "transparent",
                                      fillOpacity = 0,
                                      labelProperty = htmlwidgets::JS("function(feature){var props = feature.properties; return props.Name+\" \"}"),
                                      labelOptions = leaflet::labelOptions(style = list("color" = "orange","font-size" = "20px")),
                                      popupProperty = htmlwidgets::JS(paste0('function(feature){var props = feature.properties; return \"',
                                                                             '<b>Name:</b> \"+props.Name+\"',
                                                                             '<br><b>HUC10:</b> \"+props.HUC10+\"',
                                                                             ' \"}'))) %>% 
    leaflet.esri::addEsriFeatureLayer(url="https://arcgis.deq.state.or.us/arcgis/rest/services/WQ/WBD/MapServer/3",
                                      options = leaflet.esri::featureLayerOptions(where = where_huc12),
                                      group = "HUC12",
                                      pathOptions = leaflet::pathOptions(pane="huc12"),
                                      weight = 1,
                                      color = "grey",
                                      opacity = 1,
                                      fillColor = "transparent",
                                      fillOpacity = 0,
                                      labelProperty = htmlwidgets::JS("function(feature){var props = feature.properties; return props.Name+\" \"}"),
                                      labelOptions = leaflet::labelOptions(style = list("color" = "grey","font-size" = "20px")),
                                      popupProperty = htmlwidgets::JS(paste0('function(feature){var props = feature.properties; return \"',
                                                                             '<b>Name:</b> \"+props.Name+\"',
                                                                             '<br><b>HUC12:</b> \"+props.HUC12+\"',
                                                                             ' \"}'))) %>% 
    # __ IR ----
  leaflet.esri::addEsriFeatureLayer(url="https://arcgis.deq.state.or.us/arcgis/rest/services/WQ/IR_201820_byParameter/MapServer/0",
                                    options = leaflet.esri::featureLayerOptions(where = IR_where),
                                    useServiceSymbology = TRUE,
                                    group = "2018/2020 IR Temperature Status - Streams",
                                    pathOptions = leaflet::pathOptions(pane="ir"),
                                    color = "deeppink",
                                    weight = 3,
                                    opacity = 0.8,
                                    fill=FALSE,
                                    highlightOptions = leaflet::highlightOptions(color="red",
                                                                                 weight = 4,
                                                                                 fillOpacity = 0.5,
                                                                                 bringToFront = TRUE,
                                                                                 sendToBack = TRUE),
                                    labelProperty = htmlwidgets::JS("function(feature){var props = feature.properties; return props.AU_Name+\" \"}"),
                                    labelOptions = leaflet::labelOptions(noHide = T,
                                                                         style = list("color" = "deeppink","font-size" = "8px")),
                                    popupProperty = htmlwidgets::JS(paste0('function(feature){var props = feature.properties; return \"',
                                                                           '<b>AU.Name:</b> \"+props.AU_Name+\"',
                                                                           '<br><b>AU.ID:</b> \"+props.AU_ID+\"',
                                                                           '<br><b>Impaired.Parameter:</b> \"+props.Char_Name+\"',
                                                                           '<br><b>IR.Category:</b> \"+props.IR_category+\"',
                                                                           '<br><b>Year.Listed:</b> \"+props.Year_listed+\"',
                                                                           '<br><b>Use.Period:</b> \"+props.Period+\"',
                                                                           '<br><b>HUC12:</b> \"+props.HUC12+\"',
                                                                           ' \"}'))) %>% 
    leaflet.esri::addEsriFeatureLayer(url="https://arcgis.deq.state.or.us/arcgis/rest/services/WQ/IR_201820_byParameter/MapServer/1",
                                      options = leaflet.esri::featureLayerOptions(where = IR_where),
                                      useServiceSymbology = TRUE,
                                      group = "2018/2020 IR Temperature Status - Waterbodies",
                                      pathOptions = leaflet::pathOptions(pane="ir"),
                                      color = "deeppink",
                                      weight = 3,
                                      opacity = 0.8,
                                      fill=TRUE,
                                      fillColor = "deeppink",
                                      fillOpacity = 0.01,
                                      highlightOptions = leaflet::highlightOptions(color="red",
                                                                                   weight = 4,
                                                                                   fillOpacity = 0.5,
                                                                                   bringToFront = TRUE,
                                                                                   sendToBack = TRUE),
                                      labelProperty = htmlwidgets::JS("function(feature){var props = feature.properties; return props.AU_Name+\" \"}"),
                                      labelOptions = leaflet::labelOptions(noHide = T,
                                                                           style = list("color" = "deeppink","font-size" = "8px")),
                                      popupProperty = htmlwidgets::JS(paste0('function(feature){var props = feature.properties; return \"',
                                                                             '<b>AU.Name:</b> \"+props.AU_Name+\"',
                                                                             '<br><b>AU.ID:</b> \"+props.AU_ID+\"',
                                                                             '<br><b>Impaired.Parameter:</b> \"+props.Char_Name+\"',
                                                                             '<br><b>IR.Category:</b> \"+props.IR_category+\"',
                                                                             '<br><b>Year.Listed:</b> \"+props.Year_listed+\"',
                                                                             '<br><b>Use.Period:</b> \"+props.Period+\"',
                                                                             '<br><b>HUC12:</b> \"+props.HUC12+\"',
                                                                             ' \"}'))) %>% 
    leaflet.esri::addEsriFeatureLayer(url="https://arcgis.deq.state.or.us/arcgis/rest/services/WQ/IR_201820_byParameter/MapServer/2",
                                      options = leaflet.esri::featureLayerOptions(where = IR_where),
                                      useServiceSymbology = TRUE,
                                      group = "2018/2020 IR Temperature Status - Watershed",
                                      pathOptions = leaflet::pathOptions(pane="ir"),
                                      color = "deeppink",
                                      weight = 3,
                                      opacity = 0.8,
                                      fill = TRUE,
                                      fillColor = "deeppink",
                                      fillOpacity = 0.01,
                                      highlightOptions = leaflet::highlightOptions(color="red",
                                                                                   weight = 4,
                                                                                   fillOpacity = 0.8,
                                                                                   bringToFront = TRUE,
                                                                                   sendToBack = TRUE),
                                      labelProperty = htmlwidgets::JS("function(feature){var props = feature.properties; return props.AU_Name+\" \"}"),
                                      labelOptions = leaflet::labelOptions(noHide = T,
                                                                           style = list("color" = "deeppink","font-size" = "8px")),
                                      popupProperty = htmlwidgets::JS(paste0('function(feature){var props = feature.properties; return \"',
                                                                             '<b>AU.Name:</b> \"+props.AU_Name+\"',
                                                                             '<br><b>AU.ID:</b> \"+props.AU_ID+\"',
                                                                             '<br><b>Impaired.Parameter:</b> \"+props.Char_Name+\"',
                                                                             '<br><b>IR.Category:</b> \"+props.IR_category+\"',
                                                                             '<br><b>Year.Listed:</b> \"+props.Year_listed+\"',
                                                                             '<br><b>Use.Period:</b> \"+props.Period+\"',
                                                                             '<br><b>HUC12:</b> \"+props.HUC12+\"',
                                                                             ' \"}'))) %>% 
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
                                                                           ' \"}'))) %>% 
    leaflet.esri::addEsriFeatureLayer(url="https://arcgis.deq.state.or.us/arcgis/rest/services/WQ/WQStandards_WM/MapServer/0",
                                      options = leaflet.esri::featureLayerOptions(where = WQS_reachcode),
                                      useServiceSymbology = TRUE,
                                      group = "Salmon and Steelhead Spawning Use Designations",
                                      pathOptions = leaflet::pathOptions(pane="wqs2"),
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
                                                                             ' \"}')))
  
  # Project area layer ----
  # __ John Day River Basin ----
  if(qapp_project_area == "John Day River Basin") {
    map_area <- map_basic %>% 
      hsTempModel(hs_temp_model_extent) %>% 
      tempStation.markers(temp_stations) %>% 
      tempCalibration.markers(temp_cal_sites) %>% 
      tempBoundaryTributary.markers(temp_model_bc_tri) %>% 
      flowStation.markers(flow_stations) %>% 
      flowBoundaryTributary.markers(flow_model_bc_tri) %>% 
      metStation.markders(met_stations) %>% 
      indPS.markers(ind_ps) %>% 
      genPS.markers(gen_ps) %>% 
      leaflet::addLayersControl(overlayGroups = group.names,
                                options = leaflet::layersControlOptions(collapsed = FALSE, autoZIndex = TRUE)) %>% 
      leaflet::hideGroup(group.names.hide)
  }
  
  # __ Lower Grande Ronde, Imnaha, and Wallowa Subbasins ----
  if(qapp_project_area == "Lower Grande Ronde, Imnaha, and Wallowa Subbasins") {
    map_area <- map_basic %>% 
      hsTempModel(hs_temp_model_extent) %>% 
      hsSolarModel(hs_solar_model_extent) %>% 
      tempStation.markers(temp_stations) %>% 
      tempCalibration.markers(temp_cal_sites) %>% 
      tempBoundaryTributary.markers(temp_model_bc_tri) %>% 
      flowStation.markers(flow_stations) %>% 
      metStation.markders(met_stations) %>% 
      indPS.markers(ind_ps) %>% 
      genPS.markers(gen_ps) %>% 
      leaflet::addLayersControl(overlayGroups = group.names,
                                options = leaflet::layersControlOptions(collapsed = FALSE, autoZIndex = TRUE)) %>% 
      leaflet::hideGroup(group.names.hide)
  }
  
  # __ Lower Willamette and Clackamas Subbasins ----
  if(qapp_project_area == "Lower Willamette and Clackamas Subbasins") {
    
    # ____ * New models: BES 2019 ----
    bes_model_extent <- sf::st_read(dsn = "//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/model_QAPPs/R/data/gis/bes_pro_reaches.shp",
                                    layer = "bes_pro_reaches")  %>% 
      sf::st_transform(4326) %>% 
      sf::st_zm() %>% 
      dplyr::rename(Stream = NAME)
    
    map_area <- map_basic %>% 
      # __ BES (2019)
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
      hsTempModel(hs_temp_model_extent) %>% 
      ceModel(ce_model_extent) %>% 
      tempStation.markers(temp_stations) %>% 
      tempCalibration.markers(temp_cal_sites) %>% 
      tempBoundaryTributary.markers(temp_model_bc_tri) %>% 
      flowStation.markers(flow_stations) %>% 
      flowBoundaryTributary.markers(flow_model_bc_tri) %>% 
      metStation.markders(met_stations) %>% 
      indPS.markers(ind_ps) %>% 
      genPS.markers(gen_ps) %>% 
      leaflet::addLayersControl(overlayGroups = c("Heat Source Shade Model Extent (New Models)",
                                                  group.names),
                                options = leaflet::layersControlOptions(collapsed = FALSE, autoZIndex = TRUE)) %>% 
      leaflet::hideGroup(c("Heat Source Shade Model Extent (New Models)",
                           group.names.hide))
  }
  
  # __ Malheur River Subbasins ----
  if(qapp_project_area == "Malheur River Subbasins") {
    map_area <- map_basic %>% 
      hsSolarModel(hs_solar_model_extent) %>% 
      tempStation.markers(temp_stations) %>% 
      tempCalibration.markers(temp_cal_sites) %>% 
      flowStation.markers(flow_stations) %>% 
      metStation.markders(met_stations) %>% 
      indPS.markers(ind_ps) %>% 
      genPS.markers(gen_ps) %>% 
      leaflet::addLayersControl(overlayGroups = group.names,
                                options = leaflet::layersControlOptions(collapsed = FALSE, autoZIndex = TRUE)) %>% 
      leaflet::hideGroup(group.names.hide)
  }
  
  # __ Middle Willamette Subbasins ----
  if(qapp_project_area == "Middle Willamette Subbasins") {
    map_area <- map_basic %>% 
      hsTempModel(hs_temp_model_extent) %>% 
      tempStation.markers(temp_stations) %>% 
      tempCalibration.markers(temp_cal_sites) %>% 
      tempBoundaryTributary.markers(temp_model_bc_tri) %>% 
      flowStation.markers(flow_stations) %>% 
      flowBoundaryTributary.markers(flow_model_bc_tri) %>% 
      metStation.markders(met_stations) %>% 
      indPS.markers(ind_ps) %>% 
      genPS.markers(gen_ps) %>% 
      leaflet::addLayersControl(overlayGroups = group.names,
                                options = leaflet::layersControlOptions(collapsed = FALSE, autoZIndex = TRUE)) %>% 
      leaflet::hideGroup(group.names.hide)
  }
  
  # __ Middle Columbia-Hood, Miles Creeks ----
  if(qapp_project_area == "Middle Columbia-Hood, Miles Creeks") {
    map_area <- map_basic %>% 
      hsTempModel(hs_temp_model_extent) %>% 
      hsSolarModel(hs_solar_model_extent) %>% 
      tempStation.markers(temp_stations) %>% 
      tempCalibration.markers(temp_cal_sites) %>% 
      tempBoundaryTributary.markers(temp_model_bc_tri) %>% 
      flowStation.markers(flow_stations) %>% 
      metStation.markders(met_stations) %>% 
      indPS.markers(ind_ps) %>% 
      leaflet::addLayersControl(overlayGroups = group.names,
                                options = leaflet::layersControlOptions(collapsed = FALSE, autoZIndex = TRUE)) %>% 
      leaflet::hideGroup(group.names.hide)
  }
  
  # __ North Umpqua Subbasin ----
  if(qapp_project_area == "North Umpqua Subbasin") {
    
    # ____ * New models: Fish Creek 2009 ----
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
      # __ Fish Creek (2009)
      leaflet::addPolylines(data = fish_creek_2009,
                            group = "Heat Source Temperature Model Extent (New Models)",
                            options = leaflet::leafletOptions(pane="mod2009"),
                            label = ~Stream,
                            labelOptions = labelOptions(style = list("color" = "black",
                                                                     "font-size" = "20px")),
                            color = "#993404",
                            opacity = 0.5,
                            weight = 10) %>% 
      leaflet::addCircleMarkers(data = n_umpqua_model_nodes,
                                group = "North Umpqua River Model Nodes",
                                options = leaflet::leafletOptions(pane="node"),
                                color = "#e34a33", #red orange
                                stroke = FALSE, 
                                fillOpacity = 0.5,
                                label = ~Location,
                                labelOptions = labelOptions(textsize = "15px")) %>% 
      hsTempModel(hs_temp_model_extent) %>% 
      tempStation.markers(temp_stations) %>% 
      tempCalibration.markers(temp_cal_sites) %>% 
      tempBoundaryTributary.markers(temp_model_bc_tri) %>% 
      flowStation.markers(flow_stations) %>% 
      metStation.markders(met_stations) %>% 
      indPS.markers(ind_ps) %>% 
      genPS.markers(gen_ps) %>% 
      leaflet::addLayersControl(overlayGroups = c("Heat Source Temperature Model Extent (New Models)",
                                                  "North Umpqua River Model Nodes",
                                                  group.names),
                                options = leaflet::layersControlOptions(collapsed = FALSE, autoZIndex = TRUE)) %>% 
      leaflet::hideGroup(c("Heat Source Temperature Model Extent (New Models)",
                           "North Umpqua River Model Nodes",
                           group.names.hide))
  }
  
  # __ Rogue River Basin ----
  if(qapp_project_area == "Rogue River Basin") {
    map_area <- map_basic %>% 
      hsTempModel(hs_temp_model_extent) %>% 
      shModel(sh_model_extent) %>% 
      tempStation.markers(temp_stations) %>% 
      tempCalibration.markers(temp_cal_sites) %>% 
      tempBoundaryTributary.markers(temp_model_bc_tri) %>% 
      flowStation.markers(flow_stations) %>% 
      flowBoundaryTributary.markers(flow_model_bc_tri) %>% 
      metStation.markders(met_stations) %>% 
      indPS.markers(ind_ps) %>% 
      genPS.markers(gen_ps) %>% 
      leaflet::addLayersControl(overlayGroups = group.names,
                                options = leaflet::layersControlOptions(collapsed = FALSE, autoZIndex = TRUE)) %>% 
      leaflet::hideGroup(group.names.hide)
  }
  
  # __ Sandy Subbasin ----
  if(qapp_project_area == "Sandy Subbasin") {
    
    # ____ * New models: Sandy 2016 ----
    sandy_2016 <- sf::st_read(dsn = "//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/model_QAPPs/R/data/gis/sandy_2016.shp",
                              layer = "sandy_2016") %>% 
      sf::st_transform(4326) %>% 
      sf::st_zm()
    
    map_area <- map_basic %>% 
      # __ Bull Run River, Salmon River and Sandy Rivers (2016)
      leaflet::addPolylines(data = sandy_2016,
                            group = "Heat Source Temperature Model Extent (New Models)",
                            options = leaflet::leafletOptions(pane="mod2016"),
                            label = ~Name,
                            labelOptions = labelOptions(style = list("color" = "black",
                                                                     "font-size" = "20px")),
                            color = "#993404",
                            opacity = 0.5,
                            weight = 10) %>% 
      hsTempModel(hs_temp_model_extent) %>% 
      tempStation.markers(temp_stations) %>% 
      tempCalibration.markers(temp_cal_sites) %>% 
      tempBoundaryTributary.markers(temp_model_bc_tri) %>% 
      flowStation.markers(flow_stations) %>% 
      flowBoundaryTributary.markers(flow_model_bc_tri) %>% 
      metStation.markders(met_stations) %>% 
      indPS.markers(ind_ps) %>% 
      genPS.markers(gen_ps) %>% 
      leaflet::addLayersControl(overlayGroups = c("Heat Source Temperature Model Extent (New Models)",
                                                  group.names),
                                options = leaflet::layersControlOptions(collapsed = FALSE, autoZIndex = TRUE)) %>% 
      leaflet::hideGroup(c("Heat Source Temperature Model Extent (New Models)",
                           group.names.hide))
  }
  
  # __ South Umpqua and Umpqua Subbasins ----
  if(qapp_project_area == "South Umpqua and Umpqua Subbasins") {
    map_area <- map_basic %>% 
      hsTempModel(hs_temp_model_extent) %>% 
      tempStation.markers(temp_stations) %>% 
      tempCalibration.markers(temp_cal_sites) %>% 
      tempBoundaryTributary.markers(temp_model_bc_tri) %>% 
      flowStation.markers(flow_stations) %>% 
      flowBoundaryTributary.markers(flow_model_bc_tri) %>% 
      metStation.markders(met_stations) %>% 
      indPS.markers(ind_ps) %>% 
      genPS.markers(gen_ps) %>% 
      leaflet::addLayersControl(overlayGroups = group.names,
                                options = leaflet::layersControlOptions(collapsed = FALSE, autoZIndex = TRUE)) %>% 
      leaflet::hideGroup(group.names.hide)
  }
  
  # __ Southern Willamette Subbasins ----
  if(qapp_project_area == "Southern Willamette Subbasins") {
    map_area <- map_basic %>% 
      hsTempModel(hs_temp_model_extent) %>% 
      hsSolarArea(hs_solar_model_area) %>% 
      tempStation.markers(temp_stations) %>% 
      tempCalibration.markers(temp_cal_sites) %>% 
      tempBoundaryTributary.markers(temp_model_bc_tri) %>% 
      flowStation.markers(flow_stations) %>% 
      flowBoundaryTributary.markers(flow_model_bc_tri) %>% 
      metStation.markders(met_stations) %>% 
      indPS.markers(ind_ps) %>% 
      genPS.markers(gen_ps) %>% 
      leaflet::addLayersControl(overlayGroups = group.names,
                                options = leaflet::layersControlOptions(collapsed = FALSE, autoZIndex = TRUE)) %>% 
      leaflet::hideGroup(group.names.hide)
  }
  
  # __ Walla Walla Subbasin ----
  if(qapp_project_area == "Walla Walla Subbasin") {
    map_area <- map_basic %>% 
      hsTempModel(hs_temp_model_extent) %>% 
      hsSolarModel(hs_solar_model_extent) %>% 
      tempStation.markers(temp_stations) %>% 
      tempCalibration.markers(temp_cal_sites) %>% 
      flowStation.markers(flow_stations) %>% 
      metStation.markders(met_stations) %>% 
      leaflet::addLayersControl(overlayGroups = group.names,
                                options = leaflet::layersControlOptions(collapsed = FALSE, autoZIndex = TRUE)) %>% 
      leaflet::hideGroup(group.names.hide)
  }
  
  # __ Willamette River Mainstem and Major Tributaries ----
  if(qapp_project_area == "Willamette River Mainstem and Major Tributaries") {
    map_area <- map_basic %>% 
      ceModel(ce_model_extent) %>% 
      tempStation.markers(temp_stations) %>% 
      tempCalibration.markers(temp_cal_sites) %>% 
      tempBoundaryTributary.markers(temp_model_bc_tri) %>% 
      flowStation.markers(flow_stations) %>% 
      gageHeight.markers(gage_height_stations_map) %>% 
      metStation.markders(met_stations) %>% 
      indPS.markers(ind_ps) %>% 
      genPS.markers(gen_ps) %>% 
      leaflet::addLayersControl(overlayGroups = group.names,
                                options = leaflet::layersControlOptions(collapsed = FALSE, autoZIndex = TRUE)) %>% 
      leaflet::hideGroup(group.names.hide)
  }
  
  # __ Willow Creek Subbasin ----
  if(qapp_project_area == "Willow Creek Subbasin") {
    map_area <- map_basic %>% 
      hsTempModel(hs_temp_model_extent) %>% 
      tempStation.markers(temp_stations) %>% 
      tempCalibration.markers(temp_cal_sites) %>% 
      tempBoundaryTributary.markers(temp_model_bc_tri) %>% 
      flowStation.markers(flow_stations) %>% 
      metStation.markders(met_stations) %>% 
      indPS.markers(ind_ps) %>% 
      leaflet::addLayersControl(overlayGroups = group.names,
                                options = leaflet::layersControlOptions(collapsed = FALSE, autoZIndex = TRUE)) %>% 
      leaflet::hideGroup(group.names.hide)
  }
  
  # Add-ons ----
  map_final <- map_area %>% 
    # __ Search function ----
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
               }") %>% 
    # __ Toggle buttons ----
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
  )) %>% 
    leaflet::hideGroup(c("Stream Temperature Stations","Stream Flow Stations"))
  
  # SAVE DATA ----
  print(paste0(qapp_project_area,"...Save the map"))
  htmlwidgets::saveWidget(map_final, paste0(map.dir,map.file.name,".html"), 
                          background = "grey", selfcontained = TRUE)
  
#}

# *************** -----
# DO NOT RUN ONLY WHEN CHECKING DATASETS FOR ALL PROJECT AREAS ----
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
                               "flow_stations" = nrow (flow_stations),
                               "flow_model_bc_tri" = nrow(flow_model_bc_tri),
                               "gage_height_stations_map" = nrow(gage_height_stations_map),
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
