############################################################
# Master version is different from the branch versions.    #
# Eg, see Line 27 where is flagged for Yuan's location.    #
# Master version produces area maps in a batch,            #
# while branch versions produce branch maps only.          #
############################################################

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
library(base64enc)

map.dir <-  "//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/model_QAPPs/R/map/area_maps/"
data.dir <- "//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/model_QAPPs/R/data/"
data.dir.yg <- "E:/PROJECTS/20200810_RyanMichie_TempTMDLReplacement/R/branches/" # Yuan's location

source("map_functions.R")
project.areas <- read.csv(paste0(data.dir,"qapp_project_areas.csv"))

#lgnd <- base64enc::base64encode("./fig/legend.png")
logo <- base64enc::base64encode("//deqhq1/WQNPS/Status_and_Trend_Reports/Figures/DEQ-logo-color-non-transp71x107.png")

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
# qapp_project_area = "Lower Willamette and Clackamas Subbasins"          ---X
# qapp_project_area = "Malheur River Subbasins"
# qapp_project_area = "Middle Willamette Subbasins"                       ---X
# qapp_project_area = "Middle Columbia-Hood, Miles Creeks"
# qapp_project_area = "North Umpqua Subbasin"
# qapp_project_area = "Rogue River Basin"
qapp_project_area = "Sandy Subbasin"
# qapp_project_area = "South Umpqua and Umpqua Subbasins"
# qapp_project_area = "Southern Willamette Subbasins"                     ---X
# qapp_project_area = "Walla Walla Subbasin"
# qapp_project_area = "Willamette River Mainstem and Major Tributaries"
# qapp_project_area = "Willow Creek Subbasin"

#for(qapp_project_area in sort(project.areas[which(!project.areas$areas %in% c("Sandy Subbasin",
#                                                                              "Lower Willamette and Clackamas Subbasins",
#                                                                              "Middle Willamette Subbasins",
#                                                                              "Southern Willamette Subbasins")),]$areas)) {
  
  file.name <- project.areas[which(project.areas$areas == qapp_project_area),]$file.name
  
  #load(paste0(data.dir.yg,file.name,"/mQAPPrmd/data/lookup.RData"))
  load(paste0("./data/lookup.RData"))
  
  map.file.name <- paste0("map_", file.name)
  #load(paste0(data.dir.yg,file.name,"/mQAPPrmd/data/",map.file.name,".RData")) # data.R
  #load(paste0(data.dir.yg,file.name,"/mQAPPrmd/data/",map.file.name,"_qapp.RData")) # model_QAPP.Rmd
  load(paste0("./data/",map.file.name,".RData")) # data.R
  load(paste0("./data/",map.file.name,"_qapp.RData")) # model_QAPP.Rmd
  
  pro.area.extent <- unlist(strsplit(project.areas[which(project.areas$areas == qapp_project_area),]$huc8.extent, split = ","))
  subbasin_huc8 <- sort(unique(lookup.huc[which(lookup.huc$QAPP_Project_Area == qapp_project_area),]$HUC_8))
  subbasin_huc10 <- sort(unique(lookup.huc[which(lookup.huc$QAPP_Project_Area == qapp_project_area),]$HUC10))
  subbasin_huc12 <- sort(unique(lookup.huc[which(lookup.huc$QAPP_Project_Area == qapp_project_area),]$HUC12))
  
  # where clause ----
  ## used in querying the feature layers from the REST Server
  
  where_huc8 <- paste0("HUC8 IN ('", paste(subbasin_huc8, collapse = "','"),"')")
  where_huc10 <- paste0("HUC10 IN ('", paste(subbasin_huc10, collapse = "','"),"')")
  where_huc12 <- paste0("HUC12 IN ('", paste(subbasin_huc12, collapse = "','"),"')")
  where_huc12_1 <- paste0("HUC12 IN ('", paste(subbasin_huc12[1:50], collapse = "','"),"')")
  where_huc12_2 <- paste0("HUC12 IN ('", paste(subbasin_huc12[51:100], collapse = "','"),"')")
  where_huc12_3 <- paste0("HUC12 IN ('", paste(subbasin_huc12[101:150], collapse = "','"),"')")
  
  # IR2018/20
  # where_au_yearRound <- paste0("(Char_Name = 'Temperature' AND IR_category IN ('Category 4A','Category 5') AND Period = 'Year Round') AND ",
  #                              "(AU_ID IN ('", paste(tcat45$`Assessment Unit ID`, collapse = "','"),"'))")
  # 
  # where_au_spawning <- paste0("(Char_Name = 'Temperature' AND IR_category IN ('Category 4A','Category 5') AND Period = 'Spawning') AND ",
  #                             "(AU_ID IN ('", paste(tcat45$`Assessment Unit ID`, collapse = "','"),"'))")
  # Update for IR2022
  where_au_yearRound <- paste0("(Pollutant = 'Temperature' AND AU_parameter_category IN ('4A','5') AND period = 'year_round') AND ",
                               "(AU_ID IN ('", paste(tcat45$`Assessment Unit ID`, collapse = "','"),"'))")
  
  where_au_spawning <- paste0("(Pollutant = 'Temperature' AND AU_parameter_category IN ('4A','5') AND period = 'spawn') AND ",
                              "(AU_ID IN ('", paste(tcat45$`Assessment Unit ID`, collapse = "','"),"'))")
  
  if(qapp_project_area == "Willamette River Mainstem and Major Tributaries"){
    
    wms.aus <- readxl::read_xlsx("//deqhq1/tmdl/TMDL_Willamette/Willamette_Mainstem_Temperature_2025/Project_Plans/Willamette_Mainstem_AUs_2022.04.15.xlsx",sheet = "Final_AUs")
    wms.au.id <- wms.aus %>% dplyr::pull(AU_ID)
    where_au_rivers <- paste0("(AU_ID IN ('", paste(wms.au.id, collapse = "','"),"'))")
    where_au_waterbodies <- paste0("(AU_ID IN ('", paste(wms.au.id, collapse = "','"),"'))")
    where_au_watershed <- paste0("(AU_ID IN ('", paste(wms.au.id, collapse = "','"),"'))")
    
  }else{
    
    where_au_rivers <- paste0("(AU_ID IN ('", paste(pro_scope_rivers, collapse = "','"),"'))")
    where_au_waterbodies <- paste0("(AU_ID IN ('", paste(pro_scope_waterbodies, collapse = "','"),"'))")
    where_au_watershed <- paste0("(AU_ID IN ('", paste(pro_scope_watershed, collapse = "','"),"'))") 
    
  }
  
  reachcode <- paste(paste0("(ReachCode >= ", subbasin_huc8, "000000", " AND ReachCode <= ", 
                            subbasin_huc8,"999999)"), 
                     collapse =  " OR ") 
  
  # Data group names ----
  dta.mod <- data.frame(project_area = qapp_project_area,
                        data = c("hs_temp_model_extent",
                                 "hs_solar_model_extent",
                                 "hs_solar_model_area",
                                 "ce_model_extent",
                                 "sh_model_extent"),
                        NROW = c(nrow(hs_temp_model_extent),
                                 nrow(hs_solar_model_extent),
                                 nrow(hs_solar_model_area),
                                 nrow(ce_model_extent),
                                 nrow(sh_model_extent)),
                        group_name = c("Heat Source Temperature Model Extent",
                                       "Heat Source Solar Model Extent",
                                       "Heat Source Solar Model Area",
                                       "CE-QUAL-W2 Temperature Model Extent",
                                       "SHADOW Model Extent")) %>% 
    dplyr::filter(!NROW == 0)
  
  dta.stations <- data.frame(project_area = qapp_project_area,
                             data = c("temp_cal_sites",
                                      "temp_model_bc_tri",
                                      "flow_model_bc_tri",
                                      "temp_stations",
                                      "flow_stations",
                                      "gage_height_stations_map",
                                      "effective.shade.pro.area",
                                      "met_stations",
                                      "ind_ps",
                                      "gen_ps"),
                             NROW = c(nrow(temp_cal_sites),
                                      nrow(temp_model_bc_tri),
                                      nrow(flow_model_bc_tri),
                                      nrow(temp_stations),
                                      nrow(flow_stations),
                                      nrow(gage_height_stations_map),
                                      nrow(effective.shade.pro.area),
                                      nrow(met_stations),
                                      nrow(ind_ps),
                                      nrow(gen_ps)),
                             group_name = c("Model Calibration Sites",
                                            "Model Temperature Boundary Conditions and Tributary Inputs",
                                            "Model Flow Boundary Conditions and Tributary Inputs",
                                            "Stream Temperature Stations",
                                            "Stream Flow Stations",
                                            "Gage Height Stations",
                                            "Effective Shade Measurement Sites",
                                            "Meteorological Stations",
                                            "Individual NPDES Point Sources",
                                            "General NPDES Point Sources (GEN01, GEN03, GEN04, GEN05, GEN19, or GEN40)")) %>% 
    dplyr::filter(!NROW == 0)
  
  dta.stations.mod <- rbind(dta.mod,dta.stations)
  
  # IR group names ----
  # sub.tcat45 <- tcat45 %>% 
  #   dplyr::filter(`Use Period` == "Spawning, Year Round") %>% 
  #   tidyr::separate(col = `Use Period`, into = c("up1","up2"), sep = ", ") %>% 
  #   tidyr::gather(`up`,`Use Period`, `up1`,`up2`) %>% 
  #   dplyr::select(-`up`)
  # 
  # irs <- tcat45 %>%
  #   dplyr::filter(!`Use Period` == "Spawning, Year Round") %>% 
  #   rbind(sub.tcat45) %>% 
  #   dplyr::mutate(ir.grps = ifelse(substr(`Assessment Unit ID`,4,5) == "SR" & `Use Period` == "Year Round","2018/2020 303(d) Temperature Listed - Streams (Year Round Criteria)",
  #                                  ifelse(substr(`Assessment Unit ID`,4,5) == "SR" & `Use Period` == "Spawning","2018/2020 303(d) Temperature Listed - Streams (Spawning Criteria)",
  #                                         ifelse(substr(`Assessment Unit ID`,4,5) %in% c("LK","EB") & `Use Period` == "Year Round","2018/2020 303(d) Temperature Listed - Waterbodies (Year Round Criteria)",
  #                                                ifelse(substr(`Assessment Unit ID`,4,5) %in% c("LK","EB") & `Use Period` == "Spawning","2018/2020 303(d) Temperature Listed - Waterbodies (Spawning Criteria)",
  #                                                       ifelse(substr(`Assessment Unit ID`,4,5) == "WS" & `Use Period` == "Year Round","2018/2020 303(d) Temperature Listed - Watershed (Year Round Criteria)",
  #                                                              ifelse(substr(`Assessment Unit ID`,4,5) == "WS" & `Use Period` == "Spawning","2018/2020 303(d) Temperature Listed - Watershed (Spawning Criteria)",NA)))))))
  
  # Update for IR2022IR
  sub.tcat45 <- tcat45 %>% tidyr::separate(col = `Use Period (Year Listed)`, into = c("up1","up2"), sep = ", ")
  sub.tcat45.1 <- sub.tcat45[c(1:3)] %>% dplyr::rename(`Use Period (Year Listed)` = up1)
  sub.tcat45.2 <- sub.tcat45[c(1,2,4)] %>% dplyr::rename(`Use Period (Year Listed)` = up2)
  
  irs <- rbind(sub.tcat45.1,sub.tcat45.2) %>% 
    dplyr::filter(!is.na(`Use Period (Year Listed)`)) %>% 
    dplyr::mutate(ir.grps = ifelse(substr(`Assessment Unit ID`,4,5) == "SR" & substr(`Use Period (Year Listed)`,1,1) == "Y","2022 303(d) Temperature Listed - Streams (Year Round Criteria)",
                                   ifelse(substr(`Assessment Unit ID`,4,5) == "SR" & substr(`Use Period (Year Listed)`,1,1) == "S","2022 303(d) Temperature Listed - Streams (Spawning Criteria)",
                                          ifelse(substr(`Assessment Unit ID`,4,5) %in% c("LK","EB") & substr(`Use Period (Year Listed)`,1,1) == "Y","2022 303(d) Temperature Listed - Waterbodies (Year Round Criteria)",
                                                 ifelse(substr(`Assessment Unit ID`,4,5) %in% c("LK","EB") & substr(`Use Period (Year Listed)`,1,1) == "S","2022 303(d) Temperature Listed - Waterbodies (Spawning Criteria)",
                                                        ifelse(substr(`Assessment Unit ID`,4,5) == "WS" & substr(`Use Period (Year Listed)`,1,1) == "Y","2022 303(d) Temperature Listed - Watershed (Year Round Criteria)",
                                                               ifelse(substr(`Assessment Unit ID`,4,5) == "WS" & substr(`Use Period (Year Listed)`,1,1) == "S","2022 303(d) Temperature Listed - Watershed (Spawning Criteria)",NA)))))))
  
  
  ir.grps <- sort(unique(irs$ir.grps))
  
  group.names <- c(dta.stations.mod %>% dplyr::pull(group_name),
                   "HUC8","HUC10","HUC12",
                   ir.grps,
                   "Fish Use Designations",
                   "Salmon and Steelhead Spawning Use Designations",
                   "Land Ownership or Jurisdiction",
                   "Stream Names (USGS NHD)",
                   "Oregon Imagery")
  
  group.names.hide <- c(dta.stations.mod %>% dplyr::pull(group_name),
                        "HUC8","HUC10","HUC12",
                        ir.grps,
                        "Fish Use Designations",
                        "Salmon and Steelhead Spawning Use Designations",
                        "Land Ownership or Jurisdiction",
                        "Stream Names (USGS NHD)",
                        "Oregon Imagery")
  
  group.names.search <- dta.stations %>% dplyr::pull(group_name)
  
  print(qapp_project_area)
  
  # Basic layers ----
  #map.title <- tags$div(tag.map.title, HTML(paste0(qapp_project_area)))
  map.title <- tags$div(tag.map.title, HTML("Lower Columbia-Sandy River Subbasin"))
  map_basic <- leaflet::leaflet() %>%
    leaflet::addControl(map.title, position = "topleft", className="map-title") %>% 
    leaflet::addMiniMap(tiles = providers$OpenStreetMap,
                        position = "bottomright",
                        width = 200,
                        height = 150,
                        zoomLevelFixed = 5,
                        toggleDisplay = TRUE,
                        minimized = TRUE) %>% 
    leaflet.extras::addResetMapButton() %>% 
    leaflet::fitBounds(lng1 = pro.area.extent[2], lat1 = pro.area.extent[1],
                       lng2 = pro.area.extent[4], lat2 = pro.area.extent[3]) %>%
    leaflet::addMapPane("OpenStreetMap", zIndex = -20) %>%
    leaflet::addMapPane("aerial", zIndex = -10) %>%
    leaflet::addMapPane("areaOutline", zIndex = 10) %>%
    leaflet::addMapPane("dma", zIndex = 20) %>%
    leaflet::addMapPane("area", zIndex = 30) %>%
    leaflet::addMapPane("hydrotiles", zIndex = 40) %>%
    leaflet::addMapPane("huc8", zIndex = 50) %>%
    leaflet::addMapPane("huc10", zIndex = 60) %>%
    leaflet::addMapPane("huc12", zIndex = 70) %>%
    leaflet::addMapPane("wqs1", zIndex = 80) %>%
    leaflet::addMapPane("wqs2", zIndex = 90) %>%
    leaflet::addMapPane("ir", zIndex = 100) %>%
    leaflet::addMapPane("ir_gnis", zIndex = 110) %>%
    leaflet::addMapPane("mod", zIndex = 120) %>%
    leaflet::addMapPane("modbes", zIndex = 130) %>%
    leaflet::addMapPane("mod2016", zIndex = 140) %>%
    leaflet::addMapPane("mod2009", zIndex = 140) %>%
    leaflet::addMapPane("marker", zIndex = 150) %>%
    leaflet::addProviderTiles(providers$OpenStreetMap, #names(providers) to see a list of the layers
                              options = pathOptions(pane = "OpenStreetMap")) %>% 
    # __ Oregon Imagery ----
  leaflet.esri::addEsriImageMapLayer(url="https://imagery.oregonexplorer.info/arcgis/rest/services/OSIP_2018/OSIP_2018_WM/ImageServer",
                                     group = "Oregon Imagery",
                                     options = leaflet::leafletOptions(pane="aerial")) %>%
    leaflet.esri::addEsriImageMapLayer(url="https://imagery.oregonexplorer.info/arcgis/rest/services/OSIP_2017/OSIP_2017_WM/ImageServer",
                                       group = "Oregon Imagery",
                                       options = leaflet::leafletOptions(pane="aerial")) %>%
    # __ Hydro Tiles ----
  leaflet::addWMSTiles(baseUrl="https://basemap.nationalmap.gov/arcgis/services/USGSHydroCached/MapServer/WmsServer",
                       group = "Stream Names (USGS NHD)",
                       options = leaflet::WMSTileOptions(format = "image/png",
                                                         transparent = TRUE,
                                                         pane= "hydrotiles"),
                       attribution = '<a href="https://basemap.nationalmap.gov/arcgis/rest/services/USGSHydroCached/MapServer">USGS The National Map: National Hydrography Dataset.</a>',
                       layers = "0") %>%
    # __ Project scope ----
  leaflet.esri::addEsriFeatureLayer(url="https://services.arcgis.com/uUvqNMGPm7axC2dD/ArcGIS/rest/services/IR_2022_Final/FeatureServer/34",
                                    options = leaflet.esri::featureLayerOptions(where = where_au_rivers,
                                                                                simplifyFactor = 0.35),
                                    useServiceSymbology = TRUE,
                                    group = "TMDL Project Scope",
                                    pathOptions = leaflet::pathOptions(pane="area"),
                                    color = "#0868ac",
                                    weight = 3,
                                    opacity = 1,
                                    fill = FALSE,
                                    highlightOptions = leaflet::highlightOptions(color="gray",
                                                                                 weight = 4,
                                                                                 fillOpacity = 1,
                                                                                 bringToFront = TRUE,
                                                                                 sendToBack = TRUE),
                                    #labelProperty = htmlwidgets::JS("function(feature){var props = feature.properties; return props.AU_Name+\" \"}"),
                                    labelProperty = htmlwidgets::JS(paste0('function(feature){var props = feature.properties; return \"',
                                                                           '\"+props.AU_ID+\"',
                                                                           '<br>\"+props.AU_Name+\"',
                                                                           ' \"}')),
                                    labelOptions = leaflet::labelOptions(#noHide = T,
                                      style = list("color" = "black","font-size" = "12px")),
                                    popupProperty = htmlwidgets::JS(paste0('function(feature){var props = feature.properties; return \"',
                                                                           '<b>AU ID:</b> \"+props.AU_ID+\"',
                                                                           '<br><b>AU Name:</b> \"+props.AU_Name+\"',
                                                                           ' \"}'))) %>%
    leaflet.esri::addEsriFeatureLayer(url="https://services.arcgis.com/uUvqNMGPm7axC2dD/ArcGIS/rest/services/IR_2022_Final/FeatureServer/43",
                                      options = leaflet.esri::featureLayerOptions(where = where_au_waterbodies),
                                      useServiceSymbology = TRUE,
                                      group = "TMDL Project Scope",
                                      pathOptions = leaflet::pathOptions(pane="area"),
                                      color = "#0868ac",
                                      weight = 0.8,
                                      opacity = 1,
                                      fillColor = "#0868ac",
                                      fillOpacity = 0.8,
                                      highlightOptions = leaflet::highlightOptions(color="gray",
                                                                                   weight = 4,
                                                                                   fill=TRUE,
                                                                                   fillColor = "gray",
                                                                                   fillOpacity = 1,
                                                                                   bringToFront = TRUE,
                                                                                   sendToBack = TRUE),
                                      #labelProperty = htmlwidgets::JS("function(feature){var props = feature.properties; return props.AU_Name+\" \"}"),
                                      labelProperty = htmlwidgets::JS(paste0('function(feature){var props = feature.properties; return \"',
                                                                             '\"+props.AU_ID+\"',
                                                                             '<br>\"+props.AU_Name+\"',
                                                                             ' \"}')),
                                      labelOptions = leaflet::labelOptions(#noHide = T,
                                        style = list("color" = "black","font-size" = "12px")),
                                      popupProperty = htmlwidgets::JS(paste0('function(feature){var props = feature.properties; return \"',
                                                                             '<b>AU ID:</b> \"+props.AU_ID+\"',
                                                                             '<br><b>AU Name:</b> \"+props.AU_Name+\"',
                                                                             ' \"}'))) %>%
    leaflet.esri::addEsriFeatureLayer(url="https://services.arcgis.com/uUvqNMGPm7axC2dD/arcgis/rest/services/OR_AUs_Line_work/FeatureServer/1",
                                      options = leaflet.esri::featureLayerOptions(where = where_au_watershed,
                                                                                  simplifyFactor = 0.35),
                                      useServiceSymbology = TRUE,
                                      group = "TMDL Project Scope",
                                      pathOptions = leaflet::pathOptions(pane="area"),
                                      color = "#0868ac",
                                      weight = 1,
                                      opacity = 1,
                                      fill = FALSE,
                                      highlightOptions = leaflet::highlightOptions(color="gray",
                                                                                   weight = 2,
                                                                                   fillOpacity = 1,
                                                                                   bringToFront = TRUE,
                                                                                   sendToBack = TRUE),
                                      labelProperty = htmlwidgets::JS("function(feature){var props = feature.properties; return props.AU_GNIS+\" \"}"),
                                      labelOptions = leaflet::labelOptions(#noHide = T,
                                        style = list("color" = "black","font-size" = "12px")),
                                      popupProperty = htmlwidgets::JS(paste0('function(feature){var props = feature.properties; return \"',
                                                                             '<b>AU ID; GNIS Name:</b> \"+props.AU_GNIS+\"',
                                                                             '<br><b>AU Name:</b> \"+props.AU_Name+\"',
                                                                             ' \"}'))) %>%
    # __ HUCs ----
  leaflet.esri::addEsriFeatureLayer(url="https://arcgis.deq.state.or.us/arcgis/rest/services/WQ/WBD/MapServer/1",
                                    options = leaflet.esri::featureLayerOptions(where = where_huc8),
                                    group = "HUC8",
                                    pathOptions = leaflet::pathOptions(pane="huc8"),
                                    weight = 4,
                                    color = "black",
                                    opacity = 1,
                                    fillColor = "transparent",
                                    fillOpacity = 0,
                                    highlightOptions = leaflet::highlightOptions(color="black",
                                                                                 weight = 5,
                                                                                 fill=TRUE,
                                                                                 fillColor = "black",
                                                                                 fillOpacity = 0.5,
                                                                                 bringToFront = TRUE,
                                                                                 sendToBack = TRUE),
                                    labelProperty = htmlwidgets::JS("function(feature){var props = feature.properties; return props.Name+\" \"}"),
                                    labelOptions = leaflet::labelOptions(style = list("color" = "black","font-size" = "12px")),
                                    popupProperty = htmlwidgets::JS(paste0('function(feature){var props = feature.properties; return \"',
                                                                           '<b>Name:</b> \"+props.Name+\"',
                                                                           '<br><b>HUC8:</b> \"+props.HUC8+\"',
                                                                           ' \"}'))) %>% 
    leaflet.esri::addEsriFeatureLayer(url="https://arcgis.deq.state.or.us/arcgis/rest/services/WQ/WBD/MapServer/2",
                                      options = leaflet.esri::featureLayerOptions(where = where_huc10),
                                      group = "HUC10",
                                      pathOptions = leaflet::pathOptions(pane="huc10"),
                                      weight = 2,
                                      color = "black",
                                      opacity = 1,
                                      fillColor = "transparent",
                                      fillOpacity = 0,
                                      highlightOptions = leaflet::highlightOptions(color="black",
                                                                                   weight = 3,
                                                                                   fill=TRUE,
                                                                                   fillColor = "black",
                                                                                   fillOpacity = 0.5,
                                                                                   bringToFront = TRUE,
                                                                                   sendToBack = TRUE),
                                      labelProperty = htmlwidgets::JS("function(feature){var props = feature.properties; return props.Name+\" \"}"),
                                      labelOptions = leaflet::labelOptions(style = list("color" = "black","font-size" = "12px")),
                                      popupProperty = htmlwidgets::JS(paste0('function(feature){var props = feature.properties; return \"',
                                                                             '<b>Name:</b> \"+props.Name+\"',
                                                                             '<br><b>HUC10:</b> \"+props.HUC10+\"',
                                                                             ' \"}'))) %>% 
    leaflet.esri::addEsriFeatureLayer(url="https://arcgis.deq.state.or.us/arcgis/rest/services/WQ/WBD/MapServer/3",
                                      options = leaflet.esri::featureLayerOptions(where = where_huc12),
                                      group = "HUC12",
                                      pathOptions = leaflet::pathOptions(pane="huc12"),
                                      weight = 2,
                                      color = "black",
                                      opacity = 1,
                                      fillColor = "transparent",
                                      fillOpacity = 0,
                                      dashArray = c(6,12),
                                      highlightOptions = leaflet::highlightOptions(color="black",
                                                                                   weight = 3,
                                                                                   fill=TRUE,
                                                                                   fillColor = "black",
                                                                                   fillOpacity = 0.5,
                                                                                   bringToFront = TRUE,
                                                                                   sendToBack = TRUE),
                                      labelProperty = htmlwidgets::JS("function(feature){var props = feature.properties; return props.Name+\" \"}"),
                                      labelOptions = leaflet::labelOptions(style = list("color" = "black","font-size" = "12px")),
                                      popupProperty = htmlwidgets::JS(paste0('function(feature){var props = feature.properties; return \"',
                                                                             '<b>Name:</b> \"+props.Name+\"',
                                                                             '<br><b>HUC12:</b> \"+props.HUC12+\"',
                                                                             ' \"}'))) %>%
    # __ IR ----
  # leaflet.esri::addEsriFeatureLayer(#url="https://arcgis.deq.state.or.us/arcgis/rest/services/WQ/IR_201820_byParameter/MapServer/0",
  #                                   url="https://arcgis.deq.state.or.us/arcgis/rest/services/WQ/IR_201820_byParameter/MapServer/0",
  #                                   options = leaflet.esri::featureLayerOptions(where = where_au_yearRound),
  #                                   useServiceSymbology = TRUE,
  #                                   group = "2022 303(d) Temperature Listed - Streams (Year Round Criteria)",
  #                                   pathOptions = leaflet::pathOptions(pane="ir"),
  #                                   color = "red",
  #                                   weight = 3,
  #                                   opacity = 0.8,
  #                                   fill=FALSE,
  #                                   highlightOptions = leaflet::highlightOptions(color="red",
  #                                                                                weight = 5,
  #                                                                                fillOpacity = 0.5,
  #                                                                                bringToFront = TRUE,
  #                                                                                sendToBack = TRUE),
  #                                   labelProperty = htmlwidgets::JS("function(feature){var props = feature.properties; return props.AU_Name+\" \"}"),
  #                                   labelOptions = leaflet::labelOptions(#noHide = T,
  #                                     style = list("color" = "red","font-size" = "12px")),
  #                                   popupProperty = htmlwidgets::JS(paste0('function(feature){var props = feature.properties; return \"',
  #                                                                          '<b>AU Name:</b> \"+props.AU_Name+\"',
  #                                                                          '<br><b>AU ID:</b> \"+props.AU_ID+\"',
  #                                                                          '<br><b>Impaired Parameter:</b> \"+props.Char_Name+\"',
  #                                                                          '<br><b>IR Category:</b> \"+props.IR_category+\"',
  #                                                                          '<br><b>Year Listed:</b> \"+props.Year_listed+\"',
  #                                                                          '<br><b>Use Period:</b> \"+props.Period+\"',
  #                                                                          '<br><b>HUC12:</b> \"+props.HUC12+\"',
  #                                                                          ' \"}'))) %>% 
  #   leaflet.esri::addEsriFeatureLayer(url="https://arcgis.deq.state.or.us/arcgis/rest/services/WQ/IR_201820_byParameter/MapServer/0",
  #                                     options = leaflet.esri::featureLayerOptions(where = where_au_spawning),
  #                                     useServiceSymbology = TRUE,
  #                                     group = "2022 303(d) Temperature Listed - Streams (Spawning Criteria)",
  #                                     pathOptions = leaflet::pathOptions(pane="ir"),
  #                                     color = "red",
  #                                     weight = 3,
  #                                     opacity = 0.8,
  #                                     fill=FALSE,
  #                                     highlightOptions = leaflet::highlightOptions(color="red",
  #                                                                                  weight = 5,
  #                                                                                  fillOpacity = 0.5,
  #                                                                                  bringToFront = TRUE,
  #                                                                                  sendToBack = TRUE),
  #                                     labelProperty = htmlwidgets::JS("function(feature){var props = feature.properties; return props.AU_Name+\" \"}"),
  #                                     labelOptions = leaflet::labelOptions(#noHide = T,
  #                                       style = list("color" = "red","font-size" = "12px")),
  #                                     popupProperty = htmlwidgets::JS(paste0('function(feature){var props = feature.properties; return \"',
  #                                                                            '<b>AU Name:</b> \"+props.AU_Name+\"',
  #                                                                            '<br><b>AU ID:</b> \"+props.AU_ID+\"',
  #                                                                            '<br><b>Impaired Parameter:</b> \"+props.Char_Name+\"',
  #                                                                            '<br><b>IR Category:</b> \"+props.IR_category+\"',
  #                                                                            '<br><b>Year Listed:</b> \"+props.Year_listed+\"',
  #                                                                            '<br><b>Use Period:</b> \"+props.Period+\"',
  #                                                                            '<br><b>HUC12:</b> \"+props.HUC12+\"',
  #                                                                            ' \"}'))) %>% 
  #   leaflet.esri::addEsriFeatureLayer(url="https://arcgis.deq.state.or.us/arcgis/rest/services/WQ/IR_201820_byParameter/MapServer/1",
  #                                     options = leaflet.esri::featureLayerOptions(where = where_au_yearRound),
  #                                     useServiceSymbology = TRUE,
  #                                     group = "2022 303(d) Temperature Listed - Waterbodies (Year Round Criteria)",
  #                                     pathOptions = leaflet::pathOptions(pane="ir"),
  #                                     color = "red",
  #                                     weight = 2,
  #                                     opacity = 0.8,
  #                                     fill=TRUE,
  #                                     fillColor = "red",
  #                                     fillOpacity = 0.25,
  #                                     labelProperty = htmlwidgets::JS("function(feature){var props = feature.properties; return props.AU_Name+\" \"}"),
  #                                     labelOptions = leaflet::labelOptions(#noHide = T,
  #                                       style = list("color" = "red","font-size" = "12px")),
  #                                     highlightOptions = leaflet::highlightOptions(color="red",
  #                                                                                  weight = 3,
  #                                                                                  fillOpacity = 0.5,
  #                                                                                  bringToFront = TRUE,
  #                                                                                  sendToBack = TRUE),
  #                                     popupProperty = htmlwidgets::JS(paste0('function(feature){var props = feature.properties; return \"',
  #                                                                            '<b>AU Name:</b> \"+props.AU_Name+\"',
  #                                                                            '<br><b>AU ID:</b> \"+props.AU_ID+\"',
  #                                                                            '<br><b>Impaired Parameter:</b> \"+props.Char_Name+\"',
  #                                                                            '<br><b>IR Category:</b> \"+props.IR_category+\"',
  #                                                                            '<br><b>Year Listed:</b> \"+props.Year_listed+\"',
  #                                                                            '<br><b>Use Period:</b> \"+props.Period+\"',
  #                                                                            '<br><b>HUC12:</b> \"+props.HUC12+\"',
  #                                                                            ' \"}'))) %>% 
  #   leaflet.esri::addEsriFeatureLayer(url="https://arcgis.deq.state.or.us/arcgis/rest/services/WQ/IR_201820_byParameter/MapServer/1",
  #                                     options = leaflet.esri::featureLayerOptions(where = where_au_spawning),
  #                                     useServiceSymbology = TRUE,
  #                                     group = "2022 303(d) Temperature Listed - Waterbodies (Spawning Criteria)",
  #                                     pathOptions = leaflet::pathOptions(pane="ir"),
  #                                     color = "red",
  #                                     weight = 2,
  #                                     opacity = 0.8,
  #                                     fill=TRUE,
  #                                     fillColor = "red",
  #                                     fillOpacity = 0.25,
  #                                     labelProperty = htmlwidgets::JS("function(feature){var props = feature.properties; return props.AU_Name+\" \"}"),
  #                                     labelOptions = leaflet::labelOptions(#noHide = T,
  #                                       style = list("color" = "red","font-size" = "12px")),
  #                                     highlightOptions = leaflet::highlightOptions(color="red",
  #                                                                                  weight = 3,
  #                                                                                  fillOpacity = 0.5,
  #                                                                                  bringToFront = TRUE,
  #                                                                                  sendToBack = TRUE),
  #                                     popupProperty = htmlwidgets::JS(paste0('function(feature){var props = feature.properties; return \"',
  #                                                                            '<b>AU Name:</b> \"+props.AU_Name+\"',
  #                                                                            '<br><b>AU ID:</b> \"+props.AU_ID+\"',
  #                                                                            '<br><b>Impaired Parameter:</b> \"+props.Char_Name+\"',
  #                                                                            '<br><b>IR Category:</b> \"+props.IR_category+\"',
  #                                                                            '<br><b>Year Listed:</b> \"+props.Year_listed+\"',
  #                                                                            '<br><b>Use Period:</b> \"+props.Period+\"',
  #                                                                            '<br><b>HUC12:</b> \"+props.HUC12+\"',
  #                                                                            ' \"}'))) %>% 
  #   leaflet.esri::addEsriFeatureLayer(url="https://arcgis.deq.state.or.us/arcgis/rest/services/WQ/IR_201820_byParameter/MapServer/2",
  #                                     options = leaflet.esri::featureLayerOptions(where = where_au_yearRound),
  #                                     useServiceSymbology = TRUE,
  #                                     group = "2022 303(d) Temperature Listed - Watershed (Year Round Criteria)",
  #                                     pathOptions = leaflet::pathOptions(pane="ir"),
  #                                     color = "red",
  #                                     weight = 1,
  #                                     opacity = 0.8,
  #                                     fill = TRUE,
  #                                     fillColor = "red",
  #                                     fillOpacity = 0.25,
  #                                     labelProperty = htmlwidgets::JS("function(feature){var props = feature.properties; return props.AU_Name+\" \"}"),
  #                                     labelOptions = leaflet::labelOptions(#noHide = T,
  #                                       style = list("color" = "red","font-size" = "12px")),
  #                                     highlightOptions = leaflet::highlightOptions(color="red",
  #                                                                                  weight = 3,
  #                                                                                  fillOpacity = 0.8,
  #                                                                                  bringToFront = TRUE,
  #                                                                                  sendToBack = TRUE),
  #                                     popupProperty = htmlwidgets::JS(paste0('function(feature){var props = feature.properties; return \"',
  #                                                                            '<b>AU Name:</b> \"+props.AU_Name+\"',
  #                                                                            '<br><b>AU ID:</b> \"+props.AU_ID+\"',
  #                                                                            '<br><b>Impaired Parameter:</b> \"+props.Char_Name+\"',
  #                                                                            '<br><b>IR Category:</b> \"+props.IR_category+\"',
  #                                                                            '<br><b>Year Listed:</b> \"+props.Year_listed+\"',
  #                                                                            '<br><b>Use Period:</b> \"+props.Period+\"',
  #                                                                            '<br><b>HUC12:</b> \"+props.HUC12+\"',
  #                                                                            ' \"}'))) %>%
  #   leaflet.esri::addEsriFeatureLayer(url="https://arcgis.deq.state.or.us/arcgis/rest/services/WQ/IR_201820_byParameter/MapServer/2",
  #                                     options = leaflet.esri::featureLayerOptions(where = where_au_spawning),
  #                                     useServiceSymbology = TRUE,
  #                                     group = "2022 303(d) Temperature Listed - Watershed (Spawning Criteria)",
  #                                     pathOptions = leaflet::pathOptions(pane="ir"),
  #                                     color = "red",
  #                                     weight = 1,
  #                                     opacity = 0.8,
  #                                     fill = TRUE,
  #                                     fillColor = "red",
  #                                     fillOpacity = 0.25,
  #                                     labelProperty = htmlwidgets::JS("function(feature){var props = feature.properties; return props.AU_Name+\" \"}"),
  #                                     labelOptions = leaflet::labelOptions(#noHide = T,
  #                                       style = list("color" = "red","font-size" = "12px")),
  #                                     highlightOptions = leaflet::highlightOptions(color="red",
  #                                                                                  weight = 3,
  #                                                                                  fillOpacity = 0.8,
  #                                                                                  bringToFront = TRUE,
  #                                                                                  sendToBack = TRUE),
  #                                     popupProperty = htmlwidgets::JS(paste0('function(feature){var props = feature.properties; return \"',
  #                                                                            '<b>AU Name:</b> \"+props.AU_Name+\"',
  #                                                                            '<br><b>AU ID:</b> \"+props.AU_ID+\"',
  #                                                                            '<br><b>Impaired Parameter:</b> \"+props.Char_Name+\"',
  #                                                                            '<br><b>IR Category:</b> \"+props.IR_category+\"',
  #                                                                            '<br><b>Year Listed:</b> \"+props.Year_listed+\"',
  #                                                                            '<br><b>Use Period:</b> \"+props.Period+\"',
  #                                                                            '<br><b>HUC12:</b> \"+props.HUC12+\"',
  #                                                                            ' \"}'))) %>%
  # Update for IR2022
  leaflet.esri::addEsriFeatureLayer(url="https://services.arcgis.com/uUvqNMGPm7axC2dD/ArcGIS/rest/services/IR_2022_Final/FeatureServer/50",
                                    options = leaflet.esri::featureLayerOptions(where = where_au_yearRound),
                                    useServiceSymbology = TRUE,
                                    group = "2022 303(d) Temperature Listed - Streams (Year Round Criteria)",
                                    pathOptions = leaflet::pathOptions(pane="ir"),
                                    color = "#9A00C4",
                                    weight = 3,
                                    opacity = 0.8,
                                    fill=FALSE,
                                    highlightOptions = leaflet::highlightOptions(color="#9A00C4",
                                                                                 weight = 5,
                                                                                 fillOpacity = 0.5,
                                                                                 bringToFront = TRUE,
                                                                                 sendToBack = TRUE),
                                    labelProperty = htmlwidgets::JS("function(feature){var props = feature.properties; return props.AU_Name+\" \"}"),
                                    labelOptions = leaflet::labelOptions(#noHide = T,
                                      style = list("color" = "#9A00C4","font-size" = "12px")),
                                    popupProperty = htmlwidgets::JS(paste0('function(feature){var props = feature.properties; return \"',
                                                                           '<b>AU Name:</b> \"+props.AU_Name+\"',
                                                                           '<br><b>AU ID:</b> \"+props.AU_ID+\"',
                                                                           '<br><b>Impaired Parameter:</b> \"+props.Pollutant+\"',
                                                                           '<br><b>IR Category:</b> \"+props.AU_parameter_category+\"',
                                                                           '<br><b>Year Listed:</b> \"+props.Year_listed+\"',
                                                                           '<br><b>Use Period:</b> \"+props.period+\"',
                                                                           '<br><b>HUC12:</b> \"+props.HUC_12+\"',
                                                                           ' \"}'))) %>%
    leaflet.esri::addEsriFeatureLayer(url="https://services.arcgis.com/uUvqNMGPm7axC2dD/ArcGIS/rest/services/IR_2022_Final/FeatureServer/50",
                                      options = leaflet.esri::featureLayerOptions(where = where_au_spawning),
                                      useServiceSymbology = TRUE,
                                      group = "2022 303(d) Temperature Listed - Streams (Spawning Criteria)",
                                      pathOptions = leaflet::pathOptions(pane="ir"),
                                      color = "#9A00C4",
                                      weight = 3,
                                      opacity = 0.8,
                                      fill=FALSE,
                                      highlightOptions = leaflet::highlightOptions(color="#9A00C4",
                                                                                   weight = 5,
                                                                                   fillOpacity = 0.5,
                                                                                   bringToFront = TRUE,
                                                                                   sendToBack = TRUE),
                                      labelProperty = htmlwidgets::JS("function(feature){var props = feature.properties; return props.AU_Name+\" \"}"),
                                      labelOptions = leaflet::labelOptions(#noHide = T,
                                        style = list("color" = "#9A00C4","font-size" = "12px")),
                                      popupProperty = htmlwidgets::JS(paste0('function(feature){var props = feature.properties; return \"',
                                                                             '<b>AU Name:</b> \"+props.AU_Name+\"',
                                                                             '<br><b>AU ID:</b> \"+props.AU_ID+\"',
                                                                             '<br><b>Impaired Parameter:</b> \"+props.Pollutant+\"',
                                                                             '<br><b>IR Category:</b> \"+props.AU_parameter_category+\"',
                                                                             '<br><b>Year Listed:</b> \"+props.Year_listed+\"',
                                                                             '<br><b>Use Period:</b> \"+props.period+\"',
                                                                             '<br><b>HUC12:</b> \"+props.HUC_12+\"',
                                                                             ' \"}'))) %>%
    leaflet.esri::addEsriFeatureLayer(url="https://services.arcgis.com/uUvqNMGPm7axC2dD/ArcGIS/rest/services/IR_2022_Final/FeatureServer/51",
                                      options = leaflet.esri::featureLayerOptions(where = where_au_yearRound),
                                      useServiceSymbology = TRUE,
                                      group = "2022 303(d) Temperature Listed - Waterbodies (Year Round Criteria)",
                                      pathOptions = leaflet::pathOptions(pane="ir"),
                                      color = "#9A00C4",
                                      weight = 2,
                                      opacity = 0.8,
                                      fill=TRUE,
                                      fillColor = "#9A00C4",
                                      fillOpacity = 0.25,
                                      labelProperty = htmlwidgets::JS("function(feature){var props = feature.properties; return props.AU_Name+\" \"}"),
                                      labelOptions = leaflet::labelOptions(#noHide = T,
                                        style = list("color" = "#9A00C4","font-size" = "12px")),
                                      highlightOptions = leaflet::highlightOptions(color="#9A00C4",
                                                                                   weight = 3,
                                                                                   fillOpacity = 0.5,
                                                                                   bringToFront = TRUE,
                                                                                   sendToBack = TRUE),
                                      popupProperty = htmlwidgets::JS(paste0('function(feature){var props = feature.properties; return \"',
                                                                             '<b>AU Name:</b> \"+props.AU_Name+\"',
                                                                             '<br><b>AU ID:</b> \"+props.AU_ID+\"',
                                                                             '<br><b>Impaired Parameter:</b> \"+props.Pollutant+\"',
                                                                             '<br><b>IR Category:</b> \"+props.AU_parameter_category+\"',
                                                                             '<br><b>Year Listed:</b> \"+props.Year_listed+\"',
                                                                             '<br><b>Use Period:</b> \"+props.period+\"',
                                                                             '<br><b>HUC12:</b> \"+props.HUC_12+\"',
                                                                             ' \"}'))) %>%
    leaflet.esri::addEsriFeatureLayer(url="https://services.arcgis.com/uUvqNMGPm7axC2dD/ArcGIS/rest/services/IR_2022_Final/FeatureServer/51",
                                      options = leaflet.esri::featureLayerOptions(where = where_au_spawning),
                                      useServiceSymbology = TRUE,
                                      group = "2022 303(d) Temperature Listed - Waterbodies (Spawning Criteria)",
                                      pathOptions = leaflet::pathOptions(pane="ir"),
                                      color = "#9A00C4",
                                      weight = 2,
                                      opacity = 0.8,
                                      fill=TRUE,
                                      fillColor = "#9A00C4",
                                      fillOpacity = 0.25,
                                      labelProperty = htmlwidgets::JS("function(feature){var props = feature.properties; return props.AU_Name+\" \"}"),
                                      labelOptions = leaflet::labelOptions(#noHide = T,
                                        style = list("color" = "#9A00C4","font-size" = "12px")),
                                      highlightOptions = leaflet::highlightOptions(color="#9A00C4",
                                                                                   weight = 3,
                                                                                   fillOpacity = 0.5,
                                                                                   bringToFront = TRUE,
                                                                                   sendToBack = TRUE),
                                      popupProperty = htmlwidgets::JS(paste0('function(feature){var props = feature.properties; return \"',
                                                                             '<b>AU Name:</b> \"+props.AU_Name+\"',
                                                                             '<br><b>AU ID:</b> \"+props.AU_ID+\"',
                                                                             '<br><b>Impaired Parameter:</b> \"+props.Pollutant+\"',
                                                                             '<br><b>IR Category:</b> \"+props.AU_parameter_category+\"',
                                                                             '<br><b>Year Listed:</b> \"+props.Year_listed+\"',
                                                                             '<br><b>Use Period:</b> \"+props.period+\"',
                                                                             '<br><b>HUC12:</b> \"+props.HUC_12+\"',
                                                                             ' \"}'))) %>%
    leaflet.esri::addEsriFeatureLayer(url="https://services.arcgis.com/uUvqNMGPm7axC2dD/ArcGIS/rest/services/IR_2022_Final/FeatureServer/63",
                                      options = leaflet.esri::featureLayerOptions(where = where_au_yearRound),
                                      useServiceSymbology = TRUE,
                                      group = "2022 303(d) Temperature Listed - Watershed (Year Round Criteria)",
                                      pathOptions = leaflet::pathOptions(pane="ir"),
                                      color = "#9A00C4",
                                      weight = 2,
                                      opacity = 0.8,
                                      fill=TRUE,
                                      fillColor = "#9A00C4",
                                      fillOpacity = 0.25,
                                      labelProperty = htmlwidgets::JS("function(feature){var props = feature.properties; return props.AU_Name+\" \"}"),
                                      labelOptions = leaflet::labelOptions(#noHide = T,
                                        style = list("color" = "#9A00C4","font-size" = "12px")),
                                      highlightOptions = leaflet::highlightOptions(color="#9A00C4",
                                                                                   weight = 3,
                                                                                   fillOpacity = 0.5,
                                                                                   bringToFront = TRUE,
                                                                                   sendToBack = TRUE),
                                      popupProperty = htmlwidgets::JS(paste0('function(feature){var props = feature.properties; return \"',
                                                                             '<b>AU Name:</b> \"+props.AU_Name+\"',
                                                                             '<br><b>AU ID:</b> \"+props.AU_ID+\"',
                                                                             '<br><b>Impaired Parameter:</b> \"+props.Pollutant+\"',
                                                                             '<br><b>IR Category:</b> \"+props.AU_parameter_category+\"',
                                                                             '<br><b>Year Listed:</b> \"+props.Year_listed+\"',
                                                                             '<br><b>Use Period:</b> \"+props.period+\"',
                                                                             '<br><b>HUC12:</b> \"+props.HUC_12+\"',
                                                                             ' \"}'))) %>%
    leaflet.esri::addEsriFeatureLayer(url="https://services.arcgis.com/uUvqNMGPm7axC2dD/ArcGIS/rest/services/IR_2022_Final/FeatureServer/63",
                                      options = leaflet.esri::featureLayerOptions(where = where_au_spawning),
                                      useServiceSymbology = TRUE,
                                      group = "2022 303(d) Temperature Listed - Watershed (Spawning Criteria)",
                                      pathOptions = leaflet::pathOptions(pane="ir"),
                                      color = "#9A00C4",
                                      weight = 2,
                                      opacity = 0.8,
                                      fill=TRUE,
                                      fillColor = "#9A00C4",
                                      fillOpacity = 0.25,
                                      labelProperty = htmlwidgets::JS("function(feature){var props = feature.properties; return props.AU_Name+\" \"}"),
                                      labelOptions = leaflet::labelOptions(#noHide = T,
                                        style = list("color" = "#9A00C4","font-size" = "12px")),
                                      highlightOptions = leaflet::highlightOptions(color="#9A00C4",
                                                                                   weight = 3,
                                                                                   fillOpacity = 0.5,
                                                                                   bringToFront = TRUE,
                                                                                   sendToBack = TRUE),
                                      popupProperty = htmlwidgets::JS(paste0('function(feature){var props = feature.properties; return \"',
                                                                             '<b>AU Name:</b> \"+props.AU_Name+\"',
                                                                             '<br><b>AU ID:</b> \"+props.AU_ID+\"',
                                                                             '<br><b>Impaired Parameter:</b> \"+props.Pollutant+\"',
                                                                             '<br><b>IR Category:</b> \"+props.AU_parameter_category+\"',
                                                                             '<br><b>Year Listed:</b> \"+props.Year_listed+\"',
                                                                             '<br><b>Use Period:</b> \"+props.period+\"',
                                                                             '<br><b>HUC12:</b> \"+props.HUC_12+\"',
                                                                             ' \"}'))) %>%
    # __ WQS ----
  leaflet.esri::addEsriFeatureLayer(url="https://arcgis.deq.state.or.us/arcgis/rest/services/WQ/WQStandards_WM/MapServer/0",
                                    options = leaflet.esri::featureLayerOptions(where = reachcode,
                                                                                style = tempWQScolor),
                                    pathOptions = leaflet::pathOptions(pane="wqs1"),
                                    useServiceSymbology = FALSE,
                                    group = "Fish Use Designations",
                                    weight = 3,
                                    opacity = 1,
                                    fill=FALSE,
                                    popupOptions = leaflet::popupOptions(maxWidth = 600, maxHeight = 500),
                                    popupProperty = htmlwidgets::JS(paste0('function(feature){var props = feature.properties; return \"',
                                                                           '<b>Waterbody Name:</b> \"+props.GNIS_Name+\"',
                                                                           '<br><b>Temperature Criteria:</b> \"+props.Temperature_Criteria+\"',
                                                                           '<br><b>Non Spawning 7DADM deg-C:</b> \"+props.Temperature_Criterion_7dADM_C+\"',
                                                                           '<br><b>Temperature Spawning Dates:</b> \"+props.Temperature_Spawn_dates+\"',
                                                                           ' \"}'))) %>%
    leaflet.esri::addEsriFeatureLayer(url="https://arcgis.deq.state.or.us/arcgis/rest/services/WQ/WQStandards_WM/MapServer/0",
                                      options = leaflet.esri::featureLayerOptions(where = reachcode,
                                                                                  style = tempSpawncolor),
                                      pathOptions = leaflet::pathOptions(pane="wqs2"),
                                      useServiceSymbology = FALSE,
                                      group = "Salmon and Steelhead Spawning Use Designations",
                                      weight = 3,
                                      opacity = 1,
                                      fill=FALSE,
                                      popupOptions = leaflet::popupOptions(maxWidth = 600, maxHeight = 500),
                                      popupProperty = htmlwidgets::JS(paste0('function(feature){var props = feature.properties; return \"',
                                                                             '<b>Waterbody Name:</b> \"+props.GNIS_Name+\"',
                                                                             '<br><b>Temperature Spawn Dates:</b> \"+props.Temperature_Spawn_dates+\"',
                                                                             '<br><b> Spawning Criterion 7DADM deg-C:</b> 13',
                                                                             ' \"}'))) %>% 
    # __ DMA ----
  # leaflet.esri::addEsriFeatureLayer(url = "https://services.arcgis.com/uUvqNMGPm7axC2dD/ArcGIS/rest/services/OR_DMAs/FeatureServer/0",
  #                                   options = leaflet.esri::featureLayerOptions(where = where_huc12_1,
  #                                                                               style = DMAcolor,
  #                                                                               simplifyFactor = 0.35),
  #                                   group = "Land Ownership or Jurisdiction",
  #                                   pathOptions = leaflet::pathOptions(pane="dma"),
  #                                   fill = TRUE,
  #                                   fillOpacity = 0.75,
  #                                   weight = 0.1,
  #                                   opacity = 0,
  #                                   color = "white",
  #                                   highlightOptions = leaflet::highlightOptions(weight = 3,
  #                                                                                color = "white",
  #                                                                                opacity = 1,
  #                                                                                bringToFront = TRUE),
  #                                   labelProperty = htmlwidgets::JS("function(feature){var props = feature.properties; return props.DMA_RP+\" \"}"),
  #                                   labelOptions = leaflet::labelOptions(style = list("color" = "black","font-size" = "12px")),
  #                                   popupProperty = htmlwidgets::JS(paste0('function(feature){var props = feature.properties; return \"',
  #                                                                          '<b>\"+props.DMA_RP+\"</b>',
  #                                                                          '<br>\"+props.DMA_RP_Cl+\"',
  #                                                                          '<br><b>HUC12:</b> \"+props.HUC12+\"',
  #                                                                          '<br><b>Version:</b> \"+props.Version+\"',
  #                                                                          ' \"}')),
  #                                   popupOptions = leaflet::popupOptions(maxWidth = 800, maxHeight = 500)) %>%
  #   leaflet.esri::addEsriFeatureLayer(url="https://services.arcgis.com/uUvqNMGPm7axC2dD/ArcGIS/rest/services/OR_DMAs/FeatureServer/0",
  #                                     options = leaflet.esri::featureLayerOptions(where = where_huc12_2,
  #                                                                                 style = DMAcolor,
  #                                                                                 simplifyFactor = 0.35),
  #                                     group = "Land Ownership or Jurisdiction",
  #                                     pathOptions = leaflet::pathOptions(pane="dma"),
  #                                     fill = TRUE,
  #                                     fillOpacity = 0.75,
  #                                     weight = 0.1,
  #                                     opacity = 0,
  #                                     color = "white",
  #                                     highlightOptions = leaflet::highlightOptions(weight = 3,
  #                                                                                  color = "white",
  #                                                                                  opacity = 1,
  #                                                                                  bringToFront = TRUE),
  #                                     labelProperty = htmlwidgets::JS("function(feature){var props = feature.properties; return props.DMA_RP+\" \"}"),
  #                                     labelOptions = leaflet::labelOptions(style = list("color" = "black","font-size" = "12px")),
  #                                     popupProperty = htmlwidgets::JS(paste0('function(feature){var props = feature.properties; return \"',
  #                                                                            '<b>\"+props.DMA_RP+\"</b>',
  #                                                                            '<br>\"+props.DMA_RP_Cl+\"',
  #                                                                            '<br><b>HUC12:</b> \"+props.HUC12+\"',
  #                                                                            '<br><b>Version:</b> \"+props.Version+\"',
  #                                                                            ' \"}')),
  #                                     popupOptions = leaflet::popupOptions(maxWidth = 800, maxHeight = 500)) %>%
    leaflet.esri::addEsriFeatureLayer(url="https://services.arcgis.com/uUvqNMGPm7axC2dD/ArcGIS/rest/services/OR_DMAs/FeatureServer/0",
                                      options = leaflet.esri::featureLayerOptions(where = where_huc10,
                                                                                  style = DMAcolor,
                                                                                  simplifyFactor = 0.35),
                                      group = "Land Ownership or Jurisdiction",
                                      pathOptions = leaflet::pathOptions(pane="dma"),
                                      fill = TRUE,
                                      fillOpacity = 0.75,
                                      weight = 0.1,
                                      opacity = 0,
                                      color = "white",
                                      highlightOptions = leaflet::highlightOptions(weight = 3,
                                                                                   color = "white",
                                                                                   opacity = 1,
                                                                                   bringToFront = TRUE),
                                      labelProperty = htmlwidgets::JS("function(feature){var props = feature.properties; return props.DMA_RP+\" \"}"),
                                      labelOptions = leaflet::labelOptions(style = list("color" = "black","font-size" = "12px")),
                                      popupProperty = htmlwidgets::JS(paste0('function(feature){var props = feature.properties; return \"',
                                                                             '<b>\"+props.DMA_RP+\"</b>',
                                                                             '<br>\"+props.DMA_RP_Cl+\"',
                                                                             '<br><b>HUC12:</b> \"+props.HUC12+\"',
                                                                             '<br><b>Version:</b> \"+props.Version+\"',
                                                                             ' \"}')),
                                      popupOptions = leaflet::popupOptions(maxWidth = 800, maxHeight = 500))
  
  # Project area layer ----
  # __ John Day River Basin ----
  if(qapp_project_area == "John Day River Basin") {
    map_area <- map_basic %>% 
      #projectScope(pro_area) %>% 
      hsTempModel(hs_temp_model_extent) %>% 
      tempStation.markers(temp_stations) %>% 
      tempCalibration.markers(temp_cal_sites) %>% 
      tempBoundaryTributary.markers(temp_model_bc_tri) %>% 
      flowStation.markers(flow_stations) %>% 
      flowBoundaryTributary.markers(flow_model_bc_tri) %>% 
      metStation.markders(met_stations) %>% 
      indPS.markers(ind_ps) %>% 
      genPS.markers(gen_ps) %>% 
      #effectiveShade.markers(shade) %>% 
      leaflet::addLayersControl(overlayGroups = c("TMDL Project Scope",group.names),
                                options = leaflet::layersControlOptions(collapsed = FALSE, autoZIndex = TRUE)) %>% 
      leaflet::hideGroup(group.names.hide)
  }
  
  # __ Lower Grande Ronde, Imnaha, and Wallowa Subbasins ----
  if(qapp_project_area == "Lower Grande Ronde, Imnaha, and Wallowa Subbasins") {
    map_area <- map_basic %>% 
      #projectScope(pro_area) %>% 
      hsTempModel(hs_temp_model_extent) %>% 
      hsSolarModel(hs_solar_model_extent) %>% 
      tempStation.markers(temp_stations) %>% 
      tempCalibration.markers(temp_cal_sites) %>% 
      tempBoundaryTributary.markers(temp_model_bc_tri) %>% 
      flowStation.markers(flow_stations) %>% 
      metStation.markders(met_stations) %>% 
      indPS.markers(ind_ps) %>% 
      genPS.markers(gen_ps) %>% 
      leaflet::addLayersControl(overlayGroups = c("TMDL Project Scope",group.names),
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
      #projectScope(pro_area) %>% 
      # __ BES (2019)
      leaflet::addPolylines(data = bes_model_extent,
                            group = "Heat Source Shade Model Extent (New Models)",
                            options = leaflet::leafletOptions(pane="modbes"),
                            label = ~Stream,
                            labelOptions = labelOptions(style = list("color" = "black",
                                                                     "font-size" = "12px")),
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
      #effectiveShade.markers(shade) %>% 
      leaflet::addLayersControl(overlayGroups = c("TMDL Project Scope",
                                                  "Heat Source Shade Model Extent (New Models)",
                                                  group.names),
                                options = leaflet::layersControlOptions(collapsed = FALSE, autoZIndex = TRUE)) %>% 
      leaflet::hideGroup(c("Heat Source Shade Model Extent (New Models)",
                           group.names.hide))
  }
  
  # __ Malheur River Subbasins ----
  if(qapp_project_area == "Malheur River Subbasins") {
    map_area <- map_basic %>% 
      #projectScope(pro_area) %>% 
      hsSolarModel(hs_solar_model_extent) %>% 
      tempStation.markers(temp_stations) %>% 
      tempCalibration.markers(temp_cal_sites) %>% 
      flowStation.markers(flow_stations) %>% 
      metStation.markders(met_stations) %>% 
      indPS.markers(ind_ps) %>% 
      genPS.markers(gen_ps) %>% 
      #effectiveShade.markers(shade) %>% 
      leaflet::addLayersControl(overlayGroups = c("TMDL Project Scope",group.names),
                                options = leaflet::layersControlOptions(collapsed = FALSE, autoZIndex = TRUE)) %>% 
      leaflet::hideGroup(group.names.hide)
  }
  
  # __ Middle Willamette Subbasins ----
  if(qapp_project_area == "Middle Willamette Subbasins") {
    map_area <- map_basic %>% 
      #projectScope(pro_area) %>% 
      hsTempModel(hs_temp_model_extent) %>% 
      tempStation.markers(temp_stations) %>% 
      tempCalibration.markers(temp_cal_sites) %>% 
      tempBoundaryTributary.markers(temp_model_bc_tri) %>% 
      flowStation.markers(flow_stations) %>% 
      flowBoundaryTributary.markers(flow_model_bc_tri) %>% 
      metStation.markders(met_stations) %>% 
      indPS.markers(ind_ps) %>% 
      genPS.markers(gen_ps) %>% 
      leaflet::addLayersControl(overlayGroups = c("TMDL Project Scope",group.names),
                                options = leaflet::layersControlOptions(collapsed = FALSE, autoZIndex = TRUE)) %>% 
      leaflet::hideGroup(group.names.hide)
  }
  
  # __ Middle Columbia-Hood, Miles Creeks ----
  if(qapp_project_area == "Middle Columbia-Hood, Miles Creeks") {
    map_area <- map_basic %>% 
      #projectScope(pro_area) %>% 
      hsTempModel(hs_temp_model_extent) %>% 
      hsSolarModel(hs_solar_model_extent) %>% 
      tempStation.markers(temp_stations) %>% 
      tempCalibration.markers(temp_cal_sites) %>% 
      tempBoundaryTributary.markers(temp_model_bc_tri) %>% 
      flowStation.markers(flow_stations) %>% 
      metStation.markders(met_stations) %>% 
      indPS.markers(ind_ps) %>% 
      #effectiveShade.markers(shade) %>% 
      leaflet::addLayersControl(overlayGroups = c("TMDL Project Scope",group.names),
                                options = leaflet::layersControlOptions(collapsed = FALSE, autoZIndex = TRUE)) %>% 
      leaflet::hideGroup(group.names.hide)
  }
  
  # __ North Umpqua Subbasin ----
  if(qapp_project_area == "North Umpqua Subbasin") {
    
    # ____ * New models: Fish Creek 2009 and Tetra Tech models ----
    new.models <- sf::st_read(dsn = "//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/model_QAPPs/R/data/gis/n_umpqua_river_new_models.shp",
                              layer = "n_umpqua_river_new_models") %>% 
      sf::st_transform(4326) %>% 
      sf::st_zm()
    
    map_area <- map_basic %>% 
      #projectScope(pro_area) %>% 
      # __ new models
      leaflet::addPolylines(data = new.models,
                            group = "Heat Source Temperature Model Extent (New Models)",
                            options = leaflet::leafletOptions(pane="mod2009"),
                            label = ~Stream,
                            labelOptions = labelOptions(style = list("color" = "black",
                                                                     "font-size" = "12px")),
                            color = "#993404",
                            opacity = 0.5,
                            weight = 10) %>% 
      hsTempModel(hs_temp_model_extent) %>% 
      tempStation.markers(temp_stations) %>% 
      tempCalibration.markers(temp_cal_sites) %>% 
      tempBoundaryTributary.markers(temp_model_bc_tri) %>% 
      flowStation.markers(flow_stations) %>% 
      metStation.markders(met_stations) %>% 
      indPS.markers(ind_ps) %>% 
      genPS.markers(gen_ps) %>% 
      #effectiveShade.markers(shade) %>% 
      leaflet::addLayersControl(overlayGroups = c("TMDL Project Scope",
                                                  "Heat Source Temperature Model Extent (New Models)",
                                                  group.names),
                                options = leaflet::layersControlOptions(collapsed = FALSE, autoZIndex = TRUE)) %>% 
      leaflet::hideGroup(c("Heat Source Temperature Model Extent (New Models)",
                           group.names.hide))
  }
  
  # __ Rogue River Basin ----
  if(qapp_project_area == "Rogue River Basin") {
    
    # ____ * New models: Applegate and Little Applegate ----
    map_hs_applegate_and_little_applegate <- sf::st_read(dsn = "//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/model_QAPPs/R/data/gis/applegate_and_little applegate.shp",
                                                         layer = "applegate_and_little applegate") %>% 
      sf::st_transform(4326) %>% 
      sf::st_zm() 
    
    map_area <- map_basic %>% 
      #projectScope(pro_area) %>% 
      # __ Applegate and Little Applegate
      leaflet::addPolylines(data = map_hs_applegate_and_little_applegate,
                            group = "Heat Source Temperature Model Extent (New Models)",
                            options = leaflet::leafletOptions(pane="mod2009"),
                            label = ~Stream,
                            labelOptions = labelOptions(style = list("color" = "black",
                                                                     "font-size" = "12px")),
                            color = "#993404",
                            opacity = 0.5,
                            weight = 10) %>% 
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
      #effectiveShade.markers(shade) %>% 
      leaflet::addLayersControl(overlayGroups = c("TMDL Project Scope",
                                                  "Heat Source Temperature Model Extent (New Models)",
                                                  group.names),
                                options = leaflet::layersControlOptions(collapsed = FALSE, autoZIndex = TRUE)) %>% 
      leaflet::hideGroup(c("Heat Source Temperature Model Extent (New Models)",
                           group.names.hide))
  }
  
  # __ Sandy Subbasin ----
  if(qapp_project_area == "Sandy Subbasin") {
    
    # ____ * New models: Sandy 2016 ----
    sandy_2016 <- sf::st_read(dsn = "//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/model_QAPPs/R/data/gis/sandy_2016.shp",
                              layer = "sandy_2016") %>% 
      sf::st_transform(4326) %>% 
      sf::st_zm()
    
    map_area <- map_basic %>% 
      projectScope(pro_area) %>% 
      # __ Bull Run River, Salmon River and Sandy Rivers (2016)
      leaflet::addPolylines(data = sandy_2016,
                            group = "Heat Source Temperature Model Extent (New Models)",
                            options = leaflet::leafletOptions(pane="mod2016"),
                            label = ~Name,
                            labelOptions = labelOptions(style = list("color" = "black",
                                                                     "font-size" = "12px")),
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
      #effectiveShade.markers(shade) %>% 
      leaflet::addLayersControl(overlayGroups = c("TMDL Project Scope",
                                                  "Heat Source Temperature Model Extent (New Models)",
                                                  group.names),
                                options = leaflet::layersControlOptions(collapsed = FALSE, autoZIndex = TRUE)) %>% 
      leaflet::hideGroup(c("Heat Source Temperature Model Extent (New Models)",
                           group.names.hide))
  }
  
  # __ South Umpqua and Umpqua Subbasins ----
  if(qapp_project_area == "South Umpqua and Umpqua Subbasins") {
    map_area <- map_basic %>% 
      #projectScope(pro_area) %>% 
      hsTempModel(hs_temp_model_extent) %>% 
      tempStation.markers(temp_stations) %>% 
      tempCalibration.markers(temp_cal_sites) %>% 
      tempBoundaryTributary.markers(temp_model_bc_tri) %>% 
      flowStation.markers(flow_stations) %>% 
      flowBoundaryTributary.markers(flow_model_bc_tri) %>% 
      metStation.markders(met_stations) %>% 
      indPS.markers(ind_ps) %>% 
      genPS.markers(gen_ps) %>% 
      #effectiveShade.markers(shade) %>% 
      leaflet::addLayersControl(overlayGroups = c("TMDL Project Scope",group.names),
                                options = leaflet::layersControlOptions(collapsed = FALSE, autoZIndex = TRUE)) %>% 
      leaflet::hideGroup(group.names.hide)
  }
  
  # __ Southern Willamette Subbasins ----
  if(qapp_project_area == "Southern Willamette Subbasins") {
    map_area <- map_basic %>% 
      #projectScope(pro_area) %>% 
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
      #effectiveShade.markers(shade) %>% 
      leaflet::addLayersControl(overlayGroups = c("TMDL Project Scope",group.names),
                                options = leaflet::layersControlOptions(collapsed = FALSE, autoZIndex = TRUE)) %>% 
      leaflet::hideGroup(group.names.hide)
  }
  
  # __ Walla Walla Subbasin ----
  if(qapp_project_area == "Walla Walla Subbasin") {
    map_area <- map_basic %>% 
      #projectScope(pro_area) %>% 
      hsTempModel(hs_temp_model_extent) %>% 
      hsSolarModel(hs_solar_model_extent) %>% 
      tempStation.markers(temp_stations) %>% 
      tempCalibration.markers(temp_cal_sites) %>% 
      flowStation.markers(flow_stations) %>% 
      metStation.markders(met_stations) %>% 
      #effectiveShade.markers(shade) %>% 
      leaflet::addLayersControl(overlayGroups = c("TMDL Project Scope",group.names),
                                options = leaflet::layersControlOptions(collapsed = FALSE, autoZIndex = TRUE)) %>% 
      leaflet::hideGroup(group.names.hide)
  }
  
  # __ Willamette River Mainstem and Major Tributaries ----
  if(qapp_project_area == "Willamette River Mainstem and Major Tributaries") {
    map_area <- map_basic %>% 
      ceModel(ce_model_extent) %>% 
      tempStation.markers(temp_stations) %>% 
      tempCalibration.markers(temp_cal_sites) %>% 
      #tempBoundaryTributary.markers(temp_model_bc_tri) %>% 
      flowStation.markers(flow_stations) %>% 
      gageHeight.markers(gage_height_stations_map) %>% 
      metStation.markders(met_stations) %>% 
      indPS.markers(ind_ps) %>% 
      genPS.markers(gen_ps) %>% 
      leaflet::addLayersControl(overlayGroups = c("TMDL Project Scope",group.names),
                                options = leaflet::layersControlOptions(collapsed = FALSE, autoZIndex = TRUE)) %>% 
      leaflet::hideGroup(group.names.hide)
  }
  
  # __ Willow Creek Subbasin ----
  if(qapp_project_area == "Willow Creek Subbasin") {
    map_area <- map_basic %>% 
      #projectScope(pro_area) %>% 
      hsTempModel(hs_temp_model_extent) %>% 
      tempStation.markers(temp_stations) %>% 
      tempCalibration.markers(temp_cal_sites) %>% 
      tempBoundaryTributary.markers(temp_model_bc_tri) %>% 
      flowStation.markers(flow_stations) %>% 
      metStation.markders(met_stations) %>% 
      indPS.markers(ind_ps) %>% 
      #effectiveShade.markers(shade) %>% 
      leaflet::addLayersControl(overlayGroups = c("TMDL Project Scope",group.names),
                                options = leaflet::layersControlOptions(collapsed = FALSE, autoZIndex = TRUE)) %>% 
      leaflet::hideGroup(group.names.hide)
  }
  
  # Add-ons ----
  map_final <- map_area %>% 
    # __ Search function ----
  leaflet.extras::addSearchFeatures(targetGroups = group.names.search,
                                    options = searchFeaturesOptions(zoom = 10, 
                                                                    #hideMarkerOnCollapse = TRUE,
                                                                    openPopup = TRUE)) %>% 
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
    leaflet::hideGroup(group.names.search) %>% 
    # __ Legend ----
  #leaflet::addControl(position = "bottomleft", className = "legend",
  #                    html = sprintf('<html><body><div style="opacity:0.95">
  #                                      <img width="280" height="350" src="data:image/png;base64,%s">
  #                                      </div></body></html>', lgnd)) %>%
  # __ Logo ----
  leaflet::addControl(position = "bottomleft", className = "logo",
                      html = sprintf('<html><body><div style="opacity:1">
                                        <a href="https://www.oregon.gov/deq/wq/programs/Pages/wqstatustrends.aspx">
                                        <img width="60" src="data:image/png;base64,%s">
                                        </a></div></body></html>', logo))
  
# SAVE DATA ----
print(paste0(qapp_project_area,"...Save the map"))
htmlwidgets::saveWidget(map_final,paste0(map.file.name,".html"),selfcontained = TRUE) #selfcontained needs to be in the current working directory
file.rename(paste0(map.file.name,".html"), paste0(map.dir,map.file.name,".html"))

#}
