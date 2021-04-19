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
lookup.huc <- readxl::read_xlsx(paste0(data.dir, "Lookup_QAPPProjectArea.xlsx"), sheet = "Lookup_QAPPProjectArea")
schedule <- readxl::read_xlsx(paste0(data.dir, "Model_Setup_Info.xlsx"), sheet = "Schedule")
project.areas <- read.csv(paste0(data.dir,"qapp_project_areas.csv")) %>% 
  dplyr::left_join(schedule, by=c("areas"="QAPP Project Area"))
dir <-  "//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/model_QAPPs/R/map/area_maps/"

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
  
  if(station_name %in% unique(sort(temp_stations$Station))){
    
    mapTempTbl <- temp.data.sample.count %>% 
      dplyr::filter(Station == station_name) %>% 
      dplyr::select(-c(`Station ID`, Station))
    
    table <- knitr::kable(mapTempTbl,
                          format = "html", row.names = FALSE,
                          caption = tags$h5("Summary of existing temperature data at this site:")) %>% 
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
                          caption = tags$h5("Summary of existing flow data at this site:")) %>% 
      kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed","responsive"),
                                full_width = TRUE, font_size = 12,
                                position = "left")
    return(table)
    
  } else {
    
    print("")
    
  }
  
}

# project area map ----
## for test:
# qapp_project_area = "John Day River Basin"
# qapp_project_area = "Lower Grande Ronde, Imnaha, and Wallowa Subbasins"
# qapp_project_area = "Lower Willamette and Clackamas Subbasins"
# qapp_project_area = "Malheur River Subbasins"
# qapp_project_area = "Mid Willamette Subbasins"
# qapp_project_area = "Middle Columbia-Hood, Miles Creeks"
# qapp_project_area = "North Umpqua Subbasin"
# qapp_project_area = "Rogue River Basin"
# qapp_project_area = "Sandy Subbasin"
# qapp_project_area = "South Umpqua and Umpqua Subbasins"
# qapp_project_area = "Southern Willamette Subbasins"
# qapp_project_area = "Walla Walla Subbasin"
# qapp_project_area = "Willamette River Mainstem and Major Tributaries"
# qapp_project_area = "Willow Creek Subbasin"

for (qapp_project_area in project.areas[which(!project.areas$areas %in% c("Willamette River Mainstem and Major Tributaries")),]$areas) {
  
  print(qapp_project_area)
  
  map.file.name <- paste0("map_", project.areas[which(project.areas$areas == qapp_project_area),]$file.name)
  load(paste0("//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/model_QAPPs/R/data/RData/",map.file.name,".RData")) # data.R
  load(paste0("//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/model_QAPPs/R/data/RData/",map.file.name,"_qapp.RData")) # model_QAPP.Rmd
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
  
  map.title <- tags$div(tag.map.title, HTML(paste0(qapp_project_area)))
  # basic ----
  map <- leaflet::leaflet() %>% addTiles() %>% 
    leaflet::fitBounds(lng1 = pro.area.extent[2], lat1 = pro.area.extent[1],
                       lng2 = pro.area.extent[4], lat2 = pro.area.extent[3]) %>%
    leaflet::addPolygons(data = pro_area,
                         fillColor = "transparent",
                         fillOpacity = 0,
                         weight = 3,
                         color = "black",
                         opacity = 1) %>% 
    leaflet.esri::addEsriFeatureLayer(url="https://arcgis.deq.state.or.us/arcgis/rest/services/WQ/WBD/MapServer/1",
                                      options = featureLayerOptions(where = where_huc8),
                                      group = "HUC8",
                                      labelProperty = "Name",
                                      labelOptions = leaflet::labelOptions(style = list("color" = "red",
                                                                                        "font-size" = "20px")),
                                      weight = 3,
                                      color = "red",
                                      opacity = 1,
                                      fillColor = "transparent",
                                      fillOpacity = 0) %>% 
    leaflet.esri::addEsriFeatureLayer(url="https://arcgis.deq.state.or.us/arcgis/rest/services/WQ/WBD/MapServer/2",
                                      options = featureLayerOptions(where = where_huc10),
                                      group = "HUC10",
                                      labelProperty = "Name",
                                      labelOptions = leaflet::labelOptions(style = list("color" = "orange",
                                                                                        "font-size" = "20px")),
                                      weight = 2,
                                      color = "orange",
                                      opacity = 1,
                                      fillColor = "transparent",
                                      fillOpacity = 0) %>% 
    leaflet.esri::addEsriFeatureLayer(url="https://arcgis.deq.state.or.us/arcgis/rest/services/WQ/WBD/MapServer/3",
                                      options = featureLayerOptions(where = where_huc12),
                                      group = "HUC12",
                                      labelProperty = "Name",
                                      labelOptions = leaflet::labelOptions(style = list("color" = "grey",
                                                                                        "font-size" = "20px")),
                                      weight = 1,
                                      color = "grey",
                                      opacity = 1,
                                      fillColor = "transparent",
                                      fillOpacity = 0) %>% 
    leaflet.esri::addEsriFeatureLayer(url="https://arcgis.deq.state.or.us/arcgis/rest/services/WQ/WQ_Assessment_2018_2020/MapServer/3",
                                      options = featureLayerOptions(where = where_huc12),
                                      group = "2018/2020 IR Status - Streams") %>% 
    leaflet.esri::addEsriFeatureLayer(url="https://arcgis.deq.state.or.us/arcgis/rest/services/WQ/WQ_Assessment_2018_2020/MapServer/2",
                                      options = featureLayerOptions(where = where_huc12),
                                      group = "2018/2020 IR Status - Waterbodies") %>% 
    leaflet.esri::addEsriFeatureLayer(url="https://arcgis.deq.state.or.us/arcgis/rest/services/WQ/WQ_Assessment_2018_2020/MapServer/4",
                                      options = featureLayerOptions(where = where_huc12),
                                      group = "2018/2020 IR Status - Watershed") %>% 
    leaflet.esri::addEsriFeatureLayer(url="https://arcgis.deq.state.or.us/arcgis/rest/services/WQ/WQStandards_WM/MapServer/0",
                                      options = featureLayerOptions(where = reachcode),
                                      group = "Fish Use Designations") %>% 
    leaflet.esri::addEsriFeatureLayer(url="https://arcgis.deq.state.or.us/arcgis/rest/services/WQ/WQStandards_WM/MapServer/0",
                                      options = featureLayerOptions(where = reachcode),
                                      group = "Salmon and Steelhead Spawning Use Designations") %>% 
    leaflet::addMarkers(data = temp_stations,
                        group = "Stream Temperature Stations",
                        clusterOptions = markerClusterOptions(),
                        label = paste0(temp_stations$Organization, ": ", temp_stations$Station, " (", temp_stations$`Station ID`,")"),
                        labelOptions = labelOptions(textsize = "15px"),
                        popup = ~paste0("<b>", 
                                        temp_stations$Organization," Station Name: ",
                                        temp_stations$Station,"<br>",
                                        "Station ID: ", temp_stations$`Station ID`,
                                        #"<br>",
                                        #"<br>",
                                        sapply(unique(temp_stations$Station), 
                                               popupTable.temp, USE.NAMES = FALSE)),
                        popupOptions = leaflet::popupOptions(maxWidth = 600, maxHeight = 300)) %>% 
    leaflet::addMarkers(data = temp_cal_sites,
                        group = "Stream Temperature Calibration Sites",
                        clusterOptions = markerClusterOptions(),
                        label = ~`Model Location Name`,
                        labelOptions = labelOptions(textsize = "15px")) %>% 
    leaflet::addMarkers(data = temp_model_bc_tri,
                        group = "Stream Temperature Model Boundary Conditions and Tributary Inputs",
                        clusterOptions = markerClusterOptions(),
                        label = ~`Model Location Name`,
                        labelOptions = labelOptions(textsize = "15px")) %>% 
    leaflet::addMarkers(data = flow_model_bc_tri,
                        group = "Stream Flow Model Boundary Conditions and Tributary Inputs",
                        clusterOptions = markerClusterOptions(),
                        label = ~`Model Location Name`,
                        labelOptions = labelOptions(textsize = "15px")) %>% 
    leaflet::addMarkers(data = flow_stations,
                        group = "Stream Flow Stations",
                        clusterOptions = markerClusterOptions(),
                        label = paste0(flow_stations$`Data Source`, ": ", flow_stations$Station, " (", flow_stations$`Station ID`,")"),
                        labelOptions = labelOptions(textsize = "15px"),
                        popup = ~paste0("<b>", 
                                        flow_stations$`Data Source`," Station Name: ",
                                        flow_stations$Station,"<br>",
                                        "Station ID: ", flow_stations$`Station ID`,
                                        #"<br>",
                                        #"<br>",
                                        sapply(unique(flow_stations$Station), 
                                               popupTable.flow, USE.NAMES = FALSE)),
                        popupOptions = leaflet::popupOptions(maxWidth = 600, maxHeight = 300)) %>% 
    leaflet::addMarkers(data = met_stations,
                        group = "Meteorological Stations",
                        clusterOptions = markerClusterOptions(),
                        label = ~paste0(met_stations$tbl, ": ", met_stations$Station, " (", met_stations$`Station ID`, ")"),
                        labelOptions = labelOptions(textsize = "15px")) %>% 
    leaflet::addMarkers(data = ind_ps,
                        group = "Individual NPDES Point Sources",
                        clusterOptions = markerClusterOptions(),
                        label = ~`Facility Name (Facility Number)`,
                        labelOptions = labelOptions(textsize = "15px"),
                        popup = ~paste0("Facility Name (Facility Number): ", ind_ps$`Facility Name (Facility Number)`,
                                        "<br>", 
                                        "Permit Type and Description: ", ind_ps$`Permit Type and Description`,
                                        "<br>",
                                        "Stream/River Mile: ", ind_ps$`Stream/River Mile`),
                        popupOptions = leaflet::popupOptions(maxWidth = 600, maxHeight = 300)) %>% 
    leaflet::addMiniMap(position = "bottomright",
                        width = 405,
                        height = 250,
                        zoomLevelFixed = 5) %>% 
    leaflet.extras::addResetMapButton() %>% 
    leaflet::addControl(map.title, position = "topleft", className="map-title")
  
  # models ----
  hs.temp.areas <- c("John Day River Basin",
                     "Lower Grande Ronde, Imnaha, and Wallowa Subbasins",
                     "Mid Willamette Subbasins",
                     "North Umpqua Subbasin",
                     "Sandy Subbasin",
                     "South Umpqua and Umpqua Subbasins",
                     "Willow Creek Subbasin")
  
  hs.solar.areas <- c("Malheur River Subbasins")
  
  hs.temp.solar.areas <- c("Middle Columbia-Hood, Miles Creeks",
                           "Walla Walla Subbasin")
  
  hs.temp.solar.ce.areas <- c("Southern Willamette Subbasins")
  
  hs.temp.ce.areas <- c("Lower Willamette and Clackamas Subbasins")
  
  ce.areas <- c("Willamette River Mainstem and Major Tributaries")
  
  hs.temp.sh.areas <- c("Rogue River Basin")
  
  # _ hs.temp.areas ----
  if(qapp_project_area %in% hs.temp.areas) {
    
    # _ _ North Umpqua Subbasin ----
    if (qapp_project_area == "North Umpqua Subbasin"){
      # North Umpqua River model nodes
      n_umpqua_model_nodes <- sf::st_read(dsn = "//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/model_QAPPs/R/data/gis/n_umpqua_river_model_nodes.shp",
                                          layer = "n_umpqua_river_model_nodes")
      
      n_umpqua_model_nodes <- sf::st_transform(n_umpqua_model_nodes, 4326) %>%  sf::st_zm()
      
      map_pro_area <- map %>% 
        leaflet::addPolylines(data = hs_temp_model_extent,
                              group = "Heat Source Temperature Model Extent",
                              label = ~Stream,
                              labelOptions = labelOptions(style = list("color" = "black",
                                                                       "font-size" = "20px")),
                              color = "#045a8d",
                              opacity = 1,
                              weight = 4) %>% 
        leaflet::addCircleMarkers(data = n_umpqua_model_nodes,
                            group = "North Umpqua River Model Nodes",
                            color = "#e34a33", #red orange
                            stroke = FALSE, 
                            fillOpacity = 0.5,
                            label = ~Location,
                            labelOptions = labelOptions(textsize = "15px")) %>% 
        leaflet::addLayersControl(overlayGroups = c("Heat Source Temperature Model Extent",
                                                    "North Umpqua River Model Nodes",
                                                    "HUC8","HUC10","HUC12",
                                                    "2018/2020 IR Status - Streams",
                                                    "2018/2020 IR Status - Waterbodies",
                                                    "2018/2020 IR Status - Watershed",
                                                    "Fish Use Designations",
                                                    "Salmon and Steelhead Spawning Use Designations"),
                                  baseGroups = c("Stream Temperature Stations",
                                                 "Stream Temperature Calibration Sites",
                                                 "Stream Temperature Model Boundary Conditions and Tributary Inputs",
                                                 "Stream Flow Model Boundary Conditions and Tributary Inputs",
                                                 "Stream Flow Stations",
                                                 "Meteorological Stations",
                                                 "Individual NPDES Point Sources"),
                                  options = leaflet::layersControlOptions(collapsed = FALSE, autoZIndex = TRUE)) %>% 
        leaflet::hideGroup(c("North Umpqua River Model Nodes",
                             "HUC8","HUC10","HUC12",
                             "2018/2020 IR Status - Streams",
                             "2018/2020 IR Status - Waterbodies",
                             "2018/2020 IR Status - Watershed",
                             "Fish Use Designations",
                             "Salmon and Steelhead Spawning Use Designations"))
      
      print(paste0(qapp_project_area,"...Save the map"))
      htmlwidgets::saveWidget(map_pro_area, paste0(dir,map.file.name,".html"), 
                              background = "grey", selfcontained = FALSE)
      
    } else {
      
      # _ _ Sandy Subbasin ----
      if (qapp_project_area == "Sandy Subbasin"){
        
        # Sandy River 2016 Heat Source Model Extent
        sandy_2016 <- sf::st_read(dsn = "//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/model_QAPPs/R/data/gis/sandy_2016.shp",
                                  layer = "sandy_2016")
        
        sandy_2016 <- sf::st_transform(sandy_2016, 4326) %>% sf::st_zm() %>% 
          dplyr::mutate(Name = ifelse(Name == "Sandy River", "Sandy River (2016)", "Bull Run River (2016)")) 
        
        map_pro_area <- map %>% 
          leaflet::addPolylines(data = hs_temp_model_extent,
                                group = "Heat Source Temperature Model Extent",
                                label = ~Stream,
                                labelOptions = labelOptions(style = list("color" = "black",
                                                                         "font-size" = "20px")),
                                color = "#045a8d",
                                opacity = 1,
                                weight = 4) %>% 
          leaflet::addPolylines(data = sandy_2016,
                                group = "Sandy River 2016 Heat Source Model Extent",
                                label = ~Name,
                                labelOptions = labelOptions(style = list("color" = "black",
                                                                         "font-size" = "20px")),
                                color = "#993404",
                                opacity = 0.5,
                                weight = 10) %>% 
          leaflet::addLayersControl(overlayGroups = c("Heat Source Temperature Model Extent",
                                                      "Sandy River 2016 Heat Source Model Extent",
                                                      "HUC8","HUC10","HUC12",
                                                      "2018/2020 IR Status - Streams",
                                                      "2018/2020 IR Status - Waterbodies",
                                                      "2018/2020 IR Status - Watershed",
                                                      "Fish Use Designations",
                                                      "Salmon and Steelhead Spawning Use Designations"),
                                    baseGroups = c("Stream Temperature Stations",
                                                   "Stream Temperature Calibration Sites",
                                                   "Stream Temperature Model Boundary Conditions and Tributary Inputs",
                                                   "Stream Flow Model Boundary Conditions and Tributary Inputs",
                                                   "Stream Flow Stations",
                                                   "Meteorological Stations",
                                                   "Individual NPDES Point Sources"),
                                    options = leaflet::layersControlOptions(collapsed = FALSE, autoZIndex = TRUE)) %>% 
          leaflet::hideGroup(c("HUC8","HUC10","HUC12",
                               "2018/2020 IR Status - Streams",
                               "2018/2020 IR Status - Waterbodies",
                               "2018/2020 IR Status - Watershed",
                               "Fish Use Designations",
                               "Salmon and Steelhead Spawning Use Designations"))
        
        print(paste0(qapp_project_area,"...Save the map"))
        htmlwidgets::saveWidget(map_pro_area, paste0(dir,map.file.name,".html"), 
                                background = "grey", selfcontained = FALSE)
      
      } else {
        
        # _ _ other hs.temp basins ----
        map_pro_area <- map %>% 
          leaflet::addPolylines(data = hs_temp_model_extent,
                                group = "Heat Source Temperature Model Extent",
                                label = ~Stream,
                                labelOptions = labelOptions(style = list("color" = "black",
                                                                         "font-size" = "20px")),
                                color = "#045a8d",
                                opacity = 1,
                                weight = 4) %>% 
          leaflet::addLayersControl(overlayGroups = c("Heat Source Temperature Model Extent",
                                                      "HUC8","HUC10","HUC12",
                                                      "2018/2020 IR Status - Streams",
                                                      "2018/2020 IR Status - Waterbodies",
                                                      "2018/2020 IR Status - Watershed",
                                                      "Fish Use Designations",
                                                      "Salmon and Steelhead Spawning Use Designations"),
                                    baseGroups = c("Stream Temperature Stations",
                                                   "Stream Temperature Calibration Sites",
                                                   "Stream Temperature Model Boundary Conditions and Tributary Inputs",
                                                   "Stream Flow Model Boundary Conditions and Tributary Inputs",
                                                   "Stream Flow Stations",
                                                   "Meteorological Stations",
                                                   "Individual NPDES Point Sources"),
                                    options = leaflet::layersControlOptions(collapsed = FALSE, autoZIndex = TRUE)) %>% 
          leaflet::hideGroup(c("HUC8","HUC10","HUC12",
                               "2018/2020 IR Status - Streams",
                               "2018/2020 IR Status - Waterbodies",
                               "2018/2020 IR Status - Watershed",
                               "Fish Use Designations",
                               "Salmon and Steelhead Spawning Use Designations"))
        
        print(paste0(qapp_project_area,"...Save the map"))
        htmlwidgets::saveWidget(map_pro_area, paste0(dir,map.file.name,".html"), 
                                background = "grey", selfcontained = FALSE)
        
      }
      
      
    }
    
  }
    
   
    
 
  
  # _ hs.solar.areas ----
  if(qapp_project_area %in% hs.solar.areas) {
    
    map_pro_area <- map %>% 
      leaflet::addPolylines(data = hs_solar_model_extent,
                            group = "Heat Source Solar Model Extent",
                            label = ~Stream,
                            labelOptions = labelOptions(style = list("color" = "black",
                                                                     "font-size" = "20px")),
                            color = "#3690c0 ",
                            opacity = 1,
                            weight = 4) %>% 
      leaflet::addLayersControl(overlayGroups = c("Heat Source Solar Model Extent",
                                                  "HUC8","HUC10","HUC12",
                                                  "2018/2020 IR Status - Streams",
                                                  "2018/2020 IR Status - Waterbodies",
                                                  "2018/2020 IR Status - Watershed",
                                                  "Fish Use Designations",
                                                  "Salmon and Steelhead Spawning Use Designations"),
                                baseGroups = c("Stream Temperature Stations",
                                               "Stream Temperature Calibration Sites",
                                               "Stream Temperature Model Boundary Conditions and Tributary Inputs",
                                               "Stream Flow Model Boundary Conditions and Tributary Inputs",
                                               "Stream Flow Stations",
                                               "Meteorological Stations",
                                               "Individual NPDES Point Sources"),
                                options = leaflet::layersControlOptions(collapsed = FALSE, autoZIndex = TRUE)) %>% 
      leaflet::hideGroup(c("HUC8","HUC10","HUC12",
                           "2018/2020 IR Status - Streams",
                           "2018/2020 IR Status - Waterbodies",
                           "2018/2020 IR Status - Watershed",
                           "Fish Use Designations",
                           "Salmon and Steelhead Spawning Use Designations"))
    
    print(paste0(qapp_project_area,"...Save the map"))
    htmlwidgets::saveWidget(map_pro_area, paste0(dir,map.file.name,".html"), 
                            background = "grey", selfcontained = FALSE)
    
  }
  
  # _ hs.temp.solar.areas ----
  if(qapp_project_area %in% hs.temp.solar.areas) {
    
    map_pro_area <- map %>% 
      leaflet::addPolylines(data = hs_temp_model_extent,
                            group = "Heat Source Temperature Model Extent",
                            label = ~Stream,
                            labelOptions = labelOptions(style = list("color" = "black",
                                                                     "font-size" = "20px")),
                            color = "#045a8d",
                            opacity = 1,
                            weight = 4) %>% 
      leaflet::addPolylines(data = hs_solar_model_extent,
                            group = "Heat Source Solar Model Extent",
                            label = ~Stream,
                            labelOptions = labelOptions(style = list("color" = "black",
                                                                     "font-size" = "20px")),
                            color = "#3690c0",
                            opacity = 1,
                            weight = 4) %>% 
      leaflet::addLayersControl(overlayGroups = c("Heat Source Temperature Model Extent",
                                                  "Heat Source Solar Model Extent",
                                                  "HUC8","HUC10","HUC12",
                                                  "2018/2020 IR Status - Streams",
                                                  "2018/2020 IR Status - Waterbodies",
                                                  "2018/2020 IR Status - Watershed",
                                                  "Fish Use Designations",
                                                  "Salmon and Steelhead Spawning Use Designations"),
                                baseGroups = c("Stream Temperature Stations",
                                               "Stream Temperature Calibration Sites",
                                               "Stream Temperature Model Boundary Conditions and Tributary Inputs",
                                               "Stream Flow Model Boundary Conditions and Tributary Inputs",
                                               "Stream Flow Stations",
                                               "Meteorological Stations",
                                               "Individual NPDES Point Sources"),
                                options = leaflet::layersControlOptions(collapsed = FALSE, autoZIndex = TRUE)) %>% 
      leaflet::hideGroup(c("HUC8","HUC10","HUC12",
                           "2018/2020 IR Status - Streams",
                           "2018/2020 IR Status - Waterbodies",
                           "2018/2020 IR Status - Watershed",
                           "Fish Use Designations",
                           "Salmon and Steelhead Spawning Use Designations"))
    
    print(paste0(qapp_project_area,"...Save the map"))
    htmlwidgets::saveWidget(map_pro_area, paste0(dir,map.file.name,".html"), 
                            background = "grey", selfcontained = FALSE)
    
  }
  
  # _ hs.temp.solar.ce.areas ----
  
  if(qapp_project_area %in% hs.temp.solar.ce.areas) {
    
    map_pro_area <- map %>% 
      leaflet::addPolylines(data = hs_temp_model_extent,
                            group = "Heat Source Temperature Model Extent",
                            label = ~Stream,
                            labelOptions = labelOptions(style = list("color" = "black",
                                                                     "font-size" = "20px")),
                            color = "#045a8d",
                            opacity = 1,
                            weight = 4) %>% 
      leaflet::addPolygons(data = hs_solar_model_area,
                           group = "Heat Source Solar Model Extent",
                           label = ~Name,
                           labelOptions = labelOptions(style = list("color" = "black",
                                                                    "font-size" = "20px")),
                           fillColor = "#3690c0",
                           fillOpacity = 0.8,
                           weight = 3,
                           color = "#3690c0",
                           opacity = 1) %>% 
      leaflet::addPolylines(data = ce_model_extent,
                            group = "CE-QUAL-W2 Temperature Model Extent",
                            label = ~Stream,
                            labelOptions = labelOptions(style = list("color" = "black",
                                                                     "font-size" = "20px")),
                            color = "#8c510a",
                            opacity = 1,
                            weight = 4) %>% 
      leaflet::addLayersControl(overlayGroups = c("Heat Source Temperature Model Extent",
                                                  "Heat Source Solar Model Extent",
                                                  "CE-QUAL-W2 Temperature Model Extent",
                                                  "HUC8","HUC10","HUC12",
                                                  "2018/2020 IR Status - Streams",
                                                  "2018/2020 IR Status - Waterbodies",
                                                  "2018/2020 IR Status - Watershed",
                                                  "Fish Use Designations",
                                                  "Salmon and Steelhead Spawning Use Designations"),
                                baseGroups = c("Stream Temperature Stations",
                                               "Stream Temperature Calibration Sites",
                                               "Stream Temperature Model Boundary Conditions and Tributary Inputs",
                                               "Stream Flow Model Boundary Conditions and Tributary Inputs",
                                               "Stream Flow Stations",
                                               "Meteorological Stations",
                                               "Individual NPDES Point Sources"),
                                options = leaflet::layersControlOptions(collapsed = FALSE, autoZIndex = TRUE)) %>% 
      leaflet::hideGroup(c("HUC8","HUC10","HUC12",
                           "2018/2020 IR Status - Streams",
                           "2018/2020 IR Status - Waterbodies",
                           "2018/2020 IR Status - Watershed",
                           "Fish Use Designations",
                           "Salmon and Steelhead Spawning Use Designations"))
    
    print(paste0(qapp_project_area,"...Save the map"))
    htmlwidgets::saveWidget(map_pro_area, paste0(dir,map.file.name,".html"), 
                            background = "grey", selfcontained = FALSE)
    
  }
  
  # _ hs.temp.ce.areas ----
  if(qapp_project_area %in% hs.temp.ce.areas) {
    
    map_pro_area <- map %>% 
      leaflet::addPolylines(data = hs_temp_model_extent,
                            group = "Heat Source Temperature Model Extent",
                            label = ~Stream,
                            labelOptions = labelOptions(style = list("color" = "black",
                                                                     "font-size" = "20px")),
                            color = "#045a8d",
                            opacity = 1,
                            weight = 4) %>%
      leaflet::addPolylines(data = ce_model_extent,
                            group = "CE-QUAL-W2 Temperature Model Extent",
                            label = ~Stream,
                            labelOptions = labelOptions(style = list("color" = "black",
                                                                     "font-size" = "20px")),
                            color = "#8c510a",
                            opacity = 1,
                            weight = 4) %>% 
      leaflet::addLayersControl(overlayGroups = c("Heat Source Temperature Model Extent",
                                                  "CE-QUAL-W2 Temperature Model Extent",
                                                  "HUC8","HUC10","HUC12",
                                                  "2018/2020 IR Status - Streams",
                                                  "2018/2020 IR Status - Waterbodies",
                                                  "2018/2020 IR Status - Watershed",
                                                  "Fish Use Designations",
                                                  "Salmon and Steelhead Spawning Use Designations"),
                                baseGroups = c("Stream Temperature Stations",
                                               "Stream Temperature Calibration Sites",
                                               "Stream Temperature Model Boundary Conditions and Tributary Inputs",
                                               "Stream Flow Model Boundary Conditions and Tributary Inputs",
                                               "Stream Flow Stations",
                                               "Meteorological Stations",
                                               "Individual NPDES Point Sources"),
                                options = leaflet::layersControlOptions(collapsed = FALSE, autoZIndex = TRUE)) %>% 
      leaflet::hideGroup(c("HUC8","HUC10","HUC12",
                           "2018/2020 IR Status - Streams",
                           "2018/2020 IR Status - Waterbodies",
                           "2018/2020 IR Status - Watershed",
                           "Fish Use Designations",
                           "Salmon and Steelhead Spawning Use Designations"))
    
    print(paste0(qapp_project_area,"...Save the map"))
    htmlwidgets::saveWidget(map_pro_area, paste0(dir,map.file.name,".html"), 
                            background = "grey", selfcontained = FALSE)
    
  }
  
  # _ ce.areas ----
  
  # _ hs.temp.sh.areas ----
  if(qapp_project_area %in% hs.temp.sh.areas) {
    
    map_pro_area <- map %>% 
      leaflet::addPolylines(data = hs_temp_model_extent,
                            group = "Heat Source Temperature Model Extent",
                            label = ~Stream,
                            labelOptions = labelOptions(style = list("color" = "black",
                                                                     "font-size" = "20px")),
                            color = "#045a8d",
                            opacity = 1,
                            weight = 4) %>% 
      leaflet::addPolylines(data = sh_model_extent,
                            group = "SHADOW Model Extent",
                            label = ~Stname,
                            labelOptions = labelOptions(style = list("color" = "black",
                                                                     "font-size" = "20px")),
                            color = "#8c510a",
                            opacity = 1,
                            weight = 4) %>% 
      leaflet::addLayersControl(overlayGroups = c("Heat Source Temperature Model Extent",
                                                  "SHADOW Model Extent",
                                                  "HUC8","HUC10","HUC12",
                                                  "2018/2020 IR Status - Streams",
                                                  "2018/2020 IR Status - Waterbodies",
                                                  "2018/2020 IR Status - Watershed",
                                                  "Fish Use Designations",
                                                  "Salmon and Steelhead Spawning Use Designations"),
                                baseGroups = c("Stream Temperature Stations",
                                               "Stream Temperature Calibration Sites",
                                               "Stream Temperature Model Boundary Conditions and Tributary Inputs",
                                               "Stream Flow Model Boundary Conditions and Tributary Inputs",
                                               "Stream Flow Stations",
                                               "Meteorological Stations",
                                               "Individual NPDES Point Sources"),
                                options = leaflet::layersControlOptions(collapsed = FALSE, autoZIndex = TRUE)) %>% 
      leaflet::hideGroup(c("HUC8","HUC10","HUC12",
                           "2018/2020 IR Status - Streams",
                           "2018/2020 IR Status - Waterbodies",
                           "2018/2020 IR Status - Watershed",
                           "Fish Use Designations",
                           "Salmon and Steelhead Spawning Use Designations"))
    
    print(paste0(qapp_project_area,"...Save the map"))
    htmlwidgets::saveWidget(map_pro_area, paste0(dir,map.file.name,".html"), 
                            background = "grey", selfcontained = FALSE)
    
  }
  
}
