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
load(paste0(data.dir,"RData/lookup.RData"))

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

#for (qapp_project_area in project.areas[which(!project.areas$areas %in% c("Willamette River Mainstem and Major Tributaries")),]$areas) {

for (qapp_project_area in project.areas$areas) {
  
  print(qapp_project_area)
  
  map.file.name <- paste0("map_", project.areas[which(project.areas$areas == qapp_project_area),]$file.name)
  load(paste0(data.dir,"RData/",map.file.name,".RData")) # data.R
  load(paste0(data.dir,"RData/",map.file.name,"_qapp.RData")) # model_QAPP.Rmd
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
  # _ map_walla ----
  # No ind. NPDES stations
  map_basic <- leaflet::leaflet() %>%
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
    # __ IR ----
  leaflet.esri::addEsriFeatureLayer(url="https://arcgis.deq.state.or.us/arcgis/rest/services/WQ/WQ_Assessment_2018_2020/MapServer/3",
                                    options = leaflet.esri::featureLayerOptions(where = where_huc12),
                                    group = "2018/2020 IR Temperature Status - Streams",
                                    pathOptions = leaflet::pathOptions(pane="ir"),
                                    fill=FALSE) %>% 
    leaflet.esri::addEsriFeatureLayer(url="https://arcgis.deq.state.or.us/arcgis/rest/services/WQ/WQ_Assessment_2018_2020/MapServer/2",
                                      options = leaflet.esri::featureLayerOptions(where = where_huc12),
                                      group = "2018/2020 IR Temperature Status - Waterbodies",
                                      pathOptions = leaflet::pathOptions(pane="ir"),
                                      fill=FALSE) %>% 
    leaflet.esri::addEsriFeatureLayer(url="https://arcgis.deq.state.or.us/arcgis/rest/services/WQ/WQ_Assessment_2018_2020/MapServer/4",
                                      options = leaflet.esri::featureLayerOptions(where = where_huc12),
                                      group = "2018/2020 IR Temperature Status - Watershed",
                                      pathOptions = leaflet::pathOptions(pane="ir"),
                                      fill=FALSE) %>% 
    # __ WQS ----
  leaflet.esri::addEsriFeatureLayer(url="https://arcgis.deq.state.or.us/arcgis/rest/services/WQ/WQStandards_WM/MapServer/0",
                                    options = leaflet.esri::featureLayerOptions(where = reachcode),
                                    group = "Fish Use Designations",
                                    pathOptions = leaflet::pathOptions(pane="huc8")) %>% 
    leaflet.esri::addEsriFeatureLayer(url="https://arcgis.deq.state.or.us/arcgis/rest/services/WQ/WQStandards_WM/MapServer/0",
                                      options = leaflet.esri::featureLayerOptions(where = reachcode),
                                      group = "Salmon and Steelhead Spawning Use Designations",
                                      pathOptions = leaflet::pathOptions(pane="huc8")) %>% 
    # __ Stations ----
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
    leaflet::addMarkers(data = temp_cal_sites,
                        group = "Stream Temperature Calibration Sites",
                        options = leaflet::leafletOptions(pane="marker"),
                        #clusterOptions = markerClusterOptions(),
                        label = paste0(temp_cal_sites$`Model Location Name`, " (", temp_cal_sites$Parameter,")"),
                        labelOptions = labelOptions(textsize = "15px")) %>% 
    leaflet::addMarkers(data = temp_model_bc_tri,
                        group = "Stream Temperature Model Boundary Conditions and Tributary Inputs",
                        options = leaflet::leafletOptions(pane="marker"),
                        #clusterOptions = markerClusterOptions(),
                        label = paste0("Model Location: ",temp_model_bc_tri$`Model Location Name`),
                        labelOptions = labelOptions(textsize = "15px")) %>% 
    leaflet::addMarkers(data = flow_model_bc_tri,
                        group = "Stream Flow Model Boundary Conditions and Tributary Inputs",
                        options = leaflet::leafletOptions(pane="marker"),
                        #clusterOptions = markerClusterOptions(),
                        label = paste0("Model Location: ",flow_model_bc_tri$`Model Location Name`),
                        labelOptions = labelOptions(textsize = "15px")) %>% 
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
    leaflet::addMarkers(data = met_stations,
                        group = "Meteorological Stations",
                        options = leaflet::leafletOptions(pane="marker"),
                        #clusterOptions = markerClusterOptions(),
                        label = paste0(met_stations$tbl, ": ", met_stations$Station, " (", met_stations$`Station ID`, ")"),
                        labelOptions = labelOptions(textsize = "15px")) %>% 
    leaflet::hideGroup(c("HUC8","HUC10","HUC12",
                         "2018/2020 IR Temperature Status - Streams",
                         "2018/2020 IR Temperature Status - Waterbodies",
                         "2018/2020 IR Temperature Status - Watershed",
                         "Fish Use Designations",
                         "Salmon and Steelhead Spawning Use Designations",
                         "Oregon Imagery")) %>% 
    leaflet::addMiniMap(position = "bottomright",
                        width = 405,
                        height = 250,
                        zoomLevelFixed = 5) %>% 
    leaflet.extras::addResetMapButton() %>% 
    leaflet::addControl(map.title, position = "topleft", className="map-title")%>% 
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
  
  if(qapp_project_area %in% c("Walla Walla Subbasin")) {} else {
    
    # _ map ----
    map <- map_basic %>% 
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
                          popupOptions = leaflet::popupOptions(maxWidth = 650, maxHeight = 300)) 
    
  }
  
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
    
    # __ North Umpqua Subbasin ----
    if (qapp_project_area == "North Umpqua Subbasin"){
      # Fish Creek 2009
      fish_creek_2009 <- sf::st_read(dsn = "//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/model_QAPPs/R/data/gis/fish_creek_2009.shp",
                                     layer = "fish_creek_2009")
      fish_creek_2009 <- sf::st_transform(fish_creek_2009, 4326) %>% sf::st_zm()
      
      # North Umpqua River model nodes
      n_umpqua_model_nodes <- sf::st_read(dsn = "//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/model_QAPPs/R/data/gis/n_umpqua_river_model_nodes.shp",
                                          layer = "n_umpqua_river_model_nodes")
      
      n_umpqua_model_nodes <- sf::st_transform(n_umpqua_model_nodes, 4326) %>%sf::st_zm()
      
      map_pro_area <- map %>% 
        leaflet::addPolylines(data = hs_temp_model_extent,
                              group = "Heat Source Temperature Model Extent (Existing Models)",
                              options = leaflet::leafletOptions(pane="mod"),
                              label = ~Stream,
                              labelOptions = labelOptions(style = list("color" = "black",
                                                                       "font-size" = "20px")),
                              color = "#045a8d",
                              opacity = 1,
                              weight = 4) %>% 
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
                                  labelOptions = labelOptions(textsize = "15px"))%>% 
        leaflet::addLayersControl(overlayGroups = c("Heat Source Temperature Model Extent (Existing Models)",
                                                    "Heat Source Temperature Model Extent (New Models)",
                                                    "North Umpqua River Model Nodes",
                                                    "HUC8","HUC10","HUC12",
                                                    "2018/2020 IR Temperature Status - Streams",
                                                    "2018/2020 IR Temperature Status - Waterbodies",
                                                    "2018/2020 IR Temperature Status - Watershed",
                                                    "Fish Use Designations",
                                                    "Salmon and Steelhead Spawning Use Designations",
                                                    "Oregon Imagery"),
                                  baseGroups = c("Stream Temperature Stations",
                                                 "Stream Temperature Calibration Sites",
                                                 "Stream Temperature Model Boundary Conditions and Tributary Inputs",
                                                 "Stream Flow Model Boundary Conditions and Tributary Inputs",
                                                 "Stream Flow Stations",
                                                 "Meteorological Stations",
                                                 "Individual NPDES Point Sources"),
                                  options = leaflet::layersControlOptions(collapsed = FALSE, autoZIndex = TRUE))
    } else {
      
      # __ Sandy Subbasin ----
      if (qapp_project_area == "Sandy Subbasin"){
        
        # Sandy River 2016 Heat Source Model Extent
        sandy_2016 <- sf::st_read(dsn = "//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/model_QAPPs/R/data/gis/sandy_2016.shp",
                                  layer = "sandy_2016")
        
        sandy_2016 <- sf::st_transform(sandy_2016, 4326) %>% sf::st_zm()
        
        map_pro_area <- map %>% 
          leaflet::addPolylines(data = hs_temp_model_extent,
                                group = "Heat Source Temperature Model Extent (Existing Models)",
                                options = leaflet::leafletOptions(pane="mod"),
                                label = ~Stream,
                                labelOptions = labelOptions(style = list("color" = "black",
                                                                         "font-size" = "20px")),
                                color = "#045a8d",
                                opacity = 1,
                                weight = 4) %>% 
          leaflet::addPolylines(data = sandy_2016,
                                group = "Heat Source Temperature Model Extent (New Models)",
                                options = leaflet::leafletOptions(pane="mod2016"),
                                label = ~Name,
                                labelOptions = labelOptions(style = list("color" = "black",
                                                                         "font-size" = "20px")),
                                color = "#993404",
                                opacity = 0.5,
                                weight = 10) %>% 
          leaflet::addLayersControl(overlayGroups = c("Heat Source Temperature Model Extent (Existing Models)",
                                                      "Heat Source Temperature Model Extent (New Models)",
                                                      "HUC8","HUC10","HUC12",
                                                      "2018/2020 IR Temperature Status - Streams",
                                                      "2018/2020 IR Temperature Status - Waterbodies",
                                                      "2018/2020 IR Temperature Status - Watershed",
                                                      "Fish Use Designations",
                                                      "Salmon and Steelhead Spawning Use Designations",
                                                      "Oregon Imagery"),
                                    baseGroups = c("Stream Temperature Stations",
                                                   "Stream Temperature Calibration Sites",
                                                   "Stream Temperature Model Boundary Conditions and Tributary Inputs",
                                                   "Stream Flow Model Boundary Conditions and Tributary Inputs",
                                                   "Stream Flow Stations",
                                                   "Meteorological Stations",
                                                   "Individual NPDES Point Sources"),
                                    options = leaflet::layersControlOptions(collapsed = FALSE, autoZIndex = TRUE))
        
      } else {
        
        # __ other hs.temp basins ----
        map_pro_area <- map %>% 
          leaflet::addPolylines(data = hs_temp_model_extent,
                                group = "Heat Source Temperature Model Extent",
                                options = leaflet::leafletOptions(pane="mod"),
                                label = ~Stream,
                                labelOptions = labelOptions(style = list("color" = "black",
                                                                         "font-size" = "20px")),
                                color = "#045a8d",
                                opacity = 1,
                                weight = 4) %>% 
          leaflet::addLayersControl(overlayGroups = c("Heat Source Temperature Model Extent",
                                                      "HUC8","HUC10","HUC12",
                                                      "2018/2020 IR Temperature Status - Streams",
                                                      "2018/2020 IR Temperature Status - Waterbodies",
                                                      "2018/2020 IR Temperature Status - Watershed",
                                                      "Fish Use Designations",
                                                      "Salmon and Steelhead Spawning Use Designations",
                                                      "Oregon Imagery"),
                                    baseGroups = c("Stream Temperature Stations",
                                                   "Stream Temperature Calibration Sites",
                                                   "Stream Temperature Model Boundary Conditions and Tributary Inputs",
                                                   "Stream Flow Model Boundary Conditions and Tributary Inputs",
                                                   "Stream Flow Stations",
                                                   "Meteorological Stations",
                                                   "Individual NPDES Point Sources"),
                                    options = leaflet::layersControlOptions(collapsed = FALSE, autoZIndex = TRUE))
      }
      
    }
    
  }
  
  # _ hs.solar.areas ----
  if(qapp_project_area %in% hs.solar.areas) {
    
    map_pro_area <- map %>% 
      leaflet::addPolylines(data = hs_solar_model_extent,
                            group = "Heat Source Solar Model Extent",
                            options = leaflet::leafletOptions(pane="mod"),
                            label = ~Stream,
                            labelOptions = labelOptions(style = list("color" = "black",
                                                                     "font-size" = "20px")),
                            color = "#3690c0 ",
                            opacity = 1,
                            weight = 4) %>% 
      leaflet::addLayersControl(overlayGroups = c("Heat Source Solar Model Extent",
                                                  "HUC8","HUC10","HUC12",
                                                  "2018/2020 IR Temperature Status - Streams",
                                                  "2018/2020 IR Temperature Status - Waterbodies",
                                                  "2018/2020 IR Temperature Status - Watershed",
                                                  "Fish Use Designations",
                                                  "Salmon and Steelhead Spawning Use Designations",
                                                  "Oregon Imagery"),
                                baseGroups = c("Stream Temperature Stations",
                                               "Stream Temperature Calibration Sites",
                                               "Stream Temperature Model Boundary Conditions and Tributary Inputs",
                                               "Stream Flow Model Boundary Conditions and Tributary Inputs",
                                               "Stream Flow Stations",
                                               "Meteorological Stations",
                                               "Individual NPDES Point Sources"),
                                options = leaflet::layersControlOptions(collapsed = FALSE, autoZIndex = TRUE))
  }
  
  # _ hs.temp.solar.areas ----
  if(qapp_project_area %in% hs.temp.solar.areas) {
    
    if(qapp_project_area == "Walla Walla Subbasin") {
      
      map_pro_area <- map_basic %>% 
        leaflet::addPolylines(data = hs_temp_model_extent,
                              group = "Heat Source Temperature Model Extent",
                              options = leaflet::leafletOptions(pane="mod"),
                              label = ~Stream,
                              labelOptions = labelOptions(style = list("color" = "black",
                                                                       "font-size" = "20px")),
                              color = "#045a8d",
                              opacity = 1,
                              weight = 4) %>% 
        leaflet::addPolylines(data = hs_solar_model_extent,
                              group = "Heat Source Solar Model Extent",
                              options = leaflet::leafletOptions(pane="mod"),
                              label = ~Stream,
                              labelOptions = labelOptions(style = list("color" = "black",
                                                                       "font-size" = "20px")),
                              color = "#3690c0",
                              opacity = 1,
                              weight = 4) %>% 
        leaflet::addLayersControl(overlayGroups = c("Heat Source Temperature Model Extent",
                                                    "Heat Source Solar Model Extent",
                                                    "HUC8","HUC10","HUC12",
                                                    "2018/2020 IR Temperature Status - Streams",
                                                    "2018/2020 IR Temperature Status - Waterbodies",
                                                    "2018/2020 IR Temperature Status - Watershed",
                                                    "Fish Use Designations",
                                                    "Salmon and Steelhead Spawning Use Designations",
                                                    "Oregon Imagery"),
                                  baseGroups = c("Stream Temperature Stations",
                                                 "Stream Temperature Calibration Sites",
                                                 "Stream Temperature Model Boundary Conditions and Tributary Inputs",
                                                 "Stream Flow Model Boundary Conditions and Tributary Inputs",
                                                 "Stream Flow Stations",
                                                 "Meteorological Stations"),
                                  options = leaflet::layersControlOptions(collapsed = FALSE, autoZIndex = TRUE))
      
    } else {
      
      map_pro_area <- map %>% 
        leaflet::addPolylines(data = hs_temp_model_extent,
                              group = "Heat Source Temperature Model Extent",
                              options = leaflet::leafletOptions(pane="mod"),
                              label = ~Stream,
                              labelOptions = labelOptions(style = list("color" = "black",
                                                                       "font-size" = "20px")),
                              color = "#045a8d",
                              opacity = 1,
                              weight = 4) %>% 
        leaflet::addPolylines(data = hs_solar_model_extent,
                              group = "Heat Source Solar Model Extent",
                              options = leaflet::leafletOptions(pane="mod"),
                              label = ~Stream,
                              labelOptions = labelOptions(style = list("color" = "black",
                                                                       "font-size" = "20px")),
                              color = "#3690c0",
                              opacity = 1,
                              weight = 4) %>% 
        leaflet::addLayersControl(overlayGroups = c("Heat Source Temperature Model Extent",
                                                    "Heat Source Solar Model Extent",
                                                    "HUC8","HUC10","HUC12",
                                                    "2018/2020 IR Temperature Status - Streams",
                                                    "2018/2020 IR Temperature Status - Waterbodies",
                                                    "2018/2020 IR Temperature Status - Watershed",
                                                    "Fish Use Designations",
                                                    "Salmon and Steelhead Spawning Use Designations",
                                                    "Oregon Imagery"),
                                  baseGroups = c("Stream Temperature Stations",
                                                 "Stream Temperature Calibration Sites",
                                                 "Stream Temperature Model Boundary Conditions and Tributary Inputs",
                                                 "Stream Flow Model Boundary Conditions and Tributary Inputs",
                                                 "Stream Flow Stations",
                                                 "Meteorological Stations",
                                                 "Individual NPDES Point Sources"),
                                  options = leaflet::layersControlOptions(collapsed = FALSE, autoZIndex = TRUE))
    }
    
  }
  
  # _ hs.temp.solar.ce.areas ----
  if(qapp_project_area %in% hs.temp.solar.ce.areas) {
    
    map_pro_area <- map %>% 
      leaflet::addPolylines(data = hs_temp_model_extent,
                            group = "Heat Source Temperature Model Extent",
                            options = leaflet::leafletOptions(pane="mod"),
                            label = ~Stream,
                            labelOptions = labelOptions(style = list("color" = "black",
                                                                     "font-size" = "20px")),
                            color = "#045a8d",
                            opacity = 1,
                            weight = 4) %>% 
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
      leaflet::addPolylines(data = ce_model_extent,
                            group = "CE-QUAL-W2 Temperature Model Extent",
                            options = leaflet::leafletOptions(pane="mod"),
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
                                                  "2018/2020 IR Temperature Status - Streams",
                                                  "2018/2020 IR Temperature Status - Waterbodies",
                                                  "2018/2020 IR Temperature Status - Watershed",
                                                  "Fish Use Designations",
                                                  "Salmon and Steelhead Spawning Use Designations",
                                                  "Oregon Imagery"),
                                baseGroups = c("Stream Temperature Stations",
                                               "Stream Temperature Calibration Sites",
                                               "Stream Temperature Model Boundary Conditions and Tributary Inputs",
                                               "Stream Flow Model Boundary Conditions and Tributary Inputs",
                                               "Stream Flow Stations",
                                               "Meteorological Stations",
                                               "Individual NPDES Point Sources"),
                                options = leaflet::layersControlOptions(collapsed = FALSE, autoZIndex = TRUE))
  }
  
  # _ hs.temp.ce.areas ----
  if(qapp_project_area %in% hs.temp.ce.areas) {
    
    map_pro_area <- map %>% 
      leaflet::addPolylines(data = hs_temp_model_extent,
                            group = "Heat Source Temperature Model Extent",
                            options = leaflet::leafletOptions(pane="mod"),
                            label = ~Stream,
                            labelOptions = labelOptions(style = list("color" = "black",
                                                                     "font-size" = "20px")),
                            color = "#045a8d",
                            opacity = 1,
                            weight = 4) %>%
      leaflet::addPolylines(data = ce_model_extent,
                            group = "CE-QUAL-W2 Temperature Model Extent",
                            options = leaflet::leafletOptions(pane="mod"),
                            label = ~Stream,
                            labelOptions = labelOptions(style = list("color" = "black",
                                                                     "font-size" = "20px")),
                            color = "#8c510a",
                            opacity = 1,
                            weight = 4) %>% 
      leaflet::addLayersControl(overlayGroups = c("Heat Source Temperature Model Extent",
                                                  "CE-QUAL-W2 Temperature Model Extent",
                                                  "HUC8","HUC10","HUC12",
                                                  "2018/2020 IR Temperature Status - Streams",
                                                  "2018/2020 IR Temperature Status - Waterbodies",
                                                  "2018/2020 IR Temperature Status - Watershed",
                                                  "Fish Use Designations",
                                                  "Salmon and Steelhead Spawning Use Designations",
                                                  "Oregon Imagery"),
                                baseGroups = c("Stream Temperature Stations",
                                               "Stream Temperature Calibration Sites",
                                               "Stream Temperature Model Boundary Conditions and Tributary Inputs",
                                               "Stream Flow Model Boundary Conditions and Tributary Inputs",
                                               "Stream Flow Stations",
                                               "Meteorological Stations",
                                               "Individual NPDES Point Sources"),
                                options = leaflet::layersControlOptions(collapsed = FALSE, autoZIndex = TRUE))
  }
  
  # _ ce.areas ----
  if(qapp_project_area %in% ce.areas) {
    
    map_pro_area <- map %>% 
      leaflet::addPolylines(data = pro_reaches,
                            group = "CE-QUAL-W2 Temperature Model Extent",
                            options = leaflet::leafletOptions(pane="mod"),
                            label = ~GNIS_Name,
                            labelOptions = labelOptions(style = list("color" = "black",
                                                                     "font-size" = "20px")),
                            color = "#8c510a",
                            opacity = 1,
                            weight = 4) %>% 
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
      leaflet::addLayersControl(overlayGroups = c("CE-QUAL-W2 Temperature Model Extent",
                                                  "HUC8","HUC10","HUC12",
                                                  "2018/2020 IR Temperature Status - Streams",
                                                  "2018/2020 IR Temperature Status - Waterbodies",
                                                  "2018/2020 IR Temperature Status - Watershed",
                                                  "Fish Use Designations",
                                                  "Salmon and Steelhead Spawning Use Designations",
                                                  "Oregon Imagery"),
                                baseGroups = c("Stream Temperature Stations",
                                               #"Stream Temperature Calibration Sites",
                                               #"Stream Temperature Model Boundary Conditions and Tributary Inputs",
                                               #"Stream Flow Model Boundary Conditions and Tributary Inputs",
                                               "Stream Flow Stations",
                                               "Gage Height Stations",
                                               "Meteorological Stations",
                                               "Individual NPDES Point Sources"),
                                options = leaflet::layersControlOptions(collapsed = FALSE, autoZIndex = TRUE))
    
  }
  
  # _ hs.temp.sh.areas ----
  if(qapp_project_area %in% hs.temp.sh.areas) {
    
    map_pro_area <- map %>% 
      leaflet::addPolylines(data = hs_temp_model_extent,
                            group = "Heat Source Temperature Model Extent",
                            options = leaflet::leafletOptions(pane="mod"),
                            label = ~Stream,
                            labelOptions = labelOptions(style = list("color" = "black",
                                                                     "font-size" = "20px")),
                            color = "#045a8d",
                            opacity = 1,
                            weight = 4) %>% 
      leaflet::addPolylines(data = sh_model_extent,
                            group = "SHADOW Model Extent",
                            options = leaflet::leafletOptions(pane="mod"),
                            label = ~Stname,
                            labelOptions = labelOptions(style = list("color" = "black",
                                                                     "font-size" = "20px")),
                            color = "#8c510a",
                            opacity = 1,
                            weight = 4) %>% 
      leaflet::addLayersControl(overlayGroups = c("Heat Source Temperature Model Extent",
                                                  "SHADOW Model Extent",
                                                  "HUC8","HUC10","HUC12",
                                                  "2018/2020 IR Temperature Status - Streams",
                                                  "2018/2020 IR Temperature Status - Waterbodies",
                                                  "2018/2020 IR Temperature Status - Watershed",
                                                  "Fish Use Designations",
                                                  "Salmon and Steelhead Spawning Use Designations",
                                                  "Oregon Imagery"),
                                baseGroups = c("Stream Temperature Stations",
                                               "Stream Temperature Calibration Sites",
                                               "Stream Temperature Model Boundary Conditions and Tributary Inputs",
                                               "Stream Flow Model Boundary Conditions and Tributary Inputs",
                                               "Stream Flow Stations",
                                               "Meteorological Stations",
                                               "Individual NPDES Point Sources"),
                                options = leaflet::layersControlOptions(collapsed = FALSE, autoZIndex = TRUE))
  }
  
  print(paste0(qapp_project_area,"...Save the map"))
  htmlwidgets::saveWidget(map_pro_area, paste0(dir,map.file.name,".html"), 
                          background = "grey", selfcontained = TRUE)
  
}
