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

tag.map.title <- tags$style(HTML("
  .leaflet-control.map-title { 
    transform: translate(-50%,20%);
    position: fixed !important;
    left: 50%;
    text-align: center;
    padding-left: 10px; 
    padding-right: 10px; 
    background: rgba(255,255,255,0.75);
    font-weight: bold;
    font-size: 28px;
  }
"))

popupTable.temp <- function(station_name = NULL){
  
  if(station_name %in% map.temp.tbl$Station){
    
    mapTempTbl <- map.temp.tbl %>% 
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

load("//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/model_QAPPs/R/data/RData/map.RData")

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

for (qapp_project_area in project.areas$areas) {
  
  # Shapefile layers ----
  pro.area.extent <- unlist(strsplit(project.areas[which(project.areas$areas == qapp_project_area),]$huc8.extent, split = ","))
  map.file.name <- paste0("map_", project.areas[which(project.areas$areas == qapp_project_area),]$file.name, ".RData")
  #subbasin_huc6 <- unique(lookup.huc[which(lookup.huc$QAPP_Project_Area == qapp_project_area),]$HUC_6)
  subbasin_huc8 <- unique(lookup.huc[which(lookup.huc$QAPP_Project_Area == qapp_project_area),]$HUC_8)
  subbasin_huc10 <- unique(lookup.huc[which(lookup.huc$QAPP_Project_Area == qapp_project_area),]$HUC10)
  subbasin_huc12 <- unique(lookup.huc[which(lookup.huc$QAPP_Project_Area == qapp_project_area),]$HUC12)
  
  pro_area <- pro_areas %>% 
    dplyr::filter(Project_Na == qapp_project_area)
  
  ce_model_extent <- map_ce_model_extent %>% 
    dplyr::filter(Project_Na == qapp_project_area)%>% 
    sf::st_zm() 
  
  hs_temp_model_extent <- map_hs_temp_model_extent %>% 
    dplyr::filter(Project_Na == qapp_project_area)%>% 
    sf::st_zm() 
  
  hs_solar_model_extent <- map_hs_solar_model_extent %>% 
    dplyr::filter(Project_Na == qapp_project_area)%>% 
    sf::st_zm() 
  
  sh_model_extent <- map_sh_model_extent %>%
    sf::st_zm() %>% 
    dplyr::filter(sf::st_contains(pro_area, ., sparse = FALSE))
  
  #tir_extent
  
  # HUC8, 10, 12 ----
  url <- "https://arcgis.deq.state.or.us/arcgis/rest/services/WQ/WBD/MapServer/1/query?"
  huc8 <- NULL
  for(huc_8 in subbasin_huc8){
    request <- httr::GET(url = paste0(url,
                                      "where=HUC8+LIKE+%27",huc_8,"%25%27&",
                                      "geometryType=esriGeometryEnvelope&",
                                      "inSR=4326&",
                                      "spatialRel=esriSpatialRelIntersects&",
                                      "outFields=*&",
                                      "returnGeometry=true&",
                                      "returnTrueCurves=false&",
                                      "returnIdsOnly=false&",
                                      "returnCountOnly=false&",
                                      "returnZ=false&",
                                      "returnM=false&",
                                      "returnDistinctValues=false&",
                                      "returnExtentOnly=false&",
                                      "featureEncoding=esriDefault&",
                                      "f=geojson"))
    response <- httr::content(request, as = "text", encoding = "UTF-8")
    huc8_pro_area <- geojsonsf::geojson_sf(response)
    huc8 <- rbind(huc8,huc8_pro_area)
  }
  
  url <- "https://arcgis.deq.state.or.us/arcgis/rest/services/WQ/WBD/MapServer/2/query?"
  huc10 <- NULL
  for(huc_10 in subbasin_huc10){
    request <- httr::GET(url = paste0(url,
                                      "where=HUC10+LIKE+%27",huc_10,"%25%27&",
                                      "geometryType=esriGeometryEnvelope&",
                                      "inSR=4326&",
                                      "spatialRel=esriSpatialRelIntersects&",
                                      "outFields=*&",
                                      "returnGeometry=true&",
                                      "returnTrueCurves=false&",
                                      "returnIdsOnly=false&",
                                      "returnCountOnly=false&",
                                      "returnZ=false&",
                                      "returnM=false&",
                                      "returnDistinctValues=false&",
                                      "returnExtentOnly=false&",
                                      "featureEncoding=esriDefault&",
                                      "f=geojson"))
    response <- httr::content(request, as = "text", encoding = "UTF-8")
    huc10_pro_area <- geojsonsf::geojson_sf(response)
    huc10 <- rbind(huc10,huc10_pro_area)
  }
  
  url <- "https://arcgis.deq.state.or.us/arcgis/rest/services/WQ/WBD/MapServer/3/query?"
  huc12 <- NULL
  for(huc_12 in subbasin_huc12){
    request <- httr::GET(url = paste0(url,
                                      "where=HUC12+LIKE+%27",huc_12,"%25%27&",
                                      "geometryType=esriGeometryEnvelope&",
                                      "inSR=4326&",
                                      "spatialRel=esriSpatialRelIntersects&",
                                      "outFields=*&",
                                      "returnGeometry=true&",
                                      "returnTrueCurves=false&",
                                      "returnIdsOnly=false&",
                                      "returnCountOnly=false&",
                                      "returnZ=false&",
                                      "returnM=false&",
                                      "returnDistinctValues=false&",
                                      "returnExtentOnly=false&",
                                      "featureEncoding=esriDefault&",
                                      "f=geojson"))
    response <- httr::content(request, as = "text", encoding = "UTF-8")
    huc12_pro_area <- geojsonsf::geojson_sf(response)
    huc12 <- rbind(huc12,huc12_pro_area)
  }
  
  # IR2018/20 ----
  url <- "https://arcgis.deq.state.or.us/arcgis/rest/services/WQ/WQ_Assessment_2018_2020/MapServer/3/query?"
  ir_rivers <- NULL
  for(huc_8 in subbasin_huc8){
    request <- httr::GET(url = paste0(url,
                                      "where=HUC12+LIKE+%27",huc_8,"%25%27&",
                                      "geometryType=esriGeometryEnvelope&",
                                      "inSR=4326&",
                                      "spatialRel=esriSpatialRelIntersects&",
                                      "outFields=*&",
                                      "returnGeometry=true&",
                                      "returnTrueCurves=false&",
                                      "returnIdsOnly=false&",
                                      "returnCountOnly=false&",
                                      "returnZ=false&",
                                      "returnM=false&",
                                      "returnDistinctValues=false&",
                                      "returnExtentOnly=false&",
                                      "featureEncoding=esriDefault&",
                                      "f=geojson"))
    response <- httr::content(request, as = "text", encoding = "UTF-8")
    ir_rivers_pro_area <- geojsonsf::geojson_sf(response)
    ir_rivers <- rbind(ir_rivers,ir_rivers_pro_area)
  }
  ir_rivers <- ir_rivers %>% sf::st_zm() %>% 
    dplyr::filter(grepl("Temperature",Category_5_parameters)) %>% 
    dplyr::filter(!AU_ID %in% c(colum_auid))
  
  url <- "https://arcgis.deq.state.or.us/arcgis/rest/services/WQ/WQ_Assessment_2018_2020/MapServer/2/query?"
  ir_waterbodies <- NULL
  for(huc_8 in subbasin_huc8){
    request <- httr::GET(url = paste0(url,
                                      "where=HUC12+LIKE+%27",huc_8,"%25%27&",
                                      "geometryType=esriGeometryEnvelope&",
                                      "inSR=4326&",
                                      "spatialRel=esriSpatialRelIntersects&",
                                      "outFields=*&",
                                      "returnGeometry=true&",
                                      "returnTrueCurves=false&",
                                      "returnIdsOnly=false&",
                                      "returnCountOnly=false&",
                                      "returnZ=false&",
                                      "returnM=false&",
                                      "returnDistinctValues=false&",
                                      "returnExtentOnly=false&",
                                      "featureEncoding=esriDefault&",
                                      "f=geojson"))
    response <- httr::content(request, as = "text", encoding = "UTF-8")
    ir_waterbodies_pro_area <- geojsonsf::geojson_sf(response)
    ir_waterbodies <- rbind(ir_waterbodies,ir_waterbodies_pro_area)
  }
  ir_waterbodies <- ir_waterbodies %>% sf::st_zm() %>% 
    dplyr::filter(grepl("Temperature",Category_5_parameters)) %>% 
    dplyr::filter(!AU_ID %in% c(colum_auid))

  url <- "https://arcgis.deq.state.or.us/arcgis/rest/services/WQ/WQ_Assessment_2018_2020/MapServer/4/query?"
  ir_watershed <- NULL
  for(huc_8 in subbasin_huc8){
    request <- httr::GET(url = paste0(url,
                                      "where=HUC12+LIKE+%27",huc_8,"%25%27&",
                                      "geometryType=esriGeometryEnvelope&",
                                      "inSR=4326&",
                                      "spatialRel=esriSpatialRelIntersects&",
                                      "outFields=*&",
                                      "returnGeometry=true&",
                                      "returnTrueCurves=false&",
                                      "returnIdsOnly=false&",
                                      "returnCountOnly=false&",
                                      "returnZ=false&",
                                      "returnM=false&",
                                      "returnDistinctValues=false&",
                                      "returnExtentOnly=false&",
                                      "featureEncoding=esriDefault&",
                                      "f=geojson"))
    response <- httr::content(request, as = "text", encoding = "UTF-8")
    ir_watershed_pro_area <- geojsonsf::geojson_sf(response)
    ir_watershed <- rbind(ir_watershed,ir_watershed_pro_area)
  }
  ir_watershed <- ir_watershed %>% sf::st_zm() %>% 
    dplyr::filter(grepl("Temperature",Category_5_parameters)) %>% 
    dplyr::filter(!AU_ID %in% c(colum_auid))

  # Temp WQS ----
  url <- "https://arcgis.deq.state.or.us/arcgis/rest/services/WQ/WQStandards_WM/MapServer/0/query?"
  wqs <- NULL
  for(huc_8 in subbasin_huc8){
    # test: huc_8 <- "17070204"
    query_min <- paste0(huc_8,"000000")
    query_max <- paste0(huc_8,"999999")
    request <- httr::GET(url = paste0(url,
                                      "where=ReachCode+%3E%3D+",query_min,"+AND+ReachCode+%3C%3D",query_max,"&",
                                      "geometryType=esriGeometryEnvelope&",
                                      "inSR=4326&",
                                      "spatialRel=esriSpatialRelIntersects&",
                                      "outFields=*&",
                                      "returnGeometry=true&",
                                      "returnTrueCurves=false&",
                                      "returnIdsOnly=false&",
                                      "returnCountOnly=false&",
                                      "returnZ=false&",
                                      "returnM=false&",
                                      "returnDistinctValues=false&",
                                      "returnExtentOnly=false&",
                                      "featureEncoding=esriDefault&",
                                      "f=geojson"))
    response <- httr::content(request, as = "text", encoding = "UTF-8")
    wqs_pro_area <- geojsonsf::geojson_sf(response)
    wqs <- rbind(wqs,wqs_pro_area)
  }
  
  wqs <- sf::st_transform(wqs, 4326) %>% sf::st_zm()
  
  # Fish Use Designations
  wqs_fish_use <- wqs %>% 
    dplyr::filter(!Temperature_Criteria == "No Salmonid Use/Out of State/Tribal Lands") %>% 
    dplyr::mutate(Temperature_Criteria = gsub("Fish Use -","",Temperature_Criteria)) %>% 
    dplyr::mutate(Temperature_Criteria = trimws(Temperature_Criteria, "left"))
  
  # Spawning Use Designations
  wqs_spawning <- wqs %>% 
    dplyr::filter(!Temperature_Spawn_dates %in% c("No Spawning - No Jurisdiction/Out of State/Tribal Lands","No Spawning - Not Designated")) %>% 
    dplyr::mutate(Temperature_Spawn_dates = ifelse(Temperature_Spawn_dates=="X (No Spawning)", "No Spawning", Temperature_Spawn_dates)) %>% 
    dplyr::mutate(Temperature_Spawn_dates = ifelse(Temperature_Spawn_dates=="Bull Trout Spawning - No designated spawning dates",
                                                   "Bull Trout Spawning and Rearing Habitat", Temperature_Spawn_dates)) 
  
  # Data layers ----
  load(paste0("//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/model_QAPPs/R/data/RData/",map.file.name))
  
    # Map ----
  map.title <- tags$div(tag.map.title, HTML(paste0(qapp_project_area)))
  
  leaflet::leaflet() %>% addTiles() %>% 
    leaflet::fitBounds(lng1 = pro.area.extent[2], lat1 = pro.area.extent[1],
                       lng2 = pro.area.extent[4], lat2 = pro.area.extent[3]) %>%
    leaflet::addPolygons(data = pro_area) %>% 
    leaflet::addPolygons(data = huc8,
                         group = "HUC8") %>% 
    leaflet::addPolygons(data = huc10,
                         group = "HUC10") %>% 
    leaflet::addPolygons(data = huc12,
                         group = "HUC12") %>% 
    leaflet::addPolylines(data = ir_rivers,
                          group = "2018/2020 IR Status - Streams") %>% 
    leaflet::addPolygons(data = ir_waterbodies,
                         group = "2018/2020 IR Status - Waterbodies") %>% 
    leaflet::addPolygons(data = ir_watershed,
                         group = "2018/2020 IR Status - Watershed") %>% 
    #leaflet.esri::addEsriFeatureLayer(url = "https://arcgis.deq.state.or.us/arcgis/rest/services/WQ/WQStandards_WM/MapServer/0",
    #                                  group = "Temp Water Quality Standards (REST)") %>% 
    leaflet::addPolylines(data = wqs_fish_use,
                          group = "Fish Use Designations") %>% 
    leaflet::addPolylines(data = wqs_spawning,
                          group = "Salmon and Steelhead Spawning Use Designations") %>% 
    leaflet::addPolylines(data = hs_temp_model_extent,
                          group = "Heat Source Temperature Model Extent") %>% 
    leaflet::addMarkers(data = temp_stations,
                        group = "Stream Temperature Stations",
                        clusterOptions = markerClusterOptions()) %>% 
    leaflet::addMarkers(data = temp_cal_sites,
                        group = "Stream Temperature Calibration Sites",
                        clusterOptions = markerClusterOptions()) %>% 
    leaflet::addMarkers(data = temp_model_bc_tri,
                        group = "Stream Temperature Model Boundary Conditions and Tributary Inputs",
                        clusterOptions = markerClusterOptions()) %>% 
    leaflet::addMarkers(data = flow_model_bc_tri,
                        group = "Stream Flow Model Boundary Conditions and Tributary Inputs",
                        clusterOptions = markerClusterOptions()) %>% 
    leaflet::addMarkers(data = flow_stations,
                        group = "Flow Stations",
                        clusterOptions = markerClusterOptions()) %>%
    leaflet::addMarkers(data = met_stations,
                        group = "Meteorological Stations",
                        clusterOptions = markerClusterOptions()) %>% 
    leaflet::addMarkers(data = ind_ps,
                        group = "Individual NPDES Point Sources",
                        clusterOptions = markerClusterOptions()) %>% 
    leaflet::addLayersControl(overlayGroups = c("HUC8","HUC10","HUC12",
                                                "2018/2020 IR Status - Streams",
                                                "2018/2020 IR Status - Waterbodies",
                                                "2018/2020 IR Status - Watershed",
                                                "Fish Use Designations",
                                                "Salmon and Steelhead Spawning Use Designations",
                                                "Heat Source Temperature Model Extent"),
                              baseGroups = c("Stream Temperature Stations",
                                             "Stream Temperature Calibration Sites",
                                             "Stream Temperature Model Boundary Conditions and Tributary Inputs",
                                             "Stream Flow Model Boundary Conditions and Tributary Inputs",
                                             "Flow Stations",
                                             "Meteorological Stations",
                                             "Individual NPDES Point Sources"),
                              options = leaflet::layersControlOptions(collapsed = FALSE, autoZIndex = TRUE)) %>% 
    leaflet::hideGroup(c("HUC8","HUC10","HUC12",
                         "2018/2020 IR Status - Streams",
                         "2018/2020 IR Status - Waterbodies",
                         "2018/2020 IR Status - Watershed",
                         "Fish Use Designations",
                         "Salmon and Steelhead Spawning Use Designations")) %>% 
    leaflet::addMiniMap(position = "bottomleft",
                        width = 260,
                        height = 250,
                        zoomLevelFixed = 5) %>% 
    leaflet.extras::addResetMapButton() %>% 
    leaflet::addControl(map.title, position = "topleft", className="map-title")
    
    # 
    
    save 
}