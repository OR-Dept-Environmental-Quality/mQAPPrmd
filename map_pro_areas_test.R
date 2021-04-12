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
  subbasin_huc6 <- unique(lookup.huc[which(lookup.huc$QAPP_Project_Area == qapp_project_area),]$HUC_6)
  subbasin_huc8 <- unique(lookup.huc[which(lookup.huc$QAPP_Project_Area == qapp_project_area),]$HUC_8)
  subbasin_huc10 <- unique(lookup.huc[which(lookup.huc$QAPP_Project_Area == qapp_project_area),]$HUC10)
  subbasin_huc12 <- unique(lookup.huc[which(lookup.huc$QAPP_Project_Area == qapp_project_area),]$HUC12)
  
  pro_area <- pro_areas %>% 
    dplyr::filter(Project_Na == qapp_project_area)
  
  ce_model_extent
  
  hs_temp_model_extent
  
  hs_solar_model_extent
  
  sh_model_extent
  
  tir_extent
  
  # HUC8, 10, 12 ----
  url <- "https://arcgis.deq.state.or.us/arcgis/rest/services/WQ/WBD/MapServer/1/query?"
  request <- httr::GET(url = paste0(url,
                                    "where=HUC8+LIKE+%27",subbasin_huc6,"%25%27&",
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
  huc8 <- geojsonsf::geojson_sf(response) %>% 
    dplyr::filter(HUC8 %in% subbasin_huc8)
  
  url <- "https://arcgis.deq.state.or.us/arcgis/rest/services/WQ/WBD/MapServer/2/query?"
  request <- httr::GET(url = paste0(url,
                                    "where=HUC10+LIKE+%27",subbasin_huc6,"%25%27&",
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
  huc10 <- geojsonsf::geojson_sf(response) %>% 
    dplyr::filter(HUC10 %in% subbasin_huc10)
  
  url <- "https://arcgis.deq.state.or.us/arcgis/rest/services/WQ/WBD/MapServer/3/query?"
  request <- httr::GET(url = paste0(url,
                                    "where=HUC12+LIKE+%27",subbasin_huc6,"%25%27&",
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
  huc12 <- geojsonsf::geojson_sf(response) %>% 
    dplyr::filter(HUC12 %in% subbasin_huc12)
  
  # IR2018/20 ----
  url <- "https://arcgis.deq.state.or.us/arcgis/rest/services/WQ/WQ_Assessment_2018_2020/MapServer/3/query?"
  request <- httr::GET(url = paste0(url,
                                    "where=HUC12+LIKE+%27",subbasin_huc6,"%25%27&",
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
  ir_rivers <- geojsonsf::geojson_sf(response) %>% 
    sf::st_zm() %>% 
    dplyr::filter(HUC12 %in% subbasin_huc12) %>% 
    dplyr::filter(grepl("Temperature",Category_5_parameters)) %>% 
    dplyr::filter(!AU_ID %in% c(colum_auid))
  
  url <- "https://arcgis.deq.state.or.us/arcgis/rest/services/WQ/WQ_Assessment_2018_2020/MapServer/2/query?"
  request <- httr::GET(url = paste0(url,
                                    "where=HUC12+LIKE+%27",subbasin_huc6,"%25%27&",
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
  ir_waterbodies <-  geojsonsf::geojson_sf(response) %>% 
    sf::st_zm() %>% 
    dplyr::filter(HUC12 %in% subbasin_huc12) %>% 
    dplyr::filter(grepl("Temperature",Category_5_parameters)) %>% 
    dplyr::filter(!AU_ID %in% c(colum_auid))
  
  url <- "https://arcgis.deq.state.or.us/arcgis/rest/services/WQ/WQ_Assessment_2018_2020/MapServer/4/query?"
  request <- httr::GET(url = paste0(url,
                                    "where=HUC12+LIKE+%27",subbasin_huc6,"%25%27&",
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
  ir_watershed <-  geojsonsf::geojson_sf(response) %>% 
    sf::st_zm() %>% 
    dplyr::filter(HUC12 %in% subbasin_huc12) %>% 
    dplyr::filter(grepl("Temperature",Category_5_parameters)) %>% 
    dplyr::filter(!AU_ID %in% c(colum_auid))
  
  # Temp WQS ----
  url <- "https://arcgis.deq.state.or.us/arcgis/rest/services/WQ/WQStandards_WM/MapServer/0/query?"
  request <- httr::GET(url = paste0(url,
                                    "where=OBJECTID+<1000000&",
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
  wqs <- geojsonsf::geojson_sf(response) %>% 
    sf::st_zm() %>% 
    dplyr::filter(sf::st_contains(pro_area, ., sparse = FALSE))

  # 
  wqs <- sf::st_read(
    dsn = "//deqhq1/GISLIBRARY/Base_Data/DEQ_Data/Water_Quality/WQ_Standards/GeoRef_Standards.gdb",
    layer = "Oregon_Standards",
    stringsAsFactors = FALSE) %>% 
    sf::st_zm() %>%
    sf::st_transform(4326)
  
  pro_area_wqs <- wqs %>% 
    dplyr::filter(sf::st_contains(pro_area, ., sparse = FALSE)) %>% 
    dplyr::filter(!TempCode == "99")
  
  
  temp_wqs_spawning
  
  temp_wqs_non_spawning
  
  ####
  pro.area.hs.model.extent <- hs.model.extent %>% 
    dplyr::filter(Project_Na == qapp_project_area)
  
  pro.area.sh.model.extent <- sh.model.extent %>% 
    dplyr::filter(Project_Na == qapp_project_area)
  
  pro.area.map.huc8 <- map.huc8 %>% 
    dplyr::filter(HUC_8 %in% unique(lookup_huc[which(lookup_huc$QAPP_Project_Area == qapp_project_area),]$HUC_8))
  
  pro.area.map.huc10 <- map.huc10 %>% 
    #dplyr::filter(Project_Na == qapp_project_area)
    dplyr::filter(HUC_10 %in% unique(lookup_huc[which(lookup_huc$QAPP_Project_Area == qapp_project_area),]$HUC10))
  
  pro.area.map.huc12 <- map.huc12 %>% 
    #dplyr::filter(QAPP_Project_Area == qapp_project_area)
    dplyr::filter(HUC12 %in% unique(lookup_huc[which(lookup_huc$QAPP_Project_Area == qapp_project_area),]$HUC12))
  
  # Data layers ----
  load(paste0("//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/model_QAPPs/R/data/RData/",map.file.name))
  
  
  # Map ----
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
    #leaflet::addPolylines(data = pro_area_wqs,
    #                      group = "Temp Water Quality Standards (Polylines)") %>% 
    leaflet.esri::addEsriFeatureLayer(url = "https://arcgis.deq.state.or.us/arcgis/rest/services/WQ/WQStandards_WM/MapServer/0",
                                      group = "Temp Water Quality Standards (REST)") %>% 
    leaflet::addPolylines(data = ir_rivers,
                          group = "2018/2020 IR Status - Streams") %>% 
    leaflet::addPolygons(data = ir_waterbodies,
                         group = "2018/2020 IR Status - Waterbodies") %>% 
    leaflet::addPolygons(data = ir_watershed,
                         group = "2018/2020 IR Status - Watershed") %>% 
    leaflet::addMarkers(data = temp_stations,
                        group = "Stream Temperature Stations") %>% 
    leaflet::addMarkers(data = temp_cal_sites,
                        group = "Stream Temperature Calibration Sites") %>% 
    leaflet::addMarkers(data = temp_model_bc,
                        group = "Stream Temperature Model Boundary Conditions") %>% 
    leaflet::addMarkers(data = temp_model_tri,
                        group = "Stream Temperature Model Tributary Inputs") %>% 
    leaflet::addMarkers(data = flow_stations,
                        group = "Flow Stations") %>%
    leaflet::addMarkers(data = met_stations,
                        group = "Meteorological Stations") %>% 
    leaflet::addMarkers(data = ind_ps,
                        group = "Individual NPDES Point Sources") %>% 
    leaflet::addLayersControl(overlayGroups = c("HUC8","HUC10","HUC12",
                                                "Temp Water Quality Standards (Polylines)",
                                                "Temp Water Quality Standards (REST)",
                                                "2018/2020 IR Status - Streams",
                                                "2018/2020 IR Status - Waterbodies",
                                                "2018/2020 IR Status - Watershed"),
                              baseGroups = c("Stream Temperature Stations",
                                             "Stream Temperature Calibration Sites",
                                             "Stream Temperature Model Boundary Conditions",
                                             "Stream Temperature Model Tributary Inputs",
                                             "Flow Stations",
                                             "Meteorological Stations",
                                             "Individual NPDES Point Sources"),
                              options = leaflet::layersControlOptions(collapsed = FALSE, autoZIndex = TRUE)) %>% 
    leaflet::hideGroup(c("HUC8","HUC10","HUC12",
                         "Temp Water Quality Standards (Polylines)",
                         "Temp Water Quality Standards (REST)",
                         "2018/2020 IR Status - Streams",
                         "2018/2020 IR Status - Waterbodies",
                         "2018/2020 IR Status - Watershed"))
    
    
    
    save 
}