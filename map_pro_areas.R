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

load("//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/model_QAPPs/R/data/RData/map.RData") # RData created in data.R

# Available data check ----
# Don't need to run this for the leaflet map
results <- NULL

for (qapp_project_area in qapp_project_areas$areas) {
  
  pro.area.extent <- unlist(strsplit(qapp_project_areas[which(qapp_project_areas$areas == qapp_project_area),]$huc8.extent, split = ","))
  
  pro.area <- pro.areas %>% 
    dplyr::filter(Project_Na == qapp_project_area)
  
  pro.area.temp.model.streams <- temp.model.streams %>% 
    dplyr::filter(Project_Na == qapp_project_area)
  
  pro.area.shadow.model.streams <- shadow.model.streams %>% 
    dplyr::filter(Project_Na == qapp_project_area)
  
  pro.area.map.huc8 <- map.huc8 %>% 
    dplyr::filter(HU_8_NAME %in% unique(lookup_huc[which(lookup_huc$QAPP_Project_Area == qapp_project_area),]$HUC8_NAME))
  
  pro.area.map.huc10 <- map.huc10 %>% 
    dplyr::filter(HU_10_NAME %in% unique(lookup_huc[which(lookup_huc$QAPP_Project_Area == qapp_project_area),]$HUC10_NAME))
  
  pro.area.map.huc12 <- map.huc12 %>% 
    dplyr::filter(HUC12_Name %in% unique(lookup_huc[which(lookup_huc$QAPP_Project_Area == qapp_project_area),]$HUC12_Name))
  
  map.temp.pro.area <- map.temp.pro %>% 
    dplyr::filter(Project_Na == qapp_project_area)
  
  map.flow.pro.area <- map.flow.pro %>% 
    dplyr::filter(Project_Na == qapp_project_area)
  
  map.met.pro.area <- map.met.pro %>% 
    dplyr::filter(Project_Na == qapp_project_area)
  
  map.ind.npdes.pro.area <- map.ind.npdes.pro %>% 
    dplyr::filter(Project_Na == qapp_project_area)
  
  results <- rbind(results,
                   data.frame(proj_area = qapp_project_area,
                              temp_model = NROW(pro.area.temp.model.streams),
                              shadow_model = NROW(pro.area.shadow.model.streams),
                              huc8 = NROW(pro.area.map.huc8),
                              huc10 = NROW(pro.area.map.huc10),
                              huc12 = NROW(pro.area.map.huc12),
                              temp = NROW(map.temp.pro.area),
                              flow = NROW(map.flow.pro.area),
                              met = NROW(map.met.pro.area),
                              ind = NROW(map.ind.npdes.pro.area)))

}
# write.csv(results, "results.csv")

# Project Area Data ----

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
# qapp_project_area = "Walla Walla Subbasin" ############### missing ind NPDES
# qapp_project_area = "Willamette River Mainstem and Major Tributaries"
# qapp_project_area = "Willow Creek Subbasin"

# _ No Walla Walla ----
qapp_project_areas <- qapp_project_areas[-c(5,12),]

for (qapp_project_area in qapp_project_areas$areas) {
  
  pro.area.extent <- unlist(strsplit(qapp_project_areas[which(qapp_project_areas$areas == qapp_project_area),]$huc8.extent, split = ","))
  map.file.name <- qapp_project_areas[which(qapp_project_areas$areas == qapp_project_area),]$file.name
  
  pro.area <- pro.areas %>% 
    dplyr::filter(Project_Na == qapp_project_area)
  
  pro.area.temp.model.streams <- temp.model.streams %>% 
    dplyr::filter(Project_Na == qapp_project_area)
  
  pro.area.shadow.model.streams <- shadow.model.streams %>% 
    dplyr::filter(Project_Na == qapp_project_area)
  
  pro.area.map.huc8 <- map.huc8 %>% 
    dplyr::filter(HU_8_NAME %in% unique(lookup_huc[which(lookup_huc$QAPP_Project_Area == qapp_project_area),]$HUC8_NAME))
  
  pro.area.map.huc10 <- map.huc10 %>% 
    dplyr::filter(HU_10_NAME %in% unique(lookup_huc[which(lookup_huc$QAPP_Project_Area == qapp_project_area),]$HUC10_NAME))
  
  pro.area.map.huc12 <- map.huc12 %>% 
    dplyr::filter(HUC12_Name %in% unique(lookup_huc[which(lookup_huc$QAPP_Project_Area == qapp_project_area),]$HUC12_Name))
  
  map.temp.pro.area <- map.temp.pro %>% 
    dplyr::filter(Project_Na == qapp_project_area)
  
  map.flow.pro.area <- map.flow.pro %>% 
    dplyr::filter(Project_Na == qapp_project_area)
  
  map.met.pro.area <- map.met.pro %>% 
    dplyr::filter(Project_Na == qapp_project_area)
  
  map.ind.npdes.pro.area <- map.ind.npdes.pro %>% 
    dplyr::filter(Project_Na == qapp_project_area)
  
  # Leaflet Map ----
  map.title <- tags$div(tag.map.title, HTML(paste0(qapp_project_area)))
  
  map.pro.area <- leaflet::leaflet() %>% leaflet::addTiles() %>% 
    #leaflet.esri::addEsriBasemapLayer(esriBasemapLayers$Imagery) %>%
    leaflet::fitBounds(lng1 = pro.area.extent[2], lat1 = pro.area.extent[1],
                       lng2 = pro.area.extent[4], lat2 = pro.area.extent[3]) %>% 
    #leaflet.esri::addEsriImageMapLayer(url="https://imagery.oregonexplorer.info/arcgis/rest/services/OSIP_2018/OSIP_2018_WM/ImageServer",
     #                                  #group = "Oregon Imagery",
      #                                 options = leaflet::leafletOptions(pane="Aerial2")) %>%
    #leaflet.esri::addEsriImageMapLayer(url="https://imagery.oregonexplorer.info/arcgis/rest/services/OSIP_2017/OSIP_2017_WM/ImageServer",
    #                                   group = "Oregon Imagery",
    #                                   options = leaflet::leafletOptions(pane="Aerial2")) %>%
    
    leaflet::addPolygons(data = pro.area,
                         label = ~Project_Na,
                         labelOptions = labelOptions(style = list("color" = "blue",
                                                                  "font-size" = "20px")),
                         fillColor = "transparent",
                         fillOpacity = 0,
                         weight = 3,
                         color = "black",
                         opacity = 1) %>% 
    leaflet::addPolylines(data = pro.area.temp.model.streams,
                          group = "Temperature Model Extent",
                          label = ~Stream,
                          labelOptions = labelOptions(style = list("color" = "blue",
                                                                   "font-size" = "20px")),
                          color = "#6baed6",
                          opacity = 1,
                          weight = 4) %>% 
    leaflet::addPolylines(data = pro.area.shadow.model.streams,
                          group = "Shadown Model Extent",
                          label = ~Stream,
                          labelOptions = labelOptions(style = list("color" = "blue",
                                                                   "font-size" = "20px")),
                          color = "#2ca25f",
                          opacity = 1,
                          weight = 4) %>% 
    leaflet::addPolygons(data = pro.area.map.huc8,
                         group = "HUC 8",
                         label = ~HU_8_NAME,
                         labelOptions = labelOptions(style = list("color" = "red",
                                                                  "font-size" = "20px")),
                         weight = 3,
                         color = "red",
                         opacity = 1,
                         fillColor = "transparent",
                         fillOpacity = 0,
                         highlightOptions = highlightOptions(fillColor = "red",
                                                             fillOpacity = 0.2,
                                                             weight = 3,
                                                             bringToFront = TRUE)) %>% 
    leaflet::addPolygons(data = pro.area.map.huc10,
                         group = "HUC 10",
                         label = ~HU_10_NAME,
                         labelOptions = labelOptions(style = list("color" = "orange",
                                                                  "font-size" = "20px")),
                         weight = 2,
                         color = "orange",
                         opacity = 1,
                         fillColor = "transparent",
                         fillOpacity = 0,
                         highlightOptions = highlightOptions(fillColor = "orange",
                                                             fillOpacity = 0.2,
                                                             weight = 3,
                                                             bringToFront = TRUE)) %>%
    leaflet::addPolygons(data = pro.area.map.huc12,
                         group = "HUC 12",
                         label = ~HUC12_Name,
                         labelOptions = labelOptions(style = list("color" = "grey",
                                                                  "font-size" = "20px")),
                         weight = 1,
                         color = "grey",
                         opacity = 1,
                         fillColor = "transparent",
                         fillOpacity = 0,
                         highlightOptions = highlightOptions(fillColor = "grey",
                                                             fillOpacity = 0.2,
                                                             weight = 3,
                                                             bringToFront = TRUE)) %>%
    leaflet::addMarkers(data = map.temp.pro.area,
                        group = "Stream Temperature Stations",
                        clusterOptions = markerClusterOptions(),
                        label = paste0(map.temp.pro.area$Organization, ": ", map.temp.pro.area$`Station Name and ID`),
                        labelOptions = labelOptions(textsize = "15px")) %>% 
    leaflet::addMarkers(data = map.flow.pro.area,
                        group = "Flow Stations",
                        clusterOptions = markerClusterOptions(),
                        label = ~Station_Des,
                        labelOptions = labelOptions(textsize = "15px")) %>% 
    leaflet::addMarkers(data = map.met.pro.area,
                        group = "Meteorological Stations",
                        clusterOptions = markerClusterOptions(),
                        label = ~Station,
                        labelOptions = labelOptions(textsize = "15px")) %>%
    leaflet::addMarkers(data = map.ind.npdes.pro.area,
                        group = "Individual NPDES Point Sources",
                        clusterOptions = markerClusterOptions(),
                        label = ~`Facility Name and Number`,
                        labelOptions = labelOptions(textsize = "15px")) %>%
    leaflet::addLayersControl(overlayGroups = c("Temperature Model Streams","Shadown Model Streams","HUC 8", "HUC 10", "HUC 12","Oregon Imagery"),
                              baseGroups = c("Stream Temperature Stations","Flow Stations","Meteorological Stations","Individual NPDES Point Sources"),
                              options = leaflet::layersControlOptions(collapsed = FALSE, autoZIndex = TRUE)) %>% 
    leaflet::hideGroup(c("HUC 8","HUC 10", "HUC 12")) %>% 
    leaflet::addMiniMap(position = "topright",
                        width = 210,
                        height = 200,
                        zoomLevelFixed = 5) %>% 
    leaflet.extras::addResetMapButton() %>% 
    leaflet::addControl(map.title, position = "topleft", className="map-title")
  
  
  map.pro.area
  
  # Save Map ----
  dir <-  "//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/model_QAPPs/R/test_doc/20201123/project_area_maps/"
  htmlwidgets::saveWidget(map.pro.area, paste0(dir,map.file.name,".html"), 
                          background = "grey", selfcontained = FALSE)
  
}
