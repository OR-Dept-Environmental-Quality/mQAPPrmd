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

# Shapefile layers ----
for (qapp_project_area in qapp_project_areas$areas) {
  
  pro.area.extent <- unlist(strsplit(qapp_project_areas[which(qapp_project_areas$areas == qapp_project_area),]$huc8.extent, split = ","))
  map.file.name <- qapp_project_areas[which(qapp_project_areas$areas == qapp_project_area),]$file.name
  
  pro.area <- pro.areas %>% 
    dplyr::filter(Project_Na == qapp_project_area)
  
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
  
  
}

# Leaflet map ----
map.title <- tags$div(tag.map.title, HTML(paste0(qapp_project_area)))

map.pro.area.base <- leaflet::leaflet() %>% leaflet::addTiles() %>% 
    leaflet::addPolygons(data = pro.area,
                       label = ~Project_Na,
                       labelOptions = labelOptions(style = list("color" = "blue",
                                                                "font-size" = "20px")),
                       fillColor = "transparent",
                       fillOpacity = 0,
                       weight = 3,
                       color = "black",
                       opacity = 1) %>% 
  leaflet::addPolylines(data = pro.area.hs.model.extent,
                        group = "Heat Source Temperature Model Extent",
                        label = ~Stream,
                        labelOptions = labelOptions(style = list("color" = "black",
                                                                 "font-size" = "20px")),
                        color = "#6baed6",
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
  leaflet::addPolygons(data = pro.areas,
                       group = "Project Areas",
                       label = ~Project_Na,
                       popup = ~map_link,
                       fillColor = "transparent",
                       fillOpacity = 0,
                       weight = 2,
                       color = "black",
                       opacity = 1,
                       highlightOptions = highlightOptions(fillColor = "blue",
                                                           fillOpacity = 0.2,
                                                           weight = 3,
                                                           bringToFront = TRUE)) %>%
  leaflet::hideGroup(c("HUC 8","HUC 10", "HUC 12")) %>% 
  leaflet::addMiniMap(position = "topright",
                      width = 210,
                      height = 200,
                      zoomLevelFixed = 5) %>% 
  leaflet.extras::addResetMapButton() %>% 
  leaflet::addControl(map.title, position = "topleft", className="map-title")

if(NROW(pro.area.sh.model.extent)>0){
 
  map.pro.area.base <- map.pro.area.base %>% 
    leaflet::fitBounds(lng1 = pro.area.extent[2], lat1 = pro.area.extent[1],
                       lng2 = pro.area.extent[4], lat2 = pro.area.extent[3]) %>% 
    leaflet::addPolylines(data = pro.area.sh.model.extent,
                          group = "SHADOW Model Extent",
                          label = ~Stream,
                          labelOptions = labelOptions(style = list("color" = "black",
                                                                   "font-size" = "20px")),
                          color = "#2ca25f",
                          opacity = 1,
                          weight = 4) %>% 
    leaflet::addLayersControl(overlayGroups = c("Project Areas","Heat Source Model Extent","SHADOW Model Extent","HUC 8", "HUC 10", "HUC 12","Oregon Imagery"),
                              baseGroups = c("Stream Temperature Stations","Flow Stations","Meteorological Stations","Individual NPDES Point Sources"),
                              options = leaflet::layersControlOptions(collapsed = FALSE, autoZIndex = TRUE))
  
} else {
  
  map.pro.area.base <- map.pro.area.base %>% 
    leaflet::fitBounds(lng1 = pro.area.extent[2], lat1 = pro.area.extent[1],
                       lng2 = pro.area.extent[4], lat2 = pro.area.extent[3]) %>% 
    leaflet::addLayersControl(overlayGroups = c("Project Areas","Heat Source Model Extent","HUC 8", "HUC 10", "HUC 12","Oregon Imagery"),
                              baseGroups = c("Stream Temperature Stations","Flow Stations","Meteorological Stations","Individual NPDES Point Sources"),
                              options = leaflet::layersControlOptions(collapsed = FALSE, autoZIndex = TRUE))
}

if(NROW(pro.area.sh.model.extent)>0){
  
  map.pro.area.base <- map.pro.area.base %>% 
    leaflet::fitBounds(lng1 = pro.area.extent[2], lat1 = pro.area.extent[1],
                       lng2 = pro.area.extent[4], lat2 = pro.area.extent[3]) %>% 
    leaflet::addPolylines(data = pro.area.sh.model.extent,
                          group = "SHADOW Model Extent",
                          label = ~Stream,
                          labelOptions = labelOptions(style = list("color" = "black",
                                                                   "font-size" = "20px")),
                          color = "#2ca25f",
                          opacity = 1,
                          weight = 4) %>% 
    leaflet::addLayersControl(overlayGroups = c("Project Areas","Heat Source Model Extent","SHADOW Model Extent","HUC 8", "HUC 10", "HUC 12","Oregon Imagery"),
                              baseGroups = c("Stream Temperature Stations","Flow Stations","Meteorological Stations","Individual NPDES Point Sources"),
                              options = leaflet::layersControlOptions(collapsed = FALSE, autoZIndex = TRUE))
  
} else {
  
  map.pro.area.base <- map.pro.area.base %>% 
    leaflet::fitBounds(lng1 = pro.area.extent[2], lat1 = pro.area.extent[1],
                       lng2 = pro.area.extent[4], lat2 = pro.area.extent[3]) %>% 
    leaflet::addLayersControl(overlayGroups = c("Project Areas","Heat Source Model Extent","HUC 8", "HUC 10", "HUC 12","Oregon Imagery"),
                              baseGroups = c("Stream Temperature Stations","Flow Stations","Meteorological Stations","Individual NPDES Point Sources"),
                              options = leaflet::layersControlOptions(collapsed = FALSE, autoZIndex = TRUE))
}  

map.pro.area.base
