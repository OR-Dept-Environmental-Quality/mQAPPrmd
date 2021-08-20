library(tidyverse)
library(stringr)
library(leaflet)
library(leaflet.extras)
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

# dir <-  "//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/model_QAPPs/R/map/project_area_maps/"

title <- tags$div(tag.map.title, HTML("Temperature TMDL Replacement Project"))

map <- leaflet::leaflet() %>% leaflet::addTiles() %>% 
  leaflet::setView(lng = -121, lat = 44, zoom=7.2) %>%
  leaflet::addPolygons(data = pro_areas,
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
  leaflet::addPolygons(data = pro_areas,
                       group = "Completion Schedule",
                       label = ~Project_Na,
                       fillColor = ~color,
                       weight = 1,
                       color = "black",
                       opacity = 1,
                       fillOpacity = 1) %>% 
  leaflet::addPolylines(data = sf::st_zm(pro.reaches),
                        group = "Completion Schedule",
                        label = ~Project_Na,
                        color = ~color,
                        opacity = 0.7,
                        weight = 2) %>% 
  leaflet::addPolylines(data = temp.model.streams,
                        group = "Temperature Model Streams",
                        label = ~Stream,
                        color = "#6baed6",
                        opacity = 1,
                        weight = 3) %>% 
  leaflet::addPolylines(data = shadow.model.streams,
                        group = "SHADOW Model Streams",
                        label = ~Stream,
                        color = "green",
                        opacity = 1,
                        weight = 3) %>% 
  leaflet::addPolygons(data = map.huc8,
                       group = "HUC 8",
                       label = ~HU_8_NAME,
                       weight = 0.8,
                       color = "red",
                       opacity = 1,
                       fillColor = "transparent",
                       fillOpacity = 0,
                       highlightOptions = highlightOptions(fillColor = "red",
                                                           fillOpacity = 0.2,
                                                           weight = 3,
                                                           bringToFront = TRUE)) %>% 
  leaflet::addPolygons(data = map.huc10,
                       group = "HUC 10",
                       label = ~HU_10_NAME,
                       weight = 0.6,
                       color = "orange",
                       opacity = 1,
                       fillColor = "transparent",
                       fillOpacity = 0,
                       highlightOptions = highlightOptions(fillColor = "orange",
                                                           fillOpacity = 0.2,
                                                           weight = 3,
                                                           bringToFront = TRUE)) %>%
  leaflet::addPolygons(data = map.huc12,
                       group = "HUC 12",
                       label = ~HUC12_Name,
                       weight = 0.5,
                       color = "grey",
                       opacity = 1,
                       fillColor = "transparent",
                       fillOpacity = 0,
                       highlightOptions = highlightOptions(fillColor = "grey",
                                                           fillOpacity = 0.2,
                                                           weight = 3,
                                                           bringToFront = TRUE)) %>%
  leaflet::addLayersControl(overlayGroups = c("Project Areas","Temperature Model Streams","SHADOW Model Streams","HUC 8", "HUC 10", "HUC 12", "Completion Schedule"),
                            options = leaflet::layersControlOptions(collapsed = FALSE, autoZIndex = TRUE)) %>% 
  leaflet::hideGroup(c("HUC 8","HUC 10", "HUC 12", "Completion Schedule")) %>% 
  leaflet::addLegend(data = pro_areas,
                     position = "topright",
                     colors = ~unique(pro_areas$color),
                     labels = ~unique(pro_areas$CompleteD),
                     title = "Completion Date") %>% 
  leaflet::addMiniMap(position = "bottomright",
                      width = 200,
                      height = 200,
                      zoomLevelFixed = 5) %>% 
  leaflet.extras::addResetMapButton() %>% 
  leaflet::addControl(title, position = "topleft", className="map-title")


#map


# Save Map ----
dir <-  "//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/model_QAPPs/R/map/"
htmlwidgets::saveWidget(map, paste0(dir,"map.html"), 
                        title = paste("Temperature TMDL Replacement Project"), 
                        background = "grey", selfcontained = FALSE)
