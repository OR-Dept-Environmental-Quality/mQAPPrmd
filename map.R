# ____master____ ----
library(tidyverse)
library(stringr)
library(leaflet)
library(leaflet.extras)
library(sf)
library(tigris)
options(tigris_use_cache = TRUE)
library(htmltools)
library(htmlwidgets)

# DEQ logo ----
logo <- base64enc::base64encode("//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/model_QAPPs/R/data/DEQ-logo-color-non-transp71x107.png")

# QAPP Project Areas ----
pro.areas <- sf::read_sf(dsn = "//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/model_QAPPs/R/data/gis/project_extent.shp",
                         layer = "project_extent")

pro.areas <- sf::st_transform(pro.areas, 4326)

pro.areas <- pro.areas %>% 
  dplyr::mutate(color = dplyr::case_when(Project_Na == "Applegate, Illinois, Lower Rogue, and Middle Rogue Subbasins" ~ "#df65b0", #pink
                                         Project_Na == "John Day River Basin" ~ "#df65b0", #pink
                                         Project_Na == "Lower Grande Ronde, Imnaha, and Wallowa Subbasins" ~ "yellow",
                                         Project_Na == "Lower Willamette, Clackamas, and Sandy Subbasins" ~ "#253494", #blue
                                         Project_Na == "Malheur River Subbasins" ~ "#78c679",
                                         Project_Na == "Mid Willamette Subbasins" ~ "#253494", #blue
                                         Project_Na == "Middle Columbia-Hood, Miles Creeks" ~ "yellow",
                                         Project_Na == "North Umpqua Subbasin" ~ "purple",
                                         Project_Na == "South Umpqua and Umpqua Subbasins" ~ "purple",
                                         Project_Na == "Southern Willamette Subbasins" ~ "#253494", #blue
                                         Project_Na == "Upper Rogue Subbasin" ~ "#df65b0", #pink
                                         Project_Na == "Walla Walla Subbasin" ~ "#78c679", #green
                                         Project_Na == "Willow Creek Subbasin" ~ "#78c679")) #green

pro.reaches <- sf::read_sf(dsn = "//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/model_QAPPs/R/data/gis/project_reach_extent.shp",
                           layer = "project_reach_extent")

pro.reaches <- sf::st_zm(pro.reaches, drop = T, what = "ZM")

pro.reaches <- sf::st_transform(pro.reaches, 4326)

pro.reaches <- pro.reaches %>% 
  dplyr::mutate(color = dplyr::case_when(Project_Na == "Willamette River Mainstem and Major Tributaries"~ "purple",
                                         Project_Na == "Snake River – Hells Canyon"~ "yellow"))

# Model Segments ----
model.streams <- sf::read_sf(dsn = "//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/model_QAPPs/R/data/gis/temp_model_streams_temp_projects.shp",
                         layer = "temp_model_streams_temp_projects")

model.streams <- sf::st_transform(model.streams, 4326)

# HUC 8,10,12 ----
map.huc8 <- sf::read_sf(dsn = "//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/GIS/Study_Areas_v5_HUC8_scope.shp",
                        layer = "Study_Areas_v5_HUC8_scope")

map.huc8  <- sf::st_transform(map.huc8 , 4326)

map.huc10 <- sf::read_sf(dsn = "//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/GIS/Study_Areas_v5_HUC10_scope.shp",
                         layer = "Study_Areas_v5_HUC10_scope")

map.huc10 <- sf::st_transform(map.huc10, 4326)

map.huc12 <- sf::read_sf(dsn = "//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/model_QAPPs/R/data/gis/project_huc12.shp",
                         layer = "project_huc12") %>% 
  dplyr::select(HUC12,Name,geometry) %>% 
  dplyr::rename(HUC12_Name = Name)
  

map.huc12 <- tigris::geo_join(map.huc12, lookup_huc, by_sp = "HUC12_Name", by_df = "HUC12_Name", how = "left")

map.huc12 <- sf::st_transform(map.huc12, 4326)

# Stream Temperature ----
temp.awqms <- df.stations.state %>% 
  dplyr::rename(`Station ID` = MLocID) %>% 
  dplyr::rename(`Station Description` = StationDes,
                Organization = OrgID,
                Latitude = Lat_DD,
                Longitude = Long_DD) %>% 
  dplyr::select(`Station Description`,`Station ID`, Organization, Latitude, Longitude)

temp.model <- cal.input %>%
  dplyr::left_join(station_awqms[,c("Station ID", "StationDes", "OrgID")], by="Station ID") %>% 
  dplyr::filter(`Parameter` %in%  c("Water Temperature")) %>% 
  dplyr::filter(!is.na(`Data Source`) & is.na(`Interpolated Data`)) %>% 
  dplyr::mutate(`Station Description` = ifelse(is.na(StationDes),`Model Location Name`,StationDes),
                Organization = ifelse(is.na(OrgID),`Data Source`,OrgID)) %>% 
  dplyr::select(`Station Description`,`Station ID`, Organization, Latitude, Longitude)

temp.awqms.model <- rbind(temp.awqms,temp.model) %>% 
  dplyr::filter(!`Station ID` == "TIR") %>% 
  dplyr::filter(!is.na(Latitude)) %>%
  dplyr::mutate(`Station Name and ID` = ifelse(`Station ID` == "No Station ID or unknown" | is.na(`Station ID`),
                                               `Station Description`,
                                               paste0(`Station Description`, " (", `Station ID`, ")"))) %>% 
  dplyr::mutate(`Station Name and ID` = stringr::str_to_title(`Station Name and ID`)) %>% 
  dplyr::mutate_at("Station Name and ID", str_replace_all, "Ordeq", "ORDEQ") %>%
  dplyr::mutate_at("Station Name and ID", str_replace_all, " Rm", " RM") %>%
  dplyr::mutate_at("Station Name and ID", str_replace_all, "Lb", "LB") %>%
  dplyr::mutate_at("Station Name and ID", str_replace_all, "Nf", "NF") %>%
  dplyr::mutate_at("Station Name and ID", str_replace_all, "Sf", "SF") %>%
  dplyr::distinct(`Station Name and ID`, .keep_all=TRUE) %>%
  dplyr::mutate(Organization = ifelse(Organization == "OregonDEQ", "ODEQ", Organization)) %>% 
  dplyr::mutate(Organization = ifelse(Organization == "11NPSWRD_WQX", "EPA WQX", Organization)) %>% 
  dplyr::select(`Station Name and ID`, Latitude, Longitude, Organization) %>%
  sf::st_as_sf(coords = c("Longitude","Latitude"), crs = sf::st_crs("+init=EPSG:4326"))

map.temp <- sf::st_filter(temp.awqms.model, pro.areas, join = st_within)

map.temp.pro <-  sf::st_join(x = map.temp,
                         y = pro.areas,
                         join = st_within,
                         left = TRUE)

# Flow ----
flow.usgs <- usgs.stations.or %>%
  dplyr::mutate_at("station_nm", str_replace_all, " R ", " RIVER ") %>% 
  dplyr::mutate_at("station_nm", str_replace_all, " @ ", " AT ") %>% 
  dplyr::mutate_at("station_nm", str_replace_all, " & ", " AND ") %>% 
  dplyr::mutate_at("station_nm", str_replace_all, " CK ", " CREEK ") %>% 
  dplyr::mutate_at("station_nm", str_replace_all, " CR ", " CREEK ") %>% 
  dplyr::mutate_at("station_nm", str_replace_all, " NR ", " NEAR ") %>% 
  dplyr::mutate_at("station_nm", str_replace_all, "N\\.", "NORTH ") %>% 
  dplyr::mutate_at("station_nm", str_replace_all, "N ", "NORTH ") %>% 
  dplyr::mutate_at("station_nm", str_replace_all, " ABV ", " ABOVE ") %>% 
  dplyr::mutate_at("station_nm", str_replace_all, " BLW ", " BELOW ") %>% 
  dplyr::mutate_at("station_nm", str_replace_all, " P ", " POWER ") %>% 
  dplyr::mutate_at("station_nm", str_replace_all, " CA ", " CANAL ") %>% 
  dplyr::mutate_at("station_nm", str_replace_all, "OREG", "OR") %>% 
  dplyr::mutate_at("station_nm", str_replace_all, "\\.", "") %>% 
  dplyr::mutate_at("station_nm", str_replace_all, "T FLS", "TOKETEE FALLS") %>% 
  dplyr::mutate_at("station_nm", str_replace_all, ",OR", "") %>% 
  dplyr::mutate_at("station_nm", str_replace_all, ", OR", "") %>% 
  dplyr::mutate_at("station_nm", str_replace_all, " OR", "") %>% 
  dplyr::mutate(`station_nm` = stringr::str_to_title(`station_nm`)) %>% 
  dplyr::mutate_at("station_nm", str_replace_all, "So", "S") %>% 
  dplyr::mutate_at("station_nm", str_replace_all, "No", "N") %>% 
  dplyr::mutate_at("station_nm", str_replace_all, "Nrth", "North") %>% 
  dplyr::mutate_at("station_nm", str_replace_all, "Suth", "South") %>% 
  dplyr::mutate(Station = station_nm,
                `Station ID`= site_no,
                Station_Des = paste0("USGS: ",Station," (",`Station ID`,")")) %>% 
  dplyr::rename(Latitude = dec_lat_va,
                Longitude = dec_long_va) %>% 
  dplyr::select(Station, `Station ID`, Latitude, Longitude,Station_Des)

flow.hydromet <- hydromet %>% 
  dplyr::filter(Parameter %in% c("Discharge", "Spillway Discharge", "Flow")) %>% 
  dplyr::distinct(Lat, Long, .keep_all = TRUE) %>% 
  dplyr::mutate(Station = Station.Name,
                `Station ID` = Station.ID,
                Station_Des = paste0("Hydromet: ",Station," (",`Station ID`,")")) %>% 
  dplyr::rename(Latitude = Lat,
                Longitude = Long) %>% 
  dplyr::select(Station, `Station ID`, Latitude, Longitude, Station_Des)

flow.sp <- cal.input %>%
  dplyr::filter(`Parameter` %in% c("Flow")) %>%
  dplyr::filter(!`Data Source` == "USGS" & is.na(`Interpolated Data`)) %>% 
  dplyr::distinct(Latitude, Longitude, .keep_all = TRUE) %>% 
  dplyr::mutate(Station = `Model Location Name`,
                Station_Des = paste0(`Data Source`,": ",Station," (",`Station ID`,")")) %>%  
  dplyr::select(Station, `Station ID`, Latitude, Longitude, Station_Des)

flow.pro <- rbind(flow.usgs,flow.hydromet,flow.sp) %>% 
  dplyr::filter(!is.na(Latitude)) %>% 
  sf::st_as_sf(coords = c("Longitude","Latitude"), crs = sf::st_crs("+init=EPSG:4326"))

map.flow <- sf::st_filter(flow.pro, pro.areas, join = st_within)

map.flow.pro <-  sf::st_join(x = map.flow,
                             y = pro.areas,
                             join = st_within,
                             left = TRUE)

# Met ----
met.sp <- cal.input %>% 
  dplyr::filter(`Model Location Type` %in% c("Meteorological")) %>%
  dplyr::filter(!is.na(`Data Source`) & is.na(`Interpolated Data`)) %>%
  dplyr::mutate(Station = ifelse(`Station ID` == "No Station ID or unknown" | is.na(`Station ID`),
                                 `Data Source`,
                                 paste0(`Station ID`, ", ", `Data Source`))) %>% 
  dplyr::select(Station, Latitude, Longitude)

met.ncdc <- ncdc.stations %>% 
  dplyr::filter(!str_detect(id,"COOP")) %>% 
  dplyr::rename(Latitude = lat,
                Longitude = long,
                `Station ID` = id) %>%
  dplyr::mutate_at("name", str_replace_all, ", OR US", "") %>% 
  dplyr::mutate(name = stringr::str_to_title(name)) %>% 
  dplyr::mutate_at("name", str_replace_all, "Nw", "NW") %>% 
  dplyr::mutate_at("name", str_replace_all, "Ne", "NE") %>% 
  dplyr::mutate_at("name", str_replace_all, "Nnw", "NNW") %>% 
  dplyr::mutate_at("name", str_replace_all, "Se", "SE") %>% 
  dplyr::mutate_at("name", str_replace_all, "Ese", "ESE") %>% 
  dplyr::mutate_at("name", str_replace_all, "Wnw", "WNW") %>% 
  dplyr::mutate_at("name", str_replace_all, "Ssw", "SSW") %>% 
  dplyr::mutate_at("name", str_replace_all, "Ene", "ENE") %>% 
  dplyr::mutate_at("name", str_replace_all, "Sse", "SSE") %>% 
  dplyr::mutate_at("name", str_replace_all, "Sw", "SW") %>% 
  dplyr::mutate(`Station` = paste0(`Station ID`," at ",name," (NCDC)")) %>% 
  dplyr::select(Station, Latitude, Longitude)

sf::st_geometry(met.ncdc) <- NULL

met.raws <- raws.stations %>% 
  dplyr::rename(Latitude = lat,
                Longitude = long) %>% 
  dplyr::mutate(`Station` = paste0(agency," station ", wrccID, " at ", siteName, " (RAWS)")) %>% 
  dplyr::select(Station, Latitude, Longitude)

sf::st_geometry(met.raws) <- NULL

met.agrimet <- agrimet.stations.or %>%
  dplyr::rename(Latitude = lat,
                Longitude = long) %>%
  dplyr::mutate(`Station` = paste0(siteid, " - ", description, " (AgriMet)")) %>% 
  dplyr::select(Station, Latitude, Longitude)

sf::st_geometry(met.agrimet) <- NULL

met.hydromet <- hydromet %>% 
  dplyr::filter(Parameter %in% c("Air Temperature","Precipitation")) %>% 
  dplyr::rename(Latitude = Lat,
                Longitude = Long) %>%
  dplyr::mutate(`Station` = paste0(Station.ID, " - ", Station.Name, " (Hydromet)")) %>% 
  dplyr::select(Station, Latitude, Longitude)

met.mw <- mw.stations %>% 
  dplyr::rename(Latitude = lat,
                Longitude = long) %>%
  dplyr::mutate(`Station` = paste0(STID, " - ", NAME, " (MesoWest)")) %>% 
  dplyr::select(Station, Latitude, Longitude)

sf::st_geometry(met.mw) <- NULL

met.pro <- rbind(met.sp, met.ncdc, met.raws, met.agrimet, met.hydromet, met.mw) %>% 
  dplyr::filter(!is.na(Latitude)) %>% 
  sf::st_as_sf(coords = c("Longitude","Latitude"), crs = sf::st_crs("+init=EPSG:4326"))

map.met <- sf::st_filter(met.pro, pro.areas, join = st_within)

map.met.pro <-  sf::st_join(x = map.met,
                             y = pro.areas,
                             join = st_within,
                             left = TRUE)

# Ind NPDES ----
ind.npdes.pro <- npdes.ind %>% 
  dplyr::filter(!is.na(Latitude)) %>% 
  dplyr::mutate(`Common Name` = stringr::str_to_title(`Common Name`)) %>%
  dplyr::mutate_at("Common Name", str_replace_all, "Stp", "STP") %>%
  dplyr::mutate(`Facility Name and Number` = paste0(`Common Name`," (DEQ File #", `WQ File Nbr`,")"),
                `Permit Type and Description` = paste0(`Permit Type`, ": ", `Permit Description`),
                `Stream/River Mile` = ifelse(is.na(`Stream Name`), NA, paste0(`Stream Name`, " ", " RM ",`River Mile`))) %>% 
  dplyr::select(`Facility Name and Number`, Latitude, Longitude) %>% 
  sf::st_as_sf(coords = c("Longitude","Latitude"), crs = sf::st_crs("+init=EPSG:4326"))


map.ind.npdes <- sf::st_filter(ind.npdes.pro, pro.areas, join = st_within)

map.ind.npdes.pro <- sf::st_join(x = map.ind.npdes,
                                 y = pro.areas,
                                 join = st_within,
                                 left = TRUE)

# Leaflet Map ----
map <- leaflet::leaflet() %>% leaflet::addTiles() %>% 
  leaflet::setView(lng = -121, lat = 44, zoom=7.8) %>%
  leaflet::addPolygons(data = pro.areas,
                       group = "Project Areas",
                       label = ~Project_Na,
                       fillColor = "transparent",
                       fillOpacity = 0,
                       weight = 2,
                       color = "black",
                       opacity = 1,
                       highlightOptions = highlightOptions(fillColor = "blue",
                                                           fillOpacity = 0.2,
                                                           weight = 3,
                                                           bringToFront = TRUE)) %>%
  leaflet::addPolygons(data = pro.areas,
                       group = "Completion Schedule",
                       label = ~Project_Na,
                       fillColor = ~color,
                       weight = 1,
                       color = "black",
                       opacity = 1,
                       fillOpacity = 1) %>% 
  leaflet::addPolylines(data = pro.reaches,
                        group = "Project Areas",
                        label = ~Project_Na,
                        color = ~color,
                        opacity = 0.7,
                        weight = 2) %>% 
  leaflet::addPolylines(data = model.streams,
                        group = "Model Streams",
                        label = ~Stream,
                        color = "#6baed6",
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
  leaflet::addMarkers(data = map.temp.pro,
                      group = "Stream Temperature Stations",
                      clusterOptions = markerClusterOptions(),
                      popup = paste0(map.temp.pro$Organization, ": ", map.temp.pro$`Station Name and ID`)) %>% 
  leaflet::addMarkers(data = map.flow.pro,
                      group = "Flow Stations",
                      clusterOptions = markerClusterOptions(),
                      popup = ~Station_Des) %>% 
  leaflet::addMarkers(data = map.met.pro,
                      group = "Meteorological Stations",
                      clusterOptions = markerClusterOptions(),
                      popup = ~Station) %>%
  leaflet::addMarkers(data = map.ind.npdes.pro,
                      group = "Individual NPDES Point Sources",
                      clusterOptions = markerClusterOptions(),
                      popup = ~`Facility Name and Number`) %>%
  leaflet::addLayersControl(overlayGroups = c("Project Areas","Model Streams","HUC 8", "HUC 10", "HUC 12", "Completion Schedule"),
                            baseGroups = c("Stream Temperature Stations","Flow Stations","Meteorological Stations","Individual NPDES Point Sources"),
                            options = leaflet::layersControlOptions(collapsed = FALSE, autoZIndex = TRUE)) %>% 
  leaflet::hideGroup(c("HUC 8","HUC 10", "HUC 12", "Completion Schedule")) %>% 
  leaflet::addLegend(data = pro.areas,
                     position = "topright",
                     colors = ~unique(pro.areas$color),
                     labels = ~unique(pro.areas$Complete_D),
                     title = "Completion Date") %>% 
  leaflet::addMiniMap(position = "bottomright",
                      width = 200,
                      height = 200,
                      zoomLevelFixed = 5) %>% 
  leaflet.extras::addResetMapButton()


map


# Save Map ----
dir <-  "//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/model_QAPPs/R/test_doc/20201123/"
htmlwidgets::saveWidget(map, paste0(dir,"map.html"), 
                        title = paste("Temperature TMDL Replacement Project"), 
                        background = "grey", selfcontained = FALSE)
