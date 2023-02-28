library(dplyr)
library(leaflet)
map.dir <-  "//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/model_QAPPs/R/map/area_maps/"
source("map_functions.R")
logo <- base64enc::base64encode("//deqhq1/WQNPS/Status_and_Trend_Reports/Figures/DEQ-logo-color-non-transp71x107.png")
tag.map.title <- htmltools::tags$style(htmltools::HTML("
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

# text-align: center;
# line-height: 1;

qapp_project_area <- "Willamette Subbasins"
file.name <- "Willamette Subbasins"
map.file.name <-  "map_Willamette_Subbasins"
Willamette_Subbasins <- c("Lower Willamette and Clackamas Subbasins","Middle Willamette Subbasins","Southern Willamette Subbasins")
# Map data ----
load("E:/PROJECTS/20200810_RyanMichie_TempTMDLReplacement/R/branches/Lower_Willamette_and_Clackamas_Subbasins/mQAPPrmd/data/lookup.RData")
#cgwtools::lsdata("E:/PROJECTS/20200810_RyanMichie_TempTMDLReplacement/R/branches/Lower_Willamette_and_Clackamas_Subbasins/mQAPPrmd/data/lookup.RData")
# Temporary correction of lookup table, remove after updating it in data.R and data_will.R
lookup.huc <- readxl::read_xlsx("//deqhq1/tmdl/Planning statewide/Temperature_TMDL_Revisions/model_QAPPs/R/data/Lookup_QAPPProjectArea.xlsx", 
                                sheet = "Lookup_QAPPProjectArea") %>% 
  dplyr::mutate(HUC_6 = as.character(HUC_6),
                HUC_8 = as.character(HUC_8),
                HUC10 = as.character(HUC10),
                HUC12 = as.character(HUC12))
load("E:/PROJECTS/20200810_RyanMichie_TempTMDLReplacement/R/branches/Lower_Willamette_and_Clackamas_Subbasins/mQAPPrmd/data/map_Lower_Willamette_and_Clackamas_Subbasins.RData")
# cgwtools::lsdata("E:/PROJECTS/20200810_RyanMichie_TempTMDLReplacement/R/branches/Lower_Willamette_and_Clackamas_Subbasins/mQAPPrmd/data/map_Lower_Willamette_and_Clackamas_Subbasins.RData")
# pro_area.lw <- pro_area
hs_temp_model_extent.lw <- hs_temp_model_extent
hs_solar_model_extent.lw <- hs_solar_model_extent
hs_solar_model_area.lw <- hs_solar_model_area
ce_model_extent.lw <- ce_model_extent
sh_model_extent.lw <- sh_model_extent
pro.cat.45.tbl.lw <- pro.cat.45.tbl
load("E:/PROJECTS/20200810_RyanMichie_TempTMDLReplacement/R/branches/Lower_Willamette_and_Clackamas_Subbasins/mQAPPrmd/data/map_Lower_Willamette_and_Clackamas_Subbasins_qapp.RData")
# cgwtools::lsdata("E:/PROJECTS/20200810_RyanMichie_TempTMDLReplacement/R/branches/Lower_Willamette_and_Clackamas_Subbasins/mQAPPrmd/data/map_Lower_Willamette_and_Clackamas_Subbasins_qapp.RData")
temp.data.sample.count.lw <- temp.data.sample.count
flow.data.sample.count.lw <- flow.data.sample.count
temp_stations.lw <- temp_stations
temp_cal_sites.lw <- temp_cal_sites
temp_model_bc_tri.lw <- temp_model_bc_tri
flow_model_bc_tri.lw <- flow_model_bc_tri
flow_stations.lw <- flow_stations
gage_height_stations_map.lw <- gage_height_stations_map
met_stations.lw <- met_stations
ind_ps.lw <- ind_ps
gen_ps.lw <- gen_ps
tcat45.lw <- tcat45
load("E:/PROJECTS/20200810_RyanMichie_TempTMDLReplacement/R/branches/Middle_Willamette_Subbasins/mQAPPrmd/data/map_Middle_Willamette_Subbasins.RData")
# cgwtools::lsdata("E:/PROJECTS/20200810_RyanMichie_TempTMDLReplacement/R/branches/Middle_Willamette_Subbasins/mQAPPrmd/data/map_Middle_Willamette_Subbasins.RData")
# pro_area.mw <- pro_area
hs_temp_model_extent.mw <- hs_temp_model_extent
hs_solar_model_extent.mw <- hs_solar_model_extent
hs_solar_model_area.mw <- hs_solar_model_area
ce_model_extent.mw <- ce_model_extent
sh_model_extent.mw <- sh_model_extent
pro.cat.45.tbl.mw <- pro.cat.45.tbl
load("E:/PROJECTS/20200810_RyanMichie_TempTMDLReplacement/R/branches/Middle_Willamette_Subbasins/mQAPPrmd/data/map_Middle_Willamette_Subbasins_qapp.RData")
# cgwtools::lsdata("E:/PROJECTS/20200810_RyanMichie_TempTMDLReplacement/R/branches/Middle_Willamette_Subbasins/mQAPPrmd/data/map_Middle_Willamette_Subbasins_qapp.RData")
temp.data.sample.count.mw <- temp.data.sample.count
flow.data.sample.count.mw <- flow.data.sample.count
temp_stations.mw <- temp_stations
temp_cal_sites.mw <- temp_cal_sites
temp_model_bc_tri.mw <- temp_model_bc_tri
flow_model_bc_tri.mw <- flow_model_bc_tri
flow_stations.mw <- flow_stations
gage_height_stations_map.mw <- gage_height_stations_map
met_stations.mw <- met_stations
ind_ps.mw <- ind_ps
gen_ps.mw <- gen_ps
tcat45.mw <- tcat45
load("E:/PROJECTS/20200810_RyanMichie_TempTMDLReplacement/R/branches/Southern_Willamette_Subbasins/mQAPPrmd/data/map_Southern_Willamette_Subbasins.RData")
# cgwtools::lsdata("E:/PROJECTS/20200810_RyanMichie_TempTMDLReplacement/R/branches/Southern_Willamette_Subbasins/mQAPPrmd/data/map_Southern_Willamette_Subbasins.RData")
# pro_area.sw <- pro_area
hs_temp_model_extent.sw <- hs_temp_model_extent
hs_solar_model_extent.sw <- hs_solar_model_extent
hs_solar_model_area.sw <- hs_solar_model_area
ce_model_extent.sw <- ce_model_extent
sh_model_extent.sw <- sh_model_extent
pro.cat.45.tbl.sw <- pro.cat.45.tbl
load("E:/PROJECTS/20200810_RyanMichie_TempTMDLReplacement/R/branches/Southern_Willamette_Subbasins/mQAPPrmd/data/map_Southern_Willamette_Subbasins_qapp.RData")
# cgwtools::lsdata("E:/PROJECTS/20200810_RyanMichie_TempTMDLReplacement/R/branches/Southern_Willamette_Subbasins/mQAPPrmd/data/map_Southern_Willamette_Subbasins_qapp.RData")
temp.data.sample.count.sw <- temp.data.sample.count
flow.data.sample.count.sw <- flow.data.sample.count
temp_stations.sw <- temp_stations
temp_cal_sites.sw <- temp_cal_sites
temp_model_bc_tri.sw <- temp_model_bc_tri
flow_model_bc_tri.sw <- flow_model_bc_tri
flow_stations.sw <- flow_stations
gage_height_stations_map.sw <- gage_height_stations_map
met_stations.sw <- met_stations
ind_ps.sw <- ind_ps
gen_ps.sw <- gen_ps
tcat45.sw <- tcat45
# Combination ----
hs_temp_model_extent <- rbind(hs_temp_model_extent.lw,hs_temp_model_extent.mw,hs_temp_model_extent.sw)
hs_solar_model_extent <- rbind(hs_solar_model_extent.lw,hs_solar_model_extent.mw,hs_solar_model_extent.sw)
hs_solar_model_area <- rbind(hs_solar_model_area.lw,hs_solar_model_area.mw,hs_solar_model_area.sw)
ce_model_extent <- rbind(ce_model_extent.lw,ce_model_extent.mw,ce_model_extent.sw)
sh_model_extent <- rbind(sh_model_extent.lw,sh_model_extent.mw,sh_model_extent.sw)
pro.cat.45.tbl <- rbind(pro.cat.45.tbl.lw,pro.cat.45.tbl.mw,pro.cat.45.tbl.sw)
temp.data.sample.count <- rbind(temp.data.sample.count.lw,temp.data.sample.count.mw,temp.data.sample.count.sw)
flow.data.sample.count <- rbind(flow.data.sample.count.lw,flow.data.sample.count.mw,flow.data.sample.count.sw)
temp_stations <- rbind(temp_stations.lw,temp_stations.mw,temp_stations.sw)
temp_cal_sites <- rbind(temp_cal_sites.lw,temp_cal_sites.mw,temp_cal_sites.sw)
temp_model_bc_tri <- rbind(temp_model_bc_tri.lw,temp_model_bc_tri.mw,temp_model_bc_tri.sw)
flow_model_bc_tri <- rbind(flow_model_bc_tri.lw,flow_model_bc_tri.mw,flow_model_bc_tri.sw)
flow_stations <- rbind(flow_stations.lw,flow_stations.mw,flow_stations.sw)
gage_height_stations_map <- rbind(gage_height_stations_map.lw,gage_height_stations_map.mw,gage_height_stations_map.sw)
met_stations <- rbind(met_stations.lw,met_stations.mw,met_stations.sw)
ind_ps <- rbind(ind_ps.lw,ind_ps.mw,ind_ps.sw)
gen_ps <- rbind(gen_ps.lw,gen_ps.mw,gen_ps.sw)
tcat45 <- rbind(tcat45.lw,tcat45.mw,tcat45.sw)

# Project and AU layers ----
pro_area <- sf::st_read(dsn = "//deqhq1/tmdl/TMDL_Willamette/Willamette_Subbasins_Temperature_2024/GIS/WillametteSubbasins/data/Willamette Subbasins Boundary_modified.shp",
                        layer = "Willamette Subbasins Boundary_modified") %>% sf::st_transform(4326) %>% sf::st_zm()
au_rivers <- sf::st_read(dsn = "//deqhq1/GISLIBRARY/Base_Data/DEQ_Data/Water_Quality/WQ_Assessment/WQ_2022_IntegratedReport_FINAL/IR_2022_Final.gdb",
                         layer = "AU_OR_Rivers_CoastLine") #%>% sf::st_transform(4326) %>% sf::st_zm()
au_waterbodies <- sf::st_read(dsn = "//deqhq1/GISLIBRARY/Base_Data/DEQ_Data/Water_Quality/WQ_Assessment/WQ_2022_IntegratedReport_FINAL/IR_2022_Final.gdb",
                              layer = "AU_OR_Waterbodies") #%>% sf::st_transform(4326) %>% sf::st_zm()
au_watershed <- sf::st_read(dsn = "//deqhq1/GISLIBRARY/Base_Data/DEQ_Data/Water_Quality/WQ_Assessment/WQ_2022_IntegratedReport_FINAL/IR_2022_Final.gdb",
                            layer = "AU_OR_Watershed_Area") #%>% sf::st_transform(4326) %>% sf::st_zm()
# WMS layer ----
wms.aus <- readxl::read_xlsx("//deqhq1/tmdl/TMDL_Willamette/Willamette_Mainstem_Temperature_2025/Project_Plans/Willamette_Mainstem_AUs_2022.04.15.xlsx",sheet = "Final_AUs")
wms.au.id <- wms.aus %>% dplyr::pull(AU_ID)
sf::sf_use_s2(FALSE)

#
pro.area.extent <- "43.356,-123.6642,45.94129,-121.6514"
subbasin_huc8 <- sort(unique(lookup.huc[which(lookup.huc$QAPP_Project_Area %in% Willamette_Subbasins),]$HUC_8))
subbasin_huc10 <- sort(unique(lookup.huc[which(lookup.huc$QAPP_Project_Area %in% Willamette_Subbasins),]$HUC10))
subbasin_huc12 <- sort(unique(lookup.huc[which(lookup.huc$QAPP_Project_Area %in% Willamette_Subbasins),]$HUC12))
subbasin_huc8_lower <- sort(unique(lookup.huc[which(lookup.huc$QAPP_Project_Area %in% "Lower Willamette and Clackamas Subbasins"),]$HUC_8))
subbasin_huc8_middle <- sort(unique(lookup.huc[which(lookup.huc$QAPP_Project_Area %in% "Middle Willamette Subbasins"),]$HUC_8))
subbasin_huc8_southern <- sort(unique(lookup.huc[which(lookup.huc$QAPP_Project_Area %in% "Southern Willamette Subbasins"),]$HUC_8))

pro_scope_rivers <- au_rivers %>% sf::st_drop_geometry() %>% 
  dplyr::left_join(lookup.huc,by="HUC12") %>% 
  dplyr::filter(HUC_8 %in% subbasin_huc8) %>% 
  dplyr::filter(!AU_ID %in% wms.au.id) %>% 
  dplyr::pull(AU_ID)
pro_scope_waterbodies <- au_waterbodies %>% sf::st_drop_geometry() %>% 
  dplyr::left_join(lookup.huc,by="HUC12") %>% 
  dplyr::filter(HUC_8 %in% subbasin_huc8) %>% 
  dplyr::filter(!AU_ID %in% wms.au.id)%>% 
  dplyr::pull(AU_ID)
pro_scope_watershed <- au_watershed %>% sf::st_drop_geometry() %>% 
  dplyr::left_join(lookup.huc,by="HUC12") %>% 
  dplyr::filter(HUC_8 %in% subbasin_huc8) %>% 
  dplyr::filter(!AU_ID %in% wms.au.id)%>% 
  dplyr::pull(AU_ID)

# where clause ----
## used in querying the feature layers from the REST Server
where_huc8 <- paste0("HUC8 IN ('", paste(subbasin_huc8, collapse = "','"),"')")
where_huc10 <- paste0("HUC10 IN ('", paste(subbasin_huc10, collapse = "','"),"')")
where_huc12 <- paste0("HUC12 IN ('", paste(subbasin_huc12, collapse = "','"),"')")
where_huc8_lower <- paste0("HUC8 IN ('", paste(subbasin_huc8_lower, collapse = "','"),"')")
where_huc8_middle <- paste0("HUC8 IN ('", paste(subbasin_huc8_middle, collapse = "','"),"')")
where_huc8_southern <- paste0("HUC8 IN ('", paste(subbasin_huc8_southern, collapse = "','"),"')")
where_au_rivers <- paste0("(AU_ID IN ('", paste(pro_scope_rivers, collapse = "','"),"'))")
where_au_waterbodies <- paste0("(AU_ID IN ('", paste(pro_scope_waterbodies, collapse = "','"),"'))")
where_au_watershed <- paste0("(AU_ID IN ('", paste(pro_scope_watershed, collapse = "','"),"'))")
where_au_wms <- paste0("(AU_ID IN ('", paste(wms.au.id, collapse = "','"),"'))")
where_au_yearRound <- paste0("(Pollutant = 'Temperature' AND AU_parameter_category IN ('4A','5') AND period = 'year_round') AND ",
                             "(AU_ID IN ('", paste(tcat45$`Assessment Unit ID`, collapse = "','"),"'))")
where_au_spawning <- paste0("(Pollutant = 'Temperature' AND AU_parameter_category IN ('4A','5') AND period = 'spawn') AND ",
                            "(AU_ID IN ('", paste(tcat45$`Assessment Unit ID`, collapse = "','"),"'))")
reachcode <- paste(paste0("(ReachCode >= ", subbasin_huc8, "000000", " AND ReachCode <= ", subbasin_huc8,"999999)"),collapse =  " OR ")

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
                                    #"shade",
                                    "met_stations",
                                    "ind_ps",
                                    "gen_ps"),
                           NROW = c(nrow(temp_cal_sites),
                                    nrow(temp_model_bc_tri),
                                    nrow(flow_model_bc_tri),
                                    nrow(temp_stations),
                                    nrow(flow_stations),
                                    nrow(gage_height_stations_map),
                                    #nrow(shade),
                                    nrow(met_stations),
                                    nrow(ind_ps),
                                    nrow(gen_ps)),
                           group_name = c("Model Calibration Sites",
                                          "Model Temperature Boundary Conditions and Tributary Inputs",
                                          "Model Flow Boundary Conditions and Tributary Inputs",
                                          "Stream Temperature Stations",
                                          "Stream Flow Stations",
                                          "Gage Height Stations",
                                          #"Effective Shade Measurement Sites",
                                          "Meteorological Stations",
                                          "Individual NPDES Point Sources",
                                          "General NPDES Point Sources (GEN01, GEN03, GEN04, GEN05, GEN19, or GEN40)")) %>% 
  dplyr::filter(!NROW == 0)

dta.stations.mod <- rbind(dta.mod,dta.stations)

# IR group names ----
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
                 "Land Ownership or Jurisdiction (Lower Willamette and Clackamas Subbasins)",
                 "Land Ownership or Jurisdiction (Middle Willamette Subbasins)",
                 "Land Ownership or Jurisdiction (Southern Willamette Subbasins)",
                 "Stream Names (USGS NHD)",
                 "Oregon Imagery")

#group.names.hide <- c(dta.stations.mod[-1,] %>% dplyr::pull(group_name),
group.names.hide <- c(dta.stations.mod %>% dplyr::pull(group_name),
                      "HUC8","HUC10","HUC12",
                      ir.grps,
                      "Fish Use Designations",
                      "Salmon and Steelhead Spawning Use Designations",
                      "Land Ownership or Jurisdiction (Lower Willamette and Clackamas Subbasins)",
                      "Land Ownership or Jurisdiction (Middle Willamette Subbasins)",
                      "Land Ownership or Jurisdiction (Southern Willamette Subbasins)",
                      "Stream Names (USGS NHD)",
                      "Oregon Imagery")

group.names.search <- dta.stations %>% dplyr::pull(group_name)

# Basic layers ----
map.title <- htmltools::tags$div(tag.map.title, htmltools::HTML(paste0(qapp_project_area)))
#map.title <- htmltools::tags$div(tag.map.title, htmltools::HTML(paste0(htmltools::tags$h1("Willamette Subbasins"),htmltools::tags$h3("(Lower Willamette, Middle Willamette and Southern Willamette)"))))
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
  leaflet::fitBounds(lng1 = -123.6642, lat1 = 43.356,
                     lng2 = -121.6514, lat2 = 45.94129) %>%
  leaflet::addMapPane("OpenStreetMap", zIndex = -20) %>%
  leaflet::addMapPane("aerial", zIndex = -10) %>%
  leaflet::addMapPane("areaOutline", zIndex = 10) %>%
  leaflet::addMapPane("dma", zIndex = 20) %>%
  leaflet::addMapPane("area", zIndex = 30) %>%
  leaflet::addMapPane("area_wms", zIndex = 40) %>%
  leaflet::addMapPane("hydrotiles", zIndex = 50) %>%
  leaflet::addMapPane("huc8", zIndex = 60) %>%
  leaflet::addMapPane("huc10", zIndex = 70) %>%
  leaflet::addMapPane("huc12", zIndex = 80) %>%
  leaflet::addMapPane("wqs1", zIndex = 90) %>%
  leaflet::addMapPane("wqs2", zIndex = 100) %>%
  leaflet::addMapPane("ir", zIndex = 110) %>%
  leaflet::addMapPane("ir_gnis", zIndex = 120) %>%
  leaflet::addMapPane("mod", zIndex = 130) %>%
  leaflet::addMapPane("modbes", zIndex = 140) %>%
  # leaflet::addMapPane("mod2016", zIndex = -200) %>%
  # leaflet::addMapPane("mod2009", zIndex = -200) %>%
  leaflet::addMapPane("marker", zIndex = 110) %>%
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
  # __ Project scope: WS ----
leaflet.esri::addEsriFeatureLayer(url="https://services.arcgis.com/uUvqNMGPm7axC2dD/ArcGIS/rest/services/IR_2022_Final/FeatureServer/34",
                                  options = leaflet.esri::featureLayerOptions(where = where_au_rivers,
                                                                              simplifyFactor = 0.35),
                                  useServiceSymbology = TRUE,
                                  group = "Willamette Subbasins TMDL Project Scope",
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
                                    group = "Willamette Subbasins TMDL Project Scope",
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
                                    group = "Willamette Subbasins TMDL Project Scope",
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
# __ Project scope: WMS ----
leaflet.esri::addEsriFeatureLayer(url="https://services.arcgis.com/uUvqNMGPm7axC2dD/ArcGIS/rest/services/IR_2022_Final/FeatureServer/34",
                                  options = leaflet.esri::featureLayerOptions(where = where_au_wms),
                                  useServiceSymbology = TRUE,
                                  group = "Willamette River Mainstem and Major Tributaries TMDL Project Scope",
                                  pathOptions = leaflet::pathOptions(pane="area_wms"),
                                  color = "#810f7c",
                                  weight = 3,
                                  opacity = 1,
                                  fill = FALSE,
                                  highlightOptions = leaflet::highlightOptions(color="#8c6bb1",
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
                                    options = leaflet.esri::featureLayerOptions(where = where_au_wms),
                                    useServiceSymbology = TRUE,
                                    group = "Willamette River Mainstem and Major Tributaries TMDL Project Scope",
                                    pathOptions = leaflet::pathOptions(pane="area_wms"),
                                    color = "#810f7c",
                                    weight = 0.8,
                                    opacity = 1,
                                    fillColor = "#810f7c",
                                    fillOpacity = 0.8,
                                    highlightOptions = leaflet::highlightOptions(color="#8c6bb1",
                                                                                 weight = 4,
                                                                                 fill=TRUE,
                                                                                 fillColor = "#8c6bb1",
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
  # __ HUCs ----
leaflet.esri::addEsriFeatureLayer(url="https://arcgis.deq.state.or.us/arcgis/rest/services/WQ/WBD/MapServer/1",
                                  options = leaflet.esri::featureLayerOptions(where = where_huc8),
                                  group = "HUC8",
                                  pathOptions = leaflet::pathOptions(pane="huc8"),
                                  weight = 4,
                                  color = "black",
                                  opacity = 3,
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
leaflet.esri::addEsriFeatureLayer(url="https://services.arcgis.com/uUvqNMGPm7axC2dD/ArcGIS/rest/services/IR_2022_Final/FeatureServer/50",
                                  options = leaflet.esri::featureLayerOptions(where = where_au_yearRound),
                                  useServiceSymbology = TRUE,
                                  group = "2022 303(d) Temperature Listed - Streams (Year Round Criteria)",
                                  pathOptions = leaflet::pathOptions(pane="ir"),
                                  color = "#9A00C4",
                                  weight = 3,
                                  opacity = 0.8,
                                  fill = FALSE,
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
                                    fill = FALSE,
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
                                    fill = TRUE,
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
                                    #9A00C4                                    fillOpacity = 0.25,
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
leaflet.esri::addEsriFeatureLayer(url = "https://services.arcgis.com/uUvqNMGPm7axC2dD/ArcGIS/rest/services/OR_DMAs/FeatureServer/0",
                                  options = leaflet.esri::featureLayerOptions(where = where_huc8_lower,
                                                                              style = DMAcolor,
                                                                              simplifyFactor = 0.35),
                                  group = "Land Ownership or Jurisdiction (Lower Willamette and Clackamas Subbasins)",
                                  pathOptions = leaflet::pathOptions(pane = "dma"),
                                  fill = TRUE,
                                  fillOpacity = 0.75,
                                  weight = 0.1,
                                  opacity = 0,
                                  color = "white",
                                  labelProperty = htmlwidgets::JS("function(feature){var props = feature.properties; return props.DMA_RP+\" \"}"),
                                  labelOptions = leaflet::labelOptions(offset = c(0,0),
                                                                       opacity = 1,
                                                                       textsize = "12px",
                                                                       sticky = TRUE),
                                  highlightOptions = leaflet::highlightOptions(weight = 3,
                                                                               color = "white",
                                                                               opacity = 1,
                                                                               bringToFront = TRUE),
                                  popupProperty = htmlwidgets::JS(paste0('function(feature){var props = feature.properties; return \"',
                                                                         '<b>\"+props.DMA_RP+\"</b>',
                                                                         '<br>\"+props.DMA_RP_Cl+\"',
                                                                         '<br><b>HUC12:</b> \"+props.HUC12+\"',
                                                                         '<br><b>Version:</b> \"+props.Version+\"',
                                                                         ' \"}')),
                                  popupOptions = leaflet::popupOptions(maxWidth = 800, maxHeight = 500)) %>%
  leaflet.esri::addEsriFeatureLayer(url="https://services.arcgis.com/uUvqNMGPm7axC2dD/ArcGIS/rest/services/OR_DMAs/FeatureServer/0",
                                    options = leaflet.esri::featureLayerOptions(where = where_huc8_middle,
                                                                                style = DMAcolor,
                                                                                simplifyFactor = 0.35),
                                    group = "Land Ownership or Jurisdiction (Middle Willamette Subbasins)",
                                    pathOptions = leaflet::pathOptions(pane = "dma"),
                                    fill = TRUE,
                                    fillOpacity = 0.75,
                                    weight = 0.1,
                                    opacity = 0,
                                    color = "white",
                                    labelProperty = htmlwidgets::JS("function(feature){var props = feature.properties; return props.DMA_RP+\" \"}"),
                                    labelOptions = leaflet::labelOptions(offset = c(0,0),
                                                                         opacity = 1,
                                                                         textsize = "12px",
                                                                         sticky = TRUE),
                                    highlightOptions = leaflet::highlightOptions(weight = 3,
                                                                                 color = "white",
                                                                                 opacity = 1,
                                                                                 bringToFront = TRUE),
                                    popupProperty = htmlwidgets::JS(paste0('function(feature){var props = feature.properties; return \"',
                                                                           '<b>\"+props.DMA_RP+\"</b>',
                                                                           '<br>\"+props.DMA_RP_Cl+\"',
                                                                           '<br><b>HUC12:</b> \"+props.HUC12+\"',
                                                                           '<br><b>Version:</b> \"+props.Version+\"',
                                                                           ' \"}')),
                                    popupOptions = leaflet::popupOptions(maxWidth = 800, maxHeight = 500)) %>%
  leaflet.esri::addEsriFeatureLayer(url="https://services.arcgis.com/uUvqNMGPm7axC2dD/ArcGIS/rest/services/OR_DMAs/FeatureServer/0",
                                    options = leaflet.esri::featureLayerOptions(where = where_huc8_southern,
                                                                                style = DMAcolor,
                                                                                simplifyFactor = 0.35),
                                    group = "Land Ownership or Jurisdiction (Southern Willamette Subbasins)",
                                    pathOptions = leaflet::pathOptions(pane = "dma"),
                                    fill = TRUE,
                                    fillOpacity = 0.75,
                                    weight = 0.1,
                                    opacity = 0,
                                    color = "white",
                                    labelProperty = htmlwidgets::JS("function(feature){var props = feature.properties; return props.DMA_RP+\" \"}"),
                                    labelOptions = leaflet::labelOptions(offset = c(0,0),
                                                                         opacity = 1,
                                                                         textsize = "12px",
                                                                         sticky = TRUE),
                                    highlightOptions = leaflet::highlightOptions(weight = 3,
                                                                                 color = "white",
                                                                                 opacity = 1,
                                                                                 bringToFront = TRUE),
                                    popupProperty = htmlwidgets::JS(paste0('function(feature){var props = feature.properties; return \"',
                                                                           '<b>\"+props.DMA_RP+\"</b>',
                                                                           '<br>\"+props.DMA_RP_Cl+\"',
                                                                           '<br><b>HUC12:</b> \"+props.HUC12+\"',
                                                                           '<br><b>Version:</b> \"+props.Version+\"',
                                                                           ' \"}')),
                                    popupOptions = leaflet::popupOptions(maxWidth = 800, maxHeight = 500))
# Project area layer ----
# ____ * New models: BES 2019 ----
bes_model_extent <- sf::st_read(dsn = "//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/model_QAPPs/R/data/gis/bes_pro_reaches.shp",
                                layer = "bes_pro_reaches")  %>% 
  sf::st_transform(4326) %>% 
  sf::st_zm() %>% 
  dplyr::rename(Stream = NAME)

map_area <- map_basic %>% 
  projectScope(pro_area) %>% 
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
  hsSolarArea(hs_solar_model_area) %>% 
  ceModel(ce_model_extent) %>% 
  tempStation.markers(temp_stations) %>% 
  tempCalibration.markers(temp_cal_sites) %>% 
  tempBoundaryTributary.markers(temp_model_bc_tri) %>% 
  flowStation.markers(flow_stations) %>% 
  flowBoundaryTributary.markers(flow_model_bc_tri) %>% 
  metStation.markders(met_stations) %>% 
  indPS.markers(ind_ps) %>% 
  genPS.markers(gen_ps) %>% 
  leaflet::addLayersControl(overlayGroups = c("Willamette Subbasins TMDL Project Scope",
                                              "Willamette River Mainstem and Major Tributaries TMDL Project Scope",
                                              "Heat Source Shade Model Extent (New Models)",
                                              group.names),
                            options = leaflet::layersControlOptions(collapsed = FALSE, autoZIndex = TRUE)) %>% 
  leaflet::hideGroup(c("Willamette River Mainstem and Major Tributaries TMDL Project Scope",
                       "Heat Source Shade Model Extent (New Models)",
                       group.names.hide))

# Add-ons ----
map_final <- map_area %>% 
  # __ Search function ----
leaflet.extras::addSearchFeatures(targetGroups = group.names.search,
                                  options = leaflet.extras::searchFeaturesOptions(zoom = 10, 
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
