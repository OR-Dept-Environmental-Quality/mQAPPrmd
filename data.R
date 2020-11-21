library(tidyverse)
library(readxl)
library(rgdal)
library(sf)
library(ggplot2)

# Functions ----

strip_alpha <- function(x) {
  
  x2<- gsub(pattern="[a-z]$", replacement="", x=x, ignore.case = TRUE)
  
  return(x2)
}

strip_tbl_num <- function(x) {
  
  m <- tbls(name = x, display="cite")
  n <- as.numeric(gsub(pattern="Table ", replacement="", x=m, ignore.case = TRUE))
  
  return(n)
}

# General data ----

load("//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/data/R/statewide/df_awqms_raw_state.RData") # df.awqms.raw.state
load("//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/data/R/statewide/df_stations_state.RData") #df.stations.state
data.dir <- "//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/model_QAPPs/R/data/"
load(paste0(data.dir,"/download/data_sources.RData"))
cal.model <- readxl::read_xlsx(paste0(data.dir, "Model_Setup_Info.xlsx"), sheet = "Calibration Model Setup Info")
cal.input <- readxl::read_xlsx(paste0(data.dir, "Model_Setup_Info.xlsx"), sheet = "Calibration Inputs")
ref <- readxl::read_xlsx(paste0(data.dir, "Model_Setup_Info.xlsx"), sheet = "References")
npdes.ind <- readxl::read_xlsx(paste0(data.dir, "NPDES_communication_list.xlsx"), sheet = "Individual_NDPES")
npdes.gen <- readxl::read_xlsx(paste0(data.dir, "NPDES_communication_list.xlsx"), sheet = "Gen_NPDES")
agrimet.stations <- read.csv(paste0(data.dir, "download/agrimet_stations.csv"))
agrimet.parameters <- read.csv(paste0(data.dir, "download/agrimet_parameters.csv"))
hydromet <- read.csv(paste0(data.dir, "download/hydromet.csv"))
lookup_huc <- readxl::read_xlsx(paste0(data.dir, "Lookup_QAPPProjectArea_HUC10.xlsx"), sheet = "Lookup_QAPPProjectArea_HUC10")
web.huc8 <- sf::read_sf(dsn = "//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/GIS/Study_Areas_v5_HUC8_scope.shp",
                        layer = "Study_Areas_v5_HUC8_scope")
qapp_project_areas <- read.csv(paste0(data.dir,"qapp_project_area.csv"))

# Project Area Data ----

## for test:
# qapp_project_area = "Applegate, Illinois, Lower Rogue, and Middle Rogue Subbasins"
# qapp_project_area = "John Day River Basin"
# qapp_project_area = "Lower Grande Ronde, Imnaha, and Wallowa Subbasins"
# qapp_project_area = "Lower Willamette, Clackamas, and Sandy Subbasins"
# qapp_project_area = "Malheur River Subbasins"
# qapp_project_area = "Mid Willamette Subbasins"
# qapp_project_area = "Middle Columbia-Hood, Miles Creeks"
# qapp_project_area = "North Umpqua Subbasin"
# qapp_project_area = "South Umpqua and Umpqua Subbasins"
# qapp_project_area = "Southern Willamette Subbasins"
# qapp_project_area = "Upper Rogue Subbasin"
# qapp_project_area = "Walla Walla Subbasin"
# qapp_project_area = "Willamette River Mainstem and Major Tributaries"
# qapp_project_area = "Willow Creek Subbasin"

for (qapp_project_area in qapp_project_areas$areas) {
  
  huc8.extent <- qapp_project_areas[which(qapp_project_areas$areas == qapp_project_area),]$huc8.extent
  file.name <- qapp_project_areas[which(qapp_project_areas$areas == qapp_project_area),]$file.name
  
  subbasin <- unique(lookup_huc[which(lookup_huc$QAPP_Project_Area == qapp_project_area),]$HUC8_NAME)
  subbasin_num <- unique(lookup_huc[which(lookup_huc$QAPP_Project_Area == qapp_project_area),]$HUC8)
  
  # _ AWQMS ----
  station_awqms <- df.stations.state %>% 
    dplyr::filter(HUC8_Name %in% subbasin) %>% 
    dplyr::rename(`Station ID` = MLocID)
  
  station_model <- cal.input %>% 
    dplyr::filter(`QAPP Project Area` %in%  qapp_project_area) %>% 
    dplyr::left_join(station_awqms[,c("Station ID", "StationDes", "OrgID")], by="Station ID")
  
  data <- df.awqms.raw.state %>% 
    dplyr::filter(HUC8_Name %in%  subbasin) %>% 
    # QA/QC check:
    dplyr::filter(Result_status %in% c("Final", "Provisional") | QualifierAbbr %in% c("DQL=A","DQL=B","DQL=E"))
  
  # data.sample.count will be used in the Section 5.3
  data.sample.count <- data %>% 
    dplyr::filter(Statistical_Base == "Maximum") %>% 
    dplyr::select(Org_Name, MLocID, SampleStartDate, Statistical_Base, Result_Numeric) %>% 
    dplyr::mutate(date = lubridate::date(SampleStartDate),
                  month=lubridate::month(SampleStartDate, label=TRUE, abbr=TRUE),
                  year=lubridate::year(SampleStartDate)) %>% 
    dplyr::distinct(Org_Name, MLocID, Statistical_Base, Result_Numeric, date, .keep_all=TRUE) %>% 
    dplyr::group_by(MLocID, Statistical_Base, year, month) %>%
    dplyr::summarize(Org_Names=paste0(unique(Org_Name), collapse=" ,"),n=n()) %>%
    dplyr::ungroup() %>%
    tidyr::pivot_wider(names_from = month, values_from=n) %>%
    dplyr::rename(`Station ID` = MLocID) %>% 
    dplyr::left_join(station_awqms[,c("Station ID", "StationDes")], by="Station ID") %>% 
    dplyr::rename(Year = year)
  
  model.info <- cal.model %>% 
    dplyr::filter(`QAPP Project Area` %in%  qapp_project_area)
  
  model.input  <- cal.input %>% 
    dplyr::filter(`QAPP Project Area` %in%  qapp_project_area) %>% 
    dplyr::mutate(Latitude = round(as.numeric(Latitude),4),
                  Longitude = round(Longitude,3))
  
  qapp_project_area_huc8 <- web.huc8 %>% 
    dplyr::filter(HUC_8 %in% subbasin_num)
  
  qapp_project_area_huc8_union <-  sf::st_union(qapp_project_area_huc8)
  
  # _ USGS flow data ----
  usgs.stations <- usgs.stations.or %>%  # Discharge [ft3/s]
    dplyr::filter(!(site_tp_cd %in% c("SP","GW"))) %>% # ST = Stream
    dplyr::filter(data_type_cd %in% c("dv", "id", "iv")) %>% # dv=daily values; id=historical instantaneous values; iv=instantaneous values
    dplyr::filter(huc_cd %in% subbasin_num) %>%
    dplyr::filter(!is.na(dec_lat_va)) %>% 
    dplyr::select(agency_cd, site_no, station_nm, site_tp_cd, dec_lat_va, dec_long_va,begin_date, end_date) %>% 
    dplyr::mutate(long_sf=dec_long_va,
                  lat_sf=dec_lat_va) %>%
    dplyr::distinct() %>%
    sf::st_as_sf(coords = c("long_sf",  "lat_sf"), crs = sf::st_crs("+init=EPSG:4269"))
  
  usgs.stations.subbasin <-  sf::st_join(x = usgs.stations,
                                         y = qapp_project_area_huc8,
                                         join = st_intersects,
                                         left = TRUE)
  
  #ggplot() +
  #  geom_sf(data = qapp_project_area_huc8) +
  #  geom_sf(data = usgs.stations.subbasin)
  
  sf::st_geometry(usgs.stations.subbasin) <- NULL
  
  usgs.station.tbl <- usgs.stations.subbasin %>% 
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
    dplyr::arrange(`station_nm`)
  
  # _ NCDC met data ----
  ncdc.stations <- ncdc.station.or %>% 
    dplyr::mutate(lat = latitude,
                  long = longitude) %>% 
    sf::st_as_sf(coords = c("longitude", "latitude"), crs = sf::st_crs("+init=EPSG:4269"))
  
  ncdc.stations.huc8 <- ncdc.stations %>% 
    filter(sf::st_contains(qapp_project_area_huc8_union, ., sparse = FALSE))
  
  ncdc.stations.subbasin <-  sf::st_join(x = ncdc.stations.huc8,
                                         y = qapp_project_area_huc8,
                                         join = st_intersects,
                                         left = TRUE)
  
  sf::st_geometry(ncdc.stations.subbasin) <- NULL
  
  ncdc.datacats <- ncdc.datacats.or %>% 
    dplyr::filter(name %in% c("Air Temperature", "Precipitation", "Weather Type", "Wind", "Wind Speed", "Cloudiness", "Relative Humidity", "Humidity")) %>% 
    dplyr::group_by(station.id) %>% 
    dplyr::summarise(Parameter = toString(sort(name))) %>% 
    dplyr::ungroup()
  
  ncdc.station.tbl <- ncdc.stations.subbasin %>% 
    dplyr::rename(station.id = id) %>% 
    dplyr::left_join(ncdc.datacats, by = "station.id") %>% 
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
    dplyr::mutate_at("name", str_replace_all, "Sw", "SW")
  
  # _ RAWS met data ----
  raws.stations <- raws.meta %>% 
    dplyr::mutate(lat = latitude,
                  long = longitude) %>%
    sf::st_as_sf(coords = c("longitude", "latitude"), crs = sf::st_crs("+init=EPSG:4269"))
  
  raws.stations.huc8 <- raws.stations %>% 
    filter(sf::st_contains(qapp_project_area_huc8_union, ., sparse = FALSE))
  
  raws.stations.subbasin <-  sf::st_join(x = raws.stations.huc8,
                                         y = qapp_project_area_huc8,
                                         join = st_intersects,
                                         left = TRUE)
  
  sf::st_geometry(raws.stations.subbasin) <- NULL
  
  raws.station.data.type <- raws.data.type %>% 
    dplyr::rename(Parameter = "unlist.type.list.columnNames.") %>% 
    dplyr::filter(Parameter %in% c("humidity","precipitation","temperature","windDirection","windSpeed")) %>% 
    dplyr::mutate_at("Parameter", str_replace_all, "humidity", "Humidity") %>% 
    dplyr::mutate_at("Parameter", str_replace_all, "precipitation", "Precipitation") %>%
    dplyr::mutate_at("Parameter", str_replace_all, "temperature", "Temperature") %>%
    dplyr::mutate_at("Parameter", str_replace_all, "windDirection", "Wind Direction") %>%
    dplyr::mutate_at("Parameter", str_replace_all, "windSpeed", "Wind Speed")
    #dplyr::group_by(wrccID) %>% 
    #dplyr::summarise(Parameter = toString(sort(Parameter)))

  raws.station.tbl <- raws.stations.subbasin %>% 
    dplyr::left_join(raws.station.data.type, by = "wrccID")
  
  # _ USBR AgriMet ----
  agrimet.stations.or <- agrimet.stations %>%
    dplyr::filter(state == "OR") %>% 
    # dplyr::left_join(agrimet.parameters, by = "siteid") %>% 
    dplyr::mutate(lat = latitude, long = longitude) %>% 
    sf::st_as_sf(coords = c("longitude", "latitude"), crs = sf::st_crs("+init=EPSG:4269"))

  agrimet.stations.huc8 <- agrimet.stations.or %>% 
    filter(sf::st_contains(qapp_project_area_huc8_union, ., sparse = FALSE))
  
  #ggplot() +
  #  geom_sf(data = qapp_project_area_huc8) +
  #  geom_sf(data = agrimet.stations.or)
  
  agrimet.stations.subbasin <-  sf::st_join(x = agrimet.stations.huc8,
                                            y = qapp_project_area_huc8,
                                            join = st_intersects,
                                            left = TRUE)
  
  sf::st_geometry(agrimet.stations.subbasin) <- NULL
  
  agrimet.station.data.type <- agrimet.parameters %>% 
    dplyr::filter(Type %in% c("Air Temperature","Precipitation", "Relative Humidity", "Wind"))
    #dplyr::group_by(siteid) %>% 
    #dplyr::summarise(Type = toString(sort(Type)))
  
  agrimet.station.tbl <- agrimet.stations.subbasin %>% 
    dplyr::left_join(agrimet.station.data.type, by = "siteid")
  
  # _ USBR Hydromet ----
  hydromet.station.tbl <- hydromet %>% 
    dplyr::filter(`QAPP.Project.Area` == qapp_project_area)
  
  # _ MesoWest climate data ----
  mw.stations <- mw.meta$STATION %>%
    dplyr::mutate(lat = LATITUDE, long = LONGITUDE) %>% 
    sf::st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = sf::st_crs("+init=EPSG:4269"))
  
  mw.stations.huc8 <- mw.stations %>% 
    filter(sf::st_contains(qapp_project_area_huc8_union, ., sparse = FALSE))
  
  mw.stations.subbasin <-  sf::st_join(x = mw.stations.huc8,
                                       y = qapp_project_area_huc8,
                                       join = st_intersects,
                                       left = TRUE)
  
  sf::st_geometry(mw.stations.subbasin) <- NULL
  
  mw.station.tbl <- mw.stations.subbasin %>% 
    dplyr::mutate(NAME = stringr::str_to_title(NAME)) %>% 
    dplyr::mutate_at("NAME", str_replace_all, "Or", "OR") %>% 
    dplyr::mutate_at("NAME", str_replace_all, "ese", "ESE") %>% 
    dplyr::mutate_at("NAME", str_replace_all, "es", "SE") %>%
    dplyr::mutate_at("NAME", str_replace_all, "Ew", "EW") %>%
    dplyr::mutate_at("NAME", str_replace_all, "Sw", "SW") %>% 
    dplyr::mutate_at("NAME", str_replace_all, "Mp", "MP") %>% 
    dplyr::mutate_at("NAME", str_replace_all, "Nf", "NF") %>% 
    dplyr::mutate_at("NAME", str_replace_all, "Se", "SE") %>% 
    dplyr::mutate_at("NAME", str_replace_all, "se", "SE") %>% 
    dplyr::mutate_at("NAME", str_replace_all, "sse", "SSE")
  
  setwd("//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/model_QAPPs/R/data/RData")
  
  save(model.info,
       model.input,
       station_awqms,
       station_model,
       data.sample.count,
       npdes.ind,
       npdes.gen,
       usgs.station.tbl,
       ncdc.station.tbl,
       raws.station.tbl,
       agrimet.station.tbl,
       hydromet.station.tbl,
       mw.station.tbl,
       ref,
       data.dir,
       huc8.extent,
       file.name,
       qapp_project_area,
       qapp_project_areas,
       subbasin,
       subbasin_num,
       strip_alpha,
       strip_tbl_num,
       file = paste0(file.name,".RData"))
  
}
