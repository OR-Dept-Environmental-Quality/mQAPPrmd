library(tidyverse)
library(readxl)
library(dataRetrieval)
library(rgdal)
library(sf)
library(rnoaa) # NCDC
library(mesowest) # Mesowest

# RUN 1: ----
# Function ----
## Used in the TIR reference:
strip_alpha <- function(x) {
  
  x2<- gsub(pattern="[a-z]$", replacement="", x=x, ignore.case = TRUE)
  
  return(x2)
}
## Used for rmarkdown inline table number
strip_tbl_num <- function(x) {
  
  m <- tbls(name = x, display="cite")
  n <- as.numeric(gsub(pattern="Table ", replacement="", x=m, ignore.case = TRUE))
  
  return(n)
}

# Model Setup Info, NPDES and AWQMS data----
load("//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/data/R/statewide/df_awqms_raw_state.RData") # df.awqms.raw.state
load("//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/data/R/statewide/df_stations_state.RData") #df.stations.state
data.dir <- "//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/model_QAPPs/R/data/"
cal.model <- readxl::read_xlsx(paste0(data.dir, "Model_Setup_Info.xlsx"), sheet = "Calibration Model Setup Info")
cal.input <- readxl::read_xlsx(paste0(data.dir, "Model_Setup_Info.xlsx"), sheet = "Calibration Inputs")
ref <- readxl::read_xlsx(paste0(data.dir, "Model_Setup_Info.xlsx"), sheet = "References")
npdes.ind <- readxl::read_xlsx(paste0(data.dir, "NPDES_communication_list.xlsx"), sheet = "Individual_NDPES")
npdes.gen <- readxl::read_xlsx(paste0(data.dir, "NPDES_communication_list.xlsx"), sheet = "Gen_NPDES")
lookup_huc <- readxl::read_xlsx(paste0(data.dir, "Lookup_QAPPProjectArea_HUC10.xlsx"), sheet = "Lookup_QAPPProjectArea_HUC10")
web.huc8 <- sf::read_sf(dsn = "T:/Temperature_TMDL_Revisions/GIS/Study_Areas_v5_HUC8_scope.shp",
                        layer = "Study_Areas_v5_HUC8_scope")

# RUN 2: ----
# Project Area Info. ----
# _ (1) "Applegate, Illinois, Lower Rogue, and Middle Rogue Subbasins" ----
qapp_project_area = "Applegate, Illinois, Lower Rogue, and Middle Rogue Subbasins"

#usgs.station.tbl <- read.csv(paste0(data.dir, "flow/usgs_station_tbl_ApplegateIllinoisLowerRogueMiddleRogue.csv"))
#ncdc.station.tbl <- read.csv(paste0(data.dir, "met/ncdc_station_tbl_ApplegateIllinoisLowerRogueMiddleRogue.csv"))

# for NCDC meteorological data
## qapp_project_area_huc8
## bbox: xmin: -124.4334 ymin: 41.99523 xmax: -122.4746 ymax: 42.79945
huc8.extent <- c(41.99523,-124.4334,42.79945,-122.4746)

# _ (2) "John Day River Basin" ----
qapp_project_area = "John Day River Basin"

#usgs.station.tbl <- read.csv(paste0(data.dir, "flow/usgs_station_tbl_JohnDayRiver.csv"))
#ncdc.station.tbl <- read.csv(paste0(data.dir, "met/ncdc_station_tbl_JohnDayRiver.csv"))

# for NCDC meteorological data
## qapp_project_area_huc8
## bbox: xmin: -120.821 ymin: 43.90315 xmax: -118.2234 ymax: 45.7355
huc8.extent <- c(43.90315,-120.821,45.7355,-118.2234)

# _ (3) "Lower Grande Ronde, Imnaha, and Wallowa Subbasins" ----
qapp_project_area = "Lower Grande Ronde, Imnaha, and Wallowa Subbasins"

#usgs.station.tbl <- read.csv(paste0(data.dir, "flow/usgs_station_tbl_LowerGrandeRondeImnahaWallowa.csv"))
#ncdc.station.tbl <- read.csv(paste0(data.dir, "met/ncdc_station_tbl_LowerGrandeRondeImnahaWallowa.csv"))

# for NCDC meteorological data
## qapp_project_area_huc8
# bbox: xmin: -117.957 ymin: 45.05496 xmax: -116.5826 ymax: 46.00037
huc8.extent <- c(45.05496,-117.957,46.00037,-116.5826)

# _ (4) "Lower Willamette, Clackamas, and Sandy Subbasins" ----
qapp_project_area = "Lower Willamette, Clackamas, and Sandy Subbasins"

#usgs.station.tbl <- read.csv(paste0(data.dir, "flow/usgs_station_tbl_LowerWillametteClackamasSandy.csv"))
#ncdc.station.tbl <- read.csv(paste0(data.dir, "met/ncdc_station_tbl_LowerWillametteClackamasSandy.csv"))

# for met data
## qapp_project_area_huc8
## bbox:xmin: -123.0536 ymin: 44.8004 xmax: -121.6514 ymax: 45.94129
huc8.extent <- c(44.8004,-123.0536,45.94129,-121.6514)

# _ (5) "Malheur River Subbasins" ----
qapp_project_area = "Malheur River Subbasins"

#usgs.station.tbl <- read.csv(paste0(data.dir, "flow/usgs_station_tbl_MalheurRiver.csv"))
#ncdc.station.tbl <- read.csv(paste0(data.dir, "met/ncdc_station_tbl_MalheurRiver.csv"))

# for met data
## qapp_project_area_huc8
## bbox: xmin: -118.8507 ymin: 43.04326 xmax: -116.894 ymax: 44.4603
huc8.extent <- c(43.04326,-118.8507,44.4603,-116.894)

# _ (6) "Mid Willamette Subbasins" ----
qapp_project_area = "Mid Willamette Subbasins"

#usgs.station.tbl <- read.csv(paste0(data.dir, "flow/usgs_station_tbl_MidWillamette.csv"))
#ncdc.station.tbl <- read.csv(paste0(data.dir, "met/ncdc_station_tbl_MidWillamette.csv"))

# for met data
## qapp_project_area_huc8
## bbox: xmin: -123.6173 ymin: 44.26713 xmax: -121.7597 ymax: 45.38605
huc8.extent <- c(44.26713,-123.6173,45.38605,-121.7597)

# _ (7) "Middle Columbia-Hood, Miles Creeks" ----
qapp_project_area = "Middle Columbia-Hood, Miles Creeks"

#usgs.station.tbl <- read.csv(paste0(data.dir, "flow/usgs_station_tbl_MiddleColumbia-HoodMilesCreeks.csv"))
#ncdc.station.tbl <- read.csv(paste0(data.dir, "met/ncdc_station_tbl_MiddleColumbia-HoodMilesCreeks.csv"))

# for met data
## qapp_project_area_huc8
## bbox: xmin: -121.8071 ymin: 45.2981 xmax: -120.9026 ymax: 45.71937
huc8.extent <- c(45.2981,-121.8071,45.71937,-120.9026)

# _ (8) "North Umpqua Subbasin" ----
qapp_project_area = "North Umpqua Subbasin"

#usgs.station.tbl <- read.csv(paste0(data.dir, "flow/usgs_station_tbl_NorthUmpqua.csv"))
#ncdc.station.tbl <- read.csv(paste0(data.dir, "met/ncdc_station_tbl_NorthUmpqua.csv"))

# for NCDC meteorological data
## qapp_project_area_huc8
## bbox: xmin: -123.4457 ymin: 42.97277 xmax: -121.9742 ymax: 43.59734
huc8.extent <- c(42.97277,-123.4457,43.59734,-121.9742)

# _ (9) "South Umpqua and Umpqua Subbasins" ----
qapp_project_area = "South Umpqua and Umpqua Subbasins"

#usgs.station.tbl <- read.csv(paste0(data.dir, "flow/usgs_station_tbl_SouthUmpquaUmpqua.csv"))
#ncdc.station.tbl <- read.csv(paste0(data.dir, "met/ncdc_station_tbl_SouthUmpquaUmpqua.csv"))

# for NCDC meteorological data
## qapp_project_area_huc8
## bbox: xmin: -124.2082 ymin: 42.69849 xmax: -122.4185 ymax: 43.97359
huc8.extent <- c(42.69849,-124.2082,43.97359,-122.4185)

# _ (10) "Southern Willamette Subbasins" ----
qapp_project_area = "Southern Willamette Subbasins"

#usgs.station.tbl <- read.csv(paste0(data.dir, "flow/usgs_station_tbl_SouthernWillamette.csv"))
#ncdc.station.tbl <- read.csv(paste0(data.dir, "met/ncdc_station_tbl_SouthernWillamette.csv"))

# for NCDC meteorological data
## qapp_project_area_huc8
## bbox: xmin: -123.6642 ymin: 43.356 xmax: -121.768 ymax: 44.90894
huc8.extent <- c(43.356,-123.6642,44.90894,-121.768)

# _ (11) "Upper Klamath and Lost Subbasins" ----
# _ (12) "Upper Rogue Subbasin" ----
qapp_project_area = "Upper Rogue Subbasin"

#usgs.station.tbl <- read.csv(paste0(data.dir, "flow/usgs_station_tbl_UpperRogue.csv"))
#ncdc.station.tbl <- read.csv(paste0(data.dir, "met/ncdc_station_tbl_UpperRogue.csv"))

# for NCDC meteorological data
## qapp_project_area_huc8
## bbox: xmin: -122.9461 ymin: 42.21762 xmax: -122.1365 ymax: 43.13418
huc8.extent <- c(42.21762,-122.9461,43.13418,-122.1365)

# _ (13) "Walla Walla Subbasin" ----
qapp_project_area = "Walla Walla Subbasin"

#usgs.station.tbl <- read.csv(paste0(data.dir, "flow/usgs_station_tbl_WallaWalla.csv"))
#ncdc.station.tbl <- read.csv(paste0(data.dir, "met/ncdc_station_tbl_WallaWalla.csv"))

# for NCDC meteorological data
## qapp_project_area_huc8
## bbox: xmin: -118.8499 ymin: 45.75956 xmax: -117.9185 ymax: 46.00114
huc8.extent <- c(45.75956,-118.8499,46.00114,-117.9185)

# _ (14) "Willamette River Mainstem and Major Tributaries" ----
# _ (15) "Willow Creek Subbasin" ----
qapp_project_area = "Willow Creek Subbasin"

#usgs.station.tbl <- read.csv(paste0(data.dir, "flow/usgs_station_tbl_WillowCreek.csv"))
#ncdc.station.tbl <- read.csv(paste0(data.dir, "met/ncdc_station_tbl_WillowCreek.csv"))

# for NCDC meteorological data
## qapp_project_area_huc8
## bbox: xmin: -120.1739 ymin: 45.10561 xmax: -119.2475 ymax: 45.80005
huc8.extent <- c(45.10561,-120.1739,45.80005,-119.2475)


# RUN 3: ----
subbasin <- unique(lookup_huc[which(lookup_huc$QAPP_Project_Area == qapp_project_area),]$HUC8_NAME)
subbasin_num <- unique(lookup_huc[which(lookup_huc$QAPP_Project_Area == qapp_project_area),]$HUC8) # for USGS flow data

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

# data.sample.count will be used in chap5 5.3
data.sample.count <- data %>% 
  dplyr::filter(Statistical_Base == "Maximum") %>% 
  dplyr::select(Org_Name, MLocID, SampleStartDate, Statistical_Base, Result_Numeric) %>% 
  dplyr::mutate(month=lubridate::month(SampleStartDate, label=TRUE, abbr=TRUE),
                year=lubridate::year(SampleStartDate)) %>% 
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

# RUN 4: USGS flow and met data When need to update ----
qapp_project_area_huc8 <- web.huc8 %>% 
  dplyr::filter(HUC_8 %in% subbasin_num)

qapp_project_area_huc8_union <-  sf::st_union(qapp_project_area_huc8) 

# _ USGS Flow data ----
usgs.stations <- dataRetrieval::whatNWISdata(stateCd="OR",
                                             parameterCd = "00060") %>%  # 00060	= Discharge [ft3/s]
  dplyr::filter(!(site_tp_cd %in% c("SP","GW"))) %>% # ST = Stream
  dplyr::filter(data_type_cd %in% c("dv", "id", "iv")) %>% # dv=daily values; id=historical instantaneous values; iv=instantaneous values
  dplyr::filter(huc_cd %in% subbasin_num) %>%
  dplyr::filter(!is.na(dec_lat_va)) %>% 
  dplyr::select(agency_cd, site_no, station_nm, site_tp_cd, dec_lat_va, dec_long_va,begin_date, end_date) %>% 
  dplyr::mutate(long_sf=dec_long_va,
                lat_sf=dec_lat_va) %>%
  dplyr::distinct() %>%
  sf::st_as_sf(coords = c("long_sf",  "lat_sf"), crs = sf::st_crs("+init=EPSG:4269"))

#ggplot2::ggplot() + 
#geom_sf(data = web.huc12, colour = "light gray", fill = NA) +
#geom_sf(data = web.huc8, colour = "blue", fill = NA) +
#geom_sf(data = qapp_project_area_huc8, colour = "red", fill = NA)

usgs.stations.subbasin <-  sf::st_join(x = usgs.stations,
                                       y = qapp_project_area_huc8,
                                       join = st_intersects,
                                       left = TRUE)

#ggplot2::ggplot(usgs.stations.subbasin) + geom_sf()

#ggplot() +
#geom_sf(data = qapp_project_area_huc8)+
# geom_sf(data = qapp_project_area_huc12) +
#geom_sf(data = usgs.stations.subbasin)

sf::st_geometry(usgs.stations.subbasin) <- NULL

usgs.station.tbl <- usgs.stations.subbasin %>% 
  dplyr::mutate(`Station Name and ID` = paste0(station_nm, " (", site_no, ")")) %>% 
  dplyr::mutate(`Latitude/Longitude` = paste0(round(dec_lat_va,4), ", ",round(dec_long_va,3))) %>% 
  dplyr::select(`Station Name and ID`, `Latitude/Longitude`, begin_date, end_date) %>% 
  dplyr::rename(`Begin Date` = begin_date,
                `End Date` = end_date) %>%
  dplyr::mutate_at("Station Name and ID", str_replace_all, " R ", " RIVER ") %>% 
  dplyr::mutate_at("Station Name and ID", str_replace_all, " @ ", " AT ") %>% 
  dplyr::mutate_at("Station Name and ID", str_replace_all, " & ", " AND ") %>% 
  dplyr::mutate_at("Station Name and ID", str_replace_all, " CK ", " CREEK ") %>% 
  dplyr::mutate_at("Station Name and ID", str_replace_all, " CR ", " CREEK ") %>% 
  dplyr::mutate_at("Station Name and ID", str_replace_all, " NR ", " NEAR ") %>% 
  dplyr::mutate_at("Station Name and ID", str_replace_all, "N\\.", "NORTH ") %>% 
  dplyr::mutate_at("Station Name and ID", str_replace_all, "N ", "NORTH ") %>% 
  dplyr::mutate_at("Station Name and ID", str_replace_all, " ABV ", " ABOVE ") %>% 
  dplyr::mutate_at("Station Name and ID", str_replace_all, " BLW ", " BELOW ") %>% 
  dplyr::mutate_at("Station Name and ID", str_replace_all, " P ", " POWER ") %>% 
  dplyr::mutate_at("Station Name and ID", str_replace_all, " CA ", " CANAL ") %>% 
  dplyr::mutate_at("Station Name and ID", str_replace_all, "OREG", "OR") %>% 
  dplyr::mutate_at("Station Name and ID", str_replace_all, "\\.", "") %>% 
  dplyr::mutate_at("Station Name and ID", str_replace_all, "T FLS", "TOKETEE FALLS") %>% 
  dplyr::mutate_at("Station Name and ID", str_replace_all, ",OR", "") %>% 
  dplyr::mutate_at("Station Name and ID", str_replace_all, ", OR", "") %>% 
  dplyr::mutate_at("Station Name and ID", str_replace_all, " OR", "") %>% 
  dplyr::mutate(`Station Name and ID` = stringr::str_to_title(`Station Name and ID`)) %>% 
  dplyr::arrange(`Station Name and ID`)

# write.csv(usgs.station.tbl, "usgs_station_tbl_LowerGrandeRondeImnahaWallowa.csv")

# _ NCDC meteorological data ----
# NOAA National Climatic Data Center: https://www.ncdc.noaa.gov/
options(noaakey = "aQnyFVAjwXAXPGTLMWeGkJLVllZPJHuk")
# Find "huc8.extent" above in the project area section
ncdc.stations.extent <- rnoaa::ncdc_stations(extent = huc8.extent,limit = 1000)

ncdc.stations <- ncdc.stations.extent$data %>% 
  dplyr::mutate(lat = latitude,
                long = longitude) %>% 
  sf::st_as_sf(coords = c("longitude", "latitude"), crs = sf::st_crs("+init=EPSG:4269"))

#ggplot() +
#geom_sf(data = qapp_project_area_huc8) +
#geom_sf(data = qapp_project_area_huc8_union) +
#geom_sf(data = ncdc.stations)

ncdc.stations.huc8 <- ncdc.stations %>% 
  filter(sf::st_contains(qapp_project_area_huc8_union, ., sparse = FALSE))

#ggplot() +
#geom_sf(data = qapp_project_area_huc8) +
#geom_sf(data = ncdc.stations.huc8)

ncdc.stations.subbasin <-  sf::st_join(x = ncdc.stations.huc8,
                                       y = qapp_project_area_huc8,
                                       join = st_intersects,
                                       left = TRUE)

sf::st_geometry(ncdc.stations.subbasin) <- NULL

# Datesets: From the NOAA API docs: All of our data are in datasets.
# To retrieve any data from us, you must know what dataset it is in.
##ncdc.datasets <- NULL
##for(id in 1:length(ncdc.stations.subbasin$id)){
##  get <- rnoaa::ncdc_datasets(stationid = ncdc.stations.subbasin$id[id],
##                              limit = 1000)
##  if(NROW(get$data)>0){
##    get$data$station.id <- ncdc.stations.subbasin$id[id]
##  }
##    ncdc.datasets <- rbind(ncdc.datasets,get$data)
##}
##ncdc.datasets

# Data types: From the NOAA API docs: Describes the type of data, acts as a label.
# For example: If it’s 64 degrees out right now, then the data type is Air Temperature and the data is 64.
##ncdc.datatypes <- NULL
##for(id in 1:length(ncdc.stations.subbasin$id)){
##  get <- ncdc_datatypes(stationid = ncdc.stations.subbasin$id[id],
##                       limit = 1000)
##  if(NROW(get$data)>0){
##    get$data$station.id <- ncdc.stations.subbasin$id[id]
##  }
##  ncdc.datatypes <- rbind(ncdc.datatypes,get$data)
##}
##ncdc.datatypes

#### 
# Date categories: Data Categories represent groupings of data types.
#ncdc.station.datacats <- NULL

#for(id in 1:length(ncdc.stations.subbasin$id)){
#  get <- ncdc_datacats(stationid = ncdc.stations.subbasin$id[id],
#                       limit = 1000)
#  if(NROW(get$data)>0){
#    get$data$station.id <- ncdc.stations.subbasin$id[id]
#  }
#  ncdc.station.datacats <- rbind(ncdc.station.datacats,get$data)
#}

#ncdc.datacats <- ncdc.station.datacats %>% 
#  dplyr::filter(name %in% c("Air Temperature", "Precipitation", "Weather Type", "Wind", "wind speed", "Cloudiness", "relative humidity")) %>% 
#  dplyr::group_by(station.id) %>% 
#  dplyr::summarise(Parameter = toString(name))

ncdc.station.tbl <- ncdc.stations.subbasin %>% 
  dplyr::rename(station.id = id) %>% 
  #dplyr::left_join(ncdc.datacats, by = "station.id") %>% 
  dplyr::mutate_at("name", str_replace_all, ", OR US", "") %>% 
  dplyr::mutate(name = stringr::str_to_title(name)) %>% 
  dplyr::mutate_at("name", str_replace_all, "Nw", "NW") %>% 
  dplyr::mutate_at("name", str_replace_all, "Ne", "NE") %>% 
  dplyr::mutate_at("name", str_replace_all, "Nnw", "NNW") %>% 
  dplyr::mutate_at("name", str_replace_all, "Se", "SE") %>% 
  dplyr::mutate_at("name", str_replace_all, "Ese", "ESE") %>% 
  dplyr::mutate_at("name", str_replace_all, "Wnw", "WNW") %>% 
  dplyr::mutate_at("name", str_replace_all, "Ssw", "SSW")

# write.csv(ncdc.station.tbl, "ncdc_station_tbl_LowerGrandeRondeImnahaWallowa.csv")

# _ NIFC RAWS meteorological data ----
# Remote Automatic Weather Stations: https://raws.nifc.gov/
# Github installation: https://github.com/MazamaScience/RAWSmet
#devtools::install_github("MazamaScience/MazamaSpatialUtils@jon")
#devtools::install_github('MazamaScience/RAWSmet')

library(RAWSmet)
# dir.create('//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/model_QAPPs/R/data/RAWS', recursive = TRUE)
setRawsDataDir('//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/model_QAPPs/R/data/RAWS')
raws.meta <- RAWSmet::wrcc_loadMeta(stateCode = "OR")

#parameters:
#a <- RAWSmet::example_wrccList
#colnames(a$waWSAD$dat)
#[1] "datetime"         "temperature"      "humidity"         "windSpeed"        "windDirection"    "maxGustSpeed"     "maxGustDirection"
#[8] "precipitation"    "solarRadiation"   "fuelMoisture"     "fuelTemperature"  "monitorType"

raws.stations <- raws.meta %>% 
  dplyr::mutate(lat = latitude,
                long = longitude) %>%
  sf::st_as_sf(coords = c("longitude", "latitude"), crs = sf::st_crs("+init=EPSG:4269"))

raws.stations.huc8 <- raws.stations %>% 
  filter(sf::st_contains(qapp_project_area_huc8_union, ., sparse = FALSE))

#ggplot() +
#geom_sf(data = qapp_project_area_huc8) +
#geom_sf(data = raws.stations)

raws.stations.subbasin <-  sf::st_join(x = raws.stations.huc8,
                                       y = qapp_project_area_huc8,
                                       join = st_intersects,
                                       left = TRUE)

sf::st_geometry(raws.stations.subbasin) <- NULL

raws.station.tbl <- raws.stations.subbasin

# _ USBR AgriMet ----
# Bureau of Reclamation Columbia-Pacific Northwest Region: https://www.usbr.gov/pn/agrimet/
agrimet.stations.or <- read.csv("T:/Temperature_TMDL_Revisions/model_QAPPs/R/data/agrimet_stations.csv") %>% 
  dplyr::filter(state == "OR")
agrimet.parameters <- read.csv("T:/Temperature_TMDL_Revisions/model_QAPPs/R/data/agrimet_parameters.csv")
agrimet.stations <- agrimet.stations.or  %>%
  dplyr::left_join(agrimet.parameters, by = "siteid") %>% 
  dplyr::mutate(lat = latitude, long = longitude) %>% 
  sf::st_as_sf(coords = c("longitude", "latitude"), crs = sf::st_crs("+init=EPSG:4269"))

#ggplot() +
#geom_sf(data = agrimet.stations)

agrimet.stations.huc8 <- agrimet.stations %>% 
  filter(sf::st_contains(qapp_project_area_huc8_union, ., sparse = FALSE))

#ggplot() +
#geom_sf(data = web.huc8)+
#geom_sf(data = agrimet.stations)

#ggplot() +
#geom_sf(data = qapp_project_area_huc8) +
#geom_sf(data = agrimet.stations.huc8)

agrimet.stations.subbasin <-  sf::st_join(x = agrimet.stations.huc8,
                                       y = qapp_project_area_huc8,
                                       join = st_intersects,
                                       left = TRUE)

sf::st_geometry(agrimet.stations.subbasin) <- NULL

agrimet.station.tbl <- agrimet.stations.subbasin

# _ MesoWest climate data ----
#devtools::install_github('fickse/mesowest')
#mesowest::requestToken(apikey = "KyGeNUAVnZg7VgSnUe9zVv15e1yg2hxTUnZ4SdZw0y")
# RUN "mw.meta" when need to update.
#mw.meta <- mesowest::mw(service = "metadata", state = "OR")
#write.csv(mw.meta$STATION,"mw_meta.csv")
#mw.variables.list  <- mwvariables()
#mw.variables <- data.frame(matrix(unlist(mw.variables.list$VARIABLES)))
#library(purrr)
#mw.variables <- purrr::map_df(mw.variables.list$VARIABLES, ~as.data.frame(.x), .id="id")
#write.csv(mw.variables,"mw_variables.csv")
mw.meta <- read.csv(paste0(data.dir, "met/mw_meta.csv"))
mw.stations <- mw.meta %>% # for downloading meta data (a list), should be "mw.meta$STATION"
  dplyr::mutate(lat = LATITUDE, long = LONGITUDE) %>% 
  sf::st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = sf::st_crs("+init=EPSG:4269"))

mw.stations.huc8 <- mw.stations %>% 
  filter(sf::st_contains(qapp_project_area_huc8_union, ., sparse = FALSE))

mw.stations.subbasin <-  sf::st_join(x = mw.stations.huc8,
                                     y = qapp_project_area_huc8,
                                     join = st_intersects,
                                     left = TRUE)

#ggplot() +
#geom_sf(data = qapp_project_area_huc8) +
#geom_sf(data = mw.stations.subbasin)

sf::st_geometry(mw.stations.subbasin) <- NULL

mw.station.tbl <- mw.stations.subbasin

# SAVE DATA ----

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
     mw.station.tbl,
     ref,
     data.dir,
     huc8.extent,
     qapp_project_area,
     subbasin,
     subbasin_num,
     strip_alpha,
     strip_tbl_num,
     file = "data.RData")

# Note ----
```{r, label=`load-data`, include=FALSE}

# load("data.RData")

```

