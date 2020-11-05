library(tidyverse)
library(readxl)
library(dataRetrieval)
library(rgdal)
library(sf)
library(rnoaa) # NCDC
library(mesowest) # Mesowest


# Function ----
## Used in the TIR reference:
strip_alpha <- function(x) {
  
  x2<- gsub(pattern="[a-z]$", replacement="", x=x, ignore.case = TRUE)
  
  return(x2)
}


# Model Setup Info and NPDES data----
data.dir <- "T:/Temperature_TMDL_Revisions/model_QAPPs/R/data/"

cal.model <- readxl::read_xlsx(paste0(data.dir, "Model_Setup_Info.xlsx"), sheet = "Calibration Model Setup Info")
cal.input <- readxl::read_xlsx(paste0(data.dir, "Model_Setup_Info.xlsx"), sheet = "Calibration Inputs")
ref <- readxl::read_xlsx(paste0(data.dir, "Model_Setup_Info.xlsx"), sheet = "References")
npdes.ind <- readxl::read_xlsx(paste0(data.dir, "NPDES_communication_list.xlsx"), sheet = "Individual_NDPES")
npdes.gen <- readxl::read_xlsx(paste0(data.dir, "NPDES_communication_list.xlsx"), sheet = "Gen_NPDES")

# Project Area Info. ----
# _ (1) "Applegate, Illinois, Lower Rogue, and Middle Rogue Subbasins" ----
# _ (2) "John Day River Basin" ----
# _ (3) "Lower Grande Ronde, Imnaha, and Wallowa Subbasins" ----
# _ (4) "Lower Willamette, Clackamas, and Sandy Subbasins" ----
# _ (5) "Malheur River Subbasins" ----
# _ (6) "Mid Willamette Subbasins" ----
# _ (7) "Middle Columbia-Hood, Miles Creeks" ----
# _ (8) "North Umpqua Subbasin" ----
qapp_project_area = "North Umpqua Subbasin"

load("T:/Temperature_TMDL_Revisions/data/R/statewide/df_awqms_raw_Umpqua Basin.RData") # df.awqms.raw
load("T:/Temperature_TMDL_Revisions/data/R/statewide/df_stations_state.RData") # df.stations.state
unique(sort(df.awqms.raw$HUC8_Name))
# "North Umpqua" "South Umpqua" "Umpqua"
subbasin <- "North Umpqua"
subbasin_num <- unique(df.awqms.raw[which(df.awqms.raw$HUC8_Name == subbasin),]$HUC8) # for USGS flow data

station_awqms <- df.stations.state %>% 
  dplyr::filter(HUC8_Name %in%  subbasin) %>% 
  # Check and compare the HUC10_Names to QAPP Model Waterbody names, which are also sub-section names
  # write.csv(station_awqms,"station_awqms.csv")
  # UPDATE REQUIRED:
  dplyr::mutate(section_name = ifelse(HUC10_Name %in%  c("Headwaters North Umpqua River", 
                                                         "Lower North Umpqua River",
                                                         "Middle North Umpqua River",
                                                         "Upper North Umpqua River"),
                                      "North Umpqua River",
                                      HUC10_Name)) %>% 
  dplyr::mutate(section_name = ifelse(HUC12_Name %in%  c("Upper Cavitt Creek"),
                                      "Cavitt Creek",
                                      section_name)) %>% 
  dplyr::mutate(section_name = ifelse(HUC12_Name %in%  c("Lake Creek"),
                                      "Lake Creek",
                                      section_name)) %>% 
  dplyr::rename(`Station ID` = MLocID,
                `Station Description` = StationDes,
                Organization = OrgID,
                Latitude = Lat_DD,
                Longitude = Long_DD)

# unique(sort(station_awqms$section_name))

# for NCDC meteorological data
## qapp_project_area_huc8
## bbox: xmin: -123.4457 ymin: 42.97277 xmax: -121.9742 ymax: 43.59734
huc8.extent <- c(42.97277,-123.4457,43.59734,-121.9742)


# _ (9) "South Umpqua and Umpqua Subbasins" ----
# _ (10) "Southern Willamette Subbasins" ----
# _ (11) "Upper Klamath and Lost Subbasins" ----
# _ (12) "Upper Rogue Subbasin" ----
qapp_project_area = "Upper Rogue Subbasin"

load("T:/Temperature_TMDL_Revisions/data/R/statewide/df_awqms_raw_Rogue Basin.RData") # df.awqms.raw
load("T:/Temperature_TMDL_Revisions/data/R/statewide/df_stations_state.RData") # df.stations.state
unique(sort(df.awqms.raw$HUC8_Name))
# "Applegate"    "Illinois"     "Lower Rogue"  "Middle Rogue" "Upper Rogue"
subbasin <- "Upper Rogue"
subbasin_num <- unique(df.awqms.raw[which(df.awqms.raw$HUC8_Name == subbasin),]$HUC8) # for USGS flow data

station_awqms <- df.stations.state %>% 
  dplyr::filter(HUC8_Name %in%  subbasin) %>% 
  # Check and compare the HUC10_Names to QAPP Model Waterbody names, which are also sub-section names
  # write.csv(station_awqms,"station_awqms.csv")
  # UPDATE REQUIRED:
  dplyr::filter(!HUC10_Name %in% c("Big Butte Creek", "Trail Creek")) %>% 
  dplyr::mutate(section_name = ifelse(HUC10_Name %in%  c("Headwaters Rogue River", 
                                                         "Lost Creek-Rogue River",
                                                         "Shady Cove-Rogue River",
                                                         "South Fork Rogue River"),
                                      "Rogue River",
                                      HUC10_Name)) %>% 
  dplyr::mutate(section_name = ifelse(section_name == "Little Butte Creek",
                                      "Little Butte Creek and North Fork Little Butte Creek",
                                      section_name)) %>% 
  dplyr::mutate(section_name = ifelse(HUC12_Name %in%  c("Upper Antelope Creek",
                                                         "Lower Antelope Creek"),
                                      "Antelope Creek",
                                      section_name)) %>% 
  dplyr::mutate(section_name = ifelse(HUC12_Name %in%  c("Middle South Fork Little Butte Creek",
                                                         "Lower South Fork Little Butte Creek"),
                                      "South Fork Little Butte Creek",
                                      section_name)) %>% 
  dplyr::rename(`Station ID` = MLocID,
                `Station Description` = StationDes,
                Organization = OrgID,
                Latitude = Lat_DD,
                Longitude = Long_DD)

# CHECK!! unique(sort(station_awqms$section_name))

# for NCDC meteorological data
## qapp_project_area_huc8
## bbox: xmin: -122.9461 ymin: 42.21762 xmax: -122.1365 ymax: 43.13418
huc8.extent <- c(42.21762,-122.9461,43.13418,-122.1365)

# _ (13) "Walla Walla Subbasin" ----
# _ (14) "Willamette River Mainstem and Major Tributaries" ----
# _ (15) "Willow Creek Subbasin" ----


# Build Data ----
# _ AWQMS and Model Setup Info ----
station_model <- cal.input %>% 
  dplyr::filter(`QAPP Project Area` %in%  qapp_project_area) %>% 
  dplyr::left_join(station_awqms[,c("Station ID", "Station Description", "Organization")], by="Station ID") %>% 
  dplyr::mutate(Latitude = as.numeric(Latitude))

lookup_section_name <- station_awqms %>% 
  dplyr::select(HUC10, section_name) %>% 
  dplyr::group_by(HUC10, section_name) %>% 
  dplyr::summarise(n = n())

data <- df.awqms.raw %>% 
  dplyr::filter(HUC8_Name %in%  subbasin) %>% 
  # QA/QC check:
  dplyr::filter(Result_status %in% c("Final", "Provisional") | 
                  QualifierAbbr %in% c("DQL=A","DQL=B","DQL=E")) %>% 
  dplyr::left_join(lookup_section_name, by="HUC10")

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
  dplyr::left_join(station_awqms[,c("Station ID", "Station Description", "section_name")], by="Station ID") %>% 
  dplyr::rename(Year = year,
                Organization = Org_Names)

model.info <- cal.model %>% 
  dplyr::filter(`QAPP Project Area` %in%  qapp_project_area)

model.input  <- cal.input %>% 
  dplyr::filter(`QAPP Project Area` %in%  qapp_project_area) %>% 
  dplyr::mutate(Latitude = round(as.numeric(Latitude),4),
                Longitude = round(Longitude,3))

# _ USGS Flow data ----
usgs.stations <- dataRetrieval::whatNWISdata(stateCd="OR",
                                             parameterCd = "00060") %>%  # 00060	= Discharge [ft3/s]
  dplyr::filter(!(site_tp_cd %in% c("SP","GW"))) %>% # ST = Stream
  dplyr::filter(data_type_cd %in% c("dv", "id", "iv")) %>% # dv=daily values; id=historical instantaneous values; iv=instantaneous values
  dplyr::filter(huc_cd %in% subbasin_num) %>%
  dplyr::select(agency_cd, site_no, station_nm, site_tp_cd, dec_lat_va, dec_long_va,begin_date, end_date) %>% 
  dplyr::mutate(long_sf=dec_long_va,
                lat_sf=dec_lat_va) %>%
  dplyr::distinct() %>%
  sf::st_as_sf(coords = c("long_sf",  "lat_sf"), crs = sf::st_crs("+init=EPSG:4269"))

# sf::st_write(usgs.stations, "N_Umpqua_usgs_flow_sites.shp")

web.huc8 <- sf::read_sf(dsn = "T:/Temperature_TMDL_Revisions/model_QAPPs/R/data/gis/web_hu08.shp",
                        layer = "web_hu08")

web.huc12 <- sf::read_sf(dsn = "T:/Temperature_TMDL_Revisions/model_QAPPs/R/data/gis/web_poly.shp",
                         layer = "web_poly")

qapp_project_area_huc8 <- web.huc8 %>% 
  dplyr::filter(HUC_8 %in% subbasin_num)

#ggplot2::ggplot() + 
#geom_sf(data = web.huc12, colour = "light gray", fill = NA) +
#geom_sf(data = web.huc8, colour = "blue", fill = NA) +
#geom_sf(data = qapp_project_area_huc8, colour = "red", fill = NA)

qapp_project_area_huc12 <- web.huc12 %>% 
  dplyr::filter(sf::st_contains(qapp_project_area_huc8, ., sparse = FALSE))

#ggplot2::ggplot(qapp_project_area_huc12) + geom_sf()

usgs.stations.subbasin <-  sf::st_join(x = usgs.stations,
                                       y = qapp_project_area_huc12,
                                       join = st_intersects,
                                       left = TRUE)

#ggplot2::ggplot(usgs.stations.subbasin) + geom_sf()

#ggplot() +
#geom_sf(data = qapp_project_area_huc12) +
#geom_sf(data = usgs.stations.subbasin)

sf::st_geometry(usgs.stations.subbasin) <- NULL

usgs.station.tbl <- usgs.stations.subbasin %>% 
  dplyr::mutate(`Station Name and ID` = paste0(station_nm, " (", site_no, ")")) %>% 
  dplyr::mutate(`Latitude/Longitude` = paste0(round(dec_lat_va,4), ", ",round(dec_long_va,3))) %>% 
  dplyr::select(`Station Name and ID`, `Latitude/Longitude`, begin_date, end_date, HUC_10) %>% 
  dplyr::rename(`Begin Date` = begin_date,
                `End Date` = end_date,
                HUC10 = HUC_10) %>% 
  dplyr::left_join(lookup_section_name, by="HUC10") %>% 
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
  dplyr::mutate(`Station Name and ID` = stringr::str_to_title(`Station Name and ID`))

# _ NCDC meteorological data ----
# NOAA National Climatic Data Center: https://www.ncdc.noaa.gov/

options(noaakey = "aQnyFVAjwXAXPGTLMWeGkJLVllZPJHuk")
ncdc.stations.extent <- rnoaa::ncdc_stations(extent = huc8.extent,limit = 1000)

ncdc.stations <- ncdc.stations.extent$data %>% 
  dplyr::mutate(lat = latitude,
                long = longitude) %>% 
  sf::st_as_sf(coords = c("longitude", "latitude"), crs = sf::st_crs("+init=EPSG:4269"))

#ggplot() +
#geom_sf(data = qapp_project_area_huc8) +
#geom_sf(data = ncdc.stations)

ncdc.stations.huc8 <- ncdc.stations %>% 
  filter(sf::st_contains(qapp_project_area_huc8, ., sparse = FALSE))

#ggplot() +
#geom_sf(data = qapp_project_area_huc8) +
#geom_sf(data = ncdc.stations.subbasin)

ncdc.stations.subbasin <-  sf::st_join(x = ncdc.stations.huc8,
                                       y = qapp_project_area_huc12,
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

# Date categories: Data Categories represent groupings of data types.
ncdc.station.datacats <- NULL

for(id in 1:length(ncdc.stations.subbasin$id)){
  get <- ncdc_datacats(stationid = ncdc.stations.subbasin$id[id],
                       limit = 1000)
  if(NROW(get$data)>0){
    get$data$station.id <- ncdc.stations.subbasin$id[id]
  }
  ncdc.station.datacats <- rbind(ncdc.station.datacats,get$data)
}

ncdc.datacats <- ncdc.station.datacats %>% 
  dplyr::filter(name %in% c("Air Temperature", "Precipitation", "Weather Type", "Wind", "wind speed", "Cloudiness", "relative humidity")) %>% 
  dplyr::group_by(station.id) %>% 
  dplyr::summarise(Parameter = toString(name))

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
  dplyr::mutate_at("name", str_replace_all, "Ssw", "SSW")

# _ NIFC RAWS meteorological data ----
# Remote Automatic Weather Stations: https://raws.nifc.gov/
# setup
# Note that vignettes require knitr and rmarkdown
#install.packages('knitr')
#install.packages('rmarkdown')
#install.packages('MazamaSpatialUtils')
#install.packages('MazamaLocationUtils')
#devtools::install_github('MazamaScience/RAWSmet')
#library(MazamaSpatialUtils)
#dir.create('~/Data/Spatial', recursive = TRUE)
#setSpatialDataDir('~/Data/Spatial')
#installSpatialData()
#library(RAWSmet)
#dir.create('~/Data/RAWS', recursive = TRUE)
#setRawsDataDir('~/Data/RAWS')
# Cannot install the package "RAWSmet". Posted an issue at https://github.com/MazamaScience/RAWSmet/issues

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
  filter(sf::st_contains(qapp_project_area_huc8, ., sparse = FALSE))

#ggplot() +
#geom_sf(data = web.huc8)+
#geom_sf(data = agrimet.stations)

#ggplot() +
#geom_sf(data = qapp_project_area_huc8) +
#geom_sf(data = agrimet.stations.huc8)


agrimet.stations.subbasin <-  sf::st_join(x = agrimet.stations.huc8,
                                       y = qapp_project_area_huc12,
                                       join = st_intersects,
                                       left = TRUE)

sf::st_geometry(agrimet.stations.subbasin) <- NULL

agrimet.station.tbl <- agrimet.stations.subbasin

# _ MesoWest climate data ----
#devtools::install_github('fickse/mesowest')
#mesowest::requestToken(apikey = "KyGeNUAVnZg7VgSnUe9zVv15e1yg2hxTUnZ4SdZw0y")
mw.meta <- mesowest::mw(service = "metadata", state = "OR")
mw.variables.list  <- mwvariables()
mw.variables <- data.frame(matrix(unlist(mw.variables.list$VARIABLES)))
#library(purrr)
#mw.variables <- purrr::map_df(mw.variables.list$VARIABLES, ~as.data.frame(.x), .id="id")
#write.csv(mw.variables,"mw_variables.csv")
mw.stations <- mw.meta$STATION %>%
  dplyr::mutate(lat = LATITUDE, long = LONGITUDE) %>% 
  sf::st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = sf::st_crs("+init=EPSG:4269"))

mw.stations.huc8 <- mw.stations %>% 
  filter(sf::st_contains(qapp_project_area_huc8, ., sparse = FALSE))

mw.stations.subbasin <-  sf::st_join(x = mw.stations.huc8,
                                     y = qapp_project_area_huc12,
                                     join = st_intersects,
                                     left = TRUE)

#ggplot() +
#geom_sf(data = qapp_project_area_huc8) +
#geom_sf(data = mw.stations.subbasin)


sf::st_geometry(mw.stations.subbasin) <- NULL

mw.station.tbl <- mw.stations.subbasin


# SAVE DATA ----
rm(cal.input)
rm(cal.model)
rm(df.awqms.raw)
rm(df.stations.state)
rm(usgs.stations)
rm(web.huc8)
rm(web.huc12)
rm(qapp_project_area_huc8)
rm(qapp_project_area_huc12)
rm(ncdc.stations.extent)
rm(ncdc.stations)
rm(ncdc.stations.huc8)
rm(ncdc.stations.subbasin)
rm(ncdc.station.datacats)
rm(ncdc.datacats)
save.image(file = "data.RData")
