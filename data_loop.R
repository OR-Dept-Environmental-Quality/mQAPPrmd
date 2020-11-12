library(tidyverse)
library(readxl)
library(dataRetrieval)
library(rgdal)
library(sf)
library(rnoaa) # NCDC
library(mesowest) # Mesowest

strip_alpha <- function(x) {
  
  x2<- gsub(pattern="[a-z]$", replacement="", x=x, ignore.case = TRUE)
  
  return(x2)
}

strip_tbl_num <- function(x) {
  
  m <- tbls(name = x, display="cite")
  n <- as.numeric(gsub(pattern="Table ", replacement="", x=m, ignore.case = TRUE))
  
  return(n)
}

load("//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/data/R/statewide/df_awqms_raw_state.RData") # df.awqms.raw.state
load("//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/data/R/statewide/df_stations_state.RData") #df.stations.state
data.dir <- "//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/model_QAPPs/R/data/"
cal.model <- readxl::read_xlsx(paste0(data.dir, "Model_Setup_Info.xlsx"), sheet = "Calibration Model Setup Info")
cal.input <- readxl::read_xlsx(paste0(data.dir, "Model_Setup_Info.xlsx"), sheet = "Calibration Inputs")
ref <- readxl::read_xlsx(paste0(data.dir, "Model_Setup_Info.xlsx"), sheet = "References")
npdes.ind <- readxl::read_xlsx(paste0(data.dir, "NPDES_communication_list.xlsx"), sheet = "Individual_NDPES")
npdes.gen <- readxl::read_xlsx(paste0(data.dir, "NPDES_communication_list.xlsx"), sheet = "Gen_NPDES")
lookup_huc <- readxl::read_xlsx(paste0(data.dir, "Lookup_QAPPProjectArea_HUC10.xlsx"), sheet = "Lookup_QAPPProjectArea_HUC10")
web.huc8 <- sf::read_sf(dsn = "//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/GIS/Study_Areas_v5_HUC8_scope.shp",
                        layer = "Study_Areas_v5_HUC8_scope")
qapp_project_areas <- read.csv(paste0(data.dir,"qapp_project_area.csv"))


# Generate Project Area Data ----
# for test: qapp_project_area = "North Umpqua Subbasin"

for (qapp_project_area in qapp_project_areas$areas) {
  
  huc8.extent <- qapp_project_areas[which(qapp_project_areas$areas == qapp_project_area),]$huc8.extent
  file.name <- qapp_project_areas[which(qapp_project_areas$areas == qapp_project_area),]$file.name
  
  subbasin <- unique(lookup_huc[which(lookup_huc$QAPP_Project_Area == qapp_project_area),]$HUC8_NAME)
  subbasin_num <- unique(lookup_huc[which(lookup_huc$QAPP_Project_Area == qapp_project_area),]$HUC8)
  
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
  
  usgs.stations.subbasin <-  sf::st_join(x = usgs.stations,
                                         y = qapp_project_area_huc8,
                                         join = st_intersects,
                                         left = TRUE)
  
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
  
  # _ NCDC meteorological data ----
  options(noaakey = "aQnyFVAjwXAXPGTLMWeGkJLVllZPJHuk")
  ncdc.stations.extent <- rnoaa::ncdc_stations(extent = as.numeric(unlist(strsplit(huc8.extent,","))),limit = 1000)
  
  ncdc.stations <- ncdc.stations.extent$data %>% 
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
  
  # _ NIFC RAWS meteorological data ----
  library(RAWSmet)
  setRawsDataDir('//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/model_QAPPs/R/data/RAWS')
  raws.meta <- RAWSmet::wrcc_loadMeta(stateCode = "OR")
  
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
  
  agrimet.stations.huc8 <- agrimet.stations %>% 
    filter(sf::st_contains(qapp_project_area_huc8_union, ., sparse = FALSE))
  
  agrimet.stations.subbasin <-  sf::st_join(x = agrimet.stations.huc8,
                                            y = qapp_project_area_huc8,
                                            join = st_intersects,
                                            left = TRUE)
  
  sf::st_geometry(agrimet.stations.subbasin) <- NULL
  
  agrimet.station.tbl <- agrimet.stations.subbasin
  
  # _ MesoWest climate data ----
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
  
  sf::st_geometry(mw.stations.subbasin) <- NULL
  
  mw.station.tbl <- mw.stations.subbasin
  
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
       file.name,
       qapp_project_area,
       qapp_project_areas,
       subbasin,
       subbasin_num,
       strip_alpha,
       strip_tbl_num,
       file = paste0(file.name,".RData"))
  
}
