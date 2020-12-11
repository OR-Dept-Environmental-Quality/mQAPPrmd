library(tidyverse)
library(readxl)
library(rgdal)
library(sf)
library(ggplot2)
library(odeqtmdl)
library(tigris)
options(tigris_use_cache = TRUE)


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
load("//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/data/R/statewide/df_stations_state.RData") # df.stations.state
data.dir <- "//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/model_QAPPs/R/data/"
load(paste0(data.dir,"/download/data_sources.RData"))
agrimet.stations <- read.csv(paste0(data.dir, "download/agrimet_stations.csv"))
agrimet.parameters <- read.csv(paste0(data.dir, "download/agrimet_parameters.csv"))
hydromet <- read.csv(paste0(data.dir, "download/hydromet.csv"))

cal.model <- readxl::read_xlsx(paste0(data.dir, "Model_Setup_Info.xlsx"), sheet = "Calibration Model Setup Info") %>% 
  dplyr::filter(!`QAPP Project Area` == "Upper Klamath and Lost Subbasins") %>% 
  # for Section 6.1 General Model Parameters
  dplyr::mutate(Model_version = ifelse(substr(`Model version`, 13,13) == "6", "Heat Source Version 6",
                                        ifelse(substr(`Model version`, 13,13) == "7", "Heat Source Version 7",
                                                ifelse(substr(`Model version`, 13,13) == "8", "Heat Source Version 8",
                                                        ifelse(substr(`Model version`, 13,13) == "9", "Heat Source Version 9",
                                                                ifelse(substr(`Model version`, 1,2) == "CE", "CE-Qual-W2 Version 3", NA)))))) %>% 
  dplyr::mutate(Model_version = ifelse(is.na(`Model version`) & `Primary Model Parameter` == "Solar", "SHADOW", Model_version)) %>% 
  dplyr::mutate(mod_rmd = ifelse(Model_version == "Heat Source Version 6", "hs6",
                                  ifelse(Model_version == "Heat Source Version 7", "hs7",
                                          ifelse(Model_version == "Heat Source Version 8", "hs8",
                                                  ifelse(Model_version == "Heat Source Version 9", "hs9",
                                                          ifelse(Model_version == "CE-Qual-W2 Version 3", "ce",
                                                                  "sh")))))) %>% 
  dplyr::mutate(mod_score = ifelse(mod_rmd == "hs6", "1",
                                 ifelse(mod_rmd == "hs7", "10",
                                        ifelse(mod_rmd == "hs8", "20", "0")))) %>% 
  dplyr::mutate(mod_ref = ifelse(mod_rmd == "ce", 'Cole, T.M., and S. A. Wells. 2000. "CE-QUAL-W2: A Two-Dimensional, Laterally Averaged, Hydrodynamic and Water Quality Model, Version 3.0." Instruction Report EL-2000. US Army Engineering and Research Development Center, Vicksburg, MS.',
                ifelse(mod_rmd == "sh", 'USFS (U.S. Forest Service). 1993. “SHADOW v. 2.3 - Stream Temperature Management Program. Prepared by Chris Park USFS, Pacific Northwest Region.”',
                       NA)))

cal.input <- readxl::read_xlsx(paste0(data.dir, "Model_Setup_Info.xlsx"), sheet = "Calibration Inputs") %>% 
  dplyr::filter(!`QAPP Project Area` == "Upper Klamath and Lost Subbasins")
ref <- readxl::read_xlsx(paste0(data.dir, "Model_Setup_Info.xlsx"), sheet = "References")
# tmdl.temp <- odeqtmdl::tmdl_db %>% dplyr::filter(pollutant_name_TMDL == "Temperature")
npdes.ind <- readxl::read_xlsx(paste0(data.dir, "NPDES_communication_list.xlsx"), sheet = "Individual_NDPES")
npdes.gen <- readxl::read_xlsx(paste0(data.dir, "NPDES_communication_list.xlsx"), sheet = "Gen_NPDES")
lookup_huc <- readxl::read_xlsx(paste0(data.dir, "Lookup_QAPPProjectArea_HUC10.xlsx"), sheet = "Lookup_QAPPProjectArea_HUC10")
qapp_project_areas <- read.csv(paste0(data.dir,"qapp_project_area.csv"))
web.huc8 <- sf::read_sf(dsn = "//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/GIS/Study_Areas_v5_HUC8_scope.shp",
                        layer = "Study_Areas_v5_HUC8_scope")

# _ IR2018/20 Cat 4 & 5 ----
cat.45.rivers <- sf::read_sf(dsn = "//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/GIS/2018_2020_IR_Cat4_5_Temp_Rivers_FINAL.shp",
                             layer = "2018_2020_IR_Cat4_5_Temp_Rivers_FINAL") %>% 
  dplyr::left_join(lookup_huc, by = "HUC12")

cat.45.rivers <- sf::st_transform(cat.45.rivers, 4326)

cat.45.rivers.tbl <- cat.45.rivers

sf::st_geometry(cat.45.rivers.tbl) <- NULL

# _ NCDC met data ----
ncdc.stations <- ncdc.station.or %>% 
  dplyr::mutate(lat = latitude,
                long = longitude) %>% 
  sf::st_as_sf(coords = c("longitude", "latitude"), crs = sf::st_crs("+init=EPSG:4269"))

# _ RAWS met data ----
raws.stations <- raws.meta %>% 
  dplyr::mutate(lat = latitude,
                long = longitude) %>%
  sf::st_as_sf(coords = c("longitude", "latitude"), crs = sf::st_crs("+init=EPSG:4269"))

# _ USBR AgriMet ----
agrimet.stations.or <- agrimet.stations %>%
  dplyr::filter(state == "OR") %>% 
  # dplyr::left_join(agrimet.parameters, by = "siteid") %>% 
  dplyr::mutate(lat = latitude, long = longitude) %>% 
  sf::st_as_sf(coords = c("longitude", "latitude"), crs = sf::st_crs("+init=EPSG:4269"))

# _ MesoWest climate data ----
mw.stations <- mw.meta$STATION %>%
  dplyr::mutate(lat = LATITUDE, long = LONGITUDE) %>% 
  sf::st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = sf::st_crs("+init=EPSG:4269"))

# QAPP Project Area Data ----

## for test:
# qapp_project_area = "Applegate, Illinois, Lower Rogue, and Middle Rogue Subbasins"
# qapp_project_area = "John Day River Basin"
# qapp_project_area = "Lower Grande Ronde, Imnaha, and Wallowa Subbasins"
# qapp_project_area = "Lower Willamette and Clackamas Subbasins"
# qapp_project_area = "Malheur River Subbasins"
# qapp_project_area = "Mid Willamette Subbasins"
# qapp_project_area = "Middle Columbia-Hood, Miles Creeks"
# qapp_project_area = "North Umpqua Subbasin"
# qapp_project_area = "Sandy River Basin"
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
    dplyr::rename(Year = year) %>% 
    dplyr::mutate(`StationDes` = stringr::str_to_title(`StationDes`)) %>% 
    dplyr::mutate_at("StationDes", str_replace_all, "Or", "OR")
  
  model.info <- cal.model %>% 
    dplyr::filter(`QAPP Project Area` %in%  qapp_project_area)

  model.input  <- cal.input %>% 
    dplyr::filter(`QAPP Project Area` %in%  qapp_project_area) %>% 
    dplyr::mutate(Latitude = round(as.numeric(Latitude),4),
                  Longitude = round(Longitude,3))
  
  qapp_project_area_huc8 <- web.huc8 %>% 
    dplyr::filter(HUC_8 %in% subbasin_num)
  
  qapp_project_area_huc8_union <- sf::st_union(qapp_project_area_huc8)
  
  # _ IR2018/20 Cat 4 & 5 ----
  pro.cat.45.rivers.tbl <- cat.45.rivers.tbl %>% 
    dplyr::filter(QAPP_Project_Area %in%  qapp_project_area)

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
                                         join = st_within,
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
  ncdc.stations.huc8 <- ncdc.stations %>% 
    filter(sf::st_contains(qapp_project_area_huc8_union, ., sparse = FALSE))
  
  ncdc.stations.subbasin <-  sf::st_join(x = ncdc.stations.huc8,
                                         y = qapp_project_area_huc8,
                                         join = st_within,
                                         left = TRUE)
  
  sf::st_geometry(ncdc.stations.subbasin) <- NULL
  
  #ggplot() +
  #  geom_sf(data = web.huc8) +
  #  geom_sf(data = qapp_project_area_huc8,
  #          colour = "blue") +
  #  geom_sf(data = ncdc.stations) +
  #  geom_sf_label(data = ncdc.stations,
  #                aes(lat, long, label = name))
  
  
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
  raws.stations.huc8 <- raws.stations %>% 
    filter(sf::st_contains(qapp_project_area_huc8_union, ., sparse = FALSE))
  
  raws.stations.subbasin <-  sf::st_join(x = raws.stations.huc8,
                                         y = qapp_project_area_huc8,
                                         join = st_within,
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
  agrimet.stations.huc8 <- agrimet.stations.or %>% 
    filter(sf::st_contains(qapp_project_area_huc8_union, ., sparse = FALSE))
  
#  ggplot() +
#    geom_sf(data = web.huc8)+
#    geom_sf(data = qapp_project_area_huc8,
#            colour = "blue") +
#    geom_sf(data = agrimet.stations.or) +
#    geom_sf_label(data = agrimet.stations.or,
#                  aes(lat, long, label = siteid))
  
  agrimet.stations.subbasin <-  sf::st_join(x = agrimet.stations.huc8,
                                            y = qapp_project_area_huc8,
                                            join = st_within,
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
  mw.stations.huc8 <- mw.stations %>% 
    filter(sf::st_contains(qapp_project_area_huc8_union, ., sparse = FALSE))
  
  mw.stations.subbasin <-  sf::st_join(x = mw.stations.huc8,
                                       y = qapp_project_area_huc8,
                                       join = st_within,
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
  
  # _ Save Data ----
  setwd("//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/model_QAPPs/R/data/RData")
  
  save(df.stations.state,
       web.huc8,
       model.info,
       model.input,
       station_awqms,
       station_model,
       data.sample.count,
       npdes.ind,
       npdes.gen,
       pro.cat.45.rivers.tbl,
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

# Leaflet Map Data ----
pro.areas <- sf::read_sf(dsn = "//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/model_QAPPs/R/data/gis/project_extent_3.shp",
                         layer = "project_extent_3")

pro.areas <- sf::st_transform(pro.areas, 4326)

pro.areas <- pro.areas %>% 
  dplyr::mutate(color = dplyr::case_when(Project_Na == "Applegate, Illinois, Lower Rogue, and Middle Rogue Subbasins" ~ "#df65b0", #pink
                                         Project_Na == "John Day River Basin" ~ "#df65b0", #pink
                                         Project_Na == "Lower Grande Ronde, Imnaha, and Wallowa Subbasins" ~ "yellow",
                                         Project_Na == "Lower Willamette and Clackamas Subbasins" ~ "#253494", #blue
                                         Project_Na == "Malheur River Subbasins" ~ "#78c679",
                                         Project_Na == "Mid Willamette Subbasins" ~ "#253494", #blue
                                         Project_Na == "Middle Columbia-Hood, Miles Creeks" ~ "yellow",
                                         Project_Na == "North Umpqua Subbasin" ~ "purple",
                                         Project_Na == "Sandy River Basin" ~ "#253494", #blue
                                         Project_Na == "South Umpqua and Umpqua Subbasins" ~ "purple",
                                         Project_Na == "Southern Willamette Subbasins" ~ "#253494", #blue
                                         Project_Na == "Upper Rogue Subbasin" ~ "#df65b0", #pink
                                         Project_Na == "Walla Walla Subbasin" ~ "#78c679", #green
                                         Project_Na == "Willow Creek Subbasin" ~ "#78c679")) %>%  #green
  dplyr::left_join(qapp_project_areas, by = c("Project_Na" = "areas")) %>% 
  dplyr::mutate(CompleteD = format(as.Date(complete.date,"%m/%d/%Y"),"%b %d, %Y")) %>% 
  dplyr::mutate(map_link = paste0("<a href='http://192.168.0.12:8888/mQAPPrmd/maps/",file.name,".html'>",Project_Na,"</a>"))

pro.reaches <- sf::read_sf(dsn = "//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/model_QAPPs/R/data/gis/project_reach_extent.shp",
                           layer = "project_reach_extent")

pro.reaches <- sf::st_zm(pro.reaches, drop = T, what = "ZM")

pro.reaches <- sf::st_transform(pro.reaches, 4326)

pro.reaches <- pro.reaches %>% 
  dplyr::mutate(color = dplyr::case_when(Project_Na == "Willamette River Mainstem and Major Tributaries"~ "purple",
                                         Project_Na == "Snake River – Hells Canyon"~ "yellow"))

# _ Model Segments ----
temp.model.streams <- sf::read_sf(dsn = "//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/model_QAPPs/R/data/gis/temp_model_streams_temp_projects.shp",
                             layer = "temp_model_streams_temp_projects")

temp.model.streams <- sf::st_transform(temp.model.streams, 4326)

shade.model.streams <- sf::read_sf(dsn = "//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/model_QAPPs/R/data/gis/shade_model_streams_temp_projects.shp",
                                  layer = "shade_model_streams_temp_projects")

shade.model.streams <- sf::st_transform(shade.model.streams, 4326)

shade.model.area <- sf::read_sf(dsn = "//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/model_QAPPs/R/data/gis/shade_model_area_SWillamette_temp_projects.shp",
                                   layer = "shade_model_area_SWillamette_temp_projects")

shade.model.area <- sf::st_transform(shade.model.area, 4326)



# _ HUC 8,10,12 ----
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

# _ Stream Temperature ----
temp.awqms <- df.stations.state %>% 
  dplyr::rename(`Station ID` = MLocID) %>% 
  dplyr::rename(`Station Description` = StationDes,
                Organization = OrgID,
                Latitude = Lat_DD,
                Longitude = Long_DD) %>% 
  dplyr::select(`Station Description`,`Station ID`, Organization, Latitude, Longitude)

temp.model <- cal.input %>%
  dplyr::left_join(temp.awqms[,c("Station ID", "Station Description", "Organization")], by="Station ID") %>% 
  dplyr::filter(`Parameter` %in%  c("Water Temperature")) %>% 
  dplyr::filter(!is.na(`Data Source`) & is.na(`Interpolated Data`)) %>% 
  dplyr::mutate(`Station Description` = ifelse(is.na(`Station Description`),`Model Location Name`,`Station Description`),
                Organization = ifelse(is.na(Organization),`Data Source`,Organization)) %>% 
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

# _ Flow ----
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

# _ Met ----
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

# _ Ind NPDES ----
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

# _ Save Data ----
setwd("//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/model_QAPPs/R/data/RData")

save(lookup_huc,
     qapp_project_areas,
     pro.areas,
     temp.model.streams,
     shade.model.streams,
     shade.model.area,
     map.huc8,
     map.huc10,
     map.huc12,
     map.temp.pro,
     map.flow.pro,
     map.met.pro,
     map.ind.npdes.pro,
     file = "map.RData")
