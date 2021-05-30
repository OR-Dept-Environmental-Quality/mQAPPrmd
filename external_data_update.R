library(devtools)
library(tidyverse)
library(readxl)
#install.packages("dataRetrieval")
library(dataRetrieval) # USGS FLOW
#install.packages("rnoaa")
library(rnoaa) # NCDC
options(noaakey = "aQnyFVAjwXAXPGTLMWeGkJLVllZPJHuk") # NCDC
#devtools::install_github("MazamaScience/MazamaSpatialUtils@jon") # RAWS
#devtools::install_github('MazamaScience/RAWSmet') # RAWS
library(RAWSmet) # RAWS
#devtools::install_github('fickse/mesowest') # MesoWest
library(mesowest) # MesoWest
mesowest::requestToken(apikey = "KyGeNUAVnZg7VgSnUe9zVv15e1yg2hxTUnZ4SdZw0y") # MesoWest
library(rvest)

setwd("//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/model_QAPPs/R/data/download")
data.dir <- "//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/model_QAPPs/R/data/"

# USGS Flow Data ----
## Github: https://github.com/USGS-R/dataRetrieval
usgs.fl.stations.or <- dataRetrieval::whatNWISdata(stateCd="OR", parameterCd = "00060") # 00060	= Discharge [ft3/s]
usgs.fl.data.or <- dataRetrieval::readNWISdata(stateCd="OR", 
                                            parameterCd = "00060", # and statCd = "00003" for daily mean which is default
                                            startDate = "1990-01-01", # start and end dates match AWQMS data pull
                                            endDate = "2020-12-31")
save(usgs.fl.stations.or, usgs.fl.data.or, file="usgs_fl.RData") # updated date: 5/24/2021

# USGS Water Level Data ----
# for Willamette Mainstem QAPP only
usgs.wl.stations.or <- dataRetrieval::whatNWISdata(stateCd="OR", parameterCd = "00065") # 00065 = Stream stage, the height of the water surface, in feet, above an established altitude where the stage is zero
usgs.wl.data.or <- dataRetrieval::readNWISdata(stateCd="OR", 
                                               parameterCd = "00065",
                                               startDate = "2010-01-01", # as Ryan's suggestion
                                               endDate = "2020-12-31")
save(usgs.wl.stations.or, usgs.wl.data.or, file="usgs_wl.RData") # updated date: 5/24/2021

usgs.wl.stations.wa <- dataRetrieval::whatNWISdata(stateCd="WA", parameterCd = "00065")

# OWRI Temp and Flow Data ----
devtools::source_gist("https://gist.github.com/DEQrmichie/835c7c8b3f373ed80e4b9e34c656951d")

owrd.stations.or <- readxl::read_xlsx(paste0(data.dir, "wrd_nonUSGS_stations.xlsx"), sheet = "Non-USGS_stations")

owrd.stations.nbr <- owrd.stations.or %>% 
  dplyr::distinct(station_nbr) %>% 
  dplyr::pull(station_nbr)

owrd.data.or <- NULL
for(station in owrd.stations.nbr) {
owrd.data.ind <- owrd_data(station = station,
                       startdate = "1/1/1990",
                       enddate = "12/31/2020",
                       char = c("MDF", "WTEMP_MAX")) # MDF - Mean Daily Flow
owrd.data.or <- rbind(owrd.data.or,owrd.data.ind)
}

save(owrd.stations.or, owrd.data.or, file="owrd.RData") # updated date: 3/4/2021

# NCEI Station Meta ----
# https://www.ncdc.noaa.gov/homr/reports
ncei <- read.delim("//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/model_QAPPs/R/data/download/emshr_lite.txt")

ncei.databases <- c("NCDC","COOP","WBAN","ICAO","FAA","NWSLI","WMO   TRANS","GHCND") 

ncei.datacats.or <- NULL

for(db in ncei.databases){
  
  get <- rnoaa::ncdc_datacats(stationid = ncei$db,
                              extent = c(41.5,-125,46.5,-116),
                              limit = 1000)
  
  if(NROW(get$data)>0){
    get$data$station.id <- db
  }
  
  ncei.datacats.or <- rbind(ncei.datacats.or,get$data)
}

save(ncei, ncei.datacats.or, file="ncei.RData") # updated date: 2/27/2021

# _# NCDC Met Data ----
## NOAA National Climatic Data Center: https://www.ncdc.noaa.gov/
## Package: https://cran.r-project.org/web/packages/rnoaa/rnoaa.pdf
## Github: https://github.com/ropensci/rnoaa
# Extents of Study_Areas_v5_HUC8_scope: bbox: xmin: -124.4334 ymin: 41.99523 xmax: -116.5826 ymax: 46.00114
#ncdc.stations.a <- rnoaa::ncdc_stations(extent = c(41.5,-125,46.5,-123),limit = 1000)
#ncdc.stations.b <- rnoaa::ncdc_stations(extent = c(41.5,-123,46.5,-122),limit = 1000)
#ncdc.stations.c <- rnoaa::ncdc_stations(extent = c(41.5,-122,46.5,-119),limit = 1000)
#ncdc.stations.d <- rnoaa::ncdc_stations(extent = c(41.5,-119,46.5,-116),limit = 1000)
#ncdc.station.or <- rnoaa::ncdc_combine(ncdc.stations.a, ncdc.stations.b, ncdc.stations.c, ncdc.stations.d)

## Querying for NCDC datacats takes a long time.
#ncdc.datacats.or <- NULL

#for(id in 1:length(ncdc.station.or$id)){
#  get <- rnoaa::ncdc_datacats(stationid = ncdc.station.or$id[id],
#                              limit = 1000)
#  if(NROW(get$data)>0){
#    get$data$station.id <- ncdc.station.or$id[id]
#  }
#  ncdc.datacats.or <- rbind(ncdc.datacats.or,get$data)
#}

# RAWS Met Data----
## Remote Automatic Weather Stations: https://raws.nifc.gov/
## Github installation: https://github.com/MazamaScience/RAWSmet
#dir.create('//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/model_QAPPs/R/data/download', recursive = TRUE)
RAWSmet::setRawsDataDir('//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/model_QAPPs/R/data/download')
raws.meta <- RAWSmet::wrcc_loadMeta(stateCode = "OR") # downloaded data name: wrcc_meta_OR.rda

## Querying for RAWS data types takes a long time. Only data from day 1 of the current month till today will be downloaded.
## For example, run it on 2020-11-12, only the data from 2020-11-01 to 2020-11-12 were downloaded.
raws.data.type <- NULL

for(i in 1:length(raws.meta$wrccID)){
  if (class(try(RAWSmet::wrcc_downloadData(wrccID = raws.meta$wrccID[i]), silent = TRUE)) == "try-error") {
    raws.data.type <- raws.data.type
  } else {
    get <- RAWSmet::wrcc_downloadData(wrccID = raws.meta$wrccID[i]) # only 
    type.list <- RAWSmet::wrcc_identifyMonitorType(get)
    type.df <- data.frame(unlist(type.list$columnNames)) %>% 
      dplyr::mutate(wrccID = raws.meta$wrccID[i])
    raws.data.type <- rbind(raws.data.type,type.df)
  }
}

save(raws.meta, raws.data.type, file="raws.RData") # updated date: 2/27/2021

# AgriMet Data ----
## Bureau of Reclamation Columbia-Pacific Northwest Region: https://www.usbr.gov/pn/agrimet/
## Note: data spreadsheets were manually organized.
## agrimet_stations.csv: https://www.usbr.gov/pn/agrimet/location.html
## agrimet_parameters.csv: https://www.usbr.gov/pn/agrimet/aginfo/station_params.html#crvo
## both agrimet.stations and agrimet.parameters moved to data.R
## agrimet.stations <- read.csv("//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/model_QAPPs/R/data/download/agrimet_stations.csv")
## agrimet.parameters <- read.csv("//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/model_QAPPs/R/data/download/agrimet_parameters.csv")

# Hydromet Data ----
## Bureau of Reclamation Columbia-Pacific Northwest Region:
## Note: station and data spreadsheets were manually organized and uploaded in data.R
## Stations: https://www.usbr.gov/pn/hydromet/decod_params.html (hydromet.csv)
hydromet <- read.csv("//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/model_QAPPs/R/data/download/hydromet.csv")
## Data: https://www.usbr.gov/pn/hydromet/arcread.html
## Parameter Codes: https://www.usbr.gov/pn/hydromet/data/hydromet_pcodes.html
### QU: Unregulated Flow, Estimated Daily Average (Cubic Feet per Second)
### QD: Discharge, Daily Average (Cubic Feet per Second)
url <- "https://www.usbr.gov/pn-bin/daily.pl?"
hydromet.data <- NULL
for(stationID in unique(sort(hydromet$Station.ID))){
  # test: stationID <- "WARO"
  print(stationID)
  request <- rvest::read_html(paste0(url,
                                     "station=",stationID,"&",
                                     "format=html&",
                                     "year=1990&month=1&day=1&",
                                     "year=2020&month=12&day=31&",
                                     "pcode=qd"))
  table <- request %>% 
    rvest::html_nodes("table") %>% 
    rvest::html_table((fill = TRUE))
  dateTime <- unlist(table[[1]][1])
  Result <- unlist(table[[1]][2]) # QD
  df <- data.frame(dateTime,Result) %>% 
    dplyr::mutate(`Data Source` = "Hydromet",
                  `Station ID` = stationID)
  hydromet.data <- rbind(hydromet.data,df)
}
save(hydromet,hydromet.data, file="hydromet.RData")# updated date: 5/8/2021

# MesoWest Met Data ----
## Github: https://github.com/fickse/mesowest
mw.meta <- mesowest::mw(service = "metadata", state = "OR")
mw.variables.list  <- mesowest::mwvariables()
mw.variables <- data.frame(matrix(unlist(mw.variables.list$VARIABLES)))
mw.variables.clean <- mw.variables %>% 
  dplyr::rename(Parameter = "matrix.unlist.mw.variables.list.VARIABLES..") %>% 
  dplyr::slice(which(row_number() %% 3 == 1)) %>% 
  dplyr::group_by(Parameter) %>% 
  dplyr::summarise(n=n()) %>% 
  dplyr::ungroup()
## Edit mw.variables in Excel.
# write.csv(mw.variables.clean, "mw_variables.csv") # 1. check setwd(); 2. use lookup table
save(mw.meta, mw.variables.list, file="mw.RData") # updated date: 2/27/2021

# DOE, WA ----
# Department of Ecology
doe.stations <- read.csv("//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/model_QAPPs/R/data/doe_temp_stations.csv") %>% 
  sf::st_as_sf(coords = c("Calculated_Longitude_Decimal_Degrees_NAD83HARN","Calculated_Latitude_Decimal_Degrees_NAD83HARN"),
               crs = sf::st_crs("+init=EPSG:4269"))

will_pro_area <- sf::read_sf(dsn = "//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/GIS/willa_snake/TempTMDL_QAPP_Reaches_data_query_HUC12s.shp",
                             layer = "TempTMDL_QAPP_Reaches_data_query_HUC12s") %>% 
  dplyr::filter(Project_Na == "Willamette River Mainstem and Major Tributaries") %>% 
  dplyr::group_by(Project_Na) %>%
  dplyr::summarise(geometry = sf::st_union(geometry)) %>%
  ungroup() %>% 
  sf::st_transform(crs = sf::st_crs("+init=EPSG:4269")) #4326

doe_stations_pro_area <- doe.stations %>% 
  filter(sf::st_contains(will_pro_area, ., sparse = FALSE)) #%>% 
  #sf::st_drop_geometry()

sf::st_write(doe_stations_pro_area, "doe_stations_pro_area.shp")

url.doe <- "https://apps.ecology.wa.gov/eim/search/Eim/EIMSearchResults.aspx?"
data.doe <- NULL
for(stationID in unique(sort(doe_stations_pro_area$Location_ID))){
  # test: stationID <- "25G060"
  print(stationID)
  request <- rvest::read_html(paste0(url.doe,
                                    "ResultType=EIMTabs&",
                                    "LocationUserIds=25G060&",
                                    "LocationUserIdSearchType=Contains&",
                                    "LocationUserIDAliasSearchFlag=True&",
                                    "ResultParameterGroupIds=848&",
                                    "ResultParameterGroupNames=Temperature"))
  response <- httr::content(request, as = "text", encoding = "UTF-8")
  data.doe.station <- geojsonsf::geojson_sf(response)
  data.doe <- rbind(data.doe,data.doe.station)
}

###
url.doe <- "https://apps.ecology.wa.gov/eim/search/SMP/RiverStreamSearch.aspx?&"
data.doe <- NULL
for(stationID in unique(sort(doe_stations_pro_area$Location_ID))){
  # test: stationID <- "25G060"
  print(stationID)
  request <- rvest::read_html(paste0(url.doe,
                                     "StudyMonitoringProgramUserId=RiverStream&",
                                     "StudyMonitoringProgramUserIdSearchType=Equals&",
                                     "ResultParameterName=Temperature%2c+water&",
                                     "ResultParameterNameSearchType=Equals&",
                                     "ResultParameterNameAliasSearchFlag=True"))
  
  response <- httr::content(request, as = "text", encoding = "UTF-8")
  data.doe.station <- geojsonsf::geojson_sf(response)
  data.doe <- rbind(data.doe,data.doe.station)
}

###
https://apps.ecology.wa.gov/eim/search/Eim/EIMSearchResults.aspx?ResultType=EIMTabs&LocationUserIds=25G060&LocationUserIdSearchType=Contains&LocationUserIDAliasSearchFlag=True&ResultParameterGroupIds=848&ResultParameterGroupNames=Temperature
https://apps.ecology.wa.gov/eim/search/SMP/RiverStreamSearch.aspx?&StudyMonitoringProgramUserId=RiverStream&StudyMonitoringProgramUserIdSearchType=Equals&ResultParameterName=Temperature%2c+water&ResultParameterNameSearchType=Equals&ResultParameterNameAliasSearchFlag=True
https://apps.ecology.wa.gov/eim/search/Eim/EIMSearchResults.aspx?ResultType=EIMTabs&LocationUserIds=25G060&LocationUserIdSearchType=Contains&LocationUserIDAliasSearchFlag=True&FieldActivityDateRangeBeginning=1%2f1%2f2010+12%3a00%3a00+AM&FieldActivityDateRangeEnding=12%2f31%2f2020+12%3a00%3a00+AM
https://apps.ecology.wa.gov/eim/search/Download/Download.aspx?DownloadType=RiverStream&LocationUserIds=25G060&LocationUserIdSearchType=Contains&LocationUserIDAliasSearchFlag=True&FieldActivityDateRangeBeginning=1%2f1%2f2010+12%3a00%3a00+AM&FieldActivityDateRangeEnding=12%2f31%2f2020+12%3a00%3a00+AM


###
url <- "https://www.usbr.gov/pn-bin/daily.pl?"
hydromet.data <- NULL
for(stationID in unique(sort(hydromet$Station.ID))){
  # test: stationID <- "WARO"
  print(stationID)
  request <- rvest::read_html(paste0(url,
                                     "station=",stationID,"&",
                                     "format=html&",
                                     "year=1990&month=1&day=1&",
                                     "year=2020&month=12&day=31&",
                                     "pcode=qd"))
  table <- request %>% 
    rvest::html_nodes("table") %>% 
    rvest::html_table((fill = TRUE))
  dateTime <- unlist(table[[1]][1])
  Result <- unlist(table[[1]][2]) # QD
  df <- data.frame(dateTime,Result) %>% 
    dplyr::mutate(`Data Source` = "Hydromet",
                  `Station ID` = stationID)
  hydromet.data <- rbind(hydromet.data,df)
}
save(hydromet,hydromet.data, file="hydromet.RData")# updated date: 5/8/2021