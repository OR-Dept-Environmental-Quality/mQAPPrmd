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
library(lubridate)

file.dir <- "//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/model_QAPPs/R/data/download/"
### for Willamette Mainstem QAPP: 
willamette_huc12 <- sf::read_sf(dsn = "//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/GIS/willa_snake/TempTMDL_QAPP_Reaches_data_query_HUC12s.shp",
                                layer = "TempTMDL_QAPP_Reaches_data_query_HUC12s") %>% 
  dplyr::filter(Project_Na == "Willamette River Mainstem and Major Tributaries") %>% 
  sf::st_transform(4269) #4326
willamette_huc12_union <- sf::st_union(willamette_huc12)

# USGS Flow Data ----
## Github: https://github.com/USGS-R/dataRetrieval
## Stations:
### OR stations:
usgs.fl.stations.or <- dataRetrieval::whatNWISdata(stateCd="OR", parameterCd = "00060") # 00060	= Discharge [ft3/s]
### WA stations for Willamette Mainstem QAPP: 
usgs.fl.stations.wa <- dataRetrieval::whatNWISdata(stateCd="WA", parameterCd = "00060")
usgs.fl.stations.wa.will <- usgs.fl.stations.wa %>% 
  dplyr::mutate(lat = dec_lat_va,
                long = dec_long_va) %>%
  sf::st_as_sf(coords = c("long", "lat"), crs = sf::st_crs("+init=EPSG:4269")) %>% 
  dplyr::filter(sf::st_intersects(willamette_huc12_union, ., sparse = FALSE)) %>% 
  sf::st_drop_geometry()
usgs.fl.stations <- rbind(usgs.fl.stations.or,usgs.fl.stations.wa.will)
## Data:
usgs.fl.data <- NULL
for (id in unique(sort(usgs.fl.stations$site_no))) {
  
  print(id)
  usgs.fl.data.i <- dataRetrieval::readNWISdata(siteNumber = id,
                                                parameterCd = "00060", # and statCd = "00003" for daily mean which is default
                                                startDate = "1990-01-01", # start and end dates match AWQMS data pull
                                                endDate = "2020-12-31")
  usgs.fl.data <- dplyr::bind_rows(usgs.fl.data,usgs.fl.data.i)
  
}

save(usgs.fl.stations,usgs.fl.data, file=paste0(file.dir,"usgs_fl.RData")) # updated date: 6/9/2021

# USGS Gage Height (Water Level) Data ----
# for Willamette Mainstem QAPP only
## stations:
### OR stations:
usgs.gh.stations.or <- dataRetrieval::whatNWISdata(stateCd="OR", parameterCd = "00065") # 00065 = Stream stage, the height of the water surface, in feet, above an established altitude where the stage is zero
### WA stations:
usgs.gh.stations.wa <- dataRetrieval::whatNWISdata(stateCd="WA", parameterCd = "00065")
usgs.gh.stations.wa.will <- usgs.gh.stations.wa %>% 
  dplyr::mutate(lat = dec_lat_va,
                long = dec_long_va) %>%
  sf::st_as_sf(coords = c("long", "lat"), crs = sf::st_crs("+init=EPSG:4269")) %>% 
  dplyr::filter(sf::st_intersects(willamette_huc12_union, ., sparse = FALSE)) %>% 
  sf::st_drop_geometry()
usgs.gh.stations <- rbind(usgs.gh.stations.or,usgs.gh.stations.wa.will)
## Data:
usgs.gh.data <- NULL
for (id in unique(sort(usgs.gh.stations$site_no))) {
  
  print(id)
  usgs.gh.data.i <- dataRetrieval::readNWISdata(siteNumber = id,
                                                parameterCd = "00065",
                                                startDate = "2010-01-01", # Ryan's suggestion
                                                endDate = "2020-12-31")
  usgs.gh.data <- dplyr::bind_rows(usgs.gh.data,usgs.gh.data.i)
  
}

# Station 14211720 doesn't have daily mean data, instead it has 30 min data. Data were retrieved through the REST service.
url <- paste0("https://nwis.waterdata.usgs.gov/usa/nwis/uv/?",
              "cb_00065=on&",
              "format=html&",
              "site_no=14211720&",
              "period=&",
              "begin_date=2010-01-01&",
              "end_date=2020-12-31")
request <- rvest::read_html(url)
table <- request %>% 
  rvest::html_nodes("table") %>% 
  rvest::html_table((fill = TRUE))
data14211720 <- data.frame(table[3][1]) %>% 
  dplyr::rename(dateTime = `Date...Time`,
                gageHeight = `Gageheight...feet.MorrisonBridge`) %>% 
  dplyr::mutate(gageHeight = substr(gageHeight, 1, nchar(gageHeight)-1),
                date = as.Date(dateTime, "%m/%d/%Y")) %>% 
  dplyr::group_by(date) %>% 
  dplyr::summarise(X_00065_00003 = mean(as.numeric(gageHeight))) %>% 
  dplyr::rename(dateTime = date) %>% 
  dplyr::mutate(agency_cd = "USGS",
                site_no = "14211720")

usgs.gh.data <-  dplyr::bind_rows(usgs.gh.data,data14211720)

save(usgs.gh.stations, usgs.gh.data, file=paste0(file.dir,"usgs_wl.RData")) # updated date: 6/9/2021

# OWRI Temp and Flow Data ----
devtools::source_gist("https://gist.github.com/DEQrmichie/835c7c8b3f373ed80e4b9e34c656951d")

owrd.stations.or <- readxl::read_xlsx(paste0(file.dir,"wrd_nonUSGS_stations.xlsx"), sheet = "Non-USGS_stations")

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
#ncei <- read.delim(paste0(file.dir,"emshr_lite.txt"))
ncei <- readxl::read_xlsx(paste0(file.dir, "emshr_lite.xlsx"), sheet = "1990-2020")

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

save(ncei, ncei.datacats.or, file=paste0(file.dir,"ncei.RData")) # updated date: 2/27/2021

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

save(raws.meta, raws.data.type, file=paste0(file.dir,"raws.RData")) # updated date: 2/27/2021

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
hydromet <- read.csv(paste0(file.dir,"hydromet.csv"))
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
hydromet <- hydromet %>% 
  dplyr::filter(Station.ID %in% hydromet.data$`Station ID`) # only keep the stations with the data from 1990-2020
save(hydromet,hydromet.data, file=paste0(file.dir,"hydromet.RData"))# updated date: 5/8/2021

# MesoWest Met Data ----
## Github: https://github.com/fickse/mesowest
mw.meta.download <- mesowest::mw(service = "metadata", state = "OR")
mw.meta <- mw.meta.download$STATION %>% 
  dplyr::filter(as.numeric(substring(mw.meta.download$STATION$PERIOD_OF_RECORD$start, 1, 4))>=1990 & as.numeric(substring(mw.meta.download$STATION$PERIOD_OF_RECORD$start, 1, 4))<=2020)
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
save(mw.meta, mw.variables.list, file=paste0(file.dir,"mw.RData")) # updated date: 2/27/2021

# DOE Water Temp Data ----
# Department of Ecology, WA
# Data for Willamette Maintem project area only
# Ryan downloaded doe_temp_stations.csv from https://apps.ecology.wa.gov/eim/search/default.aspx
doe.stations <- read.csv(paste0(file.dir,"doe_temp_stations.csv")) %>% 
  dplyr::mutate(Latitude = Calculated_Latitude_Decimal_Degrees_NAD83HARN,
                Longitude = Calculated_Longitude_Decimal_Degrees_NAD83HARN) %>% 
  sf::st_as_sf(coords = c("Calculated_Longitude_Decimal_Degrees_NAD83HARN","Calculated_Latitude_Decimal_Degrees_NAD83HARN"),
               crs = sf::st_crs("+init=EPSG:4269"))

will_pro_area <- sf::read_sf(dsn = "//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/GIS/willa_snake/TempTMDL_QAPP_Reaches_data_query_HUC12s.shp",
                             layer = "TempTMDL_QAPP_Reaches_data_query_HUC12s") %>% 
  dplyr::filter(Project_Na == "Willamette River Mainstem and Major Tributaries") %>% 
  dplyr::group_by(Project_Na) %>%
  dplyr::summarise(geometry = sf::st_union(geometry)) %>%
  ungroup() %>% 
  sf::st_transform(4269) #4326

doe_stations_pro_area <- doe.stations %>% 
  filter(sf::st_contains(will_pro_area, ., sparse = FALSE)) %>% 
  sf::st_drop_geometry()

# write.csv(doe_stations_pro_area, "doe_stations_pro_area.csv")
# sf::st_write(doe_stations_pro_area, "doe_stations_pro_area.shp")

# Note: 
# _ 34 DOE station info is saved as doe_stations_pro_area.xlsx in \\deqhq1\tmdl\Planning statewide\Temperature_TMDL_Revisions\model_QAPPs\R\data\download
# _ Data of these stations were downloaded manually via URL queries (See query links in doe_stations_pro_area.xlsx tab [data_download_link])
# _ Downloaded data were saved in doe_data_pro_area.xlsx in \deqhq1\tmdl\Planning statewide\Temperature_TMDL_Revisions\model_QAPPs\R\data\download

doe.stations.pro.area <- readxl::read_xlsx(paste0(file.dir,"doe_stations_pro_area.xlsx"), sheet = "doe_stations_pro_area")
doe.data.pro.area <- readxl::read_xlsx(paste0(file.dir,"doe_data_pro_area.xlsx"), sheet = "data")

save(doe.stations.pro.area, doe.data.pro.area, file=paste0(file.dir,"doe.RData")) # updated date: 6/2/2021

# City of Portland Water Temp Data ----
## COP: https://aquarius.portlandoregon.gov/
devtools::install_github("TravisPritchardODEQ/odeqIRextdata")
library(odeqIRextdata)
#library (readr)
#urlfile <- "https://raw.githubusercontent.com/TravisPritchardODEQ/odeqIRextdata/master/BES_stations.csv"
#bes.stations <- readr::read_csv(url(urlfile))

# Use the station list provided by Peter Bryant from COP.
bes.stations <- readxl::read_xlsx(paste0(file.dir, "cop_stations.xlsx"), sheet = "2021-08-03 Surface Water Time S") %>% 
  dplyr::filter(Parameter == "Temperature",
                Label == "Primary",
                Unit == "degC") %>% 
  dplyr::select("LocationIdentifier","LocationName","Latitude","Longitude","LocationType")
bes.stations.id <- bes.stations %>% 
  dplyr::pull(LocationIdentifier)

station <- bes.stations.id
startdate <- "1990-01-01"
enddate <- "2020-12-31"
char=c("Temperature.Primary") # 'Temperature.Primary' - Continuous Water temperature (deg C)

bes.data <- odeqIRextdata::copbes_data(station, startdate, enddate, char) %>% 
  dplyr::filter(Grade.Code == 100, # 100=Good
                Approval.Level == 1200) %>%  # 1200=Approved
  dplyr::mutate(date = lubridate::date(as.Date(datetime))) %>% 
  dplyr::group_by(date, Monitoring_Location_ID, Result.Unit) %>% 
  dplyr::summarize(Result_Numeric = max(Result.Value)) %>% 
  dplyr::mutate(Char_Name = "daily_max_water_temp")

# check ----
station <- bes.stations.id
startdate <- "2015-01-01"
enddate <- "2016-12-31"
char=c("Temperature.Primary") # 'Temperature.Primary' - Continuous Water temperature (deg C)

bes.data.1516 <- odeqIRextdata::copbes_data(station, startdate, enddate, char) %>% 
  dplyr::filter(Grade.Code == 100, # 100=Good
                Approval.Level == 1200) %>%  # 1200=Approved
  dplyr::mutate(date = lubridate::date(as.Date(datetime))) %>% 
  dplyr::group_by(date, Monitoring_Location_ID, Result.Unit) %>% 
  dplyr::summarize(Result_Numeric = max(Result.Value)) %>% 
  dplyr::mutate(Char_Name = "daily_max_water_temp")

bes.data.check <- bes.data %>% 
  dplyr::group_by(Monitoring_Location_ID) %>% 
  dplyr::summarise(date_range = paste(range(date)[1]," - ",range(date)[2])) %>% 
  dplyr::ungroup()

write.csv(bes.data.check, paste0(file.dir,"bes_data_check.csv"))
# end check ----

save(bes.stations, bes.data, file=paste0(file.dir,"bes.RData")) # download date: 8/11/2021
