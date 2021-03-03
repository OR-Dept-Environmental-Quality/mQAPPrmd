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

setwd("//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/model_QAPPs/R/data/download")

# USGS Flow Data ----
## Github: https://github.com/USGS-R/dataRetrieval
usgs.stations.or <- dataRetrieval::whatNWISdata(stateCd="OR", parameterCd = "00060") # 00060	= Discharge [ft3/s]
usgs.data.or <- dataRetrieval::readNWISdata(stateCd="OR", 
                                            parameterCd = "00060", # and statCd = "00003" for daily mean which is default
                                            startDate = "1990-01-01", # start and end dates match AWQMS data pull
                                            endDate = "2020-12-31")
save(usgs.stations.or, usgs.data.or, file="usgs.RData") # updated date: 2/27/2021

# OWRI Temp and Flow Data ----
devtools::source_gist("https://gist.github.com/DEQrmichie/835c7c8b3f373ed80e4b9e34c656951d")

owrd.stations.or <- readxl::read_xlsx(paste0(data.dir, "wrd_nonUSGS_stations.xlsx"), sheet = "Non-USGS_stations")

owrd.stations.nbr <- owrd.stations.or %>% 
  dplyr::distinct(station_nbr) %>% 
  dplyr::pull(station_nbr)

owrd.data <- NULL
for(station in owrd.stations.nbr) {
owrd.data.ind <- owrd_data(station = station,
                       startdate = "1/1/1990",
                       enddate = "12/31/2020",
                       char = c("MDF", "WTEMP_MAX")) # MDF - Mean Daily Flow
owrd.data <- rbind(owrd.data,owrd.data.ind)
}

owrd.data.or <- owrd.data %>% 
  dplyr::filter(published_status %in% c("Published"))

save(owrd.stations.or, owrd.data.or, file="owrd.RData") # updated date: 2/27/2021

# NCEI Staion Meta ----
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
## Note: data spreadsheets were manually organized.
## hydromet.csv: https://www.usbr.gov/pn/hydromet/decod_params.html
## hydromet upload moved to data.R
## hydromet <- read.csv("//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/model_QAPPs/R/data/download/hydromet.csv")

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
