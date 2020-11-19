update.date <- "2020/11/12"

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

# USGS Flow Data ----
## Github: https://github.com/USGS-R/dataRetrieval
usgs.stations.or <- dataRetrieval::whatNWISdata(stateCd="OR", parameterCd = "00060") # 00060	= Discharge [ft3/s]

# NCDC Met Data ----
## NOAA National Climatic Data Center: https://www.ncdc.noaa.gov/
## Package: https://cran.r-project.org/web/packages/rnoaa/rnoaa.pdf
## Github: https://github.com/ropensci/rnoaa
# Extents of Study_Areas_v5_HUC8_scope: bbox: xmin: -124.4334 ymin: 41.99523 xmax: -116.5826 ymax: 46.00114
ncdc.stations.a <- rnoaa::ncdc_stations(extent = c(41.5,-125,46.5,-123),limit = 1000)
ncdc.stations.b <- rnoaa::ncdc_stations(extent = c(41.5,-123,46.5,-122),limit = 1000)
ncdc.stations.c <- rnoaa::ncdc_stations(extent = c(41.5,-122,46.5,-119),limit = 1000)
ncdc.stations.d <- rnoaa::ncdc_stations(extent = c(41.5,-119,46.5,-116),limit = 1000)
ncdc.station.or <- rnoaa::ncdc_combine(ncdc.stations.a, ncdc.stations.b, ncdc.stations.c, ncdc.stations.d)

## Querying for NCDC datacats takes a long time.
ncdc.datacats.or <- NULL

for(id in 1:length(ncdc.station.or$id)){
  get <- rnoaa::ncdc_datacats(stationid = ncdc.station.or$id[id],
                       limit = 1000)
  if(NROW(get$data)>0){
    get$data$station.id <- ncdc.station.or$id[id]
  }
  ncdc.datacats.or <- rbind(ncdc.datacats.or,get$data)
}

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
  dplyr::summarise(n=n())
## Edit mw.variables in Excel.
write.csv(mw.variables.clean, "mw_variables.csv") # check setwd()

# SAVE DATA ----
setwd("//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/model_QAPPs/R/data/download")

save(update.date,
     usgs.stations.or,
     ncdc.station.or,
     ncdc.datacats.or,
     raws.meta,
     raws.data.type,
     mw.meta,
     mw.variables.list,
     file = "data_sources.RData")

