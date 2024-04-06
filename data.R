# ____ master data for all project areas ____

library(tidyverse)
library(readxl)
# library(rgdal)
library(sf)
library(lubridate)

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

s <- function(x) {
  # function to return s if a noun is plural given the count of x
  
  if (x == 1) {
    return("")
  } else {
    return("s") }
  
}

is.are <- function(x) {
  # function to return present tense singular (is) or present tense plural (are) given the count of x
  
  if (x == 1) {
    return("is")
  } else {
    return("are") }
  
}

numbers.to.words <- function(x) {
  # function to convert numbers < 10 to words
  # x = numeric number
  
  words <- c("zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine")
  
  if (x >= 0 & x < 10) {
    return(words[x +1])
  } else {
    return(x) }
  
}

# General data ----
# _ AWQMS data ----
# Update date: 2021-8-28
load("//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/data/R/statewide/df_awqms_raw_state.RData") # df.awqms.raw.state
load("//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/data/R/statewide/df_stations_state.RData") # df.stations.state
load("//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/data/R/statewide/df_stations_complete.RData") # df.stations

awqms.data.temp <- df.awqms.raw.state %>% 
  # AWQMS QA/QC check:
  dplyr::filter(Result_status %in% c("Final", "Provisional") & DQL %in% c("A","B","E","Q",NA)) %>% 
  dplyr::mutate(Source = "AWQMS",
                SampleStartDate = as.Date(SampleStartDate)) %>% 
  dplyr::select("Char_Name","Result_status","Result_Numeric","MLocID","SampleStartDate","Activity_Type","AU_ID","HUC8",             
                "HUC8_Name","HUC10","HUC12","HUC12_Name","Lat_DD","Long_DD","Measure","Method_Code","MonLocType","Org_Name",
                "OrganizationID","Project1","QualifierAbbr","Reachcode","Result_Comment","Result_Depth","Result_Depth_Unit",
                "Result_Operator","Result_Type","Result_Unit","SampleStartTime","SampleStartTZ","SamplingMethod","StationDes",       
                "Statistical_Base","Time_Basis","Source") 

awqms.stations.temp <- df.stations.state %>%
  dplyr::filter(MLocID %in% awqms.data.temp$MLocID) %>% # filter out the stations that have data beyond the period of 1990-01-01 ~ 2022-11-08
  dplyr::mutate(StationDes = ifelse(MLocID == "28328-ORDEQ", "Ramsey Creek at new Forest Boundary", StationDes)) # Temporary correction of a typo in the AWQMS database. Remove this line after the station name is corrected in the AWQMS.

# _ * data.dir ----
data.dir <- "//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/model_QAPPs/R/data/"
data.dir.yg <- "E:/PROJECTS/20200810_RyanMichie_TempTMDLReplacement/R/branches/" # Yuan's location

# _ USGS flow data ----
load(paste0(data.dir,"/download/usgs_fl.RData")) # usgs.fl.stations & usgs.fl.data
usgs.flow.stations <- usgs.fl.stations %>% 
  dplyr::filter(site_no %in% usgs.fl.data$site_no) # filter out the stations that have data beyond the period of 1990-2020

# _ OWRD data ----
load(paste0(data.dir,"/download/owrd.RData")) # owrd.stations.or & owrd.data.or
owrd.stations.or <- owrd.stations.or %>% 
  dplyr::mutate(station_name = stringr::str_to_title(station_name)) %>%
  dplyr::mutate_at("station_name", str_replace_all, ", Or", ", OR")

owrd.data <- owrd.data.or %>% 
  dplyr::filter(!published_status %in% c("Missing")) %>% 
  tidyr::separate(record_date, sep = "-", into = c("month","day","year")) %>% 
  dplyr::mutate(record_date = ymd(paste(year,month,day,sep="-"))) %>% 
  dplyr::select(-c(month,day,year)) %>% 
  dplyr::left_join(owrd.stations.or[,c("station_nbr","station_name")], by="station_nbr") %>% 
  dplyr::select(Char_Name = Characteristic.Name,
                Result_status = published_status,
                Result_Numeric = Result.Value,
                MLocID = station_nbr,
                SampleStartDate = record_date,
                StationDes = station_name) %>% 
  dplyr::mutate(Activity_Type = NA,
                AU_ID = NA,
                HUC8 = NA,
                HUC8_Name = NA,
                HUC10 = NA,
                HUC12 = NA,
                HUC12_Name = NA,
                Lat_DD = NA,
                Long_DD = NA,
                Measure = NA,
                Method_Code = NA,
                MonLocType = NA,
                Org_Name = NA,
                OrganizationID = NA,
                Project1 = NA,
                QualifierAbbr = NA,
                Reachcode = NA,
                Result_Comment = NA,
                Result_Depth = NA,
                Result_Depth_Unit = NA,
                Result_Operator = NA,
                Result_Type = NA,
                Result_Unit = NA,
                SampleStartTime = NA,
                SampleStartTZ = NA,
                SamplingMethod = NA,
                Statistical_Base = "Maximum",
                Time_Basis = NA)

owrd.stations <- owrd.stations.or %>% 
  dplyr::filter(station_nbr %in% owrd.data$MLocID) # filter out the stations that have data beyond the period of 1990-2020

# _ Worksheet ----
cal.model <- readxl::read_xlsx(paste0(data.dir, "Model_Setup_Info.xlsx"), sheet = "Calibration Model Setup Info") %>% 
  dplyr::filter(!`QAPP Project Area` %in%  c("Upper Klamath and Lost Subbasins")) %>% 
  # for Section 6.1 General model inputs and parameters
  dplyr::mutate(Model_version = ifelse(substr(`Model version`, 13,13) == "6", "Heat Source version 6",
                                       ifelse(substr(`Model version`, 13,13) == "7", "Heat Source version 7",
                                              ifelse(substr(`Model version`, 13,13) == "8", "Heat Source version 8",
                                                     ifelse(substr(`Model version`, 13,13) == "9", ifelse(`Primary Model Parameter` == "Solar","Heat Source version 9 shade model","Heat Source version 9"),
                                                            ifelse(substr(`Model version`, 1,2) == "CE", "CE-QUAL-W2 version 3","SHADOW")))))) %>% 
  dplyr::mutate(mod_rmd = ifelse(Model_version == "Heat Source version 6", "hs6",
                                 ifelse(Model_version == "Heat Source version 7", "hs7",
                                        ifelse(Model_version == "Heat Source version 8", "hs8",
                                               ifelse(Model_version == "Heat Source version 9", "hs9",
                                                      ifelse(Model_version == "Heat Source version 9 shade model", "hs9",
                                                             ifelse(Model_version == "CE-QUAL-W2 version 3", "ce",
                                                                    "sh"))))))) %>% 
  dplyr::mutate(mod_score = ifelse(mod_rmd == "hs6", "1",
                                   ifelse(mod_rmd == "hs7", "10",
                                          ifelse(mod_rmd == "hs8", "20", "0")))) %>% 
  dplyr::mutate(mod_ref = ifelse(mod_rmd == "ce", 'Cole, T.M., and S. A. Wells. 2000. "CE-QUAL-W2: A Two-Dimensional, Laterally Averaged, Hydrodynamic and Water Quality Model, Version 3.0." Instruction Report EL-2000. US Army Engineering and Research Development Center, Vicksburg, MS.',
                                 ifelse(mod_rmd == "sh", 'USFS (U.S. Forest Service). 1993. "SHADOW v. 2.3 - Stream Temperature Management Program. Prepared by Chris Park USFS, Pacific Northwest Region."',
                                        NA))) %>% 
  dplyr::mutate(mod_ref_intext = ifelse(mod_rmd == "ce", "(Cole and Wells, 2000)",
                                        ifelse(mod_rmd == "sh", "(USFS, 1993)",
                                               ifelse(mod_rmd %in% c("hs7","hs8"), "(Boyd and Kasper, 2003)",
                                                      NA))))
cal.input <- readxl::read_xlsx(paste0(data.dir, "Model_Setup_Info.xlsx"), sheet = "Calibration Inputs") %>% 
  dplyr::filter(!`QAPP Project Area` %in% "Upper Klamath and Lost Subbasins") %>% 
  dplyr::mutate(`Data Source` = ifelse(`Data Source` == "DEQ File", "DEQ", `Data Source`))
tir <- readxl::read_xlsx(paste0(data.dir, "Model_Setup_Info.xlsx"), sheet = "TIR")
schedule <- readxl::read_xlsx(paste0(data.dir, "Model_Setup_Info.xlsx"), sheet = "Schedule")
ref <- readxl::read_xlsx(paste0(data.dir, "Model_Setup_Info.xlsx"), sheet = "References")
roles <- readxl::read_xlsx(paste0(data.dir,"tables.xlsx"),sheet = "roles")
risks <- readxl::read_xlsx(paste0(data.dir,"tables.xlsx"),sheet = "risks")
abbr <- readxl::read_xlsx(paste0(data.dir,"tables.xlsx"),sheet = "abbr")
data.gap <- readxl::read_xlsx(paste0(data.dir,"tables.xlsx"),sheet = "data_gap")
rev <- readxl::read_xlsx(paste0(data.dir,"tables.xlsx"),sheet = "revision_history")
effective.shade <- readxl::read_xlsx(paste0(data.dir,"Effective_shade.xlsx"),sheet = "Effective_shade") %>% dplyr::filter(!`Result Status` == "REJECT")
effective.shade.lookup <- readxl::read_xlsx(paste0(data.dir,"Effective_shade.xlsx"),sheet = "Lookup")
inst.flow <- readxl::read_xlsx(paste0(data.dir,"Inst_flow.xlsx"),sheet = "Inst_flow")

# _ NPDES ----
# correct master list lat/long based on 7Q10 lat/long 
npdes.7q10 <- readxl::read_xlsx("E:/PROJECTS/20200810_RyanMichie_TempTMDLReplacement/7Q10E/7Q10.xlsx", sheet = "NPDES") 
npdes.ind <- readxl::read_xlsx(paste0(data.dir, "NPDES_Master_list.xlsx"), sheet = "Individual_NDPES") %>% 
  dplyr::mutate(`Common Name` = stringr::str_to_title(`Common Name`)) %>%
  dplyr::mutate_at("Common Name", str_replace_all, "Ati ", "ATI ") %>%
  dplyr::mutate_at("Common Name", str_replace_all, "Bdc/", "BDC/") %>%
  dplyr::mutate_at("Common Name", str_replace_all, "cmss", "CMSS") %>%
  dplyr::mutate_at("Common Name", str_replace_all, "Llc", "LLC") %>%
  dplyr::mutate_at("Common Name", str_replace_all, "Ms4", "MS4") %>%
  dplyr::mutate_at("Common Name", str_replace_all, "Mwmc", "MWMC") %>%
  dplyr::mutate_at("Common Name", str_replace_all, "Nw ", "NW ") %>%
  dplyr::mutate_at("Common Name", str_replace_all, "Odc", "ODC") %>%
  dplyr::mutate_at("Common Name", str_replace_all, "Odfw", "ODFW") %>%
  dplyr::mutate_at("Common Name", str_replace_all, "Odot", "ODOT") %>%
  dplyr::mutate_at("Common Name", str_replace_all, "Ohsu", "OHSU") %>%
  dplyr::mutate_at("Common Name", str_replace_all, "R.u.s.a.", "R.U.S.A.") %>% #South Umpqua
  dplyr::mutate_at("Common Name", str_replace_all, "Slli", "SLLI") %>%
  dplyr::mutate_at("Common Name", str_replace_all, "Stp", "STP") %>% 
  dplyr::mutate_at("Common Name", str_replace_all, "Usa", "USA") %>% 
  dplyr::mutate_at("Common Name", str_replace_all, "Usfs", "USFS") %>% 
  dplyr::mutate_at("Common Name", str_replace_all, "Usfw", "USFW") %>% 
  dplyr::mutate_at("Common Name", str_replace_all, "Wes ", "WES ") %>%
  dplyr::mutate_at("Common Name", str_replace_all, "Wpcp", "WPCP") %>% 
  dplyr::mutate_at("Common Name", str_replace_all, "Wrf", "WRF") %>%
  dplyr::mutate_at("Common Name", str_replace_all, "wrf", "WRF") %>%
  dplyr::mutate_at("Common Name", str_replace_all, "Wwtf", "WWTF") %>%
  dplyr::mutate_at("Common Name", str_replace_all, "Wwtp", "WWTP")

for(permit_Nbr in unique(sort(npdes.7q10$NPDES_Permit_Nbr))){
  
  print(permit_Nbr)
  # test: permit_Nbr = "100522"
  if(!permit_Nbr %in% c("10109","101917")){npdes.ind[which(npdes.ind$`Permit Nbr` == permit_Nbr),]$Latitude <- unique(npdes.7q10[which(npdes.7q10$NPDES_Permit_Nbr == permit_Nbr),]$Outfall_Latitude)[1]}
  if(!permit_Nbr %in% c("10109","101917")){npdes.ind[which(npdes.ind$`Permit Nbr` == permit_Nbr),]$Longitude <- unique(npdes.7q10[which(npdes.7q10$NPDES_Permit_Nbr == permit_Nbr),]$Outfall_Longitude)[1]}
  
}

npdes.gen <- readxl::read_xlsx(paste0(data.dir, "NPDES_Master_list.xlsx"), sheet = "Gen_NPDES")

# _ Lookup table & Project areas ----
lookup.huc <- readxl::read_xlsx(paste0(data.dir, "Lookup_QAPPProjectArea.xlsx"), sheet = "Lookup_QAPPProjectArea") %>% 
  dplyr::mutate(HUC_6 = as.character(HUC_6),
                HUC_8 = as.character(HUC_8),
                HUC10 = as.character(HUC10),
                HUC12 = as.character(HUC12))

project.areas <- readxl::read_xlsx(paste0(data.dir,"qapp_project_areas.xlsx"), sheet = "qapp_project_areas") %>% 
  dplyr::left_join(schedule, by=c("areas"="QAPP Project Area"))

# _ IR2018/20 Cat 4 & 5 ----
{
  # cat.45.rivers <- sf::st_read(dsn = "//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/GIS/2018_2020_IR_Cat4_5_Temp_Rivers_FINAL.shp",
  #                              layer = "2018_2020_IR_Cat4_5_Temp_Rivers_FINAL")
  # cat.45.waterbodies <- sf::st_read(dsn = "//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/GIS/2018_2020_IR_Cat4_5_Temp_Waterbodies_FINAL.shp",
  #                                   layer = "2018_2020_IR_Cat4_5_Temp_Waterbodies_FINAL")
  # cat.45.watershed <- sf::st_read(dsn = "//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/GIS/2018_2020_IR_Cat4_5_Temp_Watershed_FINAL.shp",
  #                                 layer = "2018_2020_IR_Cat4_5_Temp_Watershed_FINAL") %>% 
  #   dplyr::mutate(AU_Name = gsub(pattern = "HUC12 Name: ","",AU_Name)) %>% 
  #   dplyr::mutate(AU_Name = paste0(AU_Name, " Watershed"))
}

# _ IR2022 Cat 4 & 5 ----
# Updated on 11/30/2023: the full extent of the McKenzie River is moved from WMS to WS
wms.aus <- readxl::read_xlsx("//deqhq1/tmdl/TMDL_Willamette/Willamette_Mainstem_Temperature_2025/Project_Plans/Willamette_Mainstem_AUs_2023.11.27.xlsx",sheet = "Final_AUs")

columbia_aus <- sf::st_read(dsn = "//deqhq1/tmdl/Planning statewide/TMDL_Priorities/2018_2020_IR/working_2020_2024",
                            layer="Columbia_River_AU_IDs",
                            stringsAsFactors=FALSE) %>% sf::st_drop_geometry()

load("//deqhq1/tmdl/Planning statewide/Temperature_TMDL_Revisions/model_QAPPs/R/data/RData/ir2022_pro4a5AUs.RData") # See oneDrive/arcgis.Rmd
# Test: plot(sf::st_geometry(ir_temp_4a5_rivers_project_areas$geometry))
cat.45.rivers <- ir_temp_4a5_rivers_project_areas %>% 
  dplyr::select(AU_ID,AU_Name,AU_Description,Assessment,Pollutant,period,Year_listed,OWRD_Basin,stations,
                AU_parameter_category = Parameter_category,
                QAPP_Project_Area,HUC_6,HUC_8,HUC8_NAME,HUC10,HUC10_NAME,HUC12,HUC12_Name,
                geometry) %>% 
  dplyr::mutate(`Period_(Year_Listed)` = paste0(period," (",Year_listed,")")) %>% 
  dplyr::filter(!AU_ID %in% columbia_aus$AU_ID)

cat.45.waterbodies <- ir_temp_4a5_waterbodies_project_areas %>% 
  dplyr::select(AU_ID,AU_Name,AU_Description,Assessment,Pollutant,period,Year_listed,OWRD_Basin,stations,
                AU_parameter_category = Parameter_category,
                QAPP_Project_Area,HUC_6,HUC_8,HUC8_NAME,HUC10,HUC10_NAME,HUC12,HUC12_Name,
                geometry = Shape) %>% 
  dplyr::mutate(`Period_(Year_Listed)` = paste0(period," (",Year_listed,")")) %>% 
  dplyr::filter(!AU_ID %in% columbia_aus$AU_ID)

cat.45.watershed <- ir_temp_4a5_watersheds_project_areas %>% 
  dplyr::select(AU_ID,AU_Name,AU_Description,Assessment,Pollutant,period,Year_listed,OWRD_Basin,stations,
                AU_parameter_category = Parameter_category,
                QAPP_Project_Area,HUC_6,HUC_8,HUC8_NAME,HUC10,HUC10_NAME,HUC12,HUC12_Name,
                geometry) %>% 
  dplyr::mutate(`Period_(Year_Listed)` = paste0(period," (",Year_listed,")")) %>% 
  dplyr::filter(!AU_ID %in% columbia_aus$AU_ID)

cat.45.tbl <- rbind(cat.45.rivers[,c("AU_parameter_category","AU_Name","AU_ID","Period_(Year_Listed)","HUC12","QAPP_Project_Area")],
                    cat.45.waterbodies[,c("AU_parameter_category","AU_Name","AU_ID","Period_(Year_Listed)","HUC12","QAPP_Project_Area")],
                    cat.45.watershed[,c("AU_parameter_category","AU_Name","AU_ID","Period_(Year_Listed)","HUC12","QAPP_Project_Area")]) %>% 
  dplyr::mutate_at("Period_(Year_Listed)", stringr::str_replace_all, "spawn", "Spawn") %>%
  dplyr::mutate_at("Period_(Year_Listed)", stringr::str_replace_all, "year_round", "Year-round")

# _ NCDC met data ----
load(paste0(data.dir,"/download/ncei.RData")) # ncei & ncei.datacats.or
ncei.stations <- ncei %>% 
  dplyr::mutate(lat = LAT_DEC,
                long = LON_DEC) %>% 
  sf::st_as_sf(coords = c("LON_DEC", "LAT_DEC"), crs = sf::st_crs("+init=EPSG:4326"))

# _ RAWS met data ----
load(paste0(data.dir,"/download/raws.RData")) # raws.meta & raws.data.type
raws.stations <- raws.meta %>% 
  dplyr::mutate(lat = latitude,
                long = longitude) %>%
  sf::st_as_sf(coords = c("longitude", "latitude"), crs = sf::st_crs("+init=EPSG:4326"))

# _ USBR AgriMet met data ----
agrimet.stations <- read.csv(paste0(data.dir, "download/agrimet_stations.csv"))
agrimet.parameters <- read.csv(paste0(data.dir, "download/agrimet_parameters.csv"))
agrimet.stations.or <- agrimet.stations %>%
  dplyr::filter(state == "OR") %>% 
  dplyr::mutate(lat = latitude, long = longitude) %>% 
  sf::st_as_sf(coords = c("longitude", "latitude"), crs = sf::st_crs("+init=EPSG:4326"))

# _ USBR Hydromet met data ----
load(paste0(data.dir,"/download/hydromet.RData")) # hydromet & hydromet.data
hydromet.stations <- hydromet %>% 
  dplyr::mutate(lat = Lat, long = Long) %>% 
  sf::st_as_sf(coords = c("Long", "Lat"), crs = sf::st_crs("+init=EPSG:4326")) 

# _ MesoWest met data ----
load(paste0(data.dir,"/download/mw.RData")) # mw.meta & mw.variables.list
mw.stations <- mw.meta %>%
  dplyr::mutate(lat = LATITUDE, long = LONGITUDE) %>% 
  sf::st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = sf::st_crs("+init=EPSG:4326"))

# _ NLCD Land Cover data ----
load(paste0(data.dir,"/RData/nlcd.tbl.RData")) # nlcd.tbl
load(paste0(data.dir,"/RData/nlcd.text.RData")) # nlcd.text

# _ DMAs ----
load(paste0(data.dir,"/RData/dmas.RData")) # dma.tbl

# _ BES temp ----
load(paste0(data.dir,"/download/bes.RData")) # bes.stations & bes.data
bes.data <- bes.data %>% 
  dplyr::left_join(bes.stations,by=c("Monitoring_Location_ID" = "LocationIdentifier")) %>% 
  dplyr::rename(MLocID = Monitoring_Location_ID,
                SampleStartDate = date,
                Result_Unit = Result.Unit) %>% 
  dplyr::mutate(Result_status = "Good, Approved",
                StationDes = LocationName,
                Activity_Type = NA,
                AU_ID = NA,
                HUC8 = NA,
                HUC8_Name = NA,
                HUC10 = NA,
                HUC12 = NA,
                HUC12_Name = NA,
                Lat_DD = NA,
                Long_DD = NA,
                Measure = NA,
                Method_Code = NA,
                MonLocType = NA,
                Org_Name = "Portland Environmental Services",
                OrganizationID = "COP",
                Project1 = NA,
                QualifierAbbr = NA,
                Reachcode = NA,
                Result_Comment = NA,
                Result_Depth = NA,
                Result_Depth_Unit = NA,
                Result_Operator = NA,
                Result_Type = NA,
                SampleStartTime = NA,
                SampleStartTZ = NA,
                SamplingMethod = NA,
                Statistical_Base = "Maximum",
                Time_Basis = NA) %>% 
  dplyr::select("Char_Name","Result_status","Result_Numeric","MLocID","SampleStartDate","StationDes",
                "Activity_Type","AU_ID","HUC8","HUC8_Name","HUC10","HUC12","HUC12_Name","Lat_DD","Long_DD",
                "Measure","Method_Code","MonLocType","Org_Name","OrganizationID","Project1","QualifierAbbr",
                "Reachcode","Result_Comment","Result_Depth","Result_Depth_Unit","Result_Operator","Result_Type",
                "Result_Unit","SampleStartTime","SampleStartTZ","SamplingMethod","Statistical_Base","Time_Basis")

# _ Project areas and HUCs ----
# pro_areas <- sf::st_read(dsn = "//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/model_QAPPs/R/data/gis/project_areas.shp",
#                          layer = "project_areas") %>% 
#   dplyr::rename(Project_Na = QAPP_Proje) # because Project_Na was used in the older shapefile and the following script.

# updated project areas for combined Willamette Subbasins 
# Note: Need to use the previous project_areas.shp when re-run QAPP documents for Willamette Subbasins
pro_areas <- sf::st_read(dsn = "//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/model_QAPPs/R/data/gis/project_areas_ws.shp",
                         layer = "project_areas_ws") %>% 
  dplyr::rename(Project_Na = QAPP_Proje) # because Project_Na was used in the older shapefile and the following script.

pro_areas_huc8 <- sf::st_read(dsn = "//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/GIS/Study_Areas_v5_HUC8_scope.shp",
                              layer = "Study_Areas_v5_HUC8_scope")

# TempTMDL_QAPP_Reaches.shp includes Porject_Na = c("Willamette River Mainstem and Major Tributaries", "Snake River – Hells Canyon", 
# and "Southern Willamette Subbasins" for the McKenzie river)
pro.reaches <- sf::st_read(dsn = "//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/GIS/willa_snake/TempTMDL_QAPP_Reaches.shp",
                           layer = "TempTMDL_QAPP_Reaches",
                           stringsAsFactors=FALSE) %>% 
  sf::st_zm() %>% 
  sf::st_drop_geometry()

## Get Willamette Mainstem AU IDs and reachcodes
will_auid <- wms.aus %>% dplyr::pull(AU_ID)

will_reachcodes <- pro.reaches %>%
  filter(Project_Na=="Willamette River Mainstem and Major Tributaries") %>%
  distinct(ReachCode) %>%
  pull(ReachCode)

## Get Snake River AU IDs to remove from Malheur and Grande Ronde project areas
snake_auid <- pro.reaches %>%
  dplyr::filter(Project_Na=="Snake River – Hells Canyon") %>%
  dplyr::distinct(AU_ID) %>%
  dplyr::pull(AU_ID)

snake_reachcodes <- pro.reaches %>%
  filter(Project_Na=="Snake River – Hells Canyon") %>%
  distinct(ReachCode) %>%
  pull(ReachCode)