# __Willamette River Mainstem and Major Tributaries__ ----
library(tidyverse)
library(readxl)
library(rgdal)
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
  dplyr::filter(MLocID %in% awqms.data.temp$MLocID) # filter out the stations that have data beyond the period of 1990-2020

# _ * data.dir ----
data.dir <- "//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/model_QAPPs/R/data/"
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
                                 ifelse(mod_rmd == "sh", 'USFS (U.S. Forest Service). 1993. “SHADOW v. 2.3 - Stream Temperature Management Program. Prepared by Chris Park USFS, Pacific Northwest Region.”',
                                        NA))) %>% 
  dplyr::mutate(mod_ref_intext = ifelse(mod_rmd == "ce", "(Cole and Wells, 2000)",
                                        ifelse(mod_rmd == "sh", "(USFS, 1993)",
                                               ifelse(mod_rmd %in% c("hs7","hs8"), "(Boyd and Kasper, 2003)",
                                                      NA))))
cal.input <- readxl::read_xlsx(paste0(data.dir, "Model_Setup_Info.xlsx"), sheet = "Calibration Inputs") %>% 
  dplyr::filter(!`QAPP Project Area` %in% "Upper Klamath and Lost Subbasins")
tir <- readxl::read_xlsx(paste0(data.dir, "Model_Setup_Info.xlsx"), sheet = "TIR")
schedule <- readxl::read_xlsx(paste0(data.dir, "Model_Setup_Info.xlsx"), sheet = "Schedule")
ref <- readxl::read_xlsx(paste0(data.dir, "Model_Setup_Info.xlsx"), sheet = "References")
roles <- readxl::read_xlsx(paste0(data.dir,"tables.xlsx"),sheet = "roles")
risks <- readxl::read_xlsx(paste0(data.dir,"tables.xlsx"),sheet = "risks")
abbr <- readxl::read_xlsx(paste0(data.dir,"tables.xlsx"),sheet = "abbr")
data.gap <- readxl::read_xlsx(paste0(data.dir,"tables.xlsx"),sheet = "data_gap")
rev <- readxl::read_xlsx(paste0(data.dir,"tables.xlsx"),sheet = "revision_history")
effective.shade <- readxl::read_xlsx(paste0(data.dir,"Effective_shade.xlsx"),sheet = "Effective_shade")
effective.shade.lookup <- readxl::read_xlsx(paste0(data.dir,"Effective_shade.xlsx"),sheet = "Lookup")
inst.flow <- readxl::read_xlsx(paste0(data.dir,"Inst_flow.xlsx"),sheet = "Inst_flow")

# _ NPDES ----
npdes.ind <- readxl::read_xlsx(paste0(data.dir, "NPDES_Master_list.xlsx"), sheet = "Individual_NDPES") %>% 
  dplyr::mutate(`Common Name` = stringr::str_to_title(`Common Name`)) %>%
  dplyr::mutate_at("Common Name", str_replace_all, "Ati ", "ATI ") %>%
  dplyr::mutate_at("Common Name", str_replace_all, "Bdc/", "BDC/") %>%
  dplyr::mutate_at("Common Name", str_replace_all, "cmss", "CMSS") %>%
  dplyr::mutate_at("Common Name", str_replace_all, "Ms4", "MS4") %>%
  dplyr::mutate_at("Common Name", str_replace_all, "Mwmc", "MWMC") %>%
  dplyr::mutate_at("Common Name", str_replace_all, "Odc", "ODC") %>%
  dplyr::mutate_at("Common Name", str_replace_all, "Odfw", "ODFW") %>%
  dplyr::mutate_at("Common Name", str_replace_all, "Odot", "ODOT") %>%
  dplyr::mutate_at("Common Name", str_replace_all, "Ohsu", "OHSU") %>%
  dplyr::mutate_at("Common Name", str_replace_all, "Slli", "SLLI") %>%
  dplyr::mutate_at("Common Name", str_replace_all, "Usa", "USA") %>% 
  dplyr::mutate_at("Common Name", str_replace_all, "Usfs", "USFS") %>% 
  dplyr::mutate_at("Common Name", str_replace_all, "Usfw", "USFW") %>% 
  dplyr::mutate_at("Common Name", str_replace_all, "Wes ", "WES ") %>%
  dplyr::mutate_at("Common Name", str_replace_all, "Wrf", "WRF") %>%
  dplyr::mutate_at("Common Name", str_replace_all, "wrf", "WRF") %>%
  dplyr::mutate_at("Common Name", str_replace_all, "Wwtp", "WWTP") %>%
  dplyr::mutate_at("Common Name", str_replace_all, "Stp", "STP")

npdes.gen <- readxl::read_xlsx(paste0(data.dir, "NPDES_Master_list.xlsx"), sheet = "Gen_NPDES")

# _ Lookup table & Project areas ----
lookup.huc <- readxl::read_xlsx(paste0(data.dir, "Lookup_QAPPProjectArea.xlsx"), sheet = "Lookup_QAPPProjectArea") %>% 
  dplyr::mutate(HUC_6 = as.character(HUC_6),
                HUC_8 = as.character(HUC_8),
                HUC10 = as.character(HUC10),
                HUC12 = as.character(HUC12))

project.areas <- read.csv(paste0(data.dir,"qapp_project_areas.csv")) %>% 
  dplyr::left_join(schedule, by=c("areas"="QAPP Project Area"))

# _ * general data for leaflet map ----
save(lookup.huc,
     project.areas,
     file = paste0("./data/lookup.RData"))

# _ IR2018/20 Cat 4 & 5 ----
cat.45.rivers <- sf::st_read(dsn = "//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/GIS/2018_2020_IR_Cat4_5_Temp_Rivers_FINAL.shp",
                             layer = "2018_2020_IR_Cat4_5_Temp_Rivers_FINAL")
cat.45.waterbodies <- sf::st_read(dsn = "//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/GIS/2018_2020_IR_Cat4_5_Temp_Waterbodies_FINAL.shp",
                                  layer = "2018_2020_IR_Cat4_5_Temp_Waterbodies_FINAL")
cat.45.watershed <- sf::st_read(dsn = "//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/GIS/2018_2020_IR_Cat4_5_Temp_Watershed_FINAL.shp",
                                layer = "2018_2020_IR_Cat4_5_Temp_Watershed_FINAL") %>% 
  dplyr::mutate(AU_Name = gsub(pattern = "HUC12 Name: ","",AU_Name)) %>% 
  dplyr::mutate(AU_Name = paste0(AU_Name, " Watershed"))

cat.45 <- rbind(cat.45.rivers[,c("IR_categor","AU_Name","AU_ID","Year_liste","Period","HUC12")],
                cat.45.waterbodies[,c("IR_categor","AU_Name","AU_ID","Year_liste","Period","HUC12")],
                cat.45.watershed[,c("IR_categor","AU_Name","AU_ID","Year_liste","Period","HUC12")]) %>% 
  sf::st_zm() %>% 
  dplyr::left_join(lookup.huc, by = "HUC12")

cat.45.tbl <- sf::st_drop_geometry(cat.45) %>% 
  #dplyr::filter(!AU_ID %in% c(colum_auid)) %>%
  dplyr::mutate_at("AU_Name", str_replace_all, "Cre\\*", "Creek") %>%
  dplyr::mutate_at("AU_Name", str_replace_all, "For\\*", "Fork Willamette River") %>%
  dplyr::mutate_at("AU_Name", str_replace_all, "Joh\\*", "John Day River") %>% 
  dplyr::mutate_at("AU_Name", str_replace_all, "John\\*", "John Day River") %>% 
  dplyr::mutate_at("AU_Name", str_replace_all, "McKenzie \\*", "McKenzie River") %>% 
  dplyr::mutate_at("AU_Name", str_replace_all, "Mill\\*", "Mill Creek") %>%
  dplyr::mutate_at("AU_Name", str_replace_all, "R\\*", "River") %>%
  dplyr::mutate_at("AU_Name", str_replace_all, "Ri\\*", "River") %>%
  dplyr::mutate_at("AU_Name", str_replace_all, "Riv\\*", "River") %>% 
  dplyr::mutate_at("AU_Name", str_replace_all, "Thunder Creek-North Unpqua River", "Thunder Creek-North Umpqua River")  %>%
  dplyr::mutate_at("AU_Name", str_replace_all, "W\\*", "Willamette River") %>%
  dplyr::mutate_at("AU_Name", str_replace_all, "Willamet\\*", "Willamette River") %>%
  dplyr::mutate_at("AU_Name", str_replace_all, "Willamett\\*", "Willamette River") %>% 
  dplyr::mutate_at("AU_Name", str_replace_all, "Willamette \\*", "Willamette River")

# _ NCDC met data ----
load(paste0(data.dir,"/download/ncei.RData")) # ncei & ncei.datacats.or
ncei.stations <- ncei %>% 
  dplyr::mutate(lat = LAT_DEC,
                long = LON_DEC) %>% 
  sf::st_as_sf(coords = c("LON_DEC", "LAT_DEC"), crs = sf::st_crs("+init=EPSG:4269"))

# _ RAWS met data ----
load(paste0(data.dir,"/download/raws.RData")) # raws.meta & raws.data.type
raws.stations <- raws.meta %>% 
  dplyr::mutate(lat = latitude,
                long = longitude) %>%
  sf::st_as_sf(coords = c("longitude", "latitude"), crs = sf::st_crs("+init=EPSG:4269"))

# _ USBR AgriMet met data ----
agrimet.stations <- read.csv(paste0(data.dir, "download/agrimet_stations.csv"))
agrimet.parameters <- read.csv(paste0(data.dir, "download/agrimet_parameters.csv"))
agrimet.stations.or <- agrimet.stations %>%
  dplyr::filter(state == "OR") %>% 
  dplyr::mutate(lat = latitude, long = longitude) %>% 
  sf::st_as_sf(coords = c("longitude", "latitude"), crs = sf::st_crs("+init=EPSG:4269"))

# _ USBR Hydromet met data ----
load(paste0(data.dir,"/download/hydromet.RData")) # hydromet & hydromet.data
hydromet.stations <- hydromet %>% 
  dplyr::mutate(lat = Lat, long = Long) %>% 
  sf::st_as_sf(coords = c("Long", "Lat"), crs = sf::st_crs("+init=EPSG:4269")) 

# _ MesoWest met data ----
load(paste0(data.dir,"/download/mw.RData")) # mw.meta & mw.variables.list
mw.stations <- mw.meta %>%
  dplyr::mutate(lat = LATITUDE, long = LONGITUDE) %>% 
  sf::st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = sf::st_crs("+init=EPSG:4269"))

# _ NLCD Land Cover data ----
load(paste0(data.dir,"/RData/nlcd.tbl_wms.RData")) # nlcd.tbl
load(paste0(data.dir,"/RData/nlcd.text_wms.RData")) # nlcd.text

# _ DMAs ----
load(paste0(data.dir,"/RData/dmas_wms.RData")) # dma.tbl

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
#pro_areas <- sf::st_read(dsn = "//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/model_QAPPs/R/data/gis/project_areas.shp",
#                         layer = "project_areas")

#pro_areas_huc8 <- sf::st_read(dsn = "//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/GIS/Study_Areas_v5_HUC8_scope.shp",
#                              layer = "Study_Areas_v5_HUC8_scope")



# *** RUN BELOW IF AFTER data.R ----
# QAPP Project Area Data ----
qapp_project_area = "Willamette River Mainstem and Major Tributaries"

print(qapp_project_area)
# _ Project area and HUCs ----
subbasin_num <- unique(lookup.huc[which(lookup.huc$QAPP_Project_Area == qapp_project_area),]$HUC12)

pro_area_huc12 <- sf::read_sf(dsn = "//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/GIS/willa_snake/TempTMDL_QAPP_Reaches_data_query_HUC12s.shp",
                              layer = "TempTMDL_QAPP_Reaches_data_query_HUC12s") %>% 
  dplyr::filter(Project_Na == qapp_project_area) %>% 
  sf::st_transform(4269) #4326

pro_area_huc12_union <- sf::st_union(pro_area_huc12)

#huc12.extent <- project.areas[which(project.areas$areas == qapp_project_area),]$huc8.extent
#HUC12 extent of "Willamette River Mainstem and Major Tributaries" was saved in the same column of project.areas$huc8.extent

file.name <- project.areas[which(project.areas$areas == qapp_project_area),]$file.name

# _ IR2018/20 Cat 4 & 5 ----
pro.cat.45.tbl <- cat.45.tbl %>% 
  dplyr::filter(QAPP_Project_Area %in% qapp_project_area)

# _ Temp data ----
# AWQMS, OWRD, BES, and DOE Temp Data
## _ (1) AWQMS ----
station.awqms <- awqms.stations.temp %>% 
  dplyr::rename(`Station ID` = MLocID,
                Station = StationDes,
                Organization = OrgID,
                Latitude = Lat_DD,
                Longitude = Long_DD) %>% 
  dplyr::mutate(Organization = ifelse(Organization == "USGS-OR(INTERNAL)", "USGS", Organization),
                Organization = ifelse(Organization == "USGS-OR", "USGS", Organization)) %>% 
  dplyr::mutate(lat = Latitude,
                long = Longitude) %>%
  sf::st_as_sf(coords = c("long", "lat"), crs = sf::st_crs("+init=EPSG:4269")) %>% 
  dplyr::filter(sf::st_intersects(pro_area_huc12_union, ., sparse = FALSE)) %>% 
  sf::st_drop_geometry()

station.awqms.usgs.or <- station.awqms %>% 
  dplyr::filter(Organization == "USGS") %>% 
  dplyr::mutate(Station = stringr::str_to_title(Station)) %>% 
  dplyr::mutate_at("Station", str_replace_all, "Or", "OR") %>% 
  dplyr::mutate_at("Station", str_replace_all, "Ordeq", "ORDEQ") %>%
  dplyr::mutate_at("Station", str_replace_all, "ORegon", "Oregon") %>% 
  dplyr::mutate_at("Station", str_replace_all, " Rm", " RM") %>%
  dplyr::mutate_at("Station", str_replace_all, "Lb", "LB") %>%
  dplyr::mutate_at("Station", str_replace_all, "Nf", "NF") %>%
  dplyr::mutate_at("Station", str_replace_all, "Sf", "SF")

station.awqms.temp <- station.awqms %>% 
  dplyr::filter(!Organization == "USGS") %>% 
  rbind(station.awqms.usgs.or)

## _ (2) OWRD ----
station.owrd <- owrd.stations %>%
  dplyr::mutate(`Station ID` = as.character(station_nbr),
                lat = Lat,
                long = Long) %>%
  sf::st_as_sf(coords = c("long", "lat"), crs = sf::st_crs("+init=EPSG:4269")) %>% 
  dplyr::filter(sf::st_intersects(pro_area_huc12_union, ., sparse = FALSE)) %>% 
  sf::st_drop_geometry() %>% 
  dplyr::select(Organization = Operator,
                `Station ID` = station_nbr,
                `Station` = station_name,
                Latitude = Lat,
                Longitude = Long) %>% 
  dplyr::distinct(`Station ID`,.keep_all=TRUE)

owrd.data.temp <- owrd.data %>% 
  dplyr::filter(Char_Name %in% c("daily_max_water_temp_C")) %>% 
  dplyr::filter(MLocID %in% station.owrd$`Station ID`) %>% 
  dplyr::mutate(Source = "owrd")

station.owrd.temp <- station.owrd %>% 
  dplyr::filter(`Station ID` %in% owrd.data.temp$MLocID) # only keep the stations that have data

## _ (3) BES ----
station.bes <- bes.stations %>% 
  dplyr::mutate(Organization = "Portland Environmental Services",
                lat = Latitude,
                long = Longitude) %>% 
  sf::st_as_sf(coords = c("long", "lat"), crs = sf::st_crs("+init=EPSG:4269")) %>% 
  dplyr::filter(sf::st_intersects(pro_area_huc12_union, ., sparse = FALSE)) %>% 
  sf::st_drop_geometry() %>% 
  dplyr::select(Organization,
                `Station ID` = LocationIdentifier,
                `Station` = LocationName,
                Latitude,
                Longitude) %>% 
  dplyr::distinct(`Station ID`,.keep_all=TRUE)

bes.data.temp <- bes.data %>% 
  dplyr::filter(MLocID %in% station.bes$`Station ID`) %>% 
  dplyr::mutate(Source = "bes")

station.bes.temp <- station.bes %>% 
  dplyr::filter(`Station ID` %in% bes.data.temp$MLocID) # only keep the stations that have data

## _ (4) DOE ----
load(paste0(data.dir,"/download/doe.RData")) # doe.stations.pro.area & doe.data.pro.area
doe.data.temp <- doe.data.pro.area %>% 
  dplyr::mutate(SampleStartDate = as.Date(Field_Collection_Start_Date_Time)) %>% 
  dplyr::filter((year(SampleStartDate)>=2010) & (year(SampleStartDate)<=2020)) %>% 
  dplyr::select(StationDes = Location_Name,
                Result_status = Result_Data_Review_Status,
                Result_Numeric = Result_Value,
                Result_Unit = Result_Unit,
                MLocID = Location_ID,
                Lat_DD = Calculated_Latitude_Decimal_Degrees_NAD83HARN,
                Long_DD = Calculated_Longitude_Decimal_Degrees_NAD83HARN,
                SampleStartDate) %>% 
  dplyr::mutate(Activity_Type = NA,
                AU_ID = NA,
                HUC8 = NA,
                HUC8_Name = NA,
                HUC10 = NA,
                HUC12 = NA,
                HUC12_Name = NA,
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
                SampleStartTime = NA,
                SampleStartTZ = NA,
                SamplingMethod = NA,
                Time_Basis = NA,
                Char_Name = "Temperature, water (daily maximum)",
                Statistical_Base = "Maximum", 
                Source = "doe")

station.doe.temp <- doe.stations.pro.area %>% 
  dplyr::filter(Location_ID %in% doe.data.temp$MLocID) %>% # filter out the stations that have data beyond the period of 2010-2020
  dplyr::rename(`Station ID` = Location_ID,
                Station = Location_Name) %>% 
  dplyr::mutate(Organization = "DOE")

## _ (5) AWQMS + OWRD + BES + DOE ----  
temp.stations <- rbind(station.awqms.temp[,c("Station","Station ID", "Organization", "Latitude", "Longitude")],
                       station.owrd.temp[,c("Station","Station ID", "Organization", "Latitude", "Longitude")],
                       station.bes.temp[,c("Station","Station ID", "Organization", "Latitude", "Longitude")],
                       station.doe.temp[,c("Station","Station ID", "Organization", "Latitude", "Longitude")]) %>% 
  dplyr::distinct(Station,`Station ID`, .keep_all=TRUE) %>%
  dplyr::mutate(Organization = ifelse(Organization == "11NPSWRD_WQX", "EPA WQX", Organization)) %>% 
  dplyr::mutate(Organization = ifelse(Organization == "CITY_GRESHAM(NOSTORETID)", "City of Gresham", Organization)) %>%
  dplyr::mutate(Organization = ifelse(Organization == "CITY_SALEM(NOSTORETID)", "City of Salem", Organization)) %>%
  dplyr::mutate(Organization = ifelse(Organization == "CRITFC(NOSTORETID)", "CRITFC", Organization)) %>% 
  dplyr::mutate(Organization = ifelse(Organization == "CTUIR_WQX", "CTUIR WQX", Organization)) %>%
  dplyr::mutate(Organization = ifelse(Organization == "EMSWCD(NOSTORETID)", "EMSWCD", Organization)) %>%
  dplyr::mutate(Organization = ifelse(Organization == "IPC(NOSTORETID)", "Idaho Power Company", Organization)) %>%
  dplyr::mutate(Organization = ifelse(Organization == "OregonDEQ", "DEQ", Organization)) %>% 
  dplyr::mutate(Organization = ifelse(Organization == "PDX_WB(NOSTORETID)", "Portland Water Bureau", Organization)) %>%
  dplyr::mutate(Organization = ifelse(Organization == "PSU(NOSTORETID)", "Portland State University", Organization)) %>%
  dplyr::mutate(Organization = ifelse(Organization == "USFS(NOSTORETID)", "USFS", Organization)) %>%
  dplyr::mutate(Organization = ifelse(Organization == "USGS-OR", "USGS", Organization)) %>% 
  dplyr::mutate(Organization = ifelse(Organization == "USGS-OR(INTERNAL)", "USGS", Organization)) %>% 
  dplyr::mutate(Organization = ifelse(Organization == "WALLAWALLA_WC(NOSTORETID)", "Walla Walla Basin Watershed Council", Organization)) %>%
  dplyr::mutate(Organization = ifelse(Organization == "WEYERHAUSER(NOSTORETID)", "Weyerhaeuser", Organization))

temp.data <- awqms.data.temp %>% 
  dplyr::filter(MLocID %in% station.awqms.temp$`Station ID`) %>%
  dplyr::left_join(station.awqms.temp[,c("Station ID","Station")], by=c("MLocID"="Station ID")) %>% 
  dplyr::select(-StationDes) %>%
  dplyr::rename(StationDes = Station) %>% 
  rbind(owrd.data.temp,bes.data.temp,doe.data.temp)

# Temp data.sample.count will be used in the Appendix A
cols <- c("Year"=NA, "Station ID"=NA, "Station"=NA, 'Statistical_Base'=NA, "Org_Names"=NA,
          "Jan"=NA, "Feb"=NA, "Mar"=NA, "Apr"=NA, "May"=NA, "Jun"=NA, "Jul"=NA, "Aug"=NA, "Sep"=NA, "Oct"=NA, "Nov"=NA, "Dec"=NA)

temp.data.sample.count <- temp.data %>% 
  dplyr::filter(Statistical_Base == "Maximum") %>% 
  dplyr::filter(!MLocID == "TIR") %>% 
  dplyr::select(Org_Name, MLocID, StationDes, SampleStartDate, Statistical_Base, Result_Numeric) %>% 
  dplyr::mutate(date = lubridate::date(as.Date(SampleStartDate)),
                month=lubridate::month(as.Date(SampleStartDate), label=TRUE, abbr=TRUE),
                year=lubridate::year(as.Date(SampleStartDate))) %>% 
  dplyr::distinct(Org_Name, MLocID, StationDes, Statistical_Base, Result_Numeric, date, .keep_all=TRUE) %>% 
  dplyr::group_by(MLocID, StationDes, Statistical_Base, year, month) %>%
  dplyr::summarize(Org_Names=paste0(unique(Org_Name), collapse=" ,"),n=n()) %>%
  dplyr::ungroup() %>%
  tidyr::pivot_wider(names_from = month, values_from=n) %>%
  dplyr::rename(`Station ID` = MLocID,
                Station = StationDes,
                Year = year) %>% 
  tibble::add_column(!!!cols[!names(cols) %in% names(.)]) %>% 
  dplyr::select(Year, `Station ID`, Station, Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec) %>% 
  dplyr::distinct(Year, `Station ID`,.keep_all=TRUE) %>% 
  dplyr::arrange(Year, `Station ID`)

# _ Flow data ----
## _ (1) USGS ----
station.usgs.flow <- usgs.flow.stations %>%  # Discharge [ft3/s]
  dplyr::filter(!(site_tp_cd %in% c("SP","GW"))) %>% # ST = Stream
  dplyr::filter(data_type_cd %in% c("dv", "id", "iv")) %>% # dv=daily values; id=historical instantaneous values; iv=instantaneous values
  dplyr::mutate(lat = dec_lat_va,
                long = dec_long_va) %>%
  sf::st_as_sf(coords = c("long", "lat"), crs = sf::st_crs("+init=EPSG:4269")) %>% 
  dplyr::filter(sf::st_intersects(pro_area_huc12_union, ., sparse = FALSE)) %>% 
  sf::st_drop_geometry() %>% 
  dplyr::filter(!is.na(dec_lat_va)) %>% 
  dplyr::filter(!site_no %in% station.owrd$`Station ID`) %>% 
  dplyr::distinct(site_no,.keep_all=TRUE)
station.usgs.flow <- station.usgs.flow %>% 
  dplyr::select(`Data Source` = agency_cd, 
                `Station ID` = site_no, 
                `Station` = station_nm, 
                `Lat` = dec_lat_va, 
                `Long` = dec_long_va)

usgs.data.flow <- usgs.fl.data %>% 
  dplyr::filter(site_no %in% station.usgs.flow$`Station ID`) %>% 
  dplyr::select(`Data Source` = agency_cd,
                `Station ID` = site_no,
                dateTime,
                Result = X_00060_00003)

## _ (2) OWRD ----
owrd.data.flow <- owrd.data %>% 
  dplyr::filter(Char_Name %in% c("mean_daily_flow_cfs")) %>% 
  dplyr::filter(MLocID %in% station.owrd$`Station ID`) %>% 
  dplyr::select(`Station ID`= MLocID,
                dateTime = SampleStartDate,
                Result = Result_Numeric)%>% 
  dplyr::mutate(`Data Source` = "OWRD")

station.owrd.flow <- station.owrd %>% 
  dplyr::filter(`Station ID` %in% owrd.data.flow$`Station ID`) %>% 
  dplyr::rename(`Data Source` = Organization,
                Lat = Latitude,
                Long = Longitude)

## _ (3) Hydromet ----
station.hydromet.flow <- hydromet.stations %>% 
  dplyr::filter(sf::st_intersects(pro_area_huc12_union, ., sparse = FALSE)) %>% 
  sf::st_drop_geometry() %>% 
  dplyr::mutate(`Data Source`= "Hydromet") %>% 
  dplyr::rename(`Station ID` = Station.ID,
                Station = Station.Name,
                Lat = lat,
                Long = long) %>% 
  dplyr::select(`Data Source`,`Station ID`,Station,Lat,Long) %>% 
  dplyr::distinct(`Station ID`, .keep_all = TRUE)

hydromet.data.flow <- hydromet.data %>%
  dplyr::filter(`Station ID` %in% station.hydromet.flow$`Station ID`)

station.hydromet.flow <- station.hydromet.flow %>% 
  dplyr::filter(`Station ID` %in% hydromet.data.flow$`Station ID`)
## _ (4) USGS + OWRD + Hydromet ----
flow.stations <- rbind(station.usgs.flow, station.owrd.flow,station.hydromet.flow) %>% 
  dplyr::distinct(`Station ID`,.keep_all=TRUE) %>% 
  dplyr::mutate(Station = stringr::str_to_title(Station)) %>% 
  dplyr::mutate_at("Station", str_replace_all, "Or", "OR") %>% 
  dplyr::mutate_at("Station", str_replace_all, "OReg.", "OR") %>% 
  dplyr::mutate_at("Station", str_replace_all, "Ordeq", "ORDEQ") %>%
  dplyr::mutate_at("Station", str_replace_all, "ORegon", "Oregon") %>% 
  dplyr::mutate_at("Station", str_replace_all, " Rm", " RM") %>%
  dplyr::mutate_at("Station", str_replace_all, "Lb", "LB") %>%
  dplyr::mutate_at("Station", str_replace_all, "Nf", "NF") %>%
  dplyr::mutate_at("Station", str_replace_all, "Sf", "SF")
#dplyr::mutate_at("Station", str_replace_all, " R ", " RIVER ") %>% 
#dplyr::mutate_at("Station", str_replace_all, " @ ", " AT ") %>% 
#dplyr::mutate_at("Station", str_replace_all, " & ", " AND ") %>% 
#dplyr::mutate_at("Station", str_replace_all, " CK ", " CREEK ") %>% 
#dplyr::mutate_at("Station", str_replace_all, " CR ", " CREEK ") %>% 
#dplyr::mutate_at("Station", str_replace_all, " NR ", " NEAR ") %>% 
#dplyr::mutate_at("Station", str_replace_all, "N\\.", "NORTH ") %>% 
#dplyr::mutate_at("Station", str_replace_all, "N ", "NORTH ") %>% 
#dplyr::mutate_at("Station", str_replace_all, " ABV ", " ABOVE ") %>% 
#dplyr::mutate_at("Station", str_replace_all, " BLW ", " BELOW ") %>% 
#dplyr::mutate_at("Station", str_replace_all, " P ", " POWER ") %>% 
#dplyr::mutate_at("Station", str_replace_all, " CA ", " CANAL ") %>% 
#dplyr::mutate_at("Station", str_replace_all, "OREG", "OR") %>% 
#dplyr::mutate_at("Station", str_replace_all, "\\.", "") %>% 
#dplyr::mutate_at("Station", str_replace_all, "T FLS", "TOKETEE FALLS") %>% 
#dplyr::mutate_at("Station", str_replace_all, ",OR", ", OR") %>% 
#dplyr::mutate_at("Station", str_replace_all, ", OR", "") %>% 
#dplyr::mutate_at("Station", str_replace_all, " OR", "") %>% 
#dplyr::mutate(`Station` = stringr::str_to_title(`Station`)) %>% 
#dplyr::mutate_at("Station", str_replace_all, "So", "S") %>% 
#dplyr::mutate_at("Station", str_replace_all, "No", "N") %>% 
#dplyr::mutate_at("Station", str_replace_all, "Nrth", "North") %>% 
#dplyr::mutate_at("Station", str_replace_all, "Suth", "South")

flow.data <- rbind(usgs.data.flow,owrd.data.flow,hydromet.data.flow)

# Flow data.sample.count will be used in the Appendix B
flow.data.sample.count <- flow.data %>% 
  dplyr::mutate(date = lubridate::date(dateTime),
                month=lubridate::month(dateTime, label=TRUE, abbr=TRUE),
                year=lubridate::year(dateTime)) %>%
  dplyr::distinct(`Station ID`,Result,date, .keep_all=TRUE) %>% 
  dplyr::group_by(`Station ID`, year, month) %>%
  dplyr::summarize(n=n()) %>%
  dplyr::ungroup() %>%
  tidyr::pivot_wider(names_from = month, values_from=n) %>%
  dplyr::left_join(flow.stations[,c("Station ID", "Station")], by="Station ID") %>%
  dplyr::rename(Year = year) %>% 
  dplyr::select(Year, `Station ID`, Station, Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec) %>% 
  dplyr::arrange(Year, `Station ID`) %>% 
  dplyr::distinct(Year, `Station ID`, .keep_all=TRUE)

## _ (5) Instantaneous flow ----
inst.flow.pro.area <- inst.flow %>% 
  dplyr::filter(`Project Area` == qapp_project_area)

# _ Effective shade data ----
effective.shade.pro.area <- effective.shade %>% 
  dplyr::filter(`Project Area` == qapp_project_area)

# _ Gage Height data ----
# Gage height or water level data are for Willamette mainstem only.
load(paste0(data.dir,"/download/usgs_wl.RData")) # usgs.gh.stations & usgs.gh.data
station.usgs.gh <- usgs.gh.stations %>% 
  dplyr::filter(site_no %in% usgs.gh.data$site_no) %>% # filter out the stations that have data beyond the period of 2010-2020
  dplyr::filter(!(site_tp_cd %in% c("SP","GW"))) %>%
  dplyr::mutate(lat = dec_lat_va,
                long = dec_long_va) %>%
  sf::st_as_sf(coords = c("long", "lat"), crs = sf::st_crs("+init=EPSG:4269")) %>% 
  dplyr::filter(sf::st_intersects(pro_area_huc12_union, ., sparse = FALSE)) %>% 
  sf::st_drop_geometry() %>% 
  dplyr::select(`Data Source` = agency_cd, 
                `Station ID` = site_no, 
                `Station` = station_nm, 
                `Lat` = dec_lat_va, 
                `Long` = dec_long_va) %>% 
  dplyr::distinct(`Station ID`,.keep_all=TRUE) %>% 
  dplyr::mutate(`Station` = stringr::str_to_title(`Station`)) %>% 
  dplyr::mutate_at("Station", str_replace_all, " Or", " OR") %>% 
  dplyr::mutate_at("Station", str_replace_all, "ORegon", " Oregon") %>% 
  dplyr::mutate_at("Station", str_replace_all, ", Wa", ", WA") %>% 
  dplyr::mutate_at("Station", str_replace_all, " Cr ", " Creek ") %>% 
  dplyr::mutate_at("Station", str_replace_all, " Crk ", " Creek ") %>% 
  dplyr::mutate_at("Station", str_replace_all, " R ", " River ") %>% 
  dplyr::mutate_at("Station", str_replace_all, " Nr ", " near ") %>% 
  dplyr::mutate_at("Station", str_replace_all, " At ", " at ") %>% 
  dplyr::mutate_at("Station", str_replace_all, " Below ", " below ") %>% 
  dplyr::mutate_at("Station", str_replace_all, " Blw ", " below ") %>% 
  dplyr::mutate_at("Station", str_replace_all, " Above ", " above ")

usgs.data.gh <- usgs.gh.data %>% 
  dplyr::filter(site_no %in% station.usgs.gh$`Station ID`) %>% 
  dplyr::select(`Data Source` = agency_cd,
                `Station ID` = site_no,
                dateTime,
                Result = X_00065_00003)

# gage.height.data.sample.count will be used in the Appendix C
gh.data.sample.count <- usgs.data.gh %>% 
  dplyr::mutate(date = lubridate::date(dateTime),
                month=lubridate::month(dateTime, label=TRUE, abbr=TRUE),
                year=lubridate::year(dateTime)) %>% 
  dplyr::mutate(date = as.Date(dateTime)) %>% 
  dplyr::distinct(`Data Source`,`Station ID`,Result,date, .keep_all=TRUE) %>% 
  dplyr::group_by(`Station ID`, year, month) %>%
  dplyr::summarize(n=n()) %>%
  dplyr::ungroup() %>%
  tidyr::pivot_wider(names_from = month, values_from=n) %>%
  dplyr::left_join(station.usgs.gh[,c("Station ID", "Station")], by="Station ID") %>% 
  dplyr::rename(Year = year) %>% 
  dplyr::select(Year, `Station ID`, Station, Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec) %>% 
  dplyr::arrange(Year, `Station ID`) %>% 
  dplyr::distinct(Year, `Station ID`,.keep_all=TRUE)

# _ Worksheet ----
model.info <- cal.model %>% 
  dplyr::filter(`QAPP Project Area` %in%  qapp_project_area)

# Model Location in W MS TT table is not numeric.
cal.input.mod.loc <- readxl::read_xlsx(paste0(data.dir, "Model_Setup_Info.xlsx"), sheet = "Calibration Inputs",col_types	
                                       = "text") %>% 
  dplyr::filter(!`QAPP Project Area` %in% "Upper Klamath and Lost Subbasins")
cal.input <- cal.input %>% 
  mutate(`Model Location` = cal.input.mod.loc$`Model Location`)
model.input  <- cal.input %>% 
  dplyr::filter(`QAPP Project Area` %in%  qapp_project_area) %>% 
  dplyr::mutate(Latitude = round(Latitude,4),
                Longitude = round(Longitude,3)) %>% 
  dplyr::left_join(station.awqms.temp[,c("Station ID", "Station", "Organization")], by="Station ID")

pro.area.tmdls <- knitr::combine_words(unique(model.info$"TMDL Document"))

# _ NCDC met data ----
ncei.stations.pro.area <- ncei.stations %>% 
  dplyr::filter(sf::st_intersects(pro_area_huc12_union, ., sparse = FALSE)) %>% 
  sf::st_drop_geometry()

ncei.station.tbl <- ncei.stations.pro.area %>%
  dplyr::mutate(STATION_NAME = ifelse(NCDC == "10009634","PORTLAND TROUTDALE AIRPORT",STATION_NAME))

# _ RAWS met data ----
raws.stations.pro.area <- raws.stations %>% 
  dplyr::filter(sf::st_intersects(pro_area_huc12_union, ., sparse = FALSE)) %>% 
  sf::st_drop_geometry()

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

raws.station.tbl <- raws.stations.pro.area %>% 
  dplyr::left_join(raws.station.data.type, by = "wrccID")

# _ USBR AgriMet ----
agrimet.stations.pro.area <- agrimet.stations.or %>% 
  dplyr::filter(sf::st_intersects(pro_area_huc12_union, ., sparse = FALSE)) %>% 
  sf::st_drop_geometry()

agrimet.station.data.type <- agrimet.parameters %>% 
  dplyr::filter(Type %in% c("Air Temperature","Precipitation", "Relative Humidity", "Wind"))
#dplyr::group_by(siteid) %>% 
#dplyr::summarise(Type = toString(sort(Type)))

agrimet.station.tbl <- agrimet.stations.pro.area %>% 
  dplyr::left_join(agrimet.station.data.type, by = "siteid")

# _ USBR Hydromet ----
hydromet.stations.pro.area <- hydromet.stations %>% 
  dplyr::filter(sf::st_intersects(pro_area_huc12_union, ., sparse = FALSE)) %>% 
  sf::st_drop_geometry()

hydromet.station.tbl <- hydromet.stations.pro.area

# _ MesoWest climate data ----
mw.stations.pro.area <- mw.stations %>% 
  dplyr::filter(sf::st_intersects(pro_area_huc12_union, ., sparse = FALSE)) %>% 
  sf::st_drop_geometry()

mw.station.tbl <- mw.stations.pro.area #%>% 
#dplyr::mutate(NAME = stringr::str_to_title(NAME)) %>% 
#dplyr::mutate_at("NAME", str_replace_all, "Cw", "CW") %>% 
#dplyr::mutate_at("NAME", str_replace_all, "Dw", "DW") %>% 
#dplyr::mutate_at("NAME", str_replace_all, "Ew", "EW") %>%
#dplyr::mutate_at("NAME", str_replace_all, "Fw", "FW") %>% 
#dplyr::mutate_at("NAME", str_replace_all, "Us26", "US26") %>% 
#dplyr::mutate_at("NAME", str_replace_all, "Us30", "US30") %>% 
#dplyr::mutate_at("NAME", str_replace_all, "Psu", "PSU") %>% 
#dplyr::mutate_at("NAME", str_replace_all, "P.g.e.", "P.G.E.") %>% 
#dplyr::mutate_at("NAME", str_replace_all, "Kgw-Tv", "KGW-TV") %>% 
#dplyr::mutate_at("NAME", str_replace_all, "Bpa", "BPA") %>% 
#dplyr::mutate_at("NAME", str_replace_all, "Or", "OR") %>% 
#dplyr::mutate_at("NAME", str_replace_all, "ese", "ESE") %>% 
#dplyr::mutate_at("NAME", str_replace_all, "es", "SE") %>%
#dplyr::mutate_at("NAME", str_replace_all, "Sw", "SW") %>% 
#dplyr::mutate_at("NAME", str_replace_all, "Mp", "MP") %>% 
#dplyr::mutate_at("NAME", str_replace_all, "Nf", "NF") %>% 
#dplyr::mutate_at("NAME", str_replace_all, "Se", "SE") %>% 
#dplyr::mutate_at("NAME", str_replace_all, "se", "SE") %>% 
#dplyr::mutate_at("NAME", str_replace_all, "sse", "SSE")

# _ NPEDES ----
npdes.ind.pro.area <- npdes.ind %>% 
  dplyr::filter(`Project Area` == qapp_project_area)
#dplyr::filter(!is.na(`WQ File Nbr`)) %>% 
#dplyr::filter(!is.na(Latitude)) %>% 
#dplyr::mutate(lat = Latitude, long = Longitude) %>% 
#sf::st_as_sf(coords = c("long", "lat"), crs = sf::st_crs("+init=EPSG:4269")) %>% 
#dplyr::filter(sf::st_intersects(pro_area_huc12_union, ., sparse = FALSE)) %>% 
#sf::st_drop_geometry()

npdes.gen.pro.area <- npdes.gen %>% 
  dplyr::filter(`Project Area` == qapp_project_area)
#dplyr::filter(!is.na(`WQ File Nbr`)) %>% 
#dplyr::filter(!is.na(Latitude)) %>% 
#dplyr::mutate(lat = Latitude, long = Longitude) %>% 
#sf::st_as_sf(coords = c("long", "lat"), crs = sf::st_crs("+init=EPSG:4269")) %>% 
#dplyr::filter(sf::st_intersects(pro_area_huc12_union, ., sparse = FALSE)) %>% 
#sf::st_drop_geometry()

# _ NLCD ----
nlcd.pro.area <- nlcd.tbl %>% 
  dplyr::ungroup() %>% 
  dplyr::filter(Project_Na == qapp_project_area) %>% 
  dplyr::mutate(Acres = ifelse(Acres == 0.0,"<0.05",Acres)) %>% 
  dplyr::mutate(Percentage = ifelse(Percentage == 0.0,"<0.05",Percentage)) %>% 
  dplyr::arrange(desc(as.numeric(Acres))) %>% 
  dplyr::mutate(NLCD_Land = ifelse(is.na(NLCD_Land), "Open Water",NLCD_Land)) %>% 
  tidyr::drop_na(Stream)
nlcd.text.pro.area <- nlcd.text %>% 
  dplyr::filter(Project_Na == qapp_project_area) %>% 
  dplyr::mutate(text = ifelse(text=="NA", "Open Water",text)) %>% 
  tidyr::drop_na(Stream)
# _ DMA ----
dma.pro.area <- dma.tbl %>% 
  dplyr::ungroup() %>% 
  dplyr::filter(Project_Na == qapp_project_area) %>% 
  dplyr::mutate(Acres = ifelse(Acres == 0.0,"<0.05",Acres)) %>% 
  dplyr::mutate(Percentage = ifelse(Percentage == 0.0,"<0.05",Percentage))%>% 
  dplyr::arrange(desc(as.numeric(Acres)))
# _ Save Data ----
save(df.stations,
     tir,
     ref,
     roles,
     risks,
     abbr,
     data.gap,
     effective.shade.lookup,
     rev,
     lookup.huc,
     project.areas,
     qapp_project_area,    
     temp.stations,
     temp.data.sample.count,
     model.info,
     model.input,
     pro.area.tmdls,
     pro.cat.45.tbl,
     flow.stations,
     flow.data.sample.count,
     inst.flow.pro.area,
     effective.shade.pro.area,
     station.usgs.gh,
     gh.data.sample.count,
     ncei.station.tbl,
     raws.station.tbl,
     agrimet.station.tbl,
     hydromet.station.tbl,
     mw.station.tbl,
     npdes.ind.pro.area,
     npdes.gen.pro.area,
     nlcd.pro.area,
     nlcd.text.pro.area,
     dma.pro.area,
     strip_alpha,
     strip_tbl_num,
     s,
     is.are,
     numbers.to.words,
     file = paste0("./data/",file.name,".RData"))

# _ Data output to Excel ----
station.output.temp <- temp.stations %>% 
  dplyr::mutate(Data = "Temp",
                Track = "TRUE") %>% 
  dplyr::select(`Station ID`,Station,Latitude,Longitude,Organization,Data,Track)

station.output.flow <- flow.stations %>% 
  dplyr::rename(Organization = `Data Source`,
                Latitude = Lat,
                Longitude = Long) %>% 
  dplyr::mutate(Data = "Flow",
                Track = "TRUE") %>% 
  dplyr::select(`Station ID`,Station,Latitude,Longitude,Organization,Data,Track)

station.output.gage <- station.usgs.gh %>% 
  dplyr::rename(Organization = `Data Source`,
                Latitude = Lat,
                Longitude = Long) %>% 
  dplyr::mutate(Data = "Gage Height",
                Track = "TRUE") %>% 
  dplyr::select(`Station ID`,Station,Latitude,Longitude,Organization,Data,Track)

station.worksheet.temp <- model.input %>% 
  dplyr::filter(`Parameter` %in%  c("Water Temperature")) %>% 
  dplyr::filter(!is.na(`Data Source`)) %>% 
  dplyr::filter(is.na(`Interpolated Data`)) %>% 
  dplyr::filter(!`Station ID` == "TIR") %>% 
  dplyr::mutate(Station = ifelse(is.na(Station),`Model Location Name`,Station),
                Organization = ifelse(is.na(Organization),`Data Source`,Organization),
                Organization = ifelse(substr(Organization,1,18) == "Watershed Sciences",
                                      paste0(gsub(",.*$", "", Organization)," (",stringi::stri_sub(strip_alpha(Organization),-4),")"),Organization),
                Data = "Temp_Worksheet",
                Track = "TRUE") %>% 
  dplyr::distinct(Station, .keep_all = TRUE) %>% 
  dplyr::select(`Station ID`,Station,Latitude,Longitude,Organization,Data,Track)

station.worksheet.flow <- model.input %>% 
  dplyr::filter(`Parameter` %in% c("Flow")) %>%
  dplyr::filter(!`Model Location Type` == "Point of Diversion") %>% 
  dplyr::filter(!`Data Source` == "USGS") %>% 
  dplyr::filter(!is.na(`Data Source`)) %>% 
  dplyr::filter(is.na(`Interpolated Data`)) %>% 
  dplyr::mutate(Station = `Model Location Name`,
                Organization = `Data Source`,
                Data = "Flow_Worksheet",
                Track = "TRUE") %>% 
  dplyr::distinct(Station, .keep_all = TRUE) %>% 
  dplyr::select(`Station ID`,Station,Latitude,Longitude,Organization,Data,Track)

station.output <- rbind(station.output.temp,
                        station.output.flow,
                        station.worksheet.temp,
                        station.worksheet.flow,
                        station.output.gage) %>% 
  tidyr::pivot_wider(names_from = Data, values_from = Track)

writexl::write_xlsx(list(Temp= temp.data.sample.count, 
                         Flow = flow.data.sample.count,
                         GageHeight = gh.data.sample.count,
                         Stations = station.output),
                    path=paste0(data.dir,"appendix_data/",file.name,"_appendix_data.xlsx"))

# Leaflet Map Data ----
library(tidyverse)
library(httr)
library(geojsonsf)
library(sf)

data.dir <- "//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/model_QAPPs/R/data/"
load(paste0("./data/lookup.RData"))

qapp_project_area <- "Willamette River Mainstem and Major Tributaries"
pro_area_huc12 <- sf::read_sf(dsn = "//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/GIS/willa_snake/TempTMDL_QAPP_Reaches_data_query_HUC12s.shp",
                              layer = "TempTMDL_QAPP_Reaches_data_query_HUC12s") %>% 
  dplyr::filter(Project_Na == qapp_project_area) %>% 
  sf::st_transform(4326) #4326 4269

pro_area_huc12_union <- sf::st_union(pro_area_huc12)
pro_area_lines <- sf::st_cast(pro_area_huc12_union,"LINESTRING") # clean the lines in the polygon
pro_area_lines <- sf::st_transform(pro_area_lines, 4326)
pro_area_lines_wms <- pro_area_lines[[1]]
pro_area <- st_polygonize(pro_area_lines_wms)
pro_area <- pro_area[[1]]

pro_reaches <- sf::st_read(dsn = "//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/GIS/willa_snake/TempTMDL_QAPP_Reaches.shp",
                           layer = "TempTMDL_QAPP_Reaches") %>% 
  dplyr::filter(Project_Na == qapp_project_area) %>% 
  sf::st_transform(4326) %>% 
  sf::st_zm()

# _ Model Extents ----
map_ce_model_extent <- sf::st_read(dsn = paste0(data.dir, "gis/ce_model_extent_Willamette.shp"),
                                   layer = "ce_model_extent_Willamette")%>% 
  sf::st_transform(4326) %>% 
  sf::st_zm()

ce_model_extent <- map_ce_model_extent %>% 
  dplyr::filter(Project_Na == qapp_project_area)

file.name <- project.areas[which(project.areas$areas == qapp_project_area),]$file.name

hs_temp_model_extent <- data.frame()
hs_solar_model_extent <- data.frame()
hs_solar_model_area <- data.frame()
sh_model_extent <- data.frame()

save(pro_area,
     pro_reaches,
     hs_temp_model_extent,
     hs_solar_model_extent,
     hs_solar_model_area,
     ce_model_extent,
     sh_model_extent,
     gh.data.sample.count,
     pro.cat.45.tbl,
     file = paste0("./data/map_",file.name,".RData"))
