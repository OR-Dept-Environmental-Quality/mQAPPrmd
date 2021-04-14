# ____master____ ----
# Install and/or load packages ----
packages.cran = c("tidyverse","readxl","rgdal","sf","ggplot2","tigris","devtools","lubridate")

package.check.cran <- lapply(
  packages.cran,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)
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
# _ AWQMS data ----
# Update date: 2021-2-22
load("//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/data/R/statewide/df_awqms_raw_state.RData") # df.awqms.raw.state
load("//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/data/R/statewide/df_stations_state.RData") # df.stations.state
load("//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/data/R/statewide/df_stations_complete.RData") # df.stations

df.stations.state <- df.stations.state %>% 
  dplyr::filter(MLocID %in% df.awqms.raw.state$MLocID) # filter out the stations that have data beyond the period of 1990-2020

# _ TMDL solicitation stations and data ----
solic.stations <- readxl::read_xlsx("//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/Data Solicitation/FINAL_Reviewed_Submissions/MLocs_ALL_Waves_FINAL.xlsx", 
                                    sheet = "Monitoring_Locations") %>% 
  #dplyr::filter(Stations.DB.Status %in% c("New")) %>% 
  dplyr::select(-c(Stations.DB.Status,Alternate.Context.2,Alternate.Context.3,Alternate.ID.2,Alternate.ID.3,
                   Monitoring.Location.Description,Monitoring.Location.Status.Comment,Monitoring.Location.Status.ID)) %>% 
  dplyr::rename(AltLocName = Alternate.Context.1,
                AltLocID = Alternate.ID.1,
                CollMethod = Coordinate.Collection.Method,
                COUNTY = County.Name,
                Created_Date = Date.Established,
                Datum = Horizontal.Datum,
                HUC8 = HUC.8.Code,
                Lat_DD = Latitude,
                Long_DD = Longitude,
                Comments = Monitoring.Location.Comments,
                MLocID = Monitoring.Location.ID,
                StationDes = Monitoring.Location.Name,
                MonLocType = Monitoring.Location.Type,
                OrgID = Organization,
                Permanent_Identifier = Permanent.Identifier,
                Reachcode = Reachcode,
                RiverMile = River.Mile,
                MapScale = Source.Map.Scale,
                STATE = State.Code,
                TribalLand = Tribal.Land,
                TribalName = Tribal.Land.Name) %>% 
  dplyr::mutate(AU_ID = NA,
                BacteriaCode = NA,
                DO_code = NA,
                DO_SpawnCode = NA,
                FishCode = NA,
                pH_code = NA,
                SpawnCode = NA,
                ben_use_code = NA,
                GNIS_Name = NA,
                HUC10 = NA,
                HUC10_Name = NA,
                HUC12 = NA,
                HUC12_Name = NA,
                HUC4_Name = NA,
                HUC6_Name = NA,
                HUC8_Name = NA,
                Source = "solic")
load("//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/Data Solicitation/FINAL_Reviewed_Submissions/Wave4_2021-03-29/Wave4_2021-03-29_SumStats.RData")
solic.data <- sumstats %>% 
  dplyr::filter(ResultStatusID == "Final") %>% 
  dplyr::select(Monitoring.Location.ID,Project,Result,ResultStatusID,ActStartDate,StatisticalBasis,charID,r_units,RsltType,RsltTimeBasis,
                ActivityType,SmplColMthd,SmplDepth,SmplDepthUnit,ActStartTime,ActStartTimeZone,Result.Analytical.Method.ID,cmnt) %>% 
  dplyr::rename(MLocID = Monitoring.Location.ID,
                Project1 = Project,
                Result_Numeric = Result,
                Result_status = ResultStatusID,
                SampleStartDate = ActStartDate,
                Statistical_Base = StatisticalBasis,
                Char_Name = charID,
                Result_Unit = r_units,
                Result_Type = RsltType,
                Time_Basis = RsltTimeBasis,
                Activity_Type = ActivityType,
                SamplingMethod = SmplColMthd,
                Result_Depth = SmplDepth,
                Result_Depth_Unit = SmplDepthUnit,
                SampleStartTime = ActStartTime,
                SampleStartTZ = ActStartTimeZone,
                Method_Code = Result.Analytical.Method.ID,
                Result_Comment = cmnt) %>% 
  dplyr::mutate(Org_Name = NA,
                QualifierAbbr = NA,
                HUC10 = NA,
                HUC12 = NA,
                HUC8_Name = NA,
                HUC12_Name = NA,
                Lat_DD = NA,
                Long_DD = NA,
                OrganizationID = NA,
                Result_Operator = NA,
                StationDes = NA,
                MonLocType = NA,
                AU_ID = NA,
                Measure = NA,
                Reachcode = NA,
                Source = "solic") %>% 
  dplyr::filter(Statistical_Base == "Daily Maximum") %>%  # match AWQMS Statistical_Base == "Maximum"
  dplyr::mutate(Statistical_Base = "Maximum") %>% 
  dplyr::left_join(solic.stations[,c("HUC8","MLocID")], by = "MLocID")

# _ AWQMS + Solicitation data ----
temp.data.sum <- df.awqms.raw.state %>% 
  dplyr::select(HUC8,MLocID,Org_Name,Project1,QualifierAbbr,Result_Numeric,Result_status,SampleStartDate,Statistical_Base,
                Char_Name,Result_Unit,Result_Type,Time_Basis,Activity_Type,SamplingMethod,Result_Depth,Result_Depth_Unit,
                SampleStartTime,SampleStartTZ,Method_Code,Result_Comment,HUC10,HUC12,HUC8_Name,HUC12_Name,Lat_DD,Long_DD,
                OrganizationID,Result_Operator,StationDes,MonLocType,AU_ID,Measure,Reachcode) %>% 
  dplyr::mutate(Source = "awqms") %>% 
  # AWQMS QA/QC check:
  dplyr::filter(Result_status %in% c("Final", "Provisional") | QualifierAbbr %in% c("DQL=A","DQL=B","DQL=E")) %>% 
  dplyr::filter(!Project1 %in% c("TMDL Data Submission")) %>%
  rbind(solic.data)

temp.stations.sum <- df.stations.state %>% 
  dplyr::select(AltLocID,AltLocName,AU_ID,BacteriaCode,ben_use_code,CollMethod,Comments,COUNTY,Created_Date,
                Datum,DO_code,DO_SpawnCode,FishCode,GNIS_Name,HUC10,HUC10_Name,HUC12,HUC12_Name,HUC4_Name,
                HUC6_Name,HUC8,HUC8_Name,Lat_DD,LLID,Long_DD,MapScale,Measure,MLocID,MonLocType,OrgID,
                Permanent_Identifier,pH_code,Reachcode,RiverMile,SpawnCode,STATE,StationDes,TribalLand,
                TribalName) %>%
  dplyr::mutate(Source = "awqms") %>% 
  rbind(solic.stations) %>% 
  dplyr::distinct(MLocID, .keep_all = TRUE)

# _ * data.dir ----
data.dir <- "//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/model_QAPPs/R/data/"
# _ USGS flow data ----
load(paste0(data.dir,"/download/usgs.RData")) # usgs.stations.or & usgs.data.or
usgs.stations <- usgs.stations.or %>% 
  dplyr::filter(site_no %in% usgs.data.or$site_no) # filter out the stations that have data beyond the period of 1990-2020
# _ OWRD data ----
load(paste0(data.dir,"/download/owrd.RData")) # owrd.stations.or & owrd.data.or
owrd.data <- owrd.data.or %>% 
  dplyr::filter(!published_status %in% c("Missing")) %>% 
  tidyr::separate(record_date, sep = "-", into = c("month","day","year")) %>% 
  dplyr::mutate(record_date = ymd(paste(year,month,day,sep="-"))) %>% 
  dplyr::select(-c(month,day,year)) %>% 
  dplyr::select(Char_Name = Characteristic.Name,
                Result_status = published_status,
                Result_Numeric = Result.Value,
                MLocID = station_nbr,
                SampleStartDate = record_date) %>% 
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
                StationDes = NA,
                Statistical_Base = NA,
                Time_Basis = NA)

owrd.stations <- owrd.stations.or %>% 
  dplyr::filter(station_nbr %in% owrd.data$MLocID) # filter out the stations that have data beyond the period of 1990-2020

# _ Worksheet ----
cal.model <- readxl::read_xlsx(paste0(data.dir, "Model_Setup_Info.xlsx"), sheet = "Calibration Model Setup Info") %>% 
  dplyr::filter(!`QAPP Project Area` %in%  c("Upper Klamath and Lost Subbasins")) %>% 
  # for Section 6.1 General Model Parameters
  dplyr::mutate(Model_version = ifelse(substr(`Model version`, 13,13) == "6", "Heat Source Version 6",
                                       ifelse(substr(`Model version`, 13,13) == "7", "Heat Source Version 7",
                                              ifelse(substr(`Model version`, 13,13) == "8", "Heat Source Version 8",
                                                     ifelse(substr(`Model version`, 13,13) == "9", "Heat Source Version 9",
                                                            ifelse(substr(`Model version`, 1,2) == "CE", "CE-QUAL-W2 Version 3","SHADOW")))))) %>% 
  dplyr::mutate(mod_rmd = ifelse(Model_version == "Heat Source Version 6", "hs6",
                                 ifelse(Model_version == "Heat Source Version 7", "hs7",
                                        ifelse(Model_version == "Heat Source Version 8", "hs8",
                                               ifelse(Model_version == "Heat Source Version 9", "hs9",
                                                      ifelse(Model_version == "CE-QUAL-W2 Version 3", "ce",
                                                             "sh")))))) %>% 
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
abbr <- readxl::read_xlsx("//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/model_QAPPs/R/data/tables.xlsx",sheet = "abbr")
data.gap <- readxl::read_xlsx(paste0(data.dir,"tables.xlsx"),sheet = "data_gap")
npdes.ind <- readxl::read_xlsx(paste0(data.dir, "NPDES_communication_list.xlsx"), sheet = "Individual_NDPES") %>% 
  dplyr::mutate(`Common Name` = stringr::str_to_title(`Common Name`)) %>%
  dplyr::mutate_at("Common Name", str_replace_all, "Stp", "STP") %>%
  dplyr::mutate_at("Common Name", str_replace_all, "Wrf", "WRF") %>%
  dplyr::mutate_at("Common Name", str_replace_all, "wrf", "WRF") %>%
  dplyr::mutate_at("Common Name", str_replace_all, "Ati ", "ATI ") %>%
  dplyr::mutate_at("Common Name", str_replace_all, "Bdc/", "BDC/") %>%
  dplyr::mutate_at("Common Name", str_replace_all, "cmss", "CMSS") %>%
  dplyr::mutate_at("Common Name", str_replace_all, "Ms4", "MS4") %>%
  dplyr::mutate_at("Common Name", str_replace_all, "Mwmc", "MWMC") %>%
  dplyr::mutate_at("Common Name", str_replace_all, "Odc", "ODC") %>%
  dplyr::mutate_at("Common Name", str_replace_all, "Odfw", "ODFW") %>%
  dplyr::mutate_at("Common Name", str_replace_all, "Odot", "ODOT") %>%
  dplyr::mutate_at("Common Name", str_replace_all, "Ohsu", "OHSU") %>%
  dplyr::mutate_at("Common Name", str_replace_all, "Wwtp", "WWTP") %>%
  dplyr::mutate_at("Common Name", str_replace_all, "Wes ", "WES ") %>%
  dplyr::mutate_at("Common Name", str_replace_all, "Slli", "SLLI") %>%
  dplyr::mutate_at("Common Name", str_replace_all, "Usa", "USA") %>% 
  dplyr::mutate_at("Common Name", str_replace_all, "Usfs", "USFS")
npdes.gen <- readxl::read_xlsx(paste0(data.dir, "NPDES_communication_list.xlsx"), sheet = "Gen_NPDES")
lookup.huc <- readxl::read_xlsx(paste0(data.dir, "Lookup_QAPPProjectArea.xlsx"), sheet = "Lookup_QAPPProjectArea")
project.areas <- read.csv(paste0(data.dir,"qapp_project_areas.csv")) %>% 
  dplyr::left_join(schedule, by=c("areas"="QAPP Project Area"))

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

colum_auid <- sf::st_read(dsn = "//deqhq1/TMDL/Planning statewide/2020_2024",
                          layer="Columbia_River_AU_IDs",
                          stringsAsFactors=FALSE) %>% 
  sf::st_drop_geometry() %>%
  dplyr::distinct(AU_ID) %>%
  dplyr::pull(AU_ID) 

cat.45.tbl <- sf::st_drop_geometry(cat.45) %>% 
  dplyr::filter(!AU_ID %in% c(colum_auid)) %>%
  dplyr::mutate_at("AU_Name", str_replace_all, "R\\*", "River") %>%
  dplyr::mutate_at("AU_Name", str_replace_all, "Ri\\*", "River") %>%
  dplyr::mutate_at("AU_Name", str_replace_all, "Riv\\*", "River") %>% 
  dplyr::mutate_at("AU_Name", str_replace_all, "For\\*", "Fork Willamette River") %>%
  dplyr::mutate_at("AU_Name", str_replace_all, "Cre\\*", "Creek") %>%
  dplyr::mutate_at("AU_Name", str_replace_all, "Willamet\\*", "Willamette River") %>%
  dplyr::mutate_at("AU_Name", str_replace_all, "Mill\\*", "Mill Creek") %>%
  dplyr::mutate_at("AU_Name", str_replace_all, "W\\*", "Willamette River") %>%
  dplyr::mutate_at("AU_Name", str_replace_all, "Joh\\*", "John Day River") %>% 
  dplyr::mutate_at("AU_Name", str_replace_all, "John\\*", "John Day River") %>% 
  dplyr::mutate_at("AU_Name", str_replace_all, "Willamett\\*", "Willamette River") %>% 
  dplyr::mutate_at("AU_Name", str_replace_all, "Willamette \\*", "Willamette River") %>% 
  dplyr::mutate_at("AU_Name", str_replace_all, "McKenzie \\*", "McKenzie River") %>% 
  dplyr::mutate_at("AU_Name", str_replace_all, "Thunder Creek-North Unpqua River", "Thunder Creek-North Umpqua River") %>% 
  dplyr::distinct(AU_ID, .keep_all = TRUE)

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

# _ USBR AgriMet ----
agrimet.stations <- read.csv(paste0(data.dir, "download/agrimet_stations.csv"))
agrimet.parameters <- read.csv(paste0(data.dir, "download/agrimet_parameters.csv"))
agrimet.stations.or <- agrimet.stations %>%
  dplyr::filter(state == "OR") %>% 
  dplyr::mutate(lat = latitude, long = longitude) %>% 
  sf::st_as_sf(coords = c("longitude", "latitude"), crs = sf::st_crs("+init=EPSG:4269"))

# _ USBR Hydromet ----
hydromet <- read.csv(paste0(data.dir, "download/hydromet.csv"))
hydromet.stations <- hydromet %>% 
  dplyr::mutate(lat = Lat, long = Long) %>% 
  sf::st_as_sf(coords = c("Long", "Lat"), crs = sf::st_crs("+init=EPSG:4269"))

# _ MesoWest climate data ----
load(paste0(data.dir,"/download/mw.RData")) # mw.meta & mw.variables.list
mw.stations <- mw.meta$STATION %>%
  dplyr::mutate(lat = LATITUDE, long = LONGITUDE) %>% 
  sf::st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = sf::st_crs("+init=EPSG:4269"))

# _ Project areas and HUCs ----
pro_areas <- sf::st_read(dsn = "//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/model_QAPPs/R/data/gis/project_areas.shp",
                         layer = "project_areas")

pro_areas_huc8 <- sf::st_read(dsn = "//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/GIS/Study_Areas_v5_HUC8_scope.shp",
                              layer = "Study_Areas_v5_HUC8_scope")

# QAPP Project Area Data ----

## for test:
# qapp_project_area = "John Day River Basin"
# qapp_project_area = "Lower Grande Ronde, Imnaha, and Wallowa Subbasins"
# qapp_project_area = "Lower Willamette and Clackamas Subbasins"
# qapp_project_area = "Malheur River Subbasins"
# qapp_project_area = "Mid Willamette Subbasins"
# qapp_project_area = "Middle Columbia-Hood, Miles Creeks"
# qapp_project_area = "North Umpqua Subbasin"
# qapp_project_area = "Rogue River Basin"
# qapp_project_area = "Sandy Subbasin"
# qapp_project_area = "South Umpqua and Umpqua Subbasins"
# qapp_project_area = "Southern Willamette Subbasins"
# qapp_project_area = "Walla Walla Subbasin"
# qapp_project_area = "Willow Creek Subbasin"

for (qapp_project_area in project.areas[which(!project.areas$areas == "Willamette River Mainstem and Major Tributaries"),]$areas) {
  
  huc8.extent <- project.areas[which(project.areas$areas == qapp_project_area),]$huc8.extent
  file.name <- project.areas[which(project.areas$areas == qapp_project_area),]$file.name
  
  subbasin <- unique(lookup.huc[which(lookup.huc$QAPP_Project_Area == qapp_project_area),]$HUC8_NAME)
  subbasin_num <- unique(lookup.huc[which(lookup.huc$QAPP_Project_Area == qapp_project_area),]$HUC_8)
  
  # _ Temp data ----
  # AWQMS, Solicitation Data and OWRD Temp Data
  station.awqms <- temp.stations.sum %>% 
    dplyr::filter(HUC8 %in% subbasin_num) %>% 
    dplyr::rename(`Station ID` = MLocID)
  
  station.model <- cal.input %>% 
    dplyr::filter(`QAPP Project Area` %in%  qapp_project_area) %>% 
    dplyr::left_join(station.awqms[,c("Station ID", "StationDes", "OrgID")], by="Station ID")
  
  station.owrd <- owrd.stations %>%
    dplyr::filter(HUC8 %in% subbasin_num) %>% 
    dplyr::select(`Data Source` = Operator,
                  `Station ID` = station_nbr,
                  `Station` = station_name,
                  Lat,
                  Long) %>% 
    dplyr::distinct(`Station ID`,.keep_all=TRUE)
  
  owrd.data.temp <- owrd.data %>% 
    dplyr::filter(Char_Name %in% c("daily_max_water_temp_C")) %>% 
    dplyr::filter(MLocID %in% station.owrd$`Station ID`) %>% 
    dplyr::mutate(Statistical_Base = "Maximum") %>% 
    dplyr::mutate(Source = "owrd")
  
  station.owrd.temp <- station.owrd %>% 
    dplyr::filter(`Station ID` %in% owrd.data.temp$MLocID)
  
  temp.data <- temp.data.sum %>% 
    rbind(owrd.data.temp) %>% 
    dplyr::filter(HUC8 %in% subbasin_num)
  
  # Temp data.sample.count will be used in the Appendix A
  temp.data.sample.count <- temp.data %>% 
    dplyr::filter(Statistical_Base == "Maximum") %>% 
    dplyr::filter(!MLocID == "TIR") %>% 
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
    dplyr::left_join(station.awqms[,c("Station ID", "StationDes")], by="Station ID") %>% 
    dplyr::rename(Year = year,
                  Station = StationDes) %>% 
    dplyr::mutate(`Station` = stringr::str_to_title(`Station`)) %>% 
    dplyr::mutate_at("Station", str_replace_all, "Or", "OR") %>% 
    dplyr::select(Year, `Station ID`, Station, Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec) %>% 
    dplyr::arrange(Year, `Station ID`) %>% 
    dplyr::distinct(Year, `Station ID`,.keep_all=TRUE)
  
  model.info <- cal.model %>% 
    dplyr::filter(`QAPP Project Area` %in%  qapp_project_area)
  
  model.input  <- cal.input %>% 
    dplyr::filter(`QAPP Project Area` %in%  qapp_project_area) %>% 
    dplyr::mutate(Latitude = round(Latitude,4),
                  Longitude = round(Longitude,3))
  
  pro.area.tmdls <- knitr::combine_words(unique(model.info$"TMDL Document"))
  
  pro_area <- pro_areas %>% 
    dplyr::filter(Project_Na == qapp_project_area)
  
  pro_area_huc8 <- pro_areas_huc8 %>% 
    dplyr::filter(HUC_8 %in% subbasin_num)
  
  # _ IR2018/20 Cat 4 & 5 ----
  pro.cat.45.tbl <- cat.45.tbl %>% 
    dplyr::filter(QAPP_Project_Area %in% qapp_project_area)
  
  # _ Flow data ----
  station.usgs <- usgs.stations %>%  # Discharge [ft3/s]
    dplyr::filter(!(site_tp_cd %in% c("SP","GW"))) %>% # ST = Stream
    dplyr::filter(data_type_cd %in% c("dv", "id", "iv")) %>% # dv=daily values; id=historical instantaneous values; iv=instantaneous values
    dplyr::filter(huc_cd %in% subbasin_num) %>%
    dplyr::filter(!is.na(dec_lat_va)) %>% 
    dplyr::select(`Data Source` = agency_cd, 
                  `Station ID` = site_no, 
                  `Station` = station_nm, 
                  `Lat` = dec_lat_va, 
                  `Long` = dec_long_va) %>% 
    dplyr::distinct(`Station ID`,.keep_all=TRUE) %>% 
    dplyr::filter(!`Station ID` %in% station.owrd$`Station ID`)
  
  usgs.data <- usgs.data.or %>% 
    dplyr::filter(site_no %in% station.usgs$`Station ID`) %>% 
    dplyr::select(`Data Source` = agency_cd,
                  `Station ID` = site_no,
                  dateTime,
                  Result = X_00060_00003)
  
  owrd.data.flow <- owrd.data %>% 
    dplyr::filter(Char_Name %in% c("mean_daily_flow_cfs")) %>% 
    dplyr::filter(MLocID %in% station.owrd$`Station ID`) %>% 
    dplyr::select(`Station ID`= MLocID,
                  dateTime = SampleStartDate,
                  Result = Result_Numeric)%>% 
    dplyr::mutate(`Data Source` = "OWRD")
  
  flow.data <- rbind(usgs.data,owrd.data.flow)
  
  station.owrd.flow <- station.owrd %>% 
    dplyr::filter(`Station ID` %in% owrd.data.flow$`Station ID`)
  
  flow.stations <- rbind(station.usgs, station.owrd.flow) %>% 
    dplyr::distinct(`Station ID`,.keep_all=TRUE) %>% 
    dplyr::mutate_at("Station", str_replace_all, " R ", " RIVER ") %>% 
    dplyr::mutate_at("Station", str_replace_all, " @ ", " AT ") %>% 
    dplyr::mutate_at("Station", str_replace_all, " & ", " AND ") %>% 
    dplyr::mutate_at("Station", str_replace_all, " CK ", " CREEK ") %>% 
    dplyr::mutate_at("Station", str_replace_all, " CR ", " CREEK ") %>% 
    dplyr::mutate_at("Station", str_replace_all, " NR ", " NEAR ") %>% 
    dplyr::mutate_at("Station", str_replace_all, "N\\.", "NORTH ") %>% 
    #dplyr::mutate_at("Station", str_replace_all, "N ", "NORTH ") %>% 
    dplyr::mutate_at("Station", str_replace_all, " ABV ", " ABOVE ") %>% 
    dplyr::mutate_at("Station", str_replace_all, " BLW ", " BELOW ") %>% 
    dplyr::mutate_at("Station", str_replace_all, " P ", " POWER ") %>% 
    dplyr::mutate_at("Station", str_replace_all, " CA ", " CANAL ") %>% 
    dplyr::mutate_at("Station", str_replace_all, "OREG", "OR") %>% 
    dplyr::mutate_at("Station", str_replace_all, "\\.", "") %>% 
    dplyr::mutate_at("Station", str_replace_all, "T FLS", "TOKETEE FALLS") %>% 
    dplyr::mutate_at("Station", str_replace_all, ",OR", "") %>% 
    dplyr::mutate_at("Station", str_replace_all, ", OR", "") %>% 
    dplyr::mutate_at("Station", str_replace_all, " OR", "") %>% 
    dplyr::mutate(`Station` = stringr::str_to_title(`Station`)) %>% 
    dplyr::mutate_at("Station", str_replace_all, "So", "S") %>% 
    dplyr::mutate_at("Station", str_replace_all, "No", "N") %>% 
    dplyr::mutate_at("Station", str_replace_all, "Nrth", "North") %>% 
    dplyr::mutate_at("Station", str_replace_all, "Suth", "South")
  
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
    dplyr::distinct(Year, `Station ID`,.keep_all=TRUE)
  
  # _ NCDC met data ----
  ncei.stations.pro.area <- ncei.stations %>% 
    dplyr::filter(sf::st_contains(pro_area, ., sparse = FALSE)) %>% 
    sf::st_drop_geometry()
  
  ncei.station.tbl <- ncei.stations.pro.area %>%
    dplyr::mutate(STATION_NAME = stringr::str_to_title(STATION_NAME))
  
  # _ RAWS met data ----
  raws.stations.pro.area <- raws.stations %>% 
    filter(sf::st_contains(pro_area, ., sparse = FALSE)) %>% 
    sf::st_drop_geometry()
  
  raws.station.data.type <- raws.data.type %>% 
    dplyr::rename(Parameter = "unlist.type.list.columnNames.") %>% 
    dplyr::filter(Parameter %in% c("humidity","precipitation","temperature","windDirection","windSpeed")) %>% 
    dplyr::mutate_at("Parameter", str_replace_all, "humidity", "Humidity") %>% 
    dplyr::mutate_at("Parameter", str_replace_all, "precipitation", "Precipitation") %>%
    dplyr::mutate_at("Parameter", str_replace_all, "temperature", "Temperature") %>%
    dplyr::mutate_at("Parameter", str_replace_all, "windDirection", "Wind Direction") %>%
    dplyr::mutate_at("Parameter", str_replace_all, "windSpeed", "Wind Speed")
  
  raws.station.tbl <- raws.stations.pro.area %>%
    dplyr::left_join(raws.station.data.type, by = "wrccID")
  
  # _ USBR AgriMet ----
  agrimet.stations.pro.area <- agrimet.stations.or %>% 
    filter(sf::st_contains(pro_area, ., sparse = FALSE)) %>% 
    sf::st_drop_geometry()
  
  agrimet.station.data.type <- agrimet.parameters %>% 
    dplyr::filter(Type %in% c("Air Temperature","Precipitation", "Relative Humidity", "Wind"))
  
  agrimet.station.tbl <- agrimet.stations.pro.area %>%
    dplyr::left_join(agrimet.station.data.type, by = "siteid")
  
  # _ USBR Hydromet ----
  hydromet.stations.pro.area <- hydromet.stations %>% 
    filter(sf::st_contains(pro_area, ., sparse = FALSE)) %>% 
    sf::st_drop_geometry()
  
  hydromet.station.tbl <- hydromet.stations.pro.area
  
  # _ MesoWest climate data ----
  mw.stations.pro.area <- mw.stations %>% 
    filter(sf::st_contains(pro_area, ., sparse = FALSE)) %>% 
    sf::st_drop_geometry()
  
  mw.station.tbl <- mw.stations.pro.area %>% 
    dplyr::mutate(NAME = stringr::str_to_title(NAME))  
  #dplyr::mutate_at("NAME", str_replace_all, "Or", "OR") %>% 
  #dplyr::mutate_at("NAME", str_replace_all, "ese", "ESE") %>% 
  #dplyr::mutate_at("NAME", str_replace_all, "es", "SE") %>%
  #dplyr::mutate_at("NAME", str_replace_all, "Ew", "EW") %>%
  #dplyr::mutate_at("NAME", str_replace_all, "Sw", "SW") %>% 
  #dplyr::mutate_at("NAME", str_replace_all, "Mp", "MP") %>% 
  #dplyr::mutate_at("NAME", str_replace_all, "Nf", "NF") %>% 
  #dplyr::mutate_at("NAME", str_replace_all, "Se", "SE") %>% 
  #dplyr::mutate_at("NAME", str_replace_all, "se", "SE") %>% 
  #dplyr::mutate_at("NAME", str_replace_all, "sse", "SSE")
  
  # _ Save Data ----
  #setwd("//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/model_QAPPs/R/data/RData")
  save(df.stations,
       tir,
       ref,
       roles,
       risks,
       abbr,
       data.gap,
       npdes.ind,
       npdes.gen,
       lookup.huc,
       project.areas,
       qapp_project_area,
       station.awqms,
       station.model,
       station.owrd.temp,
       temp.data.sample.count,
       model.info,
       model.input,
       pro.area.tmdls,
       pro.cat.45.tbl,
       flow.stations,
       flow.data.sample.count,
       ncei.station.tbl,
       raws.station.tbl,
       agrimet.station.tbl,
       hydromet.station.tbl,
       mw.station.tbl,
       strip_alpha,
       strip_tbl_num,
       file = paste0("//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/model_QAPPs/R/data/RData/",
                     file.name,".RData"))
  
}

# Leaflet Map Data ----
data.dir <- "//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/model_QAPPs/R/data/"
lookup.huc <- readxl::read_xlsx(paste0(data.dir, "Lookup_QAPPProjectArea.xlsx"), sheet = "Lookup_QAPPProjectArea")
schedule <- readxl::read_xlsx(paste0(data.dir, "Model_Setup_Info.xlsx"), sheet = "Schedule")
project.areas <- read.csv(paste0(data.dir,"qapp_project_areas.csv")) %>% 
  dplyr::left_join(schedule, by=c("areas"="QAPP Project Area"))

pro_areas <- sf::st_read(dsn = "//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/model_QAPPs/R/data/gis/project_areas.shp",
                         layer = "project_areas")
pro_areas <- sf::st_transform(pro_areas, 4326)
pro_areas <- pro_areas %>% 
  dplyr::mutate(color = dplyr::case_when(Project_Na == "John Day River Basin" ~ "#df65b0", #pink
                                         Project_Na == "Lower Grande Ronde, Imnaha, and Wallowa Subbasins" ~ "yellow",
                                         Project_Na == "Lower Willamette and Clackamas Subbasins" ~ "#253494", #blue
                                         Project_Na == "Malheur River Subbasins" ~ "#78c679", #green
                                         Project_Na == "Mid Willamette Subbasins" ~ "#253494", #blue
                                         Project_Na == "Middle Columbia-Hood, Miles Creeks" ~ "yellow",
                                         Project_Na == "North Umpqua Subbasin" ~ "purple",
                                         Project_Na == "Rogue River Basin" ~ "#df65b0", #pink
                                         Project_Na == "Sandy Subbasin" ~ "#253494", #blue
                                         Project_Na == "South Umpqua and Umpqua Subbasins" ~ "purple",
                                         Project_Na == "Southern Willamette Subbasins" ~ "#253494", #blue
                                         Project_Na == "Walla Walla Subbasin" ~ "#78c679", #green
                                         Project_Na == "Willow Creek Subbasin" ~ "#78c679")) %>%  #green
  dplyr::left_join(project.areas, by = c("Project_Na" = "areas")) %>% 
  dplyr::mutate(CompleteD = format(as.Date(EPA.Approval,"%Y-%m-%d", tz="UTC"),"%b %d, %Y")) %>% 
  #dplyr::mutate(map_link = paste0("<a href='area_maps/'",file.name,".html'>",Project_Na,"</a>")) %>% 
  dplyr::mutate(map_link = paste0("<a href='area_maps/",file.name,".html'>",Project_Na,"</a>")) %>% 
  dplyr::arrange(EPA.Approval)

pro_reaches <- sf::st_read(dsn = "//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/model_QAPPs/R/data/gis/project_reach_extent.shp",
                           layer = "project_reach_extent")
#pro_reaches <- sf::st_zm(pro_reaches, drop = T, what = "ZM")
pro_reaches <- sf::st_transform(pro_reaches, 4326)
pro_reaches <- pro_reaches %>% 
  dplyr::mutate(color = dplyr::case_when(Project_Na == "Willamette River Mainstem and Major Tributaries"~ "purple",
                                         Project_Na == "Snake River – Hells Canyon"~ "yellow"))

# _ Model Extents ----
map_ce_model_extent <- sf::st_read(dsn = "//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/model_QAPPs/R/data/gis/ce_model_extent.shp",
                               layer = "ce_model_extent")

map_ce_model_extent <- sf::st_transform(map_ce_model_extent, 4326)

map_hs_temp_model_extent <- sf::st_read(dsn = "//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/model_QAPPs/R/data/gis/hs_temp_model_extent.shp",
                                    layer = "hs_temp_model_extent")

map_hs_temp_model_extent <- sf::st_transform(map_hs_temp_model_extent, 4326)

map_hs_solar_model_extent <- sf::st_read(dsn = "//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/model_QAPPs/R/data/gis/hs_solar_model_extent.shp",
                                         layer = "hs_solar_model_extent")

map_hs_solar_model_extent <- sf::st_transform(map_hs_solar_model_extent, 4326)

# Shadow model is only for Rouge River Basin
map_sh_model_extent <- sf::st_read(dsn = "//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/model_QAPPs/R/data/gis/shade_model_streams_temp_projects_clean.shp",
                               layer = "shade_model_streams_temp_projects_clean")

map_sh_model_extent <- sf::st_transform(map_sh_model_extent, 4326) 

# map.tir_extent

# _ HUC 8,10,12 ---- 
# Codes for HUCs here are not used. HUCs are pulled from the REST server in the map_pro_areas.R.
#map_huc8 <- sf::st_read(dsn = "//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/GIS/Study_Areas_v5_HUC8_scope.shp",
#                        layer = "Study_Areas_v5_HUC8_scope") %>% 
#  dplyr::select(HUC_8,geometry)
#map_huc8 <- tigris::geo_join(map_huc8, lookup.huc, by_sp = "HUC_8", by_df = "HUC_8", how = "left")
#map_huc8  <- sf::st_transform(map_huc8 , 4326)

#map_huc10 <- sf::st_read(dsn = "//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/GIS/Study_Areas_v6_HUC10_scope.shp",
#                         layer = "Study_Areas_v6_HUC10_scope")%>% 
#  dplyr::select(HUC_10,geometry)
#map_huc10 <- tigris::geo_join(map_huc10, lookup.huc, by_sp = "HUC_10", by_df = "HUC10", how = "left")
#map_huc10 <- sf::st_transform(map_huc10, 4326)

#map_huc12 <- sf::st_read(dsn = "//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/model_QAPPs/R/data/gis/project_huc12.shp",
#                         layer = "project_huc12") %>% 
#  dplyr::select(HUC12,geometry)
#map_huc12 <- tigris::geo_join(map_huc12, lookup.huc, by_sp = "HUC12", by_df = "HUC12", how = "left")
#map_huc12 <- sf::st_transform(map_huc12, 4326)

# _ IR2018/20 ----
colum_auid <- sf::st_read(dsn = "//deqhq1/TMDL/Planning statewide/2020_2024",
                          layer="Columbia_River_AU_IDs",
                          stringsAsFactors=FALSE) %>% 
  sf::st_drop_geometry() %>%
  dplyr::distinct(AU_ID) %>%
  dplyr::pull(AU_ID) 

# _ Temperature WQS ----
## this takes too long to get the layer. Need to use the REST layer.
#wqs <- sf::st_read(
#  dsn = "//deqhq1/GISLIBRARY/Base_Data/DEQ_Data/Water_Quality/WQ_Standards/GeoRef_Standards.gdb",
#  layer = "Oregon_Standards",
#  stringsAsFactors = FALSE) %>% 
#  sf::st_zm() %>%
#  sf::st_transform(4326)

#pro_area_wqs <- wqs %>% 
#  dplyr::filter(sf::st_contains(pro_area, ., sparse = FALSE)) %>% 
#  dplyr::filter(!TempCode == "99")

# _ Save Data ----
save(lookup.huc,
     project.areas,
     pro_areas,
     pro_reaches,
     map_ce_model_extent,
     map_hs_temp_model_extent,
     map_hs_solar_model_extent,
     map_sh_model_extent,
     colum_auid,
     file = "//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/model_QAPPs/R/data/RData/map.RData")

