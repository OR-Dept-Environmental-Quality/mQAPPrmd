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
# _ TMDL solicitation data ----
load("//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/Data Solicitation/FINAL_Reviewed_Submissions/Wave1_2021-01-08/Wave1_2021-01-08_SumStats.RData")
wave1 <- sumstats
load("//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/Data Solicitation/FINAL_Reviewed_Submissions/Wave2_2021-02-04/Wave2_2021-02-04_SumStats.RData")
wave2 <- sumstats
load("//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/Data Solicitation/FINAL_Reviewed_Submissions/Wave3_2021-02-23/Wave3_2021-02-23_SumStats.RData")
wave3 <- sumstats
solic.data <- rbind(wave1,wave2,wave3) %>% 
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
  dplyr::mutate(HUC8 = NA,
                Org_Name = NA,
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
                Reachcode = NA)

solic.stations <- readxl::read_xlsx("//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/Data Solicitation/FINAL_Reviewed_Submissions/MLocs_ALL_Waves_FINAL.xlsx", 
                                    sheet = "Monitoring_Locations") %>% 
  dplyr::filter(Stations.DB.Status %in% c("New")) %>% 
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
                HUC8_Name = NA)

data.dir <- "//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/model_QAPPs/R/data/"
# _ USGS flow data ----
load(paste0(data.dir,"/download/usgs.RData")) # usgs.stations.or & usgs.data.or
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

# _ Worksheet ----
cal.model <- readxl::read_xlsx(paste0(data.dir, "Model_Setup_Info.xlsx"), sheet = "Calibration Model Setup Info") %>% 
  dplyr::filter(!`QAPP Project Area` %in%  c("Upper Klamath and Lost Subbasins")) %>% 
  # for Section 6.1 General Model Parameters
  dplyr::mutate(Model_version = ifelse(substr(`Model version`, 13,13) == "6", "Heat Source Version 6",
                                       ifelse(substr(`Model version`, 13,13) == "7", "Heat Source Version 7",
                                              ifelse(substr(`Model version`, 13,13) == "8", "Heat Source Version 8",
                                                     ifelse(substr(`Model version`, 13,13) == "9", "Heat Source Version 9",
                                                            ifelse(substr(`Model version`, 1,2) == "CE", "CE-Qual-W2 Version 3","SHADOW")))))) %>% 
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
  dplyr::filter(!`QAPP Project Area` %in% "Upper Klamath and Lost Subbasins")
schedule <- readxl::read_xlsx(paste0(data.dir, "Model_Setup_Info.xlsx"), sheet = "Schedule")
ref <- readxl::read_xlsx(paste0(data.dir, "Model_Setup_Info.xlsx"), sheet = "References")
roles <- readxl::read_xlsx(paste0(data.dir,"tables.xlsx"),sheet = "roles")
risks <- readxl::read_xlsx(paste0(data.dir,"tables.xlsx"),sheet = "risks")
abbr <- readxl::read_xlsx("//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/model_QAPPs/R/data/tables.xlsx",sheet = "abbr")
data.gap <- readxl::read_xlsx(paste0(data.dir,"tables.xlsx"),sheet = "data_gap")
# tmdl.temp <- odeqtmdl::tmdl_db %>% dplyr::filter(pollutant_name_TMDL %in% "Temperature")
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
lookup_huc <- readxl::read_xlsx(paste0(data.dir, "Lookup_QAPPProjectArea_HUC10.xlsx"), sheet = "Lookup_QAPPProjectArea_HUC10")
qapp_project_areas <- read.csv(paste0(data.dir,"qapp_project_area.csv")) %>% 
  dplyr::left_join(schedule, by=c("areas"="QAPP Project Area"))

# _ IR2018/20 Cat 4 & 5 ----
cat.45.rivers <- sf::st_read(dsn = "//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/GIS/2018_2020_IR_Cat4_5_Temp_Rivers_FINAL.shp",
                             layer = "2018_2020_IR_Cat4_5_Temp_Rivers_FINAL")
cat.45.waterbodies <- sf::st_read(dsn = "//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/GIS/2018_2020_IR_Cat4_5_Temp_Waterbodies_FINAL.shp",
                                  layer = "2018_2020_IR_Cat4_5_Temp_Waterbodies_FINAL")
cat.45.watershed <- sf::st_read(dsn = "//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/GIS/2018_2020_IR_Cat4_5_Temp_Watershed_FINAL.shp",
                                  layer = "2018_2020_IR_Cat4_5_Temp_Watershed_FINAL") %>% 
  dplyr::mutate(`AU_Name` = gsub(pattern = "HUC12 Name: ","",`AU_Name`))
  
cat.45 <- rbind(cat.45.rivers[,c("IR_categor","AU_Name","AU_ID","Year_liste","Period","HUC12")],
                cat.45.waterbodies[,c("IR_categor","AU_Name","AU_ID","Year_liste","Period","HUC12")],
                cat.45.watershed[,c("IR_categor","AU_Name","AU_ID","Year_liste","Period","HUC12")]) %>% 
  sf::st_zm() %>% 
  dplyr::left_join(lookup_huc, by = "HUC12")
  
cat.45.geom <- sf::st_transform(cat.45, 4326)

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
  dplyr::mutate_at("AU_Name", str_replace_all, "Thunder Creek-North Unpqua River", "Thunder Creek-North Umpqua River")
  
# _ NCDC met data ----
load(paste0(data.dir,"/download/ncei.RData"))
ncei.stations <- ncei %>% 
  dplyr::mutate(lat = LAT_DEC,
                long = LON_DEC) %>% 
  sf::st_as_sf(coords = c("LON_DEC", "LAT_DEC"), crs = sf::st_crs("+init=EPSG:4269"))

# _ RAWS met data ----
load(paste0(data.dir,"/download/raws.RData"))
raws.stations <- raws.meta %>% 
  dplyr::mutate(lat = latitude,
                long = longitude) %>%
  sf::st_as_sf(coords = c("longitude", "latitude"), crs = sf::st_crs("+init=EPSG:4269"))

# _ USBR AgriMet ----
agrimet.stations <- read.csv(paste0(data.dir, "download/agrimet_stations.csv"))
agrimet.parameters <- read.csv(paste0(data.dir, "download/agrimet_parameters.csv"))
agrimet.stations.or <- agrimet.stations %>%
  dplyr::filter(state == "OR") %>% 
  # dplyr::left_join(agrimet.parameters, by = "siteid") %>% 
  dplyr::mutate(lat = latitude, long = longitude) %>% 
  sf::st_as_sf(coords = c("longitude", "latitude"), crs = sf::st_crs("+init=EPSG:4269"))

# _ USBR Hydromet ----
hydromet <- read.csv(paste0(data.dir, "download/hydromet.csv"))

# _ MesoWest climate data ----
load(paste0(data.dir,"/download/mw.RData"))
mw.stations <- mw.meta$STATION %>%
  dplyr::mutate(lat = LATITUDE, long = LONGITUDE) %>% 
  sf::st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = sf::st_crs("+init=EPSG:4269"))

# _ HUC 8 ----
wbd.huc8 <- sf::st_read(dsn = "//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/GIS/Study_Areas_v5_HUC8_scope.shp",
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

for (qapp_project_area in qapp_project_areas$areas) {
  
  huc8.extent <- qapp_project_areas[which(qapp_project_areas$areas == qapp_project_area),]$huc8.extent
  file.name <- qapp_project_areas[which(qapp_project_areas$areas == qapp_project_area),]$file.name
  
  subbasin <- unique(lookup_huc[which(lookup_huc$QAPP_Project_Area == qapp_project_area),]$HUC8_NAME)
  subbasin_num <- unique(lookup_huc[which(lookup_huc$QAPP_Project_Area == qapp_project_area),]$HUC8)
  
  # _ Temp data ----
  # AWQMS, Solicitation Data and OWRD Temp Data
  station_awqms <- df.stations.state %>% 
    dplyr::select(AltLocID,AltLocName,AU_ID,BacteriaCode,ben_use_code,CollMethod,Comments,COUNTY,Created_Date,
                  Datum,DO_code,DO_SpawnCode,FishCode,GNIS_Name,HUC10,HUC10_Name,HUC12,HUC12_Name,HUC4_Name,
                  HUC6_Name,HUC8,HUC8_Name,Lat_DD,LLID,Long_DD,MapScale,Measure,MLocID,MonLocType,OrgID,
                  Permanent_Identifier,pH_code,Reachcode,RiverMile,SpawnCode,STATE,StationDes,TribalLand,
                  TribalName) %>%
    rbind(solic.stations) %>% 
    dplyr::filter(HUC8_Name %in% subbasin) %>% 
    dplyr::rename(`Station ID` = MLocID)
  
  station_model <- cal.input %>% 
    dplyr::filter(`QAPP Project Area` %in%  qapp_project_area) %>% 
    dplyr::left_join(station_awqms[,c("Station ID", "StationDes", "OrgID")], by="Station ID")
  
  station_owrd <- owrd.stations.or %>%
    dplyr::filter(HUC8 %in% subbasin_num) %>% 
    dplyr::select(`Data Source` = Operator,
                  `Station ID` = station_nbr,
                  `Station` = station_name,
                  Lat,
                  Long) %>% 
    dplyr::distinct(`Station ID`,.keep_all=TRUE)
  
  owrd.data.temp <- owrd.data %>% 
    dplyr::filter(Char_Name %in% c("daily_max_water_temp_C")) %>% 
    dplyr::filter(MLocID %in% station_owrd$`Station ID`) %>% 
    dplyr::mutate(Statistical_Base = "Maximum")
  
  temp.data <- df.awqms.raw.state %>% 
    dplyr::select(HUC8,MLocID,Org_Name,Project1,QualifierAbbr,Result_Numeric,Result_status,SampleStartDate,Statistical_Base,
                  Char_Name,Result_Unit,Result_Type,Time_Basis,Activity_Type,SamplingMethod,Result_Depth,Result_Depth_Unit,
                  SampleStartTime,SampleStartTZ,Method_Code,Result_Comment,HUC10,HUC12,HUC8_Name,HUC12_Name,Lat_DD,Long_DD,
                  OrganizationID,Result_Operator,StationDes,MonLocType,AU_ID,Measure,Reachcode) %>% 
    rbind(solic.data, owrd.data.temp) %>% 
    dplyr::filter(!Project1 %in% c("TMDL Data Submission")) %>% 
    dplyr::filter(HUC8 %in% subbasin_num) %>% 
    # QA/QC check:
    dplyr::filter(Result_status %in% c("Final", "Provisional") | QualifierAbbr %in% c("DQL=A","DQL=B","DQL=E"))
  
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
    dplyr::left_join(station_awqms[,c("Station ID", "StationDes")], by="Station ID") %>% 
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
    dplyr::mutate(#Latitude = round(as.numeric(Latitude),4),
                  Latitude = round(Latitude,4),
                  Longitude = round(Longitude,3))
  
  pro.area.tmdls <- knitr::combine_words(unique(model.info$"TMDL Document"))
  
  qapp_project_area_huc8 <- wbd.huc8 %>% 
    dplyr::filter(HUC_8 %in% subbasin_num)
  
  qapp_project_area_huc8_union <- sf::st_union(qapp_project_area_huc8)
  
  # _ IR2018/20 Cat 4 & 5 ----
  pro.cat.45.tbl <- cat.45.tbl %>% 
    dplyr::filter(QAPP_Project_Area %in% qapp_project_area)
  
  # _ Flow data ----
  usgs.stations <- usgs.stations.or %>%  # Discharge [ft3/s]
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
    dplyr::filter(!`Station ID` %in% station_owrd$`Station ID`)

  flow.stations <- rbind(usgs.stations, station_owrd) %>% 
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
 
  usgs.data <- usgs.data.or %>% 
    dplyr::filter(site_no %in% usgs.stations$`Station ID`) %>% 
    dplyr::select(`Data Source` = agency_cd,
                  `Station ID` = site_no,
                  dateTime,
                  Result = X_00060_00003)
  
  owrd.data.flow <- owrd.data %>% 
    dplyr::filter(Char_Name %in% c("mean_daily_flow_cfs")) %>% 
    dplyr::filter(MLocID %in% station_owrd$`Station ID`) %>% 
    dplyr::select(`Station ID`= MLocID,
                  dateTime = SampleStartDate,
                  Result = Result_Numeric)%>% 
    dplyr::mutate(`Data Source` = "OWRD")
   
  flow.data <- rbind(usgs.data,owrd.data.flow)
  
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
  ncei.stations.huc8 <- ncei.stations %>% 
    filter(sf::st_contains(qapp_project_area_huc8_union, ., sparse = FALSE))
  
  ncei.stations.subbasin <-  sf::st_join(x = ncei.stations.huc8,
                                         y = qapp_project_area_huc8,
                                         join = st_within,
                                         left = TRUE) %>% 
    sf::st_drop_geometry()
  
  ncei.station.tbl <- ncei.stations.subbasin %>% 
    dplyr::mutate(STATION_NAME = stringr::str_to_title(STATION_NAME))
  
  # _ RAWS met data ----
  raws.stations.huc8 <- raws.stations %>% 
    filter(sf::st_contains(qapp_project_area_huc8_union, ., sparse = FALSE))
  
  raws.stations.subbasin <-  sf::st_join(x = raws.stations.huc8,
                                         y = qapp_project_area_huc8,
                                         join = st_within,
                                         left = TRUE) %>% 
    sf::st_drop_geometry()
  
  raws.station.data.type <- raws.data.type %>% 
    dplyr::rename(Parameter = "unlist.type.list.columnNames.") %>% 
    dplyr::filter(Parameter %in% c("humidity","precipitation","temperature","windDirection","windSpeed")) %>% 
    dplyr::mutate_at("Parameter", str_replace_all, "humidity", "Humidity") %>% 
    dplyr::mutate_at("Parameter", str_replace_all, "precipitation", "Precipitation") %>%
    dplyr::mutate_at("Parameter", str_replace_all, "temperature", "Temperature") %>%
    dplyr::mutate_at("Parameter", str_replace_all, "windDirection", "Wind Direction") %>%
    dplyr::mutate_at("Parameter", str_replace_all, "windSpeed", "Wind Speed")
  
  raws.station.tbl <- raws.stations.subbasin %>% 
    dplyr::left_join(raws.station.data.type, by = "wrccID")
  
  # _ USBR AgriMet ----
  agrimet.stations.huc8 <- agrimet.stations.or %>% 
    filter(sf::st_contains(qapp_project_area_huc8_union, ., sparse = FALSE))
  
  #  ggplot() 
  #    geom_sf(data = wbd.huc8)
  #    geom_sf(data = qapp_project_area_huc8,
  #            colour = "blue") 
  #    geom_sf(data = agrimet.stations.or) 
  #    geom_sf_label(data = agrimet.stations.or,
  #                  aes(lat, long, label = siteid))
  
  agrimet.stations.subbasin <-  sf::st_join(x = agrimet.stations.huc8,
                                            y = qapp_project_area_huc8,
                                            join = st_within,
                                            left = TRUE) %>% 
    sf::st_drop_geometry()
  
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
                                       left = TRUE) %>% 
    sf::st_drop_geometry()
  
  mw.station.tbl <- mw.stations.subbasin %>% 
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
  setwd("//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/model_QAPPs/R/data/RData")
  
  save(df.stations,
       ref,
       roles,
       risks,
       abbr,
       data.gap,
       npdes.ind,
       npdes.gen,
       qapp_project_areas,
       qapp_project_area,
       station_awqms,
       station_model,
       station_owrd,
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
       file = paste0(file.name,".RData"))
  
}

# Leaflet Map Data ----
pro.areas <- sf::st_read(dsn = "//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/model_QAPPs/R/data/gis/project_extent_3.shp",
                         layer = "project_extent_3")

pro.areas <- sf::st_transform(pro.areas, 4326)

pro.areas <- pro.areas %>% 
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
  dplyr::left_join(qapp_project_areas, by = c("Project_Na" = "areas")) %>% 
  dplyr::mutate(CompleteD = format(as.Date(EPA.Approval,"%m/%d/%Y"),"%b %d, %Y")) %>% 
  dplyr::mutate(map_link = paste0("<a href='area_maps/'",file.name,".html'>",Project_Na,"</a>"))
  
pro.reaches <- sf::st_read(dsn = "//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/model_QAPPs/R/data/gis/project_reach_extent.shp",
                           layer = "project_reach_extent")

#pro.reaches <- sf::st_zm(pro.reaches, drop = T, what = "ZM")

pro.reaches <- sf::st_transform(pro.reaches, 4326)

pro.reaches <- pro.reaches %>% 
  dplyr::mutate(color = dplyr::case_when(Project_Na == "Willamette River Mainstem and Major Tributaries"~ "purple",
                                         Project_Na == "Snake River – Hells Canyon"~ "yellow"))

# _ Model Streams ----
temp.model.streams <- sf::st_read(dsn = "//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/model_QAPPs/R/data/gis/temp_model_streams_temp_projects.shp",
                                  layer = "temp_model_streams_temp_projects")

temp.model.streams <- sf::st_transform(temp.model.streams, 4326)

shadow.model.streams <- sf::st_read(dsn = "//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/model_QAPPs/R/data/gis/shade_model_streams_temp_projects_clean.shp",
                                    layer = "shade_model_streams_temp_projects_clean") %>% 
  dplyr::select(-`Project_Na`) %>% 
  dplyr::rename(`Project_Na` = `Project__1`)

shadow.model.streams <- sf::st_transform(shadow.model.streams, 4326)

#shadow.model.area <- sf::st_read(dsn = "//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/model_QAPPs/R/data/gis/shade_model_area_SWillamette_temp_projects.shp",
#                                 layer = "shade_model_area_SWillamette_temp_projects")

#shadow.model.area <- sf::st_transform(shadow.model.area, 4326)

#ggplot() +
#  geom_sf(data = shade.model.area) +
#  geom_sf(data = temp.model.streams,color = "red") +
#  geom_sf(data = shade.model.streams,color = "blue")

# _ HUC 8,10,12 ----
map.huc8 <- sf::st_read(dsn = "//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/GIS/Study_Areas_v5_HUC8_scope.shp",
                        layer = "Study_Areas_v5_HUC8_scope")

map.huc8  <- sf::st_transform(map.huc8 , 4326)

map.huc10 <- sf::st_read(dsn = "//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/GIS/Study_Areas_v6_HUC10_scope.shp",
                         layer = "Study_Areas_v6_HUC10_scope")

map.huc10 <- sf::st_transform(map.huc10, 4326)

map.huc12 <- sf::st_read(dsn = "//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/model_QAPPs/R/data/gis/project_huc12.shp",
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
  dplyr::distinct(`Station Description`, .keep_all=TRUE) %>% # 2/8 added
  dplyr::left_join(df.stations,by = c("Station ID"="MLocID")) %>% # 2/8 added
  dplyr::filter(!`Station ID` %in% "TIR") %>% # ?? 2/8 note: why?
  dplyr::filter(!is.na(Latitude)) %>% # 2/8 note: QAPP may have sites that have no lat/long assoicated with but may sites must have lat/long
  dplyr::mutate(`Station Name and ID` = ifelse(`Station ID` == "No Station ID or unknown" | is.na(`Station ID`),
                                               `Station Description`,
                                               paste0(`Station Description`, " (", `Station ID`, ")"))) %>% 
  dplyr::mutate(`Station Name and ID` = stringr::str_to_title(`Station Name and ID`)) %>% 
  dplyr::mutate_at("Station Name and ID", str_replace_all, "Ordeq", "ORDEQ") %>%
  dplyr::mutate_at("Station Name and ID", str_replace_all, " Rm", " RM") %>%
  dplyr::mutate_at("Station Name and ID", str_replace_all, "Lb", "LB") %>%
  dplyr::mutate_at("Station Name and ID", str_replace_all, "Nf", "NF") %>%
  dplyr::mutate_at("Station Name and ID", str_replace_all, "Sf", "SF") %>%
  # dplyr::distinct(`Station Name and ID`, .keep_all=TRUE) %>% # 2/8 changed
  dplyr::mutate(Organization = ifelse(Organization == "OregonDEQ", "ODEQ", Organization)) %>% 
  dplyr::mutate(Organization = ifelse(Organization == "11NPSWRD_WQX", "EPA WQX", Organization)) %>% 
  dplyr::mutate(Organization = ifelse(Organization == "CITY_SALEM(NOSTORETID)", "City of Salem", Organization)) %>%
  dplyr::mutate(Organization = ifelse(Organization == "USFS(NOSTORETID)", "USFS", Organization)) %>%
  dplyr::mutate(Organization = ifelse(Organization == "CTUIR_WQX", "CTUIR WQX", Organization)) %>%
  dplyr::mutate(Organization = ifelse(Organization == "WALLAWALLA_WC(NOSTORETID)", "Walla Walla Basin Watershed Council", Organization)) %>%
  dplyr::select(`Station Name and ID`, Latitude, Longitude, Organization, `Station Description`, `Station ID`) %>%
  sf::st_as_sf(coords = c("Longitude","Latitude"), crs = sf::st_crs("+init=EPSG:4326"))

map.temp <- sf::st_filter(temp.awqms.model, pro.areas, join = st_within)

map.temp.pro <-  sf::st_join(x = map.temp,
                             y = pro.areas,
                             join = st_within,
                             left = TRUE)

# writeOGR(map.temp.pro, ".", "map_temp_pro", driver="ESRI Shapefile")

# Temp table
map.station_awqms <- df.stations.state %>% 
  dplyr::rename(`Station ID` = MLocID)

map.temp.tbl <- df.awqms.raw.state %>%
  # QA/QC check:
  dplyr::filter(Result_status %in% c("Final", "Provisional") | QualifierAbbr %in% c("DQL=A","DQL=B","DQL=E")) %>% 
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
  dplyr::left_join(map.station_awqms[,c("Station ID", "StationDes")], by="Station ID") %>% 
  dplyr::rename(Year = year) %>% 
  dplyr::mutate(`StationDes` = stringr::str_to_title(`StationDes`)) %>% 
  dplyr::mutate_at("StationDes", str_replace_all, "Or", "OR") %>% 
  dplyr::filter(!`Station ID` == "TIR") %>% 
  dplyr::mutate(Station = StationDes) %>% 
  dplyr::select(Year, `Station ID`, Station, Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec) %>% 
  dplyr::arrange(Year, `Station ID`)
  
# _ Flow ----
flow.usgs <- usgs.stations.or %>%
  dplyr::filter(!(site_tp_cd %in% c("SP","GW"))) %>% # ST = Stream
  dplyr::filter(data_type_cd %in% c("dv", "id", "iv")) %>% # dv=daily values; id=historical instantaneous values; iv=instantaneous values
  #dplyr::filter(!is.na(dec_lat_va)) %>% # 2/8 changed
  dplyr::mutate_at("station_nm", str_replace_all, " R ", " RIVER ") %>% 
  dplyr::mutate_at("station_nm", str_replace_all, " @ ", " AT ") %>% 
  dplyr::mutate_at("station_nm", str_replace_all, " & ", " AND ") %>% 
  dplyr::mutate_at("station_nm", str_replace_all, " CK ", " CREEK ") %>% 
  dplyr::mutate_at("station_nm", str_replace_all, " CR ", " CREEK ") %>% 
  dplyr::mutate_at("station_nm", str_replace_all, " NR ", " NEAR ") %>% 
  dplyr::mutate_at("station_nm", str_replace_all, "N\\.", "NORTH ") %>% 
  #dplyr::mutate_at("station_nm", str_replace_all, "N ", "NORTH ") %>% 
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
  # dplyr::distinct(Lat, Long, .keep_all = TRUE) %>% @ 2/8 changed
  dplyr::mutate(Station = Station.Name,
                `Station ID` = Station.ID,
                Station_Des = paste0("Hydromet: ",Station," (",`Station ID`,")")) %>% 
  dplyr::rename(Latitude = Lat,
                Longitude = Long) %>% 
  dplyr::select(Station, `Station ID`, Latitude, Longitude, Station_Des)

flow.sp <- cal.input %>%
  dplyr::filter(`Parameter` %in% c("Flow")) %>%
  dplyr::filter(!`Model Location Type` == "Point of Diversion") %>% # 2/8 added
  dplyr::filter(!`Data Source` == "USGS") %>% 
  dplyr::filter(is.na(`Interpolated Data`)) %>% 
  # dplyr::distinct(Latitude, Longitude, .keep_all = TRUE) %>% # 2/8 changed
  dplyr::mutate(Station = `Model Location Name`,
                Station_Des = paste0(`Data Source`,": ",Station," (",`Station ID`,")")) %>%  
  dplyr::select(Station, `Station ID`, Latitude, Longitude, Station_Des)

#flow.pro <- rbind(flow.usgs,flow.hydromet,flow.sp) %>% # 2/8 changed
#  dplyr::filter(!is.na(Latitude)) %>% 
#  sf::st_as_sf(coords = c("Longitude","Latitude"), crs = sf::st_crs("+init=EPSG:4326"))

flow.pro <- rbind(flow.usgs,flow.hydromet,flow.sp)
#flow.pro_na_latlong <- flow.pro %>% dplyr::filter(is.na(Latitude)) # 2/8 changed
flow.pro_na_station_id <- flow.pro %>% dplyr::filter(`Station ID` == "No Station ID")
#flow.pro_na <- rbind(flow.pro_na_latlong,flow.pro_na_station_id) # 2/8 changed
flow.pro_na <- flow.pro_na_station_id # 2/8 added
flow.pro_na <- flow.pro_na[!duplicated(flow.pro_na$Station),]
flow.pro <- flow.pro[!duplicated(flow.pro$Latitude),]
flow.pro <- flow.pro[!duplicated(flow.pro$`Station ID`),] %>% 
  dplyr::filter(!`Station ID` == "No Station ID") %>% 
  rbind(flow.pro_na) %>% 
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

met.ncei <- ncei.stations %>% 
  # dplyr::filter(!str_detect(id,"COOP")) %>%  # 2/8 changed
  dplyr::distinct(NCDC, .keep_all=TRUE) %>% 
  dplyr::distinct(STATION_NAME, .keep_all=TRUE) %>% 
  dplyr::rename(Latitude = lat,
                Longitude = long) %>%
  #dplyr::mutate_at("name", str_replace_all, ", OR US", "") %>% 
  #dplyr::mutate(name = stringr::str_to_title(name)) %>% 
  #dplyr::mutate_at("name", str_replace_all, "Nw", "NW") %>% 
  #dplyr::mutate_at("name", str_replace_all, "Ne", "NE") %>% 
  #dplyr::mutate_at("name", str_replace_all, "Nnw", "NNW") %>% 
  #dplyr::mutate_at("name", str_replace_all, "Se", "SE") %>% 
  #dplyr::mutate_at("name", str_replace_all, "Ese", "ESE") %>% 
  #dplyr::mutate_at("name", str_replace_all, "Wnw", "WNW") %>% 
  #dplyr::mutate_at("name", str_replace_all, "Ssw", "SSW") %>% 
  #dplyr::mutate_at("name", str_replace_all, "Ene", "ENE") %>% 
  #dplyr::mutate_at("name", str_replace_all, "Sse", "SSE") %>% 
  #dplyr::mutate_at("name", str_replace_all, "Sw", "SW") %>% 
  dplyr::mutate(`Station` = paste0(NCDC," at ",STATION_NAME," (NCDC)")) %>% 
  dplyr::select(Station, Latitude, Longitude) %>% 
  sf::st_drop_geometry()

#sf::st_geometry(met.ncdc) <- NULL

met.raws <- raws.stations %>% 
  dplyr::distinct(wrccID, .keep_all=TRUE) %>% 
  dplyr::distinct(siteName, .keep_all=TRUE) %>% 
  dplyr::rename(Latitude = lat,
                Longitude = long) %>% 
  dplyr::mutate(`Station` = paste0(agency," station ", wrccID, " at ", siteName, " (RAWS)")) %>% 
  dplyr::select(Station, Latitude, Longitude) %>% 
  sf::st_drop_geometry()

#sf::st_geometry(met.raws) <- NULL

met.agrimet <- agrimet.stations.or %>%
  dplyr::distinct(siteid, .keep_all=TRUE) %>% 
  dplyr::distinct(description, .keep_all=TRUE) %>% 
  dplyr::rename(Latitude = lat,
                Longitude = long) %>%
  dplyr::mutate(`Station` = paste0(siteid, " - ", description, " (AgriMet)")) %>% 
  dplyr::select(Station, Latitude, Longitude) %>% 
  sf::st_drop_geometry()

#sf::st_geometry(met.agrimet) <- NULL

met.hydromet <- hydromet %>% 
  dplyr::filter(Parameter %in% c("Air Temperature","Precipitation")) %>% 
  dplyr::distinct(Station.ID, .keep_all=TRUE) %>% 
  dplyr::distinct(Station.Name, .keep_all=TRUE) %>% 
  dplyr::rename(Latitude = Lat,
                Longitude = Long) %>%
  dplyr::mutate(`Station` = paste0(Station.ID, " - ", Station.Name, " (Hydromet)")) %>% 
  dplyr::select(Station, Latitude, Longitude)

met.mw <- mw.stations %>% 
  dplyr::distinct(STID, .keep_all=TRUE) %>% 
  dplyr::distinct(NAME, .keep_all=TRUE) %>% 
  dplyr::rename(Latitude = lat,
                Longitude = long) %>%
  dplyr::mutate(`Station` = paste0(STID, " - ", NAME, " (MesoWest)")) %>% 
  dplyr::select(Station, Latitude, Longitude) %>% 
  sf::st_drop_geometry()

#sf::st_geometry(met.mw) <- NULL

met.pro <- rbind(met.sp, met.ncei, met.raws, met.agrimet, met.hydromet, met.mw) %>% 
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
                `River Mile` = round(`River Mile`,1),
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
     pro.reaches,
     temp.model.streams,
     shadow.model.streams,
     #shadow.model.area,
     map.huc8,
     map.huc10,
     map.huc12,
     map.temp.pro,
     map.temp.tbl,
     map.flow.pro,
     map.met.pro,
     map.ind.npdes.pro,
     file = "map.RData")
