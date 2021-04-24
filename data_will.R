# __Willamette River Mainstem and Major Tributaries__ ----
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
# Update date: 2021-4-23
load("//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/data/R/statewide/df_awqms_raw_state.RData") # df.awqms.raw.state
load("//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/data/R/statewide/df_stations_state.RData") # df.stations.state
load("//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/data/R/statewide/df_stations_complete.RData") # df.stations

awqms.data.temp <- df.awqms.raw.state %>% 
  # AWQMS QA/QC check:
  dplyr::filter(Result_status %in% c("Final", "Provisional") | QualifierAbbr %in% c("DQL=A","DQL=B","DQL=E"))

awqms.stations.temp <- df.stations.state %>%
  dplyr::filter(MLocID %in% awqms.data.temp$MLocID) # filter out the stations that have data beyond the period of 1990-2020

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

# RUN BELOW IF AFTER data.R ----
pro_areas_huc10 <- sf::read_sf(dsn = paste0(data.dir,"gis/project_reach_HUC10.shp"),
                          layer = "project_reach_HUC10") %>% 
  dplyr::filter(!`HU_8_NAME` == "Middle Snake-Payette")

# Willamette River Mainstem and Major Tributaries ----
qapp_project_area = "Willamette River Mainstem and Major Tributaries"

huc10.extent <- project.areas[which(project.areas$areas == qapp_project_area),]$huc8.extent
#HUC10 extent of "Willamette River Mainstem and Major Tributaries" was saved in the same in the column of project.areas$huc8.extent
file.name <- project.areas[which(project.areas$areas == qapp_project_area),]$file.name

subbasin <- unique(lookup.huc[which(lookup.huc$QAPP_Project_Area == qapp_project_area),]$HUC10_NAME)
subbasin_num <- unique(lookup.huc[which(lookup.huc$QAPP_Project_Area == qapp_project_area),]$HUC10)

# _ Temp data ----
# AWQMS, Solicitation Data and OWRD Temp Data
station.awqms <- awqms.stations.temp %>% 
  dplyr::filter(HUC8 %in% unique(lookup.huc[which(lookup.huc$HUC10 %in%subbasin_num),]$HUC_8)) %>% 
  dplyr::rename(`Station ID` = MLocID)

station.model <- cal.input %>% 
  dplyr::filter(`QAPP Project Area` %in% qapp_project_area) %>% 
  dplyr::left_join(station.awqms[,c("Station ID", "StationDes", "OrgID")], by="Station ID")

station.owrd <- owrd.stations %>%
  dplyr::filter(HUC8 %in% unique(lookup.huc[which(lookup.huc$HUC10 %in%subbasin_num),]$HUC_8)) %>% 
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
  dplyr::filter(HUC8 %in% unique(lookup.huc[which(lookup.huc$HUC10 %in%subbasin_num),]$HUC_8))

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
  dplyr::mutate_at("Station", str_replace_all, "Ordeq", "ORDEQ") %>%
  dplyr::mutate_at("Station", str_replace_all, " Rm", " RM") %>%
  dplyr::mutate_at("Station", str_replace_all, "Lb", "LB") %>%
  dplyr::mutate_at("Station", str_replace_all, "Nf", "NF") %>%
  dplyr::mutate_at("Station", str_replace_all, "Sf", "SF") %>%
  dplyr::select(Year, `Station ID`, Station, Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec) %>% 
  dplyr::arrange(Year, `Station ID`) %>% 
  dplyr::distinct(Year, `Station ID`,.keep_all=TRUE)

model.info <- cal.model %>% 
  dplyr::filter(`QAPP Project Area` %in%  qapp_project_area)

model.input  <- cal.input %>% 
  dplyr::filter(`QAPP Project Area` %in%  qapp_project_area) %>% 
  dplyr::mutate(Latitude = round(as.numeric(Latitude),4),
                Longitude = round(Longitude,3))

pro.area.tmdls <- knitr::combine_words(unique(model.info$"TMDL Document"))

pro_area_huc10 <- pro_areas_huc10 %>% 
  dplyr::filter(HUC_10 %in% subbasin_num)

pro_area_huc10_union <- sf::st_union(pro_area_huc10)

# _ IR2018/20 Cat 4 & 5 ----
pro.cat.45.tbl <- cat.45.tbl %>% 
  dplyr::filter(QAPP_Project_Area %in% qapp_project_area)

# _ Flow data ----
station.usgs <- usgs.stations %>%  # Discharge [ft3/s]
  dplyr::filter(!(site_tp_cd %in% c("SP","GW"))) %>% # ST = Stream
  dplyr::filter(data_type_cd %in% c("dv", "id", "iv")) %>% # dv=daily values; id=historical instantaneous values; iv=instantaneous values
  dplyr::filter(huc_cd %in% unique(lookup.huc[which(lookup.huc$HUC10 %in%subbasin_num),]$HUC_8)) %>%
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
  dplyr::distinct(`Data Source`,`Station ID`,Result,date, .keep_all=TRUE) %>% 
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
ncei.stations.huc10 <- ncei.stations %>% 
  filter(sf::st_contains(pro_area_huc10_union, ., sparse = FALSE)) %>% 
  sf::st_drop_geometry()

ncei.station.tbl <- ncei.stations.huc10 %>% 
  dplyr::mutate(STATION_NAME = stringr::str_to_title(STATION_NAME))

# _ RAWS met data ----
raws.stations.huc10 <- raws.stations %>% 
  filter(sf::st_contains(pro_area_huc10_union, ., sparse = FALSE)) %>% 
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

raws.station.tbl <- raws.stations.huc10 %>% 
  dplyr::left_join(raws.station.data.type, by = "wrccID")

# _ USBR AgriMet ----
agrimet.stations.huc10 <- agrimet.stations.or %>% 
  filter(sf::st_contains(pro_area_huc10_union, ., sparse = FALSE)) %>% 
  sf::st_drop_geometry()

agrimet.station.data.type <- agrimet.parameters %>% 
  dplyr::filter(Type %in% c("Air Temperature","Precipitation", "Relative Humidity", "Wind"))
#dplyr::group_by(siteid) %>% 
#dplyr::summarise(Type = toString(sort(Type)))

agrimet.station.tbl <- agrimet.stations.huc10 %>% 
  dplyr::left_join(agrimet.station.data.type, by = "siteid")

# _ USBR Hydromet ----
hydromet.stations.huc10 <- hydromet.stations %>% 
  filter(sf::st_contains(pro_area_huc10_union, ., sparse = FALSE)) %>% 
  sf::st_drop_geometry()

hydromet.station.tbl <- hydromet.stations.huc10

# _ MesoWest climate data ----
mw.stations.huc10 <- mw.stations %>% 
  filter(sf::st_contains(pro_area_huc10_union, ., sparse = FALSE)) %>% 
  sf::st_drop_geometry()

mw.station.tbl <- mw.stations.huc10 %>% 
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

# Leaflet Map Data ----
# _ Model Streams ----
pro.reaches <- sf::st_read(dsn = "//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/model_QAPPs/R/data/gis/project_reach_extent_HUC10.shp",
                           layer = "project_reach_extent_HUC10") %>% 
  dplyr::filter(!`HU_8_NAME` == "Middle Snake-Payette") %>% 
  sf::st_zm(pro.reaches, drop = T, what = "ZM")

pro.reaches <- sf::st_transform(pro.reaches, 4326)

# _ HUC 8,10,12 ----
map.huc10 <- sf::read_sf(dsn = paste0(data.dir,"gis/project_reach_HUC10.shp"),
                         layer = "project_reach_HUC10") %>% 
  dplyr::filter(!`HU_8_NAME` == "Middle Snake-Payette")

map.huc10 <- sf::st_transform(map.huc10, 4326)

map.huc8 <- sf::st_read(dsn = "//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/GIS/Study_Areas_v5_HUC8_scope.shp",
                        layer = "Study_Areas_v5_HUC8_scope") %>% 
  dplyr::filter(HUC_8 %in% pro.reaches$HUC_8)

map.huc8  <- sf::st_transform(map.huc8 , 4326)

map.huc12 <- sf::st_read(dsn = "//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/model_QAPPs/R/data/gis/project_huc12.shp",
                         layer = "project_huc12") %>% 
  dplyr::select(HUC12,Name,geometry) %>% 
  dplyr::rename(HUC12_Name = Name) %>% 
  dplyr::filter(HUC12 %in% pro.reaches$HUC12)

map.huc12 <- tigris::geo_join(map.huc12, lookup.huc, by_sp = "HUC12_Name", by_df = "HUC12_Name", how = "left")

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
  dplyr::mutate(Organization = ifelse(Organization == "PDX_WB(NOSTORETID)", "Portland Water Bureau", Organization)) %>%
  dplyr::mutate(Organization = ifelse(Organization == "WALLAWALLA_WC(NOSTORETID)", "Walla Walla Basin Watershed Council", Organization)) %>%
  dplyr::select(`Station Name and ID`, Latitude, Longitude, Organization) %>%
  sf::st_as_sf(coords = c("Longitude","Latitude"), crs = sf::st_crs("+init=EPSG:4326"))

map.temp <- sf::st_filter(temp.awqms.model, map.huc10, join = st_within)

map.temp.pro <-  sf::st_join(x = map.temp,
                             y = map.huc10,
                             join = st_within,
                             left = TRUE)

# writeOGR(map.temp.pro, ".", "map_temp_pro", driver="ESRI Shapefile")

# _ Flow ----
flow.usgs <- usgs.stations %>%
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

map.flow <- sf::st_filter(flow.pro, map.huc10, join = st_within)

map.flow.pro <-  sf::st_join(x = map.flow,
                             y = map.huc10,
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

map.met <- sf::st_filter(met.pro, map.huc10, join = st_within)

map.met.pro <-  sf::st_join(x = map.met,
                            y = map.huc10,
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


map.ind.npdes <- sf::st_filter(ind.npdes.pro, map.huc10, join = st_within)

map.ind.npdes.pro <- sf::st_join(x = map.ind.npdes,
                                 y = map.huc10,
                                 join = st_within,
                                 left = TRUE)

# _ Save Data ----
setwd("//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/model_QAPPs/R/data/RData")

save(lookup.huc,
     project.areas,
     pro.reaches,
     map.huc8,
     map.huc10,
     map.huc12,
     map.temp.pro,
     map.flow.pro,
     map.met.pro,
     map.ind.npdes.pro,
     file = "will_map.RData")

