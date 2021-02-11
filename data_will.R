# __Willamette River Mainstem and Major Tributaries__ ----
# Install and/or load packages ----
packages.cran = c("tidyverse","readxl","rgdal","sf","ggplot2","tigris","devtools")

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
load("//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/data/R/statewide/df_awqms_raw_state.RData") # df.awqms.raw.state
load("//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/data/R/statewide/df_stations_state.RData") # df.stations.state
load("//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/data/R/statewide/df_stations_complete.RData") # df.stations
data.dir <- "//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/model_QAPPs/R/data/"
# external data update date:
update.date <- "2020-12-24"
load(paste0(data.dir,"/download/data_sources_",update.date,".RData"))
agrimet.stations <- read.csv(paste0(data.dir, "download/agrimet_stations.csv"))
agrimet.parameters <- read.csv(paste0(data.dir, "download/agrimet_parameters.csv"))
hydromet <- read.csv(paste0(data.dir, "download/hydromet.csv"))
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
  dplyr::mutate_at("AU_Name", str_replace_all, "McKenzie \\*", "McKenzie River")

# _ NCDC met data ----
ncei.stations <- ncei %>% 
  dplyr::mutate(lat = LAT_DEC,
                long = LON_DEC) %>% 
  sf::st_as_sf(coords = c("LON_DEC", "LAT_DEC"), crs = sf::st_crs("+init=EPSG:4269"))

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

# RUN BELOW IF AFTER data.R ----
will.huc10 <- sf::read_sf(dsn = paste0(data.dir,"gis/project_reach_HUC10.shp"),
                          layer = "project_reach_HUC10") %>% 
  dplyr::filter(!`HU_8_NAME` == "Middle Snake-Payette")

# Willamette River Mainstem and Major Tributaries ----
qapp_project_area = "Willamette River Mainstem and Major Tributaries"

huc10.extent <- qapp_project_areas[which(qapp_project_areas$areas == qapp_project_area),]$huc8.extent
#HUC10 extent of "Willamette River Mainstem and Major Tributaries" was saved in the same in the column of qapp_project_areas$huc8.extent
file.name <- qapp_project_areas[which(qapp_project_areas$areas == qapp_project_area),]$file.name

subbasin <- unique(lookup_huc[which(lookup_huc$QAPP_Project_Area == qapp_project_area),]$HUC10_NAME)
subbasin_num <- unique(lookup_huc[which(lookup_huc$QAPP_Project_Area == qapp_project_area),]$HUC10)

# _ AWQMS ----
station_awqms <- df.stations.state %>% 
  dplyr::filter(HUC10_Name %in% subbasin) %>% 
  dplyr::rename(`Station ID` = MLocID)

station_model <- cal.input %>% 
  dplyr::filter(`QAPP Project Area` %in%  qapp_project_area) %>% 
  dplyr::left_join(station_awqms[,c("Station ID", "StationDes", "OrgID")], by="Station ID")

data <- df.awqms.raw.state %>% 
  dplyr::filter(HUC10 %in%  subbasin_num) %>% 
  # QA/QC check:
  dplyr::filter(Result_status %in% c("Final", "Provisional") | QualifierAbbr %in% c("DQL=A","DQL=B","DQL=E"))

# data.sample.count will be used in the Appendix 1
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

pro.area.tmdls <- knitr::combine_words(unique(model.info$"TMDL Document"))

qapp_project_area_huc10 <- will.huc10 %>% 
  dplyr::filter(HUC_10 %in% subbasin_num)

qapp_project_area_huc10_union <- sf::st_union(qapp_project_area_huc10)

# _ IR2018/20 Cat 4 & 5 ----
pro.cat.45.tbl <- cat.45.tbl %>% 
  dplyr::filter(QAPP_Project_Area %in%  qapp_project_area)

# _ USGS flow data ----
usgs.stations <- usgs.stations.or %>%  # Discharge [ft3/s]
  dplyr::filter(!(site_tp_cd %in% c("SP","GW"))) %>% # ST = Stream
  dplyr::filter(data_type_cd %in% c("dv", "id", "iv")) %>% # dv=daily values; id=historical instantaneous values; iv=instantaneous values
  dplyr::filter(huc_cd %in% unique(lookup_huc[which(lookup_huc$HUC10 %in%subbasin_num),]$HUC8)) %>%
  dplyr::filter(!is.na(dec_lat_va)) %>% 
  dplyr::select(agency_cd, site_no, station_nm, site_tp_cd, dec_lat_va, dec_long_va,begin_date, end_date) %>% 
  dplyr::mutate(long_sf=dec_long_va,
                lat_sf=dec_lat_va) %>%
  dplyr::distinct() %>%
  sf::st_as_sf(coords = c("long_sf",  "lat_sf"), crs = sf::st_crs("+init=EPSG:4269"))

usgs.stations.huc10 <- usgs.stations %>% 
  filter(sf::st_contains(qapp_project_area_huc10_union, ., sparse = FALSE))

usgs.stations.subbasin <-  sf::st_join(x = usgs.stations.huc10,
                                       y = qapp_project_area_huc10,
                                       join = st_within,
                                       left = TRUE) %>% 
  sf::st_drop_geometry()

#ggplot() +
#  geom_sf(data = qapp_project_area_huc10) +
#  geom_sf(data = usgs.stations.subbasin)

usgs.station.tbl <- usgs.stations.subbasin %>% 
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
  dplyr::arrange(`station_nm`)

# _ NCDC met data ----
ncei.stations.huc10 <- ncei.stations %>% 
  filter(sf::st_contains(qapp_project_area_huc10_union, ., sparse = FALSE))

ncei.stations.subbasin <-  sf::st_join(x = ncei.stations.huc10,
                                       y = qapp_project_area_huc10,
                                       join = st_within,
                                       left = TRUE) %>% 
  sf::st_drop_geometry()

ncei.station.tbl <- ncei.stations.subbasin %>% 
  dplyr::mutate(STATION_NAME = stringr::str_to_title(STATION_NAME))

# _ RAWS met data ----
raws.stations.huc10 <- raws.stations %>% 
  filter(sf::st_contains(qapp_project_area_huc10_union, ., sparse = FALSE))

raws.stations.subbasin <-  sf::st_join(x = raws.stations.huc10,
                                       y = qapp_project_area_huc10,
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
#dplyr::group_by(wrccID) %>% 
#dplyr::summarise(Parameter = toString(sort(Parameter)))

raws.station.tbl <- raws.stations.subbasin %>% 
  dplyr::left_join(raws.station.data.type, by = "wrccID")

# _ USBR AgriMet ----
agrimet.stations.huc10 <- agrimet.stations.or %>% 
  filter(sf::st_contains(qapp_project_area_huc10_union, ., sparse = FALSE))

#  ggplot() 
#    geom_sf(data = wbd.huc8)
#    geom_sf(data = qapp_project_area_huc10,
#            colour = "blue") 
#    geom_sf(data = agrimet.stations.or) 
#    geom_sf_label(data = agrimet.stations.or,
#                  aes(lat, long, label = siteid))

agrimet.stations.subbasin <-  sf::st_join(x = agrimet.stations.huc10,
                                          y = qapp_project_area_huc10,
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
mw.stations.huc10 <- mw.stations %>% 
  filter(sf::st_contains(qapp_project_area_huc10_union, ., sparse = FALSE))

mw.stations.subbasin <-  sf::st_join(x = mw.stations.huc10,
                                     y = qapp_project_area_huc10,
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

save(df.stations.state,
     df.stations,
     will.huc10 ,
     model.info,
     model.input,
     pro.area.tmdls,
     station_awqms,
     station_model,
     data.sample.count,
     roles,
     risks,
     abbr,
     data.gap,
     npdes.ind,
     npdes.gen,
     pro.cat.45.tbl,
     usgs.station.tbl,
     ncei.station.tbl,
     raws.station.tbl,
     agrimet.station.tbl,
     hydromet.station.tbl,
     mw.station.tbl,
     ref,
     data.dir,
     huc10.extent,
     file.name,
     qapp_project_area,
     qapp_project_areas,
     subbasin,
     subbasin_num,
     strip_alpha,
     strip_tbl_num,
     file = paste0(file.name,".RData"))
