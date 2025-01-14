# ____ qapp project area data ____ #
library(terra)
library(sf)
library(sp)
library(stars)
terra::gdal(lib="all")

source("data.R")
project.areas.updated <- readxl::read_xlsx(paste0(data.dir,"qapp_project_areas.xlsx"), sheet = "qapp_project_areas_updated")

## for test:
# qapp_project_area = "John Day River Basin"
# qapp_project_area = "Lower Grande Ronde, Imnaha, and Wallowa Subbasins"
# qapp_project_area = "Malheur River Subbasins"
# qapp_project_area = "Middle Columbia-Hood, Miles Creeks"
# qapp_project_area = "Rogue River Basin"
# qapp_project_area = "Sandy Subbasin"
# qapp_project_area = "Walla Walla Subbasin"
# qapp_project_area = "Willow Creek Subbasin"
# _______________________________________________________________
# qapp_project_area = "Lower Willamette and Clackamas Subbasins"
# qapp_project_area = "Middle Willamette Subbasins"
# qapp_project_area = "Southern Willamette Subbasins"

# qapp_project_area = "Willamette Subbasins"
# _______________________________________________________________
# qapp_project_area = "North Umpqua Subbasin"
# qapp_project_area = "South Umpqua and Umpqua Subbasins"

# qapp_project_area = c("Umpqua River Basin")
# _______________________________________________________________

done <- c(
  # "Lower Willamette and Clackamas Subbasins",
  # "Middle Willamette Subbasins",
  # "North Umpqua Subbasin",
  # "Rogue River Basin",
  "Sandy Subbasin"#,
  # "South Umpqua and Umpqua Subbasins",
  # "Southern Willamette Subbasins",
  # "Willamette River Mainstem and Major Tributaries"
  )

for (qapp_project_area in project.areas.updated[which(!project.areas.updated$areas %in% done),]$areas) {
  
  file.name <- project.areas.updated[which(project.areas.updated$areas %in% qapp_project_area),]$file.name
  file.dir <- project.areas.updated[which(project.areas.updated$areas %in% qapp_project_area),]$file.dir
  
  if("Willamette Subbasins" %in% qapp_project_area) {
    qapp_project_area = c("Lower Willamette and Clackamas Subbasins",
                         "Middle Willamette Subbasins",
                         "Southern Willamette Subbasins",
                         "Willamette Subbasins")
  }
  
  if("Umpqua River Basin" %in% qapp_project_area) {
    qapp_project_area = c("North Umpqua Subbasin",
                         "South Umpqua and Umpqua Subbasins",
                         "Umpqua River Basin")
  }
  
  
  print(paste0(qapp_project_area," QAPP data..."))
  
  # _ Project area and HUCs ----
  subbasin_num <- unique(lookup.huc[which(lookup.huc$QAPP_Project_Area %in% qapp_project_area),]$HUC_8)

  pro_area <- sf::st_as_sf(pro_areas) %>% 
    dplyr::filter(Project_Na %in% qapp_project_area) 
  # %>% 
  #   sf::st_transform(4326)  #4269
  
  pro_area_huc8 <- pro_areas_huc8 %>% 
    dplyr::filter(HUC_8 %in% subbasin_num)
  
  pro_area_huc12 <- sf::read_sf(dsn = paste0(data.dir,"/gis/project_huc12.shp"),
                                layer = "project_huc12") %>% 
    dplyr::filter(HUC_8 %in% subbasin_num) 
  # %>% 
  #   sf::st_transform(4326) #4326
  
  sf::sf_use_s2(FALSE)
  
  pro_area_huc12_union <- sf::st_union(pro_area_huc12) %>% 
    sf::st_transform(crs = sf::st_crs("+init=EPSG:4326"))
  
  # _ IR2018/20 Cat 4 & 5 ----
  pro.cat.45.tbl <- cat.45.tbl %>% 
    dplyr::filter(QAPP_Project_Area %in% qapp_project_area)
  
  ## In the 3 Willamette subbasin QAPPs, filter out the AUs covered in the Willamette mainstem QAPP
  if("Willamette Subbasins" %in% qapp_project_area) {
    
    pro.cat.45.tbl <- pro.cat.45.tbl %>% 
      dplyr::filter(!AU_ID %in% will_auid)
    
  }
  
  ## In the Malheur and Grande Ronde QAPPs, filter out the AUs covered in the Snake River QAPP
  if("Lower Grande Ronde, Imnaha, and Wallowa Subbasins" %in% qapp_project_area) {
    
    pro.cat.45.tbl <- pro.cat.45.tbl %>% 
      dplyr::filter(!AU_ID %in% snake_auid)
    
  }

  if("Malheur River Subbasins" %in% qapp_project_area) {
    
    pro.cat.45.tbl <- pro.cat.45.tbl %>% 
      dplyr::filter(!AU_ID %in% snake_auid)
    
  }

  # _ Temp data ----
  # AWQMS, OWRD, and BES Temp Data
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
    sf::st_as_sf(coords = c("long", "lat"), crs = sf::st_crs("+init=EPSG:4326")) #%>% 
  #   dplyr::filter(sf::st_intersects(pro_area_huc12_union, ., sparse = FALSE)) %>%
  #   sf::st_drop_geometry()
  
  station.awqms <- sf::st_intersection(station.awqms,pro_area_huc12_union, sparse = FALSE)
  station.awqms <- sf::st_drop_geometry(station.awqms)
  
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
  
  ## In the 3 Willamette subbasin QAPPs, filter out the reachcodes/stations covered in the Willamette mainstem QAPP
  if("Willamette Subbasins" %in% qapp_project_area) {
    
    station.awqms.temp <- station.awqms.temp %>% 
      dplyr::filter(!Reachcode %in% will_reachcodes)
    
    if("Lower Willamette and Clackamas Subbasins" %in% qapp_project_area) {
      
      station.awqms.temp <- station.awqms.temp %>% 
        dplyr::filter(!`Station ID` %in% c("MHNF-107","MHNF-108","MHNF-110"))
      
    }
    
  }
  
  ## In the Malheur and Grande Ronde QAPPs, filter out the reachcodes covered in the Snake River QAPP
  if("Lower Grande Ronde, Imnaha, and Wallowa Subbasins" %in% qapp_project_area) {
    
    station.awqms.temp <- station.awqms.temp %>% 
      dplyr::filter(!Reachcode %in% snake_reachcodes)
    
  }
  
  if("Malheur River Subbasins" %in% qapp_project_area) {
    
    station.awqms.temp <- station.awqms.temp %>% 
      dplyr::filter(!Reachcode %in% snake_reachcodes)
    
  }
  
  ## _ (2) OWRD ----
  station.owrd <- owrd.stations %>%
    dplyr::mutate(`Station ID` = as.character(station_nbr),
                  lat = Lat,
                  long = Long) %>%
    sf::st_as_sf(coords = c("long", "lat"), crs = sf::st_crs("+init=EPSG:4326")) #%>% 

  station.owrd <- sf::st_intersection(station.owrd,pro_area_huc12_union, sparse = FALSE)
  station.owrd <- station.owrd %>% sf::st_drop_geometry() %>% 
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
    sf::st_as_sf(coords = c("long", "lat"), crs = sf::st_crs("+init=EPSG:4326")) #%>% 
  
  station.bes <- sf::st_intersection(station.bes,pro_area_huc12_union, sparse = FALSE)
  station.bes <- station.bes %>% sf::st_drop_geometry() %>% 
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
  
  ## _ (4) AWQMS + OWRD + BES ----  
  temp.stations <- rbind(station.awqms.temp[,c("Station","Station ID", "Organization", "Latitude", "Longitude")],
                         station.owrd.temp[,c("Station","Station ID", "Organization", "Latitude", "Longitude")],
                         station.bes.temp[,c("Station","Station ID", "Organization", "Latitude", "Longitude")]) %>% 
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
    dplyr::mutate(Organization = ifelse(Organization == "WEYERHAUSER(NOSTORETID)", "Weyerhaeuser", Organization)) %>% 
    dplyr::mutate(Station = ifelse(Station == "Zig Zag River", "Zigzag River", Station)) %>% # Sandy
    dplyr::mutate(Station = ifelse(Station == "ZigZag R at Forest Boundary_LTWT", "Zigzag River at Forest Boundary_LTWT", Station)) %>%  # Sandy
    dplyr::mutate(Station = ifelse(Station == "Iron Moutain Creek", "Iron Mountain Creek", Station)) # South Umpqua
  
  temp.data <- awqms.data.temp %>% 
    dplyr::filter(MLocID %in% station.awqms.temp$`Station ID`) %>%
    dplyr::left_join(station.awqms.temp[,c("Station ID","Station")], by=c("MLocID"="Station ID")) %>% 
    dplyr::select(-StationDes) %>%
    dplyr::rename(StationDes = Station) %>% 
    rbind(owrd.data.temp,bes.data.temp) %>% 
    dplyr::mutate(StationDes = ifelse(StationDes == "Zig Zag River", "Zigzag River", StationDes)) %>% # Sandy
    dplyr::mutate(StationDes = ifelse(StationDes == "ZigZag R at Forest Boundary_LTWT", "Zigzag River at Forest Boundary_LTWT", StationDes)) # Sandy
  
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
    sf::st_as_sf(coords = c("long", "lat"), crs = sf::st_crs("+init=EPSG:4326")) #%>%
  
  station.usgs.flow <- sf::st_intersection(station.usgs.flow,pro_area_huc12_union, sparse = FALSE)
  station.usgs.flow <- station.usgs.flow %>% sf::st_drop_geometry() %>% 
    dplyr::filter(!is.na(dec_lat_va)) %>% 
    dplyr::filter(!site_no %in% station.owrd$`Station ID`) %>% 
    dplyr::distinct(site_no,.keep_all=TRUE)
  
  ## In the 3 Willamette subbasin QAPPs, filter out the reachcodes covered in the Willamette mainstem QAPP
  if("Willamette Subbasins" %in% qapp_project_area) {
    
    station.usgs.flow <- station.usgs.flow %>% 
      dplyr::left_join(df.stations[,c("MLocID","Reachcode")], by = c("site_no" = "MLocID")) %>% 
      dplyr::filter(!Reachcode %in% will_reachcodes) 
    
  } 
  
  ## In the Malheur and Grande Ronde QAPPs, filter out the reachcodes covered in the Snake River QAPP
  if("Lower Grande Ronde, Imnaha, and Wallowa Subbasins" %in% qapp_project_area) {
    
    station.usgs.flow <- station.usgs.flow %>% 
      dplyr::left_join(df.stations[,c("MLocID","Reachcode")], by = c("site_no" = "MLocID")) %>% 
      dplyr::filter(!Reachcode %in% snake_reachcodes)
    
  }
  
  if("Malheur River Subbasins" %in% qapp_project_area) {
    
    station.usgs.flow <- station.usgs.flow %>% 
      dplyr::left_join(df.stations[,c("MLocID","Reachcode")], by = c("site_no" = "MLocID")) %>% 
      dplyr::filter(!Reachcode %in% snake_reachcodes)
    
  }
  
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
  station.hydromet.flow <- sf::st_intersection(hydromet.stations,pro_area_huc12_union, sparse = FALSE)
  station.hydromet.flow <- station.hydromet.flow %>% sf::st_drop_geometry() %>% 
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
  flow.stations <- rbind(station.usgs.flow, station.owrd.flow, station.hydromet.flow) %>% 
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
    dplyr::distinct(Year, `Station ID`,.keep_all=TRUE)
  
  ## _ (5) Instantaneous flow ----
  inst.flow.pro.area <- inst.flow %>% 
    dplyr::filter(`Project Area` %in% qapp_project_area)
  
  # _ Effective shade data ----
  effective.shade.pro.area <- effective.shade %>% 
    dplyr::filter(`Project Area` %in% qapp_project_area)
  
  # _ Worksheet ----
  model.info <- cal.model %>% 
    dplyr::filter(`QAPP Project Area` %in%  qapp_project_area)
  
  model.input  <- cal.input %>% 
    dplyr::filter(`QAPP Project Area` %in%  qapp_project_area) %>% 
    dplyr::mutate(Latitude = round(Latitude,4),
                  Longitude = round(Longitude,3)) %>% 
    dplyr::left_join(station.awqms.temp[,c("Station ID", "Station", "Organization")], by="Station ID") %>% 
    dplyr::mutate(Station = ifelse(Station == "Zig Zag River", "Zigzag River", Station)) %>% # Sandy
    dplyr::mutate(Station = ifelse(Station == "ZigZag R at Forest Boundary_LTWT", "Zigzag River at Forest Boundary_LTWT", Station)) # Sandy
  
  pro.area.tmdls.total <- model.info %>% 
    dplyr::select(`TMDL Document`,`Abbreviated Reference`) %>% 
    dplyr::filter(!is.na(`TMDL Document`)) %>% 
    dplyr::filter(!is.na(`Abbreviated Reference`))
  
  # _ NCDC met data ----
  ncei.stations.pro.area <- sf::st_intersection(ncei.stations,pro_area_huc12_union, sparse = FALSE)
  ncei.stations.pro.area <- ncei.stations.pro.area %>% sf::st_drop_geometry()
  
  ncei.station.tbl <- ncei.stations.pro.area %>%
    dplyr::mutate(STATION_NAME = ifelse(NCDC == "10009634","PORTLAND TROUTDALE AIRPORT",STATION_NAME))
  
  # _ RAWS met data ----
  raws.stations.pro.area <- sf::st_intersection(raws.stations,pro_area_huc12_union, sparse = FALSE)
  raws.stations.pro.area <- raws.stations.pro.area %>% sf::st_drop_geometry()

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
  agrimet.stations.pro.area <- sf::st_intersection(agrimet.stations.or,pro_area_huc12_union, sparse = FALSE)
  agrimet.stations.pro.area <- agrimet.stations.pro.area %>% sf::st_drop_geometry()

  agrimet.station.data.type <- agrimet.parameters %>% 
    dplyr::filter(Type %in% c("Air Temperature","Precipitation", "Relative Humidity", "Wind"))
  #dplyr::group_by(siteid) %>% 
  #dplyr::summarise(Type = toString(sort(Type)))
  
  agrimet.station.tbl <- agrimet.stations.pro.area %>%
    dplyr::left_join(agrimet.station.data.type, by = "siteid")
  
  # _ USBR Hydromet ----
  hydromet.stations.pro.area <- sf::st_intersection(hydromet.stations,pro_area_huc12_union, sparse = FALSE)
  hydromet.stations.pro.area <- hydromet.stations.pro.area %>% sf::st_drop_geometry()
  
  hydromet.station.tbl <- hydromet.stations.pro.area
  
  # _ MesoWest climate data ----
  mw.stations.pro.area <- sf::st_intersection(mw.stations,pro_area_huc12_union, sparse = FALSE)
  mw.stations.pro.area <- mw.stations.pro.area %>% sf::st_drop_geometry()
  
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
    dplyr::filter(`Project Area` %in% qapp_project_area)
  #dplyr::filter(!is.na(`WQ File Nbr`)) %>% 
  #dplyr::filter(!is.na(Latitude)) %>% 
  #dplyr::mutate(lat = Latitude, long = Longitude) %>% 
  #sf::st_as_sf(coords = c("long", "lat"), crs = sf::st_crs("+init=EPSG:4326")) %>% 
  #dplyr::filter(sf::st_intersects(pro_area_huc12_union, ., sparse = FALSE)) %>% 
  #sf::st_drop_geometry()
  
  npdes.gen.pro.area <- npdes.gen %>% 
    dplyr::filter(`Project Area` %in% qapp_project_area)
  #dplyr::filter(!is.na(`WQ File Nbr`)) %>% 
  #dplyr::filter(!is.na(Latitude)) %>% 
  #dplyr::mutate(lat = Latitude, long = Longitude) %>% 
  #sf::st_as_sf(coords = c("long", "lat"), crs = sf::st_crs("+init=EPSG:4326")) %>% 
  #dplyr::filter(sf::st_intersects(pro_area_huc12_union, ., sparse = FALSE)) %>% 
  #sf::st_drop_geometry()
  
  # _ NLCD ----
  nlcd.pro.area <- nlcd.tbl %>% 
    dplyr::ungroup() %>% 
    dplyr::filter(Project_Na %in% qapp_project_area) %>% 
    dplyr::mutate(Acres = ifelse(Acres == 0.0,"<0.05",Acres)) %>% 
    dplyr::mutate(Percentage = ifelse(Percentage == 0.0,"<0.05",Percentage)) %>% 
    dplyr::arrange(desc(as.numeric(Acres))) %>% 
    dplyr::mutate(NLCD_Land = ifelse(is.na(NLCD_Land), "Open Water",NLCD_Land)) %>% 
    tidyr::drop_na(Stream)
  nlcd.text.pro.area <- nlcd.text %>% 
    dplyr::filter(Project_Na %in% qapp_project_area) %>% 
    dplyr::mutate(text = ifelse(text=="NA", "Open Water",text)) %>% 
    tidyr::drop_na(Stream)
  
  # _ DMA ----
  dma.pro.area <- dma.tbl %>% 
    dplyr::ungroup() %>% 
    dplyr::filter(Project_Na %in% qapp_project_area) %>% 
    dplyr::mutate(Acres = ifelse(Acres == 0.0,"<0.05",Acres)) %>% 
    dplyr::mutate(Percentage = ifelse(Percentage == 0.0,"<0.05",Percentage))%>% 
    dplyr::arrange(desc(as.numeric(Acres)))
  
  # _ Save Data ----
  # __ Data for QAPP documents ----
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
       pro.area.tmdls.total,
       pro.cat.45.tbl,
       flow.stations,
       flow.data.sample.count,
       inst.flow.pro.area,
       effective.shade.pro.area,
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
       #file = paste0("./data/",file.name,".RData"))
       file = paste0(data.dir.yg,file.name,"/mQAPPrmd/data/",file.name,".RData"))
  
  # __ Data for HTML maps ----
  save(#lookup.huc,
       #effective.shade,
       temp.data.sample.count,
       flow.data.sample.count,
       temp.stations,
       flow.stations,
       model.input,
       ncei.station.tbl,
       raws.station.tbl,
       agrimet.station.tbl,
       hydromet.station.tbl,
       mw.station.tbl,
       # npdes.ind.pro.area,
       npdes.gen.pro.area,
       pro.cat.45.tbl,
       file = paste0("E:/PROJECTS/20200810_RyanMichie_TempTMDLReplacement/TMDL_Maps/R_html_maps/", file.name,"_map.RData"))

  # __ Data output to Excel ----
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
                          station.worksheet.flow) %>% 
    tidyr::pivot_wider(names_from = Data, values_from = Track)  
  #dplyr::mutate(Temp = as.character(Temp),
  #              Flow = as.character(Flow),
  #              Temp_Worksheet = as.character(Temp_Worksheet),
  #              Flow_Worksheet = as.character(Flow_Worksheet))
  
  writexl::write_xlsx(list(Temp= temp.data.sample.count, 
                           Flow = flow.data.sample.count,
                           Stations = station.output),
                      path=paste0(data.dir,"appendix_data/",file.name,"_appendix_data.xlsx"))
  
}

# Leaflet Map Data ----
library(tidyverse)
library(httr)
library(geojsonsf)
library(sf)

# _ * data.dir ----
data.dir <- "//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/model_QAPPs/R/data/"
data.dir.yg <- "E:/PROJECTS/20200810_RyanMichie_TempTMDLReplacement/R/branches/" # Yuan's location
project.areas <- read.csv(paste0(data.dir,"qapp_project_areas.csv"))

pro_areas <- sf::st_read(dsn = paste0(data.dir,"gis/project_areas.shp"),
                         layer = "project_areas") %>% 
  sf::st_transform(4326) %>% 
  dplyr::mutate(color = dplyr::case_when(Project_Na == "John Day River Basin" ~ "#df65b0", #pink
                                         Project_Na == "Lower Grande Ronde, Imnaha, and Wallowa Subbasins" ~ "yellow",
                                         Project_Na == "Lower Willamette and Clackamas Subbasins" ~ "#253494", #blue
                                         Project_Na == "Malheur River Subbasins" ~ "#78c679", #green
                                         Project_Na == "Middle Willamette Subbasins" ~ "#253494", #blue
                                         Project_Na == "Middle Columbia-Hood, Miles Creeks" ~ "yellow",
                                         Project_Na == "North Umpqua Subbasin" ~ "purple",
                                         Project_Na == "Rogue River Basin" ~ "#df65b0", #pink
                                         Project_Na == "Sandy Subbasin" ~ "#253494", #blue
                                         Project_Na == "South Umpqua and Umpqua Subbasins" ~ "purple",
                                         Project_Na == "Southern Willamette Subbasins" ~ "#253494", #blue
                                         Project_Na == "Walla Walla Subbasin" ~ "#78c679", #green
                                         Project_Na == "Willow Creek Subbasin" ~ "#78c679")) %>%  #green
  dplyr::left_join(project.areas, by = c("Project_Na" = "areas")) %>% 
  #dplyr::mutate(CompleteD = format(as.Date(EPA.Approval,"%Y-%m-%d", tz="UTC"),"%b %d, %Y")) %>% 
  #dplyr::mutate(map_link = paste0("<a href='area_maps/'",file.name,".html'>",Project_Na,"</a>")) %>% 
  dplyr::mutate(map_link = paste0("<a href='area_maps/",file.name,".html'>",Project_Na,"</a>")) #%>% 
  #dplyr::arrange(EPA.Approval)

pro_reaches <- sf::st_read(dsn = "//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/GIS/willa_snake/TempTMDL_QAPP_Reaches.shp",
                           layer = "TempTMDL_QAPP_Reaches") %>% 
  sf::st_transform(4326) %>% 
  dplyr::mutate(color = dplyr::case_when(Project_Na == "Willamette River Mainstem and Major Tributaries"~ "purple",
                                         Project_Na == "Snake River – Hells Canyon"~ "yellow"))
#pro_reaches <- sf::st_zm(pro_reaches, drop = T, what = "ZM")

# _ Basin AUs ----
# au_rivers <- sf::st_read(dsn = "//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/GIS/AU_OR_Rivers_CoastLine_2022Final.shp",
#                            layer = "AU_OR_Rivers_CoastLine_2022Final") %>% sf::st_transform(4326)
# au_waterbodies <- sf::st_read(dsn = "//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/GIS/AU_OR_Waterbodies_2022Final.shp",
#                               layer = "AU_OR_Waterbodies_2022Final") %>% sf::st_transform(4326)
# au_watershed <- sf::st_read(dsn = "//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/GIS/AU_OR_Watershed_Area_2022Final.shp",
#                            layer = "AU_OR_Watershed_Area_2022Final") %>% sf::st_transform(4326)
# sf::sf_use_s2(FALSE)

au_rivers <- sf::st_read(dsn = "//deqhq1/GISLIBRARY/Base_Data/DEQ_Data/Water_Quality/WQ_Assessment/WQ_2022_IntegratedReport_FINAL/IR_2022_Final.gdb",
                         layer = "AU_OR_Rivers_CoastLine") #%>% sf::st_transform(4326) %>% sf::st_zm()
au_waterbodies <- sf::st_read(dsn = "//deqhq1/GISLIBRARY/Base_Data/DEQ_Data/Water_Quality/WQ_Assessment/WQ_2022_IntegratedReport_FINAL/IR_2022_Final.gdb",
                              layer = "AU_OR_Waterbodies") #%>% sf::st_transform(4326) %>% sf::st_zm()
au_watershed <- sf::st_read(dsn = "//deqhq1/GISLIBRARY/Base_Data/DEQ_Data/Water_Quality/WQ_Assessment/WQ_2022_IntegratedReport_FINAL/IR_2022_Final.gdb",
                            layer = "AU_OR_Watershed_Area") #%>% sf::st_transform(4326) %>% sf::st_zm()

wms.aus <- readxl::read_xlsx("//deqhq1/tmdl/TMDL_Willamette/Willamette_Mainstem_Temperature_2025/Project_Plans/Willamette_Mainstem_AUs_2022.04.15.xlsx",sheet = "Final_AUs")
wms.au.id <- wms.aus %>% dplyr::pull(AU_ID)
columbia_aus <- sf::st_read(dsn = "//deqhq1/tmdl/Planning statewide/TMDL_Priorities/2018_2020_IR/working_2020_2024",
                            layer="Columbia_River_AU_IDs",
                            stringsAsFactors=FALSE) %>%
  sf::st_drop_geometry()

# _ Model Extents ----
map_hs_temp_model_extent <- sf::st_read(dsn = paste0(data.dir, "gis/hs_temp_model_extent.shp"),
                                        layer = "hs_temp_model_extent")%>% 
  sf::st_transform(4326) %>% 
  sf::st_zm()

map_hs_solar_model_extent <- sf::st_read(dsn = paste0(data.dir, "gis/hs_solar_model_extent.shp"),
                                         layer = "hs_solar_model_extent")%>% 
  sf::st_transform(4326) %>% 
  sf::st_zm()

# Heat source solar model defined in area is only for the Southern Willamette Subbasins
map_hs_solar_model_area <- sf::st_read(dsn = "//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/GIS/shade_models/Southern_Willamette_ShadeModelArea.shp",
                                       layer = "Southern_Willamette_ShadeModelArea")%>% 
  sf::st_transform(4326) %>% 
  sf::st_zm()%>% 
  dplyr::mutate(Project_Na = "Southern Willamette Subbasins",
                Name = "Southern Willamette Subbasins Heat Source Solar Model Area")

map_ce_model_extent <- sf::st_read(dsn = paste0(data.dir, "gis/ce_model_extent_Willamette.shp"),
                                   layer = "ce_model_extent_Willamette")%>% 
  sf::st_transform(4326) %>% 
  sf::st_zm()

# Shadow model is only for the Rouge River Basin
map_sh_model_extent <- sf::st_read(dsn = paste0(data.dir, "gis/shade_model_streams_temp_projects_clean.shp"),
                                   layer = "shade_model_streams_temp_projects_clean")%>% 
  sf::st_transform(4326) %>% 
  sf::st_zm()

# map.tir_extent

# _ Effective shade ----
effective.shade <- readxl::read_xlsx(paste0(data.dir,"Effective_shade.xlsx"),sheet = "Effective_shade") %>% dplyr::filter(!`Result Status` == "REJECT")

# _ Project area map data ----
## for test:
# qapp_project_area = "John Day River Basin"
# qapp_project_area = "Lower Grande Ronde, Imnaha, and Wallowa Subbasins"
# qapp_project_area = "Lower Willamette and Clackamas Subbasins"
# qapp_project_area = "Malheur River Subbasins"
# qapp_project_area = "Middle Willamette Subbasins"
# qapp_project_area = "Middle Columbia-Hood, Miles Creeks"
# qapp_project_area = "North Umpqua Subbasin"
# qapp_project_area = "Rogue River Basin"
# qapp_project_area = "Sandy Subbasin"
# qapp_project_area = "South Umpqua and Umpqua Subbasins" ---
# qapp_project_area = "Southern Willamette Subbasins"
# qapp_project_area = "Walla Walla Subbasin"
# qapp_project_area = "Willamette River Mainstem and Major Tributaries" ---
# qapp_project_area = "Willow Creek Subbasin"

for (qapp_project_area in project.areas[which(!project.areas$areas %in% done),]$areas) {
  
  print(paste0(qapp_project_area, " map data..."))
  
  file.name <- project.areas[which(project.areas$areas %in% qapp_project_area),]$file.name
  #load(paste0("./data/lookup.RData"))
  load(paste0(data.dir.yg,file.name,"/mQAPPrmd/data/lookup.RData"))
  
  subbasin_huc8 <- unique(lookup.huc[which(lookup.huc$QAPP_Project_Area %in% qapp_project_area),]$HUC_8)
  subbasin_huc10 <- unique(lookup.huc[which(lookup.huc$QAPP_Project_Area %in% qapp_project_area),]$HUC10)
  subbasin_huc12 <- unique(lookup.huc[which(lookup.huc$QAPP_Project_Area %in% qapp_project_area),]$HUC12)
  
  pro_area <- pro_areas %>% 
    dplyr::filter(Project_Na %in% qapp_project_area)
  
  pro_scope_rivers <- au_rivers %>% sf::st_drop_geometry() %>% 
    dplyr::left_join(lookup.huc,by="HUC12") %>% 
    dplyr::filter(QAPP_Project_Area %in% qapp_project_area) %>% 
    dplyr::filter(!AU_ID %in% wms.au.id) %>% 
    dplyr::filter(!AU_ID %in% columbia_aus$AU_ID) %>% 
    dplyr::pull(AU_ID)
  pro_scope_waterbodies <- au_waterbodies %>% sf::st_drop_geometry() %>% 
    dplyr::left_join(lookup.huc,by="HUC12") %>% 
    dplyr::filter(QAPP_Project_Area %in% qapp_project_area) %>% 
    dplyr::filter(!AU_ID %in% wms.au.id) %>% 
    dplyr::filter(!AU_ID %in% columbia_aus$AU_ID) %>% 
    dplyr::pull(AU_ID)
  pro_scope_watershed <- au_watershed %>% sf::st_drop_geometry() %>% 
    dplyr::left_join(lookup.huc,by="HUC12") %>% 
    dplyr::filter(QAPP_Project_Area %in% qapp_project_area) %>% 
    dplyr::filter(!AU_ID %in% wms.au.id) %>% 
    dplyr::filter(!AU_ID %in% columbia_aus$AU_ID) %>% 
    dplyr::pull(AU_ID)
  
  hs_temp_model_extent <- map_hs_temp_model_extent %>% 
    dplyr::filter(Project_Na %in% qapp_project_area)
  
  hs_solar_model_extent <- map_hs_solar_model_extent %>% 
    dplyr::filter(Project_Na %in% qapp_project_area)
  
  hs_solar_model_area <-  map_hs_solar_model_area %>% 
    dplyr::filter(Project_Na %in% qapp_project_area)
  
  ce_model_extent <- map_ce_model_extent %>% 
    dplyr::filter(Project_Na %in% qapp_project_area)
  
  sh_model_extent <- map_sh_model_extent %>% 
    dplyr::filter(Project_Na %in% qapp_project_area)
  
  #tir_extent
  
  # effective shade
  effective.shade.pro.area <- effective.shade %>% dplyr::filter(`Project Area` %in% qapp_project_area)
  
  # _ Save Data ----
  save(pro_area,
       pro_scope_rivers,
       pro_scope_waterbodies,
       pro_scope_watershed,
       hs_temp_model_extent,
       hs_solar_model_extent,
       hs_solar_model_area,
       ce_model_extent,
       sh_model_extent,
       #tir_extent,
       effective.shade.pro.area,
       pro.cat.45.tbl,
       #file = paste0("./data/map_",file.name,".RData"))
       file = paste0(data.dir.yg,file.name,"/mQAPPrmd/data/map_",file.name,".RData"))
  
  
}
