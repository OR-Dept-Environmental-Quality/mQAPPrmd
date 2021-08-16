# The script organizes the data that are used for the Source characteristics section.
# The outputs include :
# -GIS shapefile of DMAs/RPs within 100 meters of the model extent stream centerline
# -Rdata file summarizing for each DMA the total area (acres) and the percentage of area within 
#  100 meters of the model extent stream centerline
# -Rdata file summarizing for each NLCD landcover class the total area (acres) and the percentage of area within 
#  100 meters of the model extent stream centerline

library(sf)
library(rgdal)
library(raster)
library(dplyr)
library(units)
library(foreign)
library(stringr)

output_gis_dir <- "//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/model_QAPPs/R/data/gis/"
output_rdata_dir <- "//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/model_QAPPs/R/data/RData/"

# List of County DMA shapefiles
dma_dir <- "//deqhq1/TMDL/DMA_Mapping/Final/"
dmaGIS <- list(
  "Baker"=paste0(dma_dir, "Baker_DMAs_2019-1"),
  "Benton"=paste0(dma_dir, "Benton_DMAs_2019-1"),
  "Clackamas"=paste0(dma_dir, "Clackamas_DMAs_2019-1"),
  "Columbia"=paste0(dma_dir, "Columbia_DMAs_2019-1"),
  "Curry"=paste0(dma_dir, "Curry_DMAs_2019-1"),
  "Douglas"=paste0(dma_dir, "Douglas_DMAs_2017-1"),
  "Gilliam"=paste0(dma_dir, "Gilliam_DMAs_2020-1"),
  "Grant"=paste0(dma_dir, "Grant_DMAs_2020-1"),
  "Harney"=paste0(dma_dir, "Harney_DMAs_2019-1"),
  "Hood River"=paste0(dma_dir, "HoodRiver_DMAs_2020-1"),
  "Jackson"=paste0(dma_dir, "Jackson_DMAs_2019-1"),
  "Jefferson"=paste0(dma_dir, "Jefferson_DMAs_2019-1"),
  "Josephine"=paste0(dma_dir, "Josephine_DMAs_2019-1"),
  "Lane"=paste0(dma_dir, "Lane_DMAs_2019-1"),
  "Linn"=paste0(dma_dir, "Linn_DMAs_2019-1"),
  "Malheur"=paste0(dma_dir, "Malheur_DMAs_2020-1"),
  "Marion"=paste0(dma_dir, "Marion_DMAs_2019-1"),
  "Morrow"=paste0(dma_dir, "Morrow_DMAs_2019-1"),
  "Multnomah"=paste0(dma_dir, "Multnomah_DMAs_2019-1"),
  "Polk"=paste0(dma_dir, "Polk_DMAs_2019-1"),
  "Sherman"=paste0(dma_dir, "Sherman_DMAs_2020-1"),
  "Umatilla"=paste0(dma_dir, "Umatilla_DMAs_2019-1"),
  "Union"=paste0(dma_dir, "Union_DMAs_2019-1"),
  "Wallowa"=paste0(dma_dir, "Wallowa_DMAs_2019-1"),
  "Wasco"=paste0(dma_dir, "Wasco_DMAs_2019-1"),
  "Wheeler"=paste0(dma_dir, "Wheeler_DMAs_2019-1"),
  "Yamhill"=paste0(dma_dir, "Yamhill_DMAs_2019-1"))

# Read in 2016 NLCD raster
nlcd_lay <- "//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/model_QAPPs/R/data/gis/NLCD_2016/NLCD_2016_Land_Cover_L48_20190424_Oregon.tif"
nlcd <- raster::raster(nlcd_lay)

# Read attribute table with values and landcover types
nlcd_df <- "//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/model_QAPPs/R/data/gis/NLCD_2016/NLCD_2016_Land_Cover_L48_20190424_Oregon.tif.vat.dbf"
nlcd_df <- foreign::read.dbf(nlcd_df, as.is = TRUE)

# Read in Model polyline extents
map_ce_model_extent <- sf::st_read(dsn = "//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/model_QAPPs/R/data/gis",
                                   layer = "ce_model_extent", stringsAsFactors = FALSE) %>% 
  sf::st_transform(2992) %>% 
  sf::st_zm() %>%
  dplyr::select(Stream, Project_Na)

map_hs_temp_model_extent <- sf::st_read(dsn = "//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/model_QAPPs/R/data/gis",
                                        layer = "hs_temp_model_extent", stringsAsFactors = FALSE) %>%
  sf::st_transform(2992) %>% 
  sf::st_zm() %>% 
  dplyr::select(Stream, Project_Na) %>% 
  dplyr::mutate(Stream = dplyr::case_when(Stream == "Sandy River" ~ "Sandy River (2001)",
                                          Stream == "Bull Run River" ~ "Bull Run River (2001)", 
                                          TRUE ~ Stream))

map_hs_solar_model_extent <- sf::st_read(dsn = "//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/model_QAPPs/R/data/gis",
                                         layer = "hs_solar_model_extent", stringsAsFactors = FALSE) %>% 
  sf::st_transform(2992) %>% 
  sf::st_zm() %>%
  dplyr::select(Stream, Project_Na)

map_hs_sandy_2016 <- sf::st_read(dsn = "//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/model_QAPPs/R/data/gis/sandy_2016.shp",
                                 layer = "sandy_2016") %>% 
  sf::st_transform(2992) %>% 
  sf::st_zm() %>%
  dplyr::select(Stream, Project_Na)


map_hs_fish_creek_2009 <- sf::st_read(dsn = "//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/model_QAPPs/R/data/gis/fish_creek_2009.shp",
                                      layer = "fish_creek_2009")  %>% 
  sf::st_transform(2992) %>% 
  sf::st_zm() %>%
  dplyr::select(Stream, Project_Na)

# Combine into one feature and dissolve by stream and project area
model_extents <- rbind(map_hs_temp_model_extent, map_hs_solar_model_extent, map_ce_model_extent, map_hs_sandy_2016, map_hs_fish_creek_2009) %>%
  dplyr::group_by(Stream, Project_Na) %>%
  dplyr::summarise()

# check
sf::st_write(model_extents, paste0(output_gis_dir,"model_extents.shp"), delete_layer=TRUE)

# rm(map_ce_model_extent, map_hs_temp_model_extent, map_hs_solar_model_extent)

# Read in county outline feature
county_shp <- sf::st_read("//deqhq1/TMDL/DMA_Mapping/Master/GIS", layer = "orcnty24", stringsAsFactors = FALSE) %>% 
  sf::st_transform(2992) %>% 
  dplyr::select(County=COUNTY_NAM) %>%
  dplyr::mutate(County=stringr::str_to_title(County))

# Buffer streams by 100 m
model_buff <- sf::st_buffer(model_extents, dist = units::set_units(100, m))

sf::st_write(model_buff, paste0(output_gis_dir,"model_buff.shp"), delete_layer=TRUE)

# intersect stream buffer with counties. The purpose is the get a list of 
# all the counties that need to be loaded for each stream
model_buff_county <- sf::st_intersection(x=model_buff, y=county_shp) %>%
  dplyr::group_by(Stream, Project_Na, County) %>%
  dplyr::summarise()

sort(unique(model_buff_county$County))

# -- NLCD land cover area for along the model extent ---------------------------

# Transform the stream into the projection of the NLCD (Albers)
model_buff_nlcd <- sf::st_transform(model_buff, crs = sf::st_crs(nlcd))

# check
sf::st_write(model_buff_nlcd, paste0(output_gis_dir,"model_buff_nlcd.shp"), delete_layer=TRUE)

# intersect stream buffer with NCLD, only return a data frame
extract_fun <- function(y, x=nlcd) {
  df <- raster::extract(x=x, y=y, df=TRUE)
  df$Stream <- unique(y$Stream)
  df$Project_Na <- unique(y$Project_Na)
  return(df)
}

nlcd_buff <- model_buff_nlcd %>% 
  dplyr::group_by(Stream, Project_Na) %>%
  dplyr::group_split(.keep = TRUE) %>%
  lapply(FUN = extract_fun) %>%
  dplyr::bind_rows()

# Summarize by each landcover type, excluding open water (value=11) 
# Total Acres in buffer and percentage for each stream
# correct NLCD spelling of Herbaceous
nlcd.tbl <- nlcd_buff %>%
  dplyr::rename(Value=NLCD_2016_Land_Cover_L48_20190424_Oregon) %>%
  dplyr::left_join(nlcd_df) %>%
  dplyr::filter(!Value == 11) %>%
  dplyr::mutate(NLCD_Land=dplyr::case_when(Value==71 ~ "Herbaceous",
                                           Value==95 ~ "Emergent Herbaceous Wetlands",
                                           TRUE ~ NLCD_Land)) %>%
  dplyr::group_by(Stream, Project_Na, Value, NLCD_Land) %>%
  dplyr::summarise(cell_count=n()) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(Stream, Project_Na, Value) %>%
  dplyr::mutate(Acres=round(cell_count*900*0.000247105,1)) %>%
  dplyr::group_by(Stream, Project_Na) %>%
  dplyr::mutate(Percentage=round(Acres/sum(Acres)*100, 1))

sum(nlcd.tbl$Percentage)

# Reclass the NLCD attribute table for easy interpretation for QAPP narrative
# If the project area is in E. Oregon certain NLCD values are 
# classified as Rangeland instead of Forest
# The Majority identifies if the land cover classes that are the largest and 
# make up 50% or more of the total area. Any single class over 20% is also included.
# Only majority is mentioned in the QAPP narrative.
nlcd.text.tbl <- nlcd_buff %>%
  dplyr::rename(Value=NLCD_2016_Land_Cover_L48_20190424_Oregon) %>%
  dplyr::left_join(nlcd_df) %>%
  dplyr::filter(!Value == 11) %>%
  dplyr::mutate(shrubRC=dplyr::case_when(Project_Na %in% c("John Day River Basin", 
                                                           "Lower Grande Ronde, Imnaha, and Wallowa Subbasins",
                                                           "Malheur River Subbasins",
                                                           "Walla Walla Subbasin",
                                                           "Willow Creek Subbasin") ~ "rangeland",
                                         TRUE ~ "forestry"),
                General_Landcover=dplyr::case_when(Value %in% c(21, 22, 23, 24) ~ "developed areas",
                                                   Value %in% c(41, 42, 43, 90) ~ "forestry",
                                                   Value %in% c(81, 82) ~ "agriculture",
                                                   Value %in% c(81, 82) ~ "agriculture",
                                                   Value %in% c(95) ~ "emergent herbaceous wetlands",
                                                   Value %in% c(52, 71) ~ shrubRC,
                                                   TRUE ~ NLCD_Land)) %>%
  dplyr::group_by(Stream, Project_Na, General_Landcover) %>%
  dplyr::summarise(cell_count=n()) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(Acres=round(cell_count*900*0.000247105,1)) %>%
  dplyr::group_by(Stream, Project_Na) %>%
  dplyr::mutate(Acres_Total=sum(Acres),
                Percentage=round(Acres/Acres_Total*100, 1)) %>%
  dplyr::arrange(Stream, Project_Na, dplyr::desc(Percentage)) %>%
  dplyr::mutate(CumPercent=cumsum(Percentage),
                Rank=dplyr::row_number(),
                Majority=dplyr::case_when(Rank==1 ~ TRUE,
                                          Rank >1 & lag(CumPercent) <=50 ~ TRUE,
                                          Rank >1 & lag(CumPercent) >50 & Percentage >=20 ~ TRUE,
                                          TRUE ~ FALSE))

sum(nlcd.text.tbl$Percentage)

# Build the NLCD narrative and info needed for QAPP,
# The gsub replaces the appropriate comma with a ", and"
nlcd.text <- nlcd.text.tbl %>%
  dplyr::filter(Majority) %>%
  dplyr::group_by(Stream, Project_Na) %>%
  dplyr::summarise(n=dplyr::n(),
                   MajorityPercent=round(sum(Percentage, na.rm = TRUE),0),
                   text=paste0(General_Landcover, collapse = ", ")) %>%
  dplyr::mutate(text=dplyr::case_when(n==2 ~ gsub("((?:[^,]+, ){0}[^,]+),", "\\1 and", text),
                                      n==3 ~ gsub("((?:[^,]+, ){1}[^,]+),", "\\1, and", text),
                                      TRUE ~ text))

save(nlcd.tbl, file = paste0(output_rdata_dir,"nlcd.tbl.RData"))
save(nlcd.text, file = paste0(output_rdata_dir,"nlcd.text.RData"))

# -- DMA area along the model extent -----------------------------------------

dma_summary <- function(stream_buffer, dmaGIS=dmaGIS) {
  
  county_name <- unique(stream_buffer$County)
  
  dmapath <- dmaGIS[[county_name]]
  
  print(paste0("Loading ", county_name))
  start_time <- Sys.time()
  
  # Read in DMA features
  dma.shp <- sf::read_sf(dsn = dirname(dmapath), layer = basename(dmapath)) %>% 
    dplyr::mutate(Taxlot = as.character(Taxlot),
                  RailOwner = as.character(RailOwner),
                  Tribe = as.character(Tribe))
  
  end_time <- Sys.time()
  print(paste0("Done loading ",county_name, " ", round(as.numeric(end_time - start_time, units = "mins"), 1)," minutes"))
  
  # clean up potential topology errors in DMA shapefile
  dma.shp2 <- sf::st_buffer(dma.shp, dist = 0) 
  
  # Only Keep certain fields
  dma.shp2 <- dma.shp %>%
    dplyr::select(-DMA_RP2_Ab)
  
  # Transform the stream buffer into the projection of the dma feature
  stream_buffer2 <- sf::st_transform(stream_buffer, crs = sf::st_crs(dma.shp))
  
  # intersect and union the stream buffer with DMA
  dma_buff <- sf::st_intersection(x=dma.shp2, y=stream_buffer2) %>% 
    sf::st_transform(2992)
  
  # compute polygon areas in units of DMA projection (feet)
  dma_buff$area <- sf::st_area(dma_buff)
  
  # Convert area to Acres
  dma_buff$Acres=round(as.numeric(set_units(dma_buff$area, "acres")), 1)
  
  return(dma_buff)
  
}

model_buff_dma_rp <- model_buff_county %>% 
  dplyr::group_by(County) %>%
  dplyr::group_split(.keep = TRUE) %>%
  lapply(FUN = dma_summary, dmaGIS=dmaGIS) %>%
  dplyr::bind_rows()

# This is a temporary fix to correct the names for some of 
# the DMAs in the DMA_RP column.
dmaLU <- read.csv("//deqhq1/TMDL/DMA_Mapping/Master/Lookups/DMAs.csv") %>%
  dplyr::select(DMA_RP=DMA_FullName, DMA_RP_Ab=DMA)

model_buff_dma <- model_buff_dma_rp %>%
  dplyr::select(-DMA_RP) %>%
  dplyr::left_join(dmaLU)

# Output to shapefile
sf::st_write(model_buff_dma , paste0(output_gis_dir,"model_buff_dma.shp"), delete_layer=TRUE)

# Summarize by DMA/RP. Total Acres in buffer and percentage
dma.tbl <- model_buff_dma %>%
  sf::st_drop_geometry() %>%
  dplyr::group_by(Stream, Project_Na, DMA_RP) %>%
  dplyr::summarise(Acres=sum(Acres)) %>%
  dplyr::group_by(Stream, Project_Na) %>%
  dplyr::mutate(Percentage=round(Acres/sum(Acres)*100, 1)) %>%
  dplyr::arrange(Stream, Project_Na, dplyr::desc(Percentage)) %>%
  dplyr::mutate(DMA_RP=dplyr::case_when(DMA_RP=="Water" ~ "Oregon Department of State Lands - Waterway",
                                        DMA_RP=="Oregon Department of Forestry - Private" ~ "Oregon Department of Forestry - Private Forestland",
                                        DMA_RP=="Oregon Department of Forestry - Public" ~ "Oregon Department of Forestry - State Forestland",
                                        TRUE ~ DMA_RP))

sum(dma.tbl$Percentage)

save(dma.tbl, file = paste0(output_rdata_dir, "dmas.RData"))

