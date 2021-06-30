library(tidyverse)
library(readxl)
library(writexl)

data.dir <- "//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/model_QAPPs/R/data/"
project.areas <- read.csv(paste0(data.dir,"qapp_project_areas.csv"))

tbl.sum <- NULL

for (qapp_project_area in unique(sort(project.areas$areas))) {
  
  # test: qapp_project_area <- "John Day River Basin"
  print(qapp_project_area)
  
  file.name <- project.areas[which(project.areas$areas == qapp_project_area),]$file.name
  
  tbl <- readxl::read_xlsx(paste0(data.dir,"appendix_data/",file.name,"_appendix_data.xlsx"), sheet = "Stations") %>% 
    dplyr::mutate(QAPP = qapp_project_area)
  
  tbl.sum <- dplyr::bind_rows(tbl,tbl.sum)
  
}

writexl::write_xlsx(tbl.sum,
                    path=paste0(data.dir,"appendix_data/stations.xlsx"))

