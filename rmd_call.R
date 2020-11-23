library(rmarkdown)

setwd("//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/model_QAPPs/R/data/RData")

data.dir <- "//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/model_QAPPs/R/data/"
output.dir <- "//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/model_QAPPs/R/test_doc/20201123/"

qapp_project_areas <- read.csv(paste0(data.dir,"qapp_project_area.csv"))

## for test:
# qapp_project_area = "Applegate, Illinois, Lower Rogue, and Middle Rogue Subbasins"
# qapp_project_area = "John Day River Basin"
# qapp_project_area = "Lower Grande Ronde, Imnaha, and Wallowa Subbasins"
# qapp_project_area = "Lower Willamette, Clackamas, and Sandy Subbasins"
# qapp_project_area = "Malheur River Subbasins"
# qapp_project_area = "Mid Willamette Subbasins"
# qapp_project_area = "Middle Columbia-Hood, Miles Creeks"
# qapp_project_area = "North Umpqua Subbasin"
# qapp_project_area = "South Umpqua and Umpqua Subbasins"
# qapp_project_area = "Southern Willamette Subbasins"
# qapp_project_area = "Upper Rogue Subbasin"
# qapp_project_area = "Walla Walla Subbasin"
# qapp_project_area = "Willamette River Mainstem and Major Tributaries"
# qapp_project_area = "Willow Creek Subbasin"

for (qapp_project_area in qapp_project_areas$areas) {
  
  file.name <- qapp_project_areas[which(qapp_project_areas$areas == qapp_project_area),]$file.name
  load(paste0(file.name,".RData"))
  
  rmarkdown::render(input="E:/PROJECTS/20200810_RyanMichie_TempTMDLReplacement/mQAPPrmd/model_QAPP.Rmd",
                    output_format = "word_document",
                    output_dir = output.dir,
                    output_file=paste0("QAPP_",file.name, "_", Sys.Date(),".docx"))
  
}
