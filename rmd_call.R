library(rmarkdown)

data.dir <- "//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/model_QAPPs/R/data/"
output.dir <- "//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/model_QAPPs/R/test_doc/202201"

project.areas <- read.csv(paste0(data.dir,"qapp_project_areas.csv"))

## for test:
# qapp_project_area = "John Day River Basin"
# qapp_project_area = "Lower Grande Ronde, Imnaha, and Wallowa Subbasins"
qapp_project_area = "Lower Willamette and Clackamas Subbasins"
# qapp_project_area = "Malheur River Subbasins"
# qapp_project_area = "Middle Willamette Subbasins"
# qapp_project_area = "Middle Columbia-Hood, Miles Creeks"
# qapp_project_area = "North Umpqua Subbasin"
# qapp_project_area = "Rogue River Basin"
# qapp_project_area = "Sandy Subbasin"
# qapp_project_area = "South Umpqua and Umpqua Subbasins"
# qapp_project_area = "Southern Willamette Subbasins"
# qapp_project_area = "Walla Walla Subbasin"
# qapp_project_area = "Willamette River Mainstem and Major Tributaries"
# qapp_project_area = "Willow Creek Subbasin"

#for (qapp_project_area in project.areas$areas) {

print(qapp_project_area)

file.name <- project.areas[which(project.areas$areas == qapp_project_area),]$file.name
load(paste0("./data/",file.name,".RData"))

rmarkdown::render(input=paste0("model_QAPP.Rmd"),
                  output_format = "word_document",
                  output_dir = output.dir,
                  output_file=paste0("QAPP_",file.name, ".docx"))

#}
