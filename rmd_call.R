setwd("E:/PROJECTS/20200810_RyanMichie_TempTMDLReplacement/mQAPPrmd")

output.dir <- "E:/PROJECTS/20200810_RyanMichie_TempTMDLReplacement/mQAPPrmd/markdown/"

for (qapp_project_area in qapp_project_areas$areas) {
  
  file.name <- qapp_project_areas[which(qapp_project_areas$areas == qapp_project_area),]$file.name
  load(paste0(file.name,".RData"))
  
  rmarkdown::render(input="E:/PROJECTS/20200810_RyanMichie_TempTMDLReplacement/mQAPPrmd/model_QAPP.Rmd",
                    output_format = "word_document",
                    output_dir = output.dir,
                    output_file=paste0("QAPP_",file.name, "_", Sys.Date(),".docx"))
  
}
