if(.Platform$OS.type=="unix"){
  RAWmisc::UseRClone()
  RAWmisc::AllowFileManipulationFromInitialiseProject()
  
  if(dir.exists("/dropbox")){
    SHARED <- "/dropbox/analyses/results_shared/code_major/2018/eaec"
    RCLONE_SHARED <- NULL
  } else {
    SHARED <- "/tmp/results_shared/code_major/2018/eaec/"
    RCLONE_SHARED <- "data:/analyses/results_shared/code_major/2018/eaec/"
  }
  
  RAWmisc::InitialiseProject(
    HOME = "/git/code_major/2018/eaec/",
    RAW = "/tmp/data_raw/code_major/2018/eaec/",
    CLEAN = "/tmp/data_clean/code_major/2018/eaec",
    BAKED = "/tmp/results_baked/code_major/2018/eaec/",
    FINAL = "/tmp/results_final/code_major/2018/eaec/",
    SHARED = SHARED,
    RCLONE_RAW = "crypt:/data_raw/code_major/2018/eaec/",
    RCLONE_SHARED = RCLONE_SHARED
  )
}

library(data.table)

rmarkdown::render(input = "eaec.Rmd", output_file = sprintf("eaec_%s.pdf",lubridate::today()), 
                  output_dir = RAWmisc::PROJ$SHARED_TODAY, output_format = rmarkdown::pdf_document(toc=TRUE))




RAWmisc::SaveProject()