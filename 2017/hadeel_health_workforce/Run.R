RAWmisc::AllowFileManipulationFromInitialiseProject()
RAWmisc::InitialiseProject(
  HOME = "/git/code_major/2017/hadeel_health_workforce/",
  RAW = "/dropbox/data_raw/code_major/2017/hadeel_health_workforce/",
  CLEAN = "/analyses/data_clean/code_major/2017/hadeel_health_workforce/",
  BAKED = "/analyses/results_baked/code_major/2017/hadeel_health_workforce/",
  FINAL = "/analyses/results_final/code_major/2017/hadeel_health_workforce/",
  SHARED = "/dropbox/data_raw/code_major/2017/hadeel_health_workforce/results/")

#unlink(file.path(RAWmisc::PROJ$SHARED_TODAY,sprintf("health_workforce_%s.pdf",lubridate::today())))
rmarkdown::render(input = "hadeel_health_workforce.Rmd", output_file = sprintf("health_workforce_%s.pdf",lubridate::today()), 
                  output_dir = RAWmisc::PROJ$SHARED_TODAY, output_format = rmarkdown::pdf_document(toc=TRUE))


