RAWmisc::AllowFileManipulationFromInitialiseProject()
RAWmisc::InitialiseProject(
  HOME = "/git/code_major/2017/ketil_peep/",
  RAW = "/analyses/data_raw/code_major/2017/ketil_peep/",
  CLEAN = "/analyses/data_clean/code_major/2017/ketil_peep",
  BAKED = "/analyses/results_baked/code_major/2017/ketil_peep/",
  FINAL = "/analyses/results_final/code_major/2017/ketil_peep/",
  SHARED = "/dropbox/results_shared/code_major/2017/ketil_peep/")

rmarkdown::render(input = "peep_interim_white.Rmd", output_file = "peep_interim_white.pdf", 
                  output_dir = RAWmisc::PROJ$SHARED_TODAY, output_format = rmarkdown::pdf_document())

