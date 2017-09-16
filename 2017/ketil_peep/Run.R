RAWmisc::InitialiseProject(
  HOME = "/analyses/code_major/ketil_peep/",
  RAW = "/analyses/data_raw/ketil_peep/",
  CLEAN = "/analyses/data_clean/ketil_peep",
  BAKED = "/analyses/results_baked/ketil_peep/",
  FINAL = "/analyses/results_final/ketil_peep/",
  SHARED = "/dropbox/results_shared/ketil_peep/")

rmarkdown::render(input = "peep_interim_white.Rmd", output_file = "peep_interim_white.pdf", 
                  output_dir = RAWmisc::PROJ$SHARED_TODAY, output_format = rmarkdown::pdf_document())

