RAWmisc::InitialiseProject(
  HOME = "/analyses/code_major/normomo/",
  RAW = "/analyses/data_raw/normomo/",
  CLEAN = "/analyses/data_clean/normomo",
  BAKED = "/analyses/results_baked/normomo/",
  FINAL = "/analyses/results_final/normomo/",
  SHARED = "/dropbox/results_shared/normomo/")

rmarkdown::render(input = "normomo.Rmd", output_file = "normomo.pdf", 
                  output_dir = RAWmisc::PROJ$SHARED_TODAY, output_format = rmarkdown::pdf_document())

