RAWmisc::AllowFileManipulationFromInitialiseProject()
RAWmisc::InitialiseProject(
  HOME = "/git/code_major/2017/dag_mouse_radiation/",
  RAW = "/analyses/data_raw/code_major/2017/dag_mouse_radiation/",
  CLEAN = "/analyses/data_clean/code_major/2017/dag_mouse_radiation",
  BAKED = "/analyses/results_baked/code_major/2017/dag_mouse_radiation/",
  FINAL = "/analyses/results_final/code_major/2017/dag_mouse_radiation/",
  SHARED = "/dropbox/results_shared/code_major/2017/dag_mouse_radiation/")

rmarkdown::render(input = "dag_mouse_radiation.Rmd", output_file = "dag_mouse_radiation.pdf", 
                  output_dir = RAWmisc::PROJ$SHARED_TODAY, output_format = rmarkdown::pdf_document(toc=F))

