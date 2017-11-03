RAWmisc::AllowFileManipulationFromInitialiseProject()
RAWmisc::InitialiseProject(
  HOME = "/git/code_major/2017/lamprini_momir_sample_size/",
  RAW = "/analyses/data_raw/code_major/2017/lamprini_momir_sample_size/",
  CLEAN = "/analyses/data_clean/code_major/2017/lamprini_momir_sample_size",
  BAKED = "/analyses/results_baked/code_major/2017/lamprini_momir_sample_size/",
  FINAL = "/analyses/results_final/code_major/2017/lamprini_momir_sample_size/",
  SHARED = "/dropbox/results_shared/code_major/2017/lamprini_momir_sample_size/")

rmarkdown::render(input = "lamprini_momir_sample_size.Rmd", output_file = "lamprini_momir_sample_size.pdf", 
                  output_dir = RAWmisc::PROJ$SHARED_TODAY, output_format = rmarkdown::pdf_document(toc=F))

