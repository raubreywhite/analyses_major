org::AllowFileManipulationFromInitialiseProject()
org::InitialiseProject(
  HOME = "/git/code_major/2019/advisory/",
  SHARED = "/dropbox/analyses/results_shared/code_major/2019/advisory/",
  RAW = "/data/org/data_raw/code_major/2019/advisory/"
)

rmarkdown::render(
  input = fs::path(org::PROJ$HOME,"advisory.rmd"),
  output_dir = org::PROJ$SHARED_TODAY
  )

