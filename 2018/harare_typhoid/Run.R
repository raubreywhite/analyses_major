RAWmisc::InitialiseOpinionatedUnix("code_major/2018/harare_typhoid")

rmarkdown::render(input = "prelim.Rmd",
                  output_file = sprintf("%s_harare.pdf",lubridate::today()), 
                  output_dir = file.path(RAWmisc::PROJ$SHARED_TODAY)
)
