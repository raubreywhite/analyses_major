RAWmisc::AllowFileManipulationFromInitialiseProject()
RAWmisc::InitialiseProject(
  HOME = "/git/code_major/2017/hadeel_health_workforce/",
  RAW = "/dropbox/analyses/data_raw/code_major/2017/hadeel_health_workforce/",
  CLEAN = "/analyses/data_clean/code_major/2017/hadeel_health_workforce/",
  BAKED = "/analyses/results_baked/code_major/2017/hadeel_health_workforce/",
  FINAL = "/analyses/results_final/code_major/2017/hadeel_health_workforce/",
  SHARED = "/dropbox/analyses/data_raw/code_major/2017/hadeel_health_workforce/results/")

#unlink(file.path(RAWmisc::PROJ$SHARED_TODAY,sprintf("health_workforce_%s.pdf",lubridate::today())))

dir.create(file.path(RAWmisc::PROJ$SHARED_TODAY,"without8000_NOTremoving1000"))
dir.create(file.path(RAWmisc::PROJ$SHARED_TODAY,"without8000_removing1000"))
dir.create(file.path(RAWmisc::PROJ$SHARED_TODAY,"with8000_NOTremoving1000"))
dir.create(file.path(RAWmisc::PROJ$SHARED_TODAY,"with8000_removing1000"))

dir.create(file.path(RAWmisc::PROJ$SHARED_TODAY,"without8000_NOTremoving1000","CW00"))
dir.create(file.path(RAWmisc::PROJ$SHARED_TODAY,"without8000_removing1000","CW00"))
dir.create(file.path(RAWmisc::PROJ$SHARED_TODAY,"with8000_NOTremoving1000","CW00"))
dir.create(file.path(RAWmisc::PROJ$SHARED_TODAY,"with8000_removing1000","CW00"))

dir.create(file.path(RAWmisc::PROJ$SHARED_TODAY,"without8000_NOTremoving1000","CW00_AND_CW06"))
dir.create(file.path(RAWmisc::PROJ$SHARED_TODAY,"without8000_removing1000","CW00_AND_CW06"))
dir.create(file.path(RAWmisc::PROJ$SHARED_TODAY,"with8000_NOTremoving1000","CW00_AND_CW06"))
dir.create(file.path(RAWmisc::PROJ$SHARED_TODAY,"with8000_removing1000","CW00_AND_CW06"))

rmarkdown::render(input = "hadeel_health_workforce.Rmd",
                  output_file = sprintf("CW00_with8000_removing1000_health_workforce_%s.pdf",lubridate::today()), 
                  output_dir = file.path(RAWmisc::PROJ$SHARED_TODAY,"with8000_removing1000","CW00"), output_format = rmarkdown::pdf_document(toc=TRUE),
                  params = list(EXTRA_8000 = TRUE, CW00_AND_CW06 = FALSE))

rmarkdown::render(input = "hadeel_health_workforce.Rmd",
                  output_file = sprintf("CW00_without8000_NOTremoving1000_health_workforce_%s.pdf",lubridate::today()), 
                  output_dir = file.path(RAWmisc::PROJ$SHARED_TODAY,"without8000_NOTremoving1000","CW00"), output_format = rmarkdown::pdf_document(toc=TRUE),
                  params = list(EXTRA_8000 = FALSE, REMOVE_1000 = FALSE, CW00_AND_CW06 = FALSE))

rmarkdown::render(input = "hadeel_health_workforce.Rmd", 
                  output_file = sprintf("CW00_without8000_removing1000_health_workforce_%s.pdf",lubridate::today()), 
                  output_dir = file.path(RAWmisc::PROJ$SHARED_TODAY,"without8000_removing1000","CW00"), output_format = rmarkdown::pdf_document(toc=TRUE),
                  params = list(EXTRA_8000 = FALSE, CW00_AND_CW06 = FALSE))

rmarkdown::render(input = "hadeel_health_workforce.Rmd",
                  output_file = sprintf("CW00_with8000_NOTremoving1000_health_workforce_%s.pdf",lubridate::today()), 
                  output_dir = file.path(RAWmisc::PROJ$SHARED_TODAY,"with8000_NOTremoving1000","CW00"), output_format = rmarkdown::pdf_document(toc=TRUE),
                  params = list(EXTRA_8000 = TRUE, REMOVE_1000 = FALSE, CW00_AND_CW06 = FALSE))


# CW00 and CW06
rmarkdown::render(input = "hadeel_health_workforce.Rmd",
                  output_file = sprintf("CW00_AND_CW06_with8000_removing1000_health_workforce_%s.pdf",lubridate::today()), 
                  output_dir = file.path(RAWmisc::PROJ$SHARED_TODAY,"with8000_removing1000","CW00_AND_CW06"), output_format = rmarkdown::pdf_document(toc=TRUE),
                  params = list(EXTRA_8000 = TRUE, CW00_AND_CW06 = TRUE))

rmarkdown::render(input = "hadeel_health_workforce.Rmd",
                  output_file = sprintf("CW00_AND_CW06_without8000_NOTremoving1000_health_workforce_%s.pdf",lubridate::today()), 
                  output_dir = file.path(RAWmisc::PROJ$SHARED_TODAY,"without8000_NOTremoving1000","CW00_AND_CW06"), output_format = rmarkdown::pdf_document(toc=TRUE),
                  params = list(EXTRA_8000 = FALSE, REMOVE_1000 = FALSE, CW00_AND_CW06 = TRUE))

rmarkdown::render(input = "hadeel_health_workforce.Rmd", 
                  output_file = sprintf("CW00_AND_CW06_without8000_removing1000_health_workforce_%s.pdf",lubridate::today()), 
                  output_dir = file.path(RAWmisc::PROJ$SHARED_TODAY,"without8000_removing1000","CW00_AND_CW06"), output_format = rmarkdown::pdf_document(toc=TRUE),
                  params = list(EXTRA_8000 = FALSE, CW00_AND_CW06 = TRUE))

rmarkdown::render(input = "hadeel_health_workforce.Rmd",
                  output_file = sprintf("CW00_AND_CW06_with8000_NOTremoving1000_health_workforce_%s.pdf",lubridate::today()), 
                  output_dir = file.path(RAWmisc::PROJ$SHARED_TODAY,"with8000_NOTremoving1000","CW00_AND_CW06"), output_format = rmarkdown::pdf_document(toc=TRUE),
                  params = list(EXTRA_8000 = TRUE, REMOVE_1000 = FALSE, CW00_AND_CW06 = TRUE))


Folders:
- New indicators
- New population estimates 2017 <- DONE
- New 8,000 records for data importing-received from Fayez <- DONE
- Workers records to be removed <- DONE


Change all ages to be 2018-01-01 <- DONE

- Include East Jerusalem facilities and workers in the calculations [EJ facilities and workers are part of the WB, but not the population] <- DONE
- Include all the new records in the calculations
- make one document calculations as is and one document remove all the records that left facilities [remember to include the 8000 records first]
- Fix gender calculations <- COULDNT FIND THE PROBLEM
- Include calculations that Dr. Asad wants: GPs in all distributions
- Include calculations of UNRWA
- Include new population estimates into the calculations <- DONE
- Include indicators that I added as new in the health workforce density group
- Make sure all launching indicators are calculated using the new records also
- Tell him there are UNRWA records that will be included within days
- make age distribution 2018 in new calculations  <- DONE
- disability indicators need to be divided by all health workers and not by IS0A <- DONE
- put family physicians under the specialized doctors <- DONE

priority 1:
include 8000 people
fix age distribution
do 1 calc with 8000 included, and 1 calc with the people removed

