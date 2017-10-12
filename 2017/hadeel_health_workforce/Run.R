RAWmisc::InitialiseProject(
  HOME = "/git/code_major/2017/hadeel_health_workforce/",
  RAW = "/dropbox/data_raw/code_major/2017/hadeel_health_workforce/",
  CLEAN = "/analyses/data_clean/code_major/2017/hadeel_health_workforce/",
  BAKED = "/analyses/results_baked/code_major/2017/hadeel_health_workforce/",
  FINAL = "/analyses/results_final/code_major/2017/hadeel_health_workforce/",
  SHARED = "/dropbox/results_shared/code_major/2017/hadeel_health_workforce/")

library(data.table)
library(ggplot2)

# FACILITIES
d1 <- openxlsx::read.xlsx(file.path(RAWmisc::PROJ$RAW,
                                    "Data collected by PCBS - Gaza 1st phase",
                                    "Gaza Phase I - After auditing by Fayez",
                                    "FACILITY_MAIN_Gaza I_Richard.xlsx"))

d2 <- openxlsx::read.xlsx(file.path(RAWmisc::PROJ$RAW,
                                    "Data collected by PCBS - WB 1st phase",
                                    "WB - Phase I",
                                    "FACILITY_MAIN WB I_Richard.xlsx"))

d <- data.table(rbind(d1,d2))
nrow(d)

xtabs(~d$IDH01)

xtabs(~d$TR05)
xtabs(~d$TR01)

# WORKERS
d1 <- openxlsx::read.xlsx(file.path(RAWmisc::PROJ$RAW,
                                    "Data collected by PCBS - Gaza 1st phase",
                                    "Gaza Phase I - After auditing by Fayez",
                                    "WORKERS_MAIN (1).xlsx"))

d2 <- openxlsx::read.xlsx(file.path(RAWmisc::PROJ$RAW,
                                    "Data collected by PCBS - WB 1st phase",
                                    "WB - Phase I",
                                    "WORKERS_MAIN WB.xlsx"))
d1 <- d1[,names(d2)]

d <- data.table(rbind(d1,d2))
nrow(d)

d[,isDoctor:=as.numeric(NA)]
d[CW00 %in% c(221101:221251),isDoctor:=1]

xtabs(~d$isDoctor)

d[,CW00]
xtabs(~d$)