org::AllowFileManipulationFromInitialiseProject()
org::InitialiseProject(
  HOME = "/git/code_major/2019/skabb/",
  SHARED = "/dropbox/analyses/results_shared/code_major/2019/skabb/",
  RAW = "/Volumes/crypt_data/org/data_raw/code_major/2019/skabb/"
)

library(data.table)
library(ggplot2)

d <- fread(file.path(org::PROJ$RAW,"TotalSkabb_20190301.txt"))

setnames(d,"Kj\xf8nn","sex")

res <- d[,.(
  N=.N
),keyby=.(
  PasientFylke,
  ar,
  uke,
  sex,
  Alder
)][CJ(
  unique(d$PasientFylke),
  unique(d$ar),
  unique(d$uke),
  unique(d$sex),
  unique(d$Alder)
)]
res[is.na(N),N:=0]

res <- res[sex %in% c("Mann","Kvinne")]

dcast.data.table(res,PasientFylke+ar+uke~Alder+sex, value.var = "N")
