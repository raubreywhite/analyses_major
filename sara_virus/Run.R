RAWmisc::InitialiseProject(
  PROJHOME = "/analyses/code_major/sara_virus/",
  PROJRAW = "/analyses/data_raw/sara_virus/",
  PROJCLEAN = "/analyses/data_clean/sara_virus",
  PROJBAKED = "/analyses/results_baked/sara_virus/",
  PROJFINAL = "/analyses/results_final/sara_virus/",
  PROJSHARED = "/dropbox/results_shared/sara_virus/")

suppressWarnings(suppressMessages(library(data.table)))
suppressWarnings(suppressMessages(library(ggplot2)))

assign("RUN_ALL", TRUE, envir=globalenv())

masterData <- readRDS(file.path(RAWmisc::PROJ$RAW,"2017_05_09_cleaned_legekontakt_fastlege.RDS"))[runData==TRUE]
masterData <- masterData[county=="county01" & age=="Totalt"]
masterData[,week:=data.table::isoweek(date)]
masterData[,month:=format.Date(date,"%m")]
masterData[,percWithoutVirus:=0.7]
masterData[year>=2015,percWithoutVirus:=0.5]


retval <- vector("list",200)
for(i in 1:length(retval)){
  d <- masterData[year %in% c(2008:2016),.(
    withoutVirus=sum(rbinom(.N,respiratory,percWithoutVirus)),
    withVirus=sum(rbinom(.N,respiratory,(1-percWithoutVirus)))),
    by=.(year,week)]
  d[,laterTimePeriod:=0]
  d[year>=2015,laterTimePeriod:=1]
  
  timing <- data.table(laterTimePeriod=c(0,1),percWithoutVirus=c(0.5,0.5),percWithVirus=c(0.5,0.2))
  d[timing,on=.(laterTimePeriod)]
  d[timing,on=.(laterTimePeriod),prescriptions:=rbinom(.N,withoutVirus,percWithoutVirus)+rbinom(.N,withVirus,percWithVirus)]
  fit <- lm(prescriptions ~ -1+withoutVirus+withoutVirus:laterTimePeriod+withVirus+withVirus:laterTimePeriod,data=d)
  summary(fit)
  retval[[i]] <- as.data.frame(coef(summary(fit)))
  retval[[i]]$var <- row.names(retval[[i]])
}

retval <- rbindlist(retval)
setnames(retval,c("est","se","t","p","var"))

retval[,.(
  est=mean(est),
  power=mean(p<0.05)
),by=var]