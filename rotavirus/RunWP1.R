RAWmisc::InitialiseProject(
  PROJHOME = "/analyses/code_major/rotavirus/",
  PROJRAW = "/dropbox/data_raw/rotavirus/",
  PROJCLEAN = "/analyses/data_clean/rotavirus",
  PROJBAKED = "/analyses/results_baked/rotavirus/",
  PROJFINAL = "/analyses/results_final/rotavirus/",
  PROJSHARED = "/dropbox/results_shared/rotavirus/")

suppressWarnings(suppressMessages(library(data.table)))
suppressWarnings(suppressMessages(library(ggplot2)))
suppressWarnings(suppressMessages(library(foreach)))
suppressWarnings(suppressMessages(library(pomp)))
suppressMessages(library(doParallel))
registerDoParallel()
assign("RUN_ALL", TRUE, envir=globalenv())

RR1_3wk <- 2.35
RR2_3wk <- 1.77

dataIS <- data.table(openxlsx::read.xlsx(file.path(RPROJ$PROJRAW,"IS_beregninger_mnd.xlsx")))
dataIS[,incidenceAll:=incidenceAll/4] # convert the monthly rates to weekly rates
dataIS <- dataIS[,c("week","incidenceAll"),with=F]
incidenceY0 <- mean(dataIS[week %in% c(0:51)]$incidenceAll)
incidenceIS1 <- mean(dataIS[week %in% c(6:12)]$incidenceAll)
incidenceIS2 <- mean(dataIS[week %in% c(12:16)]$incidenceAll)

dataCoverage <- data.table(haven::read_dta(file.path(RPROJ$PROJRAW,"4RichardCoverageVacc1and2.dta")))
dataCoverage <- dataCoverage[.N]

dataPop <- fread(RCurl::getURL("http://data.ssb.no/api/v0/dataset/1082.csv?lang=en"))
setnames(dataPop,c("region","sex","age","year","contents","pop"))
dataPop[,year:=as.numeric(year)]
dataPop <- dataPop[year>=2014 & age=="000 0 years",.(
  born=sum(pop)
),by=year]

data <- data.table(expand.grid(year=2014:2016,week=0:51))
data <- merge(data,dataIS,by="week")
data <- merge(data,dataPop,by="year")
setorder(data,year,week)

data[,vaccinated1:=0]
data[week %in% 6:12,vaccinated1:=(born/7)*dataCoverage$CovDose1/100]
data[,vaccinated2:=0]
data[week %in% 12:16,vaccinated2:=(born/5)*dataCoverage$CovDose2/100]

data[,vaccinated1_0:=shift(vaccinated1,n=0L)]
data[,vaccinated1_1:=shift(vaccinated1,n=1L)]
data[,vaccinated1_2:=shift(vaccinated1,n=2L)]

data[,vaccinated2_0:=shift(vaccinated2,n=0L)]
data[,vaccinated2_1:=shift(vaccinated2,n=1L)]
data[,vaccinated2_2:=shift(vaccinated2,n=2L)]

data[,ISbaseline:=born*incidenceAll/100000]

data[,ISvaccine1_0:=vaccinated1_0*(incidenceAll/100000)*(RR1_3wk-1)]
data[,ISvaccine1_1:=vaccinated1_1*(incidenceAll/100000)*(RR1_3wk-1)]
data[,ISvaccine1_2:=vaccinated1_2*(incidenceAll/100000)*(RR1_3wk-1)]

data[,ISvaccine2_0:=vaccinated2_0*(incidenceAll/100000)*(RR2_3wk-1)]
data[,ISvaccine2_1:=vaccinated2_1*(incidenceAll/100000)*(RR2_3wk-1)]
data[,ISvaccine2_2:=vaccinated2_2*(incidenceAll/100000)*(RR2_3wk-1)]

data[week %in% 6:18]

data[year>=2015,.(
  ISbaseline=sum(ISbaseline),
  ISvaccine1=sum(ISvaccine1_0+ISvaccine1_1+ISvaccine1_2),
  ISvaccine2=sum(ISvaccine2_0+ISvaccine2_1+ISvaccine2_2)
),by=year]


