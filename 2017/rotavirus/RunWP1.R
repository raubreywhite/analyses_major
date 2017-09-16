RAWmisc::InitialiseProject(
  PROJHOME = "/analyses/code_major/rotavirus/",
  PROJRAW = "/dropbox/data_raw/rotavirus/",
  PROJCLEAN = "/analyses/data_clean/rotavirus",
  PROJBAKED = "/analyses/results_baked/rotavirus/",
  PROJFINAL = "/analyses/results_final/rotavirus/",
  PROJSHARED = "/dropbox/results_shared/rotavirus/")

suppressWarnings(suppressMessages(library(data.table)))
suppressWarnings(suppressMessages(library(ggplot2)))

assign("RUN_ALL", TRUE, envir=globalenv())

unlink(file.path(RPROJ$PROJSHARED,lubridate::today()), recursive=TRUE, force=TRUE)
dir.create(file.path(RPROJ$PROJSHARED,lubridate::today()))

# CASES DIAGNOSED IN 2008-2013

RR1_1_1 <- log(6.03)
SE1_1_1 <- (log(10.07)-log(3.61))/1.96/2
RR1_2_3 <- log(1.13)
SE1_2_3 <- (log(1.79)-log(0.71))/1.96/2

RR2_1_1 <- log(1.83)
SE2_1_1 <- (log(2.50)-log(1.35))/1.96/2
RR2_2_3 <- log(1.61)
SE2_2_3 <- (log(2.47)-log(1.04))/1.96/2

RR1_3wk <- 2.35
RR2_3wk <- 1.77

dataIS <- data.table(openxlsx::read.xlsx(file.path(RPROJ$PROJRAW,"IS_weekage_birthyear_admyear.xlsx")))
names(dataIS) <- c("age","year","admission")
dataIS[,admission:=NULL]
dataIS <- data.table(dataIS)
dataIS[,age:=round(as.numeric(age))]
dataIS[,n:=1]
dataIS[,.(n=sum(n)),by=.(year)]
dataIS[year==2006,.(n=sum(n)),by=.(age)]
dataIS[year==2013,.(n=sum(n)),by=.(age)]
dataIS <- dataIS[,.(n=sum(n)),by=.(age,year)]

maxYear <- max(dataIS$year)

skeleton <- data.table(expand.grid(age=0:max(dataIS$age),year=min(dataIS$year):2016))
dataIS <- merge(skeleton,dataIS,by=c("age","year"),all.x=T)
dataIS[is.na(n),n:=0]
dataIS[year>maxYear,n:=NA]

dataCoverage <- data.table(haven::read_dta(file.path(RPROJ$PROJRAW,"4RichardCoverageVacc1and2.dta")))
dataCoverage <- dataCoverage[.N]

dataPop <- fread(RCurl::getURL("http://data.ssb.no/api/v0/dataset/1082.csv?lang=en"))
setnames(dataPop,c("region","sex","age","year","contents","pop"))
dataPop[,year:=as.numeric(year)]
dataPop <- dataPop[year>=min(dataIS$year) & age=="000 0 years",.(
  born=sum(pop)
),by=year]

data <- merge(dataIS,dataPop,by="year")
data[,bornJan:=year+(age/52)]
data[,bornDec:=year+(age/52)+51/52]
data[,weighting:=1]
data[bornDec<2008,weighting:=0]
data[,lifeIn2008:=bornDec-2008+1/52]
data[weighting>0 & year%in%c(2006:2007),weighting:=lifeIn2008]
data[weighting>1,weighting:=1]
data[,lifeIn2013:=2014-bornJan]
data[year%in%c(2012:2013),weighting:=lifeIn2013]
data[weighting<0,weighting:=0]
data[weighting>1,weighting:=1]
data[year==2006 & n>0]
data[,lifeIn2008:=NULL]
data[,lifeIn2013:=NULL]
data[,bornJan:=NULL]
data[,bornDec:=NULL]
data <- data[weighting>0]
data[,born:=born*weighting]


fit <- glm(n~offset(log(born)) + splines::ns(age, df=6),data=data, family="poisson")
summary(fit)
data[,pred:=predict(fit, data)]
data[,predSE:=predict(fit, data, se.fit=T)$se.fit]


dataYear <- data[!is.na(n),.(n=sum(n,na.rm=T),pred=log(sum(exp(pred))),born=sum(born)),by=.(age)]

q <- ggplot(dataYear,aes(x=age))
q <- q + geom_point(aes(y=n/born*100000),size=5)
q <- q + geom_line(aes(y=exp(pred)/born*100000),colour="red",lwd=3)
q <- q + scale_x_continuous("Age in weeks",breaks=seq(0,105,13))
q <- q + scale_y_continuous("Weekly IS Rate per 100,000 person-weeks")
q <- q + labs(title="Observed and predicted weekly IS rates per 100,000 person-weeks for cases diagnosed in Norway 2008-2013")
q <- q + RAWmisc::theme_SMAO()
RAWmisc::SMAOpng(file.path(RPROJ$PROJSHARED,lubridate::today(),"rates.png"))
print(q)
dev.off()

q <- ggplot(dataYear[age<52],aes(x=age))
q <- q + geom_point(aes(y=n/born*100000),size=5)
q <- q + geom_line(aes(y=exp(pred)/born*100000),colour="red",lwd=3)
q <- q + scale_x_continuous("Age in weeks",breaks=seq(0,105,13))
q <- q + scale_y_continuous("Weekly IS Rate per 100,000 person-weeks")
q <- q + labs(title="Observed and predicted weekly IS rates per 100,000 person-weeks for cases diagnosed in Norway 2008-2013")
q <- q + RAWmisc::theme_SMAO()
RAWmisc::SMAOpng(file.path(RPROJ$PROJSHARED,lubridate::today(),"rates_under1yr.png"))
print(q)
dev.off()

GetResults <- function(inData,useMean=T){
  data <- inData[age <= 51]
  setorder(data,year,age)
  setnames(data,"n","numRealIS")
  if(useMean){
    data[,pred:=exp(pred)]
  } else {
    data[,pred:=exp(rnorm(nrow(data), data$pred,data$predSE))]
  }
  data[,predSE:=NULL]
  setnames(data,"pred","numPredIS")
  setcolorder(data,c("year","age","born","weighting","numRealIS","numPredIS"))
  data[,ratePredIS:=numPredIS/born*100000]
  
  data[,vaccinated1:=0]
  data[age %in% 6:12,vaccinated1:=(born/7)*dataCoverage$CovDose1/100]
  data[,vaccinated2:=0]
  data[age %in% 12:16,vaccinated2:=(born/5)*dataCoverage$CovDose2/100]
  
  data[,vaccinated1_0:=shift(vaccinated1,n=0L)]
  data[,vaccinated1_1:=shift(vaccinated1,n=1L)]
  data[,vaccinated1_2:=shift(vaccinated1,n=2L)]
  
  data[,vaccinated2_0:=shift(vaccinated2,n=0L)]
  data[,vaccinated2_1:=shift(vaccinated2,n=1L)]
  data[,vaccinated2_2:=shift(vaccinated2,n=2L)]
  
  if(useMean){
    useRR1_1_1 <- exp(RR1_1_1)
    useRR1_2_3 <- exp(RR1_2_3)
    
    useRR2_1_1 <- exp(RR2_1_1)
    useRR2_2_3 <- exp(RR2_2_3)
  } else {
    useRR1_1_1 <- exp(rnorm(1,RR1_1_1,SE1_1_1))
    useRR1_2_3 <- exp(rnorm(1,RR1_2_3,SE1_2_3))
    
    useRR2_1_1 <- exp(rnorm(1,RR2_1_1,SE2_1_1))#exp(RR2_1_1)
    useRR2_2_3 <- exp(rnorm(1,RR2_2_3,SE2_2_3))#exp(RR2_2_3)
  }
  
  data[,ISvaccine1_0:=vaccinated1_0*(ratePredIS/100000)*(useRR1_1_1-1)]
  data[,ISvaccine1_1:=vaccinated1_1*(ratePredIS/100000)*(useRR1_2_3-1)]
  data[,ISvaccine1_2:=vaccinated1_2*(ratePredIS/100000)*(useRR1_2_3-1)]
  
  data[,ISvaccine2_0:=vaccinated2_0*(ratePredIS/100000)*(useRR2_1_1-1)]
  data[,ISvaccine2_1:=vaccinated2_1*(ratePredIS/100000)*(useRR2_2_3-1)]
  data[,ISvaccine2_2:=vaccinated2_2*(ratePredIS/100000)*(useRR2_2_3-1)]
  
  if(useMean){
    return(data[year==2016])
  } else {
    return(melt.data.table(data[year==2016,.(
      numBaselinePredIS=sum(numPredIS),
      ISvaccine1=sum(ISvaccine1_0+ISvaccine1_1+ISvaccine1_2),
      ISvaccine2=sum(ISvaccine2_0+ISvaccine2_1+ISvaccine2_2)
    ),by=year]))
  }
}


openxlsx::write.xlsx(GetResults(data,useMean=T),file=file.path(RPROJ$PROJSHARED,lubridate::today(),"mean_estimates.xlsx"))

res <- vector("list",1000)
for(i in 1:length(res)){
  res[[i]] <- GetResults(data,useMean=F)
}
res <- rbindlist(res)
estimates <- res[,.(
  pointEstimate=quantile(value,probs=.5),
  LowerConf95=quantile(value,probs=.025),
  UpperConf95=quantile(value,probs=.975)
),by=.(variable)]

saveRDS(estimates,file=file.path(RPROJ$PROJBAKED,"estimates.RDS"))

rmarkdown::render("Notes.Rmd", output_dir = file.path(RPROJ$PROJSHARED,lubridate::today()))

