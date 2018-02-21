AbovePercentile <- function(x,p){
  return(as.numeric(x >= quantile(x,p,na.rm=T)))
}

CleanSykdomsPulsen <- function(){
  ######### SYKDOMSPULSEN 1
  
  q <- vector("list",5)
  qindex <- 1
  for(a in c("0-4","5-14","15-64","65+","Totalt")){
    sykdomspulsen <- readRDS(file.path(RAWmisc::PROJ$RAW,"WP2/2016_03_30_cleaned_legekontakt_everyone.RDS"))
    sykdomspulsen <- sykdomspulsen[runData==TRUE & age==a & pop>0]
    sykdomspulsen[,age:=NULL]
    sykdomspulsen[,runData:=NULL]
    sykdomspulsen[,county:=NULL]
    setnames(sykdomspulsen,c("date","municip","year",
                             "s_influensa","n","s_respiratory","s_consult",
                             "dayOfYear","dayOfWeek","pop","yrwk","municipName","countyName",
                             "helligdagIndikator"))
    sykdomspulsen[,consult:=s_consult]
    
    municips <- unique(sykdomspulsen$municip)
    res <- vector("list",length(municips))
    for(i in 1:length(municips)){
      x <- quasipoisson.algorithm.week(sykdomspulsen[municip==municips[i]])
      x[,trend:=NULL]
      x[,w_i:=NULL]
      x[,municip:=municips[i]]
      res[[i]] <- copy(x)
    }
    res <- rbindlist(res)
    setnames(res,"n","s_gastro")
    setnames(res,"consult","s_consult")
    setnames(res,"popX","s_pop")
    setnames(res,"threshold0","s_threshold0")
    setnames(res,"threshold2","s_threshold2")
    setnames(res,"threshold4","s_threshold4")
    
    res[,s_outbreakLege:=0]
    res[s_gastro>s_threshold2,s_outbreakLege:=1]
    
    res1 <- res[,c("municip","year","week","s_outbreakLege","s_gastro","s_consult","s_pop"),with=F]
    
    ######### SYKDOMSPULSEN 2
    
    
    sykdomspulsen <- readRDS(file.path(RAWmisc::PROJ$RAW,"WP2/2016_03_30_cleaned_everyone_everyone.RDS"))
    sykdomspulsen <- sykdomspulsen[runData==TRUE & age==a & pop>0]
    sykdomspulsen[,age:=NULL]
    sykdomspulsen[,runData:=NULL]
    sykdomspulsen[,county:=NULL]
    setnames(sykdomspulsen,c("date","municip","year",
                             "s_influensa","n","s_respiratory","s_consult",
                             "dayOfYear","dayOfWeek","pop","yrwk","municipName","countyName",
                             "helligdagIndikator"))
    sykdomspulsen[,consult:=s_consult]
    
    municips <- unique(sykdomspulsen$municip)
    res <- vector("list",length(municips))
    for(i in 1:length(municips)){
      x <- quasipoisson.algorithm.week(sykdomspulsen[municip==municips[i]])
      x[,trend:=NULL]
      x[,w_i:=NULL]
      x[,municip:=municips[i]]
      res[[i]] <- copy(x)
    }
    res <- rbindlist(res)
    setnames(res,"n","s_gastro")
    setnames(res,"consult","s_consult")
    setnames(res,"popX","s_pop")
    setnames(res,"threshold0","s_threshold0")
    setnames(res,"threshold2","s_threshold2")
    setnames(res,"threshold4","s_threshold4")
    
    res[,s_outbreakAll:=0]
    res[s_gastro>s_threshold2,s_outbreakAll:=1]
    
    res2 <- res[,c("municip","year","week","s_outbreakAll"),with=F]
    
    #####
    
    res <- merge(res1,res2,by=c("municip","year","week"))
    res[,age:=a]
    q[[qindex]] <- res
    qindex <- qindex+1
  }
  q <- rbindlist(q)
  return(q)
}

CleanData <- function(){
  s <- CleanSykdomsPulsen()
  saveRDS(s,file.path(RAWmisc::PROJ$CLEAN,"sykdomspulsen.RDS"))
  
  dataCentreRadiation <- data.table(readxl::read_excel(file.path(RAWmisc::PROJ$RAW,"WP2/GlobalRadiation.xlsx")))
  dataCentrePrecip1 <- data.table(readxl::read_excel(file.path(RAWmisc::PROJ$RAW,"WP2/Centre_Precip_Dept1-12.xlsx")))
  dataCentrePrecip2 <- data.table(readxl::read_excel(file.path(RAWmisc::PROJ$RAW,"WP2/Centre_Precip_Dept14-20.xlsx")))
  dataCentrePrecip <- rbind(dataCentrePrecip1,dataCentrePrecip2)
  dataCentrePrecip[,STNR:=NULL]
  setnames(dataCentrePrecip,c("Municipality.number","Date","prec","rain"))
  
  dataCentreTemperature <- data.table(readxl::read_excel(file.path(RAWmisc::PROJ$RAW,"WP2/Centre_Max_Temp_Dept1-20.xlsx")))
  dataCentreTemperature[,STNR:=NULL]
  setnames(dataCentreTemperature,c("Municipality.number","Date","temperature"))
  
  dataAverage <- fread(file.path(RAWmisc::PROJ$RAW,"WP2/SeNorge_data_2000_2014.dat"))
  dataAverageCorrected <- fread(file.path(RAWmisc::PROJ$RAW,"WP2/Corrected_SeNorgeData.dat"))
  dim(dataAverage)
  dataAverage <- dataAverage[!(Muni %in% dataAverageCorrected$Muni & Date %in% dataAverageCorrected$Date)]
  dim(dataAverage)
  dataAverage <- rbind(dataAverage,dataAverageCorrected)
  
  dataCentre <- merge(dataCentrePrecip, dataCentreRadiation, by=c("Municipality.number","Date"),all.x=TRUE,all.y=TRUE)
  dim(dataCentreRadiation)
  dim(dataCentrePrecip)
  dim(dataCentre)
  
  dim(dataCentre)
  dataCentre <- merge(dataCentre, dataCentreTemperature, by=c("Municipality.number","Date"),all.x=TRUE,all.y=TRUE)
  dim(dataCentreTemperature)
  dim(dataCentre)
  
  setnames(dataCentre,c("municip","date","c_precip","c_rain","c_radiation","c_temperature"))
  setnames(dataAverage,c("municip","date","a_precipUncorr","a_precipCorr","a_temp","a_runoff"))
  dataCentre[,date:=as.character(date)]
  dataCentre[,c_rain:=as.numeric(c_rain)]
  
  data <- merge(dataCentre,dataAverage,by=c("municip","date"),all.x=TRUE,all.y=TRUE)
  data[,date:=as.Date(date)]
  dim(dataCentre)
  dim(dataAverage)
  dim(data)
  
  data[,municip:=paste0("municip", formatC(municip, width=4, flag="0"))]
  
  data[,year := format.Date(date,"%G")] #Week-based year, instead of normal year (%Y)
  data[,week := as.numeric(format.Date(date,"%V"))]
  data[,month:=as.numeric(stringr::str_sub(date,6,7))]
  
  data <- data[year >= 2006]
  data[, wp950_a_runoff0_0 := AbovePercentile(a_runoff, 0.95), by = municip]
  data[, wp950_a_precipCorr0_0 := AbovePercentile(a_precipCorr, 0.95), by = municip]
  data[, wp950_c_rain0_0 := AbovePercentile(c_rain, 0.95), by = municip]
  data[, wp950_c_temperature0_0 := 0]
  data[c_temperature>=20, wp950_c_temperature0_0 := 1]

  data[, wp990_a_runoff0_0 := AbovePercentile(a_runoff, 0.99), by = municip]
  data[, wp990_a_precipCorr0_0 := AbovePercentile(a_precipCorr, 0.99), by = municip]
  data[, wp990_c_rain0_0 := AbovePercentile(c_rain, 0.99), by = municip]
  data[, wp990_c_temperature0_0 := 0]
  data[c_temperature>=20, wp990_c_temperature0_0 := 1]
  
  dataWeek <- data[week %in% c(1:52) & year <=2014,
    .(c_rain = mean(c_rain),
      c_temperature = mean(c_temperature),
      a_precipCorr = mean(a_precipCorr),
      a_temp = mean(a_temp),
      a_runoff = mean(a_runoff),
      month = round(mean(month)),
      wp950_a_runoff0_0 = sum(wp950_a_runoff0_0),
      wp950_a_precipCorr0_0 = sum(wp950_a_precipCorr0_0),
      wp950_c_rain0_0 = sum(wp950_c_rain0_0),
      wp950_c_temperature0_0 = sum(wp950_c_temperature0_0),
      wp990_a_runoff0_0 = sum(wp990_a_runoff0_0),
      wp990_a_precipCorr0_0 = sum(wp990_a_precipCorr0_0),
      wp990_c_rain0_0 = sum(wp990_c_rain0_0),
      wp990_c_temperature0_0 = sum(wp990_c_temperature0_0)
    ),
    by=.(
      municip,year,week
    )
  ]
 
  
  #weeklyConsults <- dataWeek[,.(s_consult=mean(s_consult)),by=.(municip)]
  #keep <- weeklyConsults[s_consult>200]$municip
  #dataWeek <- dataWeek[municip %in% keep]
  
  
  #alloc.col(dataWeek,3000)
  
  for(i in 1:10){
    var <- paste0("wp950_c_temperature",i,"_",i)
    dataWeek[,(var):=shift(wp950_c_temperature0_0,i),by=municip]
  }
  
  wp950_exposures_a_runoff <- expand.grid(0:4, 1:1)
  names(wp950_exposures_a_runoff) <- c("lag","window")
  wp950_exposures_a_runoff$end <- wp950_exposures_a_runoff$lag+wp950_exposures_a_runoff$window-1
  wp950_exposures_a_runoff$varOfInterest <- paste0("wp950_a_runoff",wp950_exposures_a_runoff$lag,"_",wp950_exposures_a_runoff$end)
  
  wp950_exposures_a_runoff <- data.table(wp950_exposures_a_runoff)
  
  for(i in 1:10){
    var <- paste0("wp950_a_runoff",i,"_",i)
    dataWeek[,(var):=shift(wp950_a_runoff0_0,i),by=municip]
  }
  
  dataWeek[,cat_wp950_a_runoff0_0:=cut(wp950_a_runoff0_0,breaks=c(-1,0,3,100),include.lowest = TRUE)]
  dataWeek[,cat_wp950_a_runoff1_1:=cut(wp950_a_runoff1_1,breaks=c(-1,0,3,100),include.lowest = TRUE)]
  dataWeek[,cat_wp950_a_runoff2_2:=cut(wp950_a_runoff2_2,breaks=c(-1,0,3,100),include.lowest = TRUE)]
  dataWeek[,cat_wp950_a_runoff3_3:=cut(wp950_a_runoff3_3,breaks=c(-1,0,3,100),include.lowest = TRUE)]
  dataWeek[,cat_wp950_a_runoff4_4:=cut(wp950_a_runoff4_4,breaks=c(-1,0,3,100),include.lowest = TRUE)]
  
  ########
  
  wp950_exposures_a_precipCorr <- expand.grid(0:4, 1:1)
  names(wp950_exposures_a_precipCorr) <- c("lag","window")
  wp950_exposures_a_precipCorr$end <- wp950_exposures_a_precipCorr$lag+wp950_exposures_a_precipCorr$window-1
  wp950_exposures_a_precipCorr$varOfInterest <- paste0("wp950_a_precipCorr",wp950_exposures_a_precipCorr$lag,"_",wp950_exposures_a_precipCorr$end)
  
  wp950_exposures_a_precipCorr <- data.table(wp950_exposures_a_precipCorr)
  
  for(i in 1:10){
    var <- paste0("wp950_a_precipCorr",i,"_",i)
    dataWeek[,(var):=shift(wp950_a_precipCorr0_0,i),by=municip]
  }
  
  dataWeek[,cat_wp950_a_precipCorr0_0:=cut(wp950_a_precipCorr0_0,breaks=c(-1,0,3,100),include.lowest = TRUE)]
  dataWeek[,cat_wp950_a_precipCorr1_1:=cut(wp950_a_precipCorr1_1,breaks=c(-1,0,3,100),include.lowest = TRUE)]
  dataWeek[,cat_wp950_a_precipCorr2_2:=cut(wp950_a_precipCorr2_2,breaks=c(-1,0,3,100),include.lowest = TRUE)]
  dataWeek[,cat_wp950_a_precipCorr3_3:=cut(wp950_a_precipCorr3_3,breaks=c(-1,0,3,100),include.lowest = TRUE)]
  dataWeek[,cat_wp950_a_precipCorr4_4:=cut(wp950_a_precipCorr4_4,breaks=c(-1,0,3,100),include.lowest = TRUE)]
  
  #########
  
  wp950_exposures_c_rain <- expand.grid(0:4, 1:1)
  names(wp950_exposures_c_rain) <- c("lag","window")
  wp950_exposures_c_rain$end <- wp950_exposures_c_rain$lag+wp950_exposures_c_rain$window-1
  wp950_exposures_c_rain$varOfInterest <- paste0("wp950_c_rain",wp950_exposures_c_rain$lag,"_",wp950_exposures_c_rain$end)
  
  wp950_exposures_c_rain <- data.table(wp950_exposures_c_rain)
  
  for(i in 1:10){
    var <- paste0("wp950_c_rain",i,"_",i)
    dataWeek[,(var):=shift(wp950_c_rain0_0,i),by=municip]
  }
  
  dataWeek[,cat_wp950_c_rain0_0:=cut(wp950_c_rain0_0,breaks=c(-1,0,3,100),include.lowest = TRUE)]
  dataWeek[,cat_wp950_c_rain1_1:=cut(wp950_c_rain1_1,breaks=c(-1,0,3,100),include.lowest = TRUE)]
  dataWeek[,cat_wp950_c_rain2_2:=cut(wp950_c_rain2_2,breaks=c(-1,0,3,100),include.lowest = TRUE)]
  dataWeek[,cat_wp950_c_rain3_3:=cut(wp950_c_rain3_3,breaks=c(-1,0,3,100),include.lowest = TRUE)]
  dataWeek[,cat_wp950_c_rain4_4:=cut(wp950_c_rain4_4,breaks=c(-1,0,3,100),include.lowest = TRUE)]
  
  ###
  
  wp990_exposures_a_runoff <- expand.grid(0:4, 1:1)
  names(wp990_exposures_a_runoff) <- c("lag", "window")
  wp990_exposures_a_runoff$end <- wp990_exposures_a_runoff$lag + wp990_exposures_a_runoff$window - 1
  wp990_exposures_a_runoff$varOfInterest <- paste0("wp990_a_runoff", wp990_exposures_a_runoff$lag, "_", wp990_exposures_a_runoff$end)
  
  wp990_exposures_a_runoff <- data.table(wp990_exposures_a_runoff)
  
  for (i in 1:10) {
    var <- paste0("wp990_a_runoff", i, "_", i)
    dataWeek[, (var) := shift(wp990_a_runoff0_0, i), by = municip]
  }
  
  dataWeek[, cat_wp990_a_runoff0_0 := cut(wp990_a_runoff0_0, breaks = c(-1, 0, 3, 100), include.lowest = TRUE)]
  dataWeek[, cat_wp990_a_runoff1_1 := cut(wp990_a_runoff1_1, breaks = c(-1, 0, 3, 100), include.lowest = TRUE)]
  dataWeek[, cat_wp990_a_runoff2_2 := cut(wp990_a_runoff2_2, breaks = c(-1, 0, 3, 100), include.lowest = TRUE)]
  dataWeek[, cat_wp990_a_runoff3_3 := cut(wp990_a_runoff3_3, breaks = c(-1, 0, 3, 100), include.lowest = TRUE)]
  dataWeek[, cat_wp990_a_runoff4_4 := cut(wp990_a_runoff4_4, breaks = c(-1, 0, 3, 100), include.lowest = TRUE)]
  
  ########
  
  wp990_exposures_a_precipCorr <- expand.grid(0:4, 1:1)
  names(wp990_exposures_a_precipCorr) <- c("lag", "window")
  wp990_exposures_a_precipCorr$end <- wp990_exposures_a_precipCorr$lag + wp990_exposures_a_precipCorr$window - 1
  wp990_exposures_a_precipCorr$varOfInterest <- paste0("wp990_a_precipCorr", wp990_exposures_a_precipCorr$lag, "_", wp990_exposures_a_precipCorr$end)
  
  wp990_exposures_a_precipCorr <- data.table(wp990_exposures_a_precipCorr)
  
  for (i in 1:10) {
    var <- paste0("wp990_a_precipCorr", i, "_", i)
    dataWeek[, (var) := shift(wp990_a_precipCorr0_0, i), by = municip]
  }
  
  dataWeek[, cat_wp990_a_precipCorr0_0 := cut(wp990_a_precipCorr0_0, breaks = c(-1, 0, 3, 100), include.lowest = TRUE)]
  dataWeek[, cat_wp990_a_precipCorr1_1 := cut(wp990_a_precipCorr1_1, breaks = c(-1, 0, 3, 100), include.lowest = TRUE)]
  dataWeek[, cat_wp990_a_precipCorr2_2 := cut(wp990_a_precipCorr2_2, breaks = c(-1, 0, 3, 100), include.lowest = TRUE)]
  dataWeek[, cat_wp990_a_precipCorr3_3 := cut(wp990_a_precipCorr3_3, breaks = c(-1, 0, 3, 100), include.lowest = TRUE)]
  dataWeek[, cat_wp990_a_precipCorr4_4 := cut(wp990_a_precipCorr4_4, breaks = c(-1, 0, 3, 100), include.lowest = TRUE)]
  
  #########
  
  wp990_exposures_c_rain <- expand.grid(0:4, 1:1)
  names(wp990_exposures_c_rain) <- c("lag", "window")
  wp990_exposures_c_rain$end <- wp990_exposures_c_rain$lag + wp990_exposures_c_rain$window - 1
  wp990_exposures_c_rain$varOfInterest <- paste0("wp990_c_rain", wp990_exposures_c_rain$lag, "_", wp990_exposures_c_rain$end)
  
  wp990_exposures_c_rain <- data.table(wp990_exposures_c_rain)
  
  for (i in 1:10) {
    var <- paste0("wp990_c_rain", i, "_", i)
    dataWeek[, (var) := shift(wp990_c_rain0_0, i), by = municip]
  }
  
  dataWeek[, cat_wp990_c_rain0_0 := cut(wp990_c_rain0_0, breaks = c(-1, 0, 3, 100), include.lowest = TRUE)]
  dataWeek[, cat_wp990_c_rain1_1 := cut(wp990_c_rain1_1, breaks = c(-1, 0, 3, 100), include.lowest = TRUE)]
  dataWeek[, cat_wp990_c_rain2_2 := cut(wp990_c_rain2_2, breaks = c(-1, 0, 3, 100), include.lowest = TRUE)]
  dataWeek[, cat_wp990_c_rain3_3 := cut(wp990_c_rain3_3, breaks = c(-1, 0, 3, 100), include.lowest = TRUE)]
  dataWeek[, cat_wp990_c_rain4_4 := cut(wp990_c_rain4_4, breaks = c(-1, 0, 3, 100), include.lowest = TRUE)]

  ###
  
  dim(dataWeek)
  
  dataWeek[,season:="Winter"]
  dataWeek[month>=2.9 & month<=5.1,season:="Spring"]
  dataWeek[month>=5.9 & month<=8.1,season:="Summer"]
  dataWeek[month>=8.9 & month<=11.1,season:="Autumn"]
  
  s <- readRDS(file.path(RAWmisc::PROJ$CLEAN,"sykdomspulsen.RDS"))
  
  data <- merge(dataWeek,s,by=c("municip","year","week"))
  dim(data)
  dim(s)
  
  vreg <- readxl::read_excel(file.path(RAWmisc::PROJ$RAW,"VREG2014.xlsx"),skip=4)
  vreg <- vreg[,c("K_nr","Befolkning","Antall personer_50-500","Antall personer_501-5000","Antall personer_5001-","Forsyningsgrad")]
  names(vreg) <- c("municip","pop","psma","pmed","plar","percWaterwork")
  for(i in 3:6) vreg[is.na(vreg[,i]),i] <- 0
  vreg$percNotWaterwork <- 1-vreg$percWaterwork
  
  vreg$avwaterworksize <- (vreg$psma*(500-50) + vreg$pmed*(5000-501) + vreg$plar*(25000))/(vreg$pop)
  vreg$waterworkcat <- c("Under 10k")
  vreg$waterworkcat[vreg$avwaterworksize>=10000] <- "10k+"
  vreg$waterworkcat2 <- c("Under 500")
  vreg$waterworkcat2[vreg$avwaterworksize>=500] <- "500+"
  vreg$notWaterworkcat <- c("Under 50%")
  vreg$notWaterworkcat[vreg$percNotWaterwork>=0.5] <- "50%+"
  vreg$municip <- paste0("municip", formatC(vreg$municip, width=4, flag="0"))
  vreg <- data.table(vreg[,c("municip","avwaterworksize","waterworkcat","waterworkcat2","notWaterworkcat")])
  
  #q <- ggplot(vreg,aes(x=avwaterworksize),w=0.5,h=0.5)
  #q <- q + geom_histogram()
  #q <- q + RAWmisc::theme_SMAO(24)
  #q <- q + scale_x_continuous("Average waterwork size per municipality")
  #q <- q + scale_y_continuous("Count")
  #RAWmisc::SMAOpng(file.path(RAWmisc::PROJ$SHARED_TODAY,"WP2/waterworksize.png"))
  #print(q)
  #dev.off()
  
  dim(data)
  data2 <- merge(data,vreg,by="municip")
  dim(data2)
  
  cat_wp950_exposures_a_runoff=wp950_exposures_a_runoff
  cat_wp950_exposures_a_runoff$varOfInterest=paste0("cat_",cat_wp950_exposures_a_runoff$varOfInterest)
  
  cat_wp950_exposures_a_precipCorr=wp950_exposures_a_precipCorr
  cat_wp950_exposures_a_precipCorr$varOfInterest=paste0("cat_",cat_wp950_exposures_a_precipCorr$varOfInterest)
  
  cat_wp950_exposures_c_rain=wp950_exposures_c_rain
  cat_wp950_exposures_c_rain$varOfInterest=paste0("cat_",cat_wp950_exposures_c_rain$varOfInterest)

  cat_wp990_exposures_a_runoff = wp990_exposures_a_runoff
  cat_wp990_exposures_a_runoff$varOfInterest = paste0("cat_", cat_wp990_exposures_a_runoff$varOfInterest)

  cat_wp990_exposures_a_precipCorr = wp990_exposures_a_precipCorr
  cat_wp990_exposures_a_precipCorr$varOfInterest = paste0("cat_", cat_wp990_exposures_a_precipCorr$varOfInterest)

  cat_wp990_exposures_c_rain = wp990_exposures_c_rain
  cat_wp990_exposures_c_rain$varOfInterest = paste0("cat_", cat_wp990_exposures_c_rain$varOfInterest)

  set(x=data2,j=which(stringr::str_detect(names(data2),"990")),value=NULL)
  for(i in c("5_5","6_6","7_7","8_8","9_9")) set(x=data2,j=which(stringr::str_detect(names(data2),i)),value=NULL)
  
  data2[,wp950_a_runoff0_3:=wp950_a_runoff0_0+wp950_a_runoff1_1+wp950_a_runoff2_2+wp950_a_runoff3_3]
  data2[,wp950_a_precipCorr0_3:=wp950_a_precipCorr0_0+wp950_a_precipCorr1_1+wp950_a_precipCorr2_2+wp950_a_precipCorr3_3]
  data2[,wp950_c_temperature0_3:=wp950_c_temperature0_0+wp950_c_temperature1_1+wp950_c_temperature2_2+wp950_c_temperature3_3]
  data2[,wp950_c_rain0_3:=wp950_c_rain0_0+wp950_c_rain1_1+wp950_c_rain2_2+wp950_c_rain3_3]
  
  return(data2)
  d <- list(
    data=data2,
    wp950_exposures_a_runoff=wp950_exposures_a_runoff,
    wp950_exposures_a_precipCorr=wp950_exposures_a_precipCorr,
    wp950_exposures_c_rain=wp950_exposures_c_rain,
    cat_wp950_exposures_a_runoff=cat_wp950_exposures_a_runoff,
    cat_wp950_exposures_a_precipCorr=cat_wp950_exposures_a_precipCorr,
    cat_wp950_exposures_c_rain = cat_wp950_exposures_c_rain,
    wp990_exposures_a_runoff = wp990_exposures_a_runoff,
    wp990_exposures_a_precipCorr = wp990_exposures_a_precipCorr,
    wp990_exposures_c_rain = wp990_exposures_c_rain,
    cat_wp990_exposures_a_runoff = cat_wp990_exposures_a_runoff,
    cat_wp990_exposures_a_precipCorr = cat_wp990_exposures_a_precipCorr,
    cat_wp990_exposures_c_rain = cat_wp990_exposures_c_rain
  )
  #saveRDS(d,"data_clean/data.RDS")
  return(d)
}

CleanDataWeek <- function(){
  
  data <- CleanData()
  
  
  return(list(
    data=dataWeek,
    wp950_exposures_a_runoff=wp950_exposures_a_runoff,
    wp950_exposures_a_precipCorr=wp950_exposures_a_precipCorr,
    wp950_exposures_c_rain=wp950_exposures_c_rain
  ))
  
  dataWeek[,binary_a_runoff:=as.numeric(gtools::quantcut(a_runoff,q=2))-1]
  dataWeek[,binary_a_precipCorr:=as.numeric(gtools::quantcut(a_precipCorr,q=2))-1]
  dataWeek[,binary_a_precipUncorr:=as.numeric(gtools::quantcut(a_precipUncorr,q=2))-1]
  dataWeek[,binary_a_temp:=as.numeric(gtools::quantcut(a_temp,q=2))-1]
  
  dataWeek[,dummy_a_runoff:=cut(a_runoff,breaks=c(0,2.5,5,10,100),include.lowest = TRUE)]
  dataWeek[,dummy_a_precipCorr:=cut(a_precipCorr,breaks=c(0,2.5,5,10,100),include.lowest = TRUE)]
  dataWeek[,dummy_a_temp:=cut(a_temp,breaks=c(-50,0,10,20,100),include.lowest = TRUE)]
  
  return(dataWeek)
}

CheckData <- function(){
  data <- CleanData()
  
  dataLong <- melt.data.table(data,id=c("municip","date"))
  
  dataComplete <- copy(dataLong)
  dataComplete[!is.na(value),value:=1]
  dataComplete[is.na(value),value:=0]
  dataComplete <- dataComplete[,.(completeness=mean(value)),by=.(date,variable)]
  
  q <- ggplot(dataComplete,aes(x=date,y=completeness))
  q <- q + geom_area()
  q <- q + facet_wrap(~variable,scales="free",ncol=2)
  q <- q + scale_y_continuous("Proportion of municipalities with complete data\n",lim=c(0,1))
  q <- SMAOgraphs::SMAOFormatGGPlot(q)
  SMAOgraphs::SMAOpng(file.path(RAWmisc::PROJ$SHARED_TODAY,"Check_completeness_municipalities.png"),landscape=FALSE)
  print(q)
  dev.off()
  
  dataSummary <- dataLong[,.(
    p01=quantile(value,probs=c(0.01),na.rm=T),
    p25=quantile(value,probs=c(0.25),na.rm=T),
    p50=quantile(value,probs=c(0.50),na.rm=T),
    p75=quantile(value,probs=c(0.75),na.rm=T),
    p99=quantile(value,probs=c(0.99),na.rm=T)
    ),by=.(date,variable)]
  
  setnames(dataSummary,"variable","var")
  dataSummary <- melt.data.table(dataSummary, id=c("date","var"))
  
  for(i in unique(dataSummary$variable)){
    q <- ggplot(dataSummary[variable %in% i],aes(x=date,y=value))
    q <- q + geom_line()
    q <- q + facet_wrap(~var,scales="free",ncol=2)
    q <- q + scale_y_continuous(paste0(i," value from municipalities calculated every day\n"))
    q <- q + labs(title=paste0(i," value from municipalities calculated every day"))
    q <- SMAOgraphs::SMAOFormatGGPlot(q)
    SMAOgraphs::SMAOpng(file.path(RAWmisc::PROJ$SHARED_TODAY,paste0("Check_summary_",i,"_municipalities.png")),landscape=FALSE)
    print(q)
    dev.off()
  }
  
  dataVarOnly <- copy(data)
  dataVarOnly[,municip:=NULL]
  dataVarOnly[,date:=NULL]
  q <- GGally::ggpairs(dataVarOnly)
  
  SMAOgraphs::SMAOpng(file.path(RAWmisc::PROJ$SHARED_TODAY,"Check_matrix_plot.png"),landscape=TRUE)
  print(q)
  dev.off()
  
  
}

CleanDataWaterworksInternal <- function(){
  #warning("# KEEP TOC")
  
  
  ## ALTA
  CleanWP1SpecificWaterWorkLong(fileIn=file.path(RAWmisc::PROJ$RAW,"WP1_waterworks/Alta_Elvestrand/Rådata/Rapport siste 9 år Alta vannverk.xlsx"), 
                                fileOut="Rapport siste 9 år Alta vannverk.RDS",
                                      sheet=1,
                                      type="Accredited",
                                      waterwork="Alta",
                                      waterType="Raw"
                                      )
  
  ########
  # data_raw/WP1_waterworks/Arendal_Rore/Rådata/MapGraphReport3777593428727398452.xlsx
  #
  # Akk.Mikro
  ########
  
  d <- data.table(readxl::read_excel(
    file.path(RAWmisc::PROJ$RAW,"WP1_waterworks/Arendal_Rore/Rådata/edited_data.xlsx"),
                                     sheet=1,
                                     skip=1,
                                     col_names=FALSE,
                                     col_types=c("date",rep("text",4))
  ))
  
  setnames(d, c(
    "date",
    "point",
    "var",
    "value",
    "units"))
  
  d <- d[!is.na(var), which(names(d) != "X"), with = F]
  
  d[, type := "Accredited"]
  unique(d$var)
  
  d[, variable := ""]
  d[var == "09-Koliforme bakterier 37°C (/100 ml)", variable := "Coliform bacteria at 37 degrees"]
  d[var == "06-E.coli (/100 ml)", variable := "E. Coli"]
  d[var == "05-Clostridium perfringens (/100ml)", variable := "Clostridium Perfringens"]
  d[var == "07-Intestinale enterokokker (/100 ml)", variable := "Intestinal Enterococci"]
  d[var == "04-Turbiditet (FNU)", variable := "Turbidity"]
  d[var == "01-Farge (mg/l Pt)", variable := "Colour"]
  d[var == "08-Kimtall 22°C (/ml)", variable := "Colony count at 22 degrees"]
  d[var == "44-pH, surhetsgrad ( )", variable := "pH"]
  d[var == "35-Konduktivitet (mS/m)", variable := "Conductivity"]
  d[var == "53-Totalt organisk karbon (TOC) (mg/l C)", variable := "TOC"]
  d[, var := NULL]
  d <- d[variable!=""]
  
  d[, waterwork := "Arendal_Rore"]
  d[, waterType := "Raw"]
  
  saveRDS(d, file.path(RAWmisc::PROJ$CLEAN,"WP1_waterworks/Arendal_Rore.RDS"))
  
  ########
  ########
  ########
  # Asker og Bærum vannverk IKS_Aurevann
  ########
  ########
  ########
  
  ########
  # data_raw/WP1_waterworks/Asker og Bærum vannverk IKS_Aurevann/Rådata/Historiske data fra 2006_Aurevann.xlsx
  #
  # Akk.Mikro
  ########
  
  d <- data.table(readxl::read_excel(
    file.path(RAWmisc::PROJ$RAW,"WP1_waterworks/Asker og Bærum vannverk IKS_Aurevann/Rådata/Historiske data fra 2006_Aurevann.xlsx"),
                                     sheet="Akk.Mikro",
                                     skip=6,
                                     col_names=FALSE,
                                     col_types=c("date",rep("text",5))
  ))
  setnames(d,c("date","Coliform bacteria","E. Coli","Intestinal Enterococci","Clostridium Perfringens","Colony count"))
  d <- melt.data.table(d,id="date",variable.factor=FALSE)
  d[,type:="External"]
  unique(d$variable)
  d[,units:=""]
  d[variable=="Coliform bacteria", units:="ant/100ml"]
  d[variable=="E. Coli", units:="ant/100ml"]
  d[variable=="Intestinal Enterococci", units:="ant/100ml"]
  d[variable=="Clostridium Perfringens", units:="ant/100ml"]
  d[variable=="Colony count", units:="ant/ml"]
  d[, waterwork := "Asker og Baerum vannverk IKS_Aurevann"]
  d[, point := ""]
  d[, waterType := "Raw"]

  saveRDS(d,file.path(RAWmisc::PROJ$CLEAN,"WP1_waterworks/Asker og Bærum vannverk IKS_Aurevann_accredited_micro.RDS"))
  
  ########
  # data_raw/WP1_waterworks/Asker og Bærum vannverk IKS_Aurevann/Rådata/Historiske data fra 2006_Aurevann.xlsx
  #
  # Akk.Fys-kjem
  ########
  
  d <- data.table(readxl::read_excel(
    file.path(RAWmisc::PROJ$RAW,"WP1_waterworks/Asker og Bærum vannverk IKS_Aurevann/Rådata/Historiske data fra 2006_Aurevann.xlsx"),
                                     sheet="Akk.Fys-kjem",
                                     skip=6,
                                     col_names=FALSE,
                                     col_types=c("date",rep("text",4))
  ))
  setnames(d,c("date","pH","Turbidity","Conductivity","Colour"))
  d <- melt.data.table(d,id="date",variable.factor=FALSE)
  d[,type:="Accredited"]
  unique(d$variable)
  d[,units:=""]
  d[variable=="pH", units:=NA]
  d[variable=="Turbidity", units:="FTU"]
  d[variable=="Conductivity", units:="us/u"]
  d[variable=="Colour", units:="ug Pt/l"]
  d[,waterwork:="Asker og Baerum vannverk IKS_Aurevann"]
  d[, point := ""]
  d[, waterType := "Raw"]

  saveRDS(d,file.path(RAWmisc::PROJ$CLEAN,"WP1_waterworks/Asker og Bærum vannverk IKS_Aurevann_accredited_chem.RDS"))
  
  ########
  # data_raw/WP1_waterworks/Asker og Bærum vannverk IKS_Aurevann/Rådata/Historiske data fra 2006_Aurevann.xlsx
  #
  # Intern Fys-kjem
  ########
  
  d <- data.table(readxl::read_excel(file.path(RAWmisc::PROJ$RAW,"WP1_waterworks/Asker og Bærum vannverk IKS_Aurevann/Rådata/Historiske data fra 2006_Aurevann.xlsx"),
                                     sheet="Intern Fys-kjem",
                                     skip=6,
                                     col_names=FALSE,
                                     col_types=c("date",rep("text",4))
  ))
  setnames(d,c("date","pH","Turbidity","Colour","Conductivity"))
  d <- melt.data.table(d,id="date",variable.factor=FALSE)
  d[,type:="Internal"]
  unique(d$variable)
  d[,units:=""]
  d[variable=="pH", units:=NA]
  d[variable=="Turbidity", units:=NA]
  d[variable=="Colour", units:="m.filt"]
  d[variable=="Conductivity", units:="mS/m"]
  d[,waterwork:="Asker og Baerum vannverk IKS_Aurevann"]
  d[, point := ""]
  d[, waterType := "Raw"]

  saveRDS(d,file.path(RAWmisc::PROJ$CLEAN,"WP1_waterworks/Asker og Bærum vannverk IKS_Aurevann_intern_chem.RDS"))
  
  ########
  # data_raw/WP1_waterworks/Asker og Bærum vannverk IKS_Aurevann/Rådata/Historiske data fra 2006_Aurevann.xlsx
  #
  # Intern Mikro
  ########
  
  d <- data.table(readxl::read_excel(file.path(RAWmisc::PROJ$RAW,"WP1_waterworks/Asker og Bærum vannverk IKS_Aurevann/Rådata/Historiske data fra 2006_Aurevann.xlsx"),
                                     sheet="Intern Mikro",
                                     skip=5,
                                     col_names=FALSE,
                                     col_types=c("date",rep("text",2))
  ))
  setnames(d,c("date","Coliform bacteria","E. Coli"))
  d <- melt.data.table(d,id="date",variable.factor=FALSE)
  d[,type:="Internal"]
  unique(d$variable)
  d[,units:=""]
  d[variable=="Coliform bacteria", units:=NA]
  d[variable=="E. Coli", units:=NA]
  d[,waterwork:="Asker og Baerum vannverk IKS_Aurevann"]
  d[, point := ""]
  d[, waterType := "Raw"]

  saveRDS(d,file.path(RAWmisc::PROJ$CLEAN,"WP1_waterworks/Asker og Bærum vannverk IKS_Aurevann_intern_micro.RDS"))
  
  ########
  # data_raw/WP1_waterworks/Asker og Bærum vannverk IKS_Aurevann/Rådata/Historiske data fra 2006_Aurevann.xlsx
  #
  # Online
  ########
  
  d <- data.table(readxl::read_excel(file.path(RAWmisc::PROJ$RAW,"WP1_waterworks/Asker og Bærum vannverk IKS_Aurevann/Rådata/Historiske data fra 2006_Aurevann.xlsx"),
                                     sheet="Online",
                                     skip=4,
                                     col_names=FALSE,
                                     col_types=c("date",rep("text",3))
  ))
  setnames(d,c("date","Temperature","Hazen","Turbidity"))
  d <- melt.data.table(d,id="date",variable.factor=FALSE)
  d[,type:="Online"]
  unique(d$variable)
  d[,units:=""]
  d[variable=="Temperature", units:="C"]
  d[variable=="Hazen", units:=NA]
  d[variable=="Turbidity", units:="FNU"]
  d[,waterwork:="Asker og Baerum vannverk IKS_Aurevann"]
  d[, point := ""]
  d[, waterType := "Raw"]

  saveRDS(d,file.path(RAWmisc::PROJ$CLEAN,"WP1_waterworks/Asker og Bærum vannverk IKS_Aurevann_online.RDS"))
  
  ########
  ########
  ########
  # Asker og Bærum vannverk IKS_Kattås
  ########
  ########
  ########
  
  ########
  # data_raw/WP1_waterworks/Asker og Bærum vannverk IKS_Kattås/Rådata/Historiske data fra 2006_Holsfjorden.xlsx
  #
  # external
  ########
  
  d <- data.table(readxl::read_excel(file.path(RAWmisc::PROJ$RAW,"WP1_waterworks/Asker og Bærum vannverk IKS_Kattås/Rådata/Historiske data fra 2006_Holsfjorden.xlsx"),
                                     sheet="fra 2006 eks lab",
                                     skip=3,
                                     col_names=FALSE,
                                     col_types=c("date",rep("text",9))
  ))
  setnames(d,c("date","E. Coli","Coliform bacteria","Intestinal Enterococci","Colony count","Clostridium Perfringens","Turbidity","Colour","Conductivity","pH"))

  d <- melt.data.table(d,id="date",variable.factor=FALSE)
  d[,type:="External"]
  unique(d$variable)
  d[,units:=""]
  d[variable=="E. Coli", units:=NA]
  d[variable=="Coliform bacteria", units:=NA]
  d[variable=="Intestinal Enterococci", units:=NA]
  d[variable=="Colony count", units:=NA]
  d[variable=="Clostridium Perfringens", units:=NA]
  d[variable=="Turbidity", units:=NA]
  d[variable=="Colour", units:=NA]
  d[variable=="Conductivity", units:=NA]
  d[variable=="pH", units:=NA]
  d[,waterwork:="Asker og Baerum vannverk IKS_Kattås"]
  d[, point := ""]
  d[, waterType := "Raw"]

  saveRDS(d,file.path(RAWmisc::PROJ$CLEAN,"WP1_waterworks/Asker og Bærum vannverk IKS_Kattås_extern.RDS"))
  
  ########
  # data_raw/WP1_waterworks/Asker og Bærum vannverk IKS_Kattås/Rådata/Historiske data fra 2006_Holsfjorden.xlsx
  #
  # internal
  ########
  
  d <- data.table(readxl::read_excel(file.path(RAWmisc::PROJ$RAW,"WP1_waterworks/Asker og Bærum vannverk IKS_Kattås/Rådata/Historiske data fra 2006_Holsfjorden.xlsx"),
                                     sheet="fra 2006 int lab",
                                     skip=2,
                                     col_names=FALSE,
                                     col_types=c("date",rep("text",2))
  ))
  setnames(d,c("date","Coliform bacteria","E. Coli"))

  d <- melt.data.table(d,id="date",variable.factor=FALSE)
  d[,type:="Internal"]
  unique(d$variable)
  d[,units:=""]
  d[variable=="Coliform bacteria", units:=NA]
  d[variable=="E. Coli", units:=NA]
  d[,waterwork:="Asker og Baerum vannverk IKS_Kattås"]
  d[, point := ""]
  d[, waterType := "Raw"]

  saveRDS(d, file.path(RAWmisc::PROJ$CLEAN,"WP1_waterworks/Asker og Bærum vannverk IKS_Kattås_internal.RDS"))

  ########
  # data_raw/WP1_waterworks/Asker og Bærum vannverk IKS_Kattås/Rådata/Historiske data fra 2006_Holsfjorden.xlsx
  #
  # internal2
  ########

  d <- data.table(readxl::read_excel(file.path(RAWmisc::PROJ$RAW,"WP1_waterworks/Asker og Bærum vannverk IKS_Kattås/Rådata/Historiske data fra 2006_Holsfjorden.xlsx"),
                                       sheet = "kimtall int lab",
                                       skip = 3,
                                       col_names = FALSE,
                                       col_types = c("date", rep("text", 1))))
  setnames(d, c("date", "Colony count"))

  d <- melt.data.table(d, id = "date", variable.factor = FALSE)
  d[, type := "Internal"]
  unique(d$variable)
  d[, units := ""]
  d[variable == "Colony count", units := "CFU/mL"]
  d[, waterwork := "Asker og Baerum vannverk IKS_Kattås"]
  d[, point := ""]
  d[, waterType := "Raw"]

  saveRDS(d, file.path(RAWmisc::PROJ$CLEAN,"WP1_waterworks/Asker og Bærum vannverk IKS_Kattås_internal2.RDS"))

  ########
  # data_raw/WP1_waterworks/Asker og Bærum vannverk IKS_Kattås/Rådata/Historiske data fra 2006_Holsfjorden.xlsx
  #
  # online
  ########

  d <- data.table(readxl::read_excel(file.path(RAWmisc::PROJ$RAW,"WP1_waterworks/Asker og Bærum vannverk IKS_Kattås/Rådata/Historiske data fra 2006_Holsfjorden.xlsx"),
                                         sheet = "turb online",
                                         skip = 3,
                                         col_names = FALSE,
                                         col_types = c("date", rep("text", 1))))
  setnames(d, c("date", "Turbidity"))

  d <- melt.data.table(d, id = "date", variable.factor = FALSE)
  d[, type := "Online"]
  unique(d$variable)
  d[, units := ""]
  d[variable == "Turbidity", units := "FNU"]
  d[, waterwork := "Asker og Baerum vannverk IKS_Kattås"]
  d[, point := ""]
  d[, waterType := "Raw"]

  saveRDS(d, file.path(RAWmisc::PROJ$CLEAN,"WP1_waterworks/Asker og Bærum vannverk IKS_Kattås_online.RDS"))

  ########
  # data_raw/WP1_waterworks/Bergen_Espeland/Rådata/FHI_vannkvalitetsdata_Bergen vannverkOppdatertSvartediket170316.xlsx
  #
  # Svartediket
  ########

  d <- data.table(readxl::read_excel(
    file.path(RAWmisc::PROJ$RAW,"WP1_waterworks/Bergen_Espeland/Rådata/FHI_vannkvalitetsdata_Bergen vannverkOppdatertSvartediket170316.xlsx"),
                                           sheet = "Svartediket",
                                           skip = 3,
                                           col_names = FALSE,
                                           col_types = c("text", "date", rep("text", 6),
                                             "date", rep("text", 2),
                                             "date", rep("text", 2))))
  setnames(d, c("point", "date",
    "Conductivity",
    "Coliform bacteria",
    "Intestinal Enterococci",
    "Colony count",
    "pH",
    "point",
    "date",
    "E. Coli",
    "point",
    "date",
    "Colour",
    "Turbidity"
    ))
  bigData <- copy(d)

  d <- bigData[, 1:7, with = F]
  d <- melt.data.table(d, id = c("date", "point"), variable.factor = FALSE)
  d[, type := "Accredited"]
  unique(d$variable)
  d[, units := ""]
  d[variable == "Conductivity", units := "mS/m"]
  d[variable == "Coliform bacteria", units := "ant/100ml"]
  d[variable == "Intestinal Enterococci", units := "ant/100ml"]
  d[variable == "Colony count", units := "ant/ml"]
  d[variable == "pH", units := "pH"]
  d[, waterwork := "Bergen_Espeland"]
  d[, waterType := "Raw"]
  d[, point:="Svartediket"]

  saveRDS(d, file.path(RAWmisc::PROJ$CLEAN,"WP1_waterworks/Bergen_Espeland_Svartediket_1.RDS"))

  d <- bigData[, 8:10, with = F]
  d <- melt.data.table(d, id = c("date", "point"), variable.factor = FALSE)
  d[, type := "Accredited"]
  unique(d$variable)
  d[, units := ""]
  d[variable == "E. Coli", units := "ant/100 ml"]
  d[, waterwork := "Bergen_Espeland"]
  d[, waterType := "Raw"]
  d[, point:="Svartediket"]

  saveRDS(d, file.path(RAWmisc::PROJ$CLEAN,"WP1_waterworks/Bergen_Espeland_Svartediket_2.RDS"))

  d <- bigData[, 11:14, with = F]
  d <- melt.data.table(d, id = c("date", "point"), variable.factor = FALSE)
  d[, type := "Accredited"]
  unique(d$variable)
  d[, units := ""]
  d[variable == "Colour", units := "mg Pt/l"]
  d[variable == "Turbidity", units := "FNU"]
  d[, waterwork := "Bergen_Espeland"]
  d[, waterType := "Raw"]
  d[, point:="Svartediket"]

  saveRDS(d, file.path(RAWmisc::PROJ$CLEAN,"WP1_waterworks/Bergen_Espeland_Svartediket_3.RDS"))

  #ONLINE
  CleanWP1SpecificWaterWork(fileIn=file.path(RAWmisc::PROJ$RAW,"WP1_waterworks/Bergen_Svartediket/Rådata/svartediket råvann.xls"),
                            fileOut="Bergen_Espeland_Svartediket_online.RDS",
                            sheet=1,
                            skip=6,
                            col_types=c("date",rep("text", 8)),
                            col_names=c("date",
                                        "X",
                                        "Conductivity",
                                        "X",
                                        "Turbidity",
                                        "X",
                                        "pH",
                                        "X",
                                        "Colour"
                                        ),
                            type="Online",
                            units=c("Conductivity"="mS/m",
                                    "Turbidity"="FNU"),
                            waterwork="Bergen_Espeland",
                            waterType="Raw",
                            point="Svartediket"
  )
  
  ########
  # data_raw/WP1_waterworks/Bergen_Espeland/Rådata/FHI_vannkvalitetsdata_Bergen vannverkOppdatertSvartediket170316.xlsx
  #
  # Espeland
  ########

  d <- data.table(readxl::read_excel(
    file.path(RAWmisc::PROJ$RAW,"WP1_waterworks/Bergen_Espeland/Rådata/FHI_vannkvalitetsdata_Bergen vannverkOppdatertSvartediket170316.xlsx"),
                                            sheet = "Espeland",
                                            skip = 3,
                                            col_names = FALSE,
                                            col_types = c("text", "date", rep("text", 6),
                                              "date", rep("text", 2),
                                              "date", rep("text", 2))))
  setnames(d, c("point", "date",
      "Conductivity",
      "Coliform bacteria",
      "Intestinal Enterococci",
      "Colony count",
      "pH",
      "point",
      "date",
      "E. Coli",
      "point",
      "date",
      "Colour",
      "Turbidity"))
  bigData <- copy(d)

  d <- bigData[, 1:7, with = F]
  d <- melt.data.table(d, id = c("date", "point"), variable.factor = FALSE)
  d[, type := "Accredited"]
  unique(d$variable)
  d[, units := ""]
  d[variable == "Conductivity", units := "mS/m"]
  d[variable == "Coliform bacteria", units := "ant/100ml"]
  d[variable == "Intestinal Enterococci", units := "ant/100ml"]
  d[variable == "Colony count", units := "ant/ml"]
  d[variable == "pH", units := "pH"]
  d[, waterwork := "Bergen_Espeland"]
  d[, waterType := "Raw"]

  saveRDS(d, file.path(RAWmisc::PROJ$CLEAN,"WP1_waterworks/Bergen_Espeland_Espeland_1.RDS"))

  d <- bigData[, 8:10, with = F]
  d <- melt.data.table(d, id = c("date", "point"), variable.factor = FALSE)
  d[, type := "Accredited"]
  unique(d$variable)
  d[, units := ""]
  d[variable == "E. Coli", units := "ant/100 ml"]
  d[, waterwork := "Bergen_Espeland"]
  d[, waterType := "Raw"]

  saveRDS(d, file.path(RAWmisc::PROJ$CLEAN,"WP1_waterworks/Bergen_Espeland_Espeland_2.RDS"))

  d <- bigData[, 11:14, with = F]
  d <- melt.data.table(d, id = c("date", "point"), variable.factor = FALSE)
  d[, type := "Accredited"]
  unique(d$variable)
  d[, units := ""]
  d[variable == "Colour", units := "mg Pt/l"]
  d[variable == "Turbidity", units := "FNU"]
  d[, waterwork := "Bergen_Espeland"]
  d[, waterType := "Raw"]

  saveRDS(d, file.path(RAWmisc::PROJ$CLEAN,"WP1_waterworks/Bergen_Espeland_Espeland_3.RDS"))

  
  #ONLINE
  CleanWP1SpecificWaterWork(fileIn=file.path(RAWmisc::PROJ$RAW,"WP1_waterworks/Bergen_Espeland/Rådata/espeland råvann.xls"),
                            fileOut="Bergen_Espeland_Espeland_online.RDS",
                            sheet=1,
                            skip=1,
                            col_types=c("date",rep("text", 2)),
                            col_names=c("date",
                                        "X",
                                        "pH"
                                        ),
                            type="Online",
                            units=NULL,
                            waterwork="Bergen_Espeland",
                            waterType="Raw",
                            point="Espeland, rå"
  )
  
  ########
  # data_raw/WP1_waterworks/Bergen_Espeland/Rådata/FHI_vannkvalitetsdata_Bergen vannverkOppdatertSvartediket170316.xlsx
  #
  # Jordalsv
  ########

  d <- data.table(readxl::read_excel(
    file.path(RAWmisc::PROJ$RAW,"WP1_waterworks/Bergen_Espeland/Rådata/FHI_vannkvalitetsdata_Bergen vannverkOppdatertSvartediket170316.xlsx"),
                                              sheet = "Jordalsv",
                                              skip = 3,
                                              col_names = FALSE,
                                              col_types = c("text", "date", rep("text", 6),
                                                          "date", rep("text", 2),
                                                          "date", rep("text", 2))))
  setnames(d, c("point", "date",
        "Conductivity",
        "Coliform bacteria",
        "Intestinal Enterococci",
        "Colony count",
        "pH",
        "point",
        "date",
        "E. Coli",
        "point",
        "date",
        "Colour",
        "Turbidity"))
  bigData <- copy(d)

  d <- bigData[, 1:7, with = F]
  d <- melt.data.table(d, id = c("date", "point"), variable.factor = FALSE)
  d[, type := "Accredited"]
  unique(d$variable)
  d[, units := ""]
  d[variable == "Conductivity", units := "mS/m"]
  d[variable == "Coliform bacteria", units := "ant/100ml"]
  d[variable == "Intestinal Enterococci", units := "ant/100ml"]
  d[variable == "Colony count", units := "ant/ml"]
  d[variable == "pH", units := "pH"]
  d[, waterwork := "Bergen_Espeland"]
  d[, waterType := "Raw"]

  saveRDS(d, file.path(RAWmisc::PROJ$CLEAN,"WP1_waterworks/Bergen_Espeland_Jordalsv_1.RDS"))

  d <- bigData[, 8:10, with = F]
  d <- melt.data.table(d, id = c("date", "point"), variable.factor = FALSE)
  d[, type := "Accredited"]
  unique(d$variable)
  d[, units := ""]
  d[variable == "E. Coli", units := "ant/100 ml"]
  d[, waterwork := "Bergen_Espeland"]
  d[, waterType := "Raw"]

  saveRDS(d, file.path(RAWmisc::PROJ$CLEAN,"WP1_waterworks/Bergen_Espeland_Jordalsv_2.RDS"))

  d <- bigData[, 11:14, with = F]
  d <- melt.data.table(d, id = c("date", "point"), variable.factor = FALSE)
  d[, type := "Accredited"]
  unique(d$variable)
  d[, units := ""]
  d[variable == "Colour", units := "mg Pt/l"]
  d[variable == "Turbidity", units := "FNU"]
  d[, waterwork := "Bergen_Espeland"]
  d[, waterType := "Raw"]

  saveRDS(d, file.path(RAWmisc::PROJ$CLEAN,"WP1_waterworks/Bergen_Espeland_Jordalsv_3.RDS"))

  CleanWP1SpecificWaterWork(fileIn=file.path(RAWmisc::PROJ$RAW,"WP1_waterworks/Bergen_Jordalsvatnet/Rådata/jordalsvatnet råvann.xls"),
                            fileOut="Bergen_Espeland_jordalsvatnet_online.RDS",
                            sheet=1,
                            skip=6,
                            col_types=c("date",rep("text", 6)),
                            col_names=c("date",
                                        "X",
                                        "Conductivity",
                                        "X",
                                        "Turbidity",
                                        "X",
                                        "pH"
                                        ),
                            type="Online",
                            units=c("Conductivity"="myS/cm2 ",
                                    "Turbidity"="FNU"),
                            waterwork="Bergen_Espeland",
                            waterType="Raw",
                            point="Jordalsvatnet, rå"
  )
  
  ########
  # data_raw/WP1_waterworks/Bergen_Espeland/Rådata/FHI_vannkvalitetsdata_Bergen vannverkOppdatertSvartediket170316.xlsx
  #
  # Kismul
  ########

  d <- data.table(readxl::read_excel(file.path(RAWmisc::PROJ$RAW,"WP1_waterworks/Bergen_Espeland/Rådata/FHI_vannkvalitetsdata_Bergen vannverkOppdatertSvartediket170316.xlsx"),
                                                sheet = "Kismul",
                                                skip = 3,
                                                col_names = FALSE,
                                                col_types = c("text", "date", rep("text", 6),
                                                              "date", rep("text", 2),
                                                              "date", rep("text", 2))))
  setnames(d, c("point", "date",
          "Conductivity",
          "Coliform bacteria",
          "Intestinal Enterococci",
          "Colony count",
          "pH",
          "point",
          "date",
          "E. Coli",
          "point",
          "date",
          "Colour",
          "Turbidity"))
  bigData <- copy(d)

  d <- bigData[, 1:7, with = F]
  d <- melt.data.table(d, id = c("date", "point"), variable.factor = FALSE)
  d[, type := "Accredited"]
  unique(d$variable)
  d[, units := ""]
  d[variable == "Conductivity", units := "mS/m"]
  d[variable == "Coliform bacteria", units := "ant/100ml"]
  d[variable == "Intestinal Enterococci", units := "ant/100ml"]
  d[variable == "Colony count", units := "ant/ml"]
  d[variable == "pH", units := "pH"]
  d[, waterwork := "Bergen_Espeland"]
  d[, waterType := "Raw"]

  saveRDS(d, file.path(RAWmisc::PROJ$CLEAN,"WP1_waterworks/Bergen_Espeland_Kismul_1.RDS"))

  d <- bigData[, 8:10, with = F]
  d <- melt.data.table(d, id = c("date", "point"), variable.factor = FALSE)
  d[, type := "Accredited"]
  unique(d$variable)
  d[, units := ""]
  d[variable == "E. Coli", units := "ant/100 ml"]
  d[, waterwork := "Bergen_Espeland"]
  d[, waterType := "Raw"]

  saveRDS(d, file.path(RAWmisc::PROJ$CLEAN,"WP1_waterworks/Bergen_Espeland_Kismul_2.RDS"))

  d <- bigData[, 11:14, with = F]
  d <- melt.data.table(d, id = c("date", "point"), variable.factor = FALSE)
  d[, type := "Accredited"]
  unique(d$variable)
  d[, units := ""]
  d[variable == "Colour", units := "mg Pt/l"]
  d[variable == "Turbidity", units := "FNU"]
  d[, waterwork := "Bergen_Espeland"]
  d[, waterType := "Raw"]

  saveRDS(d, file.path(RAWmisc::PROJ$CLEAN,"WP1_waterworks/Bergen_Espeland_Kismul_3.RDS"))

    CleanWP1SpecificWaterWork(fileIn=file.path(RAWmisc::PROJ$RAW,"WP1_waterworks/Bergen_Kismul/Rådata/kismul råvann.xls"),
                            fileOut="Bergen_Espeland_kismul_online.RDS",
                            sheet=1,
                            skip=6,
                            col_types=c("date",rep("text", 4)),
                            col_names=c("date",
                                        "X",
                                        "pH",
                                        "X",
                                        "Turbidity"
                                        ),
                            type="Online",
                            units=NULL,
                            waterwork="Bergen_Espeland",
                            waterType="Raw",
                            point="Kismul, rå"
  )
  
  ########
  # data_raw/WP1_waterworks/Bergen_Espeland/Rådata/FHI_vannkvalitetsdata_Bergen vannverkOppdatertSvartediket170316.xlsx
  #
  # Sædalen
  ########

  d <- data.table(readxl::read_excel(file.path(RAWmisc::PROJ$RAW,"WP1_waterworks/Bergen_Espeland/Rådata/FHI_vannkvalitetsdata_Bergen vannverkOppdatertSvartediket170316.xlsx"),
                                                  sheet = "Sædalen",
                                                  skip = 3,
                                                  col_names = FALSE,
                                                  col_types = c("text", "date", rep("text", 6),
                                                    "date", rep("text", 2),
                                                    "date", rep("text", 2))))
  setnames(d, c("point", "date",
            "Conductivity",
            "Coliform bacteria",
            "Intestinal Enterococci",
            "Colony count",
            "pH",
            "point",
            "date",
            "E. Coli",
            "point",
            "date",
            "Colour",
            "Turbidity"))
  bigData <- copy(d)

  d <- bigData[, 1:7, with = F]
  d <- melt.data.table(d, id = c("date", "point"), variable.factor = FALSE)
  d[, type := "Accredited"]
  unique(d$variable)
  d[, units := ""]
  d[variable == "Conductivity", units := "mS/m"]
  d[variable == "Coliform bacteria", units := "ant/100ml"]
  d[variable == "Intestinal Enterococci", units := "ant/100ml"]
  d[variable == "Colony count", units := "ant/ml"]
  d[variable == "pH", units := "pH"]
  d[, waterwork := "Bergen_Espeland"]
  d[, waterType := "Raw"]

  saveRDS(d, file.path(RAWmisc::PROJ$CLEAN,"WP1_waterworks/Bergen_Espeland_Sædalen_1.RDS"))

  d <- bigData[, 8:10, with = F]
  d <- melt.data.table(d, id = c("date", "point"), variable.factor = FALSE)
  d[, type := "Accredited"]
  unique(d$variable)
  d[, units := ""]
  d[variable == "E. Coli", units := "ant/100 ml"]
  d[, waterwork := "Bergen_Espeland"]
  d[, waterType := "Raw"]

  saveRDS(d, file.path(RAWmisc::PROJ$CLEAN,"WP1_waterworks/Bergen_Espeland_Sædalen_2.RDS"))

  d <- bigData[, 11:14, with = F]
  d <- melt.data.table(d, id = c("date", "point"), variable.factor = FALSE)
  d[, type := "Accredited"]
  unique(d$variable)
  d[, units := ""]
  d[variable == "Colour", units := "mg Pt/l"]
  d[variable == "Turbidity", units := "FNU"]
  d[, waterwork := "Bergen_Espeland"]
  d[, waterType := "Raw"]

  saveRDS(d, file.path(RAWmisc::PROJ$CLEAN,"WP1_waterworks/Bergen_Espeland_Sædalen_3.RDS"))

  
    CleanWP1SpecificWaterWork(fileIn=file.path(RAWmisc::PROJ$RAW,"WP1_waterworks/Bergen_Sædalen/Rådata/sædalen råvann.xls"),
                            fileOut="Bergen_Espeland_saedalen_online.RDS",
                            sheet=1,
                            skip=6,
                            col_types=c("date",rep("text", 4)),
                            col_names=c("date",
                                        "X",
                                        "pH",
                                        "X",
                                        "Turbidity"
                                        ),
                            type="Online",
                            units=NULL,
                            waterwork="Bergen_Espeland",
                            waterType="Raw",
                            point="Sædalen, rå"
  )
  
  
  ########
  # FREVAR_Høyfjell\Rådata\Akkrediterte laboratorier
  #
  # 
  ########
  
  d <- data.table(readxl::read_excel(
    file.path(RAWmisc::PROJ$RAW,"WP1_waterworks/FREVAR_Høyfjell/Rådata/Akkrediterte laboratorier/accredited.xlsx"),
                                     sheet = 1,
                                     skip = 3,
                                     col_names = FALSE,
                                     col_types = c("date", rep("text", 17))))
  setnames(d, c("date",
                "Coliform bacteria",
                "E. Coli",
                "Clostridium Perfringens",
                "Intestinal Enterococci",
                "Colony count",
                "X","X",
                "pH",
                "Conductivity",
                "Turbidity",
                "Colour",
                "X","X","X",
                "TOC",
                "X","X"))
  d <- d[, which(names(d) != "X"), with = F]
  d[,point:="?"]
  
  d <- melt.data.table(d, id = c("date", "point"), variable.factor = FALSE)
  d[, type := "Accredited"]
  unique(d$variable)
  d[, units := ""]
  
  d[variable == "Coliform bacteria", units := "ant/100ml"]
  d[variable == "E. Coli", units := "ant/100ml"]
  d[variable == "Clostridium Perfringens", units := "ant/100ml"]
  d[variable == "Intestinal Enterococci", units := "ant/100ml"]
  d[variable == "Colony count", units := "ant/ml"]
  d[variable == "pH", units := "pH"]
  d[variable == "Conductivity", units := "mS/m"]
  d[variable == "Turbidity", units := "FTU"]
  d[variable == "Colour", units := ""]
  d[variable == "TOC", units := "mg/l"]
  
  d[, waterwork := "FREVAR_Høyfjell"]
  d[, waterType := "Raw"]
  
  saveRDS(d, file.path(RAWmisc::PROJ$CLEAN,"WP1_waterworks/FREVAR_Høyfjell_accredited.RDS"))
  
  ########
  # FREVAR_Høyfjell\Rådata\Online_egenanalyser
  #
  # 
  ########
  
  d <- data.table(readxl::read_excel(file.path(RAWmisc::PROJ$RAW,"WP1_waterworks/FREVAR_Høyfjell/Rådata/Online_egenanalyser/online_all.xlsx"),
                                     sheet = 1,
                                     skip = 3,
                                     col_names = FALSE,
                                     col_types = c("date", rep("text", 15))))
  setnames(d, c("date",
                "Colour","X",
                "Turbidity","X",
                "X","X",
                "Temperature","X",
                "pH","X",
                "X","X","X","X","X"
                ))
  d <- d[, which(names(d) != "X"), with = F]
  d[,point:="?"]
  
  d <- melt.data.table(d, id = c("date", "point"), variable.factor = FALSE)
  d[, type := "Online"]
  unique(d$variable)
  d[, units := ""]
  
  d[variable == "Colour", units := "mg Pt/L"]
  d[variable == "Turbidity", units := "NTU"]
  d[variable == "Temperature", units := "C"]
  d[variable == "pH", units := "pH"]
  
  d[, waterwork := "FREVAR_Høyfjell"]
  d[, waterType := "Raw"]
  
  saveRDS(d, file.path(RAWmisc::PROJ$CLEAN,"WP1_waterworks/FREVAR_Høyfjell_online.RDS"))
  
  ########
  # FREVAR_Høyfjell\Rådata\Online_egenanalyser
  #
  # 
  ########
  
  d <- data.table(readxl::read_excel(file.path(RAWmisc::PROJ$RAW,"WP1_waterworks/FREVAR_Høyfjell/Rådata/Online_egenanalyser/online_all.xlsx"),
                                     sheet = 1,
                                     skip = 3,
                                     col_names = FALSE,
                                     col_types = c("date", rep("text", 15))))
  setnames(d, c("date",
                "X","Colour",
                "X","Turbidity",
                "X","X",
                "X","Temperature",
                "X","pH",
                "X","X","X","X","X"
  ))
  d <- d[, which(names(d) != "X"), with = F]
  d[Colour==shift(Colour,type="lag"),Colour:=NA]
  d[Turbidity==shift(Turbidity,type="lag"),Turbidity:=NA]
  d[Temperature==shift(Temperature,type="lag"),Temperature:=NA]
  d[pH==shift(pH,type="lag"),pH:=NA]
  
  d[,point:="?"]
  
  d <- melt.data.table(d, id = c("date", "point"), variable.factor = FALSE)
  d[, type := "Internal"]
  unique(d$variable)
  d[, units := ""]
  
  d[variable == "Colour", units := "mg Pt/L"]
  d[variable == "Turbidity", units := "NTU"]
  d[variable == "Temperature", units := "C"]
  d[variable == "pH", units := "pH"]
  
  d[, waterwork := "FREVAR_Høyfjell"]
  d[, waterType := "Raw"]
  
  saveRDS(d, file.path(RAWmisc::PROJ$CLEAN,"WP1_waterworks/FREVAR_Høyfjell_internal.RDS"))
  
  
  ########
  # data_raw/WP1_waterworks/Glitrevannverket IKS_Kleivdammen/Rådata/Glitre råvann 2000-16.xlsx
  #
  # Landfall
  ########

  d <- data.table(readxl::read_excel(
    file.path(RAWmisc::PROJ$RAW,"WP1_waterworks/Glitrevannverket IKS_Kleivdammen/Rådata/Glitre råvann 2000-16.xlsx"),
                                                      sheet = "Landfall",
                                                      skip = 1,
                                                      col_names = FALSE,
                                                      col_types = c("date", rep("text", 27))))

  setnames(d, c("date",
    "Colony count",
    "Coliform bacteria",
    "E. Coli",
    "X",
    "Intestinal Enterococci",
    "X",
    "Clostridium Perfringens",
    "X",
    "X",
    "X",
    "TOC",
    "X",
    "Turbidity",
    "X",
    "X",
    "pH",
    "X",
    "X",
    "X",
    "Colour",
    "X",
    "X",
    "X",
    "X",
    "X",
    "X",
    "X"))
  
  d <- d[, which(names(d) != "X"), with = F]

  d <- melt.data.table(d, id = c("date"), variable.factor = FALSE)
  d[, type := "Accredited"]
  unique(d$variable)
  d[, units := ""]
  d[, waterwork := "Glitrevannverket IKS_Landfall"]
  d[, waterType := "Raw"]
  d[, point := "Landfall"]

  saveRDS(d, file.path(RAWmisc::PROJ$CLEAN,"WP1_waterworks/Glitrevannverket IKS_Kleivdammen_Landfall_online_turbidity.RDS"))
  
  ########
  # data_raw/WP1_waterworks/Glitrevannverket IKS_Landfall/Rådata/online_turbidity.xlsx
  #
  # Landfall
  ########
  
  d <- data.table(readxl::read_excel(
    file.path(RAWmisc::PROJ$RAW,"WP1_waterworks/Glitrevannverket IKS_Landfall/Rådata/online_turbidity.xlsx"),
                                     sheet = 1,
                                     skip = 2,
                                     col_names = FALSE,
                                     col_types = c("date", rep("text", 1))))
  
  setnames(d, c("date",
                "Turbidity"))
  
  d <- d[, which(names(d) != "X"), with = F]
  
  d <- melt.data.table(d, id = c("date"), variable.factor = FALSE)
  d[, type := "Online"]
  unique(d$variable)
  d[, units := ""]
  d[, waterwork := "Glitrevannverket IKS_Landfall"]
  d[, waterType := "Raw"]
  d[, point := "Landfall"]
  
  saveRDS(d, file.path(RAWmisc::PROJ$CLEAN,"WP1_waterworks/Glitrevannverket IKS_Kleivdammen_Landfall.RDS"))


  ########
  # data_raw/WP1_waterworks/Glitrevannverket IKS_Kleivdammen/Rådata/Glitre råvann 2000-16.xlsx
  #
  # Eggevollen
  ########

  d <- data.table(readxl::read_excel(file.path(RAWmisc::PROJ$RAW,"WP1_waterworks/Glitrevannverket IKS_Kleivdammen/Rådata/Glitre råvann 2000-16.xlsx"),
                                                        sheet = "Eggevollen",
                                                        skip = 1,
                                                        col_names = FALSE,
                                                        col_types = c("date", rep("text", 32))))

  setnames(d, c("date",
      "E. Coli",
      "Colony count",
      "Coliform bacteria",
      "Intestinal Enterococci",
      "X",
      "Clostridium Perfringens",
      "X",
      "X",
      "X",
      "X",
      "X",
      "X",
      "X",
      "pH",
      "TOC",
      "Conductivity",
      "X",
      "Colour",
      "Turbidity",
      "X",
      "X",
      "X",
      "X",
      "X",
      "X",
      "X",
      "X",
      "X",
      "X",
      "X",
      "X",
      "X"))

  d <- d[, which(names(d) != "X"), with = F]

  d <- melt.data.table(d, id = c("date"), variable.factor = FALSE)
  d[, type := "Accredited"]
  unique(d$variable)
  d[, units := ""]
  d[, waterwork := "Glitrevannverket IKS_Kleivdammen"]
  d[, waterType := "Raw"]
  d[, point := "Eggevollen"]

  saveRDS(d, file.path(RAWmisc::PROJ$CLEAN,"WP1_waterworks/Glitrevannverket IKS_Kleivdammen_Eggevollen.RDS"))

  ########
  # data_raw/WP1_waterworks/Glitrevannverket IKS_Kleivdammen/Rådata/Glitre råvann 2000-16.xlsx
  #
  # Kleivdammen
  ########

  d <- data.table(readxl::read_excel(file.path(RAWmisc::PROJ$RAW,"WP1_waterworks/Glitrevannverket IKS_Kleivdammen/Rådata/Glitre råvann 2000-16.xlsx"),
                                                          sheet = "Kleivdammen",
                                                          skip = 1,
                                                          col_names = FALSE,
                                                          col_types = c("date", rep("text", 22))))

  setnames(d, c("date",
        "Colony count",
        "E. Coli",
        "Coliform bacteria",
        "X",
        "Intestinal Enterococci",
        "Clostridium Perfringens",
        "X", "X", "X", "X",
        "Conductivity",
        "X",
        "pH",
        "Turbidity",
        "Colour",
        "X","X","X","X","X","X","TOC"))

  d <- d[, which(names(d) != "X"), with = F]

  d <- melt.data.table(d, id = c("date"), variable.factor = FALSE)
  d[, type := "Accredited"]
  unique(d$variable)
  d[, units := ""]
  d[, waterwork := "Glitrevannverket IKS_Kleivdammen"]
  d[, waterType := "Raw"]
  d[, point := "Kleivdammen"]

  saveRDS(d, file.path(RAWmisc::PROJ$CLEAN,"WP1_waterworks/Glitrevannverket IKS_Kleivdammen_Kleivdammen.RDS"))

  ########
  # data_raw/WP1_waterworks/Halden_Lille Erte/Rådata/Periodisk rapport, 01.01.2011 - 31.12.2012, Råvann Vannbehandlingsanlegg.xlsx
  #
  # Periodisk rapport, 01.01.2011 -
  ########

  d <- data.table(readxl::read_excel(
    file.path(RAWmisc::PROJ$RAW,"WP1_waterworks/Halden_Lille Erte/Rådata/Periodisk rapport, 01.01.2011 - 31.12.2012, Råvann Vannbehandlingsanlegg.xlsx"),
                                                            sheet = "Periodisk rapport, 01.01.2011 -",
                                                            skip = 3,
                                                            col_names = FALSE,
                                                            col_types = c("date", rep("text", 17))))

  setnames(d, c("date",
    "Colour",
    "X",
    "Turbidity",
    "X",
    "X",
    "pH",
    "Temperature",
    "X", "X", "X", "X", "X", "X", "X",
    "Coliform bacteria",
    "E. Coli",
    "Colony count"))

  d <- d[, which(names(d) != "X"), with = F]

  d <- melt.data.table(d, id = c("date"), variable.factor = FALSE)
  d[, type := "Internal"]
  unique(d$variable)
  d[, units := ""]
  d[, waterwork := "Halden_Lille Erte"]
  d[, waterType := "Raw"]
  d[, point := ""]

  saveRDS(d, file.path(RAWmisc::PROJ$CLEAN,"WP1_waterworks/Halden_Lille Erte_1.RDS"))

  ########
  # data_raw/WP1_waterworks/Halden_Lille Erte/Rådata/Periodisk rapport, 01.01.2011 - 31.12.2012, Råvann Vannbehandlingsanlegg.xlsx
  #
  # Periodisk rapport, 01.01.2013 -
  ########

  d <- data.table(readxl::read_excel(
    file.path(RAWmisc::PROJ$RAW,"WP1_waterworks/Halden_Lille Erte/Rådata/Periodisk rapport, 01.01.2013 - 31.12.2014, Råvann Vannbehandlingsanlegg.xlsx"),
                                                              sheet = "Periodisk rapport, 01.01.2013 -",
                                                              skip = 3,
                                                              col_names = FALSE,
                                                              col_types = c("date", rep("text", 17))))

  setnames(d, c("date",
      "Colour",
      "X",
      "Turbidity",
      "X",
      "X",
      "pH",
      "Temperature",
      "X", "X", "X", "X", "X", "X", "X",
      "Coliform bacteria",
      "E. Coli",
      "Colony count"))

  d <- d[, which(names(d) != "X"), with = F]

  d <- melt.data.table(d, id = c("date"), variable.factor = FALSE)
  d[, type := "Internal"]
  unique(d$variable)
  d[, units := ""]
  d[, waterwork := "Halden_Lille Erte"]
  d[, waterType := "Raw"]
  d[, point := ""]

  saveRDS(d, file.path(RAWmisc::PROJ$CLEAN,"WP1_waterworks/Halden_Lille Erte_2.RDS"))

  ########
  # data_raw/WP1_waterworks/Halden_Lille Erte/Rådata/Periodisk rapport, 01.01.2015 - 31.12.2015, Råvann Vannbehandlingsanlegg.xlsx
  #
  # Periodisk rapport, 01.01.2013 -
  ########

  d <- data.table(readxl::read_excel(file.path(RAWmisc::PROJ$RAW,"WP1_waterworks/Halden_Lille Erte/Rådata/Periodisk rapport, 01.01.2015 - 31.12.2015, Råvann Vannbehandlingsanlegg.xlsx"),
                                                                sheet = "Periodisk rapport, 01.01.2015 -",
                                                                skip = 3,
                                                                col_names = FALSE,
                                                                col_types = c("date", rep("text", 17))))

  setnames(d, c("date",
        "Colour",
        "X",
        "Turbidity",
        "X",
        "X",
        "pH",
        "Temperature",
        "X", "X", "X", "X", "X", "X", "X",
        "Coliform bacteria",
        "E. Coli",
        "Colony count"))

  d <- d[, which(names(d) != "X"), with = F]

  d <- melt.data.table(d, id = c("date"), variable.factor = FALSE)
  d[, type := "Internal"]
  unique(d$variable)
  d[, units := ""]
  d[, waterwork := "Halden_Lille Erte"]
  d[, waterType := "Raw"]
  d[, point := ""]

  saveRDS(d, file.path(RAWmisc::PROJ$CLEAN,"WP1_waterworks/Halden_Lille Erte_3.RDS"))

  ########
  # data_raw/WP1_waterworks/HIAS_Hamar/Rådata/Råvannskvalitet Hamar VBA2003-2015-31aug.xlsx
  #
  # 2015-2014
  ########

  d <- data.table(readxl::read_excel(file.path(RAWmisc::PROJ$RAW,"WP1_waterworks/HIAS_Hamar/Rådata/edited_Råvannskvalitet Hamar VBA2003-2015-31aug.xlsx"),
                                                                      sheet = 1,
                                                                      skip = 10,
                                                                      col_names = FALSE,
                                                                      col_types = c("date", rep("text", 25))))

  setnames(d, c("date",
  "Colour",
  "Turbidity",
  "pH",
  "X",
  "Conductivity",
  "X",
  "X",
  "Colony count",
  "Coliform bacteria",
  "E. Coli",
  "Clostridium Perfringens",
  "Intestinal Enterococci",
  "X","X","X","TOC","X","X","X","X","X","X","X","X","X"))

  d <- d[, which(names(d) != "X"), with = F]

  d <- melt.data.table(d, id = c("date"), variable.factor = FALSE)
  d[, type := "Accredited"]
  unique(d$variable)
  d[, units := ""]
  d[variable == "Colour", units := "mg/lPt"]
  d[variable == "Turbidity", units := "FNU"]
  d[variable == "Conductivity", units := "mS/m"]
  d[variable == "Colony count", units := "/ml"]
  d[variable == "Coliform bacteria", units := "/100ml"]
  d[variable == "E. Coli", units := "/100ml"]
  d[variable == "Clostridium Perfringens", units := "/100ml"]
  d[, waterwork := "HIAS_Hamar"]
  d[, waterType := "Raw"]
  d[, point := ""]

  saveRDS(d, file.path(RAWmisc::PROJ$CLEAN,"WP1_waterworks/HIAS_Hamar.RDS"))
  
  
  CleanWP1SpecificWaterWorkLongSingle(fileIn=file.path(RAWmisc::PROJ$RAW,"WP1_waterworks/HIAS_Hamar/Rådata/Fhi råvann VBA 180516.xlsx"),
                            fileOut="hamar_VBA 180516.RDS",
                            sheet=1,
                            skip=8,
                            col_types=c("text",rep("text", 1)),
                            col_names=c("date",
                                        "value"
                                        ),
                            type="Online",
                            variable="Colour",
                            waterwork="HIAS_Hamar",
                            waterType="Raw",
                            point="?"
  )
  
  
  CleanWP1SpecificWaterWorkLongSingle(fileIn=file.path(RAWmisc::PROJ$RAW,"WP1_waterworks/HIAS_Hamar/Rådata/Fhi råvann VBA 180516.xlsx"),
                            fileOut="hamar_VBA turbidity180516.RDS",
                            sheet=2,
                            skip=8,
                            col_types=c("text",rep("text", 1)),
                            col_names=c("date",
                                        "value"
                                        ),
                            type="Online",
                            variable="Turbidity",
                            waterwork="HIAS_Hamar",
                            waterType="Raw",
                            point="?"
  )
  
  ##########################################
  ##########################################
  ##########################################

  
  ########
  # data_raw/WP1_waterworks/HIAS_Stange/Rådata/SVBA_Online_Fargetall.xlsx
  #
  # 2015-2014
  ########

  d <- data.table(readxl::read_excel(
    file.path(RAWmisc::PROJ$RAW,"WP1_waterworks/HIAS_Stange/Rådata/SVBA_Online_Fargetall.xlsx"),
                                                                      sheet = "2015-2014",
                                                                      skip = 1,
                                                                      col_names = FALSE,
                                                                      col_types = c("date", rep("text", 3))))

  setnames(d, c("date",
              "X",
              "X",
              "Colour"))

  d <- d[, which(names(d) != "X"), with = F]

  d <- melt.data.table(d, id = c("date"), variable.factor = FALSE)
  d[, type := "Online"]
  unique(d$variable)
  d[, units := ""]
  d[, waterwork := "HIAS_Stange"]
  d[, waterType := "Raw"]
  d[, point := ""]

  saveRDS(d, file.path(RAWmisc::PROJ$CLEAN,"WP1_waterworks/HIAS_Stange_colour.RDS"))

  ########
  # data_raw/WP1_waterworks/HIAS_Stange/Rådata/SVBA_Online_Turbiditet.xlsx
  #
  # 2015-2004
  ########

  d <- data.table(readxl::read_excel(file.path(RAWmisc::PROJ$RAW,"WP1_waterworks/HIAS_Stange/Rådata/SVBA_Online_Turbiditet.xlsx"),
                                                                          sheet = "2015-2004",
                                                                          skip = 1,
                                                                          col_names = FALSE,
                                                                          col_types = c("date", rep("text", 3))))

  setnames(d, c("date",
                  "X",
                  "X",
                  "Turbidity"))

  d <- d[, which(names(d) != "X"), with = F]

  d <- melt.data.table(d, id = c("date"), variable.factor = FALSE)
  d[, type := "Online"]
  unique(d$variable)
  d[, units := ""]
  d[, waterwork := "HIAS_Stange"]
  d[, waterType := "Raw"]
  d[, point := ""]

  saveRDS(d, file.path(RAWmisc::PROJ$CLEAN,"WP1_waterworks/HIAS_Stange_turbidity.RDS"))


  ########
  # data_raw/WP1_waterworks/HIAS_Stange/Rådata/edited_Råvannskvalitet Stange VBA2003-2015-31aug.xlsx
  #
  # 2015-2014
  ########

  d <- data.table(readxl::read_excel(file.path(RAWmisc::PROJ$RAW,"WP1_waterworks/HIAS_Stange/Rådata/edited_Råvannskvalitet Stange VBA2003-2015-31aug.xlsx"),
                                                                        sheet = 1,
                                                                        skip = 12,
                                                                        col_names = FALSE,
                                                                        col_types = c("date", rep("text", 27))))

  setnames(d, c("date",
    "Colour",
    "Turbidity",
    "pH",
    "Conductivity",
    "X",
    "X",
    "Colony count",
    "Coliform bacteria",
    "E. Coli",
    "Clostridium Perfringens",
    "Intestinal Enterococci",
    "X", "X", "X", "TOC", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X"))

  d <- d[, which(names(d) != "X"), with = F]

  d <- melt.data.table(d, id = c("date"), variable.factor = FALSE)
  d[, type := "Accredited"]
  unique(d$variable)
  d[, units := ""]
  d[variable == "Colour", units := "mg/lPt"]
  d[variable == "Turbidity", units := "FNU"]
  d[variable == "Conductivity", units := "mS/m"]
  d[variable == "Colony count", units := "/ml"]
  d[variable == "Coliform bacteria", units := "/100ml"]
  d[variable == "E. Coli", units := "/100ml"]
  d[variable == "Clostridium Perfringens", units := "/100ml"]
  d[, waterwork := "HIAS_Stange"]
  d[, waterType := "Raw"]
  d[, point := ""]

  saveRDS(d, file.path(RAWmisc::PROJ$CLEAN,"WP1_waterworks/HIAS_Stange.RDS"))

  ##########################################
  ##########################################
  ##########################################

  #####
  ########
  # data_raw/WP1_waterworks/IVAR IKS_Langevatn/IVAR Råvann analyseresultater 2010-2016.xlsx
  # 
  #
  ########
  
  
  CleanWP1SpecificWaterWork(fileIn=file.path(RAWmisc::PROJ$RAW,"WP1_waterworks/IVAR IKS_Langevatn/IVAR Råvann analyseresultater 2010-2016.xlsx"),
                            fileOut="IVAR Råvann analyseresultaterStolsvatn.RDS",
                            sheet="Analyseresultater Stølsvatn ve",
                            skip=3,
                            col_types=c("date",rep("text", 9)),
                            col_names=c("date",
                                        "Coliform bacteria",
                                        "E. Coli",
                                        "Turbidity",
                                        "Colour",
                                        "X",
                                        "X",
                                        "X",
                                        "X",
                                        "X"),
                            type="Accredited",
                            units=c("Colony count"="cfu/ml",
                                    "Coliform bacteria"="MPN/100 ml",
                                    "E. Coli"="MPN/100 ml",
                                    "Clostridium Perfringens"="cfu/100 ml",
                                    "Intestinal Enterococci"="cfu/100 ml",
                                    "Turbidity"="FTU",
                                    "Colour"="mg Pt/L",
                                    "TOC"="mg/L"),
                            waterwork="IVAR IKS_Langevatn",
                            waterType="Raw",
                            point="Stolsvatn"
  )
  
  CleanWP1SpecificWaterWork(fileIn=file.path(RAWmisc::PROJ$RAW,"WP1_waterworks/IVAR IKS_Langevatn/IVAR Råvann analyseresultater 2010-2016.xlsx"),
                            fileOut="IVAR Råvann analyseresultaterStorevatn.RDS",
                            sheet="Analyseresultater Storevatn",
                            skip=3,
                            col_types=c("date",rep("text", 9)),
                            col_names=c("date",
                                        "Coliform bacteria",
                                        "E. Coli",
                                        "Turbidity",
                                        "Colour",
                                        "X",
                                        "X",
                                        "X",
                                        "X",
                                        "X"),
                            type="Accredited",
                            units=c("Colony count"="cfu/ml",
                                    "Coliform bacteria"="MPN/100 ml",
                                    "E. Coli"="MPN/100 ml",
                                    "Clostridium Perfringens"="cfu/100 ml",
                                    "Intestinal Enterococci"="cfu/100 ml",
                                    "Turbidity"="FTU",
                                    "Colour"="mg Pt/L",
                                    "TOC"="mg/L"),
                            waterwork="IVAR IKS_Langevatn",
                            waterType="Raw",
                            point="Storevatn"
  )
  
  
  CleanWP1SpecificWaterWork(fileIn=file.path(RAWmisc::PROJ$RAW,"WP1_waterworks/IVAR IKS_Langevatn/IVAR Råvann analyseresultater 2010-2016.xlsx"),
                            fileOut="IVAR Råvann analyseresultater 2010-2016_online_stolsvatn.RDS",
                            sheet="Stølsvatn - onlinemålinger",
                            skip=3,
                            col_types=c("date",rep("text", 4)),
                            col_names=c("date",
                                        "X",
                                        "Turbidity",
                                        "Colour",
                                        "X"),
                            type="Online",
                            units=c("Colony count"="cfu/ml",
                                    "Coliform bacteria"="MPN/100 ml",
                                    "E. Coli"="MPN/100 ml",
                                    "Clostridium Perfringens"="cfu/100 ml",
                                    "Intestinal Enterococci"="cfu/100 ml",
                                    "Turbidity"="FTU",
                                    "Colour"="mg Pt/L",
                                    "TOC"="mg/L"),
                            waterwork="IVAR IKS_Langevatn",
                            waterType="Raw",
                            point="Stolsvatn"
  )
  
  CleanWP1SpecificWaterWork(fileIn=file.path(RAWmisc::PROJ$RAW,"WP1_waterworks/IVAR IKS_Langevatn/IVAR Råvann analyseresultater 2010-2016.xlsx"),
                            fileOut="IVAR Råvann analyseresultater 2010-2016_online_storevatn.RDS",
                            sheet="Storevatn Onlinemålinger",
                            skip=3,
                            col_types=c("date",rep("text", 4)),
                            col_names=c("date",
                                        "X",
                                        "Turbidity",
                                        "Colour",
                                        "X"),
                            type="Online",
                            units=c("Colony count"="cfu/ml",
                                    "Coliform bacteria"="MPN/100 ml",
                                    "E. Coli"="MPN/100 ml",
                                    "Clostridium Perfringens"="cfu/100 ml",
                                    "Intestinal Enterococci"="cfu/100 ml",
                                    "Turbidity"="FTU",
                                    "Colour"="mg Pt/L",
                                    "TOC"="mg/L"),
                            waterwork="IVAR IKS_Langevatn",
                            waterType="Raw",
                            point="Storevatn"
  )
  ########
  # data_raw/WP1_waterworks/Karmøy_Brekke/Rådata/Råvannskvalitet.xlsx
  #
  # kimtall
  ########

  d <- data.table(readxl::read_excel(file.path(RAWmisc::PROJ$RAW,"WP1_waterworks/Karmøy_Brekke/Rådata/Råvannskvalitet.xlsx"),
                                                                          sheet = "Kimtall",
                                                                          skip = 1,
                                                                          col_names = FALSE,
                                                                          col_types = c("date", rep("text", 4))))

  setnames(d, c("date",
      "X",
      "X",
      "X",
      "Colony count"))

  d <- d[, which(names(d) != "X"), with = F]

  d <- melt.data.table(d, id = c("date"), variable.factor = FALSE)
  d[, type := "Accredited"]
  unique(d$variable)
  d[, units := ""]
  d[variable == "Colony count", units := "/ml"]
  d[, waterwork := "Karmøy_Brekke"]
  d[, waterType := "Raw"]
  d[, point := ""]

  saveRDS(d, file.path(RAWmisc::PROJ$CLEAN,"WP1_waterworks/Karmøy_Brekke_colony_count.RDS"))

  ########
  # data_raw/WP1_waterworks/Karmøy_Brekke/Rådata/Råvannskvalitet.xlsx
  #
  # kimtall
  ########

  d <- data.table(readxl::read_excel(file.path(RAWmisc::PROJ$RAW,"WP1_waterworks/Karmøy_Brekke/Rådata/Råvannskvalitet.xlsx"),
                                                                            sheet = "Koliforme bakterier",
                                                                            skip = 1,
                                                                            col_names = FALSE,
                                                                            col_types = c("date", rep("text", 4))))

  setnames(d, c("date",
        "X",
        "X",
        "X",
        "Coliform bacteria"))

  d <- d[, which(names(d) != "X"), with = F]

  d <- melt.data.table(d, id = c("date"), variable.factor = FALSE)
  d[, type := "Accredited"]
  unique(d$variable)
  d[, units := ""]
  d[variable == "Coliform bacteria", units := "/100ml"]
  d[, waterwork := "Karmøy_Brekke"]
  d[, waterType := "Raw"]
  d[, point := ""]

  saveRDS(d, file.path(RAWmisc::PROJ$CLEAN,"WP1_waterworks/Karmøy_Brekke_coliform_bacteria.RDS"))

  ########
  # data_raw/WP1_waterworks/Karmøy_Brekke/Rådata/Råvannskvalitet.xlsx
  #
  # E.coli
  ########

  d <- data.table(readxl::read_excel(file.path(RAWmisc::PROJ$RAW,"WP1_waterworks/Karmøy_Brekke/Rådata/Råvannskvalitet.xlsx"),
                                                                              sheet = "E.coli",
                                                                              skip = 1,
                                                                              col_names = FALSE,
                                                                              col_types = c("date", rep("text", 4))))

  setnames(d, c("date",
          "X",
          "X",
          "X",
          "E. Coli"))

  d <- d[, which(names(d) != "X"), with = F]

  d <- melt.data.table(d, id = c("date"), variable.factor = FALSE)
  d[, type := "Accredited"]
  unique(d$variable)
  d[, units := ""]
  d[variable == "E. Coli", units := "/100ml"]
  d[, waterwork := "Karmøy_Brekke"]
  d[, waterType := "Raw"]
  d[, point := ""]

  saveRDS(d, file.path(RAWmisc::PROJ$CLEAN,"WP1_waterworks/Karmøy_Brekke_ecoli.RDS"))

  ########
  # data_raw/WP1_waterworks/Karmøy_Brekke/Rådata/Råvannskvalitet.xlsx
  #
  # Intestinale
  ########

  d <- data.table(readxl::read_excel(file.path(RAWmisc::PROJ$RAW,"WP1_waterworks/Karmøy_Brekke/Rådata/Råvannskvalitet.xlsx"),
                                                                                sheet = "Intestinale",
                                                                                skip = 1,
                                                                                col_names = FALSE,
                                                                                col_types = c("date", rep("text", 4))))

  setnames(d, c("date",
            "X",
            "X",
            "X",
            "Intestinal Enterococci"))

  d <- d[, which(names(d) != "X"), with = F]

  d <- melt.data.table(d, id = c("date"), variable.factor = FALSE)
  d[, type := "Accredited"]
  unique(d$variable)
  d[, units := ""]
  d[variable == "Intestinal Enterococci", units := "/100ml"]
  d[, waterwork := "Karmøy_Brekke"]
  d[, waterType := "Raw"]
  d[, point := ""]

  saveRDS(d, file.path(RAWmisc::PROJ$CLEAN,"WP1_waterworks/Karmøy_Brekke_intestinal.RDS"))

  ########
  # data_raw/WP1_waterworks/Karmøy_Brekke/Rådata/Råvannskvalitet.xlsx
  #
  # pH
  ########

  d <- data.table(readxl::read_excel(file.path(RAWmisc::PROJ$RAW,"WP1_waterworks/Karmøy_Brekke/Rådata/Råvannskvalitet.xlsx"),
                                                                                  sheet = "pH",
                                                                                  skip = 1,
                                                                                  col_names = FALSE,
                                                                                  col_types = c("date", rep("text", 4))))

  setnames(d, c("date",
              "X",
              "X",
              "X",
              "pH"))

  d <- d[, which(names(d) != "X"), with = F]

  d <- melt.data.table(d, id = c("date"), variable.factor = FALSE)
  d[, type := "Accredited"]
  unique(d$variable)
  d[, units := ""]
  d[variable == "pH", units := ""]
  d[, waterwork := "Karmøy_Brekke"]
  d[, waterType := "Raw"]
  d[, point := ""]

  saveRDS(d, file.path(RAWmisc::PROJ$CLEAN,"WP1_waterworks/Karmøy_Brekke_ph.RDS"))

  ########
  # data_raw/WP1_waterworks/Karmøy_Brekke/Rådata/Råvannskvalitet.xlsx
  #
  # Konduktivitet
  ########

  d <- data.table(readxl::read_excel(file.path(RAWmisc::PROJ$RAW,"WP1_waterworks/Karmøy_Brekke/Rådata/Råvannskvalitet.xlsx"),
                                                                                    sheet = "Konduktivitet",
                                                                                    skip = 1,
                                                                                    col_names = FALSE,
                                                                                    col_types = c("date", rep("text", 4))))

  setnames(d, c("date",
                "X",
                "X",
                "X",
                "Conductivity"))

  d <- d[, which(names(d) != "X"), with = F]

  d <- melt.data.table(d, id = c("date"), variable.factor = FALSE)
  d[, type := "Accredited"]
  unique(d$variable)
  d[, units := ""]
  d[variable == "Conductivity", units := "mS/m"]
  d[, waterwork := "Karmøy_Brekke"]
  d[, waterType := "Raw"]
  d[, point := ""]

  saveRDS(d, file.path(RAWmisc::PROJ$CLEAN,"WP1_waterworks/Karmøy_Brekke_conductivity.RDS"))

  ########
  # data_raw/WP1_waterworks/Karmøy_Brekke/Rådata/Råvannskvalitet.xlsx
  #
  # Turbiditet
  ########

  d <- data.table(readxl::read_excel(file.path(RAWmisc::PROJ$RAW,"WP1_waterworks/Karmøy_Brekke/Rådata/Råvannskvalitet.xlsx"),
                                                                                      sheet = "Turbiditet",
                                                                                      skip = 1,
                                                                                      col_names = FALSE,
                                                                                      col_types = c("date", rep("text", 4))))

  setnames(d, c("date",
                  "X",
                  "X",
                  "X",
                  "Turbidity"))

  d <- d[, which(names(d) != "X"), with = F]

  d <- melt.data.table(d, id = c("date"), variable.factor = FALSE)
  d[, type := "Accredited"]
  unique(d$variable)
  d[, units := ""]
  d[variable == "Turbidity", units := "FNU"]
  d[, waterwork := "Karmøy_Brekke"]
  d[, waterType := "Raw"]
  d[, point := ""]

  saveRDS(d, file.path(RAWmisc::PROJ$CLEAN,"WP1_waterworks/Karmøy_Brekke_turbidity.RDS"))

  ########
  # data_raw/WP1_waterworks/Karmøy_Brekke/Rådata/Råvannskvalitet.xlsx
  #
  # Farge
  ########

  d <- data.table(readxl::read_excel(file.path(RAWmisc::PROJ$RAW,"WP1_waterworks/Karmøy_Brekke/Rådata/Råvannskvalitet.xlsx"),
                                                                                        sheet = "Farge",
                                                                                        skip = 1,
                                                                                        col_names = FALSE,
                                                                                        col_types = c("date", rep("text", 4))))

  setnames(d, c("date",
                    "X",
                    "X",
                    "X",
                    "Colour"))

  d <- d[, which(names(d) != "X"), with = F]

  d <- melt.data.table(d, id = c("date"), variable.factor = FALSE)
  d[, type := "Accredited"]
  unique(d$variable)
  d[, units := ""]
  d[variable == "Colour", units := "Milligram/l Pt"]
  d[, waterwork := "Karmøy_Brekke"]
  d[, waterType := "Raw"]
  d[, point := ""]

  saveRDS(d, file.path(RAWmisc::PROJ$CLEAN,"WP1_waterworks/Karmøy_Brekke_colour.RDS"))

  ##########################################
  ##########################################
  ##########################################
  
  ########
  # data_raw/WP1_waterworks/Kongsvinger_Granli_GIVAS/Rådata/Kongsvinger_råvann brønn 1-3 og 5-6_akkreditert.xlsx
  #
  # 
  ########
  
  d <- data.table(readxl::read_excel(file.path(RAWmisc::PROJ$RAW,"WP1_waterworks/Kongsvinger_Granli_GIVAS/Rådata/Kongsvinger_råvann brønn 1-3 og 5-6_akkreditert.xlsx"),
                                     sheet = 1,
                                     skip = 1,
                                     col_names = FALSE,
                                     col_types = c("text","date",rep("text", 4))))
  
  setnames(d, c("valid","date", "point", "var2", "value", "units"))
  d <- d[valid=="Godkjent"]
  d[,valid:=NULL]
  d <- d[, which(names(d) != "X"), with = F]
  #d[, date := as.Date(date, format = "%d.%m.%Y")]
  
  d[, variable := ""]
  unique(d$var2)
  d[var2 == "06-E.coli (/100 ml)", variable := "E. Coli"]
  d[var2 == "09-Koliforme bakterier 37°C (/100 ml)", variable := "Coliform bacteria"]
  d[var2 == "08-Kimtall 22°C (/ml)", variable := "Colony count at 22 degrees"]
  d[var2 == "01-Farge (mg/l Pt)", variable := "Colour"]
  d[var2 == "35-Konduktivitet (mS/m)", variable := "Conductivity"]
  d[var2 == "44-pH, surhetsgrad ( )", variable := "pH"]
  d[var2 == "07-Intestinale enterokokker (/100 ml)", variable := "Intestinal Enterococci"]
  d[var2 == "04-Turbiditet (FNU)", variable := "Turbidity"]
  
  dim(d)
  d <- d[variable!=""]
  dim(d)
  d[,var2:=NULL]
  
  d[, type := "Accredited"]
  unique(d$variable)
  
  d[, waterwork := "Kongsvinger_Granli_GIVAS"]
  d[, waterType := "Raw"]
  
  saveRDS(d, file.path(RAWmisc::PROJ$CLEAN,"WP1_waterworks/Kongsvinger_Granli_GIVAS.RDS"))
  
  ########
  # data_raw/WP1_waterworks/Kristiansand_Rossevann/Rådata/Klimaprosjekt 2016.xlsx
  #
  # 
  ########
  
  d <- data.table(readxl::read_excel(file.path(RAWmisc::PROJ$RAW,"WP1_waterworks/Kristiansand_Rossevann/Rådata/Klimaprosjekt 2016.xlsx"),
                                     sheet = 1,
                                     skip = 1,
                                     col_names = FALSE,
                                     col_types = c("text","date",rep("text", 4))))
  
  setnames(d, c("valid","date", "point", "var2", "value", "units"))
  d <- d[valid=="Godkjent"]
  d[,valid:=NULL]
  d <- d[, which(names(d) != "X"), with = F]
  #d[, date := as.Date(date, format = "%d.%m.%Y")]
  
  d[, variable := ""]
  unique(d$var2)
  d[var2 == "06-E.coli (/100 ml)", variable := "E. Coli"]
  d[var2 == "09-Koliforme bakterier 37°C (/100 ml)", variable := "Coliform bacteria"]
  d[var2 == "08-Kimtall 22°C (/ml)", variable := "Colony count at 22 degrees"]
  d[var2 == "01-Farge (mg/l Pt)", variable := "Colour"]
  d[var2 == "44-pH, surhetsgrad ( )", variable := "pH"]
  d[var2 == "07-Intestinale enterokokker (/100 ml)", variable := "Intestinal Enterococci"]
  d[var2 == "04-Turbiditet (FNU)", variable := "Turbidity"]
  
  dim(d)
  d <- d[variable!=""]
  dim(d)
  d[,var2:=NULL]
  
  d[, type := "Accredited"]
  unique(d$variable)
  
  d[, waterwork := "Kristiansand_Rossevann"]
  d[, waterType := "Raw"]
  
  saveRDS(d, file.path(RAWmisc::PROJ$CLEAN,"WP1_waterworks/Kristiansand_Rossevann.RDS"))
  
  ########
  # data_raw/WP1_waterworks/\Kristiansand_Tronstadvann/Rådata/Klimaprosjekt 2016.xlsx
  #
  # 
  ########
  
  d <- data.table(readxl::read_excel(file.path(RAWmisc::PROJ$RAW,"WP1_waterworks/Kristiansand_Tronstadvann/Rådata/Klimaprosjekt 2016.xlsx"),
                                     sheet = 2,
                                     skip = 1,
                                     col_names = FALSE,
                                     col_types = c("text","date",rep("text", 4))))
  
  setnames(d, c("valid","date", "point", "var2", "value", "units"))
  d <- d[valid=="Godkjent"]
  d[,valid:=NULL]
  d <- d[, which(names(d) != "X"), with = F]
  #d[, date := as.Date(date, format = "%d.%m.%Y")]
  
  d[, variable := ""]
  unique(d$var2)
  d[var2 == "06-E.coli (/100 ml)", variable := "E. Coli"]
  d[var2 == "09-Koliforme bakterier 37°C (/100 ml)", variable := "Coliform bacteria"]
  d[var2 == "08-Kimtall 22°C (/ml)", variable := "Colony count at 22 degrees"]
  d[var2 == "01-Farge (mg/l Pt)", variable := "Colour"]
  d[var2 == "44-pH, surhetsgrad ( )", variable := "pH"]
  d[var2 == "07-Intestinale enterokokker (/100 ml)", variable := "Intestinal Enterococci"]
  d[var2 == "04-Turbiditet (FNU)", variable := "Turbidity"]
  
  dim(d)
  d <- d[variable!=""]
  dim(d)
  d[,var2:=NULL]
  
  d[, type := "Accredited"]
  unique(d$variable)
  
  d[, waterwork := "Kristiansand_Tronstadvann"]
  d[, waterType := "Raw"]
  
  saveRDS(d, file.path(RAWmisc::PROJ$CLEAN,"WP1_waterworks/Kristiansand_Tronstadvann.RDS"))
  

  ########
  # data_raw/WP1_waterworks/Lillehammer/Rådata/edited_Kopi av export 2011.xlsx
  #
  # Farge
  ########

  d <- data.table(readxl::read_excel(file.path(RAWmisc::PROJ$RAW,"WP1_waterworks/Lillehammer/Rådata/edited_Kopi av export 2011.xlsx"),
                                  sheet = 1,
                                  skip = 3,
                                  col_names = FALSE,
                                  col_types = c("text", "date", rep("text", 14))))

  setnames(d, c("X", "date", "X", "point",
                      "E. Coli",
                      "Colour1",
                      "Colour2",
                      "Intestinal Enterococci",
                      "Colony count",
                      "Coliform bacteria",
                      "Conductivity1",
                      "Conductivity2",
                      "Conductivity3",
                      "pH",
                      "Turbidity1",
                      "Turbidity2"))

  d <- d[, which(names(d) != "X"), with = F]

  d <- melt.data.table(d, id = c("date", "point"), variable.factor = FALSE)
  d[,variable:=gsub("[0-9]","",variable)]
  d[, type := "Accredited"]
  unique(d$variable)
  d[, units := ""]
  d[variable == "E. Coli", units := "kde/100ml"]
  d[variable == "Colour", units := "mg Pt/l"]
  d[variable == "Intestinal Enterococci", units := "kde/100ml"]
  d[variable == "Colony count", units := "kde/ml"]
  d[variable == "Coliform bacteria", units := "kde/100ml"]
  d[variable == "Conductivity", units := "mS/m"]
  d[variable == "pH", units := ""]
  d[variable == "Turbidity", units := "FNU"]
  d[, waterwork := "Lillehammer"]
  d[, waterType := "?"]

  dim(d)
  d <- d[which(d$point=="LILLEHAMMER-KORGEN-RÅVANN")]
  dim(d)
  saveRDS(d, file.path(RAWmisc::PROJ$CLEAN,"WP1_waterworks/Lillehammer_2011.RDS"))

 
  ########
  # data_raw/WP1_waterworks/Lillehammer/Rådata/edited_Kopi av export 2012.xlsx
  #
  # Farge
  ########

  d <- data.table(readxl::read_excel(file.path(RAWmisc::PROJ$RAW,"WP1_waterworks/Lillehammer/Rådata/edited_Kopi av export 2012.xlsx"),
                                    sheet = 1,
                                    skip = 3,
                                    col_names = FALSE,
                                    col_types = c("text", "date", rep("text", 11))))

  setnames(d, c("X", "date", "X", "point",
                        "Clostridium Perfringens",
                        "E. Coli",
                        "Colour",
                        "Intestinal Enterococci",
                        "Colony count",
                        "Coliform bacteria",
                        "Conductivity1",
                        "Conductivity2",
                        "Turbidity"))

  d <- d[, which(names(d) != "X"), with = F]

  d <- melt.data.table(d, id = c("date", "point"), variable.factor = FALSE)
  d[, variable := gsub("[0-9]", "", variable)]
  d[, type := "Accredited"]
  unique(d$variable)
  d[, units := ""]
  d[variable == "Clostridium Perfringens", units := "kde/100ml"]
  d[variable == "E. Coli", units := "kde/100ml"]
  d[variable == "Colour", units := "mg Pt/l"]
  d[variable == "Intestinal Enterococci", units := "kde/100ml"]
  d[variable == "Colony count", units := "kde/ml"]
  d[variable == "Coliform bacteria", units := "kde/100ml"]
  d[variable == "Conductivity", units := "mS/m"]
  d[variable == "Turbidity", units := "FNU"]
  d[, waterwork := "Lillehammer"]
  d[, waterType := "?"]
  
  dim(d)
  d <- d[which(d$point=="LILLEHAMMER-KORGEN-RÅVANN")]
  dim(d)

  saveRDS(d, file.path(RAWmisc::PROJ$CLEAN,"WP1_waterworks/Lillehammer_2012.RDS"))


  ########
  # data_raw/WP1_waterworks/Lillehammer/Rådata/edited_Kopi av export 2013.xlsx
  #
  # Farge
  ########

  d <- data.table(readxl::read_excel(file.path(RAWmisc::PROJ$RAW,"WP1_waterworks/Lillehammer/Rådata/edited_Kopi av export 2013.xlsx"),
                                        sheet = 1,
                                        skip = 3,
                                        col_names = FALSE,
                                        col_types = c("text", "date", rep("text", 14))))

  setnames(d, c("X", "date", "X", "point",
                            "Clostridium Perfringens",
                            "E. Coli",
                            "Colour",
                            "Intestinal Enterococci",
                            "Colony count1",
                            "Colony count2",
                            "Coliform bacteria",
                            "Conductivity1",
                            "Conductivity2",
                            "pH at 19-25 degrees1",
                            "pH at 19-25 degrees2",
                            "Turbidity"))

  d <- d[, which(names(d) != "X"), with = F]

  d <- melt.data.table(d, id = c("date", "point"), variable.factor = FALSE)
  d[, variable := gsub("[0-9]*$", "", variable)]
  d[, type := "Accredited"]
  unique(d$variable)
  d[, units := ""]
  d[variable == "Clostridium Perfringens", units := "kde/100ml"]
  d[variable == "E. Coli", units := "kde/100ml"]
  d[variable == "Colour", units := "mg Pt/l"]
  d[variable == "Intestinal Enterococci", units := "kde/100ml"]
  d[variable == "Colony count", units := "kde/ml"]
  d[variable == "Coliform bacteria", units := "kde/100ml"]
  d[variable == "Conductivity", units := "mS/m"]
  d[variable == "Turbidity", units := "FNU"]
  d[, waterwork := "Lillehammer"]
  d[, waterType := "?"]
  
  dim(d)
  d <- d[which(d$point=="LILLEHAMMER-KORGEN-RÅVANN")]
  dim(d)

  saveRDS(d, file.path(RAWmisc::PROJ$CLEAN,"WP1_waterworks/Lillehammer_2013.RDS"))

  ########
  # data_raw/WP1_waterworks/Lillehammer/Rådata/edited_Kopi av export 2014.xlsx
  #
  # Farge
  ########

  d <- data.table(readxl::read_excel(file.path(RAWmisc::PROJ$RAW,"WP1_waterworks/Lillehammer/Rådata/edited_Kopi av export 2014.xlsx"),
                                          sheet = 1,
                                          skip = 3,
                                          col_names = FALSE,
                                          col_types = c("text", "date", rep("text", 14))))

  setnames(d, c("X", "date", "X", "point",
                              "Clostridium Perfringens",
                              "E. Coli",
                              "Colour",
                              "Intestinal Enterococci",
                              "Colony count1",
                              "Colony count2",
                              "Coliform bacteria",
                              "Conductivity1",
                              "Conductivity2",
                              "pH at 19-25 degrees1",
                              "pH at 19-25 degrees2",
                              "Turbidity"))

  d <- d[, which(names(d) != "X"), with = F]

  d <- melt.data.table(d, id = c("date", "point"), variable.factor = FALSE)
  d[, variable := gsub("[0-9]*$", "", variable)]
  d[, type := "Accredited"]
  unique(d$variable)
  d[, units := ""]
  d[variable == "Clostridium Perfringens", units := "kde/100ml"]
  d[variable == "E. Coli", units := "kde/100ml"]
  d[variable == "Colour", units := "mg Pt/l"]
  d[variable == "Intestinal Enterococci", units := "kde/100ml"]
  d[variable == "Colony count", units := "kde/ml"]
  d[variable == "Coliform bacteria", units := "kde/100ml"]
  d[variable == "Conductivity", units := "mS/m"]
  d[variable == "Turbidity", units := "FNU"]
  d[, waterwork := "Lillehammer"]
  d[, waterType := "?"]
  
  dim(d)
  d <- d[which(d$point=="LILLEHAMMER-KORGEN-RÅVANN")]
  dim(d)

  saveRDS(d, file.path(RAWmisc::PROJ$CLEAN,"WP1_waterworks/Lillehammer_2014.RDS"))

  ########
  # data_raw/WP1_waterworks/Lillehammer/Rådata/edited_Kopi av export 2015.xlsx
  #
  # Farge
  ########

  d <- data.table(readxl::read_excel(file.path(RAWmisc::PROJ$RAW,"WP1_waterworks/Lillehammer/Rådata/edited_Kopi av export 2015.xlsx"),
                                            sheet = 1,
                                            skip = 3,
                                            col_names = FALSE,
                                            col_types = c("text", "date", rep("text", 16))))

  setnames(d, c("X", "date", "X", "point",
                                "Clostridium Perfringens",
                                "E. Coli",
                                "Colour",
                                "Intestinal Enterococci",
                                "Colony count1",
                                "Colony count2",
                                "Coliform bacteria",
                                "Conductivity1",
                                "Conductivity2",
                                "Conductivity3",
                                "pH at 19-25 degrees1",
                                "pH at 19-25 degrees2",
                                "pH at 19-25 degrees3",
                                "Turbidity"))

  d <- d[, which(names(d) != "X"), with = F]

  d <- melt.data.table(d, id = c("date", "point"), variable.factor = FALSE)
  d[, variable := gsub("[0-9]*$", "", variable)]
  d[, type := "Accredited"]
  unique(d$variable)
  d[, units := ""]
  d[variable == "Clostridium Perfringens", units := "kde/100ml"]
  d[variable == "E. Coli", units := "kde/100ml"]
  d[variable == "Colour", units := "mg Pt/l"]
  d[variable == "Intestinal Enterococci", units := "kde/100ml"]
  d[variable == "Colony count", units := "kde/ml"]
  d[variable == "Coliform bacteria", units := "kde/100ml"]
  d[variable == "Conductivity", units := "mS/m"]
  d[variable == "Turbidity", units := "FNU"]
  d[, waterwork := "Lillehammer"]
  d[, waterType := "?"]
  
  dim(d)
  d <- d[which(d$point=="LILLEHAMMER-KORGEN-RÅVANN")]
  dim(d)

  saveRDS(d, file.path(RAWmisc::PROJ$CLEAN,"WP1_waterworks/Lillehammer_2015.RDS"))


  ########
  # data_raw/WP1_waterworks/Lillehammer/Rådata/Råvann Korgen 2006 - 2010.xlsx
  #
  # Farge
  ########

  d <- data.table(readxl::read_excel(file.path(RAWmisc::PROJ$RAW,"WP1_waterworks/Lillehammer/Rådata/Råvann Korgen 2006 - 2010.xlsx"),
                                              sheet = "aggregated",
                                              skip = 1,
                                              col_names = FALSE,
                                              col_types = c(rep("text", 5))))

  setnames(d, c("date", "X", "units", "var2", "value"))

  d <- d[, which(names(d) != "X"), with = F]
  d[, date := as.Date(date, format = "%d.%m.%Y")]

  d[, variable := ""]
  unique(d$var2)
  d[var2 == "E. Coli - Colilert", variable := "E. Coli"]
  d[var2 == "Koliforme bakterier - Colilert", variable := "Coliform bacteria"]
  d[var2 == "Totalant.bakterier 22°C", variable := "Colony count at 22 degrees"]
  d[var2 == "Totalant.bakterier 37°C", variable := "Colony count at 37 degrees"]
  d[var2 == "Fargetall (etter filtrering)", variable := "Colour (after filtering)"]
  d[var2 == "Konduktivitet  25°C", variable := "Conductivity at 25 degrees"]
  d[var2 == "Surhetsgrad (pH)", variable := "pH"]
  d[var2 == "Intestinale enterokokker", variable := "Intestinal Enterococci"]
  d[var2 == "Totalant. bakt., 22°C", variable := "Colony count at 22 degrees"]

  dim(d)
  d <- d[variable!=""]
  dim(d)
  d[,var2:=NULL]

  d[, type := "Accredited"]
  unique(d$variable)
 
  d[, waterwork := "Lillehammer"]
  d[, waterType := "Raw"]
  d[, point := "LILLEHAMMER-KORGEN-RÅVANN"]

  saveRDS(d, file.path(RAWmisc::PROJ$CLEAN,"WP1_waterworks/Lillehammer.RDS"))

  ##########################################
  ##########################################
  ##########################################

  ########
  # data_raw/WP1_waterworks/MOVAR_Vansjø/Rådata/Online målinger av råvann - 2011 tom 2015.xlsx
  #
  # online
  ########

  d <- data.table(readxl::read_excel(file.path(RAWmisc::PROJ$RAW,"WP1_waterworks/MOVAR_Vansjø/Rådata/Online målinger av råvann - 2011 tom 2015.xlsx"),
                                                sheet = 1,
                                                skip = 2,
                                                col_names = FALSE,
                                                col_types = c("date", rep("text", 3))))

  setnames(d, c("date",
    "pH",
    "Colour",
    "Turbidity"))

  d <- d[, which(names(d) != "X"), with = F]

  d <- melt.data.table(d, id = c("date"), variable.factor = FALSE)
  d[, variable := gsub("[0-9]*$", "", variable)]
  d[, type := "Online"]
  unique(d$variable)

  d[, units := ""]

  d[, waterwork := "MOVAR_Vansjø"]
  d[, waterType := "Raw"]
  d[, point := ""]

  saveRDS(d, file.path(RAWmisc::PROJ$CLEAN,"WP1_waterworks/MOVAR_Vansjø_online.RDS"))

  ########
  # data_raw/WP1_waterworks/MOVAR_Vansjø/Rådata/external_2006_2012.xlsx
  #
  #
  ########

  d <- data.table(readxl::read_excel(file.path(RAWmisc::PROJ$RAW,"WP1_waterworks/MOVAR_Vansjø/Rådata/external_2006_2012.xlsx"),
                                                  sheet = 1,
                                                  skip = 2,
                                                  col_names = FALSE,
                                                  col_types = c("date", rep("text", 10))))

  setnames(d, c("date",
      "X",
      "Coliform bacteria",
      "pH",
      "Conductivity",
      "Turbidity",
      "X",
      "Colony count",
      "E. Coli",
      "Intestinal Enterococci",
      "Colour"
      ))

  d <- d[, which(names(d) != "X"), with = F]

  d <- melt.data.table(d, id = c("date"), variable.factor = FALSE)
  d[, variable := gsub("[0-9]*$", "", variable)]
  d[, type := "External"]
  unique(d$variable)

  d[, units := ""]
  d[variable == "Coliform bacteria", units := "/100ml"]
  d[variable == "Conductivity", units := "mS/m, 25C"]
  d[variable == "Turbidity", units := "FNU"]
  d[variable == "Colony count", units := "/ml"]
  d[variable == "E. Coli", units := "/100ml"]
  d[variable == "Intestinal Enterococci", units := "/100ml"]
  d[variable == "Colour", units := "mg Pt/l"]

  d[, waterwork := "MOVAR_Vansjø"]
  d[, waterType := "Raw"]
  d[, point := ""]

  saveRDS(d, file.path(RAWmisc::PROJ$CLEAN,"WP1_waterworks/MOVAR_Vansjø_external_2006_2012.RDS"))

  ########
  # data_raw/WP1_waterworks/MOVAR_Vansjø/Rådata/Eksterne råvannsanalyser - uke og måneds kontroll 2013 tom 2015.xlsx
  #
  #
  ########

  d <- data.table(readxl::read_excel(file.path(RAWmisc::PROJ$RAW,"WP1_waterworks/MOVAR_Vansjø/Rådata/Eksterne råvannsanalyser - uke og måneds kontroll 2013 tom 2015.xlsx"),
                                                    sheet = 1,
                                                    skip = 2,
                                                    col_names = FALSE,
                                                    col_types = c("date", rep("text", 25))))

  setnames(d, c("date",
        "Coliform bacteria",
        "E. Coli",
        "Colony count",
        "Intestinal Enterococci",
        "pH",
        "Conductivity",
        "Turbidity",
        "Colour",
        rep("X",17)))

  d <- d[, which(names(d) != "X"), with = F]

  d <- melt.data.table(d, id = c("date"), variable.factor = FALSE)
  d[, variable := gsub("[0-9]*$", "", variable)]
  d[, type := "External"]
  unique(d$variable)

  d[, units := ""]

  d[, waterwork := "MOVAR_Vansjø"]
  d[, waterType := "Raw"]
  d[, point := ""]

  saveRDS(d, file.path(RAWmisc::PROJ$CLEAN,"WP1_waterworks/MOVAR_Vansjø_external_2013_2015.RDS"))

  
  CleanWP1SpecificWaterWork(fileIn=file.path(RAWmisc::PROJ$RAW,"WP1_waterworks/MOVAR_Vansjø/Rådata/aggregated_internal.xlsx"),
                            fileOut="movar_internal.RDS",
                            sheet=1,
                            skip=8,
                            col_types=c("date",rep("text", 5)),
                            col_names=c("date",
                                        "X",
                                        "X",
                                        "pH",
                                        "Conductivity",
                                        "Colour"),
                            type="Internal",
                            waterwork="MOVAR_Vansjøo",
                            waterType="Raw",
                            point="?"
  )
  
  ##########################################
  ##########################################
  ##########################################

  ########
  # data_raw/WP1_waterworks/Nedre Romerike Vannverk IKS_Hauglifjell/Rådata/Råvann analyser til Nominor.xlsx
  #
  #
  ########

  d <- data.table(readxl::read_excel(file.path(RAWmisc::PROJ$RAW,"WP1_waterworks/Nedre Romerike Vannverk IKS_Hauglifjell/Rådata/Råvann analyser til Nominor.xlsx"),
    sheet = "Samlet data",
    skip = 6,
    col_names = FALSE,
    col_types = c("text", "date", rep("text", 67))))

  setnames(d, c(
          "X",
          "date",
          "X",
          "pH",
          "X",
          "Turbidity",
          "Conductivity",
          "Colour",
          rep("X", 47),
          "Coliform bacteria1",
          "X",
          "X",
          "X", "X",
          "Colony count at 22 degrees",
          "X",
          "X",
          "E. Coli1",
          "Clostridium Perfringens",
          "X",
          "Intestinal Enterococci2",
          "E. Coli2",
          "Coliform bacteria2"))

  d <- d[, which(names(d) != "X"), with = F]

  d <- melt.data.table(d, id = c("date"), variable.factor = FALSE)
  d[, variable := gsub("[0-9]*$", "", variable)]
  d[, type := "Accredited"]
  unique(d$variable)

  d[, units := ""]
  d[variable == "Turbidity", units := "NTU"]
  d[variable == "Conductivity", units := "mS/m"]
  d[variable == "Colour", units := "mg Pt/l"]
  d[variable == "Coliform bacteria", units := "/100ml"]
  d[variable == "Colony count at 22 degrees", units := "/ml"]
  d[variable == "Colony count at 36 degrees", units := "/ml"]
  d[variable == "E. Coli", units := "/100ml"]
  d[variable == "Closteridium Perfringens", units := "/100ml"]
  d[variable == "Intestinal Enterococci", units := "/100ml"]

  d[, waterwork := "Nedre Romerike Vannverk IKS_Hauglifjell"]
  d[, waterType := "Raw"]
  d[, point := ""]

  saveRDS(d, file.path(RAWmisc::PROJ$CLEAN,"WP1_waterworks/Nedre Romerike Vannverk IKS_Hauglifjell_Samlet data.RDS"))

  ########
  # data_raw/WP1_waterworks/Nedre Romerike Vannverk IKS_Hauglifjell/Rådata/Analysedata råvann 2006-2016.xlsx
  #
  #
  ########

  d <- data.table(readxl::read_excel(file.path(RAWmisc::PROJ$RAW,"WP1_waterworks/Nedre Romerike Vannverk IKS_Hauglifjell/Rådata/Analysedata råvann 2006-2016.xlsx"),
      skip = 12,
      col_names = FALSE))

  setnames(d, c(
            "date",
            "pH",
            "X",
            "Conductivity",
            "X",
            "Turbidity",
            "X",
            "Temperature",
            "X","X"))

  d <- d[, which(names(d) != "X"), with = F]

  d <- melt.data.table(d, id = c("date"), variable.factor = FALSE)
  d[, variable := gsub("[0-9]*$", "", variable)]
  d[, type := "Online"]
  unique(d$variable)

  d[, units := ""]
  d[variable == "Conductivity", units := "mS/m"]
  d[variable == "Turbidity", units := "NTU"]
  d[variable == "Temperature", units := "C"]

  d[, waterwork := "Nedre Romerike Vannverk IKS_Hauglifjell"]
  d[, waterType := "Raw"]
  d[, point := "Silhus"]
  
  d <- d[!(date>="2015-05-16" & date<="2015-06-29")]
  d <- d[!is.na(date)]

  #warning("Det var problemer i perioden 16.5.15  - 29.6.15 så her har ikke dataene særlig stor verdi")
  saveRDS(d, file.path(RAWmisc::PROJ$CLEAN,"WP1_waterworks/Nedre Romerike Vannverk IKS_Hauglifjell_silhus.RDS"))

  ########
  # data_raw/WP1_waterworks/Nedre Romerike Vannverk IKS_Hauglifjell/Rådata/Analysedata råvann 2006-2016.xlsx
  #
  #
  ########

  d <- data.table(readxl::read_excel(file.path(RAWmisc::PROJ$RAW,"WP1_waterworks/Nedre Romerike Vannverk IKS_Hauglifjell/Rådata/Analysedata råvann 2006-2016.xlsx"),
        skip = 12,
        col_names = FALSE))

  setnames(d, c(
              "date",
              "X",
              "pH",
              "X",
              "Conductivity",
              "X",
              "Turbidity",
              "X",
              "Temperature",
              "Colour"))

  d <- d[, which(names(d) != "X"), with = F]

  d <- melt.data.table(d, id = c("date"), variable.factor = FALSE)
  d[, variable := gsub("[0-9]*$", "", variable)]
  d[, type := "Online"]
  unique(d$variable)

  d[, units := ""]
  d[variable == "Conductivity", units := "mS/m"]
  d[variable == "Turbidity", units := "NTU"]
  d[variable == "Temperature", units := "C"]

  d[, waterwork := "Nedre Romerike Vannverk IKS_Hauglifjell"]
  d[, waterType := "Raw"]
  d[, point := "Innløp R1"]

  # DONT USE THIS FILE, IT GOES THROUGH A TUNNEL
  #saveRDS(d, "data_clean/WP1_waterworks/Nedre Romerike Vannverk IKS_Hauglifjell_inløp.RDS")

  ##########################################
  ##########################################
  ##########################################
  
  ## OPPEGARD
  CleanWP1SpecificWaterWorkLongOppegard()
  
  ########
  # data_raw/WP1_waterworks/Oslo_Oset/Rådata/online.xlsx
  #
  #
  ########
  
  d <- data.table(readxl::read_excel(file.path(RAWmisc::PROJ$RAW,"WP1_waterworks/Oslo_Oset/Rådata/online.xlsx"),
                                     sheet = 1,
                                     skip = 2,
                                     col_names = FALSE,
                                     col_types = c("date", rep("text", 3))))
  
  setnames(d, c("date",
                "Turbidity",
                "Conductivity",
                "Temperature"))
  
  d <- d[, which(names(d) != "X"), with = F]
  
  d <- melt.data.table(d, id = c("date"), variable.factor = FALSE)
  d[, variable := gsub("[0-9]*$", "", variable)]
  d[, type := "Online"]
  unique(d$variable)
  
  d[, units := "?"]
  d[variable == "Temperature", units := "C"]
  d[variable == "Turbidity", units := "NTU"]
  
  d[, waterwork := "Oslo_Oset"]
  d[, waterType := "Raw"]
  d[, point:="?"]
  
  saveRDS(d, file.path(RAWmisc::PROJ$CLEAN,"WP1_waterworks/Oslo_Oset_online.RDS"))

  ########
  # data_raw/WP1_waterworks/Oslo_Oset/Rådata/Kjemi raavann.xlsx
  #
  #
  ########

  d <- data.table(readxl::read_excel(file.path(RAWmisc::PROJ$RAW,"WP1_waterworks/Oslo_Oset/Rådata/Kjemi raavann.xlsx"),
          sheet = "samlet",
          skip = 3,
          col_names = FALSE,
          col_types = c("date", rep("text", 5))))

  setnames(d, c("date",
                "pH",
                "Conductivity",
                "Colour",
                "Turbidity",
                "point"))

  d <- d[, which(names(d) != "X"), with = F]

  d <- melt.data.table(d, id = c("date","point"), variable.factor = FALSE)
  d[, variable := gsub("[0-9]*$", "", variable)]
  d[, type := "Accredited"]
  unique(d$variable)

  d[, units := ""]
  d[variable == "Conductivity", units := "mS/m"]
  d[variable == "Colour", units := "mg Pt/l"]
  d[variable == "Turbidity", units := "FTU"]

  d[, waterwork := "Oslo_Oset"]
  d[, waterType := "Raw"]

  saveRDS(d, file.path(RAWmisc::PROJ$CLEAN,"WP1_waterworks/Oslo_Oset_kjemi.RDS"))

  ########
  # data_raw/WP1_waterworks/Oslo_Oset/Rådata/Kjemi raavann.xlsx
  #
  #
  ########

  d <- data.table(readxl::read_excel(file.path(RAWmisc::PROJ$RAW,"WP1_waterworks/Oslo_Oset/Rådata/Bakterier raavann.xlsx"),
            sheet = "samlet_day",
            skip = 4,
            col_names = FALSE,
            col_types = c("date", rep("text", 7))))

  setnames(d, c("date",
    "Temperature",
    "Colony count at 22 degrees",
    "Coliform bacteria",
    "E. Coli",
    "Intestinal Enterococci",
    "Clostridium Perfringens",
    "point"))

  d <- d[, which(names(d) != "X"), with = F]

  d <- melt.data.table(d, id = c("date", "point"), variable.factor = FALSE)
  d[, variable := gsub("[0-9]*$", "", variable)]
  d[, type := "Accredited"]
  unique(d$variable)

  d[, units := ""]
  d[variable == "Temperature", units := "C"]
  d[variable == "Colony count at 22 degrees", units := "CFU/ml"]
  d[variable == "Coliform bacteria", units := "CFU/100ml"]
  d[variable == "E. Coli", units := "CFU/100ml"]
  d[variable == "Intestinal Enterococci", units := "CFU/100ml"]
  d[variable == "Clostridium Perfringens", units := "CFU/100ml"]

  d[, waterwork := "Oslo_Oset"]
  d[, waterType := "Raw"]

  saveRDS(d, file.path(RAWmisc::PROJ$CLEAN,"WP1_waterworks/Oslo_Oset_bakterier_date.RDS"))

  ########
  # data_raw/WP1_waterworks/Oslo_Oset/Rådata/Kjemi raavann.xlsx
  #
  #
  ########

  d <- data.table(readxl::read_excel(file.path(RAWmisc::PROJ$RAW,"WP1_waterworks/Oslo_Oset/Rådata/Bakterier raavann.xlsx"),
              sheet = "samlet_week",
              skip = 2,
              col_names = FALSE,
              col_types = c(rep("text", 7))))

  setnames(d, c("year", "week",
      "Coliform bacteria",
      "E. Coli",
      "Intestinal Enterococci",
      "Clostridium Perfringens",
      "point"))

  d <- d[, which(names(d) != "X"), with = F]
  d[, year := as.numeric(year)]
  d[, week := as.numeric(week)]

  a <- data.table(seq.Date(as.Date("2005-01-01"), as.Date("2015-01-01"), by = 1))
  setnames(a, "date")
  a[, year := as.numeric(strftime(date, "%G"))]
  a[, week := as.numeric(strftime(date, "%V"))]
  a <- a[,.(date=min(date)),by=.(year,week)]
  # TAKEN ON MONDAYS
#error("find out what permanent day we take samples")
  dim(d)
  d <- merge(d,a,by=c("year","week"))
  dim(d)

  d[, year := NULL]
  d[, week := NULL]

  d <- melt.data.table(d, id = c("date", "point"), variable.factor = FALSE)
  d[, variable := gsub("[0-9]*$", "", variable)]
  d[, type := "Accredited"]
  unique(d$variable)

  d[, units := ""]

  d[, waterwork := "Oslo_Oset"]
  d[, waterType := "Raw"]

  saveRDS(d, file.path(RAWmisc::PROJ$CLEAN,"WP1_waterworks/Oslo_Oset_bakterier_week.RDS"))

  
  ###
  ## PROSGRUND
  
  CleanWP1SpecificWaterWork(fileIn=file.path(RAWmisc::PROJ$RAW,"WP1_waterworks/Porsgrunn_Valleråsen/Rådata/Akkrediterte data_2006-2016_LSS.xlsx"),
                            fileOut="Porsgrunn_Valleråsen.RDS",
                            sheet=1,
                            skip=2,
                            col_types=c("date",rep("text", 7)),
                            col_names=c("date",
                                        "Colony count",
                                        "Coliform bacteria",
                                        "E. Coli",
                                        "Intestinal Enterococci",
                                        "pH",
                                        "Turbidity",
                                        "Colour"),
                            type="Accredited",
                            waterwork="Porsgrunn_Valleråsen",
                            waterType="Raw",
                            point="?"
  )
  
  ###################
  
  # sarpsborg
  CleanWP1SpecificWaterWork(fileIn=file.path(RAWmisc::PROJ$RAW,"WP1_waterworks/Sarpsborg_Baterød/Rådata/Råvanns analyser Glomma.xlsx"),
                            fileOut="sarpsborg.RDS",
                            sheet=1,
                            skip=1,
                            col_types=c("date",rep("text", 15)),
                            col_names=c("date",
                                        "Coliform bacteria",
                                        "E. Coli",
                                        "Intestinal Enterococci",
                                        "Colony count",
                                        "pH",
                                        "Temperature",
                                        "Colour",
                                        "Turbidity",
                                        "X",
                                        "X",
                                        "X",
                                        "Clostridium Perfringens",
                                        "X",
                                        "X",
                                        "X"
                                        ),
                            type="Accredited",
                            units=NULL,
                            waterwork="Sarpsborg_Baterod",
                            waterType="Raw",
                            point="Glomma"
  )

  ########
  # data_raw/WP1_waterworks/Tromsø_Kvaløya/Rådata/Kopi av Byvannverk råvann analyseresultater 2006- (2)
  #
  # 
  ########
  
  d <- data.table(readxl::read_excel(file.path(RAWmisc::PROJ$RAW,"WP1_waterworks/Tromsø_Kvaløya/Rådata/Kopi av Byvannverk råvann analyseresultater 2006- (2).xlsx"),
                                     sheet = "Kvaløya vv",
                                     skip = 3,
                                     col_names = FALSE,
                                     col_types = c("date",rep("text", 4))))
  
  setnames(d, c("date", "point", "var2", "value", "units"))
  d <- d[!is.na(var2)]
  d <- d[, which(names(d) != "X"), with = F]
  #d[, date := as.Date(date, format = "%d.%m.%Y")]
  
  d[, variable := ""]
  unique(d$var2)
  d[var2 == "01-Farge (mg/l Pt)", variable := "Colour"]
  d[var2 == "04-Turbiditet (FNU)", variable := "Turbidity"]
  d[var2 == "06-E.coli (/100 ml)", variable := "E. Coli"]
  d[var2 == "07-Intestinale enterokokker (/100 ml)", variable := "Intestinal Enterococci"]
  d[var2 == "08-Kimtall 22°C (/ml)", variable := "Colony count at 22 degrees"]
  d[var2 == "09-Koliforme bakterier 37°C (/100 ml)", variable := "Coliform bacteria"]
  d[var2 == "35-Konduktivitet (mS/m)", variable := "Conductivity"]
  d[var2 == "44-pH, surhetsgrad ( )", variable := "pH"]
  
  dim(d)
  d <- d[variable!=""]
  dim(d)
  d[,var2:=NULL]
  
  d[, type := "Accredited"]
  unique(d$variable)
  
  d[, waterwork := "Tromsø_Kvaløya"]
  d[, waterType := "Raw"]
  
  saveRDS(d, file.path(RAWmisc::PROJ$CLEAN,"WP1_waterworks/Tromsø_Kvaløya.RDS"))
  
  ########
  # data_raw/WP1_waterworks/Tromsø_Simavik/Rådata/Kopi av Byvannverk råvann analyseresultater 2006- (2)
  #
  # 
  ########
  
  d <- data.table(readxl::read_excel(file.path(RAWmisc::PROJ$RAW,"WP1_waterworks/Tromsø_Simavik/Rådata/Kopi av Byvannverk råvann analyseresultater 2006- (2).xlsx"),
                                     sheet = "Simavik vv Øvre Langvann",
                                     skip = 3,
                                     col_names = FALSE,
                                     col_types = c("date",rep("text", 4))))
  
  setnames(d, c("date", "point", "var2", "value", "units"))
  d <- d[!is.na(var2)]
  d <- d[, which(names(d) != "X"), with = F]
  #d[, date := as.Date(date, format = "%d.%m.%Y")]
  
  d[, variable := ""]
  unique(d$var2)
  d[var2 == "01-Farge (mg/l Pt)", variable := "Colour"]
  d[var2 == "04-Turbiditet (FNU)", variable := "Turbidity"]
  d[var2 == "06-E.coli (/100 ml)", variable := "E. Coli"]
  d[var2 == "07-Intestinale enterokokker (/100 ml)", variable := "Intestinal Enterococci"]
  d[var2 == "08-Kimtall 22°C (/ml)", variable := "Colony count at 22 degrees"]
  d[var2 == "09-Koliforme bakterier 37°C (/100 ml)", variable := "Coliform bacteria"]
  d[var2 == "35-Konduktivitet (mS/m)", variable := "Conductivity"]
  d[var2 == "44-pH, surhetsgrad ( )", variable := "pH"]
  
  dim(d)
  d <- d[variable!=""]
  dim(d)
  d[,var2:=NULL]
  
  d[, type := "Accredited"]
  unique(d$variable)
  
  d[, waterwork := "Tromsø_Simavik"]
  d[, waterType := "Raw"]
  
  saveRDS(d, file.path(RAWmisc::PROJ$CLEAN,"WP1_waterworks/Tromsø_Simavik.RDS"))
  
  ########
  # data_raw/WP1_waterworks/Trondheim_Vikelvdalen/Rådata/akkreditert bakterier 2006 råvann.xlsx
  #
  #
  ########
  
  d <- data.table(readxl::read_excel(file.path(RAWmisc::PROJ$RAW,"WP1_waterworks/Trondheim_Vikelvdalen/Rådata/akkreditert bakterier 2006 råvann.xlsx"),
                                     sheet = "Råvann",
                                     skip = 2,
                                     col_names = FALSE,
                                     col_types = c("text","date",rep("text", 16))))
  
  setnames(d, c("X", "date",
                "Colony count",
                "Coliform bacteria",
                "E. Coli",
                "X",
                "Intestinal Enterococci",
                "Clostridium Perfringens",
                "X","X","X","X","X","X","X","X","X","X"))
  
  d <- d[, which(names(d) != "X"), with = F]
  
  d <- melt.data.table(d, id = c("date"), variable.factor = FALSE)
  d[, variable := gsub("[0-9]*$", "", variable)]
  d[, type := "Accredited"]
  unique(d$variable)
  
  d[, units := ""]
  
  d[, waterwork := "Trondheim_Vikelvdalen"]
  d[, waterType := "Raw"]
  d[, point:="?"]
  
  saveRDS(d, file.path(RAWmisc::PROJ$CLEAN,"WP1_waterworks/Trondheim_Vikelvdalen_bakterier_2006.RDS"))
  
  ########
  # data_raw/WP1_waterworks/Trondheim_Vikelvdalen/Rådata/akkreditert bakterier 2007 råvann.xlsx
  #
  #
  ########
  
  d <- data.table(readxl::read_excel(file.path(RAWmisc::PROJ$RAW,"WP1_waterworks/Trondheim_Vikelvdalen/Rådata/Kopi av Akkreditertbakterier 2007 råvann_edited.xlsx"),
                                     sheet = "Råvann",
                                     skip = 2,
                                     col_names = FALSE,
                                     col_types = c("date",rep("text", 5))))
  
  setnames(d, c( "date",
                "Colony count",
                "Coliform bacteria",
                "E. Coli",
                "Intestinal Enterococci",
                "Clostridium Perfringens"))
  
  d <- d[, which(names(d) != "X"), with = F]
  
  d <- melt.data.table(d, id = c("date"), variable.factor = FALSE)
  d[, variable := gsub("[0-9]*$", "", variable)]
  d[, type := "Accredited"]
  unique(d$variable)
  
  d[, units := ""]
  
  d[, waterwork := "Trondheim_Vikelvdalen"]
  d[, waterType := "Raw"]
  d[, point:="?"]
  
  saveRDS(d, file.path(RAWmisc::PROJ$CLEAN,"WP1_waterworks/Trondheim_Vikelvdalen_bakterier_2007.RDS"))
  
  ########
  # data_raw/WP1_waterworks/Trondheim_Vikelvdalen/Rådata/AKKREDITERTKJEMISK 2006 råvann.xlsx
  #
  #
  ########
  
  d <- data.table(readxl::read_excel(file.path(RAWmisc::PROJ$RAW,"WP1_waterworks/Trondheim_Vikelvdalen/Rådata/AKKREDITERTKJEMISK 2006 råvann.xlsx"),
                                     sheet = "Råvann",
                                     skip = 4,
                                     col_names = FALSE,
                                     col_types = c("text","date",rep("text", 10))))
  
  setnames(d, c("X", "date",
                "pH",
                "Colour",
                "X",
                "Turbidity",
                "TOC",
                "X","X","X","X","X"))
  
  d <- d[, which(names(d) != "X"), with = F]
  
  d <- melt.data.table(d, id = c("date"), variable.factor = FALSE)
  d[, variable := gsub("[0-9]*$", "", variable)]
  d[, type := "Accredited"]
  unique(d$variable)
  
  d[, units := ""]
  d[variable=="pH", units:="pH"]
  d[variable=="Colour", units:="mg/LPt"]
  d[variable=="Turbidity", units:="FTU"]
  d[variable=="TOC", units:="ml/L"]
  
  d[, waterwork := "Trondheim_Vikelvdalen"]
  d[, waterType := "Raw"]
  d[, point:="?"]
  
  saveRDS(d, file.path(RAWmisc::PROJ$CLEAN,"WP1_waterworks/Trondheim_Vikelvdalen_chemical_2006.RDS"))
  
  
  ########
  # data_raw/WP1_waterworks/Trondheim_Vikelvdalen/Rådata/AkkreditertKJEMISK 2007råvann
  #
  #
  ########
  
  d <- data.table(readxl::read_excel(file.path(RAWmisc::PROJ$RAW,"WP1_waterworks/Trondheim_Vikelvdalen/Rådata/AkkreditertKJEMISK 2007råvann.xlsx"),
                                     sheet = "Råvann",
                                     skip = 4,
                                     col_names = FALSE,
                                     col_types = c("text","date",rep("text", 10))))
  
  setnames(d, c("X", "date",
                "pH",
                "Colour",
                "X",
                "Turbidity",
                "TOC",
                "X","X","X","X","X"))
  
  d <- d[, which(names(d) != "X"), with = F]
  
  d <- melt.data.table(d, id = c("date"), variable.factor = FALSE)
  d[, variable := gsub("[0-9]*$", "", variable)]
  d[, type := "Accredited"]
  unique(d$variable)
  
  d[, units := ""]
  d[variable=="pH", units:="pH"]
  d[variable=="Colour", units:="mg/LPt"]
  d[variable=="Turbidity", units:="FTU"]
  d[variable=="TOC", units:="ml/L"]
  
  d[, waterwork := "Trondheim_Vikelvdalen"]
  d[, waterType := "Raw"]
  d[, point:="?"]
  
  saveRDS(d, file.path(RAWmisc::PROJ$CLEAN,"WP1_waterworks/Trondheim_Vikelvdalen_chemical_2007.RDS"))
  
  
  
  ########
  # data_raw/WP1_waterworks/Trondheim_Vikelvdalen/Rådata/AkkreditertKJEMISK 2007råvann
  #
  #
  ########
  
  d <- data.table(readxl::read_excel(file.path(RAWmisc::PROJ$RAW,"WP1_waterworks/Trondheim_Vikelvdalen/Rådata/akkreditert lab råvann2008-20215.xlsx"),
                                     sheet = "Sheet0",
                                     skip = 3,
                                     col_names = FALSE,
                                     col_types = c("text","date",rep("text", 35))))
  
  setnames(d, c("X", "date",
                rep("X",8),
                "X","X",
                "TOC",
                "X","X",
                "Clostridium Perfringens",
                "X","X",
                "E. Coli",
                "Colour",
                "X","X","X",
                "Intestinal Enterococci",
                "Coliform Bacteria",
                "Conductivity",
                "Colony count",
                rep("X",6),
                "Turbidity",
                "X","X","X"))
  
  d <- d[, which(names(d) != "X"), with = F]
  
  d <- melt.data.table(d, id = c("date"), variable.factor = FALSE)
  d[, variable := gsub("[0-9]*$", "", variable)]
  d[, type := "Accredited"]
  unique(d$variable)
  
  d[, units := ""]
  
  d[variable=="TOC", units:="mg C/L"]
  d[variable=="Clostridium Perfringens", units:="/100mL"]
  d[variable=="E. Coli", units:="/100mL"]
  d[variable=="Colour", units:="?"]
  d[variable=="Intestinal Enterococci", units:="/100mL"]
  d[variable=="Coliform Bacteria", units:="/100mL"]
  d[variable=="Conductivity", units:="mS/m"]
  d[variable=="Colony count", units:="cfu/mL"]
  d[variable=="Turbidity", units:="NTU"]
  
  d[, waterwork := "Trondheim_Vikelvdalen"]
  d[, waterType := "Raw"]
  d[, point:="?"]
  
  saveRDS(d, file.path(RAWmisc::PROJ$CLEAN,"WP1_waterworks/Trondheim_Vikelvdalen_2008-2015.RDS"))

  ##########################################
  ##########################################
  ##########################################

  ########
  # data_raw/WP1_waterworks/Univann_Sjunken/Rådata/Råvannsdata 2006 - 2016.xlsx
  #
  #
  ########

  d <- data.table(readxl::read_excel(file.path(RAWmisc::PROJ$RAW,"WP1_waterworks/Univann_Sjunken/Rådata/Råvannsdata 2006 - 2016.xlsx"),
      sheet = 1,
      skip = 8,
      col_names = FALSE,
      col_types = c("text", "text", "date", rep("text", 4))))

  setnames(d, c(
            "X","X",
            "date",
            "X",
            "var",
            "value",
            "units"))

  d <- d[!is.na(var), which(names(d) != "X"), with = F]

  d[, type := "Accredited"]
  unique(d$var)

  d[, variable := ""]
  d[var == "09-Koliforme bakterier 37°C (/100 ml)", variable := "Coliform bacteria at 37 degrees"]
  d[var == "06-E.coli (/100 ml)", variable := "E. Coli"]
  d[var == "05-Clostridium perfringens (/100ml)", variable := "Clostridium Perfringens"]
  d[var == "07-Intestinale enterokokker (/100 ml)", variable := "Intestinal Enterococci"]
  d[var == "04-Turbiditet (FNU)", variable := "Turbidity"]
  d[var == "01-Farge (mg/l Pt)", variable := "Colour"]
  d[var == "08-Kimtall 22°C (/ml)", variable := "Colony count at 22 degrees"]
  d[var == "44-pH, surhetsgrad ( )", variable := "pH"]
  d[var == "35-Konduktivitet (mS/m)", variable := "Conductivity"]
  d[, var := NULL]
  d <- d[variable!=""]

  d[, waterwork := "Univann_Sjunken"]
  d[, waterType := "Raw"]
  d[, point := ""]

  saveRDS(d, file.path(RAWmisc::PROJ$CLEAN,"WP1_waterworks/Univann_Sjunken.RDS"))

  for(i in 2006:2016){
    CleanWP1SpecificWaterWork(fileIn=file.path(RAWmisc::PROJ$RAW,paste0("WP1_waterworks/Univann_Sjunken/Rådata/Egenkontroll ",i,".xls")),
                              fileOut=paste0("Univann_Sjunken_",i,".RDS"),
                              sheet=1,
                              skip=5,
                              col_types=c("text",rep("text", 17)),
                              col_names=c("date",
                                          rep("X",11),
                                          "Colour",
                                          "X",
                                          "pH",
                                          "X",
                                          "X",
                                          "Temperature"),
                              type="Internal",
                              units=NULL,
                              waterwork="Univann_Sjunken",
                              waterType="Raw",
                              point="?"
    )
  }
  ##########################################
  ##########################################
  ##########################################
  
  ########
  # data_raw/WP1_waterworks/Vestfold Vann IKS_Eidsfoss/Rådata/1-kr-Eidsfoss råvann - analyser fra hovedlab.xlsx",
  #
  #
  ########
  
  d <- data.table(readxl::read_excel(file.path(RAWmisc::PROJ$RAW,"WP1_waterworks/Vestfold Vann IKS_Eidsfoss/Rådata/1-kr-Eidsfoss råvann - analyser fra hovedlab.xlsx"),
                                     sheet = 1,
                                     skip = 5,
                                     col_names = FALSE,
                                     col_types = c("date", rep("text", 20))))
  
  setnames(d, c("date",
                "Colony count",
                "Coliform Bacteria",
                "E. Coli",
                "Intestinal Enterococci",
                "pH",
                "Turbidity",
                "Colour",
                "Clostridium Perfringens",
                "X","X","X",
                "Conductivity",
                "X","X",
                "TOC",
                "X","X","X","X","X"))
  
  d <- d[, which(names(d) != "X"), with = F]
  
  d <- melt.data.table(d, id = c("date"), variable.factor = FALSE)
  d[, variable := gsub("[0-9]*$", "", variable)]
  d[, type := "Accredited"]
  unique(d$variable)
  
  d[, units := ""]
  
  d[variable=="Colony count", units:="/mL"]
  d[variable=="Coliform Bacteria", units:="/100mL"]
  d[variable=="E. Coli", units:="/100mL"]
  d[variable=="Intestinal Enterococci", units:="/100mL"]
  d[variable=="pH", units:="pH"]
  d[variable=="Turbidity", units:="FTU"]
  d[variable=="Colour", units:=""]
  d[variable=="Clostridium Perfringens", units:="/100mL"]
  d[variable=="Conductivity", units:="mS/m"]
  d[variable=="TOC", units:="mg/L"]
  
  
  d[, waterwork := "Vestfold Vann IKS_Eidsfoss"]
  d[, waterType := "Raw"]
  d[, point:="?"]
  
  saveRDS(d, file.path(RAWmisc::PROJ$CLEAN,"WP1_waterworks/Vestfold Vann IKS_Eidsfoss.RDS"))
  
  ### INTERNAL ANALYSIS
  
  d <- data.table(readxl::read_excel(file.path(RAWmisc::PROJ$RAW,"WP1_waterworks/Vestfold Vann IKS_Eidsfoss/Rådata/2-jm-Labjournal Eidsfoss egne prøver_2009-16_råvann.xlsx"),
                                     sheet = 1,
                                     skip = 6,
                                     col_names = FALSE,
                                     col_types = c("text", rep("text", 7))))
  d[,X__1:=as.Date(as.numeric(X__1),origin="1899-12-30")]
  
  setnames(d, c("date",
                "Coliform Bacteria",
                "E. Coli",
                "Colony count",
                "pH",
                "Colour",
                "Turbidity",
                "Conductivity"))
  
  d <- d[, which(names(d) != "X"), with = F]
  
  d <- melt.data.table(d, id = c("date"), variable.factor = FALSE)
  d[, variable := gsub("[0-9]*$", "", variable)]
  d[, type := "Internal"]
  unique(d$variable)
  
  d[, units := ""]
  
  d[variable=="Colony count", units:="/mL"]
  d[variable=="Coliform Bacteria", units:="/100mL"]
  d[variable=="E. Coli", units:="/100mL"]
  d[variable=="Intestinal Enterococci", units:="/100mL"]
  d[variable=="pH", units:="pH"]
  d[variable=="Turbidity", units:="NTU"]
  d[variable=="Colour", units:="mg/l Pt"]
  d[variable=="Clostridium Perfringens", units:="/100mL"]
  d[variable=="Conductivity", units:="mS/m"]
  
  
  d[, waterwork := "Vestfold Vann IKS_Eidsfoss"]
  d[, waterType := "Raw"]
  d[, point:="?"]
  d <- na.omit(d)
  
  saveRDS(d, file.path(RAWmisc::PROJ$CLEAN,"WP1_waterworks/Vestfold Vann IKS_Eidsfoss_internal.RDS"))
  
  
  
  ########
  # data_raw/WP1_waterworks/Vestfold Vann IKS_Seierstad/Rådata/1-kr-Seierstad råvann - analyser fra hovedlab.xlsx",
  #
  #
  ########
  
  d <- data.table(readxl::read_excel(file.path(RAWmisc::PROJ$RAW,"WP1_waterworks/Vestfold Vann IKS_Seierstad/Rådata/1-kr-Seierstad råvann - analyser fra hovedlab.xlsx"),
                                     sheet = 1,
                                     skip = 5,
                                     col_names = FALSE,
                                     col_types = c("date", rep("text", 20))))
  
  setnames(d, c("date",
                "Colony count",
                "Coliform Bacteria",
                "E. Coli",
                "Intestinal Enterococci",
                "pH",
                "Turbidity",
                "Colour",
                "Clostridium Perfringens",
                "X","X","X",
                "Conductivity",
                "X","X",
                "TOC",
                "X","X","X","X","X"))
  
  d <- d[, which(names(d) != "X"), with = F]
  
  d <- melt.data.table(d, id = c("date"), variable.factor = FALSE)
  d[, variable := gsub("[0-9]*$", "", variable)]
  d[, type := "Accredited"]
  unique(d$variable)
  
  d[, units := ""]
  
  d[variable=="Colony count", units:="/mL"]
  d[variable=="Coliform Bacteria", units:="/100mL"]
  d[variable=="E. Coli", units:="/100mL"]
  d[variable=="Intestinal Enterococci", units:="/100mL"]
  d[variable=="pH", units:="pH"]
  d[variable=="Turbidity", units:="FTU"]
  d[variable=="Colour", units:=""]
  d[variable=="Clostridium Perfringens", units:="/100mL"]
  d[variable=="Conductivity", units:="mS/m"]
  d[variable=="TOC", units:="mg/L"]
  
  
  d[, waterwork := "Vestfold Vann IKS_Seierstad"]
  d[, waterType := "Raw"]
  d[, point:="?"]
  
  saveRDS(d, file.path(RAWmisc::PROJ$CLEAN,"WP1_waterworks/Vestfold Vann IKS_Seierstad.RDS"))
  
  ### INTERNAL ANALYSIS1
  
  d <- data.table(readxl::read_excel(file.path(RAWmisc::PROJ$RAW,"WP1_waterworks/Vestfold Vann IKS_Seierstad/Rådata/2-jm-lab.analyser Seierstad_egenanalyser 2006-16_råvann.xlsx"),
                                     sheet = 1,
                                     skip = 5,
                                     col_names = FALSE,
                                     col_types = c("text", rep("text", 4))))
  d[,X__1:=as.Date(as.numeric(X__1),origin="1899-12-30")]
  
  setnames(d, c("date",
                "pH",
                "Colour",
                "Turbidity",
                "Conductivity"))
  
  d <- d[, which(names(d) != "X"), with = F]
  
  d <- melt.data.table(d, id = c("date"), variable.factor = FALSE)
  d[, variable := gsub("[0-9]*$", "", variable)]
  d[, type := "Internal"]
  unique(d$variable)
  
  d[, units := ""]
  
  d[variable=="Colony count", units:="/mL"]
  d[variable=="Coliform Bacteria", units:="/100mL"]
  d[variable=="E. Coli", units:="/100mL"]
  d[variable=="Intestinal Enterococci", units:="/100mL"]
  d[variable=="pH", units:="pH"]
  d[variable=="Turbidity", units:="NTU"]
  d[variable=="Colour", units:="mg/l Pt"]
  d[variable=="Clostridium Perfringens", units:="/100mL"]
  d[variable=="Conductivity", units:="mS/m"]
  
  
  d[, waterwork := "Vestfold Vann IKS_Seierstad"]
  d[, waterType := "Raw"]
  d[, point:="?"]
  d <- na.omit(d)
  
  saveRDS(d, file.path(RAWmisc::PROJ$CLEAN,"WP1_waterworks/Vestfold Vann IKS_Seierstad_internal_1.RDS"))
  
  ### INTERNAL ANALYSIS2
  
  d <- data.table(readxl::read_excel(file.path(RAWmisc::PROJ$RAW,"WP1_waterworks/Vestfold Vann IKS_Seierstad/Rådata/2-jm-lab.analyser Seierstad_egenanalyser 2006-16_råvann.xlsx"),
                                     sheet = 1,
                                     skip = 4,
                                     col_names = FALSE,
                                     col_types = c("text", rep("text", 4))))
  d[,X__1:=as.Date(as.numeric(X__1),origin="1899-12-30")]
  
  setnames(d, c("date",
                "Coliform Bacteria",
                "E. Coli",
                "Colony count",
                "X"))
  
  d <- d[, which(names(d) != "X"), with = F]
  
  d <- melt.data.table(d, id = c("date"), variable.factor = FALSE)
  d[, variable := gsub("[0-9]*$", "", variable)]
  d[, type := "Internal"]
  unique(d$variable)
  
  d[, units := ""]
  
  d[variable=="Colony count", units:="/mL"]
  d[variable=="Coliform Bacteria", units:="/100mL"]
  d[variable=="E. Coli", units:="/100mL"]
  d[variable=="Intestinal Enterococci", units:="/100mL"]
  d[variable=="pH", units:="pH"]
  d[variable=="Turbidity", units:="NTU"]
  d[variable=="Colour", units:="mg/l Pt"]
  d[variable=="Clostridium Perfringens", units:="/100mL"]
  d[variable=="Conductivity", units:="mS/m"]
  
  
  d[, waterwork := "Vestfold Vann IKS_Seierstad"]
  d[, waterType := "Raw"]
  d[, point:="?"]
  d <- na.omit(d)
  
  saveRDS(d, file.path(RAWmisc::PROJ$CLEAN,"WP1_waterworks/Vestfold Vann IKS_Seierstad_internal_2.RDS"))
  
  
  ## BODO
  CleanWP1SpecificWaterWorkLong(fileIn=file.path(RAWmisc::PROJ$RAW,"WP1_waterworks/Bodø_Heggmoen/Rådata/heggmoenråvann.xlsx"), 
                                fileOut="heggmoenravann.RDS",
                                sheet=1,
                                type="Accredited",
                                waterwork="Bodo",
                                waterType="Raw",
                                remove=" \\[Drikkevann\\]"
                                      )
  
  
  
  ## ALESUND

  CleanWP1SpecificWaterWork(fileIn=file.path(RAWmisc::PROJ$RAW,"WP1_waterworks/Ålesund_Vasstrandlia/Rådata/Ålesund_råvann_akkreditert.xlsx"),
                            fileOut="alesund.RDS",
                            sheet=1,
                            skip=2,
                            col_types=c("date",rep("text", 11)),
                            col_names=c("date",
                                        "Coliform bacteria",
                                        "X",
                                        "Colony count",
                                        "E. Coli",
                                        "Colour",
                                        "Turbidity",
                                        "pH",
                                        "Conductivity",
                                        "Intestinal Enterococci",
                                        "Clostridium Perfringens",
                                        "TOC"
                                        ),
                            type="Accredited",
                            units=c("Colony count"="cfu/ml",
                                    "Coliform bacteria"="/100 ml",
                                    "E. Coli"="/100 ml",
                                    "Clostridium Perfringens"="cfu/100 ml",
                                    "Intestinal Enterococci"="cfu/100 ml",
                                    "Turbidity"="FNU",
                                    "Colour"="mg Pt/L",
                                    "TOC"="mg/L C",
                                    "Temperature"="C"),
                            waterwork="Alesund_Vasstrandlia",
                            waterType="Raw",
                            point="?"
  )
  
  
  CleanWP1SpecificWaterWork(fileIn=file.path(RAWmisc::PROJ$RAW,"WP1_waterworks/Ålesund_Vasstrandlia/Rådata/online.xlsx"),
                            fileOut="alesund_online.RDS",
                            sheet=1,
                            skip=2,
                            col_types=c("date",rep("text", 9)),
                            col_names=c("date",
                                        "pH",
                                        "X",
                                        "X",
                                        "Turbidity",
                                        rep("X",5)),
                            type="Online",
                            units=NULL,
                            waterwork="Alesund_Vasstrandlia",
                            waterType="Raw",
                            point="?"
  )

}

