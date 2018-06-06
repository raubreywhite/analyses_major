

WP1DataRaw <- function(){
  if(RUN_ALL) unlink(file.path(RAWmisc::PROJ$CLEAN,"WP1_raw.RDS"))
  bake(file.path(RAWmisc::PROJ$CLEAN,"WP1_raw.RDS"),{
    dir.create(file.path(RAWmisc::PROJ$CLEAN,"WP1_waterworks"))
    dir.create(file.path(RAWmisc::PROJ$FINAL,"WP1"))
    
    CleanDataWP1NVE()
    CleanDataWP1MET()
    CleanDataWaterworksRawWater()
    d <- readRDS(file.path(RAWmisc::PROJ$CLEAN,"WP1_raw.RDS"))
    d <- d[value>=0]
    
    d[,wp950_gridRunoffStandardised0_3:=wp950_gridRunoffStandardised0_0+wp950_gridRunoffStandardised1_1+wp950_gridRunoffStandardised2_2+wp950_gridRunoffStandardised3_3]
    d[,wp950_gridRain0_3:=wp950_gridRain0_0+wp950_gridRain1_1+wp950_gridRain2_2+wp950_gridRain3_3]
    d[,wp950_gridPrecip0_3:=wp950_gridPrecip0_0+wp950_gridPrecip1_1+wp950_gridPrecip2_2+wp950_gridPrecip3_3]
    d[,wp950_temperature0_3:=wp950_temperature0_0+wp950_temperature1_1+wp950_temperature2_2+wp950_temperature3_3]
    
    d[,c_gridRunoffStandardised0_3:=(c_gridRunoffStandardised0_0+c_gridRunoffStandardised1_1+c_gridRunoffStandardised2_2+c_gridRunoffStandardised3_3)/4]
    d[,c_gridRain0_3:=(c_gridRain0_0+c_gridRain1_1+c_gridRain2_2+c_gridRain3_3)/4]
    d[,c_gridPrecip0_3:=(c_gridPrecip0_0+c_gridPrecip1_1+c_gridPrecip2_2+c_gridPrecip3_3)/4]
    d[,c_temperature0_3:=(c_temperature0_0+c_temperature1_1+c_temperature2_2+c_temperature3_3)/4]
    
    }) -> d
  return(d[year %in% c(2006:2014)])
}

WP1DataClean <- function(){
  if(RUN_ALL) unlink(file.path(RAWmisc::PROJ$CLEAN,"WP1_clean.RDS"))
  bake(file.path(RAWmisc::PROJ$CLEAN,"WP1_clean.RDS"),{
    dir.create(file.path(RAWmisc::PROJ$CLEAN,"WP1_waterworks_clean_water"))
    dir.create(file.path(RAWmisc::PROJ$FINAL,"WP1"))
    
    CleanDataWP1NVE()
    CleanDataWP1MET()
    CleanDataWaterworksCleanWater()
    d <- readRDS(file.path(RAWmisc::PROJ$CLEAN,"WP1_clean.RDS"))
    d <- d[value>=0]
    
    d[,wp950_gridRunoffStandardised0_3:=wp950_gridRunoffStandardised0_0+wp950_gridRunoffStandardised1_1+wp950_gridRunoffStandardised2_2+wp950_gridRunoffStandardised3_3]
    d[,wp950_gridRain0_3:=wp950_gridRain0_0+wp950_gridRain1_1+wp950_gridRain2_2+wp950_gridRain3_3]
    d[,wp950_gridPrecip0_3:=wp950_gridPrecip0_0+wp950_gridPrecip1_1+wp950_gridPrecip2_2+wp950_gridPrecip3_3]
    d[,wp950_temperature0_3:=wp950_temperature0_0+wp950_temperature1_1+wp950_temperature2_2+wp950_temperature3_3]
    
    d[,c_gridRunoffStandardised0_3:=(c_gridRunoffStandardised0_0+c_gridRunoffStandardised1_1+c_gridRunoffStandardised2_2+c_gridRunoffStandardised3_3)/4]
    d[,c_gridRain0_3:=(c_gridRain0_0+c_gridRain1_1+c_gridRain2_2+c_gridRain3_3)/4]
    d[,c_gridPrecip0_3:=(c_gridPrecip0_0+c_gridPrecip1_1+c_gridPrecip2_2+c_gridPrecip3_3)/4]
    d[,c_temperature0_3:=(c_temperature0_0+c_temperature1_1+c_temperature2_2+c_temperature3_3)/4]
    
  }) -> d
  return(d[year %in% c(2006:2014)])
}

WP2Data <- function(initialDataCall=FALSE){
  if(initialDataCall & RUN_ALL) system(paste0("rm -f ",file.path(RAWmisc::PROJ$CLEAN,"WP2.RDS")))
  bake(file.path(RAWmisc::PROJ$CLEAN,"WP2.RDS"),{
    CleanData()
  }) -> d
  return(d[year %in% c(2006:2014)])
}


WP2WaterworkRawData <- function(){
  d1 <- readRDS(file.path(RAWmisc::PROJ$CLEAN,"WP1_raw.RDS"))
  d2 <- readRDS(file.path(RAWmisc::PROJ$CLEAN,"WP2.RDS"))
  
  d1 <- d1[type %in% c("Accredited","Internal")]
  
  keys <- readxl::read_excel(file.path(RAWmisc::PROJ$RAW,"waterworks_to_kommune.xlsx"),skip=2,sheet=2)
  keys <- keys[,c("waterworkRaw","Mottatt - K nr","Forsynings-grad 2014")]
  keys <- data.table(keys)
  keys <- keys[!is.na(waterworkRaw)]
  setnames(keys,c("waterwork","municip","forsynings"))
  keys
  keys[,municip:=formatC(municip,flag="0",width=4)]
  keys[,municip:=paste0("municip",municip)]
  keys <- keys[forsynings>0.85]
  
  d1x <- merge(d1,keys,by="waterwork",allow.cartesian = TRUE)
  d1x <- d1x[type!="Online",.(value=mean(value)),
             by=.(municip,year,week,variable)]
  d2[1]
  d <- merge(d2,d1x,by=c("municip","year","week"),all.x=T,allow.cartesian = T)
  setorder(d,variable,age,municip,year,week)
  d[,hasValue:=!is.na(value)]
  d[,numValue:=sum(hasValue),by=.(variable,age,municip)]
  d <- d[numValue>100]
  
  d <- d[,c("municip","year","week","season","month","s_outbreakLege","s_consult","s_gastro","s_pop","age","variable","value"),with=F]
  d[,value0:=value]
  d[,value1:=shift(value,n=1L),by=.(variable,age,municip)]
  d[,value2:=shift(value,n=2L),by=.(variable,age,municip)]
  d[,value3:=shift(value,n=3L),by=.(variable,age,municip)]
  d[,value4:=shift(value,n=4L),by=.(variable,age,municip)]
  d[,s_pop:=round(s_pop)]
  d[,id:=municip]
  return(d[year %in% c(2006:2014)])
}


WP2WaterworkCleanData <- function(){
  d1 <- readRDS(file.path(RAWmisc::PROJ$CLEAN,"WP1_clean.RDS"))
  d2 <- readRDS(file.path(RAWmisc::PROJ$CLEAN,"WP2.RDS"))
  
  d1 <- d1[type %in% c("Accredited","Internal")]
  
  keys <- readxl::read_excel(file.path(RAWmisc::PROJ$RAW,"waterworks_to_kommune.xlsx"),skip=2,sheet=2)
  keys <- keys[,c("waterworkClean","Mottatt - K nr","Forsynings-grad 2014")]
  keys <- data.table(keys)
  keys <- keys[!is.na(waterworkClean)]
  setnames(keys,c("waterwork","municip","forsynings"))
  keys
  keys[,municip:=formatC(municip,flag="0",width=4)]
  keys[,municip:=paste0("municip",municip)]
  keys <- keys[forsynings>0.85]
  
  d1x <- merge(d1,keys,by="waterwork",allow.cartesian = TRUE)
  d1x <- d1x[type!="Online",.(value=mean(value)),
             by=.(municip,year,week,variable)]
  d2[1]
  d <- merge(d2,d1x,by=c("municip","year","week"),all.x=T,allow.cartesian = T)
  setorder(d,variable,age,municip,year,week)
  d[,hasValue:=!is.na(value)]
  d[,numValue:=sum(hasValue),by=.(variable,age,municip)]
  d <- d[numValue>100]
  
  d <- d[,c("municip","year","week","season","month","s_outbreakLege","s_consult","s_gastro","s_pop","age","variable","value"),with=F]
  d[,value0:=value]
  d[,value1:=shift(value,n=1L),by=.(variable,age,municip)]
  d[,value2:=shift(value,n=2L),by=.(variable,age,municip)]
  d[,value3:=shift(value,n=3L),by=.(variable,age,municip)]
  d[,value4:=shift(value,n=4L),by=.(variable,age,municip)]
  d[,s_pop:=round(s_pop)]
  d[,id:=municip]
  return(d[year %in% c(2006:2014)])
}

