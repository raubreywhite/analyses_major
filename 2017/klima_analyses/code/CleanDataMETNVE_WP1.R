CleanDataWP1NVEGridded <- function(){
  files <- list.files(file.path(org::PROJ$RAW,"WP1_Gridded","precip_rain"))
  resA <- vector("list",length=length(files))
  for(i in files){
    d <- fread(file.path(org::PROJ$RAW,"WP1_Gridded","precip_rain",i))
    d[,waterwork:=gsub("_gP.txt","",i)]
    resA[[i]] <- d
  }
  resA <- rbindlist(resA)
  setnames(resA,c("year","month","day","gridPrecip","gridRain","waterwork"))
  
  files <- list.files(file.path(org::PROJ$RAW,"WP1_Gridded","runoff"))
  resB <- vector("list",length=length(files))
  for(i in files){
    d <- fread(file.path(org::PROJ$RAW,"WP1_Gridded","runoff",i))
    d[,waterwork:=gsub("_runoff.txt","",i)]
    resB[[i]] <- d
  }
  resB <- rbindlist(resB)
  setnames(resB,c("year","month","day","gridRunoffStandardised","x","waterwork"))
  resB[,x:=NULL]
  
  res <- merge(resA, resB, by=c("year","month","day","waterwork"))
  dim(res)
  dim(resA)
  dim(resB)
  
  res[,date:=as.Date(paste0(year,"-",month,"-",day))]
  res[,year:=NULL]
  res[,month:=NULL]
  res[,day:=NULL]
  res[,year := format.Date(date,"%G")] #Week-based year, instead of normal year (%Y)
  res[,week := as.numeric(format.Date(date,"%V"))]
  
  setorder(res,waterwork,date)
  res[, wp950_gridPrecip0_0 := AbovePercentile(gridPrecip, 0.95), by = waterwork]
  res[, wp950_gridRain0_0 := AbovePercentile(gridRain, 0.95), by = waterwork]
  res[, wp950_gridRunoffStandardised0_0 := AbovePercentile(gridRunoffStandardised, 0.95), by = waterwork]
  
  res <- res[week %in% c(1:52) & year <=2014,
             .(wp950_gridPrecip0_0 = sum(wp950_gridPrecip0_0),
               wp950_gridRain0_0 = sum(wp950_gridRain0_0),
               wp950_gridRunoffStandardised0_0 = sum(wp950_gridRunoffStandardised0_0),
               c_gridPrecip0_0 = mean(gridPrecip,na.rm=T),
               c_gridRain0_0 = mean(gridRain,na.rm=T),
               c_gridRunoffStandardised0_0 = mean(gridRunoffStandardised,na.rm=T)
             ),
             by=.(
               waterwork,year,week
             )
             ]
  
  #weeklyConsults <- dataWeek[,.(s_consult=mean(s_consult)),by=.(municip)]
  #keep <- weeklyConsults[s_consult>200]$municip
  #dataWeek <- dataWeek[municip %in% keep]
  
  
  #alloc.col(dataWeek,3000)
  
  
  for(i in 1:4){
    var <- paste0("wp950_gridPrecip",i,"_",i)
    res[,(var):=shift(wp950_gridPrecip0_0,i),by=waterwork]
    
    var <- paste0("wp950_gridRain",i,"_",i)
    res[,(var):=shift(wp950_gridRain0_0,i),by=waterwork]
    
    var <- paste0("wp950_gridRunoffStandardised",i,"_",i)
    res[,(var):=shift(wp950_gridRunoffStandardised0_0,i),by=waterwork]
    
    var <- paste0("c_gridPrecip",i,"_",i)
    res[,(var):=shift(c_gridPrecip0_0,i),by=waterwork]
    
    var <- paste0("c_gridRain",i,"_",i)
    res[,(var):=shift(c_gridRain0_0,i),by=waterwork]
    
    var <- paste0("c_gridRunoffStandardised",i,"_",i)
    res[,(var):=shift(c_gridRunoffStandardised0_0,i),by=waterwork]
  }
  
  
  saveRDS(res, file.path(org::PROJ$CLEAN,"wp1_nve_gridrain.RDS"))
}

CleanDataWP1NVENotGridded <- function(){
  files <- list.files(file.path(org::PROJ$RAW,"WP1_NVE"))
  res <- vector("list",length=length(files))
  for(i in files){
    d <- fread(file.path(org::PROJ$RAW,"WP1_NVE",i))
    d[,waterwork:=gsub(".txt","",i)]
    res[[i]] <- d
  }
  res <- rbindlist(res)
  setnames(res,c("year","month","day","discharge","waterwork"))
  res[,date:=as.Date(paste0(year,"-",month,"-",day))]
  res[,year:=NULL]
  res[,month:=NULL]
  res[,day:=NULL]
  res[,year := format.Date(date,"%G")] #Week-based year, instead of normal year (%Y)
  res[,week := as.numeric(format.Date(date,"%V"))]
  
  setorder(res,waterwork,date)
  res[, wp990_discharge0_0 := AbovePercentile(discharge, 0.99), by = waterwork]
  res[, wp950_discharge0_0 := AbovePercentile(discharge, 0.95), by = waterwork]
  
  res <- res[week %in% c(1:52) & year <=2014,
             .(wp990_discharge0_0 = sum(wp990_discharge0_0),
               wp950_discharge0_0 = sum(wp950_discharge0_0),
               c_discharge0_0 = mean(discharge,na.rm=T)
             ),
             by=.(
               waterwork,year,week
             )
             ]
  
  #weeklyConsults <- dataWeek[,.(s_consult=mean(s_consult)),by=.(municip)]
  #keep <- weeklyConsults[s_consult>200]$municip
  #dataWeek <- dataWeek[municip %in% keep]
  
  
  #alloc.col(dataWeek,3000)
  
  
  for(i in 1:4){
    var <- paste0("wp950_discharge",i,"_",i)
    res[,(var):=shift(wp950_discharge0_0,i),by=waterwork]
    
    var <- paste0("wp990_discharge",i,"_",i)
    res[,(var):=shift(wp990_discharge0_0,i),by=waterwork]
    
    var <- paste0("c_discharge",i,"_",i)
    res[,(var):=shift(c_discharge0_0,i),by=waterwork]
  }
  
  
  saveRDS(res, file.path(org::PROJ$CLEAN,"wp1_nve_discharge.RDS"))
}

CleanDataWP1NVE <- function(){
  CleanDataWP1NVEGridded()
  CleanDataWP1NVENotGridded()
  
  a <- readRDS(file.path(org::PROJ$CLEAN,"wp1_nve_gridrain.RDS"))
  b <- readRDS(file.path(org::PROJ$CLEAN,"wp1_nve_discharge.RDS"))
  d <- merge(a,b,by=c("waterwork","year","week"))
  return(d)
}

CleanDataWP1MET <- function(){
  tempdata <- readxl::read_excel(file.path(org::PROJ$RAW,"WP1_MET_intakepoints/WW_Max_Temp.xlsx"))
  tempdata <- data.table(tempdata)
  setnames(tempdata,c("met","stationNum","date","temperature"))
  tempdata[,temperature:=as.numeric(temperature)]
  tempdata[,stationNum:=NULL]
  
  data <- readxl::read_excel(file.path(org::PROJ$RAW,"WP1_MET_intakepoints/WW_Precip_Rain.xlsx"))
  data <- data.table(data)
  setnames(data,c("met","stationNum","date","precip","rain"))
  data[,rain:=as.numeric(rain)]
  data[,precip:=as.numeric(precip)]
  
  dim(data)
  data <- merge(data,tempdata,by=c("met","date"),all.x=T)
  dim(data)
  
  stationNames <- data.table(readxl::read_excel(file.path(org::PROJ$RAW,"names.xlsx"))) #WP1_MET_intakepoints/Kopi av Info_Vannverk.xlsx"))
  stationNames <- stationNames[!is.na(waterwork) & !is.na(nve) & !is.na(met),]
  stationNames <- data.table(stationNames[,c("waterwork","met"),with=F])
  stationNames[,met:=as.numeric(met)]
  
  dim(data)
  x <- merge(data,stationNames,by=c("met"),all.y=TRUE,allow.cartesian = TRUE)
  dim(x)
  unique(data$met)[!unique(data$met) %in% unique(x$met)]
  x[,met:=NULL]
  x[,stationNum:=NULL]
  res <- x[!is.na(waterwork)]
  dim(res)
  res <- res[,.(precip=mean(precip,na.rm=T),rain=mean(rain,na.rm=T),temperature=mean(temperature,na.rm=T)),by=.(date,waterwork)]
  dim(res)
  
  res[,year := format.Date(date,"%G")] #Week-based year, instead of normal year (%Y)
  res[,week := as.numeric(format.Date(date,"%V"))]
  
  setorder(res,waterwork,date)
  res[, wp990_rain0_0 := AbovePercentile(rain, 0.99), by = waterwork]
  res[, wp950_rain0_0 := AbovePercentile(rain, 0.95), by = waterwork]
  
  res[, wp990_precip0_0 := AbovePercentile(precip, 0.99), by = waterwork]
  res[, wp950_precip0_0 := AbovePercentile(precip, 0.95), by = waterwork]
  
  res[, wp990_temperature0_0 := 0]
  res[, wp950_temperature0_0 := 0]
  res[temperature>=20, wp990_temperature0_0 := 1]
  res[temperature>=20, wp950_temperature0_0 := 1]
  
  res <- res[week %in% c(1:52) & year <=2014,
             .(wp990_rain0_0 = sum(wp990_rain0_0,na.rm=T),
               wp950_rain0_0 = sum(wp950_rain0_0,na.rm=T),
               wp990_precip0_0 = sum(wp990_precip0_0,na.rm=T),
               wp950_precip0_0 = sum(wp950_precip0_0,na.rm=T),
               wp990_temperature0_0 = sum(wp990_temperature0_0,na.rm=T),
               wp950_temperature0_0 = sum(wp950_temperature0_0,na.rm=T),
               c_rain0_0 = mean(rain,na.rm=T),
               c_precip0_0 = mean(precip,na.rm=T),
               c_temperature0_0 = mean(temperature,na.rm=T)
             ),
             by=.(
               waterwork,year,week
             )
             ]
  
  
  for(i in 1:4){
    var <- paste0("wp950_rain",i,"_",i)
    res[,(var):=shift(wp950_rain0_0,i),by=.(waterwork)]
    
    var <- paste0("wp990_rain",i,"_",i)
    res[,(var):=shift(wp990_rain0_0,i),by=.(waterwork)]
    
    var <- paste0("c_rain",i,"_",i)
    res[,(var):=shift(c_rain0_0,i),by=.(waterwork)]
  }
  
  
  for(i in 1:4){
    var <- paste0("wp950_precip",i,"_",i)
    res[,(var):=shift(wp950_precip0_0,i),by=.(waterwork)]
    
    var <- paste0("wp990_precip",i,"_",i)
    res[,(var):=shift(wp990_precip0_0,i),by=.(waterwork)]
    
    var <- paste0("c_precip",i,"_",i)
    res[,(var):=shift(c_precip0_0,i),by=.(waterwork)]
  }
  
  for(i in 1:4){
    var <- paste0("wp950_temperature",i,"_",i)
    res[,(var):=shift(wp950_temperature0_0,i),by=.(waterwork)]
    
    var <- paste0("wp990_temperature",i,"_",i)
    res[,(var):=shift(wp990_temperature0_0,i),by=.(waterwork)]
    
    var <- paste0("c_temperature",i,"_",i)
    res[,(var):=shift(c_temperature0_0,i),by=.(waterwork)]
  }
  saveRDS(res, file.path(org::PROJ$CLEAN,"wp1_met_rain.RDS"))
}


CleanDataWaterworksRawWater <- function() {
  f <- list.files(file.path(org::PROJ$CLEAN,"WP1_waterworks"))
  for(i in f) file.remove(file.path(org::PROJ$CLEAN,"WP1_waterworks",i))
  CleanDataWaterworksInternal()
  f <- list.files(file.path(org::PROJ$CLEAN,"WP1_waterworks"))
  d <- vector("list", length(f))
  for (i in 1:length(d)) {
    d[[i]] <- readRDS(file.path(org::PROJ$CLEAN, "WP1_waterworks", f[i]))
    d[[i]][, date := as.Date(date)]
    setcolorder(d[[i]],names(d[[1]]))
  }
  
  d <- rbindlist(d)
  d[, value := gsub(" ", "", value)]
  d[, value := gsub(",", ".", value)]
  d[, value := gsub(">", "", value)]
  d[, value := gsub("<", "", value)]
  d[, value := as.numeric(value)]
  d <- d[!is.na(value)]
  unique(d[type=="Online"]$variable)
  unique(d$type)
  #d <- d[type(type=="Online" & )]
  d[is.na(units), units := "?"]
  d[is.na(type), type := "?"]
  d[is.na(waterType), waterType := "?"]
  d[is.na(point), point := "?"]
  
  d[units=="", units := "?"]
  d[type=="", type := "?"]
  d[waterType=="", waterType := "?"]
  d[point == "", point := "?"]
  
  d[variable == "pH", units := "pH"]
  d[variable == "Temperature", units := "C"]
  
  d[units %in% c("mg Pt/l", "mg/l Pt", "mg/lPt", "Milligram/l Pt"), units := "mg Pt/L"]
  d[units %in% c("/100ml", "ant/100ml", "CFU/100ml", "kde/100ml"), units := "/100mL"]
  d[units %in% c("/100 ml", "ant/100 ml"), units := "/100mL"]
  d[units %in% c("/ml", "ant/ml", "CFU/ml", "CFU/mL"), units := "/mL"]
  
  d[units=="myS/cm2 ", value:=value/10]
  d[units=="myS/cm2 ", units:="mS/m"]
  
  d <- d[,.(value=mean(value,na.rm=T)),by=.(date,point,units,type,variable,waterwork,waterType)]
  d[,waterwork:=paste0(waterwork,"||",point)]
  if(FALSE){
    #unique(d$x)
    unique(d$waterwork)
    setnames(d,"waterwork","temp")
    d[,waterwork:=""]
    d[temp=="Arendal_Rore",waterwork:="Rore"]
    d[temp=="Asker og Baerum vannverk IKS_Aurevann",waterwork:="Aurevann"]
    d[grep("Asker og Baerum vannverk IKS_Kattås",temp),waterwork:="Kattaas"]
    d[temp=="Bergen_Espeland",waterwork:="Espeland"]
    d[grep("FREVAR_Høyfjell",temp),waterwork:="Hoeyfjell"]
    d[temp=="Glitrevannverket IKS_Kleivdammen",waterwork:="Kleivdammen"]
    d[temp=="Glitrevannverket IKS_Landfall",waterwork:="Landfall"]
    d[temp=="Halden_Lille Erte",waterwork:="LilleErte"]
    d[temp=="HIAS_Hamar",waterwork:="Hamar"]
    d[temp=="HIAS_Stange",waterwork:="Stange"]
    d[grep("Karmøy_Brekke",temp),waterwork:="Brekke"]
    d[temp=="Kongsvinger_Granli_GIVAS",waterwork:="Granli"]
    d[temp=="Kristiansand_Rossevann",waterwork:="Rossevann"]
    d[temp=="Kristiansand_Tronstadvann",waterwork:="Tronstadvann"]
    d[temp=="Lillehammer",waterwork:="Lillehammer"]
    d[grep("MOVAR_Vansjø",temp),waterwork:="Vansjoe"]
    d[temp=="Nedre Romerike Vannverk IKS_Hauglifjell",waterwork:="Hauglifjell"]
    d[temp=="Oslo_Oset",waterwork:="Oset"]
    d[grep("Tromsø_Kvaløya",temp),waterwork:="Kvaloeya"]
    d[grep("Tromsø_Simavik",temp),waterwork:="Simavik"]
    d[temp=="Trondheim_Vikelvdalen",waterwork:="Vikelvdalen"]
    d[temp=="Univann_Sjunken",waterwork:="Sjunken"]
    d[temp=="Vestfold Vann IKS_Eidsfoss",waterwork:="Eidsfoss"]
    d[temp=="Vestfold Vann IKS_Seierstad",waterwork:="Seierstad"]
    
    d[,waterworkpoint:=paste0(waterwork,"/",point)]
    sort(unique(d$waterworkpoint))
  }
  
  d[,year := format.Date(date,"%G")] #Week-based year, instead of normal year (%Y)
  d[,week := as.numeric(format.Date(date,"%V"))]
  d[,month:=as.numeric(strftime(date, "%m"))]
  
  d <- d[,.(value=mean(value,na.rm=T),month=floor(month)),by=.(year,week,point,units,type,variable,waterwork,waterType)]
  d[,season:=1]
  d[month %in% 3:5,season:=2]
  d[month %in% 6:8,season:=3]
  d[month %in% 9:11,season:=4]
  Encoding(d$waterwork) <- "UTF-8"
  nve <- CleanDataWP1NVE() #readRDS(file.path(org::PROJ$CLEAN,"wp1_nve_discharge.RDS"))
  setnames(nve,"waterwork","nve")
  stationNames <- data.table(readxl::read_excel(file.path(org::PROJ$RAW,"names.xlsx"))) #WP1_MET_intakepoints/Kopi av Info_Vannverk.xlsx"))
  stationNames <- stationNames[!is.na(waterwork) & !is.na(nve) & !is.na(met),]
  stationNames <- unique(data.table(stationNames[,c("waterwork","nve"),with=F]))
  stationNames <- stationNames[!is.na(waterwork) & !is.na(nve)]
  Encoding(stationNames$waterwork) <- "UTF-8"
  # these are new waterworks
  unique(d[!waterwork %in% stationNames$waterwork]$waterwork)
  nrow(nve)
  length(unique(nve$waterwork))
  length(unique(nve$nve))
  nve <- merge(nve,stationNames,by="nve",allow.cartesian = TRUE)
  length(unique(nve$nve))
  length(unique(nve$waterwork))
  nrow(nve)
  
  met <- readRDS(file.path(org::PROJ$CLEAN,"wp1_met_rain.RDS"))
  length(unique(d$waterwork))
  d <- na.omit(d)
  d1 <- merge(d,nve,by=c("year","week","waterwork"),allow.cartesian = TRUE)
  length(unique(d1$waterwork))
  d2 <- merge(d1,met,all.x=TRUE,by=c("year","week","waterwork"))
  length(unique(d2$waterwork))
  
  dim(d)
  dim(d1)
  dim(d2)
  
  unique(d$waterwork)[!unique(d$waterwork) %in% unique(d1$waterwork)]
  unique(d$waterwork)[!unique(d$waterwork) %in% unique(d2$waterwork)]
  x <- unique(d[,c("waterwork"),with=F])
  x[,waterworkNum:=""]
  if(!file.exists(file.path(org::PROJ$RAW,"WP1_MET_intakepoints/codes.csv"))) write.table(x,file=file.path(org::PROJ$RAW,"WP1_MET_intakepoints/codes.csv"),sep=";",row.names=F)
  
  d2[type %in% c("Accredited","External"),type:="Accredited"]
  d2[,type:=factor(type,levels=c("Accredited","Internal","Online","?"))]
  
  d2[,id:=waterwork]
  #unique(d$id)[order(unique(d$id))]
  #unique(d$waterwork)[order(unique(d$waterwork))]
  
  d2[,watersource:="Surface water"]
  d2[nve %in% c("Lillehammer","Granli", "Elvestrand"),watersource:="Groundwater"]
  d2[nve %in% c("Hauglifjell","Bateroed"),watersource:="River"]
  #xtabs( ~d$watersource)
  
  #unique(d$variable)[order(unique(d$variable))]
  d2[variable %in% c("Colony count at 22 degrees","Colony count at 37 degrees"),variable:="Colony count"]
  d2[variable %in% c("pH at 19-25 degrees"),variable:="pH"]
  d2[variable %in% c("Coliform Bacteria","Coliform bacteria at 37 degrees"),variable:="Coliform bacteria"]
  d2[variable %in% c("Hazen","Colour (after filtering)"),variable:="Colour"]
  
  d2 <- d2[!variable %in% c("Clostridium Perfringens","TOC","Temperature")]
  dim(d2)
  d2 <- d2[,.(
    value=mean(value,na.rm=T),
    wp950_discharge0_0=mean(wp950_discharge0_0,na.rm=T),
    wp950_discharge1_1=mean(wp950_discharge1_1,na.rm=T),
    wp950_discharge2_2=mean(wp950_discharge2_2,na.rm=T),
    wp950_discharge3_3=mean(wp950_discharge3_3,na.rm=T),
    wp950_discharge4_4=mean(wp950_discharge4_4,na.rm=T),
    wp950_rain0_0=mean(wp950_rain0_0,na.rm=T),
    wp950_rain1_1=mean(wp950_rain1_1,na.rm=T),
    wp950_rain2_2=mean(wp950_rain2_2,na.rm=T),
    wp950_rain3_3=mean(wp950_rain3_3,na.rm=T),
    wp950_rain4_4=mean(wp950_rain4_4,na.rm=T),
    wp950_precip0_0=mean(wp950_precip0_0,na.rm=T),
    wp950_precip1_1=mean(wp950_precip1_1,na.rm=T),
    wp950_precip2_2=mean(wp950_precip2_2,na.rm=T),
    wp950_precip3_3=mean(wp950_precip3_3,na.rm=T),
    wp950_precip4_4=mean(wp950_precip4_4,na.rm=T),
    wp950_temperature0_0=mean(wp950_temperature0_0,na.rm=T),
    wp950_temperature1_1=mean(wp950_temperature1_1,na.rm=T),
    wp950_temperature2_2=mean(wp950_temperature2_2,na.rm=T),
    wp950_temperature3_3=mean(wp950_temperature3_3,na.rm=T),
    wp950_temperature4_4=mean(wp950_temperature4_4,na.rm=T),
    wp950_gridRain0_0=mean(wp950_gridRain0_0,na.rm=T),
    wp950_gridRain1_1=mean(wp950_gridRain1_1,na.rm=T),
    wp950_gridRain2_2=mean(wp950_gridRain2_2,na.rm=T),
    wp950_gridRain3_3=mean(wp950_gridRain3_3,na.rm=T),
    wp950_gridRain4_4=mean(wp950_gridRain4_4,na.rm=T),
    wp950_gridPrecip0_0=mean(wp950_gridPrecip0_0,na.rm=T),
    wp950_gridPrecip1_1=mean(wp950_gridPrecip1_1,na.rm=T),
    wp950_gridPrecip2_2=mean(wp950_gridPrecip2_2,na.rm=T),
    wp950_gridPrecip3_3=mean(wp950_gridPrecip3_3,na.rm=T),
    wp950_gridPrecip4_4=mean(wp950_gridPrecip4_4,na.rm=T),
    wp950_gridRunoffStandardised0_0=mean(wp950_gridRunoffStandardised0_0,na.rm=T),
    wp950_gridRunoffStandardised1_1=mean(wp950_gridRunoffStandardised1_1,na.rm=T),
    wp950_gridRunoffStandardised2_2=mean(wp950_gridRunoffStandardised2_2,na.rm=T),
    wp950_gridRunoffStandardised3_3=mean(wp950_gridRunoffStandardised3_3,na.rm=T),
    wp950_gridRunoffStandardised4_4=mean(wp950_gridRunoffStandardised4_4,na.rm=T),
    c_discharge0_0=mean(c_discharge0_0,na.rm=T),
    c_discharge1_1=mean(c_discharge1_1,na.rm=T),
    c_discharge2_2=mean(c_discharge2_2,na.rm=T),
    c_discharge3_3=mean(c_discharge3_3,na.rm=T),
    c_discharge4_4=mean(c_discharge4_4,na.rm=T),
    c_rain0_0=mean(c_rain0_0,na.rm=T),
    c_rain1_1=mean(c_rain1_1,na.rm=T),
    c_rain2_2=mean(c_rain2_2,na.rm=T),
    c_rain3_3=mean(c_rain3_3,na.rm=T),
    c_rain4_4=mean(c_rain4_4,na.rm=T),
    c_precip0_0=mean(c_precip0_0,na.rm=T),
    c_precip1_1=mean(c_precip1_1,na.rm=T),
    c_precip2_2=mean(c_precip2_2,na.rm=T),
    c_precip3_3=mean(c_precip3_3,na.rm=T),
    c_precip4_4=mean(c_precip4_4,na.rm=T),
    c_temperature0_0=mean(c_temperature0_0,na.rm=T),
    c_temperature1_1=mean(c_temperature1_1,na.rm=T),
    c_temperature2_2=mean(c_temperature2_2,na.rm=T),
    c_temperature3_3=mean(c_temperature3_3,na.rm=T),
    c_temperature4_4=mean(c_temperature4_4,na.rm=T),
    c_gridRain0_0=mean(c_gridRain0_0,na.rm=T),
    c_gridRain1_1=mean(c_gridRain1_1,na.rm=T),
    c_gridRain2_2=mean(c_gridRain2_2,na.rm=T),
    c_gridRain3_3=mean(c_gridRain3_3,na.rm=T),
    c_gridRain4_4=mean(c_gridRain4_4,na.rm=T),
    c_gridPrecip0_0=mean(c_gridPrecip0_0,na.rm=T),
    c_gridPrecip1_1=mean(c_gridPrecip1_1,na.rm=T),
    c_gridPrecip2_2=mean(c_gridPrecip2_2,na.rm=T),
    c_gridPrecip3_3=mean(c_gridPrecip3_3,na.rm=T),
    c_gridPrecip4_4=mean(c_gridPrecip4_4,na.rm=T),
    c_gridRunoffStandardised0_0=mean(c_gridRunoffStandardised0_0,na.rm=T),
    c_gridRunoffStandardised1_1=mean(c_gridRunoffStandardised1_1,na.rm=T),
    c_gridRunoffStandardised2_2=mean(c_gridRunoffStandardised2_2,na.rm=T),
    c_gridRunoffStandardised3_3=mean(c_gridRunoffStandardised3_3,na.rm=T),
    c_gridRunoffStandardised4_4=mean(c_gridRunoffStandardised4_4,na.rm=T)
  ), by=.(
    year,
    week,
    waterwork,
    point,
    units,
    type,
    variable,
    waterType,
    month,
    season,
    id,
    watersource
  )]
  dim(d2)
  d2[,id:=gsub("/","___",id)]
  d2 <- d2[!(year==2002 & week==7 & waterwork=="Nedre Romerike Vannverk IKS_Hauglifjell||?")]
  d2 <- d2[!(year==2003 & week==4 & waterwork=="Nedre Romerike Vannverk IKS_Hauglifjell||?")]
  d2 <- d2[!(year==2006 & week==16 & waterwork=="Nedre Romerike Vannverk IKS_Hauglifjell||?")]
  saveRDS(d2,file.path(org::PROJ$CLEAN,"WP1_raw.RDS"))
  
}

CleanDataWaterworksCleanWater <- function() {
  f <- list.files(file.path(org::PROJ$CLEAN,"WP1_waterworks_clean_water"))
  for(i in f) file.remove(file.path(org::PROJ$CLEAN,"WP1_waterworks_clean_water",i))
  CleanDataWaterworksCleanWaterInternal()
  f <- list.files(file.path(org::PROJ$CLEAN,"WP1_waterworks_clean_water"))
  d <- vector("list", length(f))
  for (i in 1:length(d)) {
    d[[i]] <- readRDS(file.path(org::PROJ$CLEAN, "WP1_waterworks_clean_water", f[i]))
    d[[i]][, date := as.Date(date)]
    setcolorder(d[[i]],names(d[[1]]))
  }
  
  d <- rbindlist(d)
  d[, value := gsub(" ", "", value)]
  d[, value := gsub(",", ".", value)]
  d[, value := gsub(">", "", value)]
  d[, value := gsub("<", "", value)]
  d[, value := as.numeric(value)]
  d <- d[!is.na(value)]
  unique(d[type=="Online"]$variable)
  unique(d$type)
  #d <- d[type(type=="Online" & )]
  d[is.na(units), units := "?"]
  d[is.na(type), type := "?"]
  d[is.na(waterType), waterType := "?"]
  d[is.na(point), point := "?"]
  
  d[units=="", units := "?"]
  d[type=="", type := "?"]
  d[waterType=="", waterType := "?"]
  d[point == "", point := "?"]
  
  d[variable == "pH", units := "pH"]
  d[variable == "Temperature", units := "C"]
  
  d[units %in% c("mg Pt/l", "mg/l Pt", "mg/lPt", "Milligram/l Pt"), units := "mg Pt/L"]
  d[units %in% c("/100ml", "ant/100ml", "CFU/100ml", "kde/100ml"), units := "/100mL"]
  d[units %in% c("/100 ml", "ant/100 ml"), units := "/100mL"]
  d[units %in% c("/ml", "ant/ml", "CFU/ml", "CFU/mL"), units := "/mL"]
  
  d[units=="myS/cm2 ", value:=value/10]
  d[units=="myS/cm2 ", units:="mS/m"]
  
  d <- d[,.(value=mean(value,na.rm=T)),by=.(date,point,units,type,variable,waterwork,waterType)]
  d[,waterwork:=paste0(waterwork,"||",point)]
  d[waterwork %in% c("HIAS_Hamar||101 HVBA Rentvann","HIAS_Hamar||HVBA Rentvann"),waterwork:="HIAS_Hamar||HVBA Rentvann"]
  d[waterwork %in% c("HIAS_Stange||401 Hemstad Rentvann","HIAS_Stange||Hemstad Rentvann"),waterwork:="HIAS_Stange||Hemstad Rentvann"]
  if(FALSE){
    #unique(d$x)
    unique(d$waterwork)
    setnames(d,"waterwork","temp")
    d[,waterwork:=""]
    d[temp=="Arendal_Rore",waterwork:="Rore"]
    d[temp=="Asker og Baerum vannverk IKS_Aurevann",waterwork:="Aurevann"]
    d[grep("Asker og Baerum vannverk IKS_Kattås",temp),waterwork:="Kattaas"]
    d[temp=="Bergen_Espeland",waterwork:="Espeland"]
    d[grep("FREVAR_Høyfjell",temp),waterwork:="Hoeyfjell"]
    d[temp=="Glitrevannverket IKS_Kleivdammen",waterwork:="Kleivdammen"]
    d[temp=="Glitrevannverket IKS_Landfall",waterwork:="Landfall"]
    d[temp=="Halden_Lille Erte",waterwork:="LilleErte"]
    d[temp=="HIAS_Hamar",waterwork:="Hamar"]
    d[temp=="HIAS_Stange",waterwork:="Stange"]
    d[grep("Karmøy_Brekke",temp),waterwork:="Brekke"]
    d[temp=="Kongsvinger_Granli_GIVAS",waterwork:="Granli"]
    d[temp=="Kristiansand_Rossevann",waterwork:="Rossevann"]
    d[temp=="Kristiansand_Tronstadvann",waterwork:="Tronstadvann"]
    d[temp=="Lillehammer",waterwork:="Lillehammer"]
    d[grep("MOVAR_Vansjø",temp),waterwork:="Vansjoe"]
    d[temp=="Nedre Romerike Vannverk IKS_Hauglifjell",waterwork:="Hauglifjell"]
    d[temp=="Oslo_Oset",waterwork:="Oset"]
    d[grep("Tromsø_Kvaløya",temp),waterwork:="Kvaloeya"]
    d[grep("Tromsø_Simavik",temp),waterwork:="Simavik"]
    d[temp=="Trondheim_Vikelvdalen",waterwork:="Vikelvdalen"]
    d[temp=="Univann_Sjunken",waterwork:="Sjunken"]
    d[temp=="Vestfold Vann IKS_Eidsfoss",waterwork:="Eidsfoss"]
    d[temp=="Vestfold Vann IKS_Seierstad",waterwork:="Seierstad"]
    
    d[,waterworkpoint:=paste0(waterwork,"/",point)]
    sort(unique(d$waterworkpoint))
  }
  
  d[,year := format.Date(date,"%G")] #Week-based year, instead of normal year (%Y)
  d[,week := as.numeric(format.Date(date,"%V"))]
  d[,month:=as.numeric(strftime(date, "%m"))]
  
  d <- d[,.(value=mean(value,na.rm=T),month=floor(month)),by=.(year,week,point,units,type,variable,waterwork,waterType)]
  d[,season:=1]
  d[month %in% 3:5,season:=2]
  d[month %in% 6:8,season:=3]
  d[month %in% 9:11,season:=4]
  Encoding(d$waterwork) <- "UTF-8"
  nve <- CleanDataWP1NVE() #readRDS(file.path(org::PROJ$CLEAN,"wp1_nve_discharge.RDS"))
  setnames(nve,"waterwork","nve")
  stationNames <- data.table(readxl::read_excel(file.path(org::PROJ$RAW,"names_clean.xlsx"))) #WP1_MET_intakepoints/Kopi av Info_Vannverk.xlsx"))
  stationNames <- stationNames[!is.na(waterwork) & !is.na(nve) & !is.na(met),]
  stationNames <- unique(data.table(stationNames[,c("waterwork","nve"),with=F]))
  stationNames <- stationNames[!is.na(waterwork) & !is.na(nve)]
  Encoding(stationNames$waterwork) <- "UTF-8"
  # these are new waterworks
  unique(d[!waterwork %in% stationNames$waterwork]$waterwork)
  nrow(nve)
  length(unique(nve$waterwork))
  length(unique(nve$nve))
  nve <- merge(nve,stationNames,by="nve",allow.cartesian = TRUE)
  length(unique(nve$nve))
  length(unique(nve$waterwork))
  nrow(nve)
  
  met <- readRDS(file.path(org::PROJ$CLEAN,"wp1_met_rain.RDS"))
  
  RAWmisc::RecodeDT(met,
                    c(
                      "Alesund_Vasstrandlia||?"="Ålesund||Vasstrandlia",
                      "Arendal_Rore||01 Råvann Rore"="Arendal||02 Rentvann Hølen",
                      "Asker og Baerum vannverk IKS_Aurevann||?"="Asker og Bærum IKS||Aurevann",
                      "Asker og Baerum vannverk IKS_Kattås||?"="Asker og Bærum IKS||Holsfjorden",
                      "Bergen_Espeland||Espeland, rå"="Bergen_Espeland||Espeland",
                      "Bergen_Espeland||Jordalsvatnet, rå"="Bergen_Jordalsvatnet||Jordalsvatnet",
                      "Bergen_Espeland||Kismul, rå"="Bergen_Kismul||Kismul",
                      "Bergen_Espeland||Sædalen, rå" ="Bergen_Saedalen||Saedalen",
                      "Bergen_Espeland||Svartediket"="Bergen_Svartediket||Svartediket",
                      "Glitrevannverket IKS_Kleivdammen||Kleivdammen"="Glitrevannverket||Kleivdammen",
                      "Glitrevannverket IKS_Landfall||Landfall"="Glitrevannverket||Landfall",
                      "Halden_Lille Erte||?"="Halden||Lille_Erte",
                      "HIAS_Hamar||?"="HIAS_Hamar||HVBA Rentvann",
                      "HIAS_Stange||?"="HIAS_Stange||Hemstad Rentvann",
                      "IVAR IKS_Langevatn||Storevatn"="IVAR IKS||Langevatn",
                      "Karmøy_Brekke||?"="Karmøy||Brekke",
                      "Kongsvinger_Granli_GIVAS||01 Granli Råvann brønn 1"="Kongsvinger||Granli",
                      "Kristiansand_Rossevann||Rossevann - Råvann"="Kristiansand||Rossevann",
                      "Kristiansand_Tronstadvann||TIV - Råvann"="Kristiansand||Tronstadvann",
                      "Lillehammer||LILLEHAMMER-KORGEN-RÅVANN"="Lillehammer||Lillehammer",
                      "MOVAR_Vansjø||?"="Movar||Vansjø",
                      "Oppegard||?"="Oppegård||?",
                      "Oslo_Oset||Oset"="Oslo||Oset",
                      "Oslo_Oset||Skullerud" ="Oslo||Skullerud",
                      "Sarpsborg_Baterod||Glomma"="Sarpsborg_Baterød||?",
                      "Tromsø_Simavik||0101 Råvann Langvann"="Tromsø_Kvaløya||0103 Simavik Rentvann",
                      "Tromsø_Kvaløya||0201 Kvaløya Råvann"="Tromsø_Kvaløya||0202 Kvaløya Rentvann",
                      "Trondheim_Vikelvdalen||?"="Trondheim||Vikelvdalen",
                      "Univann_Sjunken||?"="Univann_Sjunken||117 UNIVANN Sjunken Rentvann",
                      "Vestfold Vann IKS_Eidsfoss||?"="Vestfold||Eidsfoss",
                      "Vestfold Vann IKS_Seierstad||?"="Vestfold||Seierstad"
                    )
                    ,"waterwork")
  
  length(unique(d$waterwork))
  d <- na.omit(d)
  d1 <- merge(d,nve,by=c("year","week","waterwork"),allow.cartesian = TRUE)
  length(unique(d1$waterwork))
  d2 <- merge(d1,met,all.x=TRUE,by=c("year","week","waterwork"))
  length(unique(d2$waterwork))
  
  dim(d)
  dim(d1)
  dim(d2)
  
  unique(d$waterwork)[!unique(d$waterwork) %in% unique(d1$waterwork)]
  unique(d$waterwork)[!unique(d$waterwork) %in% unique(d2$waterwork)]
  x <- unique(d[,c("waterwork"),with=F])
  x[,waterworkNum:=""]
  if(!file.exists(file.path(org::PROJ$RAW,"WP1_MET_intakepoints/codes.csv"))) write.table(x,file=file.path(org::PROJ$RAW,"WP1_MET_intakepoints/codes.csv"),sep=";",row.names=F)
  
  d2[type %in% c("Accredited","External"),type:="Accredited"]
  d2[,type:=factor(type,levels=c("Accredited","Internal","Online","?"))]
  
  d2[,id:=waterwork]
  #unique(d$id)[order(unique(d$id))]
  #unique(d$waterwork)[order(unique(d$waterwork))]
  
  d2[,watersource:="Surface water"]
  d2[nve %in% c("Lillehammer","Granli", "Elvestrand"),watersource:="Groundwater"]
  d2[nve %in% c("Hauglifjell","Bateroed"),watersource:="River"]
  #xtabs( ~d$watersource)
  
  #unique(d$variable)[order(unique(d$variable))]
  d2[variable %in% c("Colony count at 22 degrees","Colony count at 37 degrees"),variable:="Colony count"]
  d2[variable %in% c("pH at 19-25 degrees"),variable:="pH"]
  d2[variable %in% c("Coliform Bacteria","Coliform bacteria at 37 degrees"),variable:="Coliform bacteria"]
  d2[variable %in% c("Hazen","Colour (after filtering)"),variable:="Colour"]
  
  d2 <- d2[!variable %in% c("Clostridium Perfringens","TOC","Temperature")]
  dim(d2)
  d2 <- d2[,.(
    value=mean(value,na.rm=T),
    wp950_discharge0_0=mean(wp950_discharge0_0,na.rm=T),
    wp950_discharge1_1=mean(wp950_discharge1_1,na.rm=T),
    wp950_discharge2_2=mean(wp950_discharge2_2,na.rm=T),
    wp950_discharge3_3=mean(wp950_discharge3_3,na.rm=T),
    wp950_discharge4_4=mean(wp950_discharge4_4,na.rm=T),
    wp950_rain0_0=mean(wp950_rain0_0,na.rm=T),
    wp950_rain1_1=mean(wp950_rain1_1,na.rm=T),
    wp950_rain2_2=mean(wp950_rain2_2,na.rm=T),
    wp950_rain3_3=mean(wp950_rain3_3,na.rm=T),
    wp950_rain4_4=mean(wp950_rain4_4,na.rm=T),
    wp950_precip0_0=mean(wp950_precip0_0,na.rm=T),
    wp950_precip1_1=mean(wp950_precip1_1,na.rm=T),
    wp950_precip2_2=mean(wp950_precip2_2,na.rm=T),
    wp950_precip3_3=mean(wp950_precip3_3,na.rm=T),
    wp950_precip4_4=mean(wp950_precip4_4,na.rm=T),
    wp950_temperature0_0=mean(wp950_temperature0_0,na.rm=T),
    wp950_temperature1_1=mean(wp950_temperature1_1,na.rm=T),
    wp950_temperature2_2=mean(wp950_temperature2_2,na.rm=T),
    wp950_temperature3_3=mean(wp950_temperature3_3,na.rm=T),
    wp950_temperature4_4=mean(wp950_temperature4_4,na.rm=T),
    wp950_gridRain0_0=mean(wp950_gridRain0_0,na.rm=T),
    wp950_gridRain1_1=mean(wp950_gridRain1_1,na.rm=T),
    wp950_gridRain2_2=mean(wp950_gridRain2_2,na.rm=T),
    wp950_gridRain3_3=mean(wp950_gridRain3_3,na.rm=T),
    wp950_gridRain4_4=mean(wp950_gridRain4_4,na.rm=T),
    wp950_gridPrecip0_0=mean(wp950_gridPrecip0_0,na.rm=T),
    wp950_gridPrecip1_1=mean(wp950_gridPrecip1_1,na.rm=T),
    wp950_gridPrecip2_2=mean(wp950_gridPrecip2_2,na.rm=T),
    wp950_gridPrecip3_3=mean(wp950_gridPrecip3_3,na.rm=T),
    wp950_gridPrecip4_4=mean(wp950_gridPrecip4_4,na.rm=T),
    wp950_gridRunoffStandardised0_0=mean(wp950_gridRunoffStandardised0_0,na.rm=T),
    wp950_gridRunoffStandardised1_1=mean(wp950_gridRunoffStandardised1_1,na.rm=T),
    wp950_gridRunoffStandardised2_2=mean(wp950_gridRunoffStandardised2_2,na.rm=T),
    wp950_gridRunoffStandardised3_3=mean(wp950_gridRunoffStandardised3_3,na.rm=T),
    wp950_gridRunoffStandardised4_4=mean(wp950_gridRunoffStandardised4_4,na.rm=T),
    c_discharge0_0=mean(c_discharge0_0,na.rm=T),
    c_discharge1_1=mean(c_discharge1_1,na.rm=T),
    c_discharge2_2=mean(c_discharge2_2,na.rm=T),
    c_discharge3_3=mean(c_discharge3_3,na.rm=T),
    c_discharge4_4=mean(c_discharge4_4,na.rm=T),
    c_rain0_0=mean(c_rain0_0,na.rm=T),
    c_rain1_1=mean(c_rain1_1,na.rm=T),
    c_rain2_2=mean(c_rain2_2,na.rm=T),
    c_rain3_3=mean(c_rain3_3,na.rm=T),
    c_rain4_4=mean(c_rain4_4,na.rm=T),
    c_precip0_0=mean(c_precip0_0,na.rm=T),
    c_precip1_1=mean(c_precip1_1,na.rm=T),
    c_precip2_2=mean(c_precip2_2,na.rm=T),
    c_precip3_3=mean(c_precip3_3,na.rm=T),
    c_precip4_4=mean(c_precip4_4,na.rm=T),
    c_temperature0_0=mean(c_temperature0_0,na.rm=T),
    c_temperature1_1=mean(c_temperature1_1,na.rm=T),
    c_temperature2_2=mean(c_temperature2_2,na.rm=T),
    c_temperature3_3=mean(c_temperature3_3,na.rm=T),
    c_temperature4_4=mean(c_temperature4_4,na.rm=T),
    c_gridRain0_0=mean(c_gridRain0_0,na.rm=T),
    c_gridRain1_1=mean(c_gridRain1_1,na.rm=T),
    c_gridRain2_2=mean(c_gridRain2_2,na.rm=T),
    c_gridRain3_3=mean(c_gridRain3_3,na.rm=T),
    c_gridRain4_4=mean(c_gridRain4_4,na.rm=T),
    c_gridPrecip0_0=mean(c_gridPrecip0_0,na.rm=T),
    c_gridPrecip1_1=mean(c_gridPrecip1_1,na.rm=T),
    c_gridPrecip2_2=mean(c_gridPrecip2_2,na.rm=T),
    c_gridPrecip3_3=mean(c_gridPrecip3_3,na.rm=T),
    c_gridPrecip4_4=mean(c_gridPrecip4_4,na.rm=T),
    c_gridRunoffStandardised0_0=mean(c_gridRunoffStandardised0_0,na.rm=T),
    c_gridRunoffStandardised1_1=mean(c_gridRunoffStandardised1_1,na.rm=T),
    c_gridRunoffStandardised2_2=mean(c_gridRunoffStandardised2_2,na.rm=T),
    c_gridRunoffStandardised3_3=mean(c_gridRunoffStandardised3_3,na.rm=T),
    c_gridRunoffStandardised4_4=mean(c_gridRunoffStandardised4_4,na.rm=T)
  ), by=.(
    year,
    week,
    waterwork,
    point,
    units,
    type,
    variable,
    waterType,
    month,
    season,
    id,
    watersource
  )]
  dim(d2)
  d2[,id:=gsub("/","___",id)]
  d2 <- d2[!(year==2002 & week==7 & waterwork=="Nedre Romerike Vannverk IKS_Hauglifjell||?")]
  d2 <- d2[!(year==2003 & week==4 & waterwork=="Nedre Romerike Vannverk IKS_Hauglifjell||?")]
  d2 <- d2[!(year==2006 & week==16 & waterwork=="Nedre Romerike Vannverk IKS_Hauglifjell||?")]
  saveRDS(d2,file.path(org::PROJ$CLEAN,"WP1_clean.RDS"))
  
}









