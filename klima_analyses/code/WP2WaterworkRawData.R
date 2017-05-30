WP2WaterworkRawData <- function(){
  d1 <- readRDS(file.path(RAWmisc::PROJ$CLEAN,"WP1.RDS"))
  d2 <- readRDS(file.path(RAWmisc::PROJ$CLEAN,"WP2.RDS"))$data
  
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
  return(d)
}