QuickFix1 <- function(x){
  setnames(x,c("season","days","num"))
  x[,season:=as.factor(season)]
  setattr(x$season,"levels",c("Winter","Spring","Summer","Autumn"))
  x[,N:=sum(num),by=season]
  x[,perc:=num/N*100]
  setorder(x,-days)
}

WP1GraphExtremeBySeason <- function(d){
  rain <- na.omit(d[,.(num=.N),by=.(season,wp950_rain0_0)])
  discharge <- na.omit(d[,.(num=.N),by=.(season,wp950_discharge0_0)])
  precip <- na.omit(d[,.(num=.N),by=.(season,wp950_precip0_0)])
  
  
  QuickFix1(rain)
  QuickFix1(discharge)
  QuickFix1(precip)
  rain[,type:="Rain"]
  discharge[,type:="Discharge"]
  precip[,type:="Precip"]
  pd <- rbind(rain,discharge,precip)
  
  q <- ggplot(pd,aes(x=season,fill=factor(days)))
  q <- q + geom_bar(aes(y=perc),stat="identity")
  q <- q + facet_wrap(~type)
  q <- q + scale_fill_brewer("# Extreme days/wk",palette="Set2")
  return(q)
}

QuickFix2 <- function(x){
  setnames(x,c("year","days","num"))
  x[,N:=sum(num),by=year]
  x[,perc:=num/N*100]
  setorder(x,-days)
}

WP1GraphExtremeByYear <- function(d){
  ## year vs year
  rain <- na.omit(d[,.(num=.N),by=.(year,wp950_rain0_0)])
  discharge <- na.omit(d[,.(num=.N),by=.(year,wp950_discharge0_0)])
  precip <- na.omit(d[,.(num=.N),by=.(year,wp950_precip0_0)])
  
  QuickFix2(rain)
  QuickFix2(discharge)
  QuickFix2(precip)
  rain[,type:="Rain"]
  discharge[,type:="Discharge"]
  precip[,type:="Precip"]
  pd <- rbind(rain,discharge,precip)
  
  q <- ggplot(pd,aes(x=as.numeric(year),fill=factor(days)))
  q <- q + geom_bar(aes(y=perc),stat="identity")
  q <- q + facet_wrap(~type)
  q <- q + scale_fill_brewer("# Extreme days/wk",palette="Set2")
  return(q)
}