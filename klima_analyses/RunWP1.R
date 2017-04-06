suppressWarnings(suppressMessages(library(ggplot2)))
RAWmisc::InitialiseProject(
  PROJHOME = "/analyses/code_major/klima_analyses/",
  PROJRAW = "/dropbox/data_raw/klima_analyses/",
  PROJCLEAN = "/analyses/data_clean/klima_analyses",
  PROJBAKED = "/analyses/results_baked/klima_analyses/",
  PROJFINAL = "/analyses/results_final/klima_analyses/",
  PROJSHARED = "/dropbox/results_shared/klima_analyses/")

suppressWarnings(suppressMessages(library(data.table)))
suppressWarnings(suppressMessages(library(ggplot2)))
suppressWarnings(suppressMessages(library(foreach)))
suppressWarnings(suppressMessages(library(pomp)))
suppressMessages(library(doParallel))
registerDoParallel()
assign("RUN_ALL", TRUE, envir=globalenv())

outputDirs <- c(
  file.path(RPROJ$PROJFINAL,lubridate::today(),"WP1"),
  file.path(RPROJ$PROJSHARED,lubridate::today(),"WP1")
)
importantDirs <- c(
  file.path(RPROJ$PROJCLEAN,"WP1_waterworks"),
  file.path(RPROJ$PROJCLEAN,"WP1_waterworks_clean_water"),
  outputDirs
)
for(i in importantDirs) if(!dir.exists(i)) dir.create(i,recursive=TRUE)

d <- WP1Data()
dlog <- copy(d)
dlog[,valuelog:=value]
dlog[variable!="pH",valuelog:=log(1+valuelog)]

for(od in outputDirs){
  pdf(file.path(od,"WP1_waterworks_variables.pdf"))
  q <- ggplot(d, aes(x = value))
  q <- q + geom_histogram(size=3)
  q <- q + facet_wrap(~variable,scales="free")
  print(q)
  
  q <- ggplot(d, aes(x = variable, y=value))
  q <- q + geom_boxplot()
  q <- q + facet_wrap(~variable,scales="free")
  print(q)
  dev.off()
  
  for(j in c("c_discharge0_0","c_rain0_0","c_precip0_0","c_gridRain0_0","c_gridPrecip0_0","c_gridRunoffStandardised0_0")){
    pdf(file.path(od,sprintf("WP1_waterworks_scatterplot_%s.pdf",j)))
      for(i in unique(dlog$waterwork)){  
        try({
        q <- ggplot(dlog[waterwork==i], aes_string(x = j, y="value"))
        q <- q + geom_point(size=1)
        q <- q + stat_smooth(method="lm",se=FALSE,lwd=2,colour="red")
        q <- q + facet_wrap(~variable,scales="free")
        q <- q + labs(title=sprintf("Normal scale: %s",i))
        print(q)
        
        q <- ggplot(dlog[waterwork==i], aes_string(x = j, y="valuelog"))
        q <- q + geom_point(size=1)
        q <- q + stat_smooth(method="lm",se=FALSE,lwd=2,colour="red")
        q <- q + facet_wrap(~variable,scales="free")
        q <- q + labs(title=sprintf("Log scale: %s",i))
        print(q)
        },TRUE)
      }
    dev.off()
  }
}

plotData <- d[, .(meanValue = mean(value,na.rm=T),medianValue = median(value,na.rm=T), minValue = min(value,na.rm=T), maxValue = max(value,na.rm=T),
                  dmin = min(year), dmax = max(year)), by = .(variable, type, units, waterType, point, waterwork)]


plotData <- d[, .(meanValue = mean(value,na.rm=T),medianValue = median(value,na.rm=T), minValue = min(value,na.rm=T), maxValue = max(value,na.rm=T),
                   dmin = min(year), dmax = max(year)), by = .(variable, type, units, waterType, point, waterwork)]

for(od in outputDirs){
  pdf(file.path(od,"WP1_waterworks.pdf"))
  for (i in unique(plotData$variable)) {
    q <- ggplot(plotData[variable==i], aes(y = waterwork, x = meanValue, shape = units, colour=type))
    q <- q + geom_point(size=3)
    q <- q + scale_color_brewer(palette="Set2")
    q <- q + labs(title=i)
    print(q)
  }
  dev.off()
}

setcolorder(plotData,c("waterwork","point","type","waterType","variable", "units", "dmin", "dmax","meanValue","medianValue","minValue","maxValue"))
setorder(plotData,waterwork, point, type, waterType, variable)
openxlsx::write.xlsx(plotData,file=file.path(RPROJ$PROJSHARED,lubridate::today(),"WP1","WP1_waterworks.xlsx"))

descript <- d[,.(
  valueMean=mean(value)
),by=.(variable,units,waterwork)]
setorder(descript,variable,units,waterwork)
openxlsx::write.xlsx(descript, file = file.path(RPROJ$PROJSHARED,lubridate::today(),"WP1","units.xlsx"))

for(od in outputDirs){
  pdf(file.path(od,"WP1_descriptives.pdf"),width=12,height=8)
  print(WP1GraphExtremeBySeason(d))
  print(WP1GraphExtremeByYear(d))
  dev.off()
}
## ALL
fitData <- d[variable=="Colour"]
fit <- lm(value~c_discharge0_0,data=fitData)
summary(fit)
fit <- lm(value~factor(waterwork),data=fitData)
summary(fit)
fit <- lm(value~c_discharge0_0 + factor(waterwork),data=fitData)
summary(fit)
fit <- lm(value~c_discharge1_1 + factor(waterwork) + factor(month) + as.numeric(year),data=fitData)
summary(fit)

## ACCREDITED INTERNAL
if(RUN_ALL) unlink(file.path(RPROJ$PROJBAKED,"WP1_res_accreditedinternal.RDS"))
bake(file.path(RPROJ$PROJBAKED,"WP1_res_accreditedinternal.RDS"),{
  readRDS(file.path(RPROJ$PROJCLEAN,"WP1.RDS")) -> d
  d <- d[type %in% c("Accredited","Internal")]
  WP1Analyses(d)
}) -> res

for(od in outputDirs){
  openxlsx::write.xlsx(MakeTableWP1(res),file.path(od,"WP1_continuous_accreditedinternal.xlsx"))
  pdf(file.path(od,"WP1_continuous_accreditedinternal.pdf"),width=12,height=16)
  print(PlotDetailedGridWP1(p=res[var=="Cont" & id=="All"],r2=TRUE))
  #print(PlotDetailedGridWP1(p=res[var=="Cont" & id=="All"],days=TRUE))
  #print(PlotDetailedGridWP1(p=res[var=="Cont" & id=="All"],days=FALSE))
  dev.off()
}

## ALL
if(RUN_ALL) unlink(file.path(RPROJ$PROJBAKED,"WP1_res_all.RDS"))
bake(file.path(RPROJ$PROJBAKED,"WP1_res_all.RDS"),{
  readRDS(file.path(RPROJ$PROJCLEAN,"WP1.RDS")) -> d
  WP1Analyses(d)
}) -> res



for(od in outputDirs){
  openxlsx::write.xlsx(MakeTableWP1(res),file.path(od,"WP1_continuous_all.xlsx"))
  pdf(file.path(od,"WP1_continuous_all.pdf"),width=12,height=16)
  print(PlotDetailedGridWP1(p=res[var=="Cont" & id=="All"],r2=TRUE))
  #print(PlotDetailedGridWP1(p=res[var=="Cont" & id=="All"],days=TRUE))
  #print(PlotDetailedGridWP1(p=res[var=="Cont" & id=="All"],days=FALSE))
  dev.off()
}



## ONLINE
if(RUN_ALL) unlink(file.path(RPROJ$PROJBAKED,"WP1_res_online.RDS"))
bake(file.path(RPROJ$PROJBAKED,"WP1_res_online.RDS"),{
  readRDS(file.path(RPROJ$PROJCLEAN,"WP1.RDS")) -> d
  d <- d[type %in% c("Online")]
  WP1Analyses(d)
}) -> res

for(od in outputDirs){
  pdf(file.path(od,"WP1_continuous_online.pdf"),width=12,height=16)
  print(PlotDetailedGridWP1(p=res[var=="Cont" & id=="All"],r2=TRUE))
  #print(PlotDetailedGridWP1(p=res[var=="Cont" & id=="All"],days=TRUE))
  #print(PlotDetailedGridWP1(p=res[var=="Cont" & id=="All"],days=FALSE))
  dev.off()
}






































pdf(file.path("results_final","WP1",paste0("WP1_continuous_waterworks.pdf")),width=12,height=12)
for(i in unique(res$id)){
  p <- res[var=="Cont" & id==i]

    setorder(p,season)
  x <- p[1:5]
  x[1,season:="All"]
  x[2,season:="Winter"]
  x[3,season:="Spring"]
  x[4,season:="Summer"]
  x[5,season:="Autumn"]
  x[1:5,n:=0]
  x <- rbind(x,p)
  l <- x[,.(n=mean(n)),by=season]
  l[,season:=paste0(season," (n=",round(n),")\n")]
  levels(p$season) <- l$season
  
  x <- copy(p)
  x[1,display:="Increases"]
  x[2,display:="None"]
  x[3,display:="Decreases"]
  
  q <- ggplot(p,aes(x=lag,y=stub,fill=dir))
  q <- q + geom_tile(data=x,alpha=0)
  q <- q + geom_tile(alpha=0.6,colour="white",lwd=0.2)
  q <- q + facet_grid(outcome~season)
  q <- q + scale_x_discrete("\nWeeks lag",drop=F)
  q <- q + scale_y_discrete("",drop=F)
  q <- q + scale_fill_manual(values=c("Red","Black","Green"),drop=F)
  q <- q + RAWmisc::theme_SMAO(10)
  q <- q + labs(title=paste0(i,"\n"))
  q <- q + theme(axis.line.y = NULL)
  q <- q + theme(axis.line.x = NULL)
  q <- q + theme(axis.ticks.length = unit(0,"lines"))
  q <- q + theme(axis.text.x = element_text(vjust=0.5))
  q <- q + theme(axis.text.y = element_text(hjust=1))
  q <- q + theme(axis.text = element_text(margin = rep(unit(1,"lines"),4)))
  print(q)
}
dev.off()

warning("look at days lag between extreme events for runoff, precip, and rain for each waterwork")

