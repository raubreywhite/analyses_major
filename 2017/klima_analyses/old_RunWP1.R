suppressMessages(library(ggplot2))
RAWmisc::InitialiseProject(
  HOME = "/git/code_major/2017/klima_analyses/",
  RAW = "/analyses/data_raw/code_major/2017/klima_analyses/",
  CLEAN = "/analyses/data_clean/code_major/2017/klima_analyses",
  BAKED = "/analyses/results_baked/code_major/2017/klima_analyses/",
  FINAL = "/analyses/results_final/code_major/2017/klima_analyses/",
  SHARED = "/dropbox/results_shared/code_major/2017/klima_analyses/")

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

for(type in c("raw","clean")){
  if(type=="raw"){
    d <- WP1DataRaw()
  } else {
    d <- WP1DataClean()
  }
  dlog <- copy(d)
  dlog[,valuelog:=value]
  dlog[variable!="pH",valuelog:=log(1+valuelog)]
  
  for(od in outputDirs){
    pdf(file.path(od,sprintf("WP1_waterworks_variables_%s.pdf",type)))
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
      pdf(file.path(od,sprintf("WP1_waterworks_scatterplot_%s_%s.pdf",type,j)))
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
    pdf(file.path(od,sprintf("WP1_waterworks_%s.pdf",type)))
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
  openxlsx::write.xlsx(plotData,file=file.path(RPROJ$PROJSHARED,lubridate::today(),"WP1",sprintf("WP1_waterworks_%s.xlsx",type)))
  
  for(od in outputDirs){
    pdf(file.path(od,sprintf("WP1_descriptives_%s.pdf",type)),width=12,height=8)
    print(WP1GraphExtremeBySeason(d))
    print(WP1GraphExtremeByYear(d))
    dev.off()
  }

  ## ACCREDITED INTERNAL
  if(RUN_ALL) unlink(file.path(RPROJ$PROJBAKED,sprintf("WP1_res_%s.RDS",type)))
  bake(file.path(RPROJ$PROJBAKED,sprintf("WP1_res_%s.RDS",type)),{
    readRDS(file.path(RPROJ$PROJCLEAN,sprintf("WP1_%s.RDS",type))) -> d
    d <- d[type %in% c("Accredited","Internal")]
    WP1Analyses(d)
  }) -> res
  
  for(od in outputDirs){
    openxlsx::write.xlsx(MakeTableWP1(res),file.path(od,sprintf("WP1_%s.xlsx",type)))
    pdf(file.path(od,sprintf("WP1_%s.pdf",type)),width=12,height=16)
    print(PlotDetailedGridWP1(p=res[var=="Cont" & id=="All"],r2=TRUE))
    #print(PlotDetailedGridWP1(p=res[var=="Cont" & id=="All"],days=TRUE))
    #print(PlotDetailedGridWP1(p=res[var=="Cont" & id=="All"],days=FALSE))
    dev.off()
  }
}



d <- WP1DataClean()

## ACCREDITED INTERNAL
if(RUN_ALL) unlink(file.path(RPROJ$PROJBAKED,"WP1_res_accreditedinternal_clean.RDS"))
bake(file.path(RPROJ$PROJBAKED,"WP1_res_accreditedinternal_clean.RDS"),{
  readRDS(file.path(RPROJ$PROJCLEAN,"WP1_clean.RDS")) -> d
  d <- d[type %in% c("Accredited","Internal")]
  WP1Analyses(d)
}) -> res

for(od in outputDirs){
  openxlsx::write.xlsx(MakeTableWP1(res),file.path(od,"WP1_clean.xlsx"))
  pdf(file.path(od,"WP1_clean.pdf"),width=12,height=16)
  print(PlotDetailedGridWP1(p=res[var=="Cont" & id=="All"],r2=TRUE))
  #print(PlotDetailedGridWP1(p=res[var=="Cont" & id=="All"],days=TRUE))
  #print(PlotDetailedGridWP1(p=res[var=="Cont" & id=="All"],days=FALSE))
  dev.off()
}
