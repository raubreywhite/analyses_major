suppressWarnings(suppressMessages(library(ggplot2)))
RAWmisc::InitialiseProject(
  HOME = "/analyses/code_major/klima_analyses/",
  RAW = "/dropbox/data_raw/klima_analyses/",
  CLEAN = "/analyses/data_clean/klima_analyses",
  BAKED = "/analyses/results_baked/klima_analyses/",
  FINAL = "/analyses/results_final/klima_analyses/",
  SHARED = "/dropbox/results_shared/klima_analyses/")

suppressWarnings(suppressMessages(library(data.table)))
suppressWarnings(suppressMessages(library(ggplot2)))
suppressWarnings(suppressMessages(library(foreach)))
suppressWarnings(suppressMessages(library(pomp)))
suppressMessages(library(doParallel))
registerDoParallel()
assign("RUN_ALL", TRUE, envir=globalenv())

outputDirs <- c(
  file.path(RAWmisc::PROJ$FINAL,lubridate::today(),"WP1"),
  file.path(RAWmisc::PROJ$SHARED_TODAY,"WP1")
)
importantDirs <- c(
  file.path(RAWmisc::PROJ$CLEAN,"WP1_waterworks"),
  file.path(RAWmisc::PROJ$CLEAN,"WP1_waterworks_clean_water"),
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
  openxlsx::write.xlsx(plotData,file=file.path(RAWmisc::PROJ$SHARED_TODAY,"WP1",sprintf("WP1_waterworks_%s.xlsx",type)))
  
  for(od in outputDirs){
    pdf(file.path(od,sprintf("WP1_descriptives_%s.pdf",type)),width=12,height=8)
    print(WP1GraphExtremeBySeason(d))
    print(WP1GraphExtremeByYear(d))
    dev.off()
  }

}

d <- WP1DataRaw()

## ACCREDITED INTERNAL
if(RUN_ALL) unlink(file.path(RAWmisc::PROJ$BAKED,"WP1_res_accreditedinternal_raw.RDS"))
bake(file.path(RAWmisc::PROJ$BAKED,"WP1_res_accreditedinternal_raw.RDS"),{
  readRDS(file.path(RAWmisc::PROJ$CLEAN,"WP1_raw.RDS")) -> d
  d <- d[type %in% c("Accredited","Internal")]
  WP1Analyses(d)
}) -> res

for(od in outputDirs){
  openxlsx::write.xlsx(MakeTableWP1(res),file.path(od,"WP1_raw.xlsx"))
  pdf(file.path(od,"WP1_raw.pdf"),width=12,height=16)
  print(PlotDetailedGridWP1(p=res[var=="Cont" & id=="All"],r2=TRUE))
  dev.off()
}



d <- WP1DataClean()

## ACCREDITED INTERNAL
if(RUN_ALL) unlink(file.path(RAWmisc::PROJ$BAKED,"WP1_res_accreditedinternal_clean.RDS"))
bake(file.path(RAWmisc::PROJ$BAKED,"WP1_res_accreditedinternal_clean.RDS"),{
  readRDS(file.path(RAWmisc::PROJ$CLEAN,"WP1_clean.RDS")) -> d
  d <- d[type %in% c("Accredited","Internal")]
  WP1Analyses(d)
}) -> res

for(od in outputDirs){
  openxlsx::write.xlsx(MakeTableWP1(res),file.path(od,"WP1_clean.xlsx"))
  pdf(file.path(od,"WP1_clean.pdf"),width=12,height=16)
  print(PlotDetailedGridWP1(p=res[var=="Cont" & id=="All"],r2=TRUE))
  dev.off()
}


########### WP2 METEROLOGICAL+HYDROLOGICAL
WP2Data()

if(RUN_ALL) unlink(file.path(RAWmisc::PROJ$CLEAN,"data.RDS"))
bake(file.path(RAWmisc::PROJ$CLEAN,"data.RDS"),{
  CleanData()
}) -> d

if(RUN_ALL) unlink(file.path(RAWmisc::PROJ$BAKED,"WP2_res.RDS"))
bake(file.path(RAWmisc::PROJ$BAKED,"WP2_res.RDS"),{
  WP2Analyses(d,ExtractValues=ExtractValues)
}) -> all


######### RESULTS

all[,varOfInterest:=gsub("^cat_","",varOfInterest)]
all[var=="",var:="Cont"]
all[,varOfInterest:=stringr::str_replace(varOfInterest,"[0-9]_[0-9]$","")]

plotData <- copy(all)

plotData[,dec:="None"]
plotData[pval < 0.05 & est<0,dec:="Protective"]
plotData[pval < 0.05 & est > 0, dec := "Harmful"]

plotData[, N := .N, by=water]
plotData[, lag:=factor(lag)]
plotData[, cutoff := stringr::str_sub(varOfInterest, 1, 5)]
plotData[, varOfInterest := gsub("^wp950_", "", varOfInterest)]
plotData[, varOfInterest := gsub("^wp990_", "", varOfInterest)]
plotData[, dec := factor(dec, levels=c("Harmful","None","Protective"))]
plotData[, season := factor(season, levels=c("All","Autumn","Winter","Spring","Summer"))]
levels(plotData$season) <- c("All seasons","Autumn","Winter","Spring","Summer")
plotData[, varOfInterest := factor(varOfInterest,levels=c("c_rain","a_precipCorr","a_runoff"))]
levels(plotData$varOfInterest) <- c("Rain\n(Municip. Centre)","Corrected precipication\n(Municip. Average)","Runoff\n(Municip. Average)")
plotData[,water:=factor(water,levels=c("All","Under 10k","10k+","Under 500","50%+"))]
plotData[,age:=factor(age,levels=c("Totalt","0-4","5-14","15-64","65+"))]
levels(plotData$age) <- c("All ages","0-4 years old","5-14 years old","15-64 years old","65+ years old")

data <- vector("list", 5)
plots <- vector("list", 5)

data[[1]] <- plotData[water=="All"]
data[[2]] <- plotData[water=="Under 10k"]
data[[3]] <- plotData[water=="10k+"]
data[[4]] <- plotData[water=="Under 500"]
data[[5]] <- plotData[water=="50%+"]
pdf(file.path(RAWmisc::PROJ$SHARED_TODAY,"WP2",paste0("WP2_continuous.pdf")),width=12,height=6.5)
for (i in 1:5) {
  data[[i]][pval * N > 0.05, dec:="None"]
  q <- ggplot(data[[i]], aes(x = lag, y = varOfInterest, fill = dec))
  q <- q + geom_tile(data = data[[1]],alpha=0)
  q <- q + geom_tile(alpha=0.6,colour="white",lwd=0.2)
  q <- q + facet_grid(age ~ season)
  q <- q + scale_fill_manual(values=c("Red","Black","Green"),drop=F)
  q <- q + scale_x_discrete("Weeks lag")
  q <- q + scale_y_discrete("")
  q <- q + RAWmisc::theme_SMAO(base_size=12,v=3)
  if(i==1){
    q <- q + labs(title="All municipalities")
  } else if(i==2){
    q <- q + labs(title="Municipalities with average waterwork size under 10k")
  } else if(i==3){
    q <- q + labs(title="Municipalities with average waterwork size 10k+")
  } else if(i==4){
    q <- q + labs(title="Municipalities with average waterwork size under 500")
  } else if(i==5){
    q <- q + labs(title="Municipalities with 50%+ not having waterworks")
  }
  print(q)
}
dev.off()

### WP1.5 WITH RAW WATERWORKS

d <- WP2WaterworkRawData()

if(RUN_ALL) unlink("results_baked/WP2_waterwork_raw_res.RDS")
bake("results_baked/WP2_waterwork_raw_res.RDS",{
  WP2WaterworkRawAnalyses(d,ExtractValues=ExtractValues)
}) -> res

print(PlotWP2WaterworkRawDataAnalyses(res))

