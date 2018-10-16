suppressMessages(library(ggplot2))
if(.Platform$OS.type=="unix"){
  RAWmisc::UseRClone()
  RAWmisc::AllowFileManipulationFromInitialiseProject()
  
  if(dir.exists("/dropbox")){
    SHARED <- "/dropbox/analyses/results_shared/code_major/2017/klima_analyses/"
    RCLONE_SHARED <- NULL
  } else {
    SHARED <- "/tmp/results_shared/code_major/2017/klima_analyses/"
    RCLONE_SHARED <- "data:/analyses/results_shared/code_major/2017/klima_analyses/"
  }
  
  RAWmisc::InitialiseProject(
    HOME = "/git/code_major/2017/klima_analyses/",
    RAW = "/tmp/data_raw/code_major/2017/klima_analyses/",
    CLEAN = "/tmp/data_clean/code_major/2017/klima_analyses",
    BAKED = "/tmp/results_baked/code_major/2017/klima_analyses/",
    FINAL = "/tmp/results_final/code_major/2017/klima_analyses/",
    SHARED = SHARED,
    RCLONE_RAW = "crypt:/data_raw/code_major/2017/klima_analyses/",
    RCLONE_SHARED = RCLONE_SHARED
  )
}

dir.create(file.path(RAWmisc::PROJ$SHARED_TODAY,"waterwork_specific_predictions"))

Bin.int <- function(var,breaks){
  b1 <- findInterval(var,breaks)
  b2 <- breaks[b1+1]
  b1 <- breaks[b1]
  
  return((b1+b2)/2)
}
Bin <- Vectorize(Bin.int, vectorize.args = "var")

RepeatDataTable <- function(d, n) {
  if ("data.table" %in% class(d)) return(d[rep(seq_len(nrow(d)), n)])
  return(d[rep(seq_len(nrow(d)), n), ])
}

grid_arrange_shared_legend <- function(..., ncol = length(list(...)), nrow = 1, position = c("bottom", "right")) {
  
  plots <- list(...)
  position <- match.arg(position)
  g <- ggplotGrob(plots[[1]] + theme(legend.position = position))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  lwidth <- sum(legend$width)
  gl <- lapply(plots, function(x) x + theme(legend.position="none"))
  gl <- c(gl, ncol = ncol, nrow = nrow)
  
  combined <- switch(position,
                     "bottom" = arrangeGrob(do.call(arrangeGrob, gl),
                                            legend,
                                            ncol = 1,
                                            heights = unit.c(unit(1, "npc") - lheight, lheight)),
                     "right" = arrangeGrob(do.call(arrangeGrob, gl),
                                           legend,
                                           ncol = 2,
                                           widths = unit.c(unit(1, "npc") - lwidth, lwidth)))
  
  grid.newpage()
  grid.draw(combined)
  
  # return gtable invisibly
  invisible(combined)
  
}

suppressWarnings(suppressMessages(library(data.table)))
suppressWarnings(suppressMessages(library(ggplot2)))
suppressWarnings(suppressMessages(library(foreach)))
suppressWarnings(suppressMessages(library(pomp)))
suppressMessages(library(doParallel))
library(gridExtra)
library(grid)
registerDoParallel()
assign("RUN_ALL", TRUE, envir=globalenv())

importantDirs <- c(
  file.path(RAWmisc::PROJ$CLEAN,"WP1_waterworks"),
  file.path(RAWmisc::PROJ$CLEAN,"WP1_waterworks_clean_water")
)
for(i in importantDirs) if(!dir.exists(i)) dir.create(i,recursive=TRUE)

WP2Data()




mCOORDS <- fread(file.path(RAWmisc::PROJ$RAW,"FutureScenariosAndControlRuns","CoordsUTM.csv"))
setnames(mCOORDS,"WW.NO","met")
mNames <- data.table(readxl::read_excel(file.path(
  RAWmisc::PROJ$RAW,
  "names.xlsx"
)))
mNames[,met:=as.numeric(met)]
mNames <- mNames[!is.na(met)]

nrow(mNames)
mCOORDS <- merge(mCOORDS,mNames,by="met",all.y=T)
nrow(mCOORDS)
setorder(mCOORDS,nve)

mCOORDS <- unique(mCOORDS[,c("met","nve","waterwork","region")])
mCOORDS[,n:=1:.N,by=.(nve)]
mCOORDS <- mCOORDS[n==1]
nrow(mCOORDS)
length(unique(mCOORDS$met))
length(unique(mCOORDS$nve))

setnames(mCOORDS,"met","WW.NO")
RAWmisc::RecodeDT(mCOORDS,c("weat"="west"),"region")

temp <- list()
for(i in c(
  "CNRM-CM5_CCLM_prec_temp_rain_2006-2015.csv",
  "CNRM-CM5_CCLM_prec_temp_rain_2071-2100.csv",
  "CNRM-CM5_SMHI-RCA4_prec_temp_rain_2006-2015.csv",
  "CNRM-CM5_SMHI-RCA4_prec_temp_rain_2071-2100.csv",
  "EC-EARTH_CCLM_prec_temp_rain_2006-2015.csv",
  "EC-EARTH_CCLM_prec_temp_rain_2071-2100.csv",
  "EC-EARTH_DMI-HIRHAM5_prec_temp_rain_2006-2015.csv",
  "EC-EARTH_DMI-HIRHAM5_prec_temp_rain_2071-2100.csv",
  "EC-EARTH_KNMI-RACMO_prec_temp_rain_2006-2015.csv",
  "EC-EARTH_KNMI-RACMO_prec_temp_rain_2071-2100.csv",
  "EC-EARTH_SMHI-RCA4_prec_temp_rain_2006-2015.csv",
  "EC-EARTH_SMHI-RCA4_prec_temp_rain_2071-2100.csv",
  "HADGEM2_SMHI-RCA4_prec_temp_rain_2006-2015.csv",
  "HADGEM2_SMHI-RCA4_prec_temp_rain_2071-2100.csv",
  "IPSL-CM5A_SMHI-RCA4_prec_temp_rain_2006-2015.csv",
  "IPSL-CM5A_SMHI-RCA4_prec_temp_rain_2071-2100.csv",
  "MPI_CCLM_prec_temp_rain_2006-2015.csv",
  "MPI_CCLM_prec_temp_rain_2071-2100.csv")){
  print(i)
  md <- fread(file.path(RAWmisc::PROJ$RAW,"FutureScenariosAndControlRuns",i))
  md[,TEMP:=NULL]
  
  
  xtabs(~md$WW.NO)
  nrow(md)
  md <- merge(md,mCOORDS,by="WW.NO")
  nrow(md)
  xtabs(~md$nve+md$WW.NO)
  
  # RUNOFF
  runoffdata <- list()
  for(j in list.files(file.path(RAWmisc::PROJ$RAW,"Runoff_Proj",md$MODEL[1]))){
    tmp <- fread(file.path(RAWmisc::PROJ$RAW,"Runoff_Proj",md$MODEL[1],j))
    if(stringr::str_detect(j,"rcp45")){
      tmp[,SCENARIO:="rcp45"]
    } else {
      tmp[,SCENARIO:="rcp85"]
    }
    setnames(tmp,c("YEAR","MONTH","DAY","runoff","xxx","SCENARIO"))
    tmp[,nve:=stringr::str_extract(j,"^[a-zA-Z]*")]
    runoffdata[[j]] <- tmp
  }
  runoffdata <- rbindlist(runoffdata)
  nrow(md)
  md <- merge(md,runoffdata,by=c("SCENARIO","nve","YEAR","MONTH","DAY"))
  nrow(md)
  
  # TEMP
  tempdata <- list()
  for(j in list.files(file.path(RAWmisc::PROJ$RAW,"Tmax_Proj",md$MODEL[1]))){
    tmp <- fread(file.path(RAWmisc::PROJ$RAW,"Tmax_Proj",md$MODEL[1],j))
    if(stringr::str_detect(j,"rcp45")){
      tmp[,SCENARIO:="rcp45"]
    } else {
      tmp[,SCENARIO:="rcp85"]
    }
    setnames(tmp,c("YEAR","MONTH","DAY","TEMP","SCENARIO"))
    tmp[,nve:=stringr::str_extract(j,"^[a-zA-Z]*")]
    tempdata[[j]] <- tmp
  }
  tempdata <- rbindlist(tempdata)
  nrow(md)
  md <- merge(md,tempdata,by=c("SCENARIO","nve","YEAR","MONTH","DAY"))
  nrow(md)
  
  md[,year := format.Date(sprintf("%s-%s-%s",YEAR,MONTH,DAY),"%G")] #Week-based year, instead of normal year (%Y)
  md[,week := as.numeric(format.Date(sprintf("%s-%s-%s",YEAR,MONTH,DAY),"%V"))]
  
  md <- md[,.(temperature=mean(TEMP,na.rm=T),
              gridRain=mean(RAIN,na.rm=T),
              runoff=mean(runoff,na.rm=T),
              month=min(MONTH)),by=.(year,week,nve,waterwork,SCENARIO,region)]
  md[,season:=1]
  md[month %in% 3:5,season:=2]
  md[month %in% 6:8,season:=3]
  md[month %in% 9:11,season:=4]
  
  md[,seasonchar:=as.character(season)]
  RAWmisc::RecodeDT(md,c(
    "1"="Winter",
    "2"="Spring",
    "3"="Summer",
    "4"="Autumn"
  ),"seasonchar")
  md[,season:=NULL]
  md[,season:=seasonchar]
  
  md[,type:="Accredited"]
  
  md[,c_temperature0_3:=
       (shift(temperature,n=0L)+
          shift(temperature,n=1L)+
          shift(temperature,n=2L)+
          shift(temperature,n=3L))/4,by=waterwork]
  
  md[,c_gridRain0_3:=
       (shift(gridRain,n=0L)+
          shift(gridRain,n=1L)+
          shift(gridRain,n=2L)+
          shift(gridRain,n=3L))/4,by=waterwork]
  
  md[,c_gridRunoffStandardised0_3:=
       (shift(runoff,n=0L)+
          shift(runoff,n=1L)+
          shift(runoff,n=2L)+
          shift(runoff,n=3L))/4,by=waterwork]
  
  md[,id:=waterwork]
  md[,simulation:=i]
  temp[[i]] <- copy(md)
}

md <- rbindlist(temp)
md <- md[year %in% c(2006:2014,2071:2100)]
md[,simulation:=gsub("2006-2015","",simulation)]
md[,simulation:=gsub("2071-2100","",simulation)]
md[,simulation:=paste0(SCENARIO,"-",simulation)]

WP1RES <- list()

for(type in c("raw","clean","raw_outbreaks","clean_outbreaks")){
  if(type=="raw"){
    d <- WP1DataRaw()
    outcomeVar <- "value"
  } else if(type=="clean"){
    d <- WP1DataClean()
    outcomeVar <- "value"
  } else if(type=="raw_outbreaks"){
    d <- WP2WaterworkRawData()
    d[,value:=(value0+value1+value2+value3)/4]
    outcomeVar <- "s_outbreakLege"
  } else if(type=="clean_outbreaks"){
    d <- WP2WaterworkCleanData()
    d[,value:=(value0+value1+value2+value3)/4]
    outcomeVar <- "s_outbreakLege"
  }
  
  
  
  if(type %in% c("raw","clean")){
    d <- d[type %in% c("Accredited","Internal") & variable!="Conductivity at 25 degrees"]
    toPlot <- copy(d[variable %in% c("Coliform bacteria",
                     "E. Coli",
                     "Intestinal Enterococci",
                     "Turbidity",
                     "Colour")])
    toPlot[variable!="pH",value:=log(1+value)]
    toPlot[variable!="pH",variable:=sprintf("log(1+%s)",variable)]
    
    toPlot[,cat:=Bin(value,seq(min(value),max(value),length.out=30)),by=variable]
    toPlot[,num:=.N,by=variable]
    q <- ggplot(toPlot,aes(x=cat))
    q <- q + geom_bar(aes(y=..prop..))
    q <- q + facet_wrap(~variable,scales="free_x")
    q <- q + scale_x_continuous("Value")
    q <- q + scale_y_continuous("Proportion",lim=c(0,1))
    q <- q + theme_gray(16)
    RAWmisc::saveA4(q,filename=file.path(RAWmisc::PROJ$SHARED_TODAY,sprintf("descriptives_%s.png",type)),landscape=T)
    
    d[,seasonchar:=as.character(season)]
    RAWmisc::RecodeDT(d,c(
      "1"="Winter",
      "2"="Spring",
      "3"="Summer",
      "4"="Autumn"
    ),"seasonchar")
    d[,season:=NULL]
    d[,season:=seasonchar]
  
    stack <- data.table(expand.grid(
      c("temperature0_3","gridRain0_3","gridRunoffStandardised0_3"
        ),
      c("c"),
      unique(d$variable),
      c("Whole year",unique(d$season)),
      stringsAsFactors = FALSE))
    stack <- stack[Var3!="Conductivity at 25 degrees"]
    stack[,exposure:=paste0(Var2,"_",Var1)]
    stack[,Var1:=NULL]
    stack[,Var2:=NULL]
    setnames(stack,"Var3","watervariable")
    setnames(stack,"Var4","season")

  } else {
    stack <- data.table(expand.grid(c("value"),
                        unique(d$variable),
                        c("Whole year",unique(d$season)),
                        rev(unique(d$age)),
                        stringsAsFactors = FALSE))
    stack <- stack[Var2!="Conductivity at 25 degrees"]
    setnames(stack,c("exposure","watervariable","season","age"))
  }
  
  if(type %in% c("raw","raw_outbreaks")){
    stack <- stack[watervariable%in% c(
      "Coliform bacteria",
      "E. Coli",
      "Intestinal Enterococci",
      "Turbidity",
      "Colour"
    )]
  } else {
    stack <- stack[watervariable%in% c(
      "Turbidity",
      "Colour"
    )]
  }
  
  # Your code starts here
  #stackIter <- stack[35]
  #stackIterX <- 38
  predResults <- vector("list",length=nrow(stack))
  num <- 1
  res <- vector("list",length=nrow(stack))
  pb <- RAWmisc::ProgressBarCreate(min=0,max=length(res))
  for(stackIterX in 1:nrow(stack)){
    RAWmisc::ProgressBarSet(pb,stackIterX)
    
    stackIter <- stack[stackIterX]
  
    formulaBase <- sprintf("%s ~ factor(month)",outcomeVar)
    fitData <- d[variable==stackIter$watervariable]
    
    if(!type %in% c("raw","clean")){
      totalData <- fitData[age!="Totalt"]
      fitData <- fitData[age==stackIter$age]
    }
    if(stackIter$season!="Whole year") fitData <- fitData[season==stackIter$season]
    
    fitData$origValue <- fitData[[outcomeVar]]
    if(type %in% c("raw","clean") & stackIter$watervariable!="pH") fitData[,value:=log(1+value)]
   
    formulaWatersource <- formulaType <- "" 
    if(length(unique(fitData$type[!is.na(fitData[,stackIter$exposure,with=F])]))>1){
      formulaBase <- paste0(formulaBase,"+type")
      formulaType <- "+type"
    }
    if(length(unique(fitData$watersource[!is.na(fitData[,stackIter$exposure,with=F])]))>1){
      formulaBase <- paste0(formulaBase,"+watersource")
      formulaWatersource <- "+watersource" 
    }
    formula <- paste0(formulaBase,"+",stackIter$exposure)
    
    fitData <- fitData[c(!is.na(fitData[,stackIter$exposure,with=F]))]
    fitData[,group:=as.numeric(as.factor(id))]
    fitData[,ngroup:=.N,by=group]
    
    
    fitBase <- lme4::lmer(as.formula(paste0(formulaBase,"+(1|id)")), data=fitData) 
    fit <- lme4::lmer(as.formula(paste0(formula,"+(1|id)")), data=fitData) 
    
    x <- data.frame(var="",est=NA,se=NA,pval=NA)
    try({
      x <- ExtractValues(fit,stackIter$exposure,removeLead=T,format=F)
    },TRUE)
    if(nrow(x)==0) next
    
    # FUTURE PREDICTIONS
    if(type=="raw" & stackIter$exposure %in% c("c_temperature0_3","c_gridRain0_3","c_gridRunoffStandardised0_3")){
      predData <- copy(md[id %in% unique(fitData$id)])
      if(stackIter$season!="Whole year") predData <- predData[season==stackIter$season]
      predData[,watersource:=NULL]
      ws <- unique(fitData[,c("id","watersource")])
      predData <- merge(predData,ws,by="id")
      #p <- merTools::predictInterval(merMod=fit, newdata = predData, n.sims=100, returnSims = T)
      #p <- data.table(attr(p,"sim.results"))
      #predData <- cbind(predData,p)
      predData[,p:=predict(fit,predData)]
      if(stackIter$watervariable!="pH") predData[,p:=exp(p)-1]
      setnames(predData,stackIter$exposure,"exposure")
      multiplier <- predData[,.(p=mean(p,na.rm=T),exposure=mean(exposure)),by=.(
        id,year,simulation,SCENARIO,region
      )]

      percs <- fitData[,.(m=mean(origValue,na.rm=T)),
              by=.(
                id
              )]
      all_data_original_mean <- mean(fitData$origValue,na.rm=T)
      
      #multiplier[p<0,p:=0]
      multiplier[year%in%c(2006:2014),predicted_mean_2006_2014:=mean(p,na.rm=T),by=.(id,simulation)]
      multiplier[,predicted_mean_2006_2014:=mean(predicted_mean_2006_2014,na.rm=T),by=.(id,simulation)]
      
      multiplier[year%in%c(2006:2014),all_data_predicted_mean_2006_2014:=mean(p,na.rm=T),by=.(simulation)]
      multiplier[,all_data_predicted_mean_2006_2014:=mean(all_data_predicted_mean_2006_2014,na.rm=T),by=.(simulation)]

      pred <- merge(multiplier,percs,by="id")
      #pred[,yearly_adjustment_factor:=(p-predicted_mean_2006_2014+m)/m]
      #pred[m==0,yearly_adjustment_factor:=(p-all_data_predicted_mean_2006_2014+all_data_original_mean)/all_data_original_mean]
      pred[,yearly_adjustment_factor:=p/predicted_mean_2006_2014]
      
      pred[,modelled_exposure:=exposure]
      
      simulations <- RepeatDataTable(pred[,c("id","year","simulation","SCENARIO","region","yearly_adjustment_factor","m","modelled_exposure")],1000)
      simulations[,adjusted_sampled_value:=0.0]
      for(i in unique(simulations$id)){
        simulations[id==i,adjusted_sampled_value:=sample(fitData[id==i]$origValue,.N,replace=T)*yearly_adjustment_factor]
      }
      
      simulations[,yearCat:="2006-2014"]
      simulations[year>2070,yearCat:="2071-2100"]
      simulations[id=="Bodo||0101 Heggmoen VBA Råvann" & 
                    SCENARIO=="rcp85",.(adjusted_sampled_value=mean(adjusted_sampled_value,na.rm=T),
                     m=mean(m),
                     modelled_exposure=mean(modelled_exposure,na.rm=T)),by=.(yearCat,region)]
      
      waterwork_simulations <- simulations[,.(
        p50=quantile(adjusted_sampled_value,probs = 0.5),
        p75=quantile(adjusted_sampled_value,probs = 0.75),
        p95=quantile(adjusted_sampled_value,probs = 0.95),
        p99=quantile(adjusted_sampled_value,probs = 0.99),
        p100=max(adjusted_sampled_value)
      ),by=.(id,simulation,yearCat,SCENARIO,region)]
      
      waterwork_mixedsimulations <- waterwork_simulations[,.(
        p50=median(p50),
        p75=median(p75),
        p95=median(p95),
        p99=median(p99),
        p100=median(p99)
      ),by=.(id,yearCat,SCENARIO,region)]
      waterwork_mixedsimulations[,simulation:="ALL_MIXED"]
      
      region_mixedsimulations <- waterwork_simulations[,.(
        p50=median(p50),
        p75=median(p75),
        p95=median(p95),
        p99=median(p99),
        p100=median(p99)
      ),by=.(yearCat,SCENARIO,region)]
      region_mixedsimulations[,id:="REGION"]
      region_mixedsimulations[,simulation:="ALL_MIXED"]
      
      simulations <- rbind(waterwork_simulations,waterwork_mixedsimulations,region_mixedsimulations)
      setorder(simulations,id,simulation,yearCat)
       
      simulations$exposure <- stackIter$exposure
      simulations$outcome <- stackIter$watervariable
      simulations$seasonStratified <- stackIter$season
      simulations$pval <- x$pval
      predResults[[stackIterX]] <- simulations
    }
    
    if(type %in% c("raw","clean")){
      x$exposure <- stackIter$exposure
      x$outcome <- stackIter$watervariable
    } else {
      x$exposure <- stackIter$watervariable
      x$outcome <- "Outbreak"
    }
    #if(x$outcome!="pH") x$outcome <- sprintf("log(1+%s)",x$outcome)
    x$n <- nrow(fitData)
    x$outcomeMean <- mean(fitData$origValue,na.rm=T)
    x$outcomeSD <- sd(fitData$origValue,na.rm=T)
    x$outcome50 <- quantile(fitData$origValue,na.rm=T,prob=0.5)
    x$outcome75 <- quantile(fitData$origValue,na.rm=T,prob=0.75)
    x$outcome90 <- quantile(fitData$origValue,na.rm=T,prob=0.9)
    x$outcome95 <- quantile(fitData$origValue,na.rm=T,prob=0.95)
    x$outcome99 <- quantile(fitData$origValue,na.rm=T,prob=0.99)
    
    x$exposureMean <- mean(fitData[[stackIter$exposure]],na.rm=T)
    x$exposureSD <- sd(fitData[[stackIter$exposure]],na.rm=T)
    x$exposure50 <- quantile(fitData[[stackIter$exposure]],na.rm=T,prob=0.5)
    x$exposure75 <- quantile(fitData[[stackIter$exposure]],na.rm=T,prob=0.75)
    x$exposure90 <- quantile(fitData[[stackIter$exposure]],na.rm=T,prob=0.9)
    x$exposure95 <- quantile(fitData[[stackIter$exposure]],na.rm=T,prob=0.95)
    x$exposure99 <- quantile(fitData[[stackIter$exposure]],na.rm=T,prob=0.99)
    
    x$season <- stackIter$season
    x$interactionPvalSeason <- NA
    x$interactionPvalAge <- NA
    if(type %in% c("raw","clean")){
      if(stackIter$season=="Whole year"){
        formula1 <- sprintf("%s ~ %s + factor(season) %s %s +(1|id)",outcomeVar,stackIter$exposure,formulaType,formulaWatersource)
        fit1 <- lme4::lmer(as.formula(formula1),data=fitData)
        
        formula0 <- sprintf("%s ~ %s*factor(season) %s %s +(1|id)",outcomeVar,stackIter$exposure,formulaType,formulaWatersource)
        fit0 <- lme4::lmer(as.formula(formula0),data=fitData)
        x$interactionPvalSeason <- lmtest::lrtest(fit0,fit1)$`Pr(>Chisq)`[2]
        if(type=="raw" & stackIter$exposure %in% c("c_temperature0_3","c_gridRain0_3","c_gridRunoffStandardised0_3")){
          predResults[[stackIterX]][,interactionPvalSeason := lmtest::lrtest(fit0,fit1)$`Pr(>Chisq)`[2]] 
        }
      }
    } else {
      x$age <- stackIter$age
      if(stackIter$season=="Whole year" & stackIter$age=="Totalt"){
        formula1 <- sprintf("%s ~ %s + factor(season) + age %s %s +(1|id)",outcomeVar,stackIter$exposure,formulaType,formulaWatersource)
        fit1 <- lme4::lmer(as.formula(formula1),data=totalData)
        
        formula0 <- sprintf("%s ~ %s*factor(season) + age %s %s +(1|id)",outcomeVar,stackIter$exposure,formulaType,formulaWatersource)
        fit0 <- lme4::lmer(as.formula(formula0),data=totalData)
        x$interactionPvalSeason <- lmtest::lrtest(fit0,fit1)$`Pr(>Chisq)`[2]
        
        formula0 <- sprintf("%s ~ %s*age + factor(season) %s %s +(1|id)",outcomeVar,stackIter$exposure,formulaType,formulaWatersource)
        fit0 <- lme4::lmer(as.formula(formula0),data=totalData)
        x$interactionPvalAge <- lmtest::lrtest(fit0,fit1)$`Pr(>Chisq)`[2]
      }
    }
    
    res[[stackIterX]] <- x
  }
  
  res <- rbindlist(res)
  if(type=="raw"){
    predResults <- rbindlist(predResults,fill=T)
    
    predResults[,seasonStratified:=factor(seasonStratified,levels=c(
      "Whole year",
      "Spring",
      "Summer",
      "Autumn",
      "Winter"
    ))]
    
    for(i in unique(predResults$id)){
      openxlsx::write.xlsx(predResults[id==i],file = file.path(
        RAWmisc::PROJ$SHARED_TODAY,"waterwork_specific_predictions",sprintf("future_predictions_%s.xlsx",i)
      ))
    }
    
    regionResults <- predResults[id=="REGION" & SCENARIO=="rcp85"]
    regionResults[,sigEffect:=pval<0.05]
    regionResults[,sigInteraction:=interactionPvalSeason<0.05]
    regionResults[,isSigInteraction:=as.numeric(max(sigInteraction,na.rm=T)),by=.(exposure,outcome)]
    regionResults[,showResults:=FALSE]
    regionResults[sigEffect==TRUE & seasonStratified=="Whole year",showResults:=TRUE]
    regionResults[sigEffect==TRUE & seasonStratified!="Whole year" & isSigInteraction==1,showResults:=TRUE]
    
    #regionResults[,clean95:=ifelse(showResults | yearCat=="2006-2014",p95,-9)]
    regionResults[,clean99:=ifelse(showResults | yearCat=="2006-2014",RAWmisc::Format(p99,1),"-")]
    #regionResults[,clean100:=ifelse(showResults | yearCat=="2006-2014",p100,-9)]
    
    tab <- dcast.data.table(regionResults,region+outcome+seasonStratified~yearCat+exposure,value.var = c("clean99"))
    toDrop <- expand.grid("2006-2014",c("c_temperature0_3","c_gridRain0_3","c_gridRunoffStandardised0_3"))[-1,]
    toDrop <- sprintf("%s_%s",toDrop$Var1,toDrop$Var2)
    tab[,(toDrop):=NULL]
    
    setnames(tab,c("region","outcome","season","p99_2006_2014","rain_p99_2071_2100","runoff_p99_2071_2100","temp_p99_2071_2100"))
    openxlsx::write.xlsx(tab,file.path(RAWmisc::PROJ$SHARED_TODAY,"future_predictions_rcp85.xlsx"))
    
    
  }
 
  WP1RES[[type]] <- copy(res)
  WP1RES[[type]][,typex:=type]
  if(type %in% c("raw","clean")){ 
    # graph
    # graphing <- res[!stringr::str_detect(exposure,"0_3")]
    # graphing[,week:=stringr::str_extract(exposure,"[0-9]$")]
    # graphing[,exposureName:=stringr::str_extract(exposure,"[a-zA-Z_]*")]
    # graphing[,effect:="None"]
    # graphing[pval<0.05/.N & est<0,effect:="Decreases"]
    # graphing[pval<0.05/.N & est>0,effect:="Increases"]
    # graphing[,significantIteraction:=0]
    # graphing[interactionPvalSeason<0.05,significantIteraction:=1]
    # graphing[,significantIteraction:=max(significantIteraction),by=.(exposure,outcome)]
    # 
    # graphing[,effect:=factor(effect,levels=c("Increases","None","Decreases"))]
    # RAWmisc::RecodeDT(graphing,c(
    #   "c_temperature"="Temperature",
    #   "c_gridRunoffStandardised"="Runoff",
    #   "c_gridRain"="Rain"
    # ),"exposureName")
    # graphing[,season:=factor(season,levels=c(
    #   "Whole year","Winter","Spring","Summer","Autumn"
    # ))]
    # RAWmisc::RecodeDT(graphing,c(
    #   "Intestinal Enterococci"="Intestinal\nEnterococci"
    # ),"outcome")
    # graphing3 <- graphing[1:3]
    # graphing3[1,effect:="Increases"]
    # graphing3[2,effect:="None"]
    # graphing3[3,effect:="Decreases"]
    # q <- ggplot(graphing,aes(x=week,y=exposureName,fill=effect))
    # q <- q + geom_tile(data=graphing3,alpha=0.0)
    # q <- q + geom_tile(alpha=0.6,colour="black")
    # q <- q + geom_text(data=graphing[significantIteraction==0 & season!="Whole year"],label="X")
    # q <- q + facet_grid(outcome~season,scales="free")
    # q <- q + scale_fill_manual("",values=c("red","gray","blue"))
    # q <- q + scale_x_discrete("Weeks lag")
    # q <- q + scale_y_discrete("Exposure (continuous)")
    # q <- q + labs(caption="X denotes a non-significant interaction term with season")
    # RAWmisc::saveA4(q,filename=file.path(RAWmisc::PROJ$SHARED_TODAY,
    #                 sprintf("WP10_%s.png",type)))
    # 
    # table
    res <- res[stringr::str_detect(exposure,"0_3")]
    res[,effect:=sprintf("%s%% (%s%%, %s%%)",
                         RAWmisc::Format((exp(est)-1)*100),
                         RAWmisc::Format((exp(est-1.96*se)-1)*100),
                         RAWmisc::Format((exp(est+1.96*se)-1)*100))]
    res[outcome=="pH",effect:=sprintf("%s (%s, %s)",
                                     RAWmisc::Format(est),
                                     RAWmisc::Format(est-1.96*se),
                                     RAWmisc::Format(est+1.96*se))]
    
    res[,outcomeSummary:=sprintf("50p=%s, 75p=%s, 95p=%s, 99p=%s",
                                 RAWmisc::Format(outcome50),
                                 RAWmisc::Format(outcome75),
                                 RAWmisc::Format(outcome95),
                                 RAWmisc::Format(outcome99)
                                 )]
    
    res[,exposureSummary:=sprintf("%s (%s)",
                                 RAWmisc::Format(exposureMean),
                                 RAWmisc::Format(exposureSD))]
    
    res[, pvalBonf:=pval*.N]
    res[,sig:=""]
    res[pvalBonf<0.05,sig:="*"]
    res[pvalBonf>1,pvalBonf:=1]
    res[,pvalBonf:=RAWmisc::Format(pvalBonf,3)]
    res[pvalBonf=="0.000",pvalBonf:="<0.001"]
    
    res[, pval:=RAWmisc::Format(pval,3)]                               
    res[pval=="0.000",pval:="<0.001"]
    
    res[,pvalBonf:=paste0(pvalBonf,sig)]
    
    res[,interactionPvalSeason:=RAWmisc::Format(interactionPvalSeason,3)]
    res[interactionPvalSeason=="0.000",interactionPvalSeason:="<0.001"]
    
    res <- res[,c("season","outcome","outcomeSummary","exposure","exposureSummary","n","effect","pval","pvalBonf","interactionPvalSeason")]
    res[,season:=factor(season,level=c("Whole year","Winter","Spring","Summer","Autumn"))]
    setorder(res,season,outcome,exposure)
    openxlsx::write.xlsx(res,file = file.path(
      RAWmisc::PROJ$SHARED_TODAY,sprintf("WP10_%s.xlsx",type)
    ))
  } else {
    res[,effect:=sprintf("%spp (%spp, %spp)",
                         RAWmisc::Format(est*100,2),
                         RAWmisc::Format((est-1.96*se)*100,2),
                         RAWmisc::Format((est+1.96*se)*100,2))]
    
    res[,outcomeSummary:=sprintf("%s%%",
                                 RAWmisc::Format(outcomeMean*100)
    )]
    
    res[,exposureSummary:=sprintf("50p=%s, 75p=%s, 95p=%s",
                                 RAWmisc::Format(exposure50),
                                 RAWmisc::Format(exposure75),
                                 RAWmisc::Format(exposure95)
    )]
    
    res[, pvalBonf:=pval*.N]
    res[,sig:=""]
    res[pvalBonf<0.05,sig:="*"]
    res[pvalBonf>1,pvalBonf:=1]
    res[,pvalBonf:=RAWmisc::Format(pvalBonf,3)]
    res[pvalBonf=="0.000",pvalBonf:="<0.001"]
    
    res[, pval:=RAWmisc::Format(pval,3)]                               
    res[pval=="0.000",pval:="<0.001"]
    
    res[,pvalBonf:=paste0(pvalBonf,sig)]
    
    res[,interactionPvalSeason:=RAWmisc::Format(interactionPvalSeason,3)]
    res[interactionPvalSeason=="0.000",interactionPvalSeason:="<0.001"]
    
    res[,interactionPvalAge:=RAWmisc::Format(interactionPvalAge,3)]
    res[interactionPvalAge=="0.000",interactionPvalAge:="<0.001"]
    
    res <- res[,c(
      "age",
      "season",
      "outcome",
      "outcomeSummary",
      "exposure",
      "exposureSummary",
      "n",
      "effect",
      "pval",
      "pvalBonf",
      "interactionPvalAge",
      "interactionPvalSeason")]
    res[,season:=factor(season,level=c("Whole year","Winter","Spring","Summer","Autumn"))]
    setorder(res,-age,season,outcome,exposure)
    openxlsx::write.xlsx(res,file = file.path(
      RAWmisc::PROJ$SHARED_TODAY,sprintf("WP15_%s.xlsx",type)
    ))
  }
  
}

r <- rbindlist(WP1RES,fill=T)
r[,pvalBonf:=pval*.N,by=typex]
r[,category:="Not significant"]
r[pvalBonf<0.05 & est>0,category:="Increases"]
r[pvalBonf<0.05 & est<0,category:="Decreases"]
r[,category:=factor(category,levels=c("Decreases","Not significant","Increases"))]

#r <- r[is.na(age) | age=="Totalt"]
#r <- r[season=="Whole year"]
r[,canAnalyseSeason:=ifelse(interactionPvalSeason<0.05,1,0)]
r[,canAnalyseAge:=ifelse(interactionPvalAge<0.05,1,0)]

r[,canAnalyseSeason:=max(canAnalyseSeason,na.rm=T),by=.(typex,exposure,outcome)]
r[,canAnalyseAge:=max(canAnalyseAge,na.rm=T),by=.(typex,exposure,outcome)]

r[season=="Whole year" & age=="Totalt",canAnalyseSeason:=1]
r[season=="Whole year" & age=="Totalt",canAnalyseAge:=1]

r[season=="Whole year" & is.na(age),canAnalyseSeason:=1]
r[season=="Whole year" & is.na(age),canAnalyseAge:=1]

r[season=="Whole year" & canAnalyseAge==1,canAnalyseSeason:=1]
r[canAnalyseSeason==1 & age=="Totalt",canAnalyseAge:=1]

r[is.na(age),canAnalyseAge:=1]

r[typex=="raw"]


RAWmisc::RecodeDT(r,switch=c(
  "clean"="Climate predicting\nclean water quality",
  "clean_outbreaks"="Clean water quality\npredicting outbreaks",
  "raw"="Climate predicting\nraw water quality",
  "raw_outbreaks"="Raw water quality\npredicting outbreaks"
),"typex")

r[,typex:=factor(typex,levels=c(
  "Climate predicting\nraw water quality",
  "Climate predicting\nclean water quality",
  "Raw water quality\npredicting outbreaks",
  "Clean water quality\npredicting outbreaks"
))]

RAWmisc::RecodeDT(r,switch=c(
  "c_gridRain0_3"="Rain",
  "c_gridRunoffStandardised0_3"="Runoff",
  "c_temperature0_3"="Temperature"
),"exposure")

r[,season:=factor(season,levels=c("Whole year","Winter","Spring","Summer","Autumn"))]
r[,age:=factor(age,levels=c("Totalt","0-4","5-14","15-64","65+"))]
setattr(r$age,"levels",c("Total","0-4","5-14","15-64","65+"))
setorder(r,outcome,season,age)

r[,newOutcome:=as.character(NA)]
r[!is.na(age) & !is.na(season) & is.na(newOutcome),newOutcome:=sprintf("%s - %s - %s",outcome,season,age)]
r[!is.na(season) & is.na(newOutcome),newOutcome:=sprintf("%s - %s",outcome,season)]
r[is.na(newOutcome),newOutcome:=sprintf("%s",outcome)]

r[,newOutcome:=factor(newOutcome,levels=rev(unique(r$newOutcome)))]

q <- ggplot(r[canAnalyseSeason==1 & canAnalyseAge==1],aes(x=exposure,y=newOutcome,fill=category))
q <- q + geom_tile(alpha=0.7,colour="black")
q <- q + facet_wrap(~typex,scales="free")
q <- q + scale_fill_brewer("",drop=F,palette="Set1")
q <- q + scale_x_discrete("")
q <- q + scale_y_discrete("")
q <- q + guides(fill = guide_legend(reverse=T))
q <- q + theme_gray(12)
q <- q + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))
q <- q + theme(legend.position="bottom")
q <- q + labs(caption="Exposure on x-axis, outcome on y-axis")
RAWmisc::saveA4(q,filename=file.path(RAWmisc::PROJ$SHARED_TODAY,"WP1.png"),landscape=F)

RAWmisc::SaveProject()

