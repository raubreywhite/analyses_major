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

suppressWarnings(suppressMessages(library(data.table)))
suppressWarnings(suppressMessages(library(ggplot2)))
suppressWarnings(suppressMessages(library(foreach)))
suppressWarnings(suppressMessages(library(pomp)))
suppressMessages(library(doParallel))
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

mCOORDS <- unique(mCOORDS[,c("met","nve","waterwork")])
mCOORDS[,n:=1:.N,by=.(nve)]
mCOORDS <- mCOORDS[n==1]
nrow(mCOORDS)
length(unique(mCOORDS$met))
length(unique(mCOORDS$nve))

setnames(mCOORDS,"met","WW.NO")

temp <- list()
for(i in c(
  "CNRM-CM5_CCLM_prec_temp_rain_1971-2000.csv",
  "CNRM-CM5_CCLM_prec_temp_rain_2071-2100.csv",
  "CNRM-CM5_SMHI-RCA4_prec_temp_rain_1971-2000.csv",
  "CNRM-CM5_SMHI-RCA4_prec_temp_rain_2071-2100.csv",
  "EC-EARTH_CCLM_prec_temp_rain_1971-2000.csv",
  "EC-EARTH_CCLM_prec_temp_rain_2071-2100.csv",
  "EC-EARTH_DMI-HIRHAM5_prec_temp_rain_1971-2000.csv",
  "EC-EARTH_DMI-HIRHAM5_prec_temp_rain_2071-2100.csv",
  "EC-EARTH_KNMI-RACMO_prec_temp_rain_1971-2000.csv",
  "EC-EARTH_KNMI-RACMO_prec_temp_rain_2071-2100.csv",
  "EC-EARTH_SMHI-RCA4_prec_temp_rain_1971-2000.csv",
  "EC-EARTH_SMHI-RCA4_prec_temp_rain_2071-2100.csv",
  "HADGEM2_SMHI-RCA4_prec_temp_rain_1971-2000.csv",
  "HADGEM2_SMHI-RCA4_prec_temp_rain_2071-2100.csv",
  "IPSL-CM5A_SMHI-RCA4_prec_temp_rain_1971-2000.csv",
  "IPSL-CM5A_SMHI-RCA4_prec_temp_rain_2071-2100.csv",
  "MPI_CCLM_prec_temp_rain_1971-2000.csv",
  "MPI_CCLM_prec_temp_rain_2071-2100.csv")){
  print(i)
  md <- fread(file.path(RAWmisc::PROJ$RAW,"FutureScenariosAndControlRuns",i))
  
  xtabs(~md$WW.NO)
  nrow(md)
  md <- merge(md,mCOORDS,by="WW.NO")
  nrow(md)
  xtabs(~md$nve+md$WW.NO)
  
  md[,year := format.Date(sprintf("%s-%s-%s",YEAR,MONTH,DAY),"%G")] #Week-based year, instead of normal year (%Y)
  md[,week := as.numeric(format.Date(sprintf("%s-%s-%s",YEAR,MONTH,DAY),"%V"))]
  
  md <- md[,.(temperature=mean(TEMP,na.rm=T),
              gridRain=mean(RAIN,na.rm=T),
              month=min(MONTH)),by=.(year,week,nve,waterwork)]
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
  
  md[,id:=waterwork]
  md[,simulation:=i]
  temp[[i]] <- copy(md)
}

md <- rbindlist(temp)

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
    d <- d[type %in% c("Accredited","Internal")]
    
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
      c("temperature0_3","gridRain0_3","gridRunoffStandardised0_3",
        "temperature0_0","gridRain0_0","gridRunoffStandardised0_0",
        "temperature1_1","gridRain1_1","gridRunoffStandardised1_1",
        "temperature2_2","gridRain2_2","gridRunoffStandardised2_2",
        "temperature3_3","gridRain3_3","gridRunoffStandardised3_3",
        "temperature4_4","gridRain4_4","gridRunoffStandardised4_4"
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
  
  stack <- stack[watervariable%in% c(
    "Coliform bacteria",
    "E. Coli",
    "Intestinal Enterococci",
    "Turbidity"
  )]
  
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
    if(type=="raw" & stackIter$exposure %in% c("c_temperature0_3","c_gridRain0_3")){
      predData <- copy(md[id %in% unique(fitData$id) & year %in% c(2000,2100)])
      if(stackIter$season!="Whole year") predData <- predData[season==stackIter$season]
      predData[,watersource:=NULL]
      ws <- unique(fitData[,c("id","watersource")])
      predData <- merge(predData,ws,by="id")
      #p <- merTools::predictInterval(merMod=fit, newdata = predData, n.sims=100, returnSims = T)
      #p <- data.table(attr(p,"sim.results"))
      #predData <- cbind(predData,p)
      predData[,p:=predict(fit,predData)]
      setnames(predData,stackIter$exposure,"exposure")
      predData <- predData[,.(p=mean(p),exposure=mean(exposure)),by=.(
        id,year,simulation
      )]
      predData[,simulation:=gsub("1971-2000","",simulation)]
      predData[,simulation:=gsub("2071-2100","",simulation)]
      
      predData <- dcast.data.table(predData,id+simulation~year,value.var=c("p","exposure"))
      predData[,expectedExpP1OutcomeIncreasePerc:=exp(x$est)^(exposure_2100-exposure_2000)]
      predData[,p_2000:=NULL]
      predData[,p_2100:=NULL]
      predData[,exposure_2000:=NULL]
      predData[,exposure_2100:=NULL]
     
      percs <- fitData[,.(p50=quantile(value,prob=0.5),
                 p75=quantile(value,prob=0.75),
                 p95=quantile(value,prob=0.95),
                 p100=quantile(value,prob=1)),
              by=.(
                id
              )]
      
      predData <- merge(predData,percs,by="id")
      for(i in c(50,75,95,100)){
        outcome <- sprintf("y2000_p%s",i)
        exposure <- sprintf("p%s",i)
        predData[,(outcome):=RAWmisc::Format(exp(get(exposure))-1,1)]
        
        outcome <- sprintf("y2100_p%s",i)
        exposure <- sprintf("p%s",i)
        predData[,(outcome):=RAWmisc::Format(exp(get(exposure))*expectedExpP1OutcomeIncreasePerc-1,1)]
      }
      
       
      predData$exposure <- stackIter$exposure
      predData$outcome <- stackIter$watervariable
      predData$seasonStratified <- stackIter$season
      predData$pval <- x$pval
      predResults[[stackIterX]] <- predData
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
    
    x$exposureMean <- mean(fitData[[stackIter$exposure]],na.rm=T)
    x$exposureSD <- sd(fitData[[stackIter$exposure]],na.rm=T)
    x$exposure50 <- quantile(fitData[[stackIter$exposure]],na.rm=T,prob=0.5)
    x$exposure75 <- quantile(fitData[[stackIter$exposure]],na.rm=T,prob=0.75)
    x$exposure90 <- quantile(fitData[[stackIter$exposure]],na.rm=T,prob=0.9)
    x$exposure95 <- quantile(fitData[[stackIter$exposure]],na.rm=T,prob=0.95)
    
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
        if(type=="raw" & stackIter$exposure %in% c("c_temperature0_3","c_gridRain0_3")){
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
    predResults[,p50:=NULL]
    predResults[,p75:=NULL]
    predResults[,p95:=NULL]
    predResults[,p100:=NULL]
    
    predResults[,seasonStratified:=factor(seasonStratified,levels=c(
      "Whole year",
      "Spring",
      "Summer",
      "Autumn",
      "Winter"
    ))]
    
    setcolorder(predResults,
                c("simulation",
                  "id",
                  "outcome",
                  "exposure",
                  "seasonStratified",
                  "pval",
                  "interactionPvalSeason",
                  "expectedExpP1OutcomeIncreasePerc",
                  "y2000_p50",
                  "y2100_p50",
                  "y2000_p75",
                  "y2100_p75",
                  "y2000_p95",
                  "y2100_p95",
                  "y2000_p100",
                  "y2100_p100"
                  ))
    
    setorder(predResults,
                simulation,
                  id,
                  outcome,
                  exposure,
                  seasonStratified
                  )
    
    predResults[,pval:=RAWmisc::Format(pval,3)]
    predResults[,interactionPvalSeason:=RAWmisc::Format(interactionPvalSeason,3)]
    predResults[,expectedExpP1OutcomeIncreasePerc:=RAWmisc::Format(expectedExpP1OutcomeIncreasePerc,3)]
    
    
    for(i in unique(predResults$id)){
      openxlsx::write.xlsx(predResults[id==i],file = file.path(
        RAWmisc::PROJ$SHARED_TODAY,sprintf("future_predictions_%s.xlsx",i)
      ))
    }
  }
 
  if(type %in% c("raw","clean")){ 
    # graph
    graphing <- res[!stringr::str_detect(exposure,"0_3")]
    graphing[,week:=stringr::str_extract(exposure,"[0-9]$")]
    graphing[,exposureName:=stringr::str_extract(exposure,"[a-zA-Z_]*")]
    graphing[,effect:="None"]
    graphing[pval<0.05/.N & est<0,effect:="Decreases"]
    graphing[pval<0.05/.N & est>0,effect:="Increases"]
    graphing[,effect:=factor(effect,levels=c("Increases","None","Decreases"))]
    RAWmisc::RecodeDT(graphing,c(
      "c_temperature"="Temperature",
      "c_gridRunoffStandardised"="Runoff",
      "c_gridRain"="Rain"
    ),"exposureName")
    graphing[,season:=factor(season,levels=c(
      "Whole year","Winter","Spring","Summer","Autumn"
    ))]
    RAWmisc::RecodeDT(graphing,c(
      "Intestinal Enterococci"="Intestinal\nEnterococci"
    ),"outcome")
    graphing3 <- graphing[1:3]
    graphing3[1,effect:="Increases"]
    graphing3[2,effect:="None"]
    graphing3[3,effect:="Decreases"]
    q <- ggplot(graphing,aes(x=week,y=exposureName,fill=effect))
    q <- q + geom_tile(data=graphing3,alpha=0.0)
    q <- q + geom_tile(alpha=0.6,colour="black")
    q <- q + facet_grid(outcome~season,scales="free")
    q <- q + scale_fill_manual("",values=c("red","gray","blue"))
    q <- q + scale_x_discrete("Weeks lag")
    q <- q + scale_y_discrete("Exposure (continuous)")
    RAWmisc::saveA4(q,filename=file.path(RAWmisc::PROJ$SHARED_TODAY,
                    sprintf("WP10_%s.png",type)))
    
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
    
    res[,outcomeSummary:=sprintf("50p=%s, 75p=%s, 95p=%s",
                                 RAWmisc::Format(outcome50),
                                 RAWmisc::Format(outcome75),
                                 RAWmisc::Format(outcome95)
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

RAWmisc::SaveProject()

