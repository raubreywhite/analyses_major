suppressMessages(library(ggplot2))
RAWmisc::AllowFileManipulationFromInitialiseProject()
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

importantDirs <- c(
  file.path(RAWmisc::PROJ$CLEAN,"WP1_waterworks"),
  file.path(RAWmisc::PROJ$CLEAN,"WP1_waterworks_clean_water")
)
for(i in importantDirs) if(!dir.exists(i)) dir.create(i,recursive=TRUE)

for(type in c("raw","clean")){
  if(type=="raw"){
    d <- WP1DataRaw()
  } else {
    d <- WP1DataClean()
  }
  d <- d[type %in% c("Accredited","Internal")]
  dlog <- copy(d)
  dlog[,valuelog:=value]
  dlog[variable!="pH",valuelog:=log(1+valuelog)]
  
  stack <- data.table(expand.grid(c("temperature0_3","gridRain0_3","gridPrecip0_3","gridRunoffStandardised0_3"),
                      c("wp950","c"),
                      unique(d$variable),stringsAsFactors = FALSE))
  stack <- stack[Var3!="Conductivity at 25 degrees"]
  stack[,exposure:=paste0(Var2,"_",Var1)]
  stack[,Var1:=NULL]
  stack[,Var2:=NULL]
  setnames(stack,"Var3","outcome")
  
  # Your code starts here
  #stackIter <- stack[35]
  #stackIterX <- 36
  num <- 1
  res <- list()
  for(stackIterX in 1:nrow(stack)){
    stackIter <- stack[stackIterX]
    print(paste0(num,"/",nrow(stack)))
    num <- num + 1
  
    formulaOneWaterwork <- formulaBase <- paste0("value ~ factor(month)")
    fitData <- d[variable==stackIter$outcome]
    fitData[, origValue := value]
    if(stackIter$outcome!="pH") fitData[,value:=log(1+value)]
    
    if(length(unique(fitData$type[!is.na(fitData[,stackIter$exposure,with=F])]))!=1) formulaBase <- paste0(formulaBase,"+type")
    if(length(unique(fitData$watersource[!is.na(fitData[,stackIter$exposure,with=F])]))!=1) formulaBase <- paste0(formulaBase,"+watersource")
    formula <- paste0(formulaBase,"+",stackIter$exposure)
    formulaOneWaterwork <- paste0(formulaOneWaterwork,"+",stackIter$exposure)
    
    fitData <- fitData[c(!is.na(fitData[,stackIter$exposure,with=F]))]
    fitData[,group:=as.numeric(as.factor(id))]
    fitData[,ngroup:=.N,by=group]
    
    est50 <- se50 <- est90 <- se90 <- c()
    for(q in unique(fitData$id)){
      print(q)
      if(nrow(fitData[id==q])<100) next
      if(length(unique(fitData[id==q]$value))<5){
        print("SKIPPED")
        est50 <- c(est50,0)
        se50 <- c(se50,0)
        
        est90 <- c(est90,0)
        se90 <- c(se90,0)
      } else {
        print("ANALYSING")
        try({
          temp <- as.data.frame(coef(summary(quantreg::rq(as.formula(formulaOneWaterwork),data=fitData[id==q],tau=0.5))))
          temp$var <- row.names(temp)
          temp <- temp[temp$var==stackIter$exposure,]
          est50 <- c(est50,temp$coefficients)
          se50 <- c(se50,(temp$`upper bd`-temp$`lower bd`)/2/1.96)
        },TRUE)
        
        try({
          temp <- as.data.frame(coef(summary(quantreg::rq(as.formula(formulaOneWaterwork),data=fitData[id==q],tau=0.9))))
          temp$var <- row.names(temp)
          temp <- temp[temp$var==stackIter$exposure,]
          est90 <- c(est90,temp$coefficients)
          se90 <- c(se90,(temp$`upper bd`-temp$`lower bd`)/2/1.96)
        },TRUE)
      }
    }
    #test <- lqmm(fixed = y ~ wp950_temperature0_3, random = ~1, group = id, data=as.data.frame(na.omit(fitData)))
    
    if(length(est50)==0){
      est50 <- 999
      se50 <- 999
    }
    if(length(est90)==0){
      est90 <- 999
      se90 <- 999
    }
    
    valid50 <- which(is.finite(est50) & is.finite(se50))
    valid90 <- which(is.finite(est90) & is.finite(se90))
    
    est50 <- est50[valid50]
    se50 <- se50[valid50]
    est90 <- est90[valid90]
    se90 <- se90[valid90]
    
    est50 <- mean(est50)#sum(est50*(1/se50^2))/sum(1/se50^2)
    est90 <- mean(est90)#sum(est90*(1/se90^2))/sum(1/se90^2)
    
    fitBase <- lme4::lmer(as.formula(paste0(formulaBase,"+(1|id)")), data=fitData) 
    fit <- lme4::lmer(as.formula(paste0(formula,"+(1|id)")), data=fitData) 
    x <- data.frame(var="",est=NA,se=NA,pval=NA)
    try({
      x <- ExtractValues(fit,stackIter$exposure,removeLead=T,format=F)
    },TRUE)
    
    
    x$exposure <- stackIter$exposure
    x$outcome <- stackIter$outcome
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
    x$est50 <- est50
    x$est90 <- est90
    res[[stackIterX]] <- x
  }
  
  res <- rbindlist(res)
  
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
  
  res[, est50:=sprintf("%s%%",RAWmisc::Format((exp(est50)-1)*100,2))]
  res[, est90:=sprintf("%s%%",RAWmisc::Format((exp(est90)-1)*100,2))]
  res[outcome=="pH", est50:=RAWmisc::Format(est50,2)]
  res[outcome=="pH", est90:=RAWmisc::Format(est90,2)]
  
  res[,outcome90:=RAWmisc::Format(outcome90,2)]

  res <- res[,c("outcome","outcomeSummary","exposure","exposureSummary","n","effect","pval","pvalBonf","est50","outcome90","est90")]
  setorder(res,outcome,exposure)
  openxlsx::write.xlsx(res,file = file.path(
    RAWmisc::PROJ$SHARED_TODAY,sprintf("WP1_%s.xlsx",type)
  ))
  
}

