DailyAnalyses <- function(d){
  exposures <- d$cont_exposures
  exposures$x=1
  seasons <- data.frame(season=c("Winter","Spring","Summer","Autumn"),x=1,stringsAsFactors=FALSE)
  exposures <- merge(exposures,seasons,all=TRUE,by="x",allow.cartesian=TRUE)
  offset <- data.frame(offset=c("pop","s_consult"),x=1,stringsAsFactors=FALSE)
  exposures <- merge(exposures,offset,all=TRUE,by="x",allow.cartesian=TRUE)
  
  res <- vector("list",nrow(exposures))
  for(i in 1:length(res)){
    varOfInterest <- exposures[i]$varOfInterest
    formula <- paste0("s_gastro~offset(log(pop))+",
                      "",varOfInterest,
                      "+ year + factor(month) + helligdagIndikator + (1|municip)")
    
    fitData <- d$data[season==exposures[i]$season]
    fitDataNoRunoff <- d$dataNoRunoff[season==exposures[i]$season]
    if(exposures[i]$offset=="s_consult"){
      formula <- paste0("s_gastro~offset(log(s_consult))+",
                        "",varOfInterest,
                        "+ year + factor(month) + helligdagIndikator + (1|municip)")
      fitData[,s_consult:=s_consult-s_influensa]
      fitDataNoRunoff[,s_consult:=s_consult-s_influensa]
      fitData <- fitData[s_consult>0]
      fitDataNoRunoff <- fitDataNoRunoff[s_consult>0]
    }
    pop <- fitData[,.(pop=mean(pop)),by=.(municip)]
    pop <- sum(pop$pop)
    
    fit <- lme4::glmer(
      as.formula(formula),
      data=fitData,family="poisson")
    
    a1 <- predict(fit,fitData,type="response")
    a0 <- predict(fit,fitDataNoRunoff,type="response")
    casesPer10kPerYear <- sum(a1-a0,na.rm=T)/
      length(unique(fitData$date))*
      length(unique(fitData$dayOfYear))/pop*10000
    
    res[[i]] <- ExtractValues(fit,varOfInterest,removeLead=FALSE,format=FALSE)
    res[[i]]$casesPer10kPerYear <- casesPer10kPerYear
    res[[i]]$season <- exposures[i]$season
    res[[i]]$lag <- exposures[i]$lag
    res[[i]]$window <- exposures[i]$window
    res[[i]]$offset <- exposures[i]$offset
    print(res[[i]])
    saveRDS(res,file="results_final/DailyGridSeasons.RDS")
  }
  
  res <- readRDS(file="results_final/DailyGridSeasons.RDS")
  res <- rbindlist(res)
  res[,sig:=pval<0.05]
  res[,sigCasesPerYear:=casesPerYear]
  res[sig==FALSE,sigCasesPerYear:=0]
  res[,season:=factor(season,levels=c("Winter","Spring","Summer","Autumn"))]
  res[,window:=paste0(window," day(s) of runoff exposure (window)")]
  res[,lag:=paste0(lag," day(s) lag")]
  
  q <- ggplot(res)
  q <- q + geom_bar(aes(x=factor(lag),y=sigCasesPerYear,fill=season),stat="identity",pos="dodge")
  q <- q + facet_wrap(~window,scales="free")
  q <- q + theme_SMAO45(base_size=20)
  SMAOgraphs::SMAOpng("results_final/runoff.png")
  print(q)
  dev.off()
}