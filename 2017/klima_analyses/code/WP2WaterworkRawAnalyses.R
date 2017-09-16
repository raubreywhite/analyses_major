WP2WaterworkRawAnalyses <- function(d,ExtractValues,useWeights=F){

  exposures1 <- data.table(expand.grid(variable=unique(d$variable),varOfInterest=c("value0","value1","value2","value3","value4"),stringsAsFactors = F))
  
  exposures1[,season:="Summer"]
  
  exposures2 <- copy(exposures1)
  exposures2[,season:="Spring"]
  
  exposures3 <- copy(exposures1)
  exposures3[,season:="Winter"]
  
  exposures4 <- copy(exposures1)
  exposures4[,season:="Autumn"]
  
  exposures5 <- copy(exposures1)
  exposures5[,season:="All"]
  
  exposuresx <- rbind(exposures1, exposures2, exposures3, exposures4, exposures5)
  if(!useWeights){
    exposuresx[, outcome := "s_outbreakLege"]
  } else {
    exposuresx[, outcome := "s_gastro"]
  }
  
  exposures <- vector("list",5)
  index <- 1
  for(i in c("Totalt","0-4","5-14","15-64","65+")){
    exposuresx[,age:=i]
    exposures[[index]] <- copy(exposuresx)
    index <- index+1
  }
  exposures <- rbindlist(exposures)
  
  
  num <- 0
  a <- Sys.time()
  #dCopy <- d$data
  results <- foreach(exposuresIter=iter(as.data.frame(exposures), by='row')) %do% {
    print(paste0(num,"/",nrow(exposures)))
    print(exposuresIter)
    timeSpent <- as.numeric(difftime(Sys.time(),a,units="mins"))
    print(paste0(round(timeSpent/num*nrow(exposures)-timeSpent)," min left"))
    num <- num + 1
    library(data.table)
    #if(!exists("d")) d <- redisGet("klima-wp2-data")
    #d <- redisGet("klima-wp2-data")
    
    outcomeOfInterest <- exposuresIter$outcome
    varOfInterest <- exposuresIter$varOfInterest
    variablex <- exposuresIter$variable
    fitData <- d[age==exposuresIter$age]
    fitData <- fitData[variable==variablex]
    
    timeout <- 1200
    if(exposuresIter$season!="All"){
      fitData <- fitData[season == exposuresIter$season]
    } 
    
    formulaBase <- paste0(outcomeOfInterest, "~",
                      " factor(month) + (1|municip)")
    if(useWeights){
      formulaBase <- paste0(formulaBase,"+offset(log(s_consult ))")
    }
    formula <- paste0(outcomeOfInterest, "~",
                      varOfInterest,
                      "+ factor(month) + (1|municip)")
    
    formulaSplit <- paste0(outcomeOfInterest, "~",
                           "withinEffect + betweenEffect",
                           "+ factor(month) + (1|municip)")
    
    if(useWeights){
      formula <- paste0(formula,"+offset(log(s_consult ))")
      formulaSplit <- paste0(formulaSplit,"+offset(log(s_consult ))")
    }
    
    txt <- paste0("fitData[,betweenEffect:=mean(",varOfInterest,",na.rm=T),by=municip]")
    eval(parse(text=txt))
    
    txt <- paste0("fitData[,withinEffect:=",varOfInterest,"-betweenEffect]")
    eval(parse(text=txt))
  
    rescaledSD <- sd(fitData[[varOfInterest]],na.rm=T)
    if(rescaledSD==0) return(NULL)
    #if(exposuresIter$variable!="pH") fitData[[varOfInterest]] <- log(1+fitData[[varOfInterest]])
    
    retval <- NULL
    try({
      weights <- NULL
      fam=gaussian
      fn <- lme4::lmer
      
      if(useWeights){
        fam=poisson
        fn <- lme4::glmer
        #weights <- weights/sum(weights)
        fit <- R.utils::evalWithTimeout(
          fn(
            as.formula(formula),
            data = fitData[s_consult>0], family = fam), timeout = timeout, onTimeout = "error")
      } else {
        fit <- R.utils::evalWithTimeout(
          fn(
            as.formula(formula),
            data = fitData[s_consult>0]), timeout = timeout, onTimeout = "error")
      }
      
      retval <- ExtractValues(fit, varOfInterest, removeLead = TRUE, format = FALSE)
      #retval$est <- retval$est/rescaledSD
      #retval$se <- retval$se/rescaledSD
      retval$variable <- exposuresIter$variable
      retval$varOfInterest <- varOfInterest
      retval$outcome <- outcomeOfInterest
      retval$season <- exposuresIter$season
      retval$age <- exposuresIter$age
      
      x1 <- predict(fit,fitData[s_consult>0],allow.new.levels=T)
      if(retval$est>0){
        # harmful
        fitData[[varOfInterest]] <- quantile(fitData[[varOfInterest]],probs=0.25,na.rm=T)
      } else {
        fitData[[varOfInterest]] <- quantile(fitData[[varOfInterest]],probs=0.75,na.rm=T)
      }
      x0 <- predict(fit,fitData[s_consult>0],allow.new.levels=T)
      retval$predictedNormal <- sum(x1,na.rm=T)
      retval$predicted25perc <- sum(x0,na.rm=T)
      retval$attributable <- sum(x1,na.rm=T)-sum(x0,na.rm=T)
      print(retval)
    },TRUE)
    #res[[i]]$pvalVariable <- anova(fit, fit0)$`Pr(>Chisq)`[2]
    retval
  }
  
  #removeQueue("klima-wp2")
  retval <- rbindlist(results)
  retval[,lag:=gsub("^value","",varOfInterest)]
  return(retval)
}


PlotWP2WaterworkRawDataAnalyses <- function(res){
  plotData <- copy(res)
  
  plotData[,dec:="None"]
  plotData[.N*pval < 0.05 & est<0,dec:="Protective"]
  plotData[.N*pval < 0.05 & est > 0, dec := "Harmful"]
  
  plotData[, N := .N]
  plotData[, lag:=factor(lag)]
  
  plotData[, dec := factor(dec, levels=c("Harmful","None","Protective"))]
  plotData[, season := factor(season, levels=c("All","Autumn","Winter","Spring","Summer"))]
  levels(plotData$season) <- c("All seasons","Autumn","Winter","Spring","Summer")
  
  plotData[,age:=factor(age,levels=c("Totalt","0-4","5-14","15-64","65+"))]
  levels(plotData$age) <- c("All ages","0-4 years old","5-14 years old","15-64 years old","65+ years old")
  
    q <- ggplot(plotData, aes(x = lag, y = variable, fill = dec))
    q <- q + geom_tile(data = plotData,alpha=0)
    q <- q + geom_tile(alpha=0.6,colour="white",lwd=0.2)
    q <- q + facet_grid(age ~ season)
    q <- q + scale_fill_manual("Association",values=c("Red","Black","Green"),drop=F)
    q <- q + scale_x_discrete("Weeks lag")
    q <- q + scale_y_discrete("")
    q <- q + RAWmisc::theme_SMAO(base_size=12,v=3)
    
  return(q)
}
