WP2Analyses <- function(d,ExtractValues,useWeights=F){
  #UploadWP2DataToRedis(d)
  
  exposures1 <- rbind(d$cat_wp950_exposures_a_runoff,d$cat_wp950_exposures_a_precipCorr,d$cat_wp950_exposures_c_rain,
                      d$wp950_exposures_a_runoff, d$wp950_exposures_a_precipCorr, d$wp950_exposures_c_rain,
                      d$cat_wp990_exposures_a_runoff, d$cat_wp990_exposures_a_precipCorr, d$cat_wp990_exposures_c_rain,
                      d$wp990_exposures_a_runoff, d$wp990_exposures_a_precipCorr, d$wp990_exposures_c_rain)
  
  exposures1[,season:="Summer"]
  
  exposures2 <- copy(exposures1)
  exposures2[,season:="Spring"]
  
  exposures3 <- copy(exposures1)
  exposures3[,season:="Winter"]
  
  exposures4 <- copy(exposures1)
  exposures4[,season:="Autumn"]
  
  exposures5 <- copy(exposures1)
  exposures5[,season:="All"]
  
  exposures1 <- rbind(exposures1, exposures2, exposures3, exposures4, exposures5)
  exposures1[, outcome := "s_outbreakLege"]
  
  exposures2 <- copy(exposures1)
  exposures2[, outcome := "s_outbreakAll"]
  
  exposuresx <- rbind(exposures1, exposures2)
  
  exposures <- vector("list",5)
  index <- 1
  for(i in c("Totalt","0-4","5-14","15-64","65+")){
    exposuresx[,age:=i]
    exposures[[index]] <- copy(exposuresx)
    index <- index+1
  }
  exposuresx <- rbindlist(exposures)
  
  
  exposures <- vector("list",3)
  index <- 1
  for(i in c("50%+","Under 500","Under 10k","10k+","All")){
    exposuresx[,water:=i]
    exposures[[index]] <- copy(exposuresx)
    index <- index+1
  }
  exposures <- rbindlist(exposures)
  exposures <- exposures[!stringr::str_detect(varOfInterest,"^cat_")]
  exposures <- exposures[!stringr::str_detect(varOfInterest,"^wp990_")]
  exposures <- exposures[outcome=="s_outbreakLege"]
  
  if(useWeights){
    exposures[,outcome:="s_gastro"]
    exposures <- exposures[water=="All"]
  }
  
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
    fitData <- d$data[age==exposuresIter$age]
    
    timeout <- 1200
    if(exposuresIter$season!="All"){
      fitData <- fitData[season == exposuresIter$season]
    }
    if(exposuresIter$water %in% c("Under 10k","10k+")){
      fitData <- fitData[waterworkcat == exposuresIter$water]
    }
    if(exposuresIter$water %in% c("Under 500")){
      fitData <- fitData[waterworkcat2 == exposuresIter$water]
      timeout <- 240
    }
    if(exposuresIter$water %in% c("50%+")){
      fitData <- fitData[notWaterworkcat == exposuresIter$water]
      timeout <- 240
    }
    
    formula <- paste0(outcomeOfInterest, "~",
                      varOfInterest,
                      "+ factor(month) + (1|municip)")
    
    formulaSplit <- paste0(outcomeOfInterest, "~",
                      "withinEffect + betweenEffect",
                      "+ factor(month) + (1|municip)")
    
    ### NEW
    
    if(useWeights){
      formula <- paste0(formula,"+offset(log(s_consult ))")
      formulaSplit <- paste0(formulaSplit,"+offset(log(s_consult ))")
    }
    
    rescaledMean <- mean(fitData[[varOfInterest]],na.rm=T)
    fitData[[varOfInterest]] <- fitData[[varOfInterest]] - rescaledMean
    rescaledSD <- sd(fitData[[varOfInterest]],na.rm=T)
    fitData[[varOfInterest]] <- fitData[[varOfInterest]]/rescaledSD
    
    ### NEW END
    
    
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
      retval$est <- retval$est/rescaledSD
      retval$se <- retval$se/rescaledSD
      retval$lag <- exposuresIter$lag
      retval$window <- exposuresIter$window
      retval$varOfInterest <- varOfInterest
      retval$outcome <- outcomeOfInterest
      retval$season <- exposuresIter$season
      retval$age <- exposuresIter$age
      retval$water <- exposuresIter$water
      
      x1 <- predict(fit,fitData[s_consult>0],allow.new.levels=T)
      if(retval$est>0){
        # harmful
        fitData[[varOfInterest]] <- min(fitData[[varOfInterest]],na.rm=T)
      } else {
        fitData[[varOfInterest]] <- min(fitData[[varOfInterest]],na.rm=T)
      }
      x0 <- predict(fit,fitData[s_consult>0],allow.new.levels=T)
      retval$predictedNormal <- sum(x1,na.rm=T)
      retval$predictedNone <- sum(x0,na.rm=T)
      retval$attributable <- sum(x1,na.rm=T)-sum(x0,na.rm=T)
      
      print(retval)
    },TRUE)
    
    #res[[i]]$pvalVariable <- anova(fit, fit0)$`Pr(>Chisq)`[2]
    retval
  }

  
  return(rbindlist(results))
}


WP2Graphs <- function(all){
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
  levels(plotData$season) <- c("All seasons\n","Autumn\n","Winter\n","Spring\n","Summer\n")
  plotData[, varOfInterest := factor(varOfInterest,levels=c("c_rain","a_precipCorr","a_runoff"))]
  levels(plotData$varOfInterest) <- c("Rain\n(Municip. Centre)","Corrected precipication\n(Municip. Average)","Runoff\n(Municip. Average)")
  plotData[,water:=factor(water,levels=c("All","Under 10k","10k+","Under 500","50%+"))]
  plotData[,age:=factor(age,levels=c("Totalt","0-4","5-14","15-64","65+"))]
  levels(plotData$age) <- c("All ages\n","0-4 years old\n","5-14 years old\n","15-64 years old\n","65+ years old\n")
  
  
  
  data <- vector("list", 5)
  plots <- vector("list", 5)
  
  data[[1]] <- plotData[water=="All"]
  data[[2]] <- plotData[water=="Under 10k"]
  data[[3]] <- plotData[water=="10k+"]
  data[[4]] <- plotData[water=="Under 500"]
  data[[5]] <- plotData[water=="50%+"]
  for (i in 1:5) {
    if(nrow(data[[i]])==0) next
    q <- ggplot(data[[i]], aes(x = lag, y = varOfInterest, fill = dec))
    q <- q + geom_tile(data = data[[1]],alpha=0)
    q <- q + geom_tile(alpha=0.6,colour="white",lwd=0.2)
    q <- q + geom_text(data = data[[i]][pval * N < 0.05 & dec != "None"], label = "+", size = 14)
    q <- q + geom_text(data = data[[i]][pval * N < 0.05 & dec != "None"], label = "+", size = 8, colour="white")
    q <- q + facet_grid(age ~ season)
    q <- q + scale_fill_manual(values=c("Red","Black","Green"),drop=F)
    q <- q + scale_x_discrete("Weeks lag")
    q <- q + scale_y_discrete("")
    q <- q + RAWmisc::theme_SMAO(14)
    if(i==1){
      q <- q + labs(title="All municipalities\n")
    } else if(i==2){
      q <- q + labs(title="Municipalities with average waterwork size under 10k\n")
    } else if(i==3){
      q <- q + labs(title="Municipalities with average waterwork size 10k+\n")
    } else if(i==4){
      q <- q + labs(title="Municipalities with average waterwork size under 500\n")
    } else if(i==5){
      q <- q + labs(title="Municipalities with 50%+ not having waterworks\n")
    }
    q <- q + theme(axis.line.y = NULL)
    q <- q + theme(axis.line.x = NULL)
    q <- q + theme(axis.ticks.length = unit(0,"lines"))
    q <- q + theme(axis.text.x = element_text(vjust=0.5))
    q <- q + theme(axis.text.y = element_text(hjust=1))
    q <- q + theme(axis.text = element_text(margin = rep(unit(1,"lines"),4)))
    plots[[i]] <- q
  }
  return(plots)
}