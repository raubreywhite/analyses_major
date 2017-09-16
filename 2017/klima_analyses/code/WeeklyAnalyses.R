ReverseString <- function(x){
  test <- strsplit(x, NULL)[[1]]
  test_rev <- rev(test)
  paste(test_rev,collapse="")
}

ExtractValues <- function(fit,varOfInterest,removeLead=TRUE,format=TRUE){
  coefs <- data.frame(lme4::fixef(fit))
  names(coefs) <- "est"
  coefs$var <- row.names(coefs)
  row.names(coefs) <- NULL
  coefs$se <- arm::se.fixef(fit)
  coefs <- coefs[,c("var","est","se")]
  if(format){
    coefs$ci95 <- paste0(format(round(exp(coefs$est-1.96*coefs$se),3),nsmall=3)," to ",format(round(exp(coefs$est+1.96*coefs$se),3),nsmall=3))
    coefs$pval <- format(round(2*(1-pnorm(abs(coefs$est/coefs$se))),3),nsmall=3)
    coefs$est <- format(round(exp(coefs$est),3),nsmall=3)
    coefs <- coefs[stringr::str_detect(coefs$var,varOfInterest),]
    coefs <- coefs[,c("var","est","ci95","pval")]
  } else {
    coefs$pval <- 2*(1-pnorm(abs(coefs$est/coefs$se)))
    coefs <- coefs[stringr::str_detect(coefs$var,varOfInterest),]
  }
  if(removeLead) coefs$var <- gsub(varOfInterest,"",coefs$var)
  
  return(coefs)
}

ExtractValuesLM <- function(fit,varOfInterest,removeLead=TRUE,format=TRUE){
  coefs <- data.frame(coef(fit))
  names(coefs) <- "est"
  coefs$var <- row.names(coefs)
  row.names(coefs) <- NULL
  coefs$se <- sqrt(diag(vcov(fit)))
  coefs <- coefs[,c("var","est","se")]
  if(format){
    coefs$ci95 <- paste0(format(round(exp(coefs$est-1.96*coefs$se),3),nsmall=3)," to ",format(round(exp(coefs$est+1.96*coefs$se),3),nsmall=3))
    coefs$pval <- format(round(2*(1-pnorm(abs(coefs$est/coefs$se))),3),nsmall=3)
    coefs$est <- format(round(exp(coefs$est),3),nsmall=3)
    coefs <- coefs[stringr::str_detect(coefs$var,varOfInterest),]
    coefs <- coefs[,c("var","est","ci95","pval")]
  } else {
    coefs$pval <- 2*(1-pnorm(abs(coefs$est/coefs$se)))
    coefs <- coefs[stringr::str_detect(coefs$var,varOfInterest),]
  }
  if(removeLead) coefs$var <- gsub(varOfInterest,"",coefs$var)
  
  return(coefs)
}

WeeklyDifferingConfoundingInt <- function(dataWeek,varOfInterest){
  formula1_0 <- paste0("s_gastro~offset(log(pop))+",
                       "(1|municip)")
  formula1 <-  paste0("s_gastro~offset(log(pop))+",
                      varOfInterest,
                      " + (1|municip)")
  
  formula2_0 <- paste0("s_gastro~offset(log(pop))+",
                       "year + sin(2*pi*(week-1)/52) + cos(2*pi*(week-1)/52) + helligdagIndikator + (1|municip)")
  formula2 <- paste0("s_gastro~offset(log(pop))+",
                     varOfInterest,
                     "+ year + sin(2*pi*(week-1)/52) + cos(2*pi*(week-1)/52) + helligdagIndikator + (1|municip)")
  
  other <- c("dummy_a_temp","dummy_a_precipCorr","dummy_a_runoff")
  other <- other[!other %in% varOfInterest]
  other <- paste0(other,collapse="+")
  formula3_0 <- paste0("s_gastro~offset(log(pop))+",
                       other,
                       "+ year + sin(2*pi*(week-1)/52) + cos(2*pi*(week-1)/52) + helligdagIndikator + (1|municip)")
  formula3 <- paste0("s_gastro~offset(log(pop))+",
                     "dummy_a_temp + dummy_a_precipCorr + dummy_a_runoff",
                     "+ year + sin(2*pi*(week-1)/52) + cos(2*pi*(week-1)/52) + helligdagIndikator + (1|municip)")
  
  res <- vector("list",3)
  pvalues <- rep(NA,3)
  
  fit0 <- lme4::glmer(
    as.formula(formula1_0),
    data=dataWeek,family="poisson")
  
  fit1 <- lme4::glmer(
    as.formula(formula1),
    data=dataWeek,family="poisson")
  pvalues[1] <- anova(fit0,fit1)$"Pr(>Chisq)"[2]
  res[[1]] <- ExtractValues(fit1,varOfInterest)
  print(res)
  
  fit0 <- lme4::glmer(
    as.formula(formula2_0),
    data=dataWeek,family="poisson")
  
  fit1 <- lme4::glmer(
    as.formula(formula2),
    data=dataWeek,family="poisson")
  pvalues[2] <- anova(fit0,fit1)$"Pr(>Chisq)"[2]
  res[[2]] <- ExtractValues(fit1,varOfInterest)
  print(res)
  
  fit0 <- lme4::glmer(
    as.formula(formula3_0),
    data=dataWeek,family="poisson")
  
  fit1 <- lme4::glmer(
    as.formula(formula3),
    data=dataWeek,family="poisson")
  pvalues[3] <- anova(fit0,fit1)$"Pr(>Chisq)"[2]
  res[[3]] <- ExtractValues(fit1,varOfInterest)
  print(res)
  
  results <- do.call(cbind,res)
  row.names(results) <- results[,1]
  results <- results[,-seq(1,ncol(results),4)]
  for(i in seq(3,9,3)){
    results[,i] <- NA
    results[1,i] <- format(round(pvalues[i/3],3),nsmall=3)
    results[1,i] <- ifelse(results[1,i]=="0.000","<0.001",results[1,i])
  }
  return(results)
}


WeeklyDifferingConfounding <- function(dataWeek){
  
  res <- vector("list",3)
  varsOfInterest <- c("dummy_a_temp","dummy_a_precipCorr","dummy_a_runoff")
  
  for(i in 1:3){
    print(i)
    res[[i]] <- WeeklyDifferingConfoundingInt(dataWeek,varsOfInterest[i])
  }
  
  tab <- rbind(res[[1]],res[[2]],res[[3]])
  row.names(tab) <- c(row.names(res[[1]]),row.names(res[[2]]),paste0(row.names(res[[3]])," "))
  
  tab <- htmlTable::htmlTable(tab,
                              rgroup=c("Temperature","Precipitation (corrected)","Runoff"),
                              n.rgroup=c(nrow(res[[1]]),nrow(res[[2]]),nrow(res[[3]])),
                              cgroup=c("Crude","Adjusted (not weather)","Adjusted (weather)"),
                              n.cgroup=c(3,3,3),
                              header=rep(c("IRR","95% Conf Int","P-value"),3))
  saveRDS(tab,file="results_final/WeeklyDummyRegressionsAdjustment.RDS")
}