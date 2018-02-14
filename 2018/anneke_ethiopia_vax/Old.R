if(.Platform$OS.type=="unix"){
  RAWmisc::AllowFileManipulationFromInitialiseProject()
  RAWmisc::InitialiseProject(
    HOME = "/git/code_major/2018/anneke_ethiopia_vax/",
    RAW = "/analyses/data_raw/code_major/2018/anneke_ethiopia_vax/",
    CLEAN = "/analyses/data_clean/code_major/2018/anneke_ethiopia_vax",
    BAKED = "/analyses/results_baked/code_major/2018/anneke_ethiopia_vax/",
    FINAL = "/analyses/results_final/code_major/2018/anneke_ethiopia_vax/",
    SHARED = "/dropbox/results_shared/code_major/2018/anneke_ethiopia_vax/")
}

#install.packages("RAWmisc", repos="https://raubreywhite.github.com/drat")

library(data.table)
library(foreach)
library(doSNOW)
library(iterators)
library(ggplot2)
library(lme4)
library(survey)

StackIterator <- function(stack, progressFunction) {
  library(data.table)
  it <- icount(nrow(stack))
  
  nextEl <- function() {
    i <- nextElem(it)
    progressFunction(i)
    stack[i]
  }
  
  obj <- list(nextElem=nextEl)
  class(obj) <- c('abstractiter', 'iter')
  obj
}

PvalFromEstAndSE <- function(est,se){
  return(2*(1-pnorm(abs(est/se))))
}

# YOU CAN USE THIS DATASET TO PLAY WITH
# THE DATA AND VERIFY THAT IT IS BEING
# CREATED PROPERLY
d <- GenData(nhouseholds=10,val_after_intervention=0.05,val_received_intervention=-0.06,val_intercept = 0.16, sdVal=0.04)


# TTHIS CALCULATES THE DESIGN EFFECT
# IT ISNT REALLY NECESSARY
if(FALSE){
  
  est_full <- est_srs <- 0
  for(i in 1:50){
    d <- GenData(nhouseholds=10,val_after_intervention=0.05,val_received_intervention=-0.04,val_intercept = 0.16, sdVal=0.04)
    
    fit_full <- lme4::lmer(outcome ~ received_intervention + 
                             (1|districtid)+
                             (1|healthcenterid)+
                             (1|healthpostid),data=d[after_intervention==1])
    fit_srs <- lm(outcome ~ received_intervention, d[after_intervention==1])
    
    est_full <- est_full + diag(vcov(fit_full))[2]
    est_srs <- est_srs + diag(vcov(fit_srs))[2]
    
  }
  
  est_full/est_srs
  
  
  est_randomAllocation_strataSurvey/est_randomAllocation_srsSurvey
  est_full/est_randomAllocation_srsSurvey
  
  deff <- 0
  for(i in 1:100){
    d <- GenData(nhouseholds=10,val_after_intervention=0.05,val_received_intervention=-0.06,val_intercept = 0.16, sdVal=0.04)
    dclus <- svydesign(id=~householdid, 
                       strata=~healthpostid,
                       fpc=~fpc_households,
                       data=d[after_intervention==0],nest=T)
    summary(dclus)
    a <- svymean(~outcome, dclus, deff=T)
    data.frame(a)$deff
    deff <- deff + data.frame(a)$deff
  }
  (deff <- deff/100)
}
################

nSims <- 400

stack <- data.table(expand.grid(outcome=c("fully_vax","no_vax"),
                                nhouseholds=c(8,10,12,14),
                                sim=1:nSims))


# Setting up the progress bar
pb <- RAWmisc::ProgressBarCreate(max=nrow(stack))
assign("pb", pb, envir = .GlobalEnv)

ProgressFunction <- function(n) RAWmisc::ProgressBarSet(pb, n)
assign("ProgressFunction", ProgressFunction, envir = .GlobalEnv)

opts <- list(progress=ProgressFunction)
assign("opts", opts, envir = .GlobalEnv)

# Registering the cluster so we can run it 8x faster
cl <- makeCluster(parallel::detectCores())
registerDoSNOW(cl)

res <- foreach(s=StackIterator(stack, ProgressFunction)) %dopar% {
  library(data.table)
  library(survey)
  
  # generating the datta
  if(s$outcome=="fully_vax"){
    d <- GenData(nhouseholds=s$nhouseholds,val_after_intervention=0.05,val_received_intervention=0.19,val_intercept = 0.42, sdVal=0.05)
  } else {
    d <- GenData(nhouseholds=s$nhouseholds,val_after_intervention=0.05,val_received_intervention=-0.04,val_intercept = 0.16, sdVal=0.04)
  }
  
  # survey methodology, calculating margin of error
  # of seroprevalence in the baseline study
  dclus <- svydesign(id=~householdid, 
                     strata=~healthpostid,
                     fpc=~fpc_households,
                     data=d[after_intervention==0],nest=T)
  summary(dclus)
  
  fit <- svyglm(outcome ~ 1, dclus)
  fit0x <- as.data.frame(summary(fit)$coef)
  fit0x$var <- row.names(fit0x)
  fit0x$model <- "before_intervention_survey"
  
  # we now take the strata level seroprevalence estimates
  # and merge them back into the dataset.
  # this lets us use them as a covariate in the regression
  # analysis (for testing the RCTs intervention), which
  # will increase the power of the study (because we will)
  # then be analysing the "difference from baseline"
  a <- data.table(svyby(~outcome,~healthpostid, dclus, svymean))
  setnames(a,"outcome","baseline_outcome")
  nrow(d)
  d <- merge(d,a,by="healthpostid")
  nrow(d)
  
  # we now run our regression analysis using
  # survey methodology
  dclus <- svydesign(id=~householdid, 
                     strata=~healthpostid,
                     fpc=~fpc_households,
                     data=d[after_intervention==1],nest=T)
  summary(dclus)
  
  # first ignoring the baseline seroprevalence estimates
  fit <- svyglm(outcome ~ received_intervention, dclus)
  fit3a <- as.data.frame(summary(fit)$coef)
  fit3a$var <- row.names(fit3a)
  fit3a$model <- "after_intervention_survey"
  
  # then including the baseline seroprevalence estimates
  fit <- svyglm(outcome ~ received_intervention + baseline_outcome, dclus)
  fit3b <- as.data.frame(summary(fit)$coef)
  fit3b$var <- row.names(fit3b)
  fit3b$model <- "after_intervention_survey(baseline included)"
  
  # now we try using mixed effects models
  # baseline seroprevalence estimates
  formula <- "outcome ~ (1|districtid) + (1|healthcenterid) + (1|healthpostid)"
  
  fit <- lme4::lmer(as.formula(formula),data=d[after_intervention==0])
  fit0 <- as.data.frame(summary(fit)$coef)
  fit0$var <- row.names(fit0)
  fit0$model <- "before_intervention_linear"
  
  # just using after intervention data
  formula <- sprintf("%s + received_intervention",formula)
  
  fit <- lme4::lmer(as.formula(formula),data=d[after_intervention==1])
  fit1 <- as.data.frame(summary(fit)$coef)
  fit1$var <- row.names(fit1)
  fit1$model <- "after_intervention"
  
  # using all data, doing paired tests within healthpost
  # (i.e. comparing against baseline)
  formula <- sprintf("%s + after_intervention",formula)
  
  fit <- lme4::lmer(as.formula(formula),data=d)
  fit2 <- as.data.frame(summary(fit)$coef)
  fit2$var <- row.names(fit2)
  fit2$model <- "baseline+after_intervention"
  
  # saving the data
  retval <- rbind(fit0,fit1,fit2,fit0x[,names(fit0)],fit3a[,names(fit0)],fit3b[,names(fit0)])
  retval$nhouseholds <- s$nhouseholds
  retval$npeoplepersurvey <- nrow(d)/2
  retval$outcome <- s$outcome
  
  return(retval)
}

# combining results
res <- rbindlist(res)
# generating useful variables
res[,pval:=PvalFromEstAndSE(Estimate,`Std. Error`)]
res[,sig:=pval<0.05]
res[,moe:=2*`Std. Error`]

# calculating power
res <- res[,.(est=mean(Estimate),se=mean(`Std. Error`),power=mean(sig),moe=mean(moe)),by=.(var,model,nhouseholds,npeoplepersurvey,outcome)]
res[,effectEstimate:=RAWmisc::FormatEstCIFromEstSE(est,se, exp=F)]
res[,moe:=sprintf("%spp",RAWmisc::Format(moe*100, 1))]

res[model %in% c("before_intervention_linear", "before_intervention_survey"),effectEstimate:=sprintf("%s%% (%s%%, %s%%)",
                                                                                                     RAWmisc::Format(est*100,digits=2),
                                                                                                     RAWmisc::Format((est-1.96*se)*100,digits=2),
                                                                                                     RAWmisc::Format((est+1.96*se)*100,digits=2))]


res[,est:=NULL]
res[,se:=NULL]
setcolorder(res,c("var","outcome","model","npeoplepersurvey","nhouseholds","power","effectEstimate","moe"))
setorder(res,-var,outcome,model,npeoplepersurvey,nhouseholds)

# deleting useless rows
res <- res[!(var=="(Intercept)" & model=="baseline+after_intervention")]
res <- res[!(var=="(Intercept)" & model=="after_intervention")]
res <- res[!(var=="(Intercept)" & model=="after_intervention_survey(baseline included)")]
res <- res[!(var=="(Intercept)" & model=="after_intervention_survey")]
res <- res[!(var=="baseline_outcome")]

res

openxlsx::write.xlsx(res,file=file.path(RAWmisc::PROJ$SHARED_TODAY,"power.xlsx"))




