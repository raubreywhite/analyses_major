RAWmisc::AllowFileManipulationFromInitialiseProject()
RAWmisc::InitialiseProject(
  HOME = "/git/code_major/2018/anneke_ethiopia_vax/",
  RAW = "/analyses/data_raw/code_major/2018/anneke_ethiopia_vax/",
  CLEAN = "/analyses/data_clean/code_major/2018/anneke_ethiopia_vax",
  BAKED = "/analyses/results_baked/code_major/2018/anneke_ethiopia_vax/",
  FINAL = "/analyses/results_final/code_major/2018/anneke_ethiopia_vax/",
  SHARED = "/dropbox/results_shared/code_major/2018/anneke_ethiopia_vax/")

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


nSims <- 200

stack <- data.table(expand.grid(outcome=c("fully_vax","no_vax"),
                                ndistricts=c(5,10),
                                nhealthposts=c(2,5),
                                nhouseholds=c(4,10),
                                sim=1:nSims))
if(FALSE){
  stack <- stack[
    outcome=="no_vax" &
      (
        (nhealthposts==2 &
           nhouseholds==5) |
          (nhealthposts==5 &
             nhouseholds==2)
      )
  ]
}

pb <- RAWmisc::ProgressBarCreate(max=nrow(stack))
assign("pb", pb, envir = .GlobalEnv)

ProgressFunction <- function(n) RAWmisc::ProgressBarSet(pb, n)
assign("ProgressFunction", ProgressFunction, envir = .GlobalEnv)

opts <- list(progress=ProgressFunction)
assign("opts", opts, envir = .GlobalEnv)

cl <- makeCluster(parallel::detectCores())
registerDoSNOW(cl)

res <- foreach(s=StackIterator(stack, ProgressFunction)) %dopar% {
  library(data.table)
  library(survey)
  
  if(s$outcome=="fully_vax"){
    d <- GenData(ndistricts=s$ndistricts,nhealthposts=s$nhealthposts,nhouseholds=s$nhouseholds,val_after_intervention=0.05,val_received_intervention=0.19,val_intercept = 0.42, sdVal=0.075)
  } else {
    d <- GenData(ndistricts=s$ndistricts,nhealthposts=s$nhealthposts,nhouseholds=s$nhouseholds,val_after_intervention=0.05,val_received_intervention=-0.06,val_intercept = 0.16, sdVal=0.06)
  }
  
  #using the survey package
  
  dclus <- svydesign(id=~districtid+healthcenterid+healthpostid+householdid, 
                     fpc=~fpc_district+fpc_healthcenter+fpc_healthpost+fpc_households,
                     data=d[after_intervention==0])
  summary(dclus)
  a <- svymean(~outcome, dclus, deff=T)
  aDEFF <- data.frame(a)$deff
  
  fit <- svyglm(outcome ~ 1, dclus)
  fit0x <- as.data.frame(summary(fit)$coef)
  fit0x$var <- row.names(fit0x)
  fit0x$model <- "before_intervention_survey"
  fit0x$deff <- aDEFF
  fit0x$icc <- -99
  
  dclus <- svydesign(id=~districtid+healthcenterid+healthpostid+householdid, 
                     fpc=~fpc_district+fpc_healthcenter+fpc_healthpost+fpc_households,
                     data=d)
  fit <- svyglm(outcome ~ after_intervention + received_intervention, dclus)
  
  fitx <- as.data.frame(summary(fit)$coef)
  fitx$var <- row.names(fitx)
  fitx$model <- "survey(baseline+after)"
  fitx$deff <- -99
  fitx$icc <- -99
  
  dclus <- svydesign(id=~districtid+healthcenterid+healthpostid+householdid, 
                     fpc=~fpc_district+fpc_healthcenter+fpc_healthpost+fpc_households,
                     data=d[after_intervention==1])
  fit <- svyglm(outcome ~  received_intervention, dclus)
  
  fity <- as.data.frame(summary(fit)$coef)
  fity$var <- row.names(fity)
  fity$model <- "survey(after)"
  fity$deff <- -99
  fity$icc <- -99
  
  # using lme4
 
  formula <- "outcome ~ (1|districtid) + (1|healthcenterid)"
  if(s$nhealthposts>1 & s$nhouseholds>1){
    formula <- sprintf("%s + (1|healthpostid)",formula)
  }
  
  fit <- lme4::lmer(as.formula(formula),data=d[after_intervention==0])
  fit0a <- as.data.frame(summary(fit)$coef)
  fit0a$var <- row.names(fit0a)
  fit0a$model <- "before_intervention_linear"
  
  (fakeMean <- mean(d[after_intervention==0]$outcome))
  fakeData <- sample(c(0,1),size=sum(d$after_intervention==0),replace=T,prob=c(1-fakeMean,fakeMean))
  varNODEFF <- coef(summary(lm(fakeData~1)))[1,"Std. Error"]^2
  varDEFF <- coef(summary(fit))[1,"Std. Error"]^2
  
  fit0a$deff <- varDEFF/varNODEFF
 
  v <- data.table(as.data.frame(lme4::VarCorr(fit)))[4:1]
  v[2:4,vcovsum:=cumsum(vcov)]
  v[,nested_icc:=vcovsum/sum(vcov,na.rm=T)]
  fit0a$icc<- v$nested_icc[4]
  
  ###
  
  fit <- lme4::glmer(as.formula(formula),data=d[after_intervention==0],family=binomial())
  fit0b <- as.data.frame(summary(fit)$coef)
  fit0b$var <- row.names(fit0b)
  fit0b$model <- "before_intervention_logistic"
  fit0b <- fit0b[,-which(names(fit0b)=="Pr(>|z|)")]
  fit0b$deff <- -99
  fit0b$icc <- -99
  names(fit0b) <- names(fit0a)
  
  formula <- sprintf("%s + received_intervention",formula)
  
  fit <- lme4::lmer(as.formula(formula),data=d[after_intervention==1])
  fit1 <- as.data.frame(summary(fit)$coef)
  fit1$var <- row.names(fit1)
  fit1$model <- "after_intervention"
  fit1$deff <- -99
  fit1$icc <- -99
  
  formula <- sprintf("%s + after_intervention",formula)
  
  fit <- lme4::lmer(as.formula(formula),data=d)
  fit2 <- as.data.frame(summary(fit)$coef)
  fit2$var <- row.names(fit2)
  fit2$model <- "baseline+after_intervention"
  fit2$deff <- -99
  fit2$icc <- -99
  
  retval <- rbind(fit1,fit2,fit0a,fit0b,fit0x[,names(fit0b)],fitx[,names(fit0b)],fity[,names(fit0b)])
  retval$ndistricts <- s$ndistricts
  retval$nhealthposts <- s$nhealthposts
  retval$nhouseholds <- s$nhouseholds
  retval$npeoplepersurvey <- nrow(d)/2
  retval$outcome <- s$outcome
  
  return(retval)
}

res <- rbindlist(res)
res[,pval:=PvalFromEstAndSE(Estimate,`Std. Error`)]
res[,sig:=pval<0.05]
res[,moe:=2*`Std. Error`]
res[model=="before_intervention_logistic",moe:=(boot::inv.logit(Estimate+1.96*`Std. Error`)-boot::inv.logit(Estimate-1.96*`Std. Error`))/2]
res <- res[,.(est=mean(Estimate),se=mean(`Std. Error`),power=mean(sig),moe=mean(moe),icc=mean(icc),deff=mean(deff)),by=.(var,model,ndistricts,nhealthposts,nhouseholds,npeoplepersurvey,outcome)]
res[,effectEstimate:=RAWmisc::FormatEstCIFromEstSE(est,se, exp=F)]
res[,moe:=sprintf("%spp",RAWmisc::Format(moe*100, 1))]
res[,icc:=sprintf("%s%%",RAWmisc::Format(icc*100,1))]
res[,deff:=sprintf("%s",RAWmisc::Format(deff, 1))]
res[icc=="-9900.0%",icc:=""]
res[deff=="-99.0",deff:=""]
res[model=="before_intervention_logistic",effectEstimate:=sprintf("%s%% (%s%%, %s%%)",
                                                                  RAWmisc::Format(boot::inv.logit(est)*100,digits=2),
                                                                  RAWmisc::Format(boot::inv.logit(est-1.96*se)*100,digits=2),
                                                                  RAWmisc::Format(boot::inv.logit(est+1.96*se)*100,digits=2))]
res[model %in% c("before_intervention_linear", "before_intervention_survey"),effectEstimate:=sprintf("%s%% (%s%%, %s%%)",
                                                                  RAWmisc::Format(est*100,digits=2),
                                                                  RAWmisc::Format((est-1.96*se)*100,digits=2),
                                                                  RAWmisc::Format((est+1.96*se)*100,digits=2))]



res[,est:=NULL]
res[,se:=NULL]

setcolorder(res,c("var","outcome","npeoplepersurvey","ndistricts","nhealthposts","nhouseholds","model","power","effectEstimate","moe","icc","deff"))
setorder(res,-var,outcome,npeoplepersurvey,ndistricts,nhealthposts,nhouseholds,model)
res

res[model %in% c(
  "before_intervention_linear",
  "before_intervention_logistic",
  "before_intervention_survey",
  "survey")]

res <- res[!(!model %in% c("before_intervention_logistic","before_intervention_linear", "before_intervention_survey") & var=="(Intercept)")]

openxlsx::write.xlsx(res,file=file.path(RAWmisc::PROJ$SHARED_TODAY,"power.xlsx"))

# Fully vaccinated 
## Observing the data

# grp  icc
# 1:   healthpostid 0.11
# 2: healthcenterid 0.06
# 3:     districtid 0.04
# 4:       Residual 0.79

deff <- c()
for(i in 1:10){
  d <- GenData(nhealthposts=5,nhouseholds=5,val_after_intervention=0.05,val_received_intervention=0.19,val_intercept = 0.42, sdVal=0.07)
  (fakeMean <- mean(d[after_intervention==0]$outcome))
  fakeData <- sample(c(0,1),size=sum(d$after_intervention==0),replace=T,prob=c(1-fakeMean,fakeMean))
  varNODEFF <- coef(summary(lm(fakeData~1)))[1,"Std. Error"]^2
  
  fit <- lme4::lmer(outcome ~ (1|districtid) + (1|healthcenterid) + (1|healthpostid),data=d[after_intervention==0])
  varDEFF <- coef(summary(fit))[1,"Std. Error"]^2

  (deff <- c(deff,varDEFF/varNODEFF))
}
mean(deff)

d[after_intervention==0,.(outcome=mean(outcome)),by=.(districtid)]
d[after_intervention==1 & received_intervention==0,.(outcome=mean(outcome)),by=.(districtid)]
d[after_intervention==1 & received_intervention==1,.(outcome=mean(outcome)),by=.(districtid)]

fit <- lme4::lmer(outcome ~ received_intervention + after_intervention+(1|districtid) + (1|healthcenterid) + (1|healthpostid),data=d)
v <- data.table(as.data.frame(lme4::VarCorr(fit)))[4:1]
v[2:4,vcovsum:=cumsum(vcov)]
v[,nested_icc:=vcovsum/sum(vcov,na.rm=T)]
v[,nested_icc:=sprintf("%s%%",RAWmisc::Format(nested_icc*100,2))]
print(v)


# grp  icc
# 1:   healthpostid 0.05
# 2: healthcenterid 0.03
# 3:     districtid 0.02
# 4:       Residual 0.90

res <- vector("list",100)
pb <- RAWmisc::ProgressBarCreate(min=0,max=length(res))
for(i in 1:length(res)){
  RAWmisc::ProgressBarSet(pb,i)
  d <- GenData(nhealthposts=5,nhouseholds=5,val_after_intervention=0.05,val_received_intervention=-0.06,val_intercept = 0.16, sdVal=0.06)
  mean(d$fully_vaccinated)
  
  fit <- lme4::lmer(outcome ~ received_intervention + (1|districtid) + (1|healthcenterid) + (1|healthpostid),data=d)
  fit
  
  v <- data.table(as.data.frame(lme4::VarCorr(fit)))
  v[,icc:=vcov/sum(vcov)]
  res[[i]] <- copy(v)
}
res <- rbindlist(res)
res <- res[,.(icc=mean(icc)),by=.(grp)]
res[,icc:=RAWmisc::Format(icc,2)]
print(res)


properRes

with(d,xtabs(~districtid))
with(d,xtabs(~districtid + received_intervention))
with(d[after_intervention==0],xtabs(~districtid + received_intervention))
with(d[after_intervention==1],xtabs(~districtid + received_intervention))

with(d,xtabs(~healthcenterid + received_intervention))
with(d[after_intervention==0],xtabs(~healthcenterid + received_intervention))
with(d[after_intervention==1],xtabs(~healthcenterid + received_intervention))