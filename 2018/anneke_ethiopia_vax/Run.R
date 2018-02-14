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


# Randomly generate between 2-10 healthposts
# rerun using the 3 pentavalance
# baseline of 59%, increase to 68%
# control goes goes from 59 to 62

GenData <- function(nhouseholds=5,val_after_intervention=0.00,val_received_intervention=0.19,val_intercept = 0.38, sdVal=0.11, m=c(1,1.2,1.5)){
  
  data <- data.table(expand.grid(
    districtid=1:10,
    healthcenterid=1:6,
    healthpostid=1:8,
    householdid=1:nhouseholds,
    after_intervention=0:1
  ))
   
  data[,fpc_district:=10]
  data[,fpc_healthcenter:=6]
  data[,fpc_healthpost:=5]
  data[,fpc_households:=500]
  
  data[,fpc_healthpost_above:=fpc_healthcenter*fpc_district*fpc_healthpost]
  
  data[,randomized_intervention:=0]
  data[healthcenterid %in% 1:3, randomized_intervention:=1]
  
  data[, received_intervention:=0]
  data[after_intervention==1 & healthcenterid %in% 1:3, received_intervention:=1]
  
  data[,person_randomized_intervention:=0]
  data[householdid %in% 1:(nhouseholds/2), person_randomized_intervention:=1]
  
  data[, person_received_intervention:=0]
  data[after_intervention==1 & person_randomized_intervention==1, person_received_intervention:=1]
  
  data[,healthpostid:=sprintf("%s_%s_%s",districtid,healthcenterid,healthpostid)]
  data[,healthcenterid:=sprintf("%s_%s",districtid,healthcenterid)]
  data[,districtid:=sprintf("%s",districtid)]
  
  
  x <- unique(data[,c("healthcenterid"),with=F])
  x[,keep:=sample(2:8,size=.N,replace=T)]
  x2 <- unique(data[,c("healthcenterid","healthpostid"),with=F])
  x <- merge(x,x2,by="healthcenterid")
  setorder(x,healthcenterid,healthpostid)
  x[,n:=1:.N,by=healthcenterid]
  x <- x[n<=keep]
  
  data <- data[healthpostid %in% x$healthpostid]
  
  multiplier <- c("district"=m[1],"healthcenter"=m[2],"healthpost"=m[3])
  for(i in c("district","healthcenter","healthpost")){
    id <- sprintf("%sid",i)
    intercept <- sprintf("intercept_%s",i)
    x <- unique(data[,id,with=F])
    x[,(intercept):=rnorm(.N,sd=sdVal*multiplier[i])]
    data <- merge(data,x,by=id)
  }
  
  data[,intercept:=val_intercept]
  
  data[,intercept_after_intervention:=0]
  data[after_intervention==1,intercept_after_intervention:=val_after_intervention]
  
  data[,intercept_received_intervention:=0]
  data[received_intervention==1,intercept_received_intervention:=val_received_intervention]
  
  data[,intercept_person_received_intervention:=0]
  data[person_received_intervention==1,intercept_person_received_intervention:=val_received_intervention]
  
  data[,p:=intercept + intercept_district + intercept_healthcenter + intercept_healthpost + intercept_after_intervention + intercept_received_intervention]
  data[,r:=runif(n=.N)]
  data[,outcome:=0]
  data[r<p,outcome:=1]
  
  data[,p:=intercept + intercept_district + intercept_healthcenter + intercept_healthpost + intercept_after_intervention + intercept_person_received_intervention]
  data[,r:=runif(n=.N)]
  data[,person_outcome:=0]
  data[r<p,person_outcome:=1]
  
  return(data) 
}


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

# Randomly generate between 2-10 healthposts
# rerun using the 3 pentavalance
# baseline of 59%, increase to 68%
# control goes goes from 59 to 62

# YOU CAN USE THIS DATASET TO PLAY WITH
# THE DATA AND VERIFY THAT IT IS BEING
# CREATED PROPERLY
d <- GenData(nhouseholds=10,val_after_intervention=0.05,val_received_intervention=-0.06,val_intercept = 0.16, sdVal=0.04)

CalculateICC <- function(fit){
  res <- data.table(as.data.frame(summary(fit)$varcor))
  res[,icc:=vcov/sum(vcov)]
  res <- res[grp!="Residual",c("grp","icc")]
  return(res)
}


nSims <- 1000

stack <- data.table(expand.grid(outcome=c("no_vax"),
                                nhouseholds=c(8,10:18,20,22,24,26,28,30),
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
  d <- GenData(nhouseholds=s$nhouseholds,val_after_intervention=0.03,val_received_intervention=0.06,val_intercept = 0.59, sdVal=0.06,m=c(1.5,2,2.2))
  
  fit_full <- lme4::lmer(outcome ~ received_intervention + 
                           (1|districtid)+
                           (1|healthcenterid)+
                           (1|healthpostid),data=d[after_intervention==1])
  fit_srs <- lm(person_outcome ~ person_received_intervention, d[after_intervention==1])
  
  icc <- CalculateICC(fit_full)
  
  fit <- lme4::lmer(outcome ~ received_intervention + after_intervention +
                      (1|districtid)+
                      (1|healthcenterid)+
                      (1|healthpostid),data=d)
  fit <- as.data.frame(summary(fit)$coef)
  fit$var <- row.names(fit)
  fit$nhouseholds=s$nhouseholds
  fit$npeoplepersurvey=nrow(d)/2
  fit$var_full <- diag(vcov(fit_full))[2]
  fit$var_srs <- diag(vcov(fit_srs))[2]
  fit$icc_healthpost <- icc$icc[1]
  fit$icc_healthcenterid <- icc$icc[2]
  fit$icc_districtid <- icc$icc[3]
  
  return(fit)
}

# combining results
res <- rbindlist(res)
res <- res[var=="received_intervention"]
# generating useful variables
res[,pval:=PvalFromEstAndSE(Estimate,`Std. Error`)]
res[,sig:=pval<0.05]
res[,pval:=NULL]

# calculating power
res <- res[,lapply(.SD,mean),by=.(var,nhouseholds)]
setnames(res,"sig","power")
res[,deff:=var_full/var_srs]

res[,icc_healthpost:=icc_healthpost+icc_healthcenterid+icc_districtid]
res[,icc_healthcenterid:=icc_healthcenterid+icc_districtid]

res


openxlsx::write.xlsx(res,file=file.path(RAWmisc::PROJ$SHARED_TODAY,"power.xlsx"))


q <- ggplot(res, aes(x=nhouseholds, y=power*100,label=sprintf("DEFF\n%s",RAWmisc::Format(deff))))
q <- q + geom_line()
q <- q + geom_point()
q <- q + geom_text(mapping=aes(y=100*power+0.5),hjust=1,vjust=0)
q <- q + geom_hline(yintercept=80,colour="red")
q <- q + labs(title="Sample size estimation to detect a 6 percentage point change in outcome")
q <- q + labs(caption="\n\nDistrict ICC: 3%, Healthcenter ICC: 9%, Healthpost ICC: 16%")
q <- q + scale_y_continuous("Estimated Power (1000 simulations)")
q <- q + scale_x_continuous("Number of households per health post (with 100% participation)",breaks=seq(0,50,2))
q <- q + theme_gray(base_size=16)
#q <- q + theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust=0.5))
q
RAWmisc::saveA4(q,file.path(RAWmisc::PROJ$SHARED_TODAY,"power.png"))

