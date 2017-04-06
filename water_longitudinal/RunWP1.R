RAWmisc::InitialiseProject(
  PROJHOME = "/analyses/code_major/water_longitudinal/",
  PROJRAW = "/dropbox/data_raw/water_longitudinal/",
  PROJCLEAN = "/analyses/data_clean/water_longitudinal",
  PROJBAKED = "/analyses/results_baked/water_longitudinal/",
  PROJFINAL = "/analyses/results_final/water_longitudinal/",
  PROJSHARED = "/dropbox/results_shared/water_longitudinal/")

suppressWarnings(suppressMessages(library(data.table)))
suppressWarnings(suppressMessages(library(ggplot2)))

assign("RUN_ALL", TRUE, envir=globalenv())

unlink(file.path(RPROJ$PROJSHARED,lubridate::today()), recursive=TRUE, force=TRUE)
dir.create(file.path(RPROJ$PROJSHARED,lubridate::today()))


d <- GetNorwegianHousing()[total>0,.(
  enebolig=sum(enebolig),
  tomannsbolig=sum(tomannsbolig),
  rekkehus=sum(rekkehus),
  boligblokk=sum(boligblokk),
  bofellesskap=sum(bofellesskap),
  annen=sum(annen)+sum(uoppgitt)
), by=.(kommune)]

d <- melt.data.table(d,id="kommune",variable.factor = F)
d[,waterworkSize:=sum(value),by=kommune]
d <- reshape::untable(df=d, num=round(d$value))
d[, value:=NULL]
setnames(d,"variable","housing")
d[,waterExposure := SampleWaterExposure(n=.N)]
setorder(d,kommune)
d[, personID := 1:.N]
d[,globalIntercept := -3.15]
d[,personSpecificIntercept := rnorm(.N, sd=1.15)]

kommuner <- unique(d[,c("kommune","waterworkSize"),with=F])
kommuner[,waterworkID := 1:.N]
kommuner[,waterworkSpecificIntercept := rnorm(.N, sd=1.15)]
kommuner[,waterworkCategory := cut(waterworkSize, breaks=c(0,5000,10000,25000,100000,1000000),include.lowest = T, labels=F)]
kommuner[,waterworkSize:=NULL]

kommuner[,denomWW:=.N,by=waterworkCategory]
xtabs(~kommuner$waterworkCategory)

kommuner[,waterworkNumerator:=0]
kommuner[waterworkCategory==1,waterworkNumerator:=100]
kommuner[waterworkCategory==2,waterworkNumerator:=25]
kommuner[waterworkCategory==3,waterworkNumerator:=10]
kommuner[waterworkCategory==4,waterworkNumerator:=5]
kommuner[waterworkCategory==5,waterworkNumerator:=5]
kommuner[,waterworkSamplingProb:=waterworkNumerator/denomWW]
kommuner[,denomWW:=NULL]

w <- copy(kommuner)
kommuner[,waterworkNumerator:=NULL]
kommuner[,waterworkSamplingProb:=NULL]

w[, kommune:=NULL]
w[, waterworkSpecificIntercept:=NULL]


h <- GetNorwegianHousingNumHouses()[,.(
  enebolig=sum(enebolig),
  tomannsbolig=sum(tomannsbolig),
  rekkehus=sum(rekkehus),
  boligblokk=sum(boligblokk),
  bofellesskap=sum(bofellesskap),
  annen=sum(annen)+sum(uoppgitt)
), by=.(kommune)]
h <- melt.data.table(h,id="kommune",variable.factor = F)
setnames(h,"variable","housing")
setnames(h,"value","numberHouses")

kommunerHousing <- d[,.(N=.N),by=.(kommune,housing)]
kommunerHousing[,Total:=sum(N),by=kommune]
nrow(kommunerHousing)
kommunerHousing <- merge(kommunerHousing,h,by=c("kommune","housing"),all.x=T)
nrow(kommunerHousing)
kommunerHousing[,numberHousesTotal := sum(numberHouses),by=kommune]
kommunerHousing[,personSamplingProb := (1/N)*(numberHouses/numberHousesTotal)]
kommunerHousing <- kommunerHousing[,c("kommune","housing","personSamplingProb"),with=F]

d <- merge(d,kommuner,by="kommune")
d <- merge(d,kommunerHousing,by=c("kommune","housing"))
d[, housingEffect:=0]
d[housing=="tomannsbolig", housingEffect:=0.25]
d[housing=="rekkehus", housingEffect:=0.5]
d[housing=="boligblokk", housingEffect:=0.75]
d[housing=="bofellesskap", housingEffect:=1]
d[housing=="annen", housingEffect:=1.25]
d

CreateOutcome <- function(dx, oddsRatio=c(1.05, 1.04, 1.03, 1.02, 1.00), longitudinal=T){
  data <- copy(dx)
  data[,beta:=0]
  for(i in 1:length(oddsRatio)) data[waterworkCategory==i, beta:=log(oddsRatio[i])]
  data[,ylatent := globalIntercept + personSpecificIntercept + waterworkSpecificIntercept + housingEffect + beta*waterExposure] # + data$value
  data[,prob := exp(ylatent)/(1 + exp(ylatent))]
  
  if(longitudinal) data <- reshape::untable(df=data, num=12)
  
  data[,runis := runif(.N,0,1)]
  data[,y := ifelse(runis < prob,1,0)]
  
  return(data)
}

CreateSample <- function(n=c(4000,1000,1000,1000,1000), oddsRatio=c(1.05, 1.04, 1.03, 1.02, 1.00), d,w){
  data <- vector("list",length=length(n))
  for(i in 1:length(data)){
    sData <- w[waterworkCategory==i]
    numerator <- sData$waterworkNumerator[1]
    prob <- sData$waterworkSamplingProb[1]
    ww <- sample(sData$waterworkID,numerator,prob=rep(prob,nrow(sData)))
    
    peopleData <- vector("list",length(ww))
    for(j in 1:length(peopleData)){
      sData <- d[waterworkID==ww[j]]
      pid <- sample(sData$personID,round(n[i]/numerator),prob=sData$personSamplingProb)
      peopleData[[j]] <- sData[personID %in% pid]
    }
    data[[i]] <- rbindlist(peopleData)
  }
  data <- rbindlist(data)
  return(CreateOutcome(data, oddsRatio=oddsRatio))
}

res <- vector("list",500)
pb <- txtProgressBar(min=0,max=length(pval),style=3)
for(i in 1:length(res)){
  data <- CreateSample(n=c(4000,1000,1000,1000,1000),d=d,w=w)
  
  a <- lme4::lmer(y~waterExposure*factor(waterworkCategory)+(1|waterworkID)+(1|personID),data=data)
  baselineRisk <- coef(summary(a))[1,1]
  L <- rbind(
    cbind(0,1,0,0,0,0,0,0,0,0),
    cbind(0,1,0,0,0,0,1,0,0,0),
    cbind(0,1,0,0,0,0,0,1,0,0),
    cbind(0,1,0,0,0,0,0,0,1,0),
    cbind(0,1,0,0,0,0,0,0,0,1)
  )
  test <- summary(multcomp::glht(a,L),test=multcomp::adjusted("none"))$test
  coefficients <- test$coefficients
  pvalues <- test$pvalues
  riskRatios <- (baselineRisk+coefficients)/baselineRisk
  res[[i]] <- data.frame(riskRatios,coefficients,pvalues,category=1:5,id=i)
  
  setTxtProgressBar(pb,i)
}
close(pb)

res <- rbindlist(res)
xGood <- res[,.(
  power=mean(pvalues<0.05),
  est=mean(coefficients),
  rr=mean(riskRatios)
),by=.(category)]
xGood

res <- vector("list",500)
pb <- txtProgressBar(min=0,max=length(pval),style=3)
for(i in 1:length(res)){
  data <- CreateSample(n=c(3200,1350,1350,1350,1350),d=d,w=w)
  
  a <- lme4::lmer(y~waterExposure*factor(waterworkCategory)+(1|waterworkID)+(1|personID),data=data)
  baselineRisk <- coef(summary(a))[1,1]
  L <- rbind(
    cbind(0,1,0,0,0,0,0,0,0,0),
    cbind(0,1,0,0,0,0,1,0,0,0),
    cbind(0,1,0,0,0,0,0,1,0,0),
    cbind(0,1,0,0,0,0,0,0,1,0),
    cbind(0,1,0,0,0,0,0,0,0,1)
  )
  test <- summary(multcomp::glht(a,L),test=multcomp::adjusted("none"))$test
  coefficients <- test$coefficients
  pvalues <- test$pvalues
  riskRatios <- (baselineRisk+coefficients)/baselineRisk
  res[[i]] <- data.frame(riskRatios,coefficients,pvalues,category=1:5,id=i)
  
  setTxtProgressBar(pb,i)
}
close(pb)

res <- rbindlist(res)
xBad <- res[,.(
  power=mean(pvalues<0.05),
  est=mean(coefficients),
  rr=mean(riskRatios)
),by=.(category)]
xBad

xGood
xBad

xGood$power[1]-xBad$power[1]

x$est[1]/x$est[2]
x$est[1]/x$est[3]
x$est[1]/x$est[4]

p0 <- x$est[1]/(1-x$est[1])
p1 <- x$est[2]/(1-x$est[2])
1.05/1.04

mean(pval<0.05)

pvalCombined <- coef(summary(fit1))[2,4]
logORCombined <- coef(summary(fit1))[2,1]

data <- CreateSample(d=d,w=w)
dataFull <- CreateOutcome(d,longitudinal=F)
f <- lme4::lmer(y~waterExposure*factor(waterworkCategory)+factor(housing)+(1|waterworkID),data=dataFull)
L <- rbind(
  cbind(0,1,0,0,0,0,0,0,0,0),
  cbind(0,1,0,0,0,0,1,0,0,0),
  cbind(0,1,0,0,0,0,0,1,0,0),
  cbind(0,1,0,0,0,0,0,0,1,0),
  cbind(0,1,0,0,0,0,0,0,0,1)
)
test <- summary(multcomp::glht(f,L),test=multcomp::adjusted("none"))$test
coefficients <- test$coefficients
pvalues <- test$pvalues
coefficients[1]/coefficients[2]
coefficients[1]/coefficients[3]

#fit1 <- lme4::glmer(y~waterExposure+(1|personSpecificIntercept),family=binomial,data=data, verbose=1)
fit0_a <- lme4::lmer(y~waterExposure+(1|waterworkID),data=dataFull, verbose=2)
fit0_b <- lme4::lmer(y~waterExposure+factor(housing)+(1|waterworkID),data=dataFull, verbose=2)
fit0_c <- lme4::lmer(y~waterExposure*factor(waterworkCategory)+factor(housing)+(1|waterworkID),data=dataFull, verbose=2)
fit1_a <- lme4::lmer(y~waterExposure+(1|personID),data=data, verbose=2)
fit1_b <- lme4::lmer(y~waterExposure+factor(housing)+(1|personID),data=data, verbose=2)
fit1_c <- lme4::lmer(y~waterExposure+factor(housing)+(1|personID)+(1|waterworkID),data=data, verbose=2)
fit1_d <- lme4::lmer(y~waterExposure*factor(waterworkCategory)+factor(housing)+(1|personID)+(1|waterworkID),data=data, verbose=2)
fit1_e <- lme4::lmer(y~waterExposure*factor(waterworkCategory)+(1|personID)+(1|waterworkID),data=data, verbose=2)

coef(fit0_a)[[1]][1,]
coef(fit0_b)[[1]][1,]
coef(fit0_c)[[1]][1,]

coef(fit1_a)[[1]][1,]
coef(fit1_b)[[1]][1,]
coef(fit1_c)[[1]][1,]
coef(fit1_d)[[1]][1,]
coef(fit1_e)[[1]][1,]

fitb0_a <- lme4::glmer(y~waterExposure+(1|waterworkID),family=binomial,data=dataFull, verbose=2, nAGQ=0)
fitb0_b <- lme4::glmer(y~waterExposure+factor(housing)+(1|waterworkID),family=binomial,data=dataFull, verbose=2)
fitb0_c <- lme4::glmer(y~waterExposure*factor(waterworkCategory)+factor(housing)+(1|waterworkID),family=binomial,data=dataFull, verbose=2)
fitb1_a <- lme4::glmer(y~waterExposure+(1|personID),family=binomial,data=data, verbose=2)
fitb1_b <- lme4::glmer(y~waterExposure+factor(housing)+(1|personID),family=binomial,data=data, verbose=2)
fitb1_c <- lme4::glmer(y~waterExposure+factor(housing)+(1|personID)+(1|waterworkID),family=binomial,data=data, verbose=2)
fitb1_d <- lme4::glmer(y~waterExposure*factor(waterworkCategory)+factor(housing)+(1|personID)+(1|waterworkID),family=binomial,data=data, verbose=2, nAGQ=0)
fitb1_e <- lme4::glmer(y~waterExposure*factor(waterworkCategory)+(1|personID)+(1|waterworkID),family=binomial,data=data, verbose=2, nAGQ=0)

exp(coef(fitb0_a)[[1]][1,])
exp(coef(fitb0_b)[[1]][1,])
exp(coef(fitb1_a)[[1]][1,])
exp(coef(fitb1_b)[[1]][1,])
exp(coef(fitb1_c)[[1]][1,])

exp(coef(fitb0_a)[[1]][1,])

exp(coef(fitb0_c)[[1]][1,])

exp(coef(fitb1_d)[[1]][1,])
exp(coef(fitb1_e)[[1]][1,])



coef(fit1)
summary(fit1)

length(unique(d$kommune))
sum(d$samplingProb)

CreatePeople <- function(){
  
}

CreateData <- function(npeople=c(100,200),int=-3.15,stddevPeople=1.15,stddevWaterwork=1.15,oddsRatio=c(1.0,1.05), nReps=12){
  beta <- log(oddsRatio)
  
  data <- vector("list",length=length(npeople))
  for(i in 1:length(data)){
  data[[i]] <- data.frame(
    wwlevel=i,
    id=1:npeople[i],
    exposure=SampleWaterExposure(n=npeople[i]),
    globalIntercept=int,
    personSpecificIntercept=rnorm(npeople[i],sd=stddev)
  )
  }
  data$ylatent <- data$globalIntercept + data$personSpecificIntercept + rep(beta,each=nPeople/2)*data$exposure # + data$value
  data$prob <- exp(data$ylatent)/(1 + exp(data$ylatent))
  
  dataExpanded <- data[rep(row.names(data), nReps),]
  runis <- runif(nrow(data),0,1)
  data$y <- ifelse(runis < data$prob,1,0)
  
  runis <- runif(nrow(dataExpanded),0,1)
  dataExpanded$y <- ifelse(runis < dataExpanded$prob,1,0)
  
  mean(data$y)
  mean(dataExpanded$y)
  
  #summary(glm(y~exposure,family=binomial,data=data[data$measure==1,]))
  #summary(glm(y~exposure,family=binomial,data=data))
  fit1 <- lme4::glmer(y~exposure+wwlevel+(1|id),family=binomial,data=dataExpanded)
  fit2 <- lme4::glmer(y~exposure*wwlevel+(1|id),family=binomial,data=dataExpanded)
  anovaPval <- anova(fit1,fit2)$`Pr(>Chisq)`[2]
  pvalCombined <- coef(summary(fit1))[2,4]
  logORCombined <- coef(summary(fit1))[2,1]
  
  res <- data.frame(anovaPval,pvalCombined,logORCombined)
  return(res)
}


# water exposures
data <- data.table(x = SampleWaterExposure(10000))
meanWater <- mean(data$x)
data <- data[, .(y = .N), by = .(x)]
data[, p := y / sum(y)]

## number people sick
d1025 <- copy(data)
setorder(d1025,x)
d1025[, truth := 0.0645 * (1.05 ^ x)]
d1025[, truthRisk :=round(truth*100,1)]
d1025[, waterMean := 0.0645 * (1.05 ^ meanWater)]
d1025[x < meanWater, waterMean := truth]
d1025[, waterZero := 0.0645 * (1.05 ^ 0)]

d1025[, truth := round(173000*12*truth*p)]
d1025[, waterMean := round(173000 * 12 * waterMean * p)]
d1025[, waterZero := round(173000 * 12 * waterZero * p)]
d1025[, casesCaused := truth - waterZero]
d1025[, num := round(173000 * p)]
d1025 <- d1025[, c("x", "truthRisk", "num", "truth", "casesCaused"), with = F]
d1025
apply(d1025,2,sum)


int=-3.15,stddevPeople=1.15,stddevWaterwork=1.15,oddsRatio=c(1.0,1.05), nReps=12){
  beta <- log(oddsRatio)
  
  data <- vector("list",length=length(npeople))
  for(i in 1:length(data)){
    data[[i]] <- data.frame(
      wwlevel=i,
      id=1:npeople[i],
      exposure=SampleWaterExposure(n=npeople[i]),
      globalIntercept=int,
      personSpecificIntercept=rnorm(npeople[i],sd=stddev)
    )
  }
  data$ylatent <- data$globalIntercept + data$personSpecificIntercept + rep(beta,each=nPeople/2)*data$exposure # + data$value
  data$prob <- exp(data$ylatent)/(1 + exp(data$ylatent))
