SampleWaterExposure <- function(n){
  return(sample(c(0,0.5,1,1.5,4,8),n,replace=TRUE,prob=c(3.07,5.36,2.61,29.92,44.74,14.30)))
}


RunSingleRisk <- function(npeople=8000,int=-2.75,stddev=0,oddsRatio=1.05, nReps=12){
  nPeople <- npeople
  
  beta <- log(oddsRatio)
  
  data <- data.frame(
    id=1:nPeople,
    exposure=SampleWaterExposure(n=nPeople),
    globalIntercept=int,
    personSpecificIntercept=rnorm(nPeople,sd=stddev)
  )
  data$ylatent <- data$globalIntercept + data$personSpecificIntercept + beta*data$exposure # + data$value
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
  fit <- lme4::glmer(y~exposure+(1|id),family=binomial,data=dataExpanded)
  fit <- coef(summary(fit))[2,]
  fit2 <- Hmisc::deff(dataExpanded$y,dataExpanded$id)
  
  fitOne <- summary(glm(y~exposure,family=binomial,data=data))
  fitOne <- coef(fitOne)[2,]
  
  fitAll <- summary(glm(y~exposure,family=binomial,data=dataExpanded))
  fitAll <- coef(fitAll)[2,]
  
  res <- data.frame(mixedEst=fit[[1]],mixedPval=fit[[4]],
                    oneEst=fitOne[[1]],onePval=fitOne[[4]],
                    badEst=fitAll[[1]],badPval=fitAll[[4]],icc=fit2[[3]],deff=fit2[[4]],prev=mean(dataExpanded$y))
  return(res)
}

RunSingleRisk90CI <- function(npeople=8000,int=-2.75,stddev=0,oddsRatio=1.0, nReps=12){
  nPeople <- npeople
  
  beta <- log(oddsRatio)
  
  data <- data.frame(
    id=1:nPeople,
    exposure=SampleWaterExposure(n=nPeople),
    globalIntercept=int,
    personSpecificIntercept=rnorm(nPeople,sd=stddev)
  )
  data$ylatent <- data$globalIntercept + data$personSpecificIntercept + beta*data$exposure # + data$value
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
  fit <- lme4::glmer(y~exposure+(1|id),family=binomial,data=dataExpanded)
  lower <- coef(summary(fit))[2,1] - 1.64*coef(summary(fit))[2,1]
  upper <- coef(summary(fit))[2,1] + 1.64*coef(summary(fit))[2,1]
  
  res <- data.frame(lower=lower,upper=upper)
  return(res)
}



RecreateCampy <- function(npeople=8000,int=-2.75,stddev=0,oddsRatio=1.05, nReps=1){
  nPeople <- npeople
  
  beta <- log(oddsRatio)
  
  data <- data.frame(
    id=1:nPeople,
    exposure=SampleWaterExposure(n=nPeople),
    globalIntercept=int,
    personSpecificIntercept=rnorm(nPeople,sd=stddev)
  )
  data$ylatent <- data$globalIntercept + data$personSpecificIntercept + beta*data$exposure # + data$value
  data$prob <- exp(data$ylatent)/(1 + exp(data$ylatent))
  
  runis <- runif(nrow(data),0,1)
  data$y <- ifelse(runis < data$prob,1,0)
  
  fitOne <- summary(glm(y~exposure,family=binomial,data=data))
  fitOne <- coef(fitOne)[2,]
  
  res <- data.frame(oneEst=fitOne[[1]],onePval=fitOne[[4]],prev=mean(data$y))
  return(res)
}





RunMultipleRisk <- function(npeople=8600,int=-3.15,stddev=1.15,oddsRatio=c(1.0,1.05), nReps=12){
  nPeople <- round(npeople/2)*2
  
  beta <- log(oddsRatio)
  
  data <- data.frame(
    wwlevel=factor(rep(1:2,each=nPeople/2)),
    id=1:nPeople,
    exposure=SampleWaterExposure(n=nPeople),
    globalIntercept=int,
    personSpecificIntercept=rnorm(nPeople,sd=stddev)
  )
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


GetNorwegianHousing <- function(){
  
  d <- data.table(readxl::read_excel(file.path(RPROJ$PROJRAW,"20173241235363412472457FOBbolAldBAarByg.xlsx"),skip=3))
  setnames(d,c("k","enebolig","tomannsbolig","rekkehus","boligblokk","bofellesskap","annen","uoppgitt"))
  d[,total:=enebolig+tomannsbolig+rekkehus+boligblokk+bofellesskap+annen+uoppgitt]
  d[441:nrow(d),]$k
  d[,kommune:=stringr::str_extract(k,"^[0-9][0-9][0-9][0-9]")]
  d <- d[kommune!="2111"]
  
  dx <- d[kommune=="0720"]
  dx[,kommune:="0704"]
  
  propTransferred <- 2200/dx$total
  for(i in 2:9){
    dx[,(i):=dx[[i]]*propTransferred]
    d[kommune=="0720",(i):=dx[[i]]*(1-propTransferred)]
  }
  
  d[kommune %in% c("0706","0719","0720"),kommune:="0710"]
  d[kommune %in% c("1901","1915"),kommune:="1903"]
  
  d <- rbind(dx,d)
  d <- d[!is.na(total)]
  d <- d[total>0,.(
    enebolig=sum(enebolig),
    tomannsbolig=sum(tomannsbolig),
    rekkehus=sum(rekkehus),
    boligblokk=sum(boligblokk),
    bofellesskap=sum(bofellesskap),
    annen=sum(annen),
    uoppgitt=sum(uoppgitt),
    total=sum(total)
  ), by=kommune]
  sum(d$total)
  
  #
  # Folke- og boligtellingen, boliger, 19. november 2011
  # https://www.ssb.no/fobbolig
  # Tabell: 09810: Personer i privathusholdninger, etter alder, boligens byggeår og bygningstype (K) (B)
  # https://www.ssb.no/statistikkbanken/SelectVarVal/Define.asp?subjectcode=al&ProductId=al&MainTable=FOBbolAldBAarByg&SubTable=Kommun1&PLanguage=0&nvl=True&Qid=0&gruppe1=Hele&gruppe2=Hele&gruppe3=Hele&gruppe4=Hele&gruppe5=Hele&VS1=KommunFoB&VS2=AlleAldre06ac&VS3=ByggeAarFOB02&VS4=BygnTypeFOB01&VS5=&mt=0&KortNavnWeb=fobbolig&CMSSubjectArea=befolkning&StatVariant=&checked=true
  #
  d <- data.table(readxl::read_excel(file.path(RPROJ$PROJRAW,"2017324133134512472457FOBbolAldBAarByg.xlsx"),skip=0))
  d[, kommune:= zoo::na.locf(kommune)]
  d[, age:= zoo::na.locf(age)]
  d[, type:= zoo::na.locf(type)]
  d <- d[!is.na(people)]
  d <- dcast.data.table(d,kommune+age~type,value.var="people")
  setnames(d,c("k","age","annen","boligblokk","bofellesskap","enebolig","rekkehus","tomannsbolig","uoppgitt"))
  
  d[,total:=enebolig+tomannsbolig+rekkehus+boligblokk+bofellesskap+annen+uoppgitt]
  d[,kommune:=stringr::str_extract(k,"^[0-9][0-9][0-9][0-9]")]
  d <- d[kommune!="2111"]
  
  dx <- d[kommune=="0720"]
  dx[,kommune:="0704"]
  
  propTransferred <- 2200/dx$total
  for(i in 3:10){
    dx[,(i):=dx[[i]]*propTransferred]
    d[kommune=="0720",(i):=dx[[i]]*(1-propTransferred)]
  }
  
  d[kommune %in% c("0706","0719","0720"),kommune:="0710"]
  d[kommune %in% c("1901","1915"),kommune:="1903"]
  
  d <- rbind(dx,d)
  d <- d[!is.na(total)]
  d <- d[total>0,.(
    enebolig=sum(enebolig),
    tomannsbolig=sum(tomannsbolig),
    rekkehus=sum(rekkehus),
    boligblokk=sum(boligblokk),
    bofellesskap=sum(bofellesskap),
    annen=sum(annen),
    uoppgitt=sum(uoppgitt),
    total=sum(total)
  ), by=.(kommune,age)]
  return(d)
}

GetNorwegianHousingNumHouses <- function(){
  # Tabell: 09796: Boliger i alt og bebodde boliger, etter bygningstype og husets byggeår (K) (B)
  # https://www.ssb.no/statistikkbanken/SelectVarVal/Define.asp?subjectcode=al&ProductId=al&MainTable=FOBbolBygTypAar&SubTable=Kommun1&PLanguage=0&nvl=True&Qid=0&gruppe1=Hele&gruppe2=Hele&gruppe3=Hele&gruppe4=Hele&VS1=KommunFoB&VS2=BygnTypeFOB01&VS3=ByggeAarFOB01&VS4=&mt=0&KortNavnWeb=fobbolig&CMSSubjectArea=befolkning&StatVariant=&checked=true
  
  
  d <- data.table(readxl::read_excel(file.path(RPROJ$PROJRAW,"2017329100509212658708FOBbolBygTypAar.xlsx"),skip=3))
  setnames(d,c("k","enebolig","tomannsbolig","rekkehus","boligblokk","bofellesskap","annen","uoppgitt"))
  d[,total:=enebolig+tomannsbolig+rekkehus+boligblokk+bofellesskap+annen+uoppgitt]
  d[441:nrow(d),]$k
  d[,kommune:=stringr::str_extract(k,"^[0-9][0-9][0-9][0-9]")]
  d <- d[kommune!="2111"]
  
  dx <- d[kommune=="0720"]
  dx[,kommune:="0704"]
  
  propTransferred <- 2200/dx$total
  for(i in 2:9){
    dx[,(i):=dx[[i]]*propTransferred]
    d[kommune=="0720",(i):=dx[[i]]*(1-propTransferred)]
  }
  
  d[kommune %in% c("0706","0719","0720"),kommune:="0710"]
  d[kommune %in% c("1901","1915"),kommune:="1903"]
  
  d <- rbind(dx,d)
  d <- d[!is.na(total)]
  d <- d[total>0,.(
    enebolig=sum(enebolig),
    tomannsbolig=sum(tomannsbolig),
    rekkehus=sum(rekkehus),
    boligblokk=sum(boligblokk),
    bofellesskap=sum(bofellesskap),
    annen=sum(annen),
    uoppgitt=sum(uoppgitt),
    total=sum(total)
  ), by=kommune]
  
  return(d)
  
}