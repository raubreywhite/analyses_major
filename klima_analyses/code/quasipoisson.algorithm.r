anscombe.residuals <- function(m,phi) {
  y <- m$y
  mu <- fitted.values(m)
  #Compute raw Anscombe residuals
  a <- 3/2*(y^(2/3) * mu^(-1/6) - mu^(1/2))
  #Compute standardized residuals
  a <- a/sqrt(phi * (1-hatvalues(m)))
  return(a)
}

#Dataset laget i "WeeklyReport", Ett dataset for hver av dataene "Influensa", "
quasipoisson.algorithm = function(
  dataset,
  predinterval=30,
  historical.data.years=5,
  mod.pred.window=90,
  reweights=1,
  remove.pandemic.year=F,
  remove.highcounts=0,
  sign.level=0.05){

  #FUNCTION quasipoisson.algorithm
  #
  #Description: Applys a surveillance algorithm based on a quasi-poisson regression 
  #model to the selected data. The difference from the Farrington algorithm is in how
  #seasonality is accounted for (here it is adjusted for, in Farrington it is removed
  #by design.
  #
  #Input (arguments):
  #dataset: data frame with columns for number of cases, covariates and dates per day
  #datecol: name of date-column (default: 'Dato')
  #predinterval: length of prediction interval (default: 30; last 30 days)
  #historical.data.years: number (should be greater or equal to at least 3) of full years of background data (default: 5)
  #mod.pred.window: number (greater or equal to 0) of days window between datasets used for modelling and prediction(default: 90)
  #reweights: number (greater or equal to 0) of residual reweights adjusting for previous outbreaks (default: 1; 1 reweight)
  #remove.pandemic.year: true/false (default: false; keep 2009 data)
  #remove.highcounts: number between 0 and 1 of fraction of high counts to be removed from prediction, to remove impact of earlier outbreaks (default: 0)
  #sign.level: significance level for the prediction intervals (default: 5%)
  dataset[,popX:=sum(pop),by=.(date)]
  
  #SET REGRESSION FORMULA:
  regformula = n ~ offset(log(consult)) + trend + factor(dayOfWeek) + sin(2*pi*dayOfYear/366) + cos(2*pi*dayOfYear/366) #+ HelligdagIndikator 
  
  #CREATE ADDITIONAL VARIABLES NEEDED:
  dataset$trend = 1:nrow(dataset) 
  
  #DIVIDE THE DATASET INTO A TRAINING SET AND A TEST SET:
  dates = sort(unique(dataset$date))
  #Define start and stop for prediction data:
  startpreddate=dates[length(dates)-predinterval+1]
  stoppreddate=dates[length(dates)]
  #Define period for modelling data (with or without pandemic):
  stopmoddate=dates[length(dates)-predinterval-mod.pred.window]
  startmoddate=seq(stopmoddate, length=2, by="-5 years")[2]
  if(remove.pandemic.year==T){
    startmoddate=seq(stopmoddate, length=2, by="-6 years")[2]
    moddates = seq.Date(startmoddate,stopmoddate,"days")
    moddates = moddates[format(moddates,'%Y') != 2009]
  }
  if(remove.pandemic.year==F){
    startmoddate=seq(stopmoddate, length=2, by="-5 years")[2]
    moddates = seq.Date(startmoddate,stopmoddate,"days")
  }
  #Define testset for prediction:
  dataset.test = dataset[date>=startpreddate]
  #Define trainingset for modelling:
  dataset.training = dataset[date %in% moddates]
  
  #If chosen, remove upper given percentage of counts from the prediction:
  if(remove.highcounts>0){
    dataset.training = dataset.training[n < quantile(n,(1-remove.highcounts)),]
  }
  dataset.training <- dataset.training[consult>0]
  #FIT QUASI-POISSON REGRESSION MODEL ON THE TRAINING SET:
  poisreg = glm(regformula,data=dataset.training,family=quasipoisson,na.action=na.omit)
  
  #REFIT THE REGRESSION USING RESIDUAL WEIGHTS (TO DOWNWEIGHT PREVIOUS OUTBREAKS):
  w_i = rep(1,nrow(dataset.training))
  dataset.training = cbind(dataset.training,w_i)
  
  for(i in sort(1:reweights)){
    dispersion_parameter = summary(poisreg)$dispersion
    if(i==0){break}
    anscombe.res = anscombe.residuals(poisreg, dispersion_parameter)
    anscombe.res[anscombe.res < 1] = 1 #Alt. 2.58?
    dataset.training[,w_i := anscombe.res^(-2)] #The weight
    Gamma = nrow(dataset.training)/sum(dataset.training$w_i)
    dataset.training[,w_i := Gamma*w_i] #Makes sum(w_i) = n
    poisreg = glm(regformula,data=dataset.training,weights=w_i,family=quasipoisson,na.action=na.omit)
    dispersion_parameter = summary(poisreg)$dispersion
  }
  
  #CALCULATE SIGNAL THRESHOLD (prediction interval from Farrington 1996):
  pred = predict(poisreg,type="respons",se.fit=T,newdata=dataset.test)
  dataset.test[,threshold2 := pred$fit*((1+(2/3)*qnorm(1-(0.05/2))*((((dispersion_parameter*pred$fit)+(pred$se.fit^2))/(pred$fit^2))^(1/2)))^(3/2))]
  dataset.test[,threshold4 := pred$fit*((1+(2/3)*qnorm(1-(0.0001/2))*((((dispersion_parameter*pred$fit)+(pred$se.fit^2))/(pred$fit^2))^(1/2)))^(3/2))]
 
  return(dataset.test)

}


quasipoisson.algorithm.week = function(
  dataset,
  predinterval=30,
  historical.data.years=5,
  mod.pred.window=90,
  reweights=1,
  remove.pandemic.year=F,
  remove.highcounts=0,
  sign.level=0.05,
  predintervalweeks=NULL){
  
  #FUNCTION quasipoisson.algorithm
  #
  #Description: Applys a surveillance algorithm based on a quasi-poisson regression 
  #model to the selected data. The difference from the Farrington algorithm is in how
  #seasonality is accounted for (here it is adjusted for, in Farrington it is removed
  #by design.
  #
  #Input (arguments):
  #dataset: data frame with columns for number of cases, covariates and dates per day
  #datecol: name of date-column (default: 'Dato')
  #predinterval: length of prediction interval (default: 30; last 30 days)
  #historical.data.years: number (should be greater or equal to at least 3) of full years of background data (default: 5)
  #mod.pred.window: number (greater or equal to 0) of days window between datasets used for modelling and prediction(default: 90)
  #reweights: number (greater or equal to 0) of residual reweights adjusting for previous outbreaks (default: 1; 1 reweight)
  #remove.pandemic.year: true/false (default: false; keep 2009 data)
  #remove.highcounts: number between 0 and 1 of fraction of high counts to be removed from prediction, to remove impact of earlier outbreaks (default: 0)
  #sign.level: significance level for the prediction intervals (default: 5%)
  
  dataset[,year := format.Date(date,"%G")] #Week-based year, instead of normal year (%Y)
  dataset[,week := as.numeric(format.Date(date,"%V"))] #ISO-week, instead of others (%W and %U)
  dataset <- dataset[week %in% 1:52]
  dataset <- dataset[,.(n=sum(n),consult=sum(consult),popX=sum(pop)),by=.(date,year,week)]
  dataset <- dataset[,.(n=sum(n),consult=sum(consult),popX=mean(popX)),by=.(year,week)]
  
  #SET REGRESSION FORMULA:
  regformula = n ~ offset(log(consult)) + trend + sin(2*pi*(week-1)/52) + cos(2*pi*(week-1)/52) #+ HelligdagIndikator 
  
  #CREATE ADDITIONAL VARIABLES NEEDED:
  dataset[,trend:= .I]
  
  #DIVIDE THE DATASET INTO A TRAINING SET AND A TEST SET:
  
  if(is.null(predintervalweeks)){
    startpreddate = max(dataset$trend) - ceiling(predinterval/7)
  } else startpreddate = max(dataset$trend) - predintervalweeks + 1
  stoppreddate = max(dataset$trend)
  #Define period for modelling data (with or without pandemic):
  stopmoddate=startpreddate-1-round(mod.pred.window/7)
  #If chose, remove pandemic season:
  if(remove.pandemic.year==F){
    startmoddate=stopmoddate-50*52
    moddates = startmoddate:stopmoddate
  }
  if(remove.pandemic.year==T){
    startmoddate=stopmoddate-51*52
    moddates = startmoddate:stopmoddate
    temp = dataset[year == 2009]$trend
    moddates = moddates[!(modates %in% temp)]
  }
  
  #Define testset for prediction:
  dataset.test = dataset[trend %in% startpreddate:stoppreddate]
  #Define trainingset for modelling:
  dataset.training = dataset[trend %in% moddates]
  
  #If chosen, remove upper given percentage of counts from the prediction:
  if(remove.highcounts>0){
    dataset.training = dataset.training[n < quantile(n,(1-remove.highcounts)),]
  }
  dataset.training <- dataset.training[consult>0]
  #FIT QUASI-POISSON REGRESSION MODEL ON THE TRAINING SET:
  poisreg = glm(regformula,data=dataset.training,family=quasipoisson,na.action=na.omit)
  
  #REFIT THE REGRESSION USING RESIDUAL WEIGHTS (TO DOWNWEIGHT PREVIOUS OUTBREAKS):
  w_i = rep(1,nrow(dataset.training))
  dataset.training = cbind(dataset.training,w_i)
  
  for(i in sort(1:reweights)){
    dispersion_parameter = summary(poisreg)$dispersion
    if(i==0){break}
    anscombe.res = anscombe.residuals(poisreg, dispersion_parameter)
    anscombe.res[anscombe.res < 1] = 1 #Alt. 2.58?
    dataset.training[,w_i := anscombe.res^(-2)] #The weight
    Gamma = nrow(dataset.training)/sum(dataset.training$w_i)
    dataset.training[,w_i := Gamma*w_i] #Makes sum(w_i) = n
    poisreg = glm(regformula,data=dataset.training,weights=w_i,family=quasipoisson,na.action=na.omit)
    dispersion_parameter = summary(poisreg)$dispersion
  }
  
  #CALCULATE SIGNAL THRESHOLD (prediction interval from Farrington 1996):
  pred = predict(poisreg,type="respons",se.fit=T,newdata=dataset.training)
  dataset.training[,threshold0 := pred$fit]
  dataset.training[,threshold2 := pred$fit*((1+(2/3)*qnorm(1-(0.05/2))*((((dispersion_parameter*pred$fit)+(pred$se.fit^2))/(pred$fit^2))^(1/2)))^(3/2))]
  dataset.training[,threshold4 := pred$fit*((1+(2/3)*qnorm(1-(0.0001/2))*((((dispersion_parameter*pred$fit)+(pred$se.fit^2))/(pred$fit^2))^(1/2)))^(3/2))]
  
  return(dataset.training)
  
}
