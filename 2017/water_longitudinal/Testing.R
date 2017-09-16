oddsRatio <- rep(1.05,5)
longitudinal <- FALSE

data <- copy(d[waterworkCategory==1])
data[,beta:=0]
for(i in 1:length(oddsRatio)) data[waterworkCategory==i, beta:=log(oddsRatio[i])]
data[,ylatent := globalIntercept + personSpecificIntercept + waterworkSpecificIntercept + housingEffect + beta*waterExposure] # + data$value
data[,ylatent := globalIntercept + waterworkSpecificIntercept + beta*waterExposure] # + data$value
#data[,ylatent := beta*waterExposure] # + data$value
data[,prob := exp(ylatent)/(1 + exp(ylatent))]

if(longitudinal) data <- reshape::untable(df=data, num=12)

data[,runis := runif(.N,0,1)]
data[,y := ifelse(runis < prob,1,0)]
data[,timeID:=1:.N,by=personID]




truthWW <- d[,.(N=.N),by=.(
  waterworkCategory,
  waterworkID
)]
truthWW <- truthWW[,.(N=.N),by=waterworkCategory]

sampleWW <- data[timeID==1,.(N=.N),by=.(
  waterworkCategory,
  waterworkID
)]
sampleWW <- sampleWW[,.(n=.N),by=waterworkCategory]

weightsWW <- merge(truthWW, sampleWW, by="waterworkCategory")
weightsWW[,weights1 := N/n]

truthPeople <- d[,.(N=.N),by=.(
  waterworkCategory,
  waterworkID,
  housing
)]

samplePeople <- data[timeID==1,.(n=.N),by=.(
  waterworkCategory,
  waterworkID,
  housing
)]

weightsPeople <- merge(truthPeople, samplePeople, by=c("waterworkCategory","waterworkID","housing"))
weightsPeople[,weights2 := N/n]

weights <- merge(weightsWW, weightsPeople, by=c("waterworkCategory"))
weights <- weights[,c("waterworkCategory","waterworkID","housing","weights1","weights2"),with=F]

data <- merge(data,weights,by=c("waterworkCategory","waterworkID","housing"))
x <- data[timeID==1]

design <- survey::svydesign(id = ~waterworkID+personID,
                            strata=~waterworkCategory,
                            weights = ~weights1+weights2,
                            data = data)
#summary(design)
#fit <- survey::svyglm(y~waterExposure*factor(waterworkCategory), design=design, family=binomial)
fit <- survey::svyglm(y~waterExposure, design=design, family=binomial)
summary(fit)



fit <- survey:::svyglm.survey.design(y~waterExposure, design=design, family=binomial)
summary(fit)


a <- glm(y~waterExposure, data=data,family=binomial)
summary(a)

b <- lme4::glmer(y~waterExposure + (1|waterworkID), data=data,family=binomial, weights=design$prob)
summary(b)

formula <- y~waterExposure
family=binomial
  
  data<-model.frame(design)
  
  g<-match.call()
  g$formula<-as.formula(y~waterExposure)#eval.parent(g$formula)
  g$design<-NULL
  g$var<-NULL
  if (is.null(g$weights)){
    g$weights<-quote(.survey.prob.weights)
  }else {
    g$weights<-bquote(.survey.prob.weights*.(g$weights))
  }
  g$data<-quote(data)
  g[[1]]<-quote(glm)      
  
  ##need to rescale weights for stability in binomial
  data$.survey.prob.weights<-(1/design$prob)/mean(1/design$prob)
  if (!all(all.vars(formula) %in% names(data))) 
    stop("all variables must be in design= argument")
  g<-with(list(data=data), eval(g))
  g$naive.cov<-summary(g)$cov.unscaled
  
  nas<-g$na.action
  if (length(nas))
    design<-design[-nas,]
  
  g$cov.unscaled<-svy.varcoef(g,design)
  g$df.residual <- degf(design)+1-length(coef(g)[!is.na(coef(g))])
  
  class(g)<-c("svyglm",class(g))
  g$call<-sys.call()
  g$call[[1]]<-as.name(.Generic)
  if(!("formula" %in% names(g$call))) {
    if (is.null(names(g$call)))
      i<-1
    else
      i<-min(which(names(g$call)[-1]==""))
    names(g$call)[i+1]<-"formula"
  }
  g$survey.design<-design 
  g
}

















data <- CreateSample(n=c(10000,1000,1000,1000,1000),d=d,w=w)
truthWW <- d[,.(N=.N),by=.(
  waterworkCategory,
  waterworkID
)]
truthWW <- truthWW[,.(N=.N),by=waterworkCategory]

sampleWW <- data[timeID==1,.(N=.N),by=.(
  waterworkCategory,
  waterworkID
)]
sampleWW <- sampleWW[,.(n=.N),by=waterworkCategory]

weightsWW <- merge(truthWW, sampleWW, by="waterworkCategory")
weightsWW[,weights1 := N/n]

truthPeople <- d[,.(N=.N),by=.(
  waterworkCategory,
  waterworkID,
  housing
)]

samplePeople <- data[timeID==1,.(n=.N),by=.(
  waterworkCategory,
  waterworkID,
  housing
)]

weightsPeople <- merge(truthPeople, samplePeople, by=c("waterworkCategory","waterworkID","housing"))
weightsPeople[,weights2 := N/n]

weights <- merge(weightsWW, weightsPeople, by=c("waterworkCategory"))
weights <- weights[,c("waterworkCategory","waterworkID","housing","weights1","weights2"),with=F]

data <- merge(data,weights,by=c("waterworkCategory","waterworkID","housing"))
x <- data[timeID==1]

design <- survey::svydesign(id = ~waterworkID+personID,
                            strata=~waterworkCategory,
                            weights = ~weights1+weights2,
                            data = data)
#summary(design)
a <- survey::svyglm(y~waterExposure*factor(waterworkCategory), design=design, family=binomial)
summary(a)

L <- list(
  c(0,1,0,0,0,0,0,0,0,0),
  c(0,1,0,0,0,0,1,0,0,0),
  c(0,1,0,0,0,0,0,1,0,0),
  c(0,1,0,0,0,0,0,0,1,0),
  c(0,1,0,0,0,0,0,0,0,1)
)

survey::svycontrast(a,L)


b <- lme4::glmer(y~waterExposure + (1|waterworkID), data=data,family=binomial)
summary(b)

weights <- (1/design$prob)/mean(1/design$prob)
b <- lme4::glmer(y~waterExposure + (1|waterworkID) , data=data[timeID==1],family=binomial, weights=weights[data$timeID==1])
summary(b)
