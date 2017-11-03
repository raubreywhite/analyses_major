
# Explore korrtabeller or 171013_IS.dta

IS <- data.table(haven::read_dta(file.path(RAWmisc::PROJ$RAW,"171013_IS.dta")))

library(dplyr)
glimpse(IS)

summary(IS)

# treatment: 0 spontaneous, 1 surgery, 2 enema, 3 unknown.
table(IS$treatment)
IS=IS[!IS$treatment==3,]
IS$trt <- factor(IS$treatment, levels = 0:2, labels = c("spontaneous", "surgery", "enema"))
IS$trt <-relevel(IS$trt, ref = "enema")
IS=IS[,treatment:=NULL]


IS[which(is.na(IS$durationstay)),]
IS$durationstay <- ifelse(is.na(IS$durationstay) & IS$indate==IS$sickdate, 0, IS$durationstay)

table(IS$indate-IS$sickdate, IS$durationsymptoms)
IS[which((IS$indate-IS$sickdate)<0),]
IS[which((IS$indate-IS$sickdate)<0),1:2] = IS[which((IS$indate-IS$sickdate)<0),2:1]

IS$agemonr <- round(IS$agemon,0)
IS$age6grm <- cut(round(IS$agemon,0), breaks= c(0,4,8,12,16,20,24.01), right=F)

IS$agec <- IS$agemon-mean(IS$agemon)
IS$symptmore2d <- ifelse(IS$durationsymptoms>=2, 1, 0)
IS$staymore2d <- ifelse(IS$durationstay >=2, 1, 0)


prop.table(table(IS$trt))


#------------------------------------------------------------------------------------------------------------------

# time at home: durationsymptoms vs treatment

table(IS$trt, IS$symptmore2d)


library(nnet)

fit0 <- multinom(trt ~ 1, data = IS) 

fit1 <- multinom(trt ~ symptmore2d, data = IS) 

summary(fit1)
exp(coef(fit1))
# The relative risk ratio for a one-unit increase in the variable symptmore2d is 2.5953907 for 
# being spontaneusly recovered vs. receiving an enema.
summary(fit1)$coefficients/summary(fit1)$standard.errors
(1 - pnorm(abs( summary(fit1)$coefficients/summary(fit1)$standard.errors ), 0, 1)) * 2



table(predict(fit1, type = "class"))
table(IS$trt)
apply(fitted(fit1, outcome=FALSE), 2, mean)


LLf   <- logLik(fit1)
LL0   <- logLik(fit0)
N     <- dim(IS)[1]

# McFadden pseudo-R2
as.vector(1 - (LLf / LL0))

# Cox & Snell
as.vector(1 - exp((2/N) * (LL0 - LLf)))

# Nagelkerke
as.vector((1 - exp((2/N) * (LL0 - LLf))) / (1 - exp(LL0)^(2/N)))

#------------------------------------------------------------------------------------------------------------------


# age and treatment


fit2 <- multinom(trt ~ agemonr , data = IS) 
summary(fit2)
exp(coef(fit2))
# exp(coef(fit2)[,"male"])


table(predict(fit2, type = "class"))
table(IS$trt)
apply(fitted(fit2, outcome=FALSE), 2, mean)


pchisq(2*(logLik(fit2)-logLik(fit1)), df= length(coef(fit2)) - length(coef(fit1)),lower.tail = FALSE)
qchisq(0.05 , df=2, lower.tail = F)





fitsat <- multinom(trt ~ age6grm, data = IS)
deviance(multinom(trt~1, data=IS)) - deviance(fitsat)
pchisq(14, 12-2, lower.tail=FALSE)

deviance(fitsat) - deviance(fit1)
pchisq(deviance(fitsat)- deviance(fit1), df= length(coef(fitsat)) - length(coef(fit1)), lower.tail=FALSE)













