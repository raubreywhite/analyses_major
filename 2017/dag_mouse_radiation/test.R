library(data.table)
library(survival)
library(rms)

alpha = 250
beta = 10

d <- data.table(dose=c(0,1))
d[,HR:=c(1,2)]
d[,medianSurvivalDays:=alpha/(1+(HR-1)*0.1)]
d[,mice:=10000]
d

mice <- d[rep(seq(.N), mice), !"mice", with=F]
mice[, survivalTime := stats::rweibull(.N, scale=medianSurvivalDays, shape=beta)]


summary(coxph(Surv(survivalTime, rep(1,nrow(mice)))~dose, data=mice))

WeibullReg(Surv(survivalTime, rep(1,nrow(mice)))~dose, data=mice)

ConvertWeibull(fit)
initial <- as.vector(ConvertWeibull(naive)$vars[, 1])
fit
alphax <- coef(fit)
alphax[2] <- exp(alphax[2]+alphax[1])
alphax[1] <- exp(alphax[1])
alphax
betax <- fit$scale

alphax

survival1 <- (1+(5/alphax[1])^betax)^-1
survival2 <- (1+(5/alphax[2])^betax)^-1

survival1/survival2

exp(5.52-0.09)
1/scale
exp(intercept)


ddist <- datadist(mice)
options(datadist="ddist")
s <- Surv(time=mice$survivalTime, event=rep(1,nrow(mice)), dist="loglogistic")
fit1 <- cph(s~dose, data=mice, x=T, y=T)
fit1

p1 <- data.table(Predict(fit1,
                         dose=unique(mice$dose)
))
p1[,yhat:=yhat-min(yhat)]
p1[,HR:=exp(yhat)]
p1[,ERR:=HR-1]
p1[,yhat:=NULL]
p1[,lower:=NULL]
p1[,upper:=NULL]

print(p1)
d



dose  rate weighting isHighRate        HR medianSurvivalDays mice
1:  0.0   2.5     0.125          0  1.000000           250.0000   35
2:  0.1   2.5     0.125          0  1.049171           248.3539   35
3:  1.0   2.5     0.125          0  1.616074           234.8672   35
4:  3.0   2.5     0.125          0  4.220696           210.4258   35
5:  0.0 100.0     0.125          1  1.000000           250.0000   35
6:  0.1 100.0     0.125          1  1.100759           246.7415   35
7:  1.0 100.0     0.125          1  2.611696           221.9324   35
8:  3.0 100.0     0.125          1 17.814273           181.0479   35