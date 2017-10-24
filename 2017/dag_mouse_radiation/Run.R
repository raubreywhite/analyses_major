RAWmisc::AllowFileManipulationFromInitialiseProject()
RAWmisc::InitialiseProject(
  HOME = "/git/code_major/2017/dag_mouse_radiation/",
  RAW = "/analyses/data_raw/code_major/2017/dag_mouse_radiation/",
  CLEAN = "/analyses/data_clean/code_major/2017/dag_mouse_radiation",
  BAKED = "/analyses/results_baked/code_major/2017/dag_mouse_radiation/",
  FINAL = "/analyses/results_final/code_major/2017/dag_mouse_radiation/",
  SHARED = "/dropbox/results_shared/code_major/2017/dag_mouse_radiation/")

suppressWarnings(suppressMessages(library(data.table)))
suppressWarnings(suppressMessages(library(ggplot2)))
suppressWarnings(suppressMessages(library(survival)))
suppressWarnings(suppressMessages(library(rms)))

micePossible <- 280

d <- data.table(readxl::read_excel(file.path(RAWmisc::PROJ$RAW,"Til Richard startpoints.xlsx")))
setnames(d,c("dose","hours","medianSurvivalDays","miceAssigned","notes"))
d[,notes:=NULL]
d[,rate:=dose/hours]
unique(d$rate)
da <- d[rate %in% c(0.0025,0.1) & !dose %in% 0.1]
dx <- d[dose==0]
dx[,miceAssigned:=miceAssigned/2]
dx[,rate:=0.0025]
d <- rbind(da,dx)
dx[,rate:=0.1]
d <- rbind(d,dx)
d

d[,isHighRate:=0]
d[rate==0.1,isHighRate:=1]

d[is.na(miceAssigned), miceAssigned:=0]
d[, miceNew := 0]
d[miceAssigned == 0, miceNew := round((micePossible - sum(miceAssigned))/.N)]
d[,mice:=miceAssigned+miceNew]

dt <- d[rep(seq(.N), mice), !"mice", with=F]
dt[, survivalTime := rexp(.N, rate=1/medianSurvivalDays)]

dt[, dead:=0]
dt[survivalTime<=440, dead:=1]

dt[, time := survivalTime]
dt[survivalTime > 440, time:=440]

s <- Surv(time=dt$time, event=dt$dead)
fit <- coxph(s ~ dose + dose*isHighRate,data = dt)
summary(fit)


dt[,doseAtHighRate:=dose*isHighRate]

dt[,dose:=factor(dose)]
#dt$doseAtHighRate <- with(dt,interaction(dose,isHighRate))


dt[,doseAtHighRate:=factor(doseAtHighRate)]

ddist <- datadist(dt)
options(datadist="ddist")
(fit1 <- cph(s~dose*isHighRate, data=dt, x=T, y=T))
anova(fit1)

p1 <- data.table(Predict(fit1,
                         dose=unique(d$dose)
))

q <- ggplot(p1, aes(x=dose,y=yhat,colour=factor(isHighRate)))
q <- q + geom_line()
q


(rms_surv_fit <- cph(s~rcs(dose,3), data=dt, x=T, y=T))
p1 <- data.table(Predict(rms_surv_fit,
                         dose=unique(d$dose)
))


