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
d[dose==0,rate:=0]
d <- d[is.finite(rate)]

d <- d[dose %in% c(3)]

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
fit <- coxph(s ~ hours*rate,data = dt)
summary(fit)

#dt[,dose:=factor(dose)]
dt[,rate:=factor(rate)]
ddist <- datadist(dt)
options(datadist="ddist")
(rms_surv_fit <- cph(s~rate, data=dt, x=T, y=T))

p <- data.table(Predict(rms_surv_fit,
                        rate=unique(d$rate)
))
#p <- p[dose<=3]
#p <- p[!(dose==0 & rate!=0)]

q <- ggplot(p, aes(x=rate,y=yhat,colour=factor(rate)))
q <- q + geom_line()
q <- q + geom_point()
q

