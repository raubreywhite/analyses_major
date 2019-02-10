org::AllowFileManipulationFromInitialiseProject()
org::InitialiseProject(
  HOME = "/git/code_major/2019/smallpox/",
  RAW = "/Volumes/crypt_data/org/data_raw/code_major/2019/smallpox/",
  SHARED = "/dropbox/analyses/results_shared/code_major/2019/smallpox/"
)

library(data.table)
library(ggplot2)
library(pbmcapply)
library(commuter)

# 100 i oslo
# 1 i oslo
# 1 i bodø
# 1 i utsira

# R0=4/8

# 1 i oslo, 1 i bodø try w/ 3 days infectious vs 6 days infectious
# everything else 6 days infectious (r0=8)

# 12 uker

dirTemp <- SetupCPPAndStructure()

startVals <- CreateDataFiles()
sum(startVals$di_edge_list$n)+sum(startVals$pop_wo_com$pop)
startVals <- startVals$pop_wo_com[,c("location")]
startVals[,value:=0]

startValsOslo1 <- copy(startVals)
startValsOslo1[location=="municip0301",value:=1]

startValsOslo100 <- copy(startVals)
startValsOslo100[location=="municip0301",value:=100]

startValsBodo1 <- copy(startVals)
startValsBodo1[location=="municip1804",value:=1]

startValsUtsira1 <- copy(startVals)
startValsUtsira1[location=="municip1151",value:=1]

m <- RunSim(
  startVals=startValsOslo100,
  R0=1.88,
  a=1.9,
  gammaTheoretical =3,
  gammaEffective =3,
  asymptomaticProb=0.33,
  asymptomaticRelativeInfectiousness=0.5,
  verbose=F)
sum(m[day==100]$R)

q <- ggplot(m,aes(x=day,y=INCIDENCE))
q <- q + geom_col()
q



stackUnique <- list(
  list(
    location="Oslo",
    scenario="1 person",
    startVals=startValsOslo1,
    gammaTheoretical=6,
    gammaEffective=3,
    a=12,
    asymptomaticProb=0,
    asymptomaticRelativeInfectiousness=1
  ),
  list(
    location="Oslo",
    scenario="1 person",
    startVals=startValsOslo1,
    gammaTheoretical=6,
    gammaEffective=6,
    a=12,
    asymptomaticProb=0,
    asymptomaticRelativeInfectiousness=1
  ),
  list(
    location="Oslo",
    scenario="100 personer",
    startVals=startValsOslo100,
    gammaTheoretical=6,
    gammaEffective=6,
    a=12,
    asymptomaticProb=0,
    asymptomaticRelativeInfectiousness=1
  ),
  list(
    location="Bodø",
    scenario="1 person",
    startVals=startValsBodo1,
    gammaTheoretical=6,
    gammaEffective=6,
    a=12,
    asymptomaticProb=0,
    asymptomaticRelativeInfectiousness=1
  ),
  list(
    location="Utsira",
    scenario="1 person",
    startVals=startValsUtsira1,
    gammaTheoretical=6,
    gammaEffective=6,
    a=12,
    asymptomaticProb=0,
    asymptomaticRelativeInfectiousness=1
  )
)

replicates <- 2000
stack <- vector("list",length=length(stackUnique)*replicates)
index <- 1
for(i in seq_along(stackUnique)) for(j in 1:replicates) for(R0 in c(4,8)){
  stack[[index]] <- stackUnique[[i]]
  stack[[index]]$replicate <- j
  stack[[index]]$R0 <- R0
  stack[[index]]$id <- index
  index <- index + 1
}

res <- pbmclapply(
  stack,
  function(x){
    m <- RunSim(
      id=x$id,
      startVals=x$startVals,
      R0=x$R0,
      gammaTheoretical=x$gammaTheoretical,
      gammaEffective=x$gammaEffective,
      a=x$a,
      asymptomaticProb=x$asymptomaticProb,
      asymptomaticRelativeInfectiousness=x$asymptomaticRelativeInfectiousness,
      M=7*12,
      verbose = F)
    
    retval <- m[,.(
      S=sum(S),
      E=sum(E),
      I=sum(I),
      IA=sum(IA),
      R=sum(R),
      INCIDENCE=sum(INCIDENCE)
    ),by=day]
    
    retval[,location:=x$location]
    retval[,scenario:=x$scenario]
    retval[,R0:=x$R0]
    retval[,gammaTheoretical:=x$gammaTheoretical]
    retval[,gammaEffective:=x$gammaEffective]
    retval[,a:=x$a]
    retval[,replicate:=x$replicate]
    
    return(retval)
  },
  mc.cores = parallel::detectCores()
)

res <- rbindlist(res)
res[,week:=floor((day-1)/7+1)]
res[,dayOfWeek:=(day-1)%%7+1]

res[,R_initial:=min(R),
    by=.(
      location,
      scenario,
      R0,
      gammaTheoretical,
      gammaEffective,
      a,
      replicate
    )]

res[,INCIDENCE_WEEK:=sum(INCIDENCE),
    by=.(
      location,
      scenario,
      R0,
      gammaTheoretical,
      gammaEffective,
      a,
      replicate,
      week
    )]

res[,EIR:=E+I+R-R_initial]

saveRDS(res,file.path(org::PROJ$SHARED_TODAY,"draws.RDS"))

pd <- res[dayOfWeek==7 &
            location=="Oslo" &
            scenario=="1 person" &
            R0==4 &
            gammaEffective==3]

pd <- res[dayOfWeek==7,.(
  EI_p05=round(quantile(E+I,probs=c(0.05))),
  EI_p25=round(quantile(E+I,probs=c(0.25))),
  EI_p50=round(quantile(E+I,probs=c(0.5))),
  EI_p75=round(quantile(E+I,probs=c(0.75))),
  EI_p95=round(quantile(E+I,probs=c(0.95))),
  
  EIR_p05=round(quantile(E+I+R-R_initial,probs=c(0.05))),
  EIR_p25=round(quantile(E+I+R-R_initial,probs=c(0.25))),
  EIR_p50=round(quantile(E+I+R-R_initial,probs=c(0.5))),
  EIR_p75=round(quantile(E+I+R-R_initial,probs=c(0.75))),
  EIR_p95=round(quantile(E+I+R-R_initial,probs=c(0.95)))
),keyby=.(
  day,
  location,
  scenario,
  R0,
  gammaEffective
)]

wb <- xlsx::createWorkbook()
sheet1 <- xlsx::createSheet(wb, "Sheet1")
xlsx::addDataFrame(pd, sheet1, startRow=1, startColumn=1, row.names=F)
xlsx::createFreezePane(sheet1, rowSplit=2, colSplit=6)
xlsx::saveWorkbook(wb, file.path(org::PROJ$SHARED_TODAY,"quantiles.xlsx"))

res[,rankRep:=NULL]
res[day==84,rankRep:=as.numeric(frank(EIR,ties.method="random")),
  by=.(
    location,
    scenario,
    R0,
    gammaEffective,
    a
  )]
res[,rankRep:=mean(rankRep,na.rm=T),by=.(
  location,
  scenario,
  R0,
  gammaEffective,
  a,
  replicate
)]

x <- res[day==84 &
  location=="Oslo" & 
      #ordering %in% c(0,0.05,0.5,0.95,1,25,50) &
      R0==4 & 
      gammaEffective==6 &
    scenario=="1 person"]

setorder(x,rankRep)
x

pd <- res[dayOfWeek==7 & 
            location=="Oslo" & 
            rankRep %in% c(1,replicates*0.25,replicates*0.5,replicates*0.75,replicates) &
            R0==4 & 
            gammaEffective==6]
pd[,prettyRankRep:=sprintf("Simulering #%s",formatC(rankRep,width=4,flag="0"))]
pd[,prettyScenario:=sprintf("Utbruddet begynner med %s",scenario)]

q <- ggplot(pd,
            aes(x=week,y=INCIDENCE_WEEK))
q <- q + geom_col()
q <- q + facet_grid(prettyScenario~prettyRankRep,scales="free")
q <- q + scale_y_continuous("Ukentlig insidens")
q <- q + scale_x_continuous("Uke",breaks=seq(0,14,2))
q <- q + labs(caption="\nEt utvalg forskjellige simuleringer av kopperutbrudd i Oslo med R0=4 og 6 dager smittsomhet")
q <- q + theme_gray(14)
q
RAWmisc::saveA4(q,file.path(org::PROJ$SHARED_TODAY,"examples.png"))


