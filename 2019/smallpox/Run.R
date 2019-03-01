org::AllowFileManipulationFromInitialiseProject()
org::InitialiseProject(
  HOME = "/git/code_major/2019/smallpox/",
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
startVals$di_edge_list[from=="municip1151"]
startVals$di_edge_list[to=="municip1151"]
comFrom <- startVals$di_edge_list[,.(comFrom=sum(n)),by=.(municip=from)]
comTo <- startVals$di_edge_list[,.(comTo=sum(n)),by=.(municip=to)]
stay <- startVals$pop_wo_com[,.(stay=sum(pop)),by=.(municip)]
p <- merge(stay,comFrom,by="municip")
p <- merge(p,comTo,by="municip")
p[,pop:=stay+comFrom]
p[,propComFrom:=comFrom/pop]
p[,propComTo:=comTo/pop]
setorder(p,-propComFrom)
p[municip %in% c("municip0301","municip1804","municip1151")]

sum(startVals$di_edge_list$n)+sum(startVals$pop_wo_com$pop)
startVals <- startVals$pop_wo_com[,c("municip")]
startVals[,value:=0]

startValsOslo1 <- copy(startVals)
startValsOslo1[municip=="municip0301",value:=1]

startValsOslo100 <- copy(startVals)
startValsOslo100[municip=="municip0301",value:=100]

startValsBodo1 <- copy(startVals)
startValsBodo1[municip=="municip1804",value:=1]

startValsUtsira1 <- copy(startVals)
startValsUtsira1[municip=="municip1151",value:=1]

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
m[day==100 & R>0]

q <- ggplot(m,aes(x=day,y=INCIDENCE))
q <- q + geom_col()
q

m <- RunSim(
  startVals=startValsUtsira1,
  R0=8,
  a=12,
  gammaTheoretical =6,
  gammaEffective =6,
  asymptomaticProb=0,
  asymptomaticRelativeInfectiousness=1,
  verbose=F)
setorder(m,day,location)
m[INCIDENCE>0 & location!="municip1151"]
m[location=="municip1151"]
sum(m[day==100]$R)

n <- RunSim(
  startVals=startValsOslo1,
  R0=8,
  a=12,
  gammaTheoretical =6,
  gammaEffective =6,
  asymptomaticProb=0,
  asymptomaticRelativeInfectiousness=1,
  verbose=F)
n[day==84 & R>0]
sum(n[day==84]$R)
sum(n[day==84 & location=="municip0301"]$R)
sum(n[day==21 & location=="municip0301"]$R)
mean(n[day==84]$I>0)

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

replicates <- 1000
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
      INCIDENCE=sum(INCIDENCE),
      NumMunicipWithEI=sum(E+I>0),
      PropMunicipWithEI=mean(E+I>0)
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
  mc.cores = parallel::detectCores()+2
)

res <- rbindlist(res)

res[R0==8 & gammaEffective==6 & location=="Utsira" & replicate==1 & day>40]

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


res[,S1:=shift(S,type = "lead"),by=.(location,scenario,R0,gammaEffective,a,replicate)]
res[,INCIDENCE:=as.numeric(S-S1)]

res[,INCIDENCE_WEEK:=sum(INCIDENCE,na.rm=T),
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

res[,EI:=E+I]
res[,EIR:=E+I+R-R_initial]

saveRDS(res,file.path(org::PROJ$SHARED_TODAY,"draws.RDS"))
res[scenario=="1 person" & R0==8 & gammaEffective==6 & day==28]
res[scenario=="1 person" & R0==8 & gammaEffective==6 & day==84]

pd <- res[dayOfWeek==7 &
            location=="Oslo" &
            scenario=="1 person" &
            R0==4 &
            gammaEffective==3]

res[,location:=factor(location,levels=c("Oslo","Bodø","Utsira"))]

pd <- res[dayOfWeek==7,.(
  EI_p05=round(quantile(EI,probs=c(0.05))),
  EI_p25=round(quantile(EI,probs=c(0.25))),
  EI_p50=round(quantile(EI,probs=c(0.5))),
  EI_p75=round(quantile(EI,probs=c(0.75))),
  EI_p95=round(quantile(EI,probs=c(0.95))),
  
  EIR_p05=round(quantile(EIR,probs=c(0.05))),
  EIR_p25=round(quantile(EIR,probs=c(0.25))),
  EIR_p50=round(quantile(EIR,probs=c(0.5))),
  EIR_p75=round(quantile(EIR,probs=c(0.75))),
  EIR_p95=round(quantile(EIR,probs=c(0.95))),
  
  NWEI_p05=round(quantile(NumMunicipWithEI,probs=c(0.05))),
  NWEI_p25=round(quantile(NumMunicipWithEI,probs=c(0.25))),
  NWEI_p50=round(quantile(NumMunicipWithEI,probs=c(0.5))),
  NWEI_p75=round(quantile(NumMunicipWithEI,probs=c(0.75))),
  NWEI_p95=round(quantile(NumMunicipWithEI,probs=c(0.95))),
  
  PWEI_p05=round(100*quantile(PropMunicipWithEI,probs=c(0.05))),
  PWEI_p25=round(100*quantile(PropMunicipWithEI,probs=c(0.25))),
  PWEI_p50=round(100*quantile(PropMunicipWithEI,probs=c(0.5))),
  PWEI_p75=round(100*quantile(PropMunicipWithEI,probs=c(0.75))),
  PWEI_p95=round(100*quantile(PropMunicipWithEI,probs=c(0.95)))
),keyby=.(
  location,
  scenario,
  R0,
  gammaEffective,
  day
)]

ColourRow <- function(row, colour="#deebf7"){
  cells <- xlsx::getCells(row)
  cs <- xlsx::CellStyle(wb) +
    xlsx::Font(wb,  heightInPoints=10, isBold=F, name="Arial") + 
    xlsx::Alignment(horizontal="ALIGN_RIGHT") +
    xlsx::Fill(backgroundColor="red", foregroundColor=colour,pattern="SOLID_FOREGROUND")
  
  lapply(seq_along(cells), function(i) xlsx::setCellStyle(cells[[i]], cs))
}

wb <- xlsx::createWorkbook()

sheet1 <- xlsx::createSheet(wb, "Totaldata")
xlsx::addDataFrame(pd, sheet1, startRow=1, startColumn=1, row.names=F)
xlsx::createFreezePane(sheet1, rowSplit=2, colSplit=6)
#xlsx::addAutoFilter(sheet1, "A1:E1")


sheet2 <- xlsx::createSheet(wb, "Oslo")
xlsx::addDataFrame(pd[location=="Oslo" & gammaEffective==6 & scenario=="1 person"], sheet2, startRow=1, startColumn=1, row.names=F)
xlsx::createFreezePane(sheet2, rowSplit=2, colSplit=6)
#xlsx::addAutoFilter(sheet2, "A1:E1")

rows <- xlsx::getRows(sheet2)
n <- (length(rows)-1)/12
cols <- c("white",rep(c("#deebf7","#9ecae1"),each=12,times=n))[1:length(rows)]
for(i in seq_along(cols)) ColourRow(rows[i],colour=cols[i])


sheet3 <- xlsx::createSheet(wb, "Bodø")
xlsx::addDataFrame(pd[location=="Bodø" & gammaEffective==6], sheet3, startRow=1, startColumn=1, row.names=F)
xlsx::createFreezePane(sheet3, rowSplit=2, colSplit=6)
#xlsx::addAutoFilter(sheet3, "A1:E1")

rows <- xlsx::getRows(sheet3)
n <- (length(rows)-1)/12
cols <- c("white",rep(c("#deebf7","#9ecae1"),each=12,times=n))[1:length(rows)]
for(i in seq_along(cols)) ColourRow(rows[i],colour=cols[i])


sheet4 <- xlsx::createSheet(wb, "Utsira")
xlsx::addDataFrame(pd[location=="Utsira" & gammaEffective==6], sheet4, startRow=1, startColumn=1, row.names=F)
xlsx::createFreezePane(sheet4, rowSplit=2, colSplit=6)
#xlsx::addAutoFilter(sheet4, "A1:E1")

rows <- xlsx::getRows(sheet4)
n <- (length(rows)-1)/12
cols <- c("white",rep(c("#deebf7","#9ecae1"),each=12,times=n))[1:length(rows)]
for(i in seq_along(cols)) ColourRow(rows[i],colour=cols[i])


sheet5 <- xlsx::createSheet(wb, "Oslo delayed diagnose")
xlsx::addDataFrame(pd[location=="Oslo" & scenario=="1 person"], sheet5, startRow=1, startColumn=1, row.names=F)
xlsx::createFreezePane(sheet5, rowSplit=2, colSplit=6)
#xlsx::addAutoFilter(sheet5, "A1:E1")

rows <- xlsx::getRows(sheet5)
n <- (length(rows)-1)/12
cols <- c("white",rep(c("#deebf7","#9ecae1"),each=12,times=n))[1:length(rows)]
for(i in seq_along(cols)) ColourRow(rows[i],colour=cols[i])


sheet6 <- xlsx::createSheet(wb, "Oslo angrep")
xlsx::addDataFrame(pd[location=="Oslo" & scenario=="100 personer"], sheet6, startRow=1, startColumn=1, row.names=F)
xlsx::createFreezePane(sheet6, rowSplit=2, colSplit=6)
#xlsx::addAutoFilter(sheet6, "A1:E1")

rows <- xlsx::getRows(sheet6)
n <- (length(rows)-1)/12
cols <- c("white",rep(c("#deebf7","#9ecae1"),each=12,times=n))[1:length(rows)]
for(i in seq_along(cols)) ColourRow(rows[i],colour=cols[i])


sheet7 <- xlsx::createSheet(wb, "et naturlig tilfelle 3 steder")
xlsx::addDataFrame(pd[R0==4 & scenario=="1 person" & gammaEffective==6], sheet7, startRow=1, startColumn=1, row.names=F)
xlsx::createFreezePane(sheet7, rowSplit=2, colSplit=6)
#xlsx::addAutoFilter(sheet7, "A1:E1")

rows <- xlsx::getRows(sheet7)
n <- (length(rows)-1)/12
cols <- c("white",rep(c("#deebf7","#9ecae1"),each=12,times=n))[1:length(rows)]
for(i in seq_along(cols)) ColourRow(rows[i],colour=cols[i])


sheet8<- xlsx::createSheet(wb, "terrorist 3 steder")
xlsx::addDataFrame(pd[R0==8 & scenario=="1 person" & gammaEffective==6], sheet8, startRow=1, startColumn=1, row.names=F)
xlsx::createFreezePane(sheet8, rowSplit=2, colSplit=6)
#xlsx::addAutoFilter(sheet8, "A1:E1")

rows <- xlsx::getRows(sheet8)
n <- (length(rows)-1)/12
cols <- c("white",rep(c("#deebf7","#9ecae1"),each=12,times=n))[1:length(rows)]
for(i in seq_along(cols)) ColourRow(rows[i],colour=cols[i])

xlsx::saveWorkbook(wb, file.path(org::PROJ$SHARED_TODAY,"quantiles.xlsx"))
#xlsx::saveWorkbook(wb, file.path("/git","quantiles.xlsx"))

res[,rankRep:=NULL]
res[day==28,rankRep:=as.numeric(frank(EIR,ties.method="random")),
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

# graph1
pd <- res[dayOfWeek==7 & 
            location=="Oslo" & 
            rankRep %in% c(1,replicates*0.25,replicates*0.5,replicates*0.75,replicates) &
            R0==4 & 
            gammaEffective==6 & day<=28]
pd[,prettyRankRep:=sprintf("%s. prosentil",round(100*rankRep/max(rankRep)))]
ordering <- unique(pd[,c("rankRep","prettyRankRep")])
setorder(ordering,rankRep)
pd[,prettyRankRep:=factor(prettyRankRep,levels=ordering$prettyRankRep)]
pd[,prettyScenario:=sprintf("Utbruddet begynner med %s",scenario)]

q <- ggplot(pd,
            aes(x=week,y=INCIDENCE_WEEK))
q <- q + geom_col()
q <- q + facet_grid(prettyScenario~prettyRankRep,scales="free")
q <- q + scale_y_continuous("Ukentlig insidens", labels = scales::format_format(big.mark = ".", decimal.mark = ",", scientific = FALSE))
q <- q + scale_x_continuous("Uke",breaks=seq(0,14,1), minor_breaks = NULL)
q <- q + labs(caption="\nEt utvalg forskjellige simuleringer av kopperutbrudd i Oslo med R0=4 og 6 dager smittsomhet, 1 og 100 tilfeller")
q <- q + theme_gray(14)
q
RAWmisc::saveA4(q,file.path(org::PROJ$SHARED_TODAY,"examples_oslo_r0_4_comparing_people.png"))

# graph
pd <- res[dayOfWeek==7 & 
            location=="Oslo" & 
            rankRep %in% c(1,replicates*0.25,replicates*0.5,replicates*0.75,replicates) &
            R0==8 & 
            gammaEffective==6 & day<=28]
pd[,prettyRankRep:=sprintf("%s. prosentil",round(100*rankRep/max(rankRep)))]
ordering <- unique(pd[,c("rankRep","prettyRankRep")])
setorder(ordering,rankRep)
pd[,prettyRankRep:=factor(prettyRankRep,levels=ordering$prettyRankRep)]
pd[,prettyScenario:=sprintf("Utbruddet begynner med %s",scenario)]

q <- ggplot(pd,
            aes(x=week,y=INCIDENCE_WEEK))
q <- q + geom_col()
q <- q + facet_grid(prettyScenario~prettyRankRep,scales="free")
q <- q + scale_y_continuous("Ukentlig insidens", labels = scales::format_format(big.mark = ".", decimal.mark = ",", scientific = FALSE))
q <- q + scale_x_continuous("Uke",breaks=seq(0,14,1), minor_breaks = NULL)
q <- q + labs(caption="\nEt utvalg forskjellige simuleringer av kopperutbrudd i Oslo med R0=8 og 6 dager smittsomhet, 1 og 100 tilfeller")
q <- q + theme_gray(14)
q
RAWmisc::saveA4(q,file.path(org::PROJ$SHARED_TODAY,"examples_oslo_r0_8_comparing_people.png"))

# graph
pd <- res[dayOfWeek==7 & 
            location=="Oslo" & 
            scenario=="1 person" & 
            rankRep %in% c(1,replicates*0.25,replicates*0.5,replicates*0.75,replicates) &
            R0 %in% c(4,8) & 
            gammaEffective %in% 3 & day<=28]
pd[,prettyRankRep:=sprintf("%s. prosentil",round(100*rankRep/max(rankRep)))]
ordering <- unique(pd[,c("rankRep","prettyRankRep")])
setorder(ordering,rankRep)
pd[,prettyRankRep:=factor(prettyRankRep,levels=ordering$prettyRankRep)]
pd[,prettyScenario:=sprintf("R0=%s",R0)]

q <- ggplot(pd,
            aes(x=week,y=INCIDENCE_WEEK))
q <- q + geom_col()
q <- q + facet_grid(prettyScenario~prettyRankRep,scales="free")
q <- q + scale_y_continuous("Ukentlig insidens", labels = scales::format_format(big.mark = ".", decimal.mark = ",", scientific = FALSE))
q <- q + scale_x_continuous("Uke",breaks=seq(0,14,1), minor_breaks = NULL)
q <- q + labs(caption="\nEt utvalg forskjellige simuleringer av kopperutbrudd i Oslo med R0=4 og R0=8 og 3 dager smittsomhet, 1 tilfelle")
q <- q + theme_gray(14)
q
RAWmisc::saveA4(q,file.path(org::PROJ$SHARED_TODAY,"examples_oslo_r0_48_3_days.png"))

# graph
pd <- res[dayOfWeek==7 & 
            location=="Bodø" & 
            scenario=="1 person" & 
            rankRep %in% c(1,replicates*0.25,replicates*0.5,replicates*0.75,replicates) &
            R0 %in% c(4,8) & 
            gammaEffective %in% 6 & day<=28]
pd[,prettyRankRep:=sprintf("%s. prosentil",round(100*rankRep/max(rankRep)))]
ordering <- unique(pd[,c("rankRep","prettyRankRep")])
setorder(ordering,rankRep)
pd[,prettyRankRep:=factor(prettyRankRep,levels=ordering$prettyRankRep)]
pd[,prettyScenario:=sprintf("R0=%s",R0)]

q <- ggplot(pd,
            aes(x=week,y=INCIDENCE_WEEK))
q <- q + geom_col()
q <- q + facet_grid(prettyScenario~prettyRankRep,scales="free")
q <- q + scale_y_continuous("Ukentlig insidens", labels = scales::format_format(big.mark = ".", decimal.mark = ",", scientific = FALSE))
q <- q + scale_x_continuous("Uke",breaks=seq(0,14,1), minor_breaks = NULL)
q <- q + labs(caption="\nEt utvalg forskjellige simuleringer av kopperutbrudd i Bodø med R0=4 og R0=8 og 6 dager smittsomhet, 1 tilfelle")
q <- q + theme_gray(14)
q
RAWmisc::saveA4(q,file.path(org::PROJ$SHARED_TODAY,"examples_bodo_r0_48_6_days.png"))

# graph
pd <- res[dayOfWeek==7 & 
            location=="Utsira" & 
            scenario=="1 person" & 
            rankRep %in% c(1,replicates*0.25,replicates*0.5,replicates*0.75,replicates) &
            R0 %in% c(4,8) & 
            gammaEffective %in% 6 & day<=28]
pd[,prettyRankRep:=sprintf("%s. prosentil",round(100*rankRep/max(rankRep)))]
ordering <- unique(pd[,c("rankRep","prettyRankRep")])
setorder(ordering,rankRep)
pd[,prettyRankRep:=factor(prettyRankRep,levels=ordering$prettyRankRep)]
pd[,prettyScenario:=sprintf("R0=%s",R0)]

q <- ggplot(pd,
            aes(x=week,y=INCIDENCE_WEEK))
q <- q + geom_col()
q <- q + facet_grid(prettyScenario~prettyRankRep,scales="free")
q <- q + scale_y_continuous("Ukentlig insidens", labels = scales::format_format(big.mark = ".", decimal.mark = ",", scientific = FALSE))
q <- q + scale_x_continuous("Uke",breaks=seq(0,14,1), minor_breaks = NULL)
q <- q + labs(caption="\nEt utvalg forskjellige simuleringer av kopperutbrudd i Utsira med R0=4 og R0=8 og 6 dager smittsomhet, 1 tilfelle")
q <- q + theme_gray(14)
q
RAWmisc::saveA4(q,file.path(org::PROJ$SHARED_TODAY,"examples_utsira_r0_48_6_days.png"))


