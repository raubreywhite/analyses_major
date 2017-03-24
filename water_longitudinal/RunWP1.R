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
setnames(d,c("k","age","enebolig","tomannsbolig","rekkehus","boligblokk","bofellesskap","annen","uoppgitt"))

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


