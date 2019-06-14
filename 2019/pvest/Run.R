org::AllowFileManipulationFromInitialiseProject()
org::InitialiseProject(
  HOME = "/git/code_major/2019/pvest/",
  SHARED = "/dropbox/analyses/results_shared/code_major/2019/pvest/"
)

library(data.table)
library(ggplot2)

NextYear <- function(px,p2018){
  pMultiplier <- p2018[,c("age","nextYearMultiplier")]
  py <- merge(px,pMultiplier,by="age")
  py0 <- py[age==0]
  py0[,vaxdec:=0]
  py[,pop:=pop*nextYearMultiplier]
  py[,age:=age+1]
  py <- rbind(py0,py[,names(py0),with=F])
  py[,year:=year+1]
  py <- py[age<=105]
  py[,vaxjan:=vaxdec*nextYearMultiplier*0.9]
  
  # increase number of RF people once they hit 65
  total65 <- sum(py[age==65]$pop)
  desired65RF <- total65*0.338
  current65RF <- py[age==65 & isRF==TRUE]$pop
  transfer65RF <- desired65RF-current65RF
  
  py[age==65 & isRF==FALSE,pop:=pop-transfer65RF]
  py[age==65 & isRF==TRUE,pop:=pop+transfer65RF]
  
  # recreate indicators
  py[,isLE64:= age<=64 ]
  py[,popProp:=pop/sum(pop),by=.(isRF,isLE64)]
  
  py[,nextYearMultiplier:=NULL]
  py[,vaxdec:=NULL]
  
  return(py)
}

Vaccinate <- function(p,n){
  vax <- data.table(
    n=c(0,n/3,n/3,n/3),
    isRF=c(F,T,F,T),
    isLE64=c(T,T,F,F)
  )
  p <- merge(p,vax,by=c("isRF","isLE64"))
  p[,vaxgiven:=popProp*n]
  p[,vaxdec:=vaxjan + vaxgiven]
  p[,n:=NULL]
  
  return(p)
}

p <- fhi::NorwayPopulation()
p <- p[,.(
  pop=sum(pop)
),
keyby=.(year,age)]

p[,popplus1year:=shift(pop, type="lead"),by=.(age)]
p[,popplus1yearage:=shift(popplus1year, type="lead"),by=.(year)]
p[(year==2018 & age==60) | (year==2017 & age==59)]

p[,nextYearMultiplier:=popplus1yearage/pop]
p[,popplus1year:=NULL]
p[,popplus1yearage:=NULL]
p2018 <- p[year==2018]
p[,nextYearMultiplier:=NULL]
p2019 <- p[year==2019]

a <- copy(p2019)
b <- copy(p2019)
a[,isRF:=TRUE]
b[,isRF:=FALSE]

p2019 <- rbind(a,b)
p2019[,isLE64:= age<=64 ]

p2019[isRF==TRUE & isLE64==TRUE,pop:=pop*0.083]
p2019[isRF==FALSE & isLE64==TRUE,pop:=pop*(1-0.083)]
p2019[isRF==TRUE & isLE64==FALSE,pop:=pop*0.338]
p2019[isRF==FALSE & isLE64==FALSE,pop:=pop*(1-0.338)]

p2019[,vaccinatedPerc:=0]
p2019[age<=64 & isRF==T,vaccinatedPerc:=0.15]
p2019[age>64,vaccinatedPerc:=0.15]

p2019[,vaxjan:=pop*vaccinatedPerc*0.9]
p2019[,popProp:=pop/sum(pop),by=.(isRF,isLE64)]
p2019[,vaccinatedPerc:=NULL]

# data is now setup here
p2019 <- Vaccinate(p2019, 60000)

# get 2020 jan
p2020 <- NextYear(px=p2019,p2018=p2018)
p2020 <- Vaccinate(p2020, 90000)

p2021 <- NextYear(px=p2020,p2018=p2018)
p2021 <- Vaccinate(p2021, 150000)

p2022 <- NextYear(px=p2021,p2018=p2018)
p2022 <- Vaccinate(p2022, 60000)

px <- rbind(p2019,p2020,p2021,p2022)

px[,group:=""]
px[isRF==T & isLE64==T,group:="Risk/<=64"]
px[isRF==F & isLE64==T,group:="Healthy/<=64"]
px[isRF==T & isLE64==F,group:="Risk/>=65"]
px[isRF==F & isLE64==F,group:="Healthy/>=65"]

a <- px[,.(
  pop=round(sum(pop)),
  vaxgiven=sum(vaxgiven),
  coveragejan=round(100*sum(vaxjan)/sum(pop),1),
  coveragedec=round(100*sum(vaxdec)/sum(pop),1)
),keyby=.(year,group)]

b <- px[,.(
  pop=round(sum(pop)),
  vaxgiven=sum(vaxgiven),
  coveragejan=round(100*sum(vaxjan)/sum(pop),1),
  coveragedec=round(100*sum(vaxdec)/sum(pop),1)
),keyby=.(year)]
b[,group:="Norway"]

res <- rbind(a,b)

xlsx::write.xlsx(res, file.path(org::PROJ$SHARED_TODAY,"res.xlsx"), row.names = F)

des <- glue::glue("
We start off with the 2018 and 2019 cohorts (1 year age groups). \\
We determine what proportion of people progress to the next year \\
(e.g. if we have 50 4 year olds in 2018 and 100 5 year olds in 2019, then \\
there is a 200% progression for that age group). We use these numbers \\
to advance the cohort each year in the future.

We split the cohort into four groups:
- Healthy/<=64 [100-8.3%] (never receives vaccines)
- Risk/<=64 [8.3%]

- Healthy/>=65 [100-33.8%]
- Risk/>=65 [33.8%]

When the cohort advances a year, we remove some of the \\
Healthy 64->65 year olds and place them in the Risk 65 year old \\
group to maintain the correct ratios (as specified above).

In Jan 2019, we assumed that Risk/<=64, Healthy/>=65, Risk/>=65 all had \\
15*0.9=13.5% vaccine coverate (the 0.9 accounts for the people needing a booster). \\
We then distributed 20000 vaccines to each of the three groups throughout the year. \\
We then looked at their coverage in Dec 2019 (after receiving their vaccines).

We then advanced the cohort 1 year into Jan 2020. This means that all people became \\
one year older and all vaccine coverage decreased by 10% (accounting for people needing \\
a booster). We then distributed 20000 vaccines to each of the three groups throughout \\
the year. We then looked at their coverage in Dec 2019 (after receiving their vaccines).

This process repeated over and over. Basically, 'coveragejan' is the coverage \\
after removing the people who need boosters and before applying the yearly vaccinations. \\
'coveragedec' is the coverage after applying the yearly vaccinations.

I can reproduce these numbers for any number of vaccinations given to the 4 groups at \\
any year.
")

cat(des, file=fs::path(org::PROJ$SHARED_TODAY,"details.txt"))






p2021 <- NextYear(px=p2020,p2018=p2018)
p2022 <- NextYear(px=p2021,p2018=p2018)

p <- rbind(p,p2020,p2021,p2022)
p[age==5]
p

p
jan1/2019, # 2-64 %8.3% have risk factors -> 15% have vax
65+ 15% have vax, # 33.8% with risk factors

in 2019 we have orded 60k, 20k will go to the 65+, 40k to un

# how many doses do we need to increase, 5%

go into 2022