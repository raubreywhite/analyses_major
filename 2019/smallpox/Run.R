# working group
# 
# Område
# Psykisk og fysisk helse = 163
# Smittevern, miljø og helse = 335
# Helsedata og digitalisering = 138
# Helsetjenester = 151
# Instituttstab / instituttledelse = 90
# Sum = 877

# descriptives
# stratify results by område
# factor analysis?
# 1one factor: kultur og felles identitet: 4-8
# 2one factor: organisering: 9-11
# 3one factor: 12?
# 4one factor: formaliserte samarbeidsstrukturer: 15-21
# 5one factor: samarbeid: 23-26
# 6one factor: medbestemmelse: 27-30
# 7one factor: støtte fra nærmeste leder: 33-36
# 8one factor: områdeledelse: 39-46
# 9one factor: toppledergruppen: 47-51
# 10one factor: lederrolle: 52-56
# 
# check that people who are leaders responded correctly to 52/lederrolle
# 
# try graphical lasso to see how the correlations between variables match up
#
# timeframe:
# 

# presentation
# slide 1:
# do response rate by område (graph!!!)
# response rate of leaders? 80 leaders?

# slide 2:

# response rate
# prettier op1 and 2 , 3for descriptive
# fix up op 38
# fix up svartid (compare leaders vs not leaders)
# try to include stratification by young/old people
# try to do factor analysis

# bar graphs:
# 1 graph with only top row (all vs)
# graph which is only
# do chi-squared tests for each of the bar graphs

# for all i noen grad/i stor grad etc questions
# ledere
# medarbeidere
# do 3 statistical tests
# for each that are significant, show alle/stratification afterwards
# graph 1: alle/alle
# 

# op13
# 1 graph that combines, utvalg, program, senter all in 1 graph
# yes/no

# op38
# 1 graph that combines, utvalg, program, senter all in 1 graph
# yes/no
# rank according to most popular

# op31
# definitely show område

# op31
# definitely show leaders/non-leaders
# definitely show område

org::AllowFileManipulationFromInitialiseProject()
org::InitialiseProject(
  HOME = "/git/code_major/2019/op_evaluation/",
  RAW = "/Volumes/crypt_data/org/data_raw/code_major/2019/op_evaluation/",
  SHARED = "/dropbox/analyses/results_shared/code_major/2019/op_evaluation/"
)

library(data.table)
library(ggplot2)

dir.create(file.path(org::PROJ$SHARED_TODAY,"requests"))
dir.create(file.path(org::PROJ$SHARED_TODAY,"descriptives"))
dir.create(file.path(org::PROJ$SHARED_TODAY,"factor_analysis"))
dir.create(file.path(org::PROJ$SHARED_TODAY,"presentation"))

dInfo <- haven::read_sav(file.path(org::PROJ$RAW,"op_spss.sav"))
vars <- names(dInfo)
res <- CleanData(dInfo)
resd <- CleanData(dInfo,detailed=T)
vars <- names(dInfo)[-c(1:3)]

omrade <- haven::as_factor(dInfo$op_1)

levels(omrade) <- c("PF","SM","HD","HT","SI+IL")
omrade <- factor(omrade,levels=c("SM","HT","HD","PF","SI+IL"))

arbeidsoppgaver <- haven::as_factor(dInfo$op_2)
levels(arbeidsoppgaver) <- c("Ledere","Medarbeidere","Medarbeidere","Medarbeidere")
arbeidsoppgaver <- as.character(arbeidsoppgaver)

len <- haven::as_factor(dInfo$op_3)
#levels(len) <- c("Psykisk","Smittevern","Helsedata","Helsetjenester","Instituttstab")


dFAL <- GetDataFAL(dInfo)[arbeidsoppgaver=="Ledere"]
parallel <- psych::fa.parallel(dFAL, fm = 'minres', fa = 'fa')
f <- psych::fa(dFAL, nfactors = 1, rotate = "oblimin",fm="minres")
print(f)
print(f$loadings,cutoff = 0.5)
psych::fa.diagram(f)

minVal <- maxVal <- c()
for(j in 1:1){
  loadings <- matrix(f$loadings,ncol=1)
  dFA1 <- data.table(dFAL[1:5])
  for(i in 1:ncol(dFAL)){
    sign <- 1
    if(loadings[i,j]<0) sign <- -1
    dFA1[,(i):=sign*(1:.N)]
  }
  
  dFA1 <- dFA1[c(1,5)]
  
  dF <- data.table(predict(f,rbind(as.data.frame(dFA1),as.data.frame(dFAL))))
  minVal <- c(minVal,dF[[j]][1])
  maxVal <- c(maxVal,dF[[j]][2])
}

dFL <- data.table(predict(f,dFAL))
setnames(dFL,"lederrolle")

for(i in 1:ncol(dF)){
  minVal[i] <- min(dFL[[i]],minVal[i])
  maxVal[i] <- max(dFL[[i]],maxVal[i])
  
  dFL[[i]] <- (dFL[[i]]-minVal[i])/(maxVal[i]-minVal[i])
}


dFA <- GetDataFA(dInfo)

parallel <- psych::fa.parallel(dFA, fm = 'minres', fa = 'fa')
f <- psych::fa(dFA, nfactors = 5, rotate = "oblimin",fm="minres")
print(f)
print(f$loadings,cutoff = 0.5)
psych::fa.diagram(f)

minVal <- maxVal <- c()
for(j in 1:5){
  loadings <- matrix(f$loadings,ncol=5)
  dFA1 <- dFA[1:5]
  for(i in 1:ncol(dFA)){
    sign <- 1
    if(loadings[i,j]<0) sign <- -1
    dFA1[,(i):=sign*(1:.N)]
  }
  
  dFA1 <- dFA1[c(1,5)]
  
  dF <- data.table(predict(f,rbind(as.data.frame(dFA1),as.data.frame(dFA))))
  minVal <- c(minVal,dF[[j]][1])
  maxVal <- c(maxVal,dF[[j]][2])
}

dF <- data.table(predict(f,dFA))

setnames(dF,c(
  "Struktur/\nKultur",
  "Område-\nledelse/\nMedbestem-\nmelse",
  "Utvalg/\nSentre/\nProgrammer",
  "Toppledelse",
  "Støtte fra\nnærmeste\nleder"
))
for(i in 1:ncol(dF)){
  minVal[i] <- min(dF[[i]],minVal[i])
  maxVal[i] <- max(dF[[i]],maxVal[i])
  
  dF[[i]] <- (dF[[i]]-minVal[i])/(maxVal[i]-minVal[i])
}

dF[,c_omrade:=omrade]
dF[,c_arbeidsoppgaver:=arbeidsoppgaver]
dF[,c_len:=len]

dF[c_arbeidsoppgaver=="Ledere",Lederrolle:=dFL$lederrolle]

pd <- na.omit(melt.data.table(dF,id.vars = c("c_omrade","c_arbeidsoppgaver","c_len")))

# arbeidsoppgaver
q <- ggplot(pd[variable!="Lederrolle"],aes(x=c_arbeidsoppgaver,y=value,fill=c_arbeidsoppgaver))
q <- q + geom_hline(colour="black",yintercept=0)
q <- q + geom_hline(colour="black",yintercept=1)
q <- q + stat_summary(fun.y=mean, geom="col",show.legend = T)
q <- q + lemon::facet_rep_grid(~variable,repeat.tick.labels = "y",scales="free_x")
q <- q + scale_fill_brewer(name=" ",palette="Set1")
q <- q + scale_y_continuous("Tilfredshetsskala",lim=c(0,1),breaks=seq(0,1,0.25),labels=c(1:5))
q <- q + scale_x_discrete("")
q <- q + theme_gray(20)
q <- q + theme(legend.position="bottom")
q <- q + theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5))
q
RAWmisc::saveA4(q,
                file.path(org::PROJ$SHARED_TODAY,"factor_analysis","mean_arbeidsoppgaver.png"))

# omrade

q <- ggplot(pd[variable!="Lederrolle"],aes(x=c_omrade,y=value,fill=c_omrade))
q <- q + geom_hline(colour="black",yintercept=0)
q <- q + geom_hline(colour="black",yintercept=1)
q <- q + stat_summary(fun.y=mean, geom="col",show.legend = T)
q <- q + lemon::facet_rep_grid(~variable,repeat.tick.labels = "y",scales="free_x")
q <- q + scale_fill_brewer(name=" ",palette="Set1")
q <- q + scale_y_continuous("Tilfredshetsskala",lim=c(0,1),breaks=seq(0,1,0.25),labels=c(1:5))
q <- q + scale_x_discrete("")
q <- q + theme_gray(20)
q <- q + theme(legend.position="bottom")
q <- q + theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5))
q
RAWmisc::saveA4(q,
                file.path(org::PROJ$SHARED_TODAY,"factor_analysis","mean_omrade.png"))

# lengde

q <- ggplot(pd[variable!="Lederrolle"],aes(x=c_len,y=value,fill=c_len))
q <- q + geom_hline(colour="black",yintercept=0)
q <- q + geom_hline(colour="black",yintercept=1)
q <- q + stat_summary(fun.y=mean, geom="col",show.legend = T)
q <- q + lemon::facet_rep_grid(~variable,repeat.tick.labels = "y",scales="free_x")
q <- q + scale_fill_brewer(name=" ",palette="Set1")
q <- q + scale_y_continuous("Tilfredshetsskala",lim=c(0,1),breaks=seq(0,1,0.25),labels=c(1:5))
q <- q + scale_x_discrete("")
q <- q + theme_gray(20)
q <- q + theme(legend.position="bottom")
q <- q + theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5))
q
RAWmisc::saveA4(q,
                file.path(org::PROJ$SHARED_TODAY,"factor_analysis","mean_lengde.png"))

# detailed mean

q <- ggplot(pd[variable!="Lederrolle"],aes(x=c_omrade,y=value,fill=c_arbeidsoppgaver))
q <- q + geom_hline(colour="black",yintercept=0)
q <- q + geom_hline(colour="black",yintercept=1)
q <- q + stat_summary(fun.y=mean, geom="col",show.legend = T, position="dodge", width=0.9)
q <- q + lemon::facet_rep_grid(~variable,repeat.tick.labels = "y",scales="free_x")
q <- q + scale_fill_brewer(name=" ",palette="Set1")
q <- q + scale_y_continuous("Tilfredshetsskala",lim=c(0,1),breaks=seq(0,1,0.25),labels=c(1:5))
q <- q + scale_x_discrete("")
q <- q + theme_gray(20)
q <- q + theme(legend.position="bottom")
q <- q + theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5))
q
RAWmisc::saveA4(q,
                file.path(org::PROJ$SHARED_TODAY,"factor_analysis","mean_detailed_0.png"))

# leader
# detailed mean

q <- ggplot(pd[variable=="Lederrolle"],aes(x=c_omrade,y=value,fill=c_omrade))
q <- q + geom_hline(colour="black",yintercept=0)
q <- q + geom_hline(colour="black",yintercept=1)
q <- q + stat_summary(fun.y=mean, geom="col",show.legend = T, position="dodge", width=0.9)
q <- q + lemon::facet_rep_grid(~variable,repeat.tick.labels = "y",scales="free_x")
q <- q + scale_fill_brewer(name=" ",palette="Set1")
q <- q + scale_y_continuous("Tilfredshetsskala",lim=c(0,1),breaks=seq(0,1,0.25),labels=c(1:5))
q <- q + scale_x_discrete("")
q <- q + theme_gray(20)
q <- q + theme(legend.position="bottom")
q <- q + theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5))
q
RAWmisc::saveA4(q,
                file.path(org::PROJ$SHARED_TODAY,"factor_analysis","mean_leader.png"))



x1 <- pd[variable=="Lederrolle",.(
  value=1+4*mean(value)
),keyby=.(c_omrade)]
x2 <- copy(x1)
x2[,value:=value-(1+4*mean(pd[variable=="Lederrolle"]$value))]
x1[,start:=1]
x2[,start:=0]
x1[,type:="Absolutt"]
x2[,type:="Relativ"]
x0 <- copy(x1)
x0[1,value:=5]
x <- rbind(x1,x2)

q <- ggplot(x,aes(x=c_omrade,y=value,fill=c_omrade))
#q <- q + geom_hline(data=x[type=="Absolut"],colour="black",yintercept=0)
#q <- q + geom_hline(data=x[type=="Absolut"],colour="black",yintercept=1)
q <- q + geom_segment(aes(x=c_omrade,xend=c_omrade,y=start, yend=value,color=c_omrade), size=15)
q <- q + geom_segment(data=x0,aes(x=c_omrade,xend=c_omrade,y=start, yend=value,color=c_omrade), size=15,alpha=0)
#q <- q + stat_summary(fun.y=mean, geom="col",show.legend = T, position="dodge", width=0.9)
#q <- q + stat_summary(data=x0,fun.y=mean, geom="col",show.legend = T, position="dodge", width=0.9,alpha=0)
q <- q + facet_wrap(~type,scales="free")
q <- q + scale_color_brewer(name=" ",palette="Set1")
q <- q + scale_y_continuous("Tilfredshetsskala")
q <- q + scale_x_discrete("")
q <- q + theme_gray(20)
q <- q + theme(legend.position="bottom")
q <- q + theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5))
q
RAWmisc::saveA4(q,
                file.path(org::PROJ$SHARED_TODAY,"factor_analysis","mean_leader_abs_rel.png"))






for(i in seq_along(vars)){
  varx <- vars[i]
  if(stringr::str_detect(varx,"^op_13")) next
  if(stringr::str_detect(varx,"^op_38")) next
  if(varx %in% c("op_18_10","op_22_2","op_23_4","op_57","op_58","svartid")) next
  
  all <- TRUE
  if(PlotBy(res=res,varx=varx,groupingx="c_omrade")) all <- FALSE
  if(PlotBy(res=res,varx=varx,groupingx="c_arbeidsoppgaver",force=(varx=="op_31"))) all <- FALSE
  if(PlotBy(res=res,varx=varx,groupingx="c_lenge")) all <- FALSE
  
  if(all) PlotByNothing(res=res,varx=varx)
}

pd <- res[stringr::str_detect(var,"^op_13") & c_lenge=="Alle" & c_omrade=="Alle"]
pd[,place:=stringr::str_extract(string = varName, pattern = "(?<=; ).*(?=;)")]
pd[value=="Ikke valgt",value:="Nei"]
pd[value!="Nei",value:="Ja"]
pd[,value:=factor(value,levels=c("Nei","Ja"))]
pd[place=="Kjenner ikke til noen av disse",place:="Kjenner ikke"]
pd <- pd[value=="Ja",c("place","value","c_arbeidsoppgaver","N","denom")]

pdx <- res[stringr::str_detect(var,"^op_14_1") & c_lenge=="Alle" & c_omrade=="Alle"]
pdx <- pdx[x<10,c("c_arbeidsoppgaver","N","x","value")]
pdx[,propKnowledge:=N/sum(N),by=.(c_arbeidsoppgaver)]
pdx[,N:=NULL]
pdx1 <- copy(pdx)
pdx1[,place:="Utvalg"]

pdx <- res[stringr::str_detect(var,"^op_14_2") & c_lenge=="Alle" & c_omrade=="Alle"]
pdx <- pdx[x<10,c("c_arbeidsoppgaver","N","x","value")]
pdx[,propKnowledge:=N/sum(N),by=.(c_arbeidsoppgaver)]
pdx[,N:=NULL]
pdx2 <- copy(pdx)
pdx2[,place:="Programmer"]

pdx <- res[stringr::str_detect(var,"^op_14_3") & c_lenge=="Alle" & c_omrade=="Alle"]
pdx <- pdx[x<10,c("c_arbeidsoppgaver","N","x","value")]
pdx[,propKnowledge:=N/sum(N),by=.(c_arbeidsoppgaver)]
pdx[,N:=NULL]
pdx3 <- copy(pdx)
pdx3[,place:="Sentre"]

pdx <- rbind(pdx1,pdx2,pdx3)

p <- merge(pd,pdx,by=c("c_arbeidsoppgaver","place"),all=T)
p[,finalProp:=propKnowledge*N/denom]
p[is.na(finalProp),finalProp:=N/denom]
p[is.na(value.y),value.y:="Kjenner ikke"]
p[,value.y:=factor(value.y,levels=(c(
  "Kjenner ikke",
  "I svært liten grad",
  "I liten grad",
  "I noen grad",
  "I stor grad",
  "I svært stor grad" 
)))]

q <- ggplot(p,aes(x=place, fill=value.y))
q <- q + geom_col(alpha=0.8,mapping=aes(y=finalProp),colour="black")
#q <- q + geom_text(mapping=aes(y=N/denom+0.1,label=sprintf("%s%%",round(N/denom*100))),size=2)
q <- q + lemon::facet_rep_grid(.~c_arbeidsoppgaver,scales="free", repeat.tick.labels="x")
q <- q + scale_y_continuous("Andel",labels = scales::percent_format(),lim=c(0,1))
q <- q + scale_x_discrete("",drop=T)
#q <- q + coord_flip()
q <- q + scale_fill_brewer("",palette="RdBu")
q <- q + theme_gray(20)
q <- q + theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5))
q
RAWmisc::saveA4(q,
                file.path(org::PROJ$SHARED_TODAY,"presentation","op_13.png"))



pd <- res[stringr::str_detect(var,"^op_38") & c_lenge=="Alle" & c_omrade=="Alle" & c_arbeidsoppgaver=="Alle" & N>0]
pd[,place:=stringr::str_extract(string = varName, pattern = "(?<=; ).*(?=;)")]
pd[is.na(x), value:="Nei"]
pd[!is.na(x) & place=="Vet ikke", value:="Ja"]
pd[value=="Ikke valgt",value:="Nei"]
pd[value!="Nei",value:="Ja"]
pd[,value:=factor(value,levels=c("Nei","Ja"))]

q <- ggplot(pd[value=="Ja"],aes(x=reorder(place,N)))
q <- q + geom_col(alpha=0.8,mapping=aes(y=N/denom,fill=place),colour="black",show.legend = F)
#q <- q + geom_text(mapping=aes(y=N/denom+0.1,label=sprintf("%s%%",round(N/denom*100))),size=2)
#q <- q + lemon::facet_rep_grid(c_arbeidsoppgaver~.,scales="free", repeat.tick.labels="x")
q <- q + scale_y_continuous("Andel",labels = scales::percent_format(),lim=c(0,1))
q <- q + scale_x_discrete("",drop=T)
q <- q + coord_flip()
q <- q + scale_fill_brewer("",palette="Set1")
q <- q + theme_gray(20)
q
RAWmisc::saveA4(q,
                file.path(org::PROJ$SHARED_TODAY,"presentation","op_38.png"))

pd <- res[stringr::str_detect(var,"^op_38") & c_lenge=="Alle" & c_omrade!="Alle" & c_arbeidsoppgaver=="Alle" & N>0]
pd[,place:=stringr::str_extract(string = varName, pattern = "(?<=; ).*(?=;)")]
pd[is.na(x), value:="Nei"]
pd[!is.na(x) & place=="Vet ikke", value:="Ja"]
pd[value=="Ikke valgt",value:="Nei"]
pd[value!="Nei",value:="Ja"]
pd[,value:=factor(value,levels=c("Nei","Ja"))]

q <- ggplot(pd[value=="Ja"],aes(x=reorder(place,N)))
q <- q + geom_col(alpha=0.8,mapping=aes(y=N/denom,fill=c_omrade_short),colour="black",position="dodge")
#q <- q + geom_text(mapping=aes(y=N/denom+0.1,label=sprintf("%s%%",round(N/denom*100))),size=2)
#q <- q + lemon::facet_rep_grid(.~c_omrade,scales="free", repeat.tick.labels="x")
q <- q + scale_y_continuous("Andel",labels = scales::percent_format(),lim=c(0,1))
q <- q + scale_x_discrete("",drop=T)
q <- q + coord_flip()
q <- q + scale_fill_brewer("",palette="Set1")
q <- q + theme_gray(20)
q
RAWmisc::saveA4(q,
                file.path(org::PROJ$SHARED_TODAY,"presentation","op_38_omrade.png"))


#### op_21

pd <- res[var=="op_21" & c_lenge=="Alle" & value!="Vet ikke/ikke svart"]
pd[,c_omrade_short:=factor(c_omrade_short,levels=c("Alle","HD","SI+IL","PF","SM","HT"))]
pd[,value:=factor(value,levels=c(
  "I svært liten grad",
  "I liten grad",
  "I noen grad",
  "I stor grad",
  "I svært stor grad" 
  ))]
q <- ggplot(pd[denom>=10],aes(x=c_omrade_short, fill=value))
q <- q + geom_col(alpha=0.8,mapping=aes(y=N/denom),colour="black")
#q <- q + geom_text(mapping=aes(y=N/denom+0.1,label=sprintf("%s%%",round(N/denom*100))),size=2)
q <- q + lemon::facet_rep_grid(.~c_arbeidsoppgaver,scales="free", repeat.tick.labels="y")
q <- q + scale_y_continuous("Andel",labels = scales::percent_format(),lim=c(0,1))
q <- q + scale_x_discrete("",drop=T)
#q <- q + coord_flip()
q <- q + scale_fill_brewer("",palette="RdBu")
q <- q + theme_gray(20)
#q <- q + labs(title=Shorten(attributes(dInfo[["op_21"]])$label,len=70))
q <- q + labs(caption="P-verdier: ledere mot medarbeidere<0,001; område=0,005")
q <- q + theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5))
q
RAWmisc::saveA4(q,
                file.path(org::PROJ$SHARED_TODAY,"presentation",sprintf("%s_%s.png","op_21","FIXED")))




#### op_22

pd <- res[var=="op_22_1" & x<10 & value!="Ingen av disse" ]
pd <- pd[
  (c_omrade=="Alle" & c_lenge=="Alle" & c_arbeidsoppgaver=="Alle") |
  (c_omrade!="Alle" & c_lenge=="Alle" & c_arbeidsoppgaver=="Alle") |
    (c_omrade=="Alle" & c_lenge!="Alle" & c_arbeidsoppgaver=="Alle") |
    (c_omrade=="Alle" & c_lenge=="Alle" & c_arbeidsoppgaver!="Alle")
  ]
pd[,label:=c_omrade_short]
pd[c_lenge!="Alle",label:=c_lenge]
pd[c_arbeidsoppgaver!="Alle",label:=c_arbeidsoppgaver]

pd[,label:=factor(label)]
levels(pd$label) <- c(
  "Alle",
  "Ledere",
  "Medarbeidere"  ,
  "0–3 år",
  "4 år eller lengre",
  "SI+IL",
  "SM",
  "PF",
  "HD",
  "HT"
)

ord <- unique(pd[,c("value","x")])
ord[x==5,x:=-1]
setorder(ord,x)
pd[,value:=factor(value,levels=ord$value)]

#pd[,c_omrade_short:=factor(c_omrade_short,levels=c("Alle","HDLE","SILE/ILDI","PFLE","SMOL","HTLE"))]

q <- ggplot(pd[denom>=10],aes(x=label, fill=value))
q <- q + geom_col(alpha=0.8,mapping=aes(y=N/denom),colour="black")
#q <- q + lemon::facet_rep_wrap(~label,scales="free", repeat.tick.labels="all",ncol=5)
q <- q + scale_y_continuous("Andel",labels = scales::percent_format(),lim=c(0,1))
q <- q + scale_x_discrete("",drop=T)
q <- q + scale_fill_brewer("",palette="Set1")
q <- q + theme_gray(20)
q <- q + labs(caption="P-verdier: ledere mot medarbeidere<0,001; område<0,001; lenge=0.014")
q <- q + theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5))
q <- q + guides(fill = guide_legend(nrow=2))
q <- q + theme(legend.position="bottom")
q
RAWmisc::saveA4(q,
                file.path(org::PROJ$SHARED_TODAY,"presentation",sprintf("%s_%s.png","op_22_1","FIXED")))


# 23_1 + 23_2 + 23_3 all in the same graph

pd <- res[var=="op_23_1" & x<10 & value!="Ingen av disse" ]
pd <- pd[
  (c_omrade=="Alle" & c_lenge=="Alle" & c_arbeidsoppgaver=="Alle") |
  (c_omrade!="Alle" & c_lenge=="Alle" & c_arbeidsoppgaver=="Alle")
  ]
pd[,label:=c_omrade_short]
pd[c_lenge!="Alle",label:=c_lenge]
pd[c_arbeidsoppgaver!="Alle",label:=c_arbeidsoppgaver]
pd0 <- pd

pd <- res[var=="op_23_2" & x<10 & value!="Ingen av disse" ]
pd <- pd[
  (c_omrade=="Alle" & c_lenge=="Alle" & c_arbeidsoppgaver=="Alle") |
  (c_omrade!="Alle" & c_lenge=="Alle" & c_arbeidsoppgaver=="Alle") | 
    (c_omrade=="Alle" & c_lenge=="Alle" & c_arbeidsoppgaver!="Alle") 
  ]
pd[,label:=c_omrade_short]
pd[c_lenge!="Alle",label:=c_lenge]
pd[c_arbeidsoppgaver!="Alle",label:=c_arbeidsoppgaver]
pd1<- pd

pd <- res[var=="op_23_3" & x<10 & value!="Ingen av disse" ]
pd <- pd[
  (c_omrade=="Alle" & c_lenge=="Alle" & c_arbeidsoppgaver=="Alle") |
  (c_omrade!="Alle" & c_lenge=="Alle" & c_arbeidsoppgaver=="Alle")
  ]
pd[,label:=c_omrade_short]
pd[c_lenge!="Alle",label:=c_lenge]
pd[c_arbeidsoppgaver!="Alle",label:=c_arbeidsoppgaver]
pd2 <- pd

pd <- rbind(pd0,pd1,pd2)

pd[,label:=factor(as.character(label),levels=c(
  "Alle",
  "Ledere",
  "Medarbeidere",
  "SI+IL",
  "SM",
  "PF",
  "HD",
  "HT"
))]

ord <- unique(pd[,c("value","x")])
#ord[x==5,x:=-1]
setorder(ord,x)
pd[,value:=factor(value,levels=ord$value)]
pd$value
#pd[,c_omrade_short:=factor(c_omrade_short,levels=c("Alle","HDLE","SILE/ILDI","PFLE","SMOL","HTLE"))]
pd[,varName:=factor(varName)]
levels(pd$varName) <- c("Utvalg og senter","Personlige nettverk","Ulike faglige fora")

q <- ggplot(pd,aes(x=label, fill=value))
q <- q + geom_col(alpha=0.8,mapping=aes(y=N/denom),colour="black")
q <- q + lemon::facet_rep_wrap(~varName,scales="free", repeat.tick.labels="all",ncol=5)
q <- q + scale_y_continuous("Andel",labels = scales::percent_format(),lim=c(0,1))
q <- q + scale_x_discrete("",drop=T)
q <- q + scale_fill_brewer("",palette="RdBu", direction = 1)
q <- q + theme_gray(20)
q <- q + labs(caption="Signifikante forskjeller påvist")
q <- q + theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5))
q <- q + guides(fill = guide_legend(nrow=1))
q <- q + theme(legend.position="bottom")
q
RAWmisc::saveA4(q,
                file.path(org::PROJ$SHARED_TODAY,"presentation","23_1 + 23_2 + 23_3.png"))


# 24

pd <- res[var=="op_24" & x<10 & value!="Ingen av disse" ]
pd <- pd[
  (c_omrade=="Alle" & c_lenge=="Alle" & c_arbeidsoppgaver=="Alle") |
    (c_omrade!="Alle" & c_lenge=="Alle" & c_arbeidsoppgaver=="Alle") |
    (c_omrade=="Alle" & c_lenge=="Alle" & c_arbeidsoppgaver!="Alle")
  ]
pd[,label:=c_omrade_short]
pd[c_lenge!="Alle",label:=c_lenge]
pd[c_arbeidsoppgaver!="Alle",label:=c_arbeidsoppgaver]

pd[,label:=factor(label)]
levels(pd$label) <- c(
  "Alle",
  "Ledere",
  "Medarbeidere"  ,
  "SI+IL",
  "SM",
  "PF",
  "HD",
  "HT"
)

ord <- unique(pd[,c("value","x")])
setorder(ord,x)
pd[,value:=factor(value,levels=ord$value)]

#pd[,c_omrade_short:=factor(c_omrade_short,levels=c("Alle","HDLE","SILE/ILDI","PFLE","SMOL","HTLE"))]

q <- ggplot(pd[denom>=10],aes(x=label, fill=value))
q <- q + geom_col(alpha=0.8,mapping=aes(y=N/denom),colour="black")
#q <- q + lemon::facet_rep_wrap(~label,scales="free", repeat.tick.labels="all",ncol=5)
q <- q + scale_y_continuous("Andel",labels = scales::percent_format(),lim=c(0,1))
q <- q + scale_x_discrete("",drop=T)
q <- q + scale_fill_brewer("",palette="RdBu")
q <- q + theme_gray(20)
q <- q + labs(caption="Signifikante forskjeller påvist")
q <- q + theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5))
q <- q + guides(fill = guide_legend(nrow=1))
q <- q + theme(legend.position="bottom")
q
RAWmisc::saveA4(q,
                file.path(org::PROJ$SHARED_TODAY,"presentation","op_24_FIXED.png"))

# 25 and 26 together


pd <- res[var=="op_25" & x<10 & value!="Ingen av disse" ]
pd <- pd[
  (c_omrade=="Alle" & c_lenge=="Alle" & c_arbeidsoppgaver=="Alle") |
    (c_omrade!="Alle" & c_lenge=="Alle" & c_arbeidsoppgaver=="Alle") |
    (c_omrade=="Alle" & c_lenge=="Alle" & c_arbeidsoppgaver!="Alle")
  ]
pd[,label:=c_omrade_short]
pd[c_lenge!="Alle",label:=c_lenge]
pd[c_arbeidsoppgaver!="Alle",label:=c_arbeidsoppgaver]
pd0 <- pd

pd <- res[var=="op_26" & x<10 & value!="Ingen av disse" ]
pd <- pd[
  (c_omrade=="Alle" & c_lenge=="Alle" & c_arbeidsoppgaver=="Alle") |
    (c_omrade=="Alle" & c_lenge=="Alle" & c_arbeidsoppgaver!="Alle") 
  ]
pd[,label:=c_omrade_short]
pd[c_lenge!="Alle",label:=c_lenge]
pd[c_arbeidsoppgaver!="Alle",label:=c_arbeidsoppgaver]
pd1<- pd

pd <- rbind(pd0,pd1)

pd[,label:=factor(as.character(label),levels=c(
  "Alle",
  "Ledere",
  "Medarbeidere",
  "SI+IL",
  "SM",
  "PF",
  "HD",
  "HT"
))]

ord <- unique(pd[,c("value","x")])
#ord[x==5,x:=-1]
setorder(ord,x)
pd[,value:=factor(value,levels=ord$value)]
pd$value
#pd[,c_omrade_short:=factor(c_omrade_short,levels=c("Alle","HDLE","SILE/ILDI","PFLE","SMOL","HTLE"))]
pd[,varName:=factor(varName)]
levels(pd$varName) <- c("Driftsoppgaver","Forskning og utviklingsoppgaver")

q <- ggplot(pd,aes(x=label, fill=value))
q <- q + geom_col(alpha=0.8,mapping=aes(y=N/denom),colour="black")
q <- q + lemon::facet_rep_wrap(~varName,scales="free", repeat.tick.labels="all",ncol=5)
q <- q + scale_y_continuous("Andel",labels = scales::percent_format(),lim=c(0,1))
q <- q + scale_x_discrete("",drop=T)
q <- q + scale_fill_brewer("",palette="RdBu", direction = 1)
q <- q + theme_gray(20)
q <- q + labs(caption="Signifikante forskjeller påvist")
q <- q + theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5))
q <- q + guides(fill = guide_legend(nrow=1))
q <- q + theme(legend.position="bottom")
q
RAWmisc::saveA4(q,
                file.path(org::PROJ$SHARED_TODAY,"presentation","25 + 26.png"))


# 31 and 32 together (two panels)


pd <- res[var=="op_31" & x<10 & value!="Ingen av disse" ]
pd <- pd[
  (c_omrade=="Alle" & c_lenge=="Alle" & c_arbeidsoppgaver=="Alle") |
    (c_omrade!="Alle" & c_lenge=="Alle" & c_arbeidsoppgaver=="Alle") |
    (c_omrade=="Alle" & c_lenge=="Alle" & c_arbeidsoppgaver!="Alle")
  ]
pd[,label:=c_omrade_short]
pd[c_lenge!="Alle",label:=c_lenge]
pd[c_arbeidsoppgaver!="Alle",label:=c_arbeidsoppgaver]
pd0 <- pd

pd <- res[var=="op_32" & x<10 & value!="Ingen av disse" ]
pd <- pd[
  (c_omrade=="Alle" & c_lenge=="Alle" & c_arbeidsoppgaver=="Alle") |
    (c_omrade!="Alle" & c_lenge=="Alle" & c_arbeidsoppgaver=="Alle") |
    (c_omrade=="Alle" & c_lenge=="Alle" & c_arbeidsoppgaver!="Alle")
  ]
pd[,label:=c_omrade_short]
pd[c_lenge!="Alle",label:=c_lenge]
pd[c_arbeidsoppgaver!="Alle",label:=c_arbeidsoppgaver]
pd1<- pd

pd <- rbind(pd0,pd1)

pd[,label:=factor(as.character(label),levels=c(
  "Alle",
  "Ledere",
  "Medarbeidere",
  "SI+IL",
  "SM",
  "PF",
  "HD",
  "HT"
))]

ord <- unique(pd[,c("value","x")])
#ord[x==5,x:=-1]
setorder(ord,-x)
pd[,value:=factor(value,levels=ord$value)]
pd$value
#pd[,c_omrade_short:=factor(c_omrade_short,levels=c("Alle","HDLE","SILE/ILDI","PFLE","SMOL","HTLE"))]
pd[,varName:=factor(varName)]
levels(pd$varName) <- c("Er det tydelig for deg\nhvem som er din nærmeste leder?",
                        "Er din faglige leder og\npersonalleder samme person?")

q <- ggplot(pd[value=="Ja"],aes(x=label))
q <- q + geom_col(alpha=0.8,mapping=aes(y=N/denom,fill=label),colour="black",show.legend = F)
q <- q + lemon::facet_rep_wrap(~varName,scales="free", repeat.tick.labels="all",ncol=5)
q <- q + scale_y_continuous("Andel",labels = scales::percent_format(),lim=c(0,1))
q <- q + scale_x_discrete("",drop=T)
q <- q + scale_fill_brewer("",palette="Set1", direction = 1)
q <- q + theme_gray(20)
q <- q + labs(caption="Signifikante forskjeller påvist")
q <- q + theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5))
q <- q + guides(fill = guide_legend(nrow=1))
q <- q + theme(legend.position="bottom")
q
RAWmisc::saveA4(q,
                file.path(org::PROJ$SHARED_TODAY,"presentation","31+32.png"))



# 37 


pd <- res[var=="op_37" & x<10 & value!="Ingen av disse" ]
pd <- pd[
  (c_omrade=="Alle" & c_lenge=="Alle" & c_arbeidsoppgaver=="Alle") |
    (c_omrade!="Alle" & c_lenge=="Alle" & c_arbeidsoppgaver=="Alle") |
    (c_omrade=="Alle" & c_lenge=="Alle" & c_arbeidsoppgaver!="Alle") |
    (c_omrade=="Alle" & c_lenge!="Alle" & c_arbeidsoppgaver=="Alle")
  ]
pd[,label:=c_omrade_short]
pd[c_lenge!="Alle",label:=c_lenge]
pd[c_arbeidsoppgaver!="Alle",label:=c_arbeidsoppgaver]

pd[,label:=factor(as.character(label),levels=c(
  "Alle",
  "Ledere",
  "Medarbeidere",
  "0–3 år",
  "4 år eller lengre",
  "SI+IL",
  "SM",
  "PF",
  "HD",
  "HT"
))]

ord <- unique(pd[,c("value","x")])
setorder(ord,-x)
pd[,value:=factor(value,levels=ord$value)]

#pd[,c_omrade_short:=factor(c_omrade_short,levels=c("Alle","HDLE","SILE/ILDI","PFLE","SMOL","HTLE"))]

q <- ggplot(pd[denom>=10],aes(x=label, fill=value))
q <- q + geom_col(alpha=0.8,mapping=aes(y=N/denom),colour="black")
#q <- q + lemon::facet_rep_wrap(~label,scales="free", repeat.tick.labels="all",ncol=5)
q <- q + scale_y_continuous("Andel",labels = scales::percent_format(),lim=c(0,1))
q <- q + scale_x_discrete("",drop=T)
q <- q + scale_fill_brewer("",palette="RdBu",direction = -1)
q <- q + theme_gray(20)
q <- q + labs(caption="Signifikante forskjeller påvist")
q <- q + theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5))
q <- q + guides(fill = guide_legend(nrow=1))
q <- q + theme(legend.position="bottom")
q
RAWmisc::saveA4(q,
                file.path(org::PROJ$SHARED_TODAY,"presentation","op_37_FIXED.png"))

# 38 order it but only show "alle"

# make response rate prettier
# 


# response rate (flip it, use good abreviations)
pd <- res[var=="op_1" & c_arbeidsoppgaver=="Alle" & c_omrade=="Alle"  & c_lenge=="Alle"]
pd[,denom:=0]
pd[value=="Psykisk og fysisk helse",denom:=163]
pd[value=="Smittevern, miljø og helse",denom:=335]
pd[value=="Helsedata og digitalisering",denom:=138]
pd[value=="Helsetjenester",denom:=151]
pd[value=="Instituttstab / instituttledelse",denom:=90]
pd[,prop:=N/denom]
setorder(pd,prop)
pd[,value:=factor(value)]
levels(pd$value) <- c("HD","HT","SI+IL","PF","SM")
pd[,value:=factor(value,levels=c("HD","SI+IL","HT","PF","SM"))]

q <- ggplot(pd,aes(x=value,fill=value))
q <- q + geom_col(alpha=0.8,mapping=aes(y=N/denom))
q <- q + geom_text(mapping=aes(y=N/denom+0.05, label=sprintf("%s%%",round(100*N/denom))),size=10,hjust=0.5)
q <- q + scale_y_continuous("",labels=scales::percent,lim=c(0,1))
q <- q + scale_x_discrete("")
#q <- q + coord_flip()
#q <- q + labs(title=Shorten(attributes(dInfo[[vars[i]]])$label,len=70))
q <- q + scale_fill_brewer("",palette="Set1",guide=FALSE)
q <- q + theme_gray(30)
q
RAWmisc::saveA4(q,
                file.path(org::PROJ$SHARED_TODAY,"presentation","response_rate.png"))


#op_1
# no numbers
# show 2 "alle" panels only. first panel, colour by leader/not leader, second panel colour by 0-3/4+
pd1 <- resd[var=="op_1" & c_omrade!="Alle"  & c_lenge=="Alle" & !c_arbeidsoppgaver%in%c("Alle","Ledere")]
pd2 <- res[var=="op_1" & c_omrade!="Alle"  & c_lenge=="Alle" & c_arbeidsoppgaver!="Alle"]
pd1[,grouping:=c_arbeidsoppgaver]
pd2[,grouping:=c_arbeidsoppgaver]
pd1[,type:="Medarbeidernes arbeidsoppgaver"]
pd2[,type:="Arbeidsoppgaver"]
pd <- rbind(pd1,pd2)
pd[,varName:=NULL]
pd[,value:=factor(value)]
levels(pd$value) <- c("HD","HT","SI+IL","PF","SM")

pd[,grouping:=factor(grouping,levels=c(
  "Ledere",
  "Medarbeidere",
  "Forskning",
  "Faglig produksjon",
  "Admin/Teknisk"
))]

q <- ggplot(pd,aes(x=value,fill=grouping))
q <- q + geom_col(alpha=0.75,mapping=aes(y=N))
#q <- q + geom_text(mapping=aes(y=labX,label=ifelse(N<9,"",sprintf("%s",N))),size=4,hjust=0.5)
q <- q + facet_wrap(~type)
q <- q + scale_y_continuous("Antall")
q <- q + scale_x_discrete("")
q <- q + coord_flip()
#q <- q + labs(title=Shorten(attributes(dInfo[[vars[i]]])$label,len=70))
q <- q + scale_fill_brewer("",palette="Set1")
q <- q + theme_gray(20)
q
RAWmisc::saveA4(q,
                file.path(org::PROJ$SHARED_TODAY,"presentation","op_1.png"))




















