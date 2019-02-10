org::AllowFileManipulationFromInitialiseProject()
org::InitialiseProject(
  HOME = "/git/code_major/2019/MSF_SSudan/",
  RAW = "/Volumes/crypt_data/org/data_raw/code_major/2019/MSF_SSudan/",
  SHARED = "/dropbox/analyses/results_shared/code_major/2019/MSF_SSudan/"
)

library(data.table)
library(mem)
library(ggplot2)

# Your code starts here
sudanExtremeYears <- list()
sudanNames <- readxl::excel_sheets(file.path(org::PROJ$RAW,"sudan.xlsx"))
sudanNames <- sudanNames[sudanNames!="Bunj"]
for(i in sudanNames) for(CUTOFFALPHA in c(0.95,0.975,0.995)){
  d <- readxl::read_excel(file.path(org::PROJ$RAW,"sudan.xlsx"), sheet=i)
  d <- reshape2::melt(d,id.vars="Cases")
  names(d) <- c("wk","year","influensa")
  d$wk <- as.numeric(gsub("w","",d$wk))
  d$wk <- formatC(d$wk,width=2,flag="0")
  d$yrwk <- paste0(d$year,"-",d$wk)
  d$year <- as.numeric(as.character(d$year))
  d <- data.table(d)
  if(i=="LKG") d <- d[!year %in% c(2013,2014)]
  d <- d[!year %in% c(2016)]
  maxobs <- max(d[!is.na(influensa)]$yrwk,na.rm=T)
  d[is.na(influensa) & yrwk < maxobs,influensa:=0]
  dCopy <- copy(d)
  d[,wk:=NULL]
  d[,age:="Totalt"]
  q <- RunComparisons(d, isInfluensa = FALSE, CUTOFFALPHA)
  dCopy[,yrwk:=NULL]
  setnames(dCopy,c("seasonWk","season","malaria"))
  dCopy[,seasonWk:=as.numeric(seasonWk)]
  dCopy[,season:=as.character(season)]
  xdCopy <- merge(dCopy,q,by=c("seasonWk","season"))
  sudanExtremeYears[[i]] <- xdCopy
  sudanExtremeYears[[i]][,countyName:=i]
  sudanExtremeYears[[i]][,alpha:=2*(1-CUTOFFALPHA)]
  FormatComparisons(q,i, isInfluensa=FALSE, CUTOFFALPHA)
}
sudanExtremeYears <- rbindlist(sudanExtremeYears)
sudanSpikes <- sudanExtremeYears[variable=="Linear Reg (8 wks)"]
sudanExtremeYears <- sudanExtremeYears[variable=="MeM - All data"]
sudanExtremeYears <- sudanExtremeYears[variable=="MeM - All data",.(extreme=max(value,na.rm=T)),by=.(season,countyName,variable)]
sudanExtremeYears <- sudanExtremeYears[is.finite(extreme)]
sudanExtremeYears[,extreme:=extreme>0]
openxlsx::write.xlsx(sudanExtremeYears,file=file.path(org::PROJ$SHARED_TODAY,"sudan_extreme_years.xlsx"))

stack <- expand.grid(sudanNames,c(0.95,0.975,0.995),stringsAsFactors = FALSE)
names(stack) <- c("names","CUTOFFALPHA")
res <- vector("list",nrow(stack))
for(i in 1:length(res)){
  x <- as.data.frame(readRDS(paste0(org::PROJ$SHARED_TODAY,"/rawcorr_",stack$names[i],"_",stack$CUTOFFALPHA[i],".RDS"))[,1,drop=F])
  x$var <- row.names(x)
  x$alpha <- 2*(1-stack$CUTOFFALPHA[i])
  x$name <- stack$names[i]
  res[[i]] <- x
}

res <- rbindlist(res)
setnames(res,c("value","var","alpha","name"))
res[stringr::str_detect(var,"Random"),var:="Random"]
res[,var:=factor(var,levels=c(
  "MeM - All data",
  "Mean+SD (2006)",
  "MeM (2006)",
  "Mean+SD (2 yrs)",
  "MeM (2 yrs)",
  "Linear Reg (8 wks)","Random"
))]
sudanResults <- copy(res[!var %in% c(
  "Mean+SD (2006)",
  "MeM (2006)",
  "MeM - All data")])
openxlsx::write.xlsx(sudanResults,file=file.path(org::PROJ$SHARED_TODAY,"sudan_individual_correlations.xlsx"))

q <- ggplot(res[!var %in% c(
  "Mean+SD (2006)",
  "MeM (2006)",
  "MeM - All data")],aes(x=var,y=value,colour=factor(alpha)))
q <- q + geom_boxplot(lwd=1.25,outlier.size=5)
q <- q + scale_x_discrete("\nMethod/surveillance data scenario")
q <- q + scale_y_continuous("Spearman correlation coefficient\n",lim=c(0,1))
q <- q + labs(title="Summary over 6 health stations of spearman correlation coefficient comparing cumulative sums\nof strikes between 'MeM - All data' and other methods/surveillance data scenarios\n")
q <- q + scale_colour_brewer("Alpha",palette="Set2")
q <- q + theme_gray(20)
q <- q + theme(legend.key=element_rect(fill = "white", 
                                       colour = "white"))
saveRDS(q,file.path(org::PROJ$SHARED_TODAY,"correaltion_sudan.RDS"))
RAWmisc::saveA4(q,file.path(org::PROJ$SHARED_TODAY,"correlation_sudan.png"))

d <- readRDS(file.path(org::PROJ$RAW,"2016_03_30_cleaned_legekontakt_everyone.RDS"))

counties <- c("Oestfold",
              "Akershus",
              "Oslo",
              "Hedmark",
              "Oppland",
              "Buskerud",
              "Vestfold",
              "Telemark",
              "Aust-Agder",
              "Vest-Agder",
              "Rogaland",
              "Hordaland",
              "Sogn-og-fjordane",
              "Moere-og-Romsdal",
              "Soer-Troendelag",
              "Nord-Troendelag",
              "Nordland",
              "Troms",
              "Finnmark-Finnmarku")

for(i in counties) for(CUTOFFALPHA in c(0.95,0.975,0.995)){
  q <- RunComparisons(d[countyName==i],CUTOFFALPHA=CUTOFFALPHA)
  xlsx::saveWorkbook(q,file.path(org::PROJ$SHARED_TODAY,sprintf("RAW_NORWAY_%s_%s.xlsx",i,CUTOFFALPHA)))
  FormatComparisons(q,i,CUTOFFALPHA=CUTOFFALPHA)
}

stack <- expand.grid(counties,c(0.95,0.975,0.995),stringsAsFactors = FALSE)
names(stack) <- c("names","CUTOFFALPHA")
res <- vector("list",nrow(stack))
for(i in 1:length(res)){
  x <- as.data.frame(readRDS(paste0(org::PROJ$SHARED_TODAY,"/rawcorr_",stack$names[i],"_",stack$CUTOFFALPHA[i],".RDS"))[,1,drop=F])
  x$var <- row.names(x)
  x$alpha <- 2*(1-stack$CUTOFFALPHA[i])
  x$name <- stack$names[i]
  res[[i]] <- x
}

res <- rbindlist(res)
setnames(res,c("value","var","alpha","name"))
res[stringr::str_detect(var,"Random"),var:="Random"]
res[,var:=factor(var,levels=c(
  "MeM - All data",
  "Mean+SD (2006)",
  "MeM (2006)",
  "Mean+SD (2 yrs)",
  "MeM (2 yrs)",
  "Linear Reg (8 wks)","Random"
))]

norwayResults <- copy(res[var!="MeM - All data"])

q <- ggplot(res[var!="MeM - All data"],aes(x=var,y=value,colour=factor(alpha)))
q <- q + geom_boxplot(lwd=1.25,outlier.size=5)
q <- q + scale_x_discrete("\nMethod/surveillance data scenario")
q <- q + scale_y_continuous("Spearman correlation coefficient\n",lim=c(0,1))
q <- q + labs(title="Summary over 19 counties of spearman correlation coefficient comparing cumulative sums\nof strikes between 'MeM - All data' and other methods/surveillance data scenarios\n")
q <- q + scale_colour_brewer("Alpha",palette="Set2")
q <- q + theme_gray(20)
q <- q + theme(legend.key=element_rect(fill = "white", 
                                       colour = "white"))
saveRDS(q,file.path(org::PROJ$SHARED_TODAY,"correaltion_norway.RDS"))
RAWmisc::saveA4(q,file.path(org::PROJ$SHARED_TODAY,"correlation_norway.png"))

#### MAIN RESULTS
sudanResults[,place:="S. Sudan (Malaria)"]
norwayResults[,place:="Norway (Influenza)"]

sudanResults[,pval:=as.numeric(NA)]
sudanResults[,pvalNotDoro:=as.numeric(NA)]
sudanResults[,pvalLinear:=as.numeric(NA)]
for(i in unique(sudanResults$var)) for(a in unique(sudanResults$alpha)){
  if(i=="Random") next
  p <- t.test(sudanResults[var%in%c(i) & abs(alpha-a)<0.01]$value,sudanResults[var%in%c("Random")]$value)$p.value
  #data <- sudanResults[(var==i & abs(alpha-a)<0.01) | var=="Random"]
  #p <- kruskal.test(data$value,data$var)$p.value
  sudanResults[var%in%c(i) & abs(alpha-a)<0.01,pval:=p]
}
for(i in unique(sudanResults$var)) for(a in unique(sudanResults$alpha)){
  if(i=="Random") next
  p <- t.test(sudanResults[name!="Doro" & var%in%c(i) & abs(alpha-a)<0.01]$value,sudanResults[name!="Doro" & var%in%c("Random")]$value)$p.value
  #data <- sudanResults[(var==i & abs(alpha-a)<0.01) | var=="Random"]
  #p <- kruskal.test(data$value,data$var)$p.value
  sudanResults[var%in%c(i) & abs(alpha-a)<0.01,pvalNotDoro:=p]
}
norwayResults[,pval:=as.numeric(NA)]
norwayResults[,pvalNotDoro:=as.numeric(NA)]
for(i in unique(norwayResults$var)) for(a in unique(norwayResults$alpha)){
  if(i=="Random") next
  p <- t.test(norwayResults[var%in%c(i) & abs(alpha-a)<0.01]$value,norwayResults[var%in%c("Random")]$value)$p.value
  norwayResults[var%in%c(i) & abs(alpha-a)<0.01,pval:=p]
}
norwayResults[,pvalLinear:=as.numeric(NA)]
for(i in unique(norwayResults$var)) for(a in unique(norwayResults$alpha)){
  if(i %in% c("Random","Linear Reg (8 wks)")) next
  p <- t.test(norwayResults[var%in%c(i) & abs(alpha-a)<0.01]$value,norwayResults[var%in%c("Linear Reg (8 wks)") & abs(alpha-a)<0.01]$value)$p.value
  norwayResults[var%in%c(i) & abs(alpha-a)<0.01,pvalLinear:=p]
}

res <- rbind(sudanResults,norwayResults)
res[,label:=paste0(place,", alpha=",alpha)]
res[var %in% c("MeM (2006)","MeM (2 yrs)","Random"),label:=paste0(place,"")]
res <- res[!(var %in% c("MeM (2006)","MeM (2 yrs)",c("Random")) & alpha <0.09)]
labs <- copy(res)
labs[,value:=NULL]
labs[,name:=NULL]
labs <- unique(labs)
labs[,pval:=format(round(pval,2),nsmall=2)]
labs[pval=="  NA",pval:=""]
labs[,pvalNotDoro:=format(round(pvalNotDoro,2),nsmall=2)]
labs[pvalNotDoro=="  NA",pvalNotDoro:=""]
labs[place=="S. Sudan (Malaria)",pval:=paste0(pval,"/",pvalNotDoro)]
labs[pval=="/",pval:=""]
labs[,pvalLinear:=format(round(pvalLinear,2),nsmall=2)]
labs[pvalLinear=="  NA",pvalLinear:=""]
print(labs)


q <- ggplot(res,aes(x=var,y=value,colour=label))
q <- q + geom_boxplot(lwd=1.25,outlier.size=5,position = position_dodge(width = 0.9))
q <- q + geom_text(data=labs,mapping=aes(label=pval,x=var,y=1.0),position = position_dodge(width = 0.9), angle=90,hjust=0,vjust=0.5,size=7)
q <- q + scale_x_discrete("\nMethod/surveillance data scenario")
q <- q + scale_y_continuous("Spearman correlation coefficient\n",lim=c(0,1.175),breaks=seq(0,1,0.2),labels=seq(0,1,0.2))
q <- q + labs(title="19 Norwegian counties and 5 South Sudanese health stations\nSummary of spearman correlation coefficient comparing cumulative sums\nof strikes between 'MeM - All data' and other methods/surveillance data scenarios\n")
q <- q + scale_colour_brewer("",palette="Set2")
q <- q + theme_gray(20)
q <- q + theme(legend.key=element_rect(fill = "white", 
                                       colour = "white"))
q <- q + theme(legend.position="bottom")
q <- q + guides(colour = guide_legend(direction = "horizontal", byrow=TRUE))
saveRDS(q,file.path(org::PROJ$SHARED_TODAY,"correlation.RDS"))
RAWmisc::saveA4(q,paste0("results_final/correlation.png"))


labs[,label2:=gsub("Norway (Influenza), ","", labs$label,fixed=T)]
res[,label2:=gsub("Norway (Influenza), ","", res$label,fixed=T)]
q <- ggplot(res[place=="Norway (Influenza)"],aes(x=var,y=value,colour=label2))
q <- q + geom_boxplot(lwd=1.25,outlier.size=5,position = position_dodge(width = 0.9))
q <- q + geom_text(data=labs[place=="Norway (Influenza)"],mapping=aes(label=pval,x=var,y=1.0),position = position_dodge(width = 0.9), angle=90,hjust=0,vjust=0.5,size=7)
q <- q + geom_text(data=labs[place=="Norway (Influenza)"],mapping=aes(label=pvalLinear,x=var,y=0.1),position = position_dodge(width = 0.9), angle=90,hjust=0,vjust=0.5,size=7)
q <- q + scale_x_discrete("\nMethod/surveillance data scenario")
q <- q + scale_y_continuous("Spearman correlation coefficient\n",lim=c(0,1.175),breaks=seq(0,1,0.2),labels=seq(0,1,0.2))
q <- q + labs(title="Summary of spearman correlation coefficient comparing cumulative sums of strikes\nbetween 'MeM - All data' and other methods/surveillance data scenarios in 19 Norwegian counties\n")
q <- q + scale_colour_brewer("",palette="Set2")
q <- q + theme_gray(20)
q <- q + theme(legend.key=element_rect(fill = "white", 
                                       colour = "white"))
q <- q + theme(legend.position="bottom")
q <- q + guides(colour = guide_legend(direction = "horizontal", byrow=TRUE))
RAWmisc::saveA4(q,file.path(org::PROJ$SHARED_TODAY,"correlation_norway_only.png"))

sudanSpikes[,yrwk:=paste0(season,"/",seasonWk)]
points <- expand.grid(1:52,2010:2015)
points <- paste0(points$Var2,"/",points$Var1)
sudanSpikes[,yrwk:=factor(yrwk,levels=points)]
sudanSpikes[,x:=as.numeric(yrwk)]
breaks <- unique(sudanSpikes[seasonWk==1,c("season","x"),with=F])
x <- breaks[6]
x$season <- 2016
x$x <- x$x+52
breaks <- rbind(breaks,x)
breaks$yrwk <- paste0("1/",breaks$season)

q <- ggplot(sudanSpikes,aes(x=x,y=malaria))
q <- q + geom_line(lwd=0.8)
q <- q + geom_vline(data=sudanSpikes[value>0],mapping=aes(xintercept=x),colour="red")
q <- q + scale_x_continuous("",breaks=breaks$x,labels=breaks$yrwk)
q <- q + scale_y_continuous("Weekly registered cases of malaria\n")
q <- q + labs(title="Weekly registered cases of malaria in 5 South Sudanese health stations\n")
q <- q + facet_wrap(~countyName,scales="free")

q <- q + theme_gray(20)
q <- q + theme(legend.key=element_rect(fill = "white", 
                                       colour = "white"))
q <- q + theme(legend.position="bottom")
q <- q + guides(colour = guide_legend(direction = "horizontal", byrow=TRUE))
q <- q + theme(axis.text.x = element_text(angle = 90,vjust=0.5))
saveRDS(q,file.path(org::PROJ$SHARED_TODAY,"correlation.RDS"))
RAWmisc::saveA4(q,file.path(org::PROJ$SHARED_TODAY,"sudan_weekly_cases.png"))


toSave <- norwayResults[,.(
  median=median(value),
  p25=quantile(value,probs=0.25),
  p75=quantile(value,probs=0.75),
  min=min(value),
  max=max(value)),
  by=.(var,alpha)]
openxlsx::write.xlsx(toSave,file=file.path(org::PROJ$SHARED_TODAY,"norway_correlations.xlsx"))

# Text
#RAWmisc::RmdToDOCX("reports_skeleton/report_new.Rmd",paste0("reports_formatted/",format(Sys.time(), "%Y_%m_%d"),"_text.docx"), copyFrom="reports_skeleton")
#RAWmisc::RmdToDOCX("reports_skeleton/report.Rmd",paste0("reports_formatted/",format(Sys.time(), "%Y_%m_%d"),"_text.docx"), copyFrom="reports_skeleton")

# Tables
#RAWmisc::RmdToHTMLDOCX("reports_skeleton/tables.Rmd",paste0("reports_formatted/",format(Sys.time(), "%Y_%m_%d"),"_tables.html"), copyFrom="reports_skeleton")
