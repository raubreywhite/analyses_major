suppressMessages(library(ggplot2))
RAWmisc::InitialiseProject(
  HOME = "/analyses/code_major/klima_analyses/",
  RAW = "/dropbox/data_raw/klima_analyses/",
  CLEAN = "/analyses/data_clean/klima_analyses",
  BAKED = "/analyses/results_baked/klima_analyses/",
  FINAL = "/analyses/results_final/klima_analyses/",
  SHARED = "/dropbox/results_shared/klima_analyses/")

dir.create(file.path(RAWmisc::PROJ$SHARED_TODAY,"/WP2"))
if(FALSE){
  RAWmisc::RmdToDOCX(
    inFile = "RunWP2.Rmd",outFile = paste0("reports_formatted/WP2_",gsub("-","_",lubridate::today()),".docx"))
}

suppressMessages(library(data.table))
suppressMessages(library(ggplot2))
suppressMessages(library(foreach))
suppressMessages(library(pomp))
suppressMessages(library(doRedis))

assign("RUN_ALL", FALSE, envir=globalenv())
# Your code starts here

if(RUN_ALL) unlink(file.path(RAWmisc::PROJ$CLEAN,"data.RDS"))
bake(file.path(RAWmisc::PROJ$CLEAN,"data.RDS"),{
  CleanData()
}) -> d

if(RUN_ALL) unlink(file.path(RAWmisc::PROJ$BAKED,"WP2_res.RDS"))
bake(file.path(RAWmisc::PROJ$BAKED,"WP2_res.RDS"),{
  WP2Analyses(d,ExtractValues=ExtractValues)
}) -> all


######### RESULTS

all[,varOfInterest:=gsub("^cat_","",varOfInterest)]
all[var=="",var:="Cont"]
all[,varOfInterest:=stringr::str_replace(varOfInterest,"[0-9]_[0-9]$","")]

plotData <- copy(all)

plotData[,dec:="None"]
plotData[pval < 0.05 & est<0,dec:="Protective"]
plotData[pval < 0.05 & est > 0, dec := "Harmful"]

plotData[, N := .N, by=water]
plotData[, lag:=factor(lag)]
plotData[, cutoff := stringr::str_sub(varOfInterest, 1, 5)]
plotData[, varOfInterest := gsub("^wp950_", "", varOfInterest)]
plotData[, varOfInterest := gsub("^wp990_", "", varOfInterest)]
plotData[, dec := factor(dec, levels=c("Harmful","None","Protective"))]
plotData[, season := factor(season, levels=c("All","Autumn","Winter","Spring","Summer"))]
levels(plotData$season) <- c("All seasons","Autumn","Winter","Spring","Summer")
plotData[, varOfInterest := factor(varOfInterest,levels=c("c_rain","a_precipCorr","a_runoff"))]
levels(plotData$varOfInterest) <- c("Rain\n(Municip. Centre)","Corrected precipication\n(Municip. Average)","Runoff\n(Municip. Average)")
plotData[,water:=factor(water,levels=c("All","Under 10k","10k+","Under 500","50%+"))]
plotData[,age:=factor(age,levels=c("Totalt","0-4","5-14","15-64","65+"))]
levels(plotData$age) <- c("All ages","0-4 years old","5-14 years old","15-64 years old","65+ years old")

data <- vector("list", 5)
plots <- vector("list", 5)

data[[1]] <- plotData[water=="All"]
data[[2]] <- plotData[water=="Under 10k"]
data[[3]] <- plotData[water=="10k+"]
data[[4]] <- plotData[water=="Under 500"]
data[[5]] <- plotData[water=="50%+"]
pdf(file.path(RAWmisc::PROJ$SHARED_TODAY,"WP2",paste0("WP2_continuous.pdf")),width=12,height=6.5)
for (i in 1:5) {
  data[[i]][pval * N > 0.05, dec:="None"]
  q <- ggplot(data[[i]], aes(x = lag, y = varOfInterest, fill = dec))
  q <- q + geom_tile(data = data[[1]],alpha=0)
  q <- q + geom_tile(alpha=0.6,colour="white",lwd=0.2)
  q <- q + facet_grid(age ~ season)
  q <- q + scale_fill_manual(values=c("Red","Black","Green"),drop=F)
  q <- q + scale_x_discrete("Weeks lag")
  q <- q + scale_y_discrete("")
  q <- q + RAWmisc::theme_SMAO(base_size=12,v=3)
  if(i==1){
    q <- q + labs(title="All municipalities")
  } else if(i==2){
    q <- q + labs(title="Municipalities with average waterwork size under 10k")
  } else if(i==3){
    q <- q + labs(title="Municipalities with average waterwork size 10k+")
  } else if(i==4){
    q <- q + labs(title="Municipalities with average waterwork size under 500")
  } else if(i==5){
    q <- q + labs(title="Municipalities with 50%+ not having waterworks")
  }
  print(q)
}
dev.off()



#RAWmisc::RmdToHTMLDOCX("reports_skeleton/report.Rmd",paste0("reports_formatted/HTMLReport_",format(Sys.time(), "%Y_%m_%d"),".html"), copyFrom="reports_skeleton")

