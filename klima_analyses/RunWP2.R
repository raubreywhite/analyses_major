
RAWmisc::RmdToDOCX(
  inFile = "RunWP2.Rmd",outFile = paste0("reports_formatted/WP2_",gsub("-","_",lubridate::today()),".docx"))



RmdToDOCX <- function (inFile = "", outFile = "", tocDepth = 2, copyFrom = NULL) 
{
  if (!is.null(copyFrom)) {
    if (!stringr::str_detect(inFile, paste0("^", copyFrom, 
                                            "/"))) {
      stop(paste0("inFile does not start with ", copyFrom, 
                  "/ and you are using copyFrom=", copyFrom))
    }
    file.copy(inFile, gsub(paste0("^", copyFrom, "/"), "", 
                           inFile), overwrite = TRUE)
    inFile <- gsub(paste0("^", copyFrom, "/"), "", inFile)
  }
  try({
    outDir <- tempdir()
    originalOutFile <- outFile
    
    outFile <- unlist(stringr::str_split(outFile, "/"))
    if (length(outFile) == 1) {
      #outDir <- getwd()
    }
    else {
      #outDir <- file.path(getwd(), outFile[-length(outFile)])
      outFile <- outFile[length(outFile)]
    }
    
    rmarkdown::render(input = inFile, output_file = outFile, 
                      output_dir = outDir, output_format = rmarkdown::word_document(toc = TRUE, 
                                                                                    toc_depth = tocDepth))
  
    cmd <- paste0("rm -f ",file.path(getwd(),originalOutFile))
    system(cmd)
    print(cmd)
    cmd <- paste0("cp -f ",file.path(outDir,outFile)," ",file.path(getwd(),originalOutFile))
    system(cmd)
    print(cmd)
  }, TRUE)
  if (!is.null(copyFrom)) {
    file.remove(inFile)
  }
}




suppressWarnings(tryCatch(
  msgTrap <- capture.output(suppressMessages(source("RHeader.R", echo=F))),
  error=function(err) {msgTrap <- capture.output(suppressMessages(source("/src/RHeader.R", echo=F)))}
))

suppressMessages(library(data.table))
suppressMessages(library(ggplot2))
suppressMessages(library(foreach))
suppressMessages(library(pomp))
suppressMessages(library(doRedis))


assign("RUN_ALL", FALSE, envir=globalenv())

fileSources = file.path("code",list.files("code",pattern="*.[rR]$"))
sapply(fileSources,source,.GlobalEnv)

# Your code starts here

if(RUN_ALL) unlink("data_clean/data.RDS")
bake("data_clean/data.RDS",{
  CleanData()
}) -> d

if(RUN_ALL) unlink("results_baked/WP2_res.RDS")
bake("results_baked/WP2_res.RDS",{
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
levels(plotData$season) <- c("All seasons\n","Autumn\n","Winter\n","Spring\n","Summer\n")
plotData[, varOfInterest := factor(varOfInterest,levels=c("c_rain","a_precipCorr","a_runoff"))]
levels(plotData$varOfInterest) <- c("Rain\n(Municip. Centre)","Corrected precipication\n(Municip. Average)","Runoff\n(Municip. Average)")
plotData[,water:=factor(water,levels=c("All","Under 10k","10k+","Under 500","50%+"))]
plotData[,age:=factor(age,levels=c("Totalt","0-4","5-14","15-64","65+"))]
levels(plotData$age) <- c("All ages\n","0-4 years old\n","5-14 years old\n","15-64 years old\n","65+ years old\n")

data <- vector("list", 5)
plots <- vector("list", 5)

data[[1]] <- plotData[water=="All"]
data[[2]] <- plotData[water=="Under 10k"]
data[[3]] <- plotData[water=="10k+"]
data[[4]] <- plotData[water=="Under 500"]
data[[5]] <- plotData[water=="50%+"]
pdf(file.path("results_final","WP2",paste0("WP2_continuous.pdf")),width=12,height=6.5)
for (i in 1:5) {
  
  q <- ggplot(data[[i]], aes(x = lag, y = varOfInterest, fill = dec))
  q <- q + geom_tile(data = data[[1]],alpha=0)
  q <- q + geom_tile(alpha=0.6,colour="white",lwd=0.2)
  q <- q + geom_text(data = data[[i]][pval * N < 0.05 & dec != "None"], label = "+", size = 14)
  q <- q + geom_text(data = data[[i]][pval * N < 0.05 & dec != "None"], label = "+", size = 8, colour="white")
  q <- q + facet_grid(age ~ season)
  q <- q + scale_fill_manual(values=c("Red","Black","Green"),drop=F)
  q <- q + scale_x_discrete("Weeks lag")
  q <- q + scale_y_discrete("")
  q <- q + RAWmisc::theme_SMAO(10)
  if(i==1){
    q <- q + labs(title="All municipalities\n")
  } else if(i==2){
    q <- q + labs(title="Municipalities with average waterwork size under 10k\n")
  } else if(i==3){
    q <- q + labs(title="Municipalities with average waterwork size 10k+\n")
  } else if(i==4){
    q <- q + labs(title="Municipalities with average waterwork size under 500\n")
  } else if(i==5){
    q <- q + labs(title="Municipalities with 50%+ not having waterworks\n")
  }
  q <- q + theme(axis.line.y = NULL)
  q <- q + theme(axis.line.x = NULL)
  q <- q + theme(axis.ticks.length = unit(0,"lines"))
  q <- q + theme(axis.text.x = element_text(vjust=0.5))
  q <- q + theme(axis.text.y = element_text(hjust=1))
  q <- q + theme(axis.text = element_text(margin = rep(unit(1,"lines"),4)))
  print(q)
}
dev.off()



#RAWmisc::RmdToHTMLDOCX("reports_skeleton/report.Rmd",paste0("reports_formatted/HTMLReport_",format(Sys.time(), "%Y_%m_%d"),".html"), copyFrom="reports_skeleton")

