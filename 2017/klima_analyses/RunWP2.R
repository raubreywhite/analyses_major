suppressMessages(library(ggplot2))
org::AllowFileManipulationFromInitialiseProject()
org::InitialiseProject(
  HOME = "/git/code_major/2017/klima_analyses/",
  RAW = "/Volumes/crypt_data/org/data_raw/code_major/2017/klima_analyses/",
  CLEAN = "/tmp/data_clean/code_major/2017/klima_analyses",
  BAKED = "/tmp/results_baked/code_major/2017/klima_analyses/",
  FINAL = "/tmp/results_final/code_major/2017/klima_analyses/",
  SHARED = "/dropbox/analyses/results_shared/code_major/2017/klima_analyses/"
)

suppressMessages(library(data.table))
suppressMessages(library(ggplot2))
suppressMessages(library(foreach))
suppressMessages(library(pomp))

assign("RUN_ALL", TRUE, envir=globalenv())

# exposure = "number of days >95th percentile in the last 4 weeks

d <- WP2Data()

stack <- data.table(expand.grid(c("Whole year",unique(d$season)),
                                rev(unique(d$age)),
                                c("wp950_a_runoff0_3","wp950_a_rain0_3","wp950_c_temperature0_3"),
                                stringsAsFactors = FALSE))
setnames(stack,c("season","age","exposure"))
stack <- stack[!(exposure=="wp950_c_temperature0_3" & season!="Whole year")]

res <- vector("list",length=nrow(stack))
pb <- RAWmisc::ProgressBarCreate(min=0,max=length(res))
for(i in 1:length(res)){
  RAWmisc::ProgressBarSet(pb,i)
  
  stackIter <- stack[i]
  totalData <- d[age!="Totalt"]
  
  fitData <- d[age==stackIter$age]
  if(stackIter$season!="Whole year") fitData <- fitData[season==stackIter$season]
  
  formula <- sprintf("s_outbreakAll~%s+ factor(month) + (1|municip)",stackIter$exposure)
  fit <- lme4::lmer(as.formula(formula),data=fitData)
  
  x <- data.frame(var="",est=NA,se=NA,pval=NA)
  try({
    x <- ExtractValues(fit,stackIter$exposure,removeLead=T,format=F)
  },TRUE)
  
  x$var <- stackIter$exposure
  x$age <- stackIter$age
  x$season <- stackIter$season
  x$outcomeMean <- mean(fitData$s_outbreakAll,na.rm=T)
  x$exposureMean <- mean(fitData[[stackIter$exposure]],na.rm=T)
  x$exposureSD <- sd(fitData[[stackIter$exposure]],na.rm=T)
  x$interactionPvalAge <- NA
  x$interactionPvalSeason <- NA
  
  if(stackIter$age=="Totalt" & stackIter$season=="Whole year"){
    formula1 <- sprintf("s_gastro~%s + season + age + (1|municip)",stackIter$exposure)
    fit1 <- lme4::lmer(as.formula(formula1),data=totalData)
    
    formula0 <- sprintf("s_gastro~%s*age + season + (1|municip)",stackIter$exposure)
    fit0 <- lme4::lmer(as.formula(formula0),data=totalData)
    x$interactionPvalAge <- lmtest::lrtest(fit0,fit1)$`Pr(>Chisq)`[2]
    
    formula0 <- sprintf("s_gastro~%s*season + age + (1|municip)",stackIter$exposure)
    fit0 <- lme4::lmer(as.formula(formula0),data=totalData)
    x$interactionPvalSeason <- lmtest::lrtest(fit0,fit1)$`Pr(>Chisq)`[2]
  }
  res[[i]] <- x
}

res <- rbindlist(res)


res[,effect:=sprintf("%spp (%spp, %spp)",
                     RAWmisc::Format(est*100,2),
                     RAWmisc::Format((est-1.96*se)*100,2),
                     RAWmisc::Format((est+1.96*se)*100,2))]

res[,outcomeSummary:=sprintf("%s%%",
                             RAWmisc::Format(outcomeMean*100)
)]

res[,exposureSummary:=sprintf("%s (%s)",
                              RAWmisc::Format(exposureMean),
                              RAWmisc::Format(exposureSD))]

res[, pvalBonf:=pval*.N]
res[,sig:=""]
res[pvalBonf<0.05,sig:="*"]
res[pvalBonf>1,pvalBonf:=1]
toPlot <- copy(res)
res[,pvalBonf:=RAWmisc::Format(pvalBonf,3)]
res[pvalBonf=="0.000",pvalBonf:="<0.001"]

res[, pval:=RAWmisc::Format(pval,3)]                               
res[pval=="0.000",pval:="<0.001"]

res[,pvalBonf:=paste0(pvalBonf,sig)]

res[,interactionPvalSeason:=RAWmisc::Format(interactionPvalSeason,3)]
res[,interactionPvalAge:=RAWmisc::Format(interactionPvalAge,3)]
res[interactionPvalSeason=="0.000",interactionPvalSeason:="<0.001"]
res[interactionPvalAge=="0.000",interactionPvalAge:="<0.001"]

res <- res[,c("var","age","season","outcomeSummary","exposureSummary","effect","pval","pvalBonf","interactionPvalAge","interactionPvalSeason")]
res

openxlsx::write.xlsx(res,file = file.path(
  org::PROJ$SHARED_TODAY,"WP_20.xlsx"
))


toPlot[,category:="Not significant"]
toPlot[pvalBonf<0.05 & est>0,category:="Increases"]
toPlot[pvalBonf<0.05 & est<0,category:="Decreases"]
toPlot[,category:=factor(category,levels=c("Decreases","Not significant","Increases"))]

RAWmisc::RecodeDT(toPlot,switch=c(
  "wp950_c_rain0_3"="Rain",
  "wp950_a_runoff0_3"="Runoff",
  "wp950_c_temperature0_3"="Temperature"
),"var")

toPlot[,season:=factor(season,levels=rev(c("Whole year","Winter","Spring","Summer","Autumn")))]
toPlot[,age:=factor(age,levels=c("Totalt","0-4","5-14","15-64","65+"))]
setattr(toPlot$age,"levels",c("Total","0-4","5-14","15-64","65+"))
#setorder(toPlot,outcome,season,age)

q <- ggplot(toPlot,aes(x=age,y=season,fill=category))
q <- q + geom_tile(alpha=0.7,colour="black")
q <- q + facet_grid(.~var,scales="free")
q <- q + scale_fill_brewer("",drop=F,palette="Set1")
q <- q + scale_x_discrete("")
q <- q + scale_y_discrete("")
q <- q + guides(fill = guide_legend(reverse=T))
q <- q + theme_gray(12)
q <- q + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))
q <- q + theme(legend.position="bottom")
q <- q + labs(caption="Exposure on panel titles, stratified analyses on x-axis and y-axis")
ggsave(file.path(org::PROJ$SHARED_TODAY,"WP2.png"), plot = q, width = 210, height = 297/2, 
       units = "mm")







