
setwd("F:/Felles/Protocol on Water and Health/Intervensjonsstudie/SMSstudie/SMSPowerCalc")

# Change if you want local setup to be pulled from github
upgradeRLocalSetup <- FALSE
source("RLocalSetup.R")

# Do a 'major' commit to Git
# CommitToGit("This is a big commit")

LoadPackage("SMSPowerCalc")
library(data.table)

r <- git2r::repository()
git2r::summary(r)
git2r::contributions(r,by="author")


# water exposures
data <- data.table(x=SampleWaterExposure(10000))
meanWater <- mean(data$x)
data <- data[,.(y=.N),by=.(x)]
data[,p:=y/sum(y)]

q <- ggplot(data,aes(x=factor(x),y=p))
q <- q + geom_bar(stat="identity")
q <- q + geom_label(aes(y=p+0.01,label=format(round(p,2),nsmall=2)),size=10,label.r=unit(0, "lines"))
q <- q + scale_x_discrete("Number of glasses of water per day")
q <- q + scale_y_continuous("Proportion\n")
q <- SMAOgraphs::SMAOFormatGGPlot(q)
SMAOgraphs::SMAOpng("results_final/water.png")
print(q)
dev.off()

## number people sick
d1025 <- copy(data)
d1025[,risk:=0.0565*(1.025^x)]
d1025[,minn:=0.0565*(1.025^meanWater)]
d1025[,risk:=risk-minn]
d1025[risk<0,risk:=0]
d1025[,minn:=round(100000*minn*p)]
d1025[,risk:=round(100000*risk)]
d1025 <- melt(d1025[,c("x","risk","minn"),with=F],id="x")
d1025 <- d1025[,.(value=sum(value)),by=variable]
d1025[,risk:=1.025]

d105 <- copy(data)
d105[,risk:=0.0461*(1.05^x)]
d105[,minn:=0.0461*(1.05^meanWater)]
d105[,risk:=risk-minn]
d105[risk<0,risk:=0]
d105[,minn:=round(100000*minn*p)]
d105[,risk:=round(100000*risk)]
d105 <- melt(d105[,c("x","risk","minn"),with=F],id="x")
d105 <- d105[,.(value=sum(value)),by=variable]
d105[,risk:=1.05]

plot <- rbind(d1025,d105)
plot[,variable:=factor(variable,levels=c("risk","minn"))]
levels(plot$variable) <- c("Vann","Øverige")
setorder(plot,-variable)
q <- ggplot(plot,aes(x=factor(risk),y=value,fill=variable))
q <- q + geom_bar(stat="identity")
q <- q + geom_text(data=plot[variable=="Vann"],mapping=aes(label=value),y=6500)
q <- q + labs(title="AGI tilfeller per 100.000/måned pga vann over gjennomsnitt med forskjelle odds ratio per ekstra glass vann")
q <- q + scale_x_discrete("Odds ratio risiko per ekstra glass vann")
q <- q + scale_y_continuous("")
q

data[,risk105:=round(p*1000*0.07*(1.05^(x-3)))]
data[,min105:=min(risk105)]
data[,risk105:=risk105-min105]

# equivalence tests
sampleSizes <- c(3300,8600,750,1950,200,520)
saveRDS(sampleSizes[2], "results_final/n_large_sample.RDS")
saveRDS(sampleSizes[4], "results_final/n_small_sample.RDS")

res <- list()

size <- 100
f <- data.frame(lower=rep(NA,size),upper=rep(NA,size))


pb <- txtProgressBar(min=1,max=nrow(f),style=3)
for(i in 1:nrow(f)){
  f[i,] <- RunSingleRisk90CI(npeople=sampleSizes[2],int=-3.15,stddev=1.15,oddsRatio=1.0)
  setTxtProgressBar(pb,i)
}
close(pb)

x <- data.table(f)
x[,row:=1:.N]
x[,limit:=max(abs(lower),abs(upper)),by=row]
res <- exp(quantile(x$limit,probs=0.8)[[1]])
saveRDS(res, "results_final/equivalence_large_sample.RDS")

pb <- txtProgressBar(min=1,max=nrow(f),style=3)
for(i in 1:nrow(f)){
  f[i,] <- RunSingleRisk90CI(npeople=sampleSizes[4],int=-3.15,stddev=1.15,oddsRatio=1.0)
  setTxtProgressBar(pb,i)
}
close(pb)

x <- data.table(f)
x[,row:=1:.N]
x[,limit:=max(abs(lower),abs(upper)),by=row]
res <- exp(quantile(x$limit,probs=0.8)[[1]]) #2.25
saveRDS(res, "results_final/equivalence_small_sample.RDS")

# recreating campy study


res <- list()

size <- 1000
f <- data.frame(oneEst=rep(NA,size),onePval=rep(NA,size),
                prev=rep(NA,size))


# OR 1.025
pb <- txtProgressBar(min=1,max=nrow(f),style=3)
for(i in 1:nrow(f)){
  f[i,] <- RecreateCampy(npeople=1531,int=-2.75,stddev=0,oddsRatio=1.025, nReps = 1)
  setTxtProgressBar(pb,i)
}
close(pb)

xLower <- data.table(f)
f1 <- copy(f)
xLower <- xLower[,.(campy=mean(exp(oneEst)<1.01092),power=mean(onePval<0.05))]

# OR 1.05
pb <- txtProgressBar(min=1,max=nrow(f),style=3)
for(i in 1:nrow(f)){
  f[i,] <- RecreateCampy(npeople=1531,int=-2.75,stddev=0,oddsRatio=1.05, nReps = 1)
  setTxtProgressBar(pb,i)
}
close(pb)

xHigher <- data.table(f)
f2 <- copy(f)
xHigher <- xHigher[,.(campy=mean(exp(oneEst)<1.01),power=mean(onePval<0.05))]

dif1 <- abs(f1$oneEst-log(1.01092))
dif2 <- abs(f2$oneEst-log(1.01092))

campy <- c(xLower$campy, xHigher$campy, mean(dif1<dif2))
saveRDS(campy, "results_final/campy_results.RDS")

# simple tests
res <- list()

sampleSizes <- c(3300,8600,750,1950,200,520)

size <- 100
f <- data.frame(mixedEst=rep(NA,size),mixedPval=rep(NA,size),
                oneEst=rep(NA,size),onePval=rep(NA,size),
                badEst=rep(NA,size),badPval=rep(NA,size),
                icc=rep(NA,size),deff=rep(NA,size),prev=rep(NA,size))


# OR 1.025
pb <- txtProgressBar(min=1,max=nrow(f),style=3)
for(i in 1:nrow(f)){
  f[i,] <- RunSingleRisk(npeople=sampleSizes[1],int=-2.75,stddev=0,oddsRatio=1.025)
  setTxtProgressBar(pb,i)
}
close(pb)

xLower <- data.table(f)
(res[[length(res)+1]] <- xLower[,.(mixedOR=exp(mean(mixedEst)),mixedPower=mean(ifelse(mixedPval<0.05,1,0)),
                                   oneOR=exp(mean(oneEst)),onePower=mean(ifelse(onePval<0.05,1,0)),
                                   badOR=exp(mean(badEst)),badPower=mean(ifelse(badPval<0.05,1,0)),
                                   icc=mean(icc),deff=mean(deff),prev=mean(prev)
)])

pb <- txtProgressBar(min=1,max=nrow(f),style=3)
for(i in 1:nrow(f)){
  f[i,] <- RunSingleRisk(npeople=sampleSizes[2],int=-3.15,stddev=1.15,oddsRatio=1.025)
  setTxtProgressBar(pb,i)
}
close(pb)

xHigher <- data.table(f)
(res[[length(res)+1]] <- xHigher[,.(mixedOR=exp(mean(mixedEst)),mixedPower=mean(ifelse(mixedPval<0.05,1,0)),
                                    oneOR=exp(mean(oneEst)),onePower=mean(ifelse(onePval<0.05,1,0)),
                                    badOR=exp(mean(badEst)),badPower=mean(ifelse(badPval<0.05,1,0)),
                                    icc=mean(icc),deff=mean(deff),prev=mean(prev)
)])


# OR 1.05
pb <- txtProgressBar(min=1,max=nrow(f),style=3)
for(i in 1:nrow(f)){
  f[i,] <- RunSingleRisk(npeople=sampleSizes[3],int=-2.75,stddev=0,oddsRatio=1.05)
  setTxtProgressBar(pb,i)
}
close(pb)

xLower <- data.table(f)
(res[[length(res)+1]] <- xLower[,.(mixedOR=exp(mean(mixedEst)),mixedPower=mean(ifelse(mixedPval<0.05,1,0)),
          oneOR=exp(mean(oneEst)),onePower=mean(ifelse(onePval<0.05,1,0)),
          badOR=exp(mean(badEst)),badPower=mean(ifelse(badPval<0.05,1,0)),
          icc=mean(icc),deff=mean(deff),prev=mean(prev)
)])

pb <- txtProgressBar(min=1,max=nrow(f),style=3)
for(i in 1:nrow(f)){
  f[i,] <- RunSingleRisk(npeople=sampleSizes[4],int=-3.15,stddev=1.15,oddsRatio=1.05)
  setTxtProgressBar(pb,i)
}
close(pb)

xHigher <- data.table(f)
(res[[length(res)+1]] <- xHigher[,.(mixedOR=exp(mean(mixedEst)),mixedPower=mean(ifelse(mixedPval<0.05,1,0)),
           oneOR=exp(mean(oneEst)),onePower=mean(ifelse(onePval<0.05,1,0)),
           badOR=exp(mean(badEst)),badPower=mean(ifelse(badPval<0.05,1,0)),
           icc=mean(icc),deff=mean(deff),prev=mean(prev)
)])

# OR 1.10
pb <- txtProgressBar(min=1,max=nrow(f),style=3)
for(i in 1:nrow(f)){
  f[i,] <- RunSingleRisk(npeople=sampleSizes[5],int=-2.85,stddev=0,oddsRatio=1.1)
  setTxtProgressBar(pb,i)
}
close(pb)

xLower <- data.table(f)
(res[[length(res)+1]] <- xLower[,.(mixedOR=exp(mean(mixedEst)),mixedPower=mean(ifelse(mixedPval<0.05,1,0)),
                                   oneOR=exp(mean(oneEst)),onePower=mean(ifelse(onePval<0.05,1,0)),
                                   badOR=exp(mean(badEst)),badPower=mean(ifelse(badPval<0.05,1,0)),
                                   icc=mean(icc),deff=mean(deff),prev=mean(prev)
)])

pb <- txtProgressBar(min=1,max=nrow(f),style=3)
for(i in 1:nrow(f)){
  f[i,] <- RunSingleRisk(npeople=sampleSizes[6],int=-3.35,stddev=1.15,oddsRatio=1.1)
  setTxtProgressBar(pb,i)
}
close(pb)

xHigher <- data.table(f)
(res[[length(res)+1]] <- xHigher[,.(mixedOR=exp(mean(mixedEst)),mixedPower=mean(ifelse(mixedPval<0.05,1,0)),
                                    oneOR=exp(mean(oneEst)),onePower=mean(ifelse(onePval<0.05,1,0)),
                                    badOR=exp(mean(badEst)),badPower=mean(ifelse(badPval<0.05,1,0)),
                                    icc=mean(icc),deff=mean(deff),prev=mean(prev)
)])

tableRes <- rbindlist(res)
tableRes[,n:=sampleSizes]
tableRes[,x:=round(icc,1)]
setorder(tableRes,x,mixedOR)
tableRes[,x:=NULL]
setcolorder(tableRes,c("n","mixedOR","mixedPower","badOR","badPower","oneOR","onePower","icc","deff","prev"))


tableRes[,mixedOR:=format(round(mixedOR,3),nsmall=3)]
tableRes[,mixedPower:= round(mixedPower*100)]

tableRes[,oneOR:=format(round(oneOR,3),nsmall=3)]
tableRes[,onePower:= round(onePower*100)]

tableRes[,badOR:=format(round(badOR,3),nsmall=3)]
tableRes[,badPower:= round(badPower*100)]

tableRes[,icc:=format(round(icc,2),nsmall=2)]
tableRes[,deff:= format(round(deff,2),nsmall=2)]
tableRes[,prev:= paste0(format(round(prev*100,1),nsmall=1),"%")]



tabSingle <- htmlTable::htmlTable(tableRes,
                     rnames=FALSE,
                     header=c("N","OR","Power","OR","Power","OR","Power","ICC","DEFF","Prevalence"),
                     cgroup=rbind(c("","Longitudinal analysis","Cross sectional","",NA),c("","Correct&dagger;","Incorrect&Dagger;","One time point","Summary")),
                     n.cgroup=rbind(c(1,4,2,3,NA),c(1,2,2,2,3)),
                     rgroup=c("No intra-cluster correlation","Normal intra-cluster correlation"),
                     n.rgroup=c(3,3),
                     tfoot="&dagger;Mixed models to correct for clustering. &Dagger; No correction for clustering.",
                     caption="Power calculations for identifying the risk of an additional glass of water")


saveRDS(tabSingle, "results_final/single_results.RDS")

# risk between water works

# simple tests
size <- 100
f <- data.frame(anovaPval=rep(NA,size),pval1=rep(NA,size),pval2=rep(NA,size))

# OR 1.025
pb <- txtProgressBar(min=1,max=nrow(f),style=3)
for(i in 1:nrow(f)){
  f[i,] <- RunMultipleRisk()
  setTxtProgressBar(pb,i)
}
close(pb)





RAWmisc::RmdToHTML("reports/report.Rmd",paste0("reports/HTMLReport_",format(Sys.time(), "%Y_%m_%d"),".html"),copyFromReports=TRUE)








