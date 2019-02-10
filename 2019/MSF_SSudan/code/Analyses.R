


RunComparisons <- function(d,isInfluensa=TRUE,CUTOFFALPHA){
  set.seed(4)
  CUTOFFCRIT <- qnorm(CUTOFFALPHA)
  
  d[,county:="Norway"]
  d[,countyName:="Norway"]
  d <- d[yrwk<="2016-20" & age=="Totalt",.(influensa=sum(influensa)),by=.(county,countyName,year,yrwk)]
  d[,wk:=stringr::str_replace(yrwk,as.character(year),"")]
  d[,wk:=as.numeric(gsub("-","",wk))]
  d <- d[wk %in% 1:52]
  setorder(d,county,yrwk)
  
  d[,influensa1:=shift(influensa,52*1),by=county]
  d[,influensa2:=shift(influensa,52*2),by=county]
  d[,influensa3:=shift(influensa,52*3),by=county]
  d[,influensa4:=shift(influensa,52*4),by=county]
  d[,influensa5:=shift(influensa,52*5),by=county]
  d[,influensaMean:=mean(c(influensa1,influensa2,influensa3,influensa4,influensa5),na.rm=T),by=.(county,yrwk)]
  d[,influensaSD:=sd(c(influensa1,influensa2,influensa3,influensa4,influensa5),na.rm=T),by=.(county,yrwk)]
  d[,influensaThreshold1:=influensaMean+2*influensaSD]
  d[!is.na(influensaThreshold1),extreme1:=0]
  d[influensa>influensaMean+CUTOFFCRIT*influensaSD*(1+1/5),extreme1:=2]
  #d[influensa>influensaMean+4*influensaSD,extreme1:=3]
  
  d[,influensaMean:=mean(c(influensa1,influensa2),na.rm=T),by=.(county,yrwk)]
  d[,influensaSD:=sd(c(influensa1,influensa2),na.rm=T),by=.(county,yrwk)]
  d[,influensaThreshold2:=influensaMean+2*influensaSD]
  d[!is.na(influensaThreshold2),extreme2:=0]
  d[influensa>influensaMean+CUTOFFCRIT*influensaSD*(1+1/2),extreme2:=2]
  #d[influensa>influensaMean+4*influensaSD,extreme2:=3]
  
  if(isInfluensa){
    d[wk %in% 1:29,season:=paste0(year-1,"/",year)]
    d[wk %in% 30:52,season:=paste0(year,"/",year+1)]
    d[wk %in% 30:52,seasonWk:=wk-39]
    d[wk %in% 1:29,seasonWk:=wk+13]
    seasonYears <- c(1:20,40:52)
  } else {
    d[,season:=paste0(year)]
    d[,seasonWk:=wk]
    seasonYears <- c(1:52)
  }
  allSeasons <- unique(d$season)
  allSeasons <- allSeasons[!allSeasons=="2005/2006"]
  
  extraSeasons <- NULL
  for(s in allSeasons[-c(1:2)]){
    
    x <- reshape2::dcast(d[wk %in% seasonYears & season %in% c(allSeasons[c(1:2)],extraSeasons),c("influensa","seasonWk","season"),with=F],
                         seasonWk ~ season, value.var="influensa")
    x[,"seasonWk"] <- NULL
    epi<-epimem(x)
    
    epi$epi.intervals[,4]
    epi$pre.post.intervals[2,3]
    
    d[season %in% s,extreme3:=0]
    d[season %in% s & influensa>epi$pre.post.intervals[1,3],extreme3:=1]
    d[season %in% s & influensa>epi$epi.intervals[1,4],extreme3:=2]
    #d[season %in% s & influensa>epi$epi.intervals[2,4],extreme3:=3]
    #d[season %in% s & influensa>epi$epi.intervals[3,4],extreme3:=4]
    extraSeasons <- c(extraSeasons,s)
  }
  
  extraSeasons <- allSeasons[c(1:2)]
  for(s in allSeasons[-c(1:2)]){
    
    x <- reshape2::dcast(d[wk %in% seasonYears & season %in% c(extraSeasons),c("influensa","seasonWk","season"),with=F],
                         seasonWk ~ season, value.var="influensa")
    x[,"seasonWk"] <- NULL
    epi<-epimem(x)
    
    epi$epi.intervals[,4]
    epi$pre.post.intervals[2,3]
    
    d[season %in% s,extreme4:=0]
    d[season %in% s & influensa>epi$pre.post.intervals[1,3],extreme4:=1]
    d[season %in% s & influensa>epi$epi.intervals[1,4],extreme4:=2]
    #d[season %in% s & influensa>epi$epi.intervals[2,4],extreme4:=3]
    #d[season %in% s & influensa>epi$epi.intervals[3,4],extreme4:=4]
    extraSeasons <- c(extraSeasons[-1],s)
  }
  
  
  for(s in allSeasons){
    
    x <- reshape2::dcast(d[wk %in% seasonYears,c("influensa","seasonWk","season"),with=F],
                         seasonWk ~ season, value.var="influensa")
    x[,"seasonWk"] <- NULL
    epi<-epimem(x)
    
    epi$epi.intervals[,4]
    epi$pre.post.intervals[2,3]
    
    d[season %in% s,extreme6:=0]
    d[season %in% s & influensa>epi$pre.post.intervals[1,3],extreme6:=1]
    d[season %in% s & influensa>epi$epi.intervals[1,4],extreme6:=2]
    #d[season %in% s & influensa>epi$epi.intervals[2,4],extreme6:=3]
    #d[season %in% s & influensa>epi$epi.intervals[3,4],extreme6:=4]
  }
  
  d[,influensaPredicted2:=c(rep(NA,8),zoo::rollapply(influensa,9,function(x){
    df <- data.frame(x=1:9,y=c(x[1:8],NA))
    #return(df$x[2])
    fit <- lm(y~x,data=df)
    return(predict(fit,df,interval="prediction",level=(1-2*(1-CUTOFFALPHA)))[9,3])
  },align="left"))]
  
  #d[,c("yrwk","influensa","influensaPredicted2","extreme5"),with=F][1:20]
  
  #d[,influensaPredicted4:=c(rep(NA,8),zoo::rollapply(influensa,9,function(x){
  #  df <- data.frame(x=1:9,y=c(x[1:8],NA))
  #  fit <- lm(y~x,data=df)
  #  return(predict(fit,df,interval="prediction",level=pnorm(4))[9,3])
  #},align="left"))]
  
  d[!is.na(influensaPredicted2),extreme5:=0]
  d[influensa>influensaPredicted2 & influensaPredicted2>0,extreme5:=2]
  #d[influensa>influensaPredicted4 & influensaPredicted4>0,extreme5:=3]
  
  set.seed(4)
  d[,extreme7:=sample(c(0,2),nrow(d),replace=TRUE,prob=c(1-mean(d$extreme6>1,na.rm=T),mean(d$extreme6>1,na.rm=T)))]
  d[,extreme8:=sample(c(0,2),nrow(d),replace=TRUE,prob=c(1-mean(d$extreme6>1,na.rm=T),mean(d$extreme6>1,na.rm=T)))]
  d[,extreme9:=sample(c(0,2),nrow(d),replace=TRUE,prob=c(1-mean(d$extreme6>1,na.rm=T),mean(d$extreme6>1,na.rm=T)))]
  d[,extreme10:=sample(c(0,2),nrow(d),replace=TRUE,prob=c(1-mean(d$extreme6>1,na.rm=T),mean(d$extreme6>1,na.rm=T)))]
  d[,extreme11:=sample(c(0,2),nrow(d),replace=TRUE,prob=c(1-mean(d$extreme6>1,na.rm=T),mean(d$extreme6>1,na.rm=T)))]
  d[,extreme12:=sample(c(0,2),nrow(d),replace=TRUE,prob=c(1-mean(d$extreme6>1,na.rm=T),mean(d$extreme6>1,na.rm=T)))]
  d[,extreme13:=sample(c(0,2),nrow(d),replace=TRUE,prob=c(1-mean(d$extreme6>1,na.rm=T),mean(d$extreme6>1,na.rm=T)))]
  d[,extreme14:=sample(c(0,2),nrow(d),replace=TRUE,prob=c(1-mean(d$extreme6>1,na.rm=T),mean(d$extreme6>1,na.rm=T)))]
  d[,extreme15:=sample(c(0,2),nrow(d),replace=TRUE,prob=c(1-mean(d$extreme6>1,na.rm=T),mean(d$extreme6>1,na.rm=T)))]
  
  
  dPlot <- d[,c("season","seasonWk","countyName","extreme1","extreme2","extreme3","extreme4","extreme5","extreme6","extreme7","extreme8","extreme9","extreme10","extreme11","extreme12","extreme13","extreme14","extreme15"),with=F]
  setnames(dPlot,c("extreme1","extreme2","extreme3","extreme4","extreme5","extreme6","extreme7","extreme8","extreme9","extreme10","extreme11","extreme12","extreme13","extreme14","extreme15"),c("Mean+SD (2006)","Mean+SD (2 yrs)","MeM (2006)","MeM (2 yrs)","Linear Reg (8 wks)","MeM - All data","Random1","Random2","Random3","Random4","Random5","Random6","Random7","Random8","Random9"))
  dPlot <- melt.data.table(dPlot,id=c("season","seasonWk","countyName"))
  dPlot[,cumsum:=0]
  dPlot[value>1,cumsum:=1]
  dPlot[,cumsum:=cumsum(cumsum),by=.(variable,season)]
  dPlot[is.na(value),cumsum:=NA]
  
  dPlot[,variable:=factor(variable,levels=c(
    "MeM - All data",
    "Mean+SD (2006)",
    "MeM (2006)",
    "Mean+SD (2 yrs)",
    "MeM (2 yrs)",
    "Linear Reg (8 wks)","Random1","Random2","Random3","Random4","Random5","Random6","Random7","Random8","Random9"
  ))]
  
  return(dPlot)
  
}

FormatComparisons <- function(dPlot,tag, isInfluensa=TRUE, CUTOFFALPHA,allyears=FALSE){
  if(allyears){
    x <- reshape2::dcast(dPlot,"season+seasonWk~variable",value.var="cumsum")
  } else {
    x <- na.omit(reshape2::dcast(dPlot,"season+seasonWk~variable",value.var="cumsum"))
  }
  
#  res <- vector("list",length=length(unique(x$season)))
#  for(i in 1:length(res)){
#    res[[i]] <- cor(x[x$season==unique(x$season)[i],-c(1:2)],use="pairwise.complete.obs",method="spearman")[,-c(7:15)]
#    for(j in 1:ncol(res[[i]])) res[[i]][is.na(res[[i]][,j]),j] <- 0
#  }
#  res <- do.call("rbind",res)
  res <- cor(x[,-c(1:2)],use="pairwise.complete.obs",method="spearman")[,-c(7:15)]
  saveRDS(res,paste0(org::PROJ$SHARED_TODAY,"/rawcorr_",tag,"_",CUTOFFALPHA,".RDS"))
  
  if(FALSE){
  x <- htmlTable::htmlTable(format(round(res,2),nsmall=2),
                       caption="Spearmans correlation coefficient for the cumulative number of spikes per year",
                       n.rgroup=c(1,2,3,3),
                       rgroup=c("All data - gold standard","Surveillance begins 1/1/2006","Surveillance data restricted","Spikes randomly assigned"),
                       col.columns = c(rep("#E6E6F0", 1),
                                       rep("none", 5)))
  saveRDS(x,paste0("results_final/corr_",tag,"_",CUTOFFALPHA,".RDS"))
  
  if(isInfluensa){
    q <- ggplot(dPlot[season!="2005/2006" & variable %in% c(
    "Mean+SD (2006)","MeM (2006)","Mean+SD (2 yrs)","MeM (2 yrs)","Linear Reg (8 wks)","MeM - All data"
  )],aes(x=seasonWk,y=cumsum))
  } else {
    q <- ggplot(dPlot[season!="2005/2006" & variable %in% c(
      "Mean+SD (2 yrs)","MeM (2 yrs)","Linear Reg (8 wks)","MeM - All data"
    )],aes(x=seasonWk,y=cumsum))
  }
  q <- q + geom_line(data=dPlot[season!="2005/2006" & variable %in% c("MeM - All data")],lwd=3,colour="red")
  q <- q + geom_line(aes(colour=variable),lwd=1.25)
  q <- q + facet_wrap(~season)
  if(isInfluensa){
    q <- q + geom_vline(aes(xintercept=0),col="red")
    q <- q + geom_vline(aes(xintercept=32),col="red")
  }
  q <- q + scale_colour_manual("",values=c("red","darkgreen","black","orange","purple","blue"))
  q <- q + scale_x_continuous("Season week")
  q <- q + scale_y_continuous("Cumulative sum of spikes")
  RAWmisc::SMAOpng(paste0("results_final/cumsum_",tag,".png"),w=.7,h=.7)
  print(q)
  dev.off()
  
  if(isInfluensa){
    q <- ggplot(dPlot[season!="2005/2006" & !stringr::str_detect(variable,"Random")],aes(x=seasonWk,y=variable,fill=factor(value)))
  } else {
    q <- ggplot(dPlot[season!="2005/2006" & variable %in% c("Mean+SD (2 yrs)","MeM (2 yrs)","Linear Reg (8 wks)","MeM - All data")],aes(x=seasonWk,y=variable,fill=factor(value)))
  }
  q <- q + geom_tile()
  q <- q + facet_wrap(~season)
  if(isInfluensa){
    q <- q + geom_vline(aes(xintercept=0),col="red")
    q <- q + geom_vline(aes(xintercept=32),col="red")
  }
  q <- q + scale_fill_manual("",
    values=c("grey","#fef0d9","#fdcc8a","#fc8d59"),
    labels=c("No epidemic","Low intensity","Medium intensity","High/very high intensity"))
  q <- q + scale_x_continuous("Season week")
  q <- q + scale_y_discrete("")
  RAWmisc::SMAOpng(paste0("results_final/blocks_",tag,".png"),w=.7,h=.7)
  print(q)
  dev.off()
  }
}