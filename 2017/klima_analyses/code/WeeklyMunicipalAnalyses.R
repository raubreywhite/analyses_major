WeeklyMuncipialAnalyses <- function(dataWeek){
  variablesOfInterest <- c("binary_a_precipCorr","binary_a_precipUncorr","binary_a_runoff","binary_a_temp")
  variablesOfInterestContinuous <- c(gsub("binary_","",variablesOfInterest),"pop")
  municipLevels <- unique(data$municip)
  res <- vector("list",length(municipLevels)*4)
  index <- 1
  for(i in 1:length(municipLevels)){
    m <- municipLevels[i]
    for(j in variablesOfInterest){
      formula <- paste0("s_gastro~offset(log(pop)) +",j,"+ year + sin(2*pi*(week-1)/52) + cos(2*pi*(week-1)/52) + helligdagIndikator")
    
      fit <- glm(as.formula(formula),
                 data=dataWeek[municip==m],family="poisson")
      
      temp <- data.frame(coef=na.omit(coef(fit)),se=arm::se.coef(fit))
      temp$municip <- m
      temp$var <- row.names(temp)
      row.names(temp) <- NULL
      temp <- temp[stringr::str_detect(temp$var,j),]
      temp$pval <- 2*(1-pnorm(abs(temp$coef/temp$se)))
      res[[index]] <- temp
      index <- index + 1
    }
  }
  
  res <- rbindlist(res)
  res[,isSig:=pval<0.05]
  res[,direction:="Non-significant"]
  res[isSig==TRUE & coef<0,direction:="Protective"]
  res[isSig==TRUE & coef>0,direction:="Harmful"]
  
  for(i in variablesOfInterest){
    map <- merge(mapSkeleton,res[var==i],by="municip",all.x=TRUE)
    setorder(map,id,order)
    
    lcc_proj <- "+proj=lcc +lat_1=-18 +lat_2=-36 +lat_0=-25 +lon_0=135 +ellps=aust_SA  +units=m +no_defs"
    q <- ggplot()
    q <- q + geom_map(data=map,map=map,aes(x=long,y=lat,map_id=id,fill=direction))
    #q <- q + ggalt::coord_proj(lcc_proj)
    q <- SMAOgraphs::SMAOFormatGGPlot(q)
    q <- q + scale_fill_brewer("",palette="Set1")
    SMAOgraphs::SMAOpng(paste0("results_final/map_weekly_municipal_",i,".png"))
    print(q)
    dev.off()
  }

  dataInfo <- dataWeek[,
    lapply(.SD,mean)
  ,by=municip,.SDcols=c(variablesOfInterestContinuous)]
  
  dataInfo[,(variablesOfInterestContinuous):=lapply(.SD, function(x) as.numeric(gtools::quantcut(x,q=4))),.SDcols=c(variablesOfInterestContinuous)]
  
  resInfo <- merge(res,dataInfo,by="municip")
  resInfo[,coef:=NULL]
  resInfo[,se:=NULL]
  resInfo[,pval:=NULL]
  resInfo[,isSig:=NULL]
  resInfo <- melt(resInfo,id=c("municip","var","direction"))
  resInfo <- resInfo[,.(
    num=.N
  ),by=.(
    var,variable,value,direction
  )]
  resInfo[,denom:=sum(num),by=.(var,variable,value)]
  resInfo[,percSig:=num/denom*100]
  resInfo$direction <- factor(resInfo$direction,levels=rev(c("Non-significant","Protective","Harmful")))
  setorder(resInfo,direction)
  resInfo[,variable:=paste0("Stratified by:\n",variable)]
  
  q <- ggplot(resInfo,aes(y=percSig,x=value,fill=direction))
  q <- q + geom_bar(stat="identity")
  q <- q + geom_hline(yintercept=5,colour="red",lty=3)
  q <- q + facet_grid(variable~var,scales="free_x")
  q <- q + scale_fill_brewer("",palette="Set1")
  q <- q + scale_x_continuous("Quartile of stratification variable")
  q <- q + scale_y_continuous("%")
  q <- SMAOgraphs::SMAOFormatGGPlot(q)
  SMAOgraphs::SMAOpng(paste0("results_final/perc_sig_weekly_municipal.png"))
  print(q)
  dev.off()
  
}