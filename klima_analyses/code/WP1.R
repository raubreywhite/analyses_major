R2LMER <- function(fit){
  lmfit <- lm(model.response(model.frame(fit))~fitted(fit))
  summary(lmfit)$r.squared
}

WP1Analyses <- function(d){
  stack <- data.table(expand.grid(c(0:4),c("discharge","rain","precip"),c("wp950"),unique(d$variable),c("All",unique(d$id)),c(0:4),stringsAsFactors = FALSE))
  stack[,exposure:=paste0(Var3,"_",Var2,Var1,"_",Var1)]
  stack[,Var1:=NULL]
  stack[,Var2:=NULL]
  stack[,Var3:=NULL]
  setnames(stack,"Var4","outcome")
  setnames(stack,"Var5","id")
  setnames(stack,"Var6","season")
  
  
  # Your code starts here
  res <- foreach(stackIter=iter(stack, by='row')) %dopar% {
    tryCatch({
      formulaBase <- paste0("value ~ sin(2*pi*(week-1)/52) + cos(2*pi*(week-1)/52)")
      if(stackIter$id=="All"){
        fitData <- d[variable==stackIter$outcome]
      } else {
        fitData <- d[variable==stackIter$outcome & id==stackIter$id]
      }
      if(stackIter$season!=0){
        fitData <- fitData[season==stackIter$season]
      }
      
      if(length(unique(fitData$type[!is.na(fitData[,stackIter$exposure,with=F])]))!=1) formula <- paste0(formulaBase,"+type")
      if(length(unique(fitData$watersource[!is.na(fitData[,stackIter$exposure,with=F])]))!=1) formulaBase <- paste0(formulaBase,"+watersource")
      formula <- paste0(formulaBase,"+",stackIter$exposure)
      
      fitData <- fitData[c(!is.na(fitData[,stackIter$exposure,with=F]))]
      if(stackIter$id=="All"){
        fitBase <- lme4::lmer(as.formula(paste0(formulaBase,"+(1|id)")), data=fitData) 
        fit <- lme4::lmer(as.formula(paste0(formula,"+(1|id)")), data=fitData) 
        x <- ExtractValues(fit,stackIter$exposure,removeLead=T,format=F)
        x$r2increase <- R2LMER(fit)-R2LMER(fitBase)
      } else {
        fitBase <- lm(as.formula(formula), data=fitData)
        fit <- lm(as.formula(formulaBase), data=fitData)
        x <- ExtractValuesLM(fit,stackIter$exposure,removeLead=T,format=F)
        x$r2increase <- summary(fit)$r.squared-summary(fitBase)$r.squared
      }
      
      x$stub <- stackIter$exposure
      x$outcome <- stackIter$outcome
      x$id <- stackIter$id
      x$season <- stackIter$season
      x$n <- nrow(fitData)
      x$outcomeSD <- sd(fitData$value,na.rm=T)
      
      x
    },error=function(err) {
      NULL
    })
  }
  
  res <- rbindlist(res)
  if(is.null(res)) return(NULL)
  if(nrow(res)==0) return(NULL)
  res[var=="",var:="Cont"]
  res[,stub:=gsub("cat_","",stub)]
  
  res[,sig:=0]
  res[pval<0.05,sig:=1]
  
  res[,dir:="None"]
  res[sig==1 & est>0,dir:="Increases"]
  res[sig==1 & est<0,dir:="Decreases"]
  res[,dir:=factor(dir,levels=c("Increases","None","Decreases"))]
  res[,var:=factor(var,levels=c("Cont","(0,3]","(3,100]"))]
  
  res[,display:=as.character(dir)]
  res[,display:=factor(display,levels=c("Increases","None","Decreases"))]
  
  res[,window:=stringr::str_extract(stub,"[0-9]_[0-9]")]
  for(i in 1:nrow(res)) res[i,stub:=gsub(window,"",stub)]
  setnames(res,"window","lag")
  res[,lag:=stringr::str_extract(lag,"^[0-9]")]
  
  res[,outcome:=paste0("\n",outcome,"\n")]
  setorder(res,display)
  
  
  res[,season:=factor(season)]
  levels(res$season) <- c("All","Winter","Spring","Summer","Autumn")
  res <- res[!is.nan(pval)]
  return(res)
}

PlotDetailedGridWP1 <- function(p){
  displayLevels <- c(
    "Increases 0.10+ SD/day",
    "Increases 0.05-0.09 SD/day",
    "Increases 0.00-0.04 SD/day",
    "None",
    "Decreases 0.00-0.04 SD/day",
    "Decreases 0.05-0.09 SD/day",
    "Decreases 0.10+ SD/day")
  
  p[,dirDetail:="None"]
  p[sig==1 & est/outcomeSD>0,dirDetail:=displayLevels[1]]
  p[sig==1 & est/outcomeSD>=0.05,dirDetail:=displayLevels[2]]
  p[sig==1 & est/outcomeSD>=0.10,dirDetail:=displayLevels[3]]
  p[sig==1 & est/outcomeSD<0,dirDetail:=displayLevels[5]]
  p[sig==1 & est/outcomeSD<= -0.05,dirDetail:=displayLevels[6]]
  p[sig==1 & est/outcomeSD<= -0.10,dirDetail:=displayLevels[7]]
  p[,dirDetail:=factor(dirDetail,levels=displayLevels)]
  
  setorder(p,season)
  x <- p[1:5]
  x[1,season:="All"]
  x[2,season:="Winter"]
  x[3,season:="Spring"]
  x[4,season:="Summer"]
  x[5,season:="Autumn"]
  x[1:5,n:=0]
  x <- rbind(x,p)
  l <- x[,.(n=mean(n)),by=season]
  l[,season:=paste0(season," (n=",round(n),")\n")]
  levels(p$season) <- l$season
  
  x <- copy(p)
  x[1,dirDetail:=displayLevels[1]]
  x[2,dirDetail:=displayLevels[2]]
  x[3,dirDetail:=displayLevels[3]]
  x[4,dirDetail:=displayLevels[4]]
  x[5,dirDetail:=displayLevels[5]]
  x[6,dirDetail:=displayLevels[6]]
  x[7,dirDetail:=displayLevels[7]]
  
  q <- ggplot(p,aes(x=lag,y=stub,fill=dirDetail))
  q <- q + geom_tile(data=x,alpha=0)
  q <- q + geom_tile(alpha=0.6,colour="white",lwd=0.2)
  q <- q + facet_grid(outcome~season)
  q <- q + scale_x_discrete("\nWeeks lag",drop=F)
  q <- q + scale_y_discrete("",drop=F)
  q <- q + scale_fill_manual(values=c("#d73027","#fc8d59","#fee08b","Black","#d9ef8b","#91cf60","#1a9850"),drop=F)
  q <- q + RAWmisc::theme_SMAO(10)
  q <- q + theme(axis.line.y = NULL)
  q <- q + theme(axis.line.x = NULL)
  q <- q + theme(axis.ticks.length = unit(0,"lines"))
  q <- q + theme(axis.text.x = element_text(vjust=0.5))
  q <- q + theme(axis.text.y = element_text(hjust=1))
  q <- q + theme(axis.text = element_text(margin = rep(unit(1,"lines"),4)))
  return(q)
}


PlotCoefficientsWP1 <- function(p,standardized=FALSE){
  
  levels(p$season) <- c("Autumn","Summer","Spring","Winter","All")
  p[,season:=as.numeric(season)]
  p[stub=="wp950_discharge",season:=season+0.2]
  p[stub=="wp950_rain",season:=season-0.2]
  
  pSig <- p[sig==1]
  
  q <- ggplot(pSig,aes(y=season))
  if(standardized){
    q <- q + geom_point(aes(x=est/outcomeSD,colour=stub),position = "identity", alpha=0.9,size=4)
  } else {
    q <- q + geom_point(aes(x=est,colour=stub),position = "identity", alpha=0.9,size=4)
  }
  q <- q + geom_vline(xintercept=0,lty=1,colour="red",lwd=1)
  q <- q + geom_hline(yintercept=1.5,lty=1,colour="black",lwd=1)
  q <- q + geom_hline(yintercept=2.5,lty=1,colour="black",lwd=1)
  q <- q + geom_hline(yintercept=3.5,lty=1,colour="black",lwd=1)
  q <- q + geom_hline(yintercept=4.5,lty=1,colour="black",lwd=1)
  if(standardized){
    q <- q + facet_wrap(~outcome,ncol=2)
    q <- q + scale_x_continuous("\nDaily coefficient/outcome SD")
  } else {
    q <- q + facet_wrap(~outcome,ncol=2, scales="free")
    q <- q + scale_x_continuous("\nDaily coefficient")
  }
  q <- q + scale_y_continuous("",breaks = 1:5,labels=c("Autumn","Summer","Spring","Winter","All"))
  #q <- q + scale_y_discrete("",drop=F)
  #q <- q + scale_colour_manual(values=c("black","blue","yellow","green","brown"))
  q <- q + scale_colour_brewer(palette="Set2")
  q <- q + RAWmisc::theme_SMAO(10)
  q <- q + theme(axis.line.y = NULL)
  q <- q + theme(axis.line.x = NULL)
  q <- q + theme(axis.ticks.length = unit(0,"lines"))
  q <- q + theme(axis.text.x = element_text(vjust=0.5))
  q <- q + theme(axis.text.y = element_text(hjust=1))
  q <- q + theme(axis.text = element_text(margin = rep(unit(1,"lines"),4)))
  return(q)
}



PlotR2IncreaseWP1 <- function(p){
  levels(p$season) <- c("Autumn","Summer","Spring","Winter","All")
  p[,season:=as.numeric(season)]
  p[stub=="wp950_discharge",season:=season+0.2]
  p[stub=="wp950_rain",season:=season-0.2]
  
  pSig <- p[sig==1]
  
  q <- ggplot(pSig,aes(x=season))
  q <- q + geom_point(aes(y=r2increase,colour=stub),position = "identity", alpha=0.9,size=4)
  q <- q + geom_hline(yintercept=0,lty=1,colour="red",lwd=1)
  q <- q + geom_vline(xintercept=1.5,lty=1,colour="black",lwd=1)
  q <- q + geom_vline(xintercept=2.5,lty=1,colour="black",lwd=1)
  q <- q + geom_vline(xintercept=3.5,lty=1,colour="black",lwd=1)
  q <- q + geom_vline(xintercept=4.5,lty=1,colour="black",lwd=1)
  q <- q + facet_wrap(~outcome,ncol=2)
  q <- q + scale_y_continuous("\nR2 increase")
  q <- q + scale_x_continuous("",breaks = 1:5,labels=c("Autumn","Summer","Spring","Winter","All"))
  q <- q + coord_flip()
  #q <- q + scale_y_discrete("",drop=F)
  #q <- q + scale_colour_manual(values=c("black","blue","yellow","green","brown"))
  q <- q + scale_colour_brewer(palette="Set2")
  q <- q + RAWmisc::theme_SMAO(10)
  q <- q + theme(axis.line.y = NULL)
  q <- q + theme(axis.line.x = NULL)
  q <- q + theme(axis.ticks.length = unit(0,"lines"))
  q <- q + theme(axis.text.x = element_text(vjust=0.5))
  q <- q + theme(axis.text.y = element_text(hjust=1))
  q <- q + theme(axis.text = element_text(margin = rep(unit(1,"lines"),4)))
  return(q)
}
