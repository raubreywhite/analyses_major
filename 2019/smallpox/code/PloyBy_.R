PlotBy <- function(res,varx,groupingx="c_omrade",force=FALSE){
  groupings <- c("c_omrade","c_arbeidsoppgaver","c_lenge")
  groupings <- groupings[!groupings %in% groupingx]
  res[,grouping:=get(groupingx)]
  pd <- res[var==varx & get(groupings[1])=="Alle" & get(groupings[2])=="Alle"]
  
  testing <- pd[grouping!="Alle"][,c("N","grouping","x")]
  testing <- dcast.data.table(testing,x~grouping,value.var = "N")
  testing <- as.matrix(testing)[,2:ncol(testing)]
  for(i in 1:ncol(testing)){
    testing[is.na(testing[,i]),] <- 0
    #testing[testing[,i]==0,] <- 1
  }
  pval <- round(fisher.test(testing,simulate.p.value=TRUE)$p.value,3)
  if(pval>0.05 & !force) return(FALSE)
  
  ordering <- unique(pd[,c("x","value")])
  ordering[,xx:=as.numeric(x)]
  setorder(ordering,xx)
  ordering[,colours:="black"]
  ordering[!value %in% c("Vet ikke",
                         "Vet ikke/ikke svart",
                         "Ikke valgt",
                         "Vet ikke, Ikke relevant"),colours:=RColorBrewer::brewer.pal(.N,"RdBu")]
  
  display <- ordering$colours
  names(display) <- ordering$value
  pd[,value:=factor(value,levels=rev(ordering$value))]
  if(nrow(pd)==0) next
  #if(stringr::str_detect(vars[i],"^op_38")) next
  #if(stringr::str_detect(vars[i],"^op_13")) next
  
  q <- ggplot(pd[denom>=10],aes(x=grouping, fill=value))
  q <- q + geom_col(alpha=0.6,mapping=aes(y=N/denom),colour="black")
  #q <- q + geom_text(mapping=aes(y=N/denom+0.1,label=sprintf("%s%%",round(N/denom*100))),size=2)
  q <- q + lemon::facet_rep_grid(grouping~.,scales="free", repeat.tick.labels="x")
  q <- q + scale_y_continuous("Andel",labels = scales::percent_format(),lim=c(0,1))
  q <- q + scale_x_discrete("",drop=T)
  q <- q + coord_flip()
  q <- q + scale_fill_manual("",values=display)
  q <- q + labs(title=Shorten(attributes(dInfo[[varx]])$label,len=70))
  q <- q + labs(caption=sprintf("P-verdi=%s",pval))
  q <- q + theme_gray(20)
  q <- q + theme(strip.text.y = element_blank(),
                 strip.text.x = element_blank())
  q
  RAWmisc::saveA4(q,
                  file.path(org::PROJ$SHARED_TODAY,"descriptives",sprintf("%s_%s.png",varx,groupingx)))
  return(TRUE)
}


PlotByNothing <- function(res,varx){
  groupings <- c("c_omrade","c_arbeidsoppgaver","c_lenge")
  res[,grouping:="Alle"]
  pd <- res[var==varx & get(groupings[1])=="Alle" & get(groupings[2])=="Alle" & get(groupings[3])=="Alle"]
  
  ordering <- unique(pd[,c("x","value")])
  ordering[,xx:=as.numeric(x)]
  setorder(ordering,xx)
  ordering[,colours:="black"]
  ordering[!value %in% c("Vet ikke",
                         "Vet ikke/ikke svart",
                         "Ikke valgt",
                         "Vet ikke, Ikke relevant"),colours:=RColorBrewer::brewer.pal(.N,"RdBu")]
  
  display <- ordering$colours
  names(display) <- ordering$value
  pd[,value:=factor(value,levels=rev(ordering$value))]
  if(nrow(pd)==0) next
  #if(stringr::str_detect(vars[i],"^op_38")) next
  #if(stringr::str_detect(vars[i],"^op_13")) next
  
  q <- ggplot(pd[denom>=10],aes(x=grouping, fill=value))
  q <- q + geom_col(alpha=0.6,mapping=aes(y=N/denom),colour="black")
  #q <- q + geom_text(mapping=aes(y=N/denom+0.1,label=sprintf("%s%%",round(N/denom*100))),size=2)
  q <- q + lemon::facet_rep_grid(grouping~.,scales="free", repeat.tick.labels="x")
  q <- q + scale_y_continuous("Andel",labels = scales::percent_format(),lim=c(0,1))
  q <- q + scale_x_discrete("",drop=T)
  q <- q + coord_flip()
  q <- q + scale_fill_manual("",values=display)
  q <- q + labs(title=Shorten(attributes(dInfo[[varx]])$label,len=70))
  q <- q + theme_gray(20)
  #q <- q + labs(caption=sprintf("P-verdi=%s",pval))
  q <- q + theme(strip.text.y = element_blank(),
                 strip.text.x = element_blank())
  q
  RAWmisc::saveA4(q,
                  file.path(org::PROJ$SHARED_TODAY,"descriptives",sprintf("%s_%s.png",varx,"all")))
  return(TRUE)
}
