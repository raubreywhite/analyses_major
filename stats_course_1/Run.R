RAWmisc::InitialiseProject(
  HOME = "/analyses/code_major/stats_course_1/",
  RAW = "/analyses/data_raw/stats_course_1/",
  CLEAN = "/analyses/data_clean/stats_course_1",
  BAKED = "/analyses/results_baked/stats_course_1/",
  FINAL = "/analyses/results_final/stats_course_1/",
  SHARED = "/dropbox/results_shared/stats_course_1/")

library(data.table)
library(ggplot2)
library(pomp)

GetAllResponses <- function(){
  lastVal <- list.files(RAWmisc::PROJ$RAW)
  if(length(lastVal)==0){
    return(NULL)
  } else {
    retval <- vector("list",length(lastVal))
    for(i in 1:length(retval)){
      retval[[i]] <- readRDS(file.path(RAWmisc::PROJ$RAW,lastVal[i]))
      x <- list()
      m <- 1
      for(j in 1:length(retval[[i]])){
        if(names(retval[[i]])[j]=="id") next
        for(l in 1:length(retval[[i]][[j]])){
          x[[m]] <- data.frame(var=names(retval[[i]])[j],val=retval[[i]][[j]][l],id=retval[[i]]$id)
          m <- m + 1
        }
      }
      retval[[i]] <- rbindlist(x)
    }
    retval <- rbindlist(retval)
    return(retval)
  }
}


d <- GetAllResponses()
pd <- d[var=="daySubmitted",.(N=.N),by=.(val)]
pd[,date:=as.Date(val)]
q <- ggplot(pd,aes(x=date,y=N))
#q <- q + geom_hline(yintercept=0)
q <- q + geom_line()
q <- q + geom_point()
q <- q + scale_x_date("Date submitted")
q <- q + scale_y_continuous("N",breaks=scales::pretty_breaks())
q <- q + expand_limits(y=0)
q <- q + labs(title="Number of questionnaires submitted")
q