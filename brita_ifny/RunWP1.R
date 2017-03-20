RAWmisc::InitialiseProject(
  PROJHOME = "/analyses/code_major/brita_ifny/",
  PROJRAW = "/analyses/data_raw/brita_ifny/",
  PROJCLEAN = "/analyses/data_clean/brita_ifny",
  PROJBAKED = "/analyses/results_baked/brita_ifny/",
  PROJFINAL = "/analyses/results_final/brita_ifny/",
  PROJSHARED = "/dropbox/results_shared/brita_ifny/")

suppressWarnings(suppressMessages(library(data.table)))
suppressWarnings(suppressMessages(library(ggplot2)))

assign("RUN_ALL", TRUE, envir=globalenv())

unlink(file.path(RPROJ$PROJSHARED,lubridate::today()), recursive=TRUE, force=TRUE)
dir.create(file.path(RPROJ$PROJSHARED,lubridate::today()))

d <- readxl::read_excel(file.path(RPROJ$PROJRAW,"Kopi av RW_Spline_numbers_for_Richard_19_Jan_2017.xlsx"),sheet=2)
names(d) <- c("type","x","y","ymin","ymax","x1","x2","x3","x4")
d$y <- exp(d$y)
d$ymin <- exp(d$ymin)
d$ymin[d$type=="b" & d$x>0 & d$x<1] <- log(d$ymin[d$type=="b" & d$x>0 & d$x<1])
d$ymax <- exp(d$ymax)

q <- ggplot(d[d$type=="a",],aes(x=x,y=log2(y),ymin=log2(ymin),ymax=log2(ymax)))
q <- q + geom_rect(xmin=-Inf,xmax=0.35,ymin=-Inf,ymax=Inf,fill="#99d594",alpha=0.05)
q <- q + geom_rect(xmin=0.35,xmax=0.7,ymin=-Inf,ymax=Inf,fill="gray",alpha=0.04)
q <- q + geom_rect(xmin=0.7,xmax=1,ymin=-Inf,ymax=Inf,fill="#ffffbf",alpha=0.05)
#q <- q + geom_rect(xmin=1,xmax=Inf,ymin=-Inf,ymax=Inf,fill="#fc8d59",alpha=0.05)
q <- q + geom_line(lwd=2)
q <- q + geom_pointrange(lwd=3)
q <- q + geom_hline(yintercept=0,col="red")
q <- q + geom_label(data=d[d$type=="a" & d$y!=1,],mapping=aes(y=log2(ymax)+0.2,label=formatC(y,digits=1,format="f")),size=10)
q <- q + scale_y_continuous("Hazard ratio",breaks=log2(c(0.5,1,2,4,8,16)),labels=c("1/2","1","2","4","8","16"))
q <- q + scale_x_continuous("IFN-y value",
                            breaks=c(0,0.35,0.7,1,2,3,4,5,6,7,8,9,10),
                            labels=c("0","0.35","0.7","1","2","3","4","5","6","7","8","9","10"))
q <- q + RAWmisc::theme_SMAO(28)
q <- q + theme(panel.grid.minor=element_blank())
q <- q + theme(panel.grid.major = element_line(colour = "black", 
                                               size = 1, linetype = 3))
RAWmisc::SMAOpng(file.path(RPROJ$PROJSHARED,lubridate::today(),"Figure1_a.png"),h=0.9)
print(q)
dev.off()

q <- ggplot(d[d$type=="b",],aes(x=x,y=log2(y),ymin=log2(ymin),ymax=log2(ymax)))
q <- q + geom_rect(xmin=-Inf,xmax=0.35,ymin=-Inf,ymax=Inf,fill="#99d594",alpha=0.05)
q <- q + geom_rect(xmin=0.35,xmax=0.7,ymin=-Inf,ymax=Inf,fill="gray",alpha=0.04)
q <- q + geom_rect(xmin=0.7,xmax=1,ymin=-Inf,ymax=Inf,fill="#ffffbf",alpha=0.05)
#q <- q + geom_rect(xmin=1,xmax=Inf,ymin=-Inf,ymax=Inf,fill="#fc8d59",alpha=0.05)
q <- q + geom_line(lwd=2)
q <- q + geom_pointrange(lwd=3)
q <- q + geom_hline(yintercept=0,col="red")
q <- q + geom_label(data=d[d$type=="b" & d$y!=1,],mapping=aes(y=log2(ymax)+0.2,label=formatC(y,digits=1,format="f")),size=10)
q <- q + scale_y_continuous("Hazard ratio",breaks=log2(c(0.5,1,2,4,8,16)),labels=c("1/2","1","2","4","8","16"))
q <- q + scale_x_continuous("IFN-y value",
                            breaks=c(0,0.35,0.7,1,2,3,4,5,6,7,8,9,10),
                            labels=c("0","0.35","0.7","1","2","3","4","5","6","7","8","9","10"))
q <- q + RAWmisc::theme_SMAO(28)
q <- q + theme(panel.grid.minor=element_blank())
q <- q + theme(panel.grid.major = element_line(colour = "black", 
                                               size = 1, linetype = 3))
RAWmisc::SMAOpng(file.path(RPROJ$PROJSHARED,lubridate::today(),"Figure1_b.png"),h=0.9)
print(q)
dev.off()

