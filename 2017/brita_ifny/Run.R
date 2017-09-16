RAWmisc::AllowFileManipulationFromInitialiseProject()
RAWmisc::InitialiseProject(
  HOME = "/analyses/code_major/2017/brita_ifny/",
  RAW = "/analyses/data_raw/code_major/2017/brita_ifny/",
  CLEAN = "/analyses/data_clean/code_major/2017/brita_ifny",
  BAKED = "/analyses/results_baked/code_major/2017/brita_ifny/",
  FINAL = "/analyses/results_final/code_major/2017/brita_ifny/",
  SHARED = "/dropbox/results_shared/code_major/2017/brita_ifny/")

suppressWarnings(suppressMessages(library(data.table)))
suppressWarnings(suppressMessages(library(ggplot2)))

unlink(RAWmisc::PROJ$SHARED_TODAY, recursive=TRUE, force=TRUE)
dir.create(RAWmisc::PROJ$SHARED_TODAY)

saveA4 <- function(q,filename,landscape=T){
  ggsave(filename,plot=q,width=297,height=210, units="mm")
}

d <- readxl::read_excel(file.path(RAWmisc::PROJ$RAW,"2017_09_14_RW_Spline_numbers_for_Richard.xlsx"),sheet=4)
names(d) <- c("type","x","y","ymin","ymax","x1","x2","x3","x4")
d$y <- exp(d$y)
d$ymin <- exp(d$ymin)
#d$ymin[d$type=="b" & d$x>0 & d$x<1] <- log(d$ymin[d$type=="b" & d$x>0 & d$x<1])
d$ymax <- exp(d$ymax)

q <- ggplot(d[d$type=="a",],aes(x=x,y=log2(y),ymin=log2(ymin),ymax=log2(ymax)))
q <- q + geom_rect(xmin=-Inf,xmax=0.35,ymin=-Inf,ymax=Inf,fill="#99d594",alpha=0.05)
q <- q + geom_rect(xmin=0.35,xmax=0.7,ymin=-Inf,ymax=Inf,fill="gray",alpha=0.04)
q <- q + geom_rect(xmin=0.7,xmax=1,ymin=-Inf,ymax=Inf,fill="#ffffbf",alpha=0.05)
#q <- q + geom_rect(xmin=1,xmax=Inf,ymin=-Inf,ymax=Inf,fill="#fc8d59",alpha=0.05)
q <- q + geom_line()
q <- q + geom_pointrange()
q <- q + geom_hline(yintercept=0,col="red")
q <- q + geom_label(data=d[d$type=="a" & d$y!=1,],mapping=aes(y=log2(ymax)+0.2,label=formatC(y,digits=1,format="f")))
q <- q + scale_y_continuous("Hazard ratio on log scale",breaks=log2(c(0.5,1,2,4,8,16)),labels=c("1/2","1","2","4","8","16"))
q <- q + scale_x_continuous(bquote("IFN-"*gamma*" level (IU/ml)"),
                            breaks=c(0,0.35,0.7,1,2,3,4,5,6,7,8,9,10),
                            labels=c("0","0.35","0.7","1","2","3","4","5","6","7","8","9","10"))
q <- q + theme_classic(16)
q <- q + theme(panel.grid.major = element_line(colour = "black", linetype = 3))
saveA4(q,file.path(RAWmisc::PROJ$SHARED_TODAY,"Figure1_a.png"))

q <- ggplot(d[d$type=="b",],aes(x=x,y=log2(y),ymin=log2(ymin),ymax=log2(ymax)))
q <- q + geom_rect(xmin=-Inf,xmax=0.35,ymin=-Inf,ymax=Inf,fill="#99d594",alpha=0.05)
q <- q + geom_rect(xmin=0.35,xmax=0.7,ymin=-Inf,ymax=Inf,fill="gray",alpha=0.04)
q <- q + geom_rect(xmin=0.7,xmax=1,ymin=-Inf,ymax=Inf,fill="#ffffbf",alpha=0.05)
#q <- q + geom_rect(xmin=1,xmax=Inf,ymin=-Inf,ymax=Inf,fill="#fc8d59",alpha=0.05)
q <- q + geom_line()
q <- q + geom_pointrange()
q <- q + geom_hline(yintercept=0,col="red")
q <- q + geom_label(data=d[d$type=="b" & d$y!=1,],mapping=aes(y=log2(ymax)+0.2,label=formatC(y,digits=1,format="f")))
q <- q + scale_y_continuous("Hazard ratio on log scale",breaks=log2(c(0.5,1,2,4,8,16)),labels=c("1/2","1","2","4","8","16"))
q <- q + scale_x_continuous(bquote("IFN-"*gamma*" level (IU/ml)"),
                            breaks=c(0,0.35,0.7,1,2,3,4,5,6,7,8,9,10),
                            labels=c("0","0.35","0.7","1","2","3","4","5","6","7","8","9","10"))
q <- q + theme_classic(16)
q <- q + theme(panel.grid.major = element_line(colour = "black", linetype = 3))
saveA4(q,file.path(RAWmisc::PROJ$SHARED_TODAY,"Figure1_b.png"))

