RAWmisc::InitialiseProject(
  HOME = "/analyses/code_major/emily_salmonella/",
  RAW = "/analyses/data_raw/emily_salmonella/",
  CLEAN = "/analyses/data_clean/emily_salmonella",
  BAKED = "/analyses/results_baked/emily_salmonella/",
  FINAL = "/analyses/results_final/emily_salmonella/",
  SHARED = "/dropbox/results_shared/emily_salmonella/")

library(data.table)
library(ggplot2)
library(pomp)


theme_SMAO_V3 <- function(base_size=24, base_family=""){
  half_line <- base_size / 2
  theme_gray(base_size) +
    theme(
      # Elements in this first block aren't used directly, but are inherited
      # by others
      legend.key.size =    unit(0.11*base_size, "lines"),
      panel.grid.major =   element_line(colour = "white", size = base_size*0.045),
      panel.grid.minor =   element_line(colour = "white", size = half_line*0.045)
    )
  
}

d <- data.table(haven::read_dta(file.path(RAWmisc::PROJ$RAW,"analysis_file_salm.dta")))
d[is_diag_salm==1,.(N=.N),by=Smstoff]

d[,serovar:="S Typhimurium"]
d[Smstoff!="S Typhimurium",serovar:="Other"]

pd <- d[is_diag_salm==1,.(N=.N),by=.(serovar,month)]
skeleton <- data.table(expand.grid(serovar=c("S Typhimurium","Other"),month=1:12))
pd <- merge(skeleton,pd,by=c("serovar","month"),all.x=T)

q <- ggplot(pd,aes(x=month,y=N,fill=serovar,colour=serovar))
q <- q + geom_line(lwd=3)
q <- q + scale_x_continuous("Month",breaks=1:12,labels=c("J","F","M","A","M","J","J","A","S","O","N","D"), minor_breaks=NULL)
q <- q + scale_color_brewer("Serovar",palette="Set1")
q <- q + theme_SMAO_V3(30)
RAWmisc::SMAOpng(file.path(RAWmisc::PROJ$SHARED_TODAY,"monthly_serovar.png"))
print(q)
dev.off()



