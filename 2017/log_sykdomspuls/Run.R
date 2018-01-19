RAWmisc::AllowFileManipulationFromInitialiseProject()
RAWmisc::InitialiseProject(
  HOME = "/git/code_major/2017/log_sykdomspuls/",
  RAW = "/dropbox/data_raw/log_sykdomspuls/",
  CLEAN = "/analyses/data_clean/code_major/2017/log_sykdomspuls",
  BAKED = "/analyses/results_baked/code_major/2017/log_sykdomspuls/",
  FINAL = "/analyses/results_final/code_major/2017/log_sykdomspuls/",
  SHARED = "/dropbox/results_shared/code_major/2017/log_sykdomspuls/")

library(data.table)
library(ggplot2)

Colours <- function(n){
  retval <- c()
  for(i in 1:(n%%8)){
    if(i==1){
      retval <- c(retval,RColorBrewer::brewer.pal(8,"Set1"))
    } else if(i==2){
      retval <- c(retval,RColorBrewer::brewer.pal(8,"Set2"))
    } else {
      retval <- c(retval,RColorBrewer::brewer.pal(8,"Set3"))
    }
  }
  return(retval[1:n])
}

saveA4 <- function(q,filename,landscape=T){
  ggsave(filename,plot=q,width=297,height=210, units="mm")
}

files <- list.files(file.path(RAWmisc::PROJ$RAW))
d <- vector("list",length=length(files))
for(i in 2:length(files)){
  
  normalFunction <- function(){
    return(fread(file.path(RAWmisc::PROJ$RAW,files[i]),header=FALSE,sep=' '))
  }
  
  exceptionFunction <- function(err){
    newFile <- tempfile()
    con = file(file.path(RAWmisc::PROJ$RAW,files[i]), "r")
    while ( TRUE ) {
      line = readLines(con, n = 1)
      line <- stringr::str_replace(line, ", ","")
      if ( length(line) == 0 ) {
        break
      }
      cat(line,"\n",file=newFile, append=TRUE)
    }
    close(con)
    return(fread(newFile,header=FALSE,sep=' '))
  }
  d[[i]] <- tryCatch(normalFunction(), error=exceptionFunction, warning=exceptionFunction)
  if(ncol(d[[i]])==10){
    setnames(d[[i]],c("date","time","x","ipForwarded","x","ipRaw","x","page","x","args"))
    d[[i]] <- d[[i]][,-which(names(d[[i]])=="x"),with=F]
  } else {
    d[[i]] <- NULL
  }
}

d <- rbindlist(d)
d[,ipForwarded:=stringr::str_extract(ipForwarded,"^[0-9]*.[0-9]*.[0-9]*.[0-9]*")]

ips <- unique(d$ipForwarded)
ips <- ips[!ips %in% ips[stringr::str_detect(ips,"[0-9][0-9][0-9][0-9]$")]]

locations <- list()
desiredIPS <- split(ips, ceiling(seq_along(ips)/20))
for(i in 16:length(desiredIPS)){
  print(i)
  locations[[i]] <- data.table(rgeolocate::ip_api(desiredIPS[[i]]))
  #locations[[i]][,ipForwarded:=desiredIPS[[i]]]
  Sys.sleep(2)
}
locations <- rbindlist(locations)

d <- merge(d,locations,by="ipForwarded")
setorder(d,ipForwarded,ipRaw,date,time)
d <- d[status=="success"]
d[page=="/test" & args=="?x=1",session:=c(1:.N),by=ipForwarded]
d[,session:=zoo::na.locf(session)]
d <- d[page!="/test"]
xtabs(~d$country_code)
d <- d[country_code=="NO"]
xtabs(~d$city_name)
xtabs(~d$region_name)
xtabs(~d$page)

d <- d[date>="2017-08-15" & !(city_name=="Oslo" & isp=="UNINETT AS")]
d[,yrwk:=RAWmisc::YearWeek(date)]

page <- d[,.(pageVisits=.N),by=.(yrwk,page)]
arg <- d[,.(pageVisits=.N),by=.(yrwk,args,page)]

arg[,xname:=stringr::str_extract(args,"xname=[a-zA-Z0-9]*")]
arg[,xname:=stringr::str_replace(xname,"xname=","")]

arg[,xage:=stringr::str_extract(args,"xage=[a-zA-Z0-9]*")]
arg[,xage:=stringr::str_replace(xage,"xage=","")]

arg[,xtype:=stringr::str_extract(args,"xtype=[a-zA-Z0-9]*")]
arg[,xtype:=stringr::str_replace(xtype,"xtype=","")]

xname <- arg[,.(pageVisits=sum(pageVisits,na.rm=T)),by=.(xname)]
setorder(xname,-pageVisits)
xname <- xname[1:20]
xname[,x:=1:.N]
q <- ggplot(xname,aes(x=x,y=pageVisits))
q <- q + geom_bar(stat="identity")
q <- q + scale_x_continuous("",breaks=xname$x,labels=xname$xname)
q <- q + theme(axis.text.x= element_text(angle=90,hjust=1,vjust=0.5))
q

xage <- arg[,.(pageVisits=sum(pageVisits,na.rm=T)),by=.(xage)]
setorder(xage,-pageVisits)
xage <- xage[1:20]
xage[,x:=1:.N]
q <- ggplot(xage,aes(x=x,y=pageVisits))
q <- q + geom_bar(stat="identity")
q <- q + scale_x_continuous("",breaks=xage$x,labels=xage$xage)
q <- q + theme(axis.text.x= element_text(angle=90,hjust=1,vjust=0.5))
q

xtype <- arg[,.(pageVisits=sum(pageVisits,na.rm=T)),by=.(xtype)]
setorder(xtype,-pageVisits)
xtype <- xtype[1:20]
xtype[,x:=1:.N]
q <- ggplot(xtype,aes(x=x,y=pageVisits))
q <- q + geom_bar(stat="identity")
q <- q + scale_x_continuous("",breaks=xtype$x,labels=xtype$xtype)
q <- q + theme(axis.text.x= element_text(angle=90,hjust=1,vjust=0.5))
q

peopleFirst <- d[,.(firstwk=min(yrwk)),by=c("ipForwarded","region_name")]
peopleFirst <- peopleFirst[,.(uniqueIPs=.N),by=.(firstwk,region_name)]
q <- ggplot(peopleFirst,aes(x=firstwk,y=uniqueIPs,fill=region_name))
q <- q + geom_bar(stat="identity",colour="black",alpha=0.75)
q <- q + scale_fill_manual(values=Colours(length(unique(peopleFirst$region_name))))
q <- q + labs(caption=sprintf("%s unique IPs in total",sum(peopleFirst$uniqueIPs)))
q

people <- unique(d[,c("ipForwarded","region_name","yrwk")])
peopleTotal <- unique(d[,c("ipForwarded","region_name")])
peopleTotal <- peopleTotal[,.(uniqueIPs=.N),by=.(region_name)] 
people <- people[,.(uniqueIPs=.N),by=.(region_name,yrwk)] 
q <- ggplot(people,aes(x=yrwk,y=uniqueIPs,fill=region_name))
q <- q + geom_bar(stat="identity",colour="black",alpha=0.75)
q <- q + scale_fill_manual(values=Colours(length(unique(peopleTotal$region_name))))
q <- q + labs(caption=sprintf("%s unique IPs in total",sum(peopleTotal$uniqueIPs)))
q <- q + theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust=0.5))
q
RAWmisc::saveA4(q, file.path(RAWmisc::PROJ$SHARED_TODAY,"unique_ips_per_week.png"))

sum(peopleTotal$uniqueIPs)


peopleTotal <- people[,.(uniqueIPs=.N),by=.(date)] 
peopleTotal[,region_name:="Totalt"]
people <- people[,.(uniqueIPs=.N),by=.(region_name,date)]
people <- rbind(people,peopleTotal)
people[,date:=as.Date(date)]
setorder(people,date)

skeleton <- data.table(expand.grid(
  region_name=unique(people$region_name),
  date=seq(min(people$date),max(people$date),1)
  ))
people <- merge(skeleton,people,by=c("region_name","date"),all.x=T)
people[is.na(uniqueIPs),uniqueIPs:=0]

q <- ggplot(people, aes(x=date,y=uniqueIPs))
q <- q + geom_vline(xintercept=as.Date("2017-08-15"),colour="red")
q <- q + geom_bar(stat="identity")
#q <- q + geom_step(direction="hv")
q <- q + facet_wrap(~region_name)
q

people[date==lubridate::today()-1 & uniqueIPs>0]
people[date==lubridate::today() & uniqueIPs>0]

sum(people[region_name=="Totalt" & date>="2017-08-15"]$uniqueIPs)

#rmarkdown::render(input = "peep_interim_white.Rmd", output_file = "peep_interim_white.pdf", 
                  output_dir = RAWmisc::PROJ$SHARED_TODAY, output_format = rmarkdown::pdf_document())

