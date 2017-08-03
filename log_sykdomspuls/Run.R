RAWmisc::InitialiseProject(
  HOME = "/analyses/code_major/log_sykdomspuls/",
  RAW = "/analyses/data_raw/log_sykdomspuls/",
  CLEAN = "/analyses/data_clean/log_sykdomspuls",
  BAKED = "/analyses/results_baked/log_sykdomspuls/",
  FINAL = "/analyses/results_final/log_sykdomspuls/",
  SHARED = "/dropbox/results_shared/log_sykdomspuls/")

library(data.table)

files <- list.files(file.path(RAWmisc::PROJ$RAW))
d <- vector("list",length=length(files))
for(i in 1:length(files)){
  d[[i]] <- fread(file.path(RAWmisc::PROJ$RAW,files[i]),header=FALSE)
  setnames(d[[i]],c("date","time","x","ipForwarded","x","ipRaw","x","page","x","args"))
  d[[i]] <- d[[i]][,-which(names(d[[i]])=="x"),with=F]
}

d <- rbindlist(d)
d[,ipForwarded:=stringr::str_extract(ipForwarded,"^[0-9]*.[0-9]*.[0-9]*.[0-9]*")]

ips <- unique(d$ipForwarded)
locations <- data.table(rgeolocate::ip_api(ips))
locations[,ipForwarded:=ips]

d <- merge(d,locations,by="ipForwarded")
setorder(d,ipForwarded,ipRaw,date,time)
d[page=="/test" & args=="?x=1",session:=c(1:.N),by=ipForwarded]
d[,session:=zoo::na.locf(session)]
d <- d[page!="/test"]

xtabs(~d$city_name)
xtabs(~d$page)

d[,.(pageVisits=.N),by=.(ipForwarded,city_name)]
q <- ggplot(d)


rmarkdown::render(input = "peep_interim_white.Rmd", output_file = "peep_interim_white.pdf", 
                  output_dir = RAWmisc::PROJ$SHARED_TODAY, output_format = rmarkdown::pdf_document())

