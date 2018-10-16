RAWmisc::InitialiseOpinionatedUnix("code_major/2018/lreports")

library(tidyverse)
library(xml2)
library(data.table)

GetServProvider <- function(x){
  temp <- xml2::xml_find_all(x, ".//ServProvider//Inst//Name")
  temp <- xml_text(temp)
  
  return(temp[1])
}

GetInvDate <- function(r){
  temp <- xml2::xml_find_all(r, ".//InvDate")
  temp <- as.character(unlist(xml_attrs(temp)))
  
  return(temp)
}

GetRefAnalysedSubject <- function(r){
  temp <- xml2::xml_find_all(r, ".//RefAnalysedSubject")
  temp <- xml2::xml_text(temp)
  
  return(temp)
}

GetInvestigation <- function(r){
  temp <- xml2::xml_find_all(r, ".//Investigation//Id")
  temp2 <- list()
  for(i in 1:length(temp)){
    temp2[[i]] <- data.frame(t(unlist(xml_attrs(temp[[i]]))))
  }
  temp <- rbindlist(temp2)
  return(temp)
}

GetTextResultValue <- function(r){
  temp <- xml2::xml_find_all(r, ".//TextResultValue")
  temp <- xml2::xml_text(temp)
  
  return(temp)
}

FormatResult <- function(r){
  invDate <- GetInvDate(r)
  refAnalysedSubject <- GetRefAnalysedSubject(r)
  investigation <- GetInvestigation(r)
  textResultValue <- GetTextResultValue(r)
  
  retval <- copy(investigation)
  retval[,invDate:=invDate]
  retval[,refAnalysedSubject:=refAnalysedSubject]
  retval[,textResultValue:=textResultValue]
  
  return(retval)
}

files <- list.files(file.path(RAWmisc::PROJ$RAW,"2018-08-21"))
#f <- "0a05e307-4b18-41e7-977a-4fa9b7125655.xml"
dir.create(file.path(RAWmisc::PROJ$RAW,"sorted"))
for(f in files){
  x <- xml2::read_xml(file.path(RAWmisc::PROJ$RAW,"2018-08-21",f))
  sp <- GetServProvider(x)
  dir.create(file.path(RAWmisc::PROJ$RAW,"sorted",sp))
  xml2::write_xml(x,file.path(RAWmisc::PROJ$RAW,"sorted",sp,f))
}
RAWmisc::RCloneSync(from=file.path(RAWmisc::PROJ$RAW,"sorted"),to=file.path(RAWmisc::PROJ$RCLONE_RAW,"sorted"))


files <- list.files(file.path(RAWmisc::PROJ$RAW,"sorted","Helse Bergen HF"))
for(f in files){
  x <- xml2::read_xml(file.path(RAWmisc::PROJ$RAW,"sorted","Helse Bergen HF",f))
  sp <- GetServProvider(x)
  results <- xml2::xml_find_all(x, ".//ResultItem")
  for(r in results){
    
  }
  dir.create(file.path(RAWmisc::PROJ$RAW,"sorted",sp))
  xml2::write_xml(x,file.path(RAWmisc::PROJ$RAW,"sorted",sp,f))
}
RAWmisc::RCloneSync(from=file.path(RAWmisc::PROJ$RAW,"sorted"),to=file.path(RAWmisc::PROJ$RCLONE_RAW,"sorted"))



institutions <- xml2::xml_find_all(x, ".//Inst")
results <- xml2::xml_find_all(x, ".//ResultItem")
r <- results[[1]]
for(i in 1:length(results)){
  
  xml2::xml_find_all(x, ".//ResultItem")
}
r <- xml2::xml_child(results,1)
 xml2::xml_find_all(r, ".//Investigation")

strsplit(vals, "[[:space:]]+") %>% 
  map_df(~as_data_frame(as.list(setNames(., cols)))) %>% 
  mutate(area_name=labs)