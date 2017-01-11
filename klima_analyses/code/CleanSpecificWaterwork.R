CleanWP1SpecificWaterWork <- function(fileIn, fileOut,
                                      sheet,
                                      skip,
                                      col_types,
                                      col_names,
                                      type,
                                      units=NULL,
                                      waterwork,
                                      waterType,
                                      point="?"
                                      ){
  ########
  # data_raw/WP1_waterworks/Trondheim_Vikelvdalen/Rådata/AkkreditertKJEMISK 2007råvann
  #
  #
  ########
  
  d <- data.table(readxl::read_excel(fileIn,
                                     sheet = sheet,
                                     skip = skip,
                                     col_names = FALSE,
                                     col_types = col_types))
  
  setnames(d, col_names)
  
  d <- d[, which(names(d) != "X"), with = F]
  
  if(class(d$date[1])[1]=="character"){
    if(!is.na(as.Date(d$date[1],format="%d.%m.%Y"))){
      d[,date:=as.Date(date,format="%d.%m.%Y")]
    }
  }
  d[,date:=as.Date(date)]
  
  d <- melt.data.table(d, id = c("date"), variable.factor = FALSE)
  d[, variable := gsub("[0-9]*$", "", variable)]
  d[, type := type]
  unique(d$variable)
  
  d[, units := ""]
  if(!is.null(units)) for(i in 1:length(units)){
    v = names(units)[i]
    u = units[i]
    d[variable==v, units:=u]
  }
  
  d[, waterwork := waterwork]
  d[, waterType := waterType]
  d[, point:=point]
  
  saveRDS(d, paste0("data_clean/WP1_waterworks/",fileOut))
  
  print(d[c(1,10)])
}


CleanWP1SpecificWaterWorkLong <- function(fileIn, fileOut,
                                      sheet=1,
                                      type,
                                      waterwork,
                                      waterType,
                                      remove=NULL
                                      ){


d <- data.table(readxl::read_excel(fileIn,
                                     sheet = sheet,
                                     skip = 1,
                                     col_names = FALSE,
                                     col_types = c("text","text","date",rep("text", 5))))
  
  setnames(d, c("valid","X","date", "point", "var2", "value", "units","X"))
  d <- d[valid=="Godkjent"]
  d[,valid:=NULL]
  d <- d[, which(names(d) != "X"), with = F]
  #d[, date := as.Date(date, format = "%d.%m.%Y")]
  
  d[, variable := ""]
  unique(d$var2)
  if(!is.null(remove)) d[,var2:=gsub(remove,"",var2)]
  d[var2 == "06-E.coli (/100 ml)", variable := "E. Coli"]
  d[var2 == "09-Koliforme bakterier 37°C (/100 ml)", variable := "Coliform bacteria"]
  d[var2 == "08-Kimtall 22°C (/ml)", variable := "Colony count at 22 degrees"]
  d[var2 == "01-Farge (mg/l Pt)", variable := "Colour"]
  d[var2 == "35-Konduktivitet (mS/m)", variable := "Conductivity"]
  d[var2 == "44-pH, surhetsgrad ( )", variable := "pH"]
  d[var2 == "07-Intestinale enterokokker (/100 ml)", variable := "Intestinal Enterococci"]
  d[var2 == "04-Turbiditet (FNU)", variable := "Turbidity"]
  
  d[var2 == "09-Koliforme bakterier 37°C (/100 ml)", variable := "Coliform bacteria at 37 degrees"]
  d[var2 == "06-E.coli (/100 ml)", variable := "E. Coli"]
  d[var2 == "05-Clostridium perfringens (/100ml)", variable := "Clostridium Perfringens"]
  d[var2 == "07-Intestinale enterokokker (/100 ml)", variable := "Intestinal Enterococci"]
  d[var2 == "04-Turbiditet (FNU)", variable := "Turbidity"]
  d[var2 == "01-Farge (mg/l Pt)", variable := "Colour"]
  d[var2 == "08-Kimtall 22°C (/ml)", variable := "Colony count at 22 degrees"]
  d[var2 == "44-pH, surhetsgrad ( )", variable := "pH"]
  d[var2 == "35-Konduktivitet (mS/m)", variable := "Conductivity"]
  d[var2 == "53-Totalt organisk karbon (TOC) (mg/l C)", variable := "TOC"]
  
  
  d <- d[variable!=""]
  
  d[,var2:=NULL]
  
  d[, type := type]
  
  d[, waterwork := waterwork]
  d[, waterType := waterType]
  
  saveRDS(d, paste0("data_clean/WP1_waterworks/",fileOut))
  
  print(d[c(1,10)])
}

CleanWP1SpecificWaterWorkLongSingle <- function(fileIn,
                            fileOut,
                            sheet=1,
                            skip=8,
                            col_types=c("text",rep("text", 1)),
                            col_names=c("date",
                                        "value"
                                        ),
                            type="Online",
                            variable,
                            units="?",
                            waterwork,
                            waterType="Raw",
                            point="?"){


d <- data.table(readxl::read_excel(fileIn,
                                     sheet = sheet,
                                     skip = skip,
                                     col_names = FALSE,
                                     col_types = col_types))
  
  setnames(d, col_names)
  d <- d[, which(names(d) != "X"), with = F]
  if(class(d$date)=="character"){
    d[,date:=as.Date(stringr::str_sub(date,1,10))]
  }
  d <- na.omit(d)
  d <- d[value>=0.0]
  #d[, date := as.Date(date, format = "%d.%m.%Y")]
  
  d[, variable := variable]
  d[, units := units]
  
  d[, type := type]
  
  d[, waterwork := waterwork]
  d[, waterType := waterType]
  d[,point := point]
  
  saveRDS(d, paste0("data_clean/WP1_waterworks/",fileOut))
  
  print(d[c(1,10)])
}

CleanWP1SpecificWaterWorkLongOppegard <- function(fileIn="data_raw/WP1_waterworks/Oppegård/Rådata/aggregated.xlsx",
                                                  fileOut="oppegard.RDS",
                                      sheet=1,
                                      type="Accredited",
                                      waterwork="Oppegard",
                                      waterType="Raw"
                                      ){


d <- data.table(readxl::read_excel(fileIn,
                                     sheet = sheet,
                                     skip = 1,
                                     col_names = FALSE,
                                     col_types = c(rep("text",4),"text","text")))
  
  setnames(d, c("var2","value","units", "wtype", "X", "code"))
  d <- d[stringr::str_detect(wtype,"åvann")]
  d[,wtype:=NULL]
  d <- d[, which(names(d) != "X"), with = F]
  d[,dy:=stringr::str_sub(code,5,8)]
  d[,dm:=stringr::str_sub(code,10,11)]
  d[,dd:=stringr::str_sub(code,12,13)]
  d[,dx:=paste0(dy,"-",dm,"-",dd)]
  d[dx=="NA-NA-NA",dx:=NA]
  d[,date:=as.Date(dx)]
  d[,dy:=NULL]
  d[,dm:=NULL]
  d[,dd:=NULL]
  d[,dx:=NULL]
  d[,code:=NULL]
  d <- d[!is.na(date)]
  #d[, date := as.Date(date, format = "%d.%m.%Y")]
  
  d[, variable := ""]
  sort(unique(d$var2))
  d[var2 == "E. coli", variable := "E. Coli"]
  d[var2 == "Koliforme", variable := "Coliform bacteria"]
  d[var2 == "Kimtall 22°C", variable := "Colony count"]
  d[var2 == "Kimtall 22ºC", variable := "Colony count"]
  d[var2 == "Fargetall", variable := "Colour"]
  d[var2 == "Konduktivitet/ledningsevne", variable := "Conductivity"]
  d[var2 == "pH", variable := "pH"]
  d[var2 == "Intestinale enterokokker", variable := "Intestinal Enterococci"]
  d[var2 == "Turbiditet", variable := "Turbidity"]
  
  d <- d[variable!=""]
  
  d[,var2:=NULL]
  
  d[, type := type]
  
  d[, waterwork := waterwork]
  d[, waterType := waterType]
  d[, point:="?"]
  
  saveRDS(d, paste0("data_clean/WP1_waterworks/",fileOut))
  
  print(d[c(1,10)])
}


