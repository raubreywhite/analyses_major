
CleanWP1SpecificWaterWorkLong_Custom <- function(fileIn, 
                                                   sheet=1,
                                                 variables,
                                                 units=c("Conductivity"="mS/m",
                                                         "Turbidity"="FTU",
                                                         "Coliform bacteria"="/100ml",
                                                         "E. Coli"="/100ml",
                                                         "Colony count"="/ml",
                                                         "Intestinal Enterococci"="/100ml"),
                                                   type,
                                                   waterwork,
                                                   waterType,
                                                   point="?"
){
  
  
  d <- data.table(readxl::read_excel(fileIn,
                                     sheet = sheet,
                                     
                                     skip = 1,
                                     col_names = FALSE,
                                     col_types = rep("text", 4)))
  
  setnames(d, c("date", "X", "var2", "value"))
  print(unique(d$var2))
  d <- d[, which(names(d) != "X"), with = F]
  d[, datex := as.Date(date, format = "%d.%m.%Y")]
  d[,date:=as.Date(as.numeric(date),origin = "1899-12-30")]
  d[is.na(date),date:=datex]
  d[,datex:=NULL]
  #d[, date := as.Date(date, format = "%d.%m.%Y")]
  
  d[, variable := ""]
  unique(d$var2)
 
  d[, variable := ""]
  if(!is.null(variables)) for(i in 1:length(variables)){
    v = names(variables)[i]
    u = variables[i]
    d[var2==v, variable:=u]
  }
  
  d[, units := ""]
  if(!is.null(units)) for(i in 1:length(units)){
    v = names(units)[i]
    u = units[i]
    d[variable==v, units:=u]
  }
  
  d <- d[variable!=""]
  d <- d[!is.na(value)]
  d[,var2:=NULL]
  
  d[, type := type]
  
  d[, waterwork := waterwork]
  d[, waterType := waterType]
  d[, point:=point]
  
  return(d)
  
  
  print(d[c(1,5,10,11,15)])
}


CleanWP1SpecificWaterWorkLong_MapGraph_Internal <- function(d){
  d[, variable := ""]
  unique(d$var2)
  
  d[var2 == "01-Farge (mg/l Pt)" & units=="", units := "mg/l Pt"]
  d[var2 == "01-Farge (mg/l Pt)", variable := "Colour"]
  
  d[var2 == "01-Farge" & units=="", units := "mg/l Pt"]
  d[var2 == "01-Farge", variable := "Colour"]
  
  d[var2 == "04-Turbiditet (FNU)" & units=="", units := "FNU"]
  d[var2 == "04-Turbiditet (FNU)", variable := "Turbidity"]
  
  d[var2 == "04-Turbiditet" & units=="", units := "FNU"]
  d[var2 == "04-Turbiditet", variable := "Turbidity"]
  
  d[var2 == "05-Clostridium perfringens (/100ml)" & units=="", units := "/100ml"]
  d[var2 == "05-Clostridium perfringens (/100ml)", variable := "Clostridium Perfringens"]
  
  d[var2 == "05-Clostridium perfringens" & units=="", units := "/100ml"]
  d[var2 == "05-Clostridium perfringens", variable := "Clostridium Perfringens"]
  
  d[var2 == "06-E.coli (/100 ml)" & units=="", units:="/100ml"]
  d[var2 == "06-E.coli (/100 ml)", variable := "E. Coli"]
  
  d[var2 == "06-E.coli" & units=="", units:="/100ml"]
  d[var2 == "06-E.coli", variable := "E. Coli"]
  
  d[var2 == "07-Intestinale enterokokker (/100 ml)" & units=="", units := "/100ml"]
  d[var2 == "07-Intestinale enterokokker (/100 ml)", variable := "Intestinal Enterococci"]
  
  d[var2 == "07-Intestinale enterokokker" & units=="", units := "/100ml"]
  d[var2 == "07-Intestinale enterokokker", variable := "Intestinal Enterococci"]
  
  d[var2 == "08-Kimtall 22°C (/ml)" & units=="", units := "/ml"]
  d[var2 == "08-Kimtall 22°C (/ml)", variable := "Colony count at 22 degrees"]
  
  d[var2 == "08-Kimtall 22°C" & units=="", units := "/ml"]
  d[var2 == "08-Kimtall 22°C", variable := "Colony count at 22 degrees"]
  
  d[var2 == "09-Koliforme bakterier 37°C (/100 ml)" & units=="", units := "/100ml"]
  d[var2 == "09-Koliforme bakterier 37°C (/100 ml)", variable := "Coliform bacteria"]
  
  d[var2 == "09-Koliforme bakterier 37°C" & units=="", units := "/100ml"]
  d[var2 == "09-Koliforme bakterier 37°C", variable := "Coliform bacteria"]
  
  d[var2 == "35-Konduktivitet (mS/m)" & units=="", units := "mS/m"]
  d[var2 == "35-Konduktivitet (mS/m)", variable := "Conductivity"]
  
  d[var2 == "35-Konduktivitet" & units=="", units := "mS/m"]
  d[var2 == "35-Konduktivitet", variable := "Conductivity"]
  
  d[var2 == "44-pH, surhetsgrad ( )" & units=="", units := "pH"]
  d[var2 == "44-pH, surhetsgrad ( )", variable := "pH"]
  
  d[var2 == "44-pH, surhetsgrad" & units=="", units := "pH"]
  d[var2 == "44-pH, surhetsgrad", variable := "pH"]
  
  d[var2 == "53-Totalt organisk karbon (TOC) (mg/l C)" & units=="", units := "mg/l C"]
  d[var2 == "53-Totalt organisk karbon (TOC) (mg/l C)", variable := "TOC"]
  
  d <- d[variable!=""]
  d <- d[!is.na(value)]
  
  d[,var2:=NULL]
  return(d)
}

CleanWP1SpecificWaterWorkLong_MapGraph <- function(fileIn, 
                                                   sheet=1,
                                                   skip=1,
                                                   type,
                                                   waterwork,
                                                   waterType,
                                                   remove=NULL
){
  
  
  d <- data.table(readxl::read_excel(fileIn,
                                     sheet = sheet,
                                     skip = skip,
                                     col_names = FALSE,
                                     col_types = c("text","text","text",rep("text", 5))))
  
  setnames(d, c("valid","X","date", "point", "var2", "value", "units","X"))
  d <- d[valid=="Godkjent"]
  d[,valid:=NULL]
  d <- d[, which(names(d) != "X"), with = F]
  d[,date:=as.Date(as.numeric(date),origin = "1899-12-30")]
  #d[, date := as.Date(date, format = "%d.%m.%Y")]
  
  d <- CleanWP1SpecificWaterWorkLong_MapGraph_Internal(d)
  
  d[, type := type]
  
  d[, waterwork := waterwork]
  d[, waterType := waterType]
  
  return(d)
  
  
  print(d[c(1,5,10,11,15)])
}

CleanWP1SpecificWaterWorkLong_MapGraphMini <- function(fileIn, 
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
                                     col_types = rep("text", 6)))
  
  setnames(d, c("date", "point", "var2", "value", "units","X"))
  
  d <- d[, which(names(d) != "X"), with = F]
  d[,date:=as.Date(as.numeric(date),origin = "1899-12-30")]
  #d[, date := as.Date(date, format = "%d.%m.%Y")]
  
  d <- CleanWP1SpecificWaterWorkLong_MapGraph_Internal(d)
  
  d[, type := type]
  
  d[, waterwork := waterwork]
  d[, waterType := waterType]
  
  return(d)
  
  
  print(d[c(1,5,10,11,15)])
}

CleanWP1SpecificWaterWorkWide_MapGraph <- function(fileIn, 
                                                   sheet,
                                                   skip,
                                                   col_types,
                                                   col_names,
                                                   type,
                                                   units=NULL,
                                                   waterwork,
                                                   waterType
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
  
  try(d[,xdate:=as.Date(date)],TRUE)
  d[,date:=as.Date(as.numeric(date),origin = "1899-12-30")]
  try({
    d[is.na(date),date:=xdate]
    d[,xdate:=NULL]
  },TRUE)
  if(sum(names(d)=="point")==0) d[,point:="?"]
  d <- melt.data.table(d, id = c("point","date"), variable.factor = FALSE)
  d[, variable := gsub("[0-9]*$", "", variable)]
  d[,units:=""]
  setnames(d,"variable","var2")
  
  d <- CleanWP1SpecificWaterWorkLong_MapGraph_Internal(d)
  
  d[, type := type]
  
  d[, waterwork := waterwork]
  d[, waterType := waterType]
  
  return(d)
  
  print(d[c(1,5,10,11,15)])
}

CleanWP1SpecificWaterWorkWide <- function(fileIn,
                                      sheet,
                                      skip,
                                      col_types,
                                      col_names,
                                      type,
                                      units=NULL,
                                      waterwork,
                                      waterType,
                                      point="?",
                                      dateAsNumber=FALSE
){
  
  d <- data.table(readxl::read_excel(fileIn,
                                     sheet = sheet,
                                     skip = skip,
                                     col_names = FALSE,
                                     col_types = col_types))
  
  setnames(d, col_names)
  
  d <- d[, which(names(d) != "X"), with = F]
  
  if(dateAsNumber){
    d[,date:=as.Date(as.numeric(date),origin = "1899-12-30")]
  } else if(class(d$date[1])[1]=="character"){
    if(!is.na(as.Date(d$date[1],format="%d.%m.%Y"))){
      d[,date:=as.Date(date,format="%d.%m.%Y")]
    }
  }
  d[,date:=as.Date(date)]
  
  if(sum(names(d)=="point")==0){
    d <- melt.data.table(d, id = c("date"), variable.factor = FALSE)
  } else {
    d <- melt.data.table(d, id = c("date","point"), variable.factor = FALSE)
  }
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
  if(sum(names(d)=="point")==0){
    d[, point:=point]
  }
  
  d <- d[!is.na(date) & !is.na(value)]
  
  return(d)
}


CleanDataWaterworksCleanWaterInternal <- function(){
  #warning("# KEEP TOC")
  
  
  ## ALTA
  d <- CleanWP1SpecificWaterWorkLong_MapGraph(
    fileIn=file.path(org::PROJ$RAW,"WP1_clean_water/Alta_Elvestrand/Rådata/Rapport siste 9 år Alta vannverk.xlsx"), 
    sheet=1,
    type="Accredited",
    waterwork="Alta",
    waterType="Clean"
  )
  saveRDS(d, file.path(org::PROJ$CLEAN,"WP1_waterworks_clean_water","Rapport siste 9 år Alta vannverk.RDS"))
  
  ## ARENDAL RORE
  
  d <- CleanWP1SpecificWaterWorkWide_MapGraph(
    fileIn=file.path(
      org::PROJ$RAW,
      "WP1_clean_water",
      "Arendal_Rore",
      "Rådata",
      "Renvann",
      "MapGraphReport5792963950253546358.xlsx"), 
    sheet=1,
    skip=1,
    col_types=c("text","text",rep("text",8)),
    col_names=c(
      "point",
      "date",
      "01-Farge (mg/l Pt)",
      "04-Turbiditet (FNU)",
      "05-Clostridium perfringens (/100ml)",
      "06-E.coli (/100 ml)",
      "07-Intestinale enterokokker (/100 ml)",
      "08-Kimtall 22°C (/ml)",
      "09-Koliforme bakterier 37°C (/100 ml)",
      "44-pH, surhetsgrad ( )"
    ),
    type="Accredited",
    units=NULL,
    waterwork="Arendal",
    waterType="Clean"
  )
  d <- d[point=="02 Rentvann Hølen"]
  saveRDS(d, file.path(org::PROJ$CLEAN,"WP1_waterworks_clean_water","Arendal_Rore.RDS"))
  
  ##
  ## Asker og Bærum IKS
  ## Holsfjorden
  
  d <- CleanWP1SpecificWaterWorkWide(
    fileIn=file.path(
      org::PROJ$RAW,
      "WP1_clean_water",
      "Asker og Bærum vannverk IKS_Holsfjorden Kattås",
      "Rådata",
      "Renvann",
      "Historiske data fra 2006_Holsfjorden.xlsx"), 
    sheet=5,
    skip=4,
    col_types=c("text",rep("text", 8)),
    col_names=c(
      "date",
      "pH",
      "Turbidity",
      "Colour",
      "Conductivity",
      "Coliform bacteria",
      "E. Coli",
      "Colony count",
      "Intestinal Enterococci"
    ),
    type="Accredited",
    units=c("Conductivity"="mS/m",
            "Turbidity"="FTU",
            "Coliform bacteria"="/100ml",
            "E. Coli"="/100ml",
            "Colony count"="/ml",
            "Intestinal Enterococci"="/100ml"),
    waterwork="Asker og Bærum IKS",
    waterType="Clean",
    point="Holsfjorden",
    dateAsNumber=TRUE
  )
  saveRDS(d, file.path(org::PROJ$CLEAN,"WP1_waterworks_clean_water","Asker_og_Baerum_IKS_Holsfjorden_1.RDS"))
  
  d <- CleanWP1SpecificWaterWorkWide(
    fileIn=file.path(
      org::PROJ$RAW,
      "WP1_clean_water",
      "Asker og Bærum vannverk IKS_Holsfjorden Kattås",
      "Rådata",
      "Renvann",
      "Historiske data fra 2006_Holsfjorden.xlsx"), 
    sheet=6,
    skip=3,
    col_types=c("text",rep("text", 2)),
    col_names=c(
      "date",
      "Coliform bacteria",
      "E. Coli"
    ),
    type="Internal",
    units=c("Conductivity"="mS/m",
            "Turbidity"="FTU",
            "Coliform bacteria"="/100ml",
            "E. Coli"="/100ml",
            "Colony count"="/ml",
            "Intestinal Enterococci"="/100ml"),
    waterwork="Asker og Bærum IKS",
    waterType="Clean",
    point="Holsfjorden",
    dateAsNumber=TRUE
  )
  print(d)
  saveRDS(d, file.path(org::PROJ$CLEAN,"WP1_waterworks_clean_water","Asker_og_Baerum_IKS_Holsfjorden_2.RDS"))
  
  d <- CleanWP1SpecificWaterWorkWide(
    fileIn=file.path(
      org::PROJ$RAW,
      "WP1_clean_water",
      "Asker og Bærum vannverk IKS_Holsfjorden Kattås",
      "Rådata",
      "Renvann",
      "Historiske data fra 2006_Holsfjorden.xlsx"), 
    sheet=7,
    skip=4,
    col_types=c("text",rep("text", 1)),
    col_names=c(
      "date",
      "Colony count"
    ),
    type="Internal",
    units=c("Conductivity"="mS/m",
            "Turbidity"="FTU",
            "Coliform bacteria"="/100ml",
            "E. Coli"="/100ml",
            "Colony count"="/ml",
            "Intestinal Enterococci"="/100ml"),
    waterwork="Asker og Bærum IKS",
    waterType="Clean",
    point="Holsfjorden",
    dateAsNumber=TRUE
  )
  print(d)
  saveRDS(d, file.path(org::PROJ$CLEAN,"WP1_waterworks_clean_water","Asker_og_Baerum_IKS_Holsfjorden_3.RDS"))
  
  d <- CleanWP1SpecificWaterWorkWide(
    fileIn=file.path(
      org::PROJ$RAW,
      "WP1_clean_water",
      "Asker og Bærum vannverk IKS_Holsfjorden Kattås",
      "Rådata",
      "Renvann",
      "Historiske data fra 2006_Holsfjorden.xlsx"), 
    sheet=8,
    skip=3,
    col_types=c("text",rep("text", 1)),
    col_names=c(
      "date",
      "Turbidity"
    ),
    type="Online",
    units=c("Conductivity"="mS/m",
            "Turbidity"="FTU",
            "Coliform bacteria"="/100ml",
            "E. Coli"="/100ml",
            "Colony count"="/ml",
            "Intestinal Enterococci"="/100ml"),
    waterwork="Asker og Bærum IKS",
    waterType="Clean",
    point="Holsfjorden",
    dateAsNumber=TRUE
  )
  print(d)
  saveRDS(d, file.path(org::PROJ$CLEAN,"WP1_waterworks_clean_water","Asker_og_Baerum_IKS_Holsfjorden_4.RDS"))
  
  ##
  ## Asker og Bærum IKS
  ## Aurevann
  
  d <- CleanWP1SpecificWaterWorkWide(
    fileIn=file.path(
      org::PROJ$RAW,
      "WP1_clean_water",
      "Asker og Bærum vannverk IKS_Aurevann",
      "Rådata",
      "Renvann",
      "Historiske data fra 2006_Aurevann rentvann.xlsx"), 
    sheet=1,
    skip=6,
    col_types=c("text",rep("text", 5)),
    col_names=c(
      "date",
      "Coliform bacteria",
      "E. Coli",
      "Intestinal Enterococci",
      "Clostridium perfringens",
      "Colony count"
    ),
    type="Accredited",
    units=c("Conductivity"="mS/m",
            "Turbidity"="FTU",
            "Coliform bacteria"="/100ml",
            "E. Coli"="/100ml",
            "Colony count"="/ml",
            "Intestinal Enterococci"="/100ml"),
    waterwork="Asker og Bærum IKS",
    waterType="Clean",
    point="Aurevann",
    dateAsNumber=TRUE
  )
  print(d)
  saveRDS(d, file.path(org::PROJ$CLEAN,"WP1_waterworks_clean_water","Asker_og_Baerum_IKS_Aurevann_1.RDS"))
  
  d <- CleanWP1SpecificWaterWorkWide(
    fileIn=file.path(
      org::PROJ$RAW,
      "WP1_clean_water",
      "Asker og Bærum vannverk IKS_Aurevann",
      "Rådata",
      "Renvann",
      "Historiske data fra 2006_Aurevann rentvann.xlsx"), 
    sheet=2,
    skip=7,
    col_types=c("text",rep("text", 4)),
    col_names=c(
      "date",
      "pH",
      "Turbidity",
      "Colour",
      "Conductivity"
    ),
    type="Accredited",
    units=c(
      "Colour"="mg Pt/l",
      "Conductivity"="mS/m",
      "Turbidity"="FTU",
      "Coliform bacteria"="/100ml",
      "E. Coli"="/100ml",
      "Colony count"="/ml",
      "Intestinal Enterococci"="/100ml"),
    waterwork="Asker og Bærum IKS",
    waterType="Clean",
    point="Aurevann",
    dateAsNumber=TRUE
  )
  print(d)
  saveRDS(d, file.path(org::PROJ$CLEAN,"WP1_waterworks_clean_water","Asker_og_Baerum_IKS_Aurevann_2.RDS"))
  
  d <- CleanWP1SpecificWaterWorkWide(
    fileIn=file.path(
      org::PROJ$RAW,
      "WP1_clean_water",
      "Asker og Bærum vannverk IKS_Aurevann",
      "Rådata",
      "Renvann",
      "Historiske data fra 2006_Aurevann rentvann.xlsx"), 
    sheet=3,
    skip=6,
    col_types=c("text",rep("text", 10)),
    col_names=c(
      "date",
      "pH",
      "Turbidity",
      "Colour",
      "Conductivity",
      rep("X",6)
    ),
    type="Internal",
    units=c(
      "Colour"="mg Pt/l",
      "Conductivity"="mS/m",
      "Turbidity"="FTU",
      "Coliform bacteria"="/100ml",
      "E. Coli"="/100ml",
      "Colony count"="/ml",
      "Intestinal Enterococci"="/100ml"),
    waterwork="Asker og Bærum IKS",
    waterType="Clean",
    point="Aurevann",
    dateAsNumber=TRUE
  )
  print(d)
  saveRDS(d, file.path(org::PROJ$CLEAN,"WP1_waterworks_clean_water","Asker_og_Baerum_IKS_Aurevann_3.RDS"))
  
  d <- CleanWP1SpecificWaterWorkWide(
    fileIn=file.path(
      org::PROJ$RAW,
      "WP1_clean_water",
      "Asker og Bærum vannverk IKS_Aurevann",
      "Rådata",
      "Renvann",
      "Historiske data fra 2006_Aurevann rentvann.xlsx"), 
    sheet=4,
    skip=6,
    col_types=c("text",rep("text", 3)),
    col_names=c(
      "date",
      "Coliform bacteria",
      "E. Coli",
      "Colony count"
    ),
    type="Internal",
    units=c(
      "Colour"="mg Pt/l",
      "Conductivity"="mS/m",
      "Turbidity"="FTU",
      "Coliform bacteria"="/100ml",
      "E. Coli"="/100ml",
      "Colony count"="/ml",
      "Intestinal Enterococci"="/100ml"),
    waterwork="Asker og Bærum IKS",
    waterType="Clean",
    point="Aurevann",
    dateAsNumber=TRUE
  )
  print(d)
  saveRDS(d, file.path(org::PROJ$CLEAN,"WP1_waterworks_clean_water","Asker_og_Baerum_IKS_Aurevann_4.RDS"))
  
  d <- CleanWP1SpecificWaterWorkWide(
    fileIn=file.path(
      org::PROJ$RAW,
      "WP1_clean_water",
      "Asker og Bærum vannverk IKS_Aurevann",
      "Rådata",
      "Renvann",
      "Historiske data fra 2006_Aurevann rentvann.xlsx"), 
    sheet=5,
    skip=6,
    col_types=c("text",rep("text", 2)),
    col_names=c(
      "date",
      "Turbidity",
      "X"
    ),
    type="Online",
    units=c(
      "Colour"="mg Pt/l",
      "Conductivity"="mS/m",
      "Turbidity"="FTU",
      "Coliform bacteria"="/100ml",
      "E. Coli"="/100ml",
      "Colony count"="/ml",
      "Intestinal Enterococci"="/100ml"),
    waterwork="Asker og Bærum IKS",
    waterType="Clean",
    point="Aurevann",
    dateAsNumber=TRUE
  )
  print(d)
  saveRDS(d, file.path(org::PROJ$CLEAN,"WP1_waterworks_clean_water","Asker_og_Baerum_IKS_Aurevann_5.RDS"))
  
  ##
  ## Bergen_Espeland
  ## Espeland
  
  d <- CleanWP1SpecificWaterWorkWide(
    fileIn=file.path(
      org::PROJ$RAW,
      "WP1_clean_water",
      "Bergen_Espeland",
      "Rådata",
      "Renvann",
      "Klimaforsk rentv. Espeland.xlsx"), 
    sheet=1,
    skip=6,
    col_types=c("text",rep("text", 20)),
    col_names=c(
      "date",
      "pH",
      "Conductivity",
      "Turbidity",
      "Colour",
      "Colony count",
      "Coliform bacteria",
      "E. Coli",
      "Intestinal Enterococci",
      rep("X",12)
    ),
    type="Accredited",
    units=c("Conductivity"="mS/m",
            "Turbidity"="FTU",
            "Coliform bacteria"="/100ml",
            "E. Coli"="/100ml",
            "Colony count"="/ml",
            "Intestinal Enterococci"="/100ml"),
    waterwork="Bergen_Espeland",
    waterType="Clean",
    point="Espeland",
    dateAsNumber=TRUE
  )
  print(d)
  saveRDS(d, file.path(org::PROJ$CLEAN,"WP1_waterworks_clean_water","Bergen_Espeland_Espeland_1.RDS"))
  
  d <- CleanWP1SpecificWaterWorkWide(
    fileIn=file.path(
      org::PROJ$RAW,
      "WP1_clean_water",
      "Bergen_Espeland",
      "Rådata",
      "Renvann",
      "Klimaforsk rentv. Espeland.xlsx"), 
    sheet=1,
    skip=6,
    col_types=c("text",rep("text", 20)),
    col_names=c(
      rep("X",12),
      "date",
      "E. Coli",
      "Conductivity",
      "Coliform bacteria",
      "Colour",
      "Intestinal Enterococci",
      "Colony count",
      "pH",
      "Turbidity"
    ),
    type="Accredited",
    units=c("Conductivity"="mS/m",
            "Turbidity"="FTU",
            "Coliform bacteria"="/100ml",
            "E. Coli"="/100ml",
            "Colony count"="/ml",
            "Intestinal Enterococci"="/100ml"),
    waterwork="Bergen_Espeland",
    waterType="Clean",
    point="Espeland",
    dateAsNumber=TRUE
  )
  print(d)
  saveRDS(d, file.path(org::PROJ$CLEAN,"WP1_waterworks_clean_water","Bergen_Espeland_Espeland_2.RDS"))
  
  ##
  ## Bergen_Sædalen
  ## Saedalen
  
  d <- CleanWP1SpecificWaterWorkWide(
    fileIn=file.path(
      org::PROJ$RAW,
      "WP1_clean_water",
      "Bergen_Sædalen",
      "Rådata",
      "Renvann",
      "Klimaforsk rentv. Sædalen.xlsx"), 
    sheet=1,
    skip=4,
    col_types=c("text",rep("text", 20)),
    col_names=c(
      "date",
      "pH",
      "Conductivity",
      "Turbidity",
      "Colour",
      "Colony count",
      "Coliform bacteria",
      "E. Coli",
      "Intestinal Enterococci",
      rep("X",12)
    ),
    type="Accredited",
    units=c("Conductivity"="mS/m",
            "Turbidity"="FTU",
            "Coliform bacteria"="/100ml",
            "E. Coli"="/100ml",
            "Colony count"="/ml",
            "Intestinal Enterococci"="/100ml"),
    waterwork="Bergen_Saedalen",
    waterType="Clean",
    point="Saedalen",
    dateAsNumber=TRUE
  )
  print(d)
  saveRDS(d, file.path(org::PROJ$CLEAN,"WP1_waterworks_clean_water","Bergen_Saedalen_Saedalen_1.RDS"))
  
  d <- CleanWP1SpecificWaterWorkWide(
    fileIn=file.path(
      org::PROJ$RAW,
      "WP1_clean_water",
      "Bergen_Sædalen",
      "Rådata",
      "Renvann",
      "Klimaforsk rentv. Sædalen.xlsx"), 
    sheet=1,
    skip=4,
    col_types=c("text",rep("text", 20)),
    col_names=c(
      rep("X",12),
      "date",
      "E. Coli",
      "Conductivity",
      "Coliform bacteria",
      "Colour",
      "Intestinal Enterococci",
      "Colony count",
      "pH",
      "Turbidity"
    ),
    type="Accredited",
    units=c("Conductivity"="mS/m",
            "Turbidity"="FTU",
            "Coliform bacteria"="/100ml",
            "E. Coli"="/100ml",
            "Colony count"="/ml",
            "Intestinal Enterococci"="/100ml"),
    waterwork="Bergen_Saedalen",
    waterType="Clean",
    point="Saedalen",
    dateAsNumber=TRUE
  )
  print(d)
  saveRDS(d, file.path(org::PROJ$CLEAN,"WP1_waterworks_clean_water","Bergen_Saedalen_Saedalen_2.RDS"))
  
  d <- CleanWP1SpecificWaterWorkWide(
    fileIn=file.path(
      org::PROJ$RAW,
      "WP1_clean_water",
      "Bergen_Sædalen",
      "Rådata",
      "Renvann",
      "Saedalen rentvann online 2006-2016.xlsx"), 
    sheet=1,
    skip=6,
    col_types=c("text",rep("text", 2)),
    col_names=c(
      "date",
      "pH",
      "Turbidity"
    ),
    type="Online",
    units=c("Conductivity"="mS/m",
            "Turbidity"="FTU",
            "Coliform bacteria"="/100ml",
            "E. Coli"="/100ml",
            "Colony count"="/ml",
            "Intestinal Enterococci"="/100ml"),
    waterwork="Bergen_Saedalen",
    waterType="Clean",
    point="Saedalen",
    dateAsNumber=TRUE
  )
  print(d)
  saveRDS(d, file.path(org::PROJ$CLEAN,"WP1_waterworks_clean_water","Bergen_Saedalen_Saedalen_3.RDS"))
  
  ##
  ## Bergen_Svartediket
  ## Svartediket
  
  d <- CleanWP1SpecificWaterWorkWide(
    fileIn=file.path(
      org::PROJ$RAW,
      "WP1_clean_water",
      "Bergen_Svartediket",
      "Rådata",
      "Renvann",
      "Klimaforsk rentv. Svartediket.xlsx"), 
    sheet=1,
    skip=4,
    col_types=c("text",rep("text", 20)),
    col_names=c(
      "date",
      "pH",
      "Conductivity",
      "Turbidity",
      "Colour",
      "Colony count",
      "Coliform bacteria",
      "E. Coli",
      "Intestinal Enterococci",
      rep("X",12)
    ),
    type="Accredited",
    units=c("Conductivity"="mS/m",
            "Turbidity"="FTU",
            "Coliform bacteria"="/100ml",
            "E. Coli"="/100ml",
            "Colony count"="/ml",
            "Intestinal Enterococci"="/100ml"),
    waterwork="Bergen_Svartediket",
    waterType="Clean",
    point="Svartediket",
    dateAsNumber=TRUE
  )
  print(d)
  saveRDS(d, file.path(org::PROJ$CLEAN,"WP1_waterworks_clean_water","Bergen_Svartediket_Svartediket_1.RDS"))
  
  d <- CleanWP1SpecificWaterWorkWide(
    fileIn=file.path(
      org::PROJ$RAW,
      "WP1_clean_water",
      "Bergen_Svartediket",
      "Rådata",
      "Renvann",
      "Klimaforsk rentv. Svartediket.xlsx"), 
    sheet=1,
    skip=4,
    col_types=c("text",rep("text", 20)),
    col_names=c(
      rep("X",12),
      "date",
      "E. Coli",
      "Conductivity",
      "Coliform bacteria",
      "Colour",
      "Intestinal Enterococci",
      "Colony count",
      "pH",
      "Turbidity"
    ),
    type="Accredited",
    units=c("Conductivity"="mS/m",
            "Turbidity"="FTU",
            "Coliform bacteria"="/100ml",
            "E. Coli"="/100ml",
            "Colony count"="/ml",
            "Intestinal Enterococci"="/100ml"),
    waterwork="Bergen_Svartediket",
    waterType="Clean",
    point="Svartediket",
    dateAsNumber=TRUE
  )
  print(d)
  saveRDS(d, file.path(org::PROJ$CLEAN,"WP1_waterworks_clean_water","Bergen_Svartediket_Svartediket_2.RDS"))
  
  d <- CleanWP1SpecificWaterWorkWide(
    fileIn=file.path(
      org::PROJ$RAW,
      "WP1_clean_water",
      "Bergen_Svartediket",
      "Rådata",
      "Renvann",
      "Svartediket rentvann online 2006-2016.xls"), 
    sheet=1,
    skip=5,
    col_types=c("text",rep("text", 3)),
    col_names=c(
      "date",
      "pH",
      "Turbidity",
      "Conductivity"
    ),
    type="Online",
    units=c("Conductivity"="mS/m",
            "Turbidity"="FTU",
            "Coliform bacteria"="/100ml",
            "E. Coli"="/100ml",
            "Colony count"="/ml",
            "Intestinal Enterococci"="/100ml"),
    waterwork="Bergen_Svartediket",
    waterType="Clean",
    point="Svartediket",
    dateAsNumber=TRUE
  )
  print(d)
  saveRDS(d, file.path(org::PROJ$CLEAN,"WP1_waterworks_clean_water","Bergen_Svartediket_Svartediket_3.RDS"))
  
  ##
  ## Bergen_Kismul
  ## Kismul
  
  d <- CleanWP1SpecificWaterWorkWide(
    fileIn=file.path(
      org::PROJ$RAW,
      "WP1_clean_water",
      "Bergen_Kismul",
      "Rådata",
      "Renvann",
      "Klimaforsk rentv. Kismul.xlsx"), 
    sheet=1,
    skip=4,
    col_types=c("text",rep("text", 20)),
    col_names=c(
      "date",
      "pH",
      "Conductivity",
      "Turbidity",
      "Colour",
      "Colony count",
      "Coliform bacteria",
      "E. Coli",
      "Intestinal Enterococci",
      rep("X",12)
    ),
    type="Accredited",
    units=c("Conductivity"="mS/m",
            "Turbidity"="FTU",
            "Coliform bacteria"="/100ml",
            "E. Coli"="/100ml",
            "Colony count"="/ml",
            "Intestinal Enterococci"="/100ml"),
    waterwork="Bergen_Kismul",
    waterType="Clean",
    point="Kismul",
    dateAsNumber=TRUE
  )
  print(d)
  saveRDS(d, file.path(org::PROJ$CLEAN,"WP1_waterworks_clean_water","Bergen_Kismul_Kismul_1.RDS"))
  
  d <- CleanWP1SpecificWaterWorkWide(
    fileIn=file.path(
      org::PROJ$RAW,
      "WP1_clean_water",
      "Bergen_Kismul",
      "Rådata",
      "Renvann",
      "Klimaforsk rentv. Kismul.xlsx"), 
    sheet=1,
    skip=4,
    col_types=c("text",rep("text", 20)),
    col_names=c(
      rep("X",12),
      "date",
      "E. Coli",
      "Conductivity",
      "Coliform bacteria",
      "Colour",
      "Intestinal Enterococci",
      "Colony count",
      "pH",
      "Turbidity"
    ),
    type="Accredited",
    units=c("Conductivity"="mS/m",
            "Turbidity"="FTU",
            "Coliform bacteria"="/100ml",
            "E. Coli"="/100ml",
            "Colony count"="/ml",
            "Intestinal Enterococci"="/100ml"),
    waterwork="Bergen_Kismul",
    waterType="Clean",
    point="Kismul",
    dateAsNumber=TRUE
  )
  print(d)
  saveRDS(d, file.path(org::PROJ$CLEAN,"WP1_waterworks_clean_water","Bergen_Kismul_Kismul_2.RDS"))
  
  d <- CleanWP1SpecificWaterWorkWide(
    fileIn=file.path(
      org::PROJ$RAW,
      "WP1_clean_water",
      "Bergen_Kismul",
      "Rådata",
      "Renvann",
      "Kismul rentvann online 2006-2016.xls"), 
    sheet=1,
    skip=6,
    col_types=c("text",rep("text", 4)),
    col_names=c(
      "date",
      "X",
      "Turbidity",
      "X",
      "pH"
    ),
    type="Online",
    units=c("Conductivity"="mS/m",
            "Turbidity"="FTU",
            "Coliform bacteria"="/100ml",
            "E. Coli"="/100ml",
            "Colony count"="/ml",
            "Intestinal Enterococci"="/100ml"),
    waterwork="Bergen_Kismul",
    waterType="Clean",
    point="Kismul",
    dateAsNumber=TRUE
  )
  print(d)
  saveRDS(d, file.path(org::PROJ$CLEAN,"WP1_waterworks_clean_water","Bergen_Kismul_Kismul_3.RDS"))
  
  ##
  ## Bergen_Jordalsvatnet
  ## Jordalsvatnet
  
  d <- CleanWP1SpecificWaterWorkWide(
    fileIn=file.path(
      org::PROJ$RAW,
      "WP1_clean_water",
      "Bergen_Jordalsvatnet",
      "Rådata",
      "Renvann",
      "Klimaforsk rent. Jordalsvatnet.xlsx"), 
    sheet=1,
    skip=4,
    col_types=c("text",rep("text", 20)),
    col_names=c(
      "date",
      "pH",
      "Conductivity",
      "Turbidity",
      "Colour",
      "Colony count",
      "Coliform bacteria",
      "E. Coli",
      "Intestinal Enterococci",
      rep("X",12)
    ),
    type="Accredited",
    units=c("Conductivity"="mS/m",
            "Turbidity"="FTU",
            "Coliform bacteria"="/100ml",
            "E. Coli"="/100ml",
            "Colony count"="/ml",
            "Intestinal Enterococci"="/100ml"),
    waterwork="Bergen_Jordalsvatnet",
    waterType="Clean",
    point="Jordalsvatnet",
    dateAsNumber=TRUE
  )
  print(d)
  saveRDS(d, file.path(org::PROJ$CLEAN,"WP1_waterworks_clean_water","Bergen_Jordalsvatnet_Jordalsvatnet_1.RDS"))
  
  d <- CleanWP1SpecificWaterWorkWide(
    fileIn=file.path(
      org::PROJ$RAW,
      "WP1_clean_water",
      "Bergen_Jordalsvatnet",
      "Rådata",
      "Renvann",
      "Klimaforsk rent. Jordalsvatnet.xlsx"), 
    sheet=1,
    skip=4,
    col_types=c("text",rep("text", 20)),
    col_names=c(
      rep("X",12),
      "date",
      "E. Coli",
      "Conductivity",
      "Coliform bacteria",
      "Colour",
      "Intestinal Enterococci",
      "Colony count",
      "pH",
      "Turbidity"
    ),
    type="Accredited",
    units=c("Conductivity"="mS/m",
            "Turbidity"="FTU",
            "Coliform bacteria"="/100ml",
            "E. Coli"="/100ml",
            "Colony count"="/ml",
            "Intestinal Enterococci"="/100ml"),
    waterwork="Bergen_Jordalsvatnet",
    waterType="Clean",
    point="Jordalsvatnet",
    dateAsNumber=TRUE
  )
  print(d)
  saveRDS(d, file.path(org::PROJ$CLEAN,"WP1_waterworks_clean_water","Bergen_Jordalsvatnet_Jordalsvatnet_2.RDS"))
  
  d <- CleanWP1SpecificWaterWorkWide(
    fileIn=file.path(
      org::PROJ$RAW,
      "WP1_clean_water",
      "Bergen_Jordalsvatnet",
      "Rådata",
      "Renvann",
      "Jordalsvatnet rentvann online 2006-2016.xlsx"), 
    sheet=1,
    skip=6,
    col_types=c("text",rep("text", 3)),
    col_names=c(
      "date",
      "pH",
      "Conductivity",
      "Turbidity"
    ),
    type="Online",
    units=c("Conductivity"="mS/m",
            "Turbidity"="FTU",
            "Coliform bacteria"="/100ml",
            "E. Coli"="/100ml",
            "Colony count"="/ml",
            "Intestinal Enterococci"="/100ml"),
    waterwork="Bergen_Jordalsvatnet",
    waterType="Clean",
    point="Jordalsvatnet",
    dateAsNumber=TRUE
  )
  print(d)
  saveRDS(d, file.path(org::PROJ$CLEAN,"WP1_waterworks_clean_water","Bergen_Jordalsvatnet_Jordalsvatnet_3.RDS"))
  
  ##
  ## FREVAR_Høyfjell
  ## Accredited
  
  d <- CleanWP1SpecificWaterWorkWide(
    fileIn=file.path(
      org::PROJ$RAW,
      "WP1_clean_water",
      "FREVAR_Høyfjell",
      "Rådata",
      "Renvann",
      "Akkreditert",
      "combined.xlsx"), 
    sheet=1,
    skip=3,
    col_types=rep("text", 10),
    col_names=c(
      "date",
      "Coliform bacteria",
      "E. Coli",
      "Clostridium perfringens",
      "Intestinal Enterococci",
      "Colony count",
      "pH",
      "Conductivity",
      "Turbidity",
      "Colour"
    ),
    type="Accredited",
    units=c("Conductivity"="mS/m",
            "Turbidity"="FTU",
            "Coliform bacteria"="/100ml",
            "E. Coli"="/100ml",
            "Colony count"="/ml",
            "Intestinal Enterococci"="/100ml"),
    waterwork="FREVAR_Høyfjell",
    waterType="Clean",
    point="?",
    dateAsNumber=TRUE
  )
  print(d)
  saveRDS(d, file.path(org::PROJ$CLEAN,"WP1_waterworks_clean_water","FREVAR_Høyfjell_1.RDS"))
  
  ##
  ## FREVAR_Høyfjell
  ## online
  
  d <- CleanWP1SpecificWaterWorkWide(
    fileIn=file.path(
      org::PROJ$RAW,
      "WP1_clean_water",
      "FREVAR_Høyfjell",
      "Rådata",
      "Renvann",
      "Online_egenanalyser",
      "combined.xlsx"), 
    sheet=1,
    skip=3,
    col_types=rep("text", 16),
    col_names=c(
      "date",
      "X",
      "Turbidity",
      "X",
      "X",
      "X",
      "X",
      "X",
      "pH",
      "X",
      "X",
      "X",
      "X",
      "X",
      "Conductivity",
      "X"
    ),
    type="Online",
    units=c("Conductivity"="mS/m",
            "Turbidity"="FTU",
            "Coliform bacteria"="/100ml",
            "E. Coli"="/100ml",
            "Colony count"="/ml",
            "Intestinal Enterococci"="/100ml"),
    waterwork="FREVAR_Høyfjell",
    waterType="Clean",
    point="?",
    dateAsNumber=TRUE
  )
  print(d)
  saveRDS(d, file.path(org::PROJ$CLEAN,"WP1_waterworks_clean_water","FREVAR_Høyfjell_2.RDS"))
  
  ##
  ## Glitrevannverket IKS_Kleivdammen
  ## Accredited
  
  d <- CleanWP1SpecificWaterWorkWide(
    fileIn=file.path(
      org::PROJ$RAW,
      "WP1_clean_water",
      "Glitrevannverket IKS_Kleivdammen",
      "Rådata",
      "Renvann",
      "Glitre rå- og rentvann 2000-16.xlsx"), 
    sheet="Kleivdammen-Beh. vann",
    skip=1,
    col_types=rep("text", 36),
    col_names=c(
      "date",
      "Colour",
      "X",
      "X",
      "Turbidity",
      "X",
      "Conductivity",
      "pH",
      "X",
      "X",
      "X",
      "X",
      "X",
      "X",
      "X",
      "E. Coli",
      "Colony count",
      "Coliform bacteria",
      "Intestinal Enterococci",
      "X",
      "Clostridium perfringens",
      "X",
      "X",
      "X",
      "X",
      "X",
      "X",
      "X",
      "X",
      "X",
      "X",
      "X",
      "X",
      "X",
      "X",
      "X"
    ),
    type="Accredited",
    units=c("Conductivity"="mS/m",
            "Turbidity"="FTU",
            "Coliform bacteria"="/100ml",
            "E. Coli"="/100ml",
            "Colony count"="/ml",
            "Intestinal Enterococci"="/100ml"),
    waterwork="Glitrevannverket",
    waterType="Clean",
    point="Kleivdammen",
    dateAsNumber=TRUE
  )
  print(d)
  saveRDS(d, file.path(org::PROJ$CLEAN,"WP1_waterworks_clean_water","Glitrevannverket_IKS_Kleivdammen.RDS"))
  
  ##
  ## Glitrevannverket IKS_Landfall
  ## Accredited
  
  d <- CleanWP1SpecificWaterWorkWide(
    fileIn=file.path(
      org::PROJ$RAW,
      "WP1_clean_water",
      "Glitrevannverket IKS_Landfall",
      "Rådata",
      "Renvann",
      "Glitre rå- og rentvann 2000-16.xlsx"), 
    sheet=1,
    skip=1,
    col_types=rep("text", 10),
    col_names=c(
      "date",
      "Colony count",
      "Turbidity",
      "Colour",
      "pH",
      "E. Coli",
      "Coliform bacteria",
      "Intestinal Enterococci",
      "Conductivity",
      "Clostridium perfringens"
    ),
    type="Accredited",
    units=c("Conductivity"="mS/m",
            "Turbidity"="FTU",
            "Coliform bacteria"="/100ml",
            "E. Coli"="/100ml",
            "Colony count"="/ml",
            "Intestinal Enterococci"="/100ml"),
    waterwork="Glitrevannverket",
    waterType="Clean",
    point="Landfall",
    dateAsNumber=TRUE
  )
  print(d)
  saveRDS(d, file.path(org::PROJ$CLEAN,"WP1_waterworks_clean_water","Glitrevannverket_IKS_Landfall.RDS"))
  
  
  ##
  ## Halden_Lille Erte
  ## Internal
  
  d <- CleanWP1SpecificWaterWorkWide(
    fileIn=file.path(
      org::PROJ$RAW,
      "WP1_clean_water",
      "Halden_Lille Erte",
      "Rådata",
      "Renvann",
      "combined.xlsx"), 
    sheet=1,
    skip=3,
    col_types=rep("text", 15),
    col_names=c(
      "date",
      "X",
      "X",
      "X",
      "X",
      "Colour",
      "X",
      "Turbidity",
      "X",
      "X",
      "pH",
      "X",
      "Colony count",
      "Coliform bacteria",
      "X"
    ),
    type="Internal",
    units=c("Conductivity"="mS/m",
            "Turbidity"="FTU",
            "Coliform bacteria"="/100ml",
            "E. Coli"="/100ml",
            "Colony count"="/ml",
            "Intestinal Enterococci"="/100ml"),
    waterwork="Halden",
    waterType="Clean",
    point="Lille_Erte",
    dateAsNumber=TRUE
  )
  print(d)
  saveRDS(d, file.path(org::PROJ$CLEAN,"WP1_waterworks_clean_water","Halden_Lille Erte.RDS"))
  
  ##
  ## HIAS_Hamar
  ## Internal
  
  d <- CleanWP1SpecificWaterWorkLong_MapGraphMini(
    fileIn=file.path(
      org::PROJ$RAW,
      "WP1_clean_water",
      "HIAS_Hamar",
      "Rådata",
      "Renvann",
      "FHI 2016.xls.xlsx"), 
    sheet=1,
    type="Accredited",
    waterwork="HIAS_Hamar",
    waterType="Clean"
  )
  print(d)
  saveRDS(d, file.path(org::PROJ$CLEAN,"WP1_waterworks_clean_water","HIAS_Hamar_1.RDS"))
  
  d <- CleanWP1SpecificWaterWorkWide(
    fileIn=file.path(
      org::PROJ$RAW,
      "WP1_clean_water",
      "HIAS_Hamar",
      "Rådata",
      "Renvann",
      "FHI 141116.xls (2).xlsx"), 
    sheet=1,
    skip=1,
    col_types=rep("text", 4),
    col_names=c(
      "date",
      "X",
      "X",
      "Turbidity"
    ),
    type="Online",
    units=c("Conductivity"="mS/m",
            "Turbidity"="FTU",
            "Coliform bacteria"="/100ml",
            "E. Coli"="/100ml",
            "Colony count"="/ml",
            "Intestinal Enterococci"="/100ml"),
    waterwork="HIAS_Hamar",
    waterType="Clean",
    point="HVBA Rentvann",
    dateAsNumber=TRUE
  )
  print(d)
  saveRDS(d, file.path(org::PROJ$CLEAN,"WP1_waterworks_clean_water","HIAS_Hamar_2.RDS"))
  
  ##
  ## HIAS_Stange
  ## Internal
  
  d <- CleanWP1SpecificWaterWorkLong_MapGraphMini(
    fileIn=file.path(
      org::PROJ$RAW,
      "WP1_clean_water",
      "HIAS_Hamar",
      "Rådata",
      "Renvann",
      "FHI 2016.xls.xlsx"), 
    sheet=3,
    type="Accredited",
    waterwork="HIAS_Stange",
    waterType="Clean"
  )
  print(d)
  saveRDS(d, file.path(org::PROJ$CLEAN,"WP1_waterworks_clean_water","HIAS_Stange_1.RDS"))
  
  d <- CleanWP1SpecificWaterWorkWide(
    fileIn=file.path(
      org::PROJ$RAW,
      "WP1_clean_water",
      "HIAS_Hamar",
      "Rådata",
      "Renvann",
      "FHI 141116.xls (2).xlsx"), 
    sheet=2,
    skip=1,
    col_types=rep("text", 4),
    col_names=c(
      "date",
      "X",
      "X",
      "Turbidity"
    ),
    type="Online",
    units=c("Conductivity"="mS/m",
            "Turbidity"="FTU",
            "Coliform bacteria"="/100ml",
            "E. Coli"="/100ml",
            "Colony count"="/ml",
            "Intestinal Enterococci"="/100ml"),
    waterwork="HIAS_Stange",
    waterType="Clean",
    point="Hemstad Rentvann",
    dateAsNumber=TRUE
  )
  print(d)
  saveRDS(d, file.path(org::PROJ$CLEAN,"WP1_waterworks_clean_water","HIAS_Stange_2.RDS"))
  
  ##
  ## IVAR IKS_Langevatn
  ##
  
  d <- CleanWP1SpecificWaterWorkWide(
    fileIn=file.path(
      org::PROJ$RAW,
      "WP1_clean_water",
      "IVAR IKS_Langevatn",
      "Rådata",
      "Renvann",
      "Kopi av Langevatn renvann 2010-okt2016.xlsx"), 
    sheet=1,
    skip=8,
    col_types=rep("text", 22),
    col_names=c(
      "date",
      "Colony count",
      "Coliform bacteria",
      "E. Coli",
      "Clostridium perfringens",
      "Intestinal Enterococci",
      "X",
      "X",
      "Turbidity",
      "Colour",
      "pH",
      "X",
      "X",
      "X",
      "X",
      "X",
      "X",
      "X",
      "Conductivity",
      "X",
      "X",
      "X"
    ),
    type="Accredited",
    units=c("Conductivity"="mS/m",
            "Turbidity"="FTU",
            "Coliform bacteria"="/100ml",
            "E. Coli"="/100ml",
            "Colony count"="/ml",
            "Intestinal Enterococci"="/100ml"),
    waterwork="IVAR IKS",
    waterType="Clean",
    point="Langevatn",
    dateAsNumber=TRUE
  )
  print(d)
  saveRDS(d, file.path(org::PROJ$CLEAN,"WP1_waterworks_clean_water","IVAR IKS_Langevatn.RDS"))
  
  ##
  ## Karmøy_Brekke
  ##
  
  d <- CleanWP1SpecificWaterWorkLong_Custom(
    fileIn=file.path(
      org::PROJ$RAW,
      "WP1_clean_water",
      "Karmøy_Brekke",
      "Rådata",
      "Renvann",
      "Renvannsdata 2006.xlsx"), 
    sheet=1,
    variables=c(
      "Kimtall 22grC, 3 døgn"="Colony count",
      "Koliforme bakterier"="Coliform bacteria",
      "E. coli"="E. Coli",
      "Intestinale enterokokker"="Intestinal Enterococci",
      "pH (Surhetsgrad)"="pH",
      "Konduktivitet"="Conductivity",
      "Turbiditet"="Turbidity",
      "Farge"="Colour"
      ),
    type="Accredited",
    units=c("Conductivity"="mS/m",
            "Turbidity"="FTU",
            "Coliform bacteria"="/100ml",
            "E. Coli"="/100ml",
            "Colony count"="/ml",
            "Intestinal Enterococci"="/100ml"),
    waterwork="Karmøy",
    waterType="Clean",
    point="Brekke"
  )
  print(d)
  saveRDS(d, file.path(org::PROJ$CLEAN,"WP1_waterworks_clean_water","Karmøy_Brekke.RDS"))
  
  ##
  ## Kongsvinger_Granli_GIVAS
  ##
  
  d <- CleanWP1SpecificWaterWorkWide_MapGraph(
    fileIn=file.path(
      org::PROJ$RAW,
      "WP1_clean_water",
      "Kongsvinger_Granli_GIVAS",
      "Rådata",
      "Renvann",
      "MapGraphReport2573335470019199181.xlsx"), 
    sheet=1,
    skip=1,
    col_types=rep("text",10),
    col_names=c(
      "date",
      "08-Kimtall 22°C",
      "09-Koliforme bakterier 37°C",
      "04-Turbiditet",
      "01-Farge",
      "07-Intestinale enterokokker",
      "05-Clostridium perfringens",
      "44-pH, surhetsgrad",
      "35-Konduktivitet",
      "06-E.coli"
    ),
    type="Accredited",
    units=NULL,
    waterwork="Kongsvinger",
    waterType="Clean"
  )
  d[,point:="Granli"]

  saveRDS(d, file.path(org::PROJ$CLEAN,"WP1_waterworks_clean_water","Kongsvinger_Granli_GIVAS.RDS"))
  
  ##
  ## Kristiansand - Rossevann
  ##
  
  d <- CleanWP1SpecificWaterWorkLong_MapGraph(
    fileIn=file.path(org::PROJ$RAW,"WP1_clean_water/Kristiansand_Rossevann/Rådata/Renvann/Rossevann + Tronstadvann - Rentvann 2006 - 2016.xlsx"), 
    sheet=1,
    skip=8,
    type="Accredited",
    waterwork="Kristiansand",
    waterType="Clean"
  )
  d[,point:="Rossevann"]
  d <- d[date>=as.Date("2010-01-01")]
  saveRDS(d, file.path(org::PROJ$CLEAN,"WP1_waterworks_clean_water","Kristiansand_Rossevann.RDS"))
  
  ##
  ## Kristiansand - Tronstadvann
  ##
  
  d <- CleanWP1SpecificWaterWorkLong_MapGraph(
    fileIn=file.path(org::PROJ$RAW,"WP1_clean_water/Kristiansand_Rossevann/Rådata/Renvann/Rossevann + Tronstadvann - Rentvann 2006 - 2016.xlsx"), 
    sheet=2,
    skip=8,
    type="Accredited",
    waterwork="Kristiansand",
    waterType="Clean"
  )
  d[,point:="Tronstadvann"]
  
  saveRDS(d, file.path(org::PROJ$CLEAN,"WP1_waterworks_clean_water","Kristiansand_Tronstadvann.RDS"))

  ## Lillehammer
  
  d <- CleanWP1SpecificWaterWorkWide(
    fileIn=file.path(
      org::PROJ$RAW,
      "WP1_clean_water",
      "Lillehammer",
      "Rådata",
      "Renvann",
      "Renvann_all.xlsx"), 
    sheet=1,
    skip=3,
    col_types=rep("text",9),
    col_names=c(
      "X",
      "date",
      "X",
      "point",
      "E. Coli",
      "Intestinal Enterococci",
      "Colony count",
      "Coliform bacteria",
      "Turbidity"
    ),
    type="Accredited",
    waterwork="Lillehammer",
    waterType="Clean",
    dateAsNumber=TRUE
  )
  d <- d[point=="LILLEHAMMER-KORGEN-RENVANN"]
  d[,point:="Lillehammer"]
  saveRDS(d, file.path(org::PROJ$CLEAN,"WP1_waterworks_clean_water","Lillehammer_1.RDS"))
  
  ## Lillehammer_2
  
  d <- CleanWP1SpecificWaterWorkLong_Custom(
    fileIn=file.path(
      org::PROJ$RAW,
      "WP1_clean_water",
      "Lillehammer",
      "Rådata",
      "Renvann",
      "Kopi av Rentvann Korgen 2006 - 2010.xlsx"), 
    sheet=1,
    variables=c(
      "E. Coli - Colilert"="E. Coli",
      "Totalant.bakterier 22°C"="Colony count",
      "Koliforme bakterier - Colilert"="Coliform bacteria",
      "Intestinale enterokokker"="Intestinal Enterococci",
      "Surhetsgrad (pH)"="pH",
      "Konduktivitet  25°C"="Conductivity",
      "Turbiditet"="Turbidity"
    ),
    type="Accredited",
    units=c("Conductivity"="mS/m",
            "Turbidity"="FTU",
            "Coliform bacteria"="/100ml",
            "E. Coli"="/100ml",
            "Colony count"="/ml",
            "Intestinal Enterococci"="/100ml"),
    waterwork="Lillehammer",
    waterType="Clean",
    point="Lillehammer"
  )
  print(d)
  saveRDS(d, file.path(org::PROJ$CLEAN,"WP1_waterworks_clean_water","Lillehammer_2.RDS"))

  ## movar vansjø
  
  d <- CleanWP1SpecificWaterWorkWide(
    fileIn=file.path(
      org::PROJ$RAW,
      "WP1_clean_water",
      "MOVAR_Vansjø",
      "Rådata",
      "Renvann",
      "combined.xlsx"), 
    sheet=1,
    skip=1,
    col_types=rep("text",9),
    col_names=c(
      "date",
      "Turbidity",
      "pH",
      "Conductivity",
      "Colour",
      "X",
      "X",
      "X",
      "X"
    ),
    type="Internal",
    waterwork="Movar",
    waterType="Clean",
    dateAsNumber=TRUE,
    point="Vansjø"
  )
  
  saveRDS(d, file.path(org::PROJ$CLEAN,"WP1_waterworks_clean_water","movar_vansjø.RDS"))
  
  ## oppegård
  
  d <- CleanWP1SpecificWaterWorkLong_Custom(
    fileIn=file.path(
      org::PROJ$RAW,
      "WP1_clean_water",
      "Oppegård",
      "Rådata",
      "Renvann",
      "Oppegård kommune- rentvannsdata- fhi.xlsx"), 
    sheet=1,
    variables=c(
      "Turbiditet"="Turbidity",
      "Fargetall"="Colour",
      "Kimtall 22°C "="Colony count",
      "Koliforme"="Coliform bacteria",
      "Clostridium perfringens"="Clostridium perfringens",
      "Koliforme - E. coli  "="E. Coli",
      "Konduktivitet/ledningsevne"="Conductivity",
      "Intestinale enterokokker"="Intestinal Enterococci",
      "pH"="pH"
    ),
    type="Accredited",
    units=c("Conductivity"="mS/m",
            "Turbidity"="FTU",
            "Coliform bacteria"="/100ml",
            "E. Coli"="/100ml",
            "Colony count"="/ml",
            "Intestinal Enterococci"="/100ml"),
    waterwork="Oppegård",
    waterType="Clean"
  )
  print(d)
  saveRDS(d, file.path(org::PROJ$CLEAN,"WP1_waterworks_clean_water","Oppegård.RDS"))
  
  ## Oslo_Oset 1
  
  d <- CleanWP1SpecificWaterWorkWide(
    fileIn=file.path(
      org::PROJ$RAW,
      "WP1_clean_water",
      "Oslo_Oset",
      "Rådata",
      "Renvann",
      "bacteria.xlsx"), 
    sheet=1,
    skip=2,
    col_types=rep("text",7),
    col_names=c(
      "date",
      "X",
      "Colony count",
      "Coliform bacteria",
      "E. Coli",
      "Intestinal Enterococci",
      "Clostridium perfringens"
    ),
    type="External",
    waterwork="Oslo",
    waterType="Clean",
    dateAsNumber=TRUE,
    point="Oset"
  )
  print(d)
  saveRDS(d, file.path(org::PROJ$CLEAN,"WP1_waterworks_clean_water","Oslo_Oset_1.RDS"))
  
  ## Oslo_Oset 2
  
  d <- CleanWP1SpecificWaterWorkWide(
    fileIn=file.path(
      org::PROJ$RAW,
      "WP1_clean_water",
      "Oslo_Oset",
      "Rådata",
      "Renvann",
      "bacteria.xlsx"), 
    sheet=2,
    skip=2,
    col_types=rep("text",5),
    col_names=c(
      "date",
      "pH",
      "Conductivity",
      "Colour",
      "Turbidity"
    ),
    type="External",
    waterwork="Oslo",
    waterType="Clean",
    dateAsNumber=TRUE,
    point="Oset"
  )
  print(d)
  saveRDS(d, file.path(org::PROJ$CLEAN,"WP1_waterworks_clean_water","Oslo_Oset_2.RDS"))
  
  ## Oslo_Skullerud 1
  
  d <- CleanWP1SpecificWaterWorkWide(
    fileIn=file.path(
      org::PROJ$RAW,
      "WP1_clean_water",
      "Oslo_Skullerud",
      "Rådata",
      "Renvann",
      "compiled.xlsx"), 
    sheet=1,
    skip=2,
    col_types=rep("text",6),
    col_names=c(
      "date",
      "Colony count",
      "Coliform bacteria",
      "E. Coli",
      "Intestinal Enterococci",
      "Clostridium perfringens"
    ),
    type="External",
    waterwork="Oslo",
    waterType="Clean",
    dateAsNumber=TRUE,
    point="Skullerud"
  )
  print(d)
  saveRDS(d, file.path(org::PROJ$CLEAN,"WP1_waterworks_clean_water","Oslo_Skullerud_1.RDS"))
  
  ## Oslo_Oset 2
  
  d <- CleanWP1SpecificWaterWorkWide(
    fileIn=file.path(
      org::PROJ$RAW,
      "WP1_clean_water",
      "Oslo_Skullerud",
      "Rådata",
      "Renvann",
      "compiled.xlsx"), 
    sheet=2,
    skip=2,
    col_types=rep("text",5),
    col_names=c(
      "date",
      "pH",
      "Conductivity",
      "Colour",
      "Turbidity"
    ),
    type="External",
    waterwork="Oslo",
    waterType="Clean",
    dateAsNumber=TRUE,
    point="Skullerud"
  )
  print(d)
  saveRDS(d, file.path(org::PROJ$CLEAN,"WP1_waterworks_clean_water","Oslo_Skullerud_2.RDS"))
  
  ## Sarpsborg_Baterød
  
  d <- CleanWP1SpecificWaterWorkLong_Custom(
    fileIn=file.path(
      org::PROJ$RAW,
      "WP1_clean_water",
      "Sarpsborg_Baterød",
      "Rådata",
      "Renvann",
      "reformat.xlsx"), 
    sheet=1,
    variables=c(
      "Clostridium perfringens,  MF"="Clostridium perfringens",
      "Intestinale enterok. 37°,2d MF"="Intestinal Enterococci",
      "E.coli, Colilert 18"="E. Coli",
      "Kimtall,22°C,  3d"="Colony count",
      "Koliforme bakt.   Colilert 18"="Coliform bacteria",
      "Konduktivitet"="Conductivity",
      "pH i vann"="pH",
      "Turbiditet"="Turbidity",
      "Fargetall"="Colour"
    ),
    type="Accredited",
    units=c("Conductivity"="mS/m",
            "Turbidity"="FTU",
            "Coliform bacteria"="/100ml",
            "E. Coli"="/100ml",
            "Colony count"="/ml",
            "Intestinal Enterococci"="/100ml"),
    waterwork="Sarpsborg_Baterød",
    waterType="Clean"
  )
  print(d)
  saveRDS(d, file.path(org::PROJ$CLEAN,"WP1_waterworks_clean_water","Sarpsborg_Baterød.RDS"))
  
  ## Tromsø_Kvaløya
  
  d <- CleanWP1SpecificWaterWorkLong_MapGraphMini(
    fileIn=file.path(
      org::PROJ$RAW,
      "WP1_clean_water",
      "Tromsø_Kvaløya",
      "Rådata",
      "Renvann",
      "Simavik vv og Kvaløya vv rentvann analyseresultater (2006 - 2016).xlsx"), 
    sheet=1,
    type="Accredited",
    waterwork="Tromsø_Kvaløya",
    waterType="Clean"
  )
  print(d)
  d <- d[point=="0103 Simavik Rentvann"]
  saveRDS(d, file.path(org::PROJ$CLEAN,"WP1_waterworks_clean_water","Tromsø_Kvaløya.RDS"))
  
  ## Tromsø_Kvaløya
  
  d <- CleanWP1SpecificWaterWorkLong_MapGraphMini(
    fileIn=file.path(
      org::PROJ$RAW,
      "WP1_clean_water",
      "Tromsø_Simavik",
      "Rådata",
      "Renvann",
      "Simavik vv og Kvaløya vv rentvann analyseresultater (2006 - 2016).xlsx"), 
    sheet=1,
    type="Accredited",
    waterwork="Tromsø_Kvaløya",
    waterType="Clean"
  )
  print(d)
  d <- d[point=="0202 Kvaløya Rentvann"]
  saveRDS(d, file.path(org::PROJ$CLEAN,"WP1_waterworks_clean_water","Tromsø_Simavik.RDS"))
  
  ## Trondheim_Vikelvdalen
  
  d <- CleanWP1SpecificWaterWorkWide(
    fileIn=file.path(
      org::PROJ$RAW,
      "WP1_clean_water",
      "Trondheim_Vikelvdalen",
      "Rådata",
      "Renvann",
      "Viva behandlet vann.xlsx"), 
    sheet=1,
    skip=2,
    col_types=rep("text", 10),
    col_names=c(
      "date",
      "Clostridium perfringens",
      "E. Coli",
      "Colour",
      "Intestinal Enterococci",
      "Coliform bacteria",
      "Conductivity",
      "Colony count",
      "pH",
      "Turbidity"
    ),
    type="Accredited",
    units=c("Conductivity"="mS/m",
            "Turbidity"="FTU",
            "Coliform bacteria"="/100ml",
            "E. Coli"="/100ml",
            "Colony count"="/ml",
            "Intestinal Enterococci"="/100ml"),
    waterwork="Trondheim",
    waterType="Clean",
    point="Vikelvdalen",
    dateAsNumber=TRUE
  )
  print(d)
  saveRDS(d, file.path(org::PROJ$CLEAN,"WP1_waterworks_clean_water","Trondheim_Vikelvdalen.RDS"))
  
  ## Univann_Sjunken
  d <- CleanWP1SpecificWaterWorkLong_MapGraph(
    fileIn=file.path(
      org::PROJ$RAW,
      "WP1_clean_water",
      "Univann_Sjunken",
      "Rådata",
      "Renvann",
      "Analyseprøver Ullensaker vannverk og Univann behandlingsanlegg 2006 - 2016.xlsx"), 
    sheet=4,
    type="Accredited",
    waterwork="Univann_Sjunken",
    waterType="Clean"
  )
  print(d)
  saveRDS(d, file.path(org::PROJ$CLEAN,"WP1_waterworks_clean_water","Univann_Sjunken.RDS"))
  
  ## Vestfold Vann IKS_Eidsfoss 1
  
  d <- CleanWP1SpecificWaterWorkWide(
    fileIn=file.path(
      org::PROJ$RAW,
      "WP1_clean_water",
      "Vestfold Vann IKS_Eidsfoss",
      "Rådata",
      "Renvann",
      "Eidsfoss rentvann - analyser fra hovedlab.xlsx"), 
    sheet=1,
    skip=5,
    col_types=rep("text",10),
    col_names=c(
      "date",
      "Colony count",
      "Coliform bacteria",
      "E. Coli",
      "Intestinal Enterococci",
      "pH",
      "Turbidity",
      "Colour",
      "Clostridium perfringens",
      "Conductivity"
    ),
    type="External",
    waterwork="Vestfold",
    waterType="Clean",
    dateAsNumber=TRUE,
    point="Eidsfoss"
  )
  print(d)
  saveRDS(d, file.path(org::PROJ$CLEAN,"WP1_waterworks_clean_water","Vestfold Vann IKS_Eidsfoss_1.RDS"))
  
  
  ## Vestfold Vann IKS_Eidsfoss 2
  
  d <- CleanWP1SpecificWaterWorkWide(
    fileIn=file.path(
      org::PROJ$RAW,
      "WP1_clean_water",
      "Vestfold Vann IKS_Eidsfoss",
      "Rådata",
      "Renvann",
      "2-jm-Labjournal Eidsfoss egne prøver_2009_renvann.xlsx"), 
    sheet=1,
    skip=6,
    col_types=rep("text",8),
    col_names=c(
      "date",
      "Coliform bacteria",
      "E. Coli",
      "Colony count",
      "pH",
      "Colour",
      "Turbidity",
      "Conductivity"
    ),
    type="Internal",
    waterwork="Vestfold",
    waterType="Clean",
    dateAsNumber=TRUE,
    point="Eidsfoss"
  )
  print(d)
  saveRDS(d, file.path(org::PROJ$CLEAN,"WP1_waterworks_clean_water","Vestfold Vann IKS_Eidsfoss_2.RDS"))
  
  
  ## Vestfold Vann IKS_Seierstad 1
  
  d <- CleanWP1SpecificWaterWorkWide(
    fileIn=file.path(
      org::PROJ$RAW,
      "WP1_clean_water",
      "Vestfold Vann IKS_Seierstad",
      "Rådata",
      "Renvann",
      "Seierstad rentvann - analyser fra hovedlab.xlsx"), 
    sheet=1,
    skip=5,
    col_types=rep("text",10),
    col_names=c(
      "date",
      "Colony count",
      "Coliform bacteria",
      "E. Coli",
      "Intestinal Enterococci",
      "pH",
      "Turbidity",
      "Colour",
      "Clostridium perfringens",
      "Conductivity"
    ),
    type="External",
    waterwork="Vestfold",
    waterType="Clean",
    dateAsNumber=TRUE,
    point="Seierstad"
  )
  print(d)
  saveRDS(d, file.path(org::PROJ$CLEAN,"WP1_waterworks_clean_water","Vestfold Vann IKS_Seierstad_1.RDS"))
  
  
  ## Vestfold Vann IKS_Eidsfoss 2
  
  d <- CleanWP1SpecificWaterWorkWide(
    fileIn=file.path(
      org::PROJ$RAW,
      "WP1_clean_water",
      "Vestfold Vann IKS_Seierstad",
      "Rådata",
      "Renvann",
      "compiled.xlsx"), 
    sheet=1,
    skip=5,
    col_types=rep("text",5),
    col_names=c(
      "date",
      "pH",
      "Colour",
      "Turbidity",
      "Conductivity"
    ),
    type="Internal",
    waterwork="Vestfold",
    waterType="Clean",
    dateAsNumber=TRUE,
    point="Seierstad"
  )
  print(d)
  saveRDS(d, file.path(org::PROJ$CLEAN,"WP1_waterworks_clean_water","Vestfold Vann IKS_Seierstad_2.RDS"))
  
  ## Vestfold Vann IKS_Eidsfoss 3
  
  d <- CleanWP1SpecificWaterWorkWide(
    fileIn=file.path(
      org::PROJ$RAW,
      "WP1_clean_water",
      "Vestfold Vann IKS_Seierstad",
      "Rådata",
      "Renvann",
      "compiled.xlsx"), 
    sheet=2,
    skip=4,
    col_types=rep("text",4),
    col_names=c(
      "date",
      "Coliform bacteria",
      "E. Coli",
      "Colony count"
    ),
    type="Internal",
    waterwork="Vestfold",
    waterType="Clean",
    dateAsNumber=TRUE,
    point="Seierstad"
  )
  print(d)
  saveRDS(d, file.path(org::PROJ$CLEAN,"WP1_waterworks_clean_water","Vestfold Vann IKS_Seierstad_3.RDS"))
  
  ## ålesund_Vasstrandlia
  
  d <- CleanWP1SpecificWaterWorkWide(
    fileIn=file.path(
      org::PROJ$RAW,
      "WP1_clean_water",
      "Ålesund_Vasstrandlia",
      "Rådata",
      "Renvann",
      "test.xlsx"), 
    sheet=1,
    skip=2,
    col_types=rep("text",10),
    col_names=c(
      "date",
      "Coliform bacteria",
      "Colony count",
      "E. Coli",
      "Colour",
      "Turbidity",
      "pH",
      "Conductivity",
      "Intestinal Enterococci",
      "Clostridium perfringens"
    ),
    type="Internal",
    waterwork="ålesund",
    waterType="Clean",
    dateAsNumber=TRUE,
    point="Vasstrandlia"
  )
  print(d)
  saveRDS(d, file.path(org::PROJ$CLEAN,"WP1_waterworks_clean_water","ålesund_Vasstrandlia.RDS"))
  
  
}





























