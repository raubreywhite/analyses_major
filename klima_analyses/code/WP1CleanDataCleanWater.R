CleanWP1SpecificWaterWorkLong_MapGraph_Internal <- function(d){
  d[, variable := ""]
  unique(d$var2)
  
  d[var2 == "01-Farge (mg/l Pt)" & units=="", units := "mg/l Pt"]
  d[var2 == "01-Farge (mg/l Pt)", variable := "Colour"]
  
  d[var2 == "04-Turbiditet (FNU)" & units=="", units := "FNU"]
  d[var2 == "04-Turbiditet (FNU)", variable := "Turbidity"]
  
  d[var2 == "05-Clostridium perfringens (/100ml)" & units=="", units := "/100ml"]
  d[var2 == "05-Clostridium perfringens (/100ml)", variable := "Clostridium Perfringens"]
  
  d[var2 == "06-E.coli (/100 ml)" & units=="", units:="/100ml"]
  d[var2 == "06-E.coli (/100 ml)", variable := "E. Coli"]
  
  d[var2 == "07-Intestinale enterokokker (/100 ml)" & units=="", units := "/100ml"]
  d[var2 == "07-Intestinale enterokokker (/100 ml)", variable := "Intestinal Enterococci"]
  
  d[var2 == "08-Kimtall 22°C (/ml)" & units=="", units := "/ml"]
  d[var2 == "08-Kimtall 22°C (/ml)", variable := "Colony count at 22 degrees"]
  
  d[var2 == "09-Koliforme bakterier 37°C (/100 ml)" & units=="", units := "/100ml"]
  d[var2 == "09-Koliforme bakterier 37°C (/100 ml)", variable := "Coliform bacteria"]
  
  d[var2 == "35-Konduktivitet (mS/m)" & units=="", units := "mS/m"]
  d[var2 == "35-Konduktivitet (mS/m)", variable := "Conductivity"]
  
  d[var2 == "44-pH, surhetsgrad ( )" & units=="", units := "pH"]
  d[var2 == "44-pH, surhetsgrad ( )", variable := "pH"]
  
  d[var2 == "53-Totalt organisk karbon (TOC) (mg/l C)" & units=="", units := "mg/l C"]
  d[var2 == "53-Totalt organisk karbon (TOC) (mg/l C)", variable := "TOC"]
  
  
  d <- d[variable!=""]
  d <- d[!is.na(value)]
  
  d[,var2:=NULL]
  return(d)
}

CleanWP1SpecificWaterWorkLong_MapGraph <- function(fileIn, 
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
  
  d[,date:=as.Date(as.numeric(date),origin = "1899-12-30")]
  
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
  
  d <- d[!is.na(date) & !is.na(value)]
  
  return(d)
}


CleanDataWaterworksCleanWaterInternal <- function(){
  #warning("# KEEP TOC")
  
  
  ## ALTA
  d <- CleanWP1SpecificWaterWorkLong_MapGraph(
    fileIn=file.path(RPROJ$PROJRAW,"WP1_clean_water/Alta_Elvestrand/Rådata/Rapport siste 9 år Alta vannverk.xlsx"), 
    sheet=1,
    type="Accredited",
    waterwork="Alta",
    waterType="Clean"
  )
  saveRDS(d, file.path(RPROJ$PROJCLEAN,"WP1_waterworks_clean_water","Rapport siste 9 år Alta vannverk.RDS"))
  
  ## ARENDAL RORE
  
  d <- CleanWP1SpecificWaterWorkWide_MapGraph(
    fileIn=file.path(
      RPROJ$PROJRAW,
      "WP1_clean_water",
      "Arendal_Rore",
      "Rådata",
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
  saveRDS(d, file.path(RPROJ$PROJCLEAN,"WP1_waterworks_clean_water","Arendal_Rore.RDS"))
  
  ##
  ## Asker og Bærum IKS
  ## Holsfjorden
  
  d <- CleanWP1SpecificWaterWorkWide(
    fileIn=file.path(
      RPROJ$PROJRAW,
      "WP1_clean_water",
      "Asker og Bærum vannverk IKS_Holsfjorden Kattås",
      "Rådata",
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
  saveRDS(d, file.path(RPROJ$PROJCLEAN,"WP1_waterworks_clean_water","Asker_og_Baerum_IKS_Holsfjorden_1.RDS"))
  
  d <- CleanWP1SpecificWaterWorkWide(
    fileIn=file.path(
      RPROJ$PROJRAW,
      "WP1_clean_water",
      "Asker og Bærum vannverk IKS_Holsfjorden Kattås",
      "Rådata",
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
  saveRDS(d, file.path(RPROJ$PROJCLEAN,"WP1_waterworks_clean_water","Asker_og_Baerum_IKS_Holsfjorden_2.RDS"))
  
  d <- CleanWP1SpecificWaterWorkWide(
    fileIn=file.path(
      RPROJ$PROJRAW,
      "WP1_clean_water",
      "Asker og Bærum vannverk IKS_Holsfjorden Kattås",
      "Rådata",
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
  saveRDS(d, file.path(RPROJ$PROJCLEAN,"WP1_waterworks_clean_water","Asker_og_Baerum_IKS_Holsfjorden_3.RDS"))
  
  d <- CleanWP1SpecificWaterWorkWide(
    fileIn=file.path(
      RPROJ$PROJRAW,
      "WP1_clean_water",
      "Asker og Bærum vannverk IKS_Holsfjorden Kattås",
      "Rådata",
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
  saveRDS(d, file.path(RPROJ$PROJCLEAN,"WP1_waterworks_clean_water","Asker_og_Baerum_IKS_Holsfjorden_4.RDS"))
  
  ##
  ## Asker og Bærum IKS
  ## Aurevann
  
  d <- CleanWP1SpecificWaterWorkWide(
    fileIn=file.path(
      RPROJ$PROJRAW,
      "WP1_clean_water",
      "Asker og Bærum vannverk IKS_Aurevann",
      "Rådata",
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
  saveRDS(d, file.path(RPROJ$PROJCLEAN,"WP1_waterworks_clean_water","Asker_og_Baerum_IKS_Aurevann_1.RDS"))
  
  d <- CleanWP1SpecificWaterWorkWide(
    fileIn=file.path(
      RPROJ$PROJRAW,
      "WP1_clean_water",
      "Asker og Bærum vannverk IKS_Aurevann",
      "Rådata",
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
  saveRDS(d, file.path(RPROJ$PROJCLEAN,"WP1_waterworks_clean_water","Asker_og_Baerum_IKS_Aurevann_2.RDS"))
  
  d <- CleanWP1SpecificWaterWorkWide(
    fileIn=file.path(
      RPROJ$PROJRAW,
      "WP1_clean_water",
      "Asker og Bærum vannverk IKS_Aurevann",
      "Rådata",
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
  saveRDS(d, file.path(RPROJ$PROJCLEAN,"WP1_waterworks_clean_water","Asker_og_Baerum_IKS_Aurevann_3.RDS"))
  
  d <- CleanWP1SpecificWaterWorkWide(
    fileIn=file.path(
      RPROJ$PROJRAW,
      "WP1_clean_water",
      "Asker og Bærum vannverk IKS_Aurevann",
      "Rådata",
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
  saveRDS(d, file.path(RPROJ$PROJCLEAN,"WP1_waterworks_clean_water","Asker_og_Baerum_IKS_Aurevann_4.RDS"))
  
  d <- CleanWP1SpecificWaterWorkWide(
    fileIn=file.path(
      RPROJ$PROJRAW,
      "WP1_clean_water",
      "Asker og Bærum vannverk IKS_Aurevann",
      "Rådata",
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
  saveRDS(d, file.path(RPROJ$PROJCLEAN,"WP1_waterworks_clean_water","Asker_og_Baerum_IKS_Aurevann_5.RDS"))
  
  ##
  ## Bergen_Espeland
  ## Espeland
  
  d <- CleanWP1SpecificWaterWorkWide(
    fileIn=file.path(
      RPROJ$PROJRAW,
      "WP1_clean_water",
      "Bergen_Espeland",
      "Rådata",
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
  saveRDS(d, file.path(RPROJ$PROJCLEAN,"WP1_waterworks_clean_water","Bergen_Espeland_Espeland_1.RDS"))
  
  d <- CleanWP1SpecificWaterWorkWide(
    fileIn=file.path(
      RPROJ$PROJRAW,
      "WP1_clean_water",
      "Bergen_Espeland",
      "Rådata",
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
  saveRDS(d, file.path(RPROJ$PROJCLEAN,"WP1_waterworks_clean_water","Bergen_Espeland_Espeland_2.RDS"))
  
  ##
  ## Bergen_Sædalen
  ## Saedalen
  
  d <- CleanWP1SpecificWaterWorkWide(
    fileIn=file.path(
      RPROJ$PROJRAW,
      "WP1_clean_water",
      "Bergen_Sædalen",
      "Rådata",
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
  saveRDS(d, file.path(RPROJ$PROJCLEAN,"WP1_waterworks_clean_water","Bergen_Saedalen_Saedalen_1.RDS"))
  
  d <- CleanWP1SpecificWaterWorkWide(
    fileIn=file.path(
      RPROJ$PROJRAW,
      "WP1_clean_water",
      "Bergen_Sædalen",
      "Rådata",
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
  saveRDS(d, file.path(RPROJ$PROJCLEAN,"WP1_waterworks_clean_water","Bergen_Saedalen_Saedalen_2.RDS"))
  
  d <- CleanWP1SpecificWaterWorkWide(
    fileIn=file.path(
      RPROJ$PROJRAW,
      "WP1_clean_water",
      "Bergen_Sædalen",
      "Rådata",
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
  saveRDS(d, file.path(RPROJ$PROJCLEAN,"WP1_waterworks_clean_water","Bergen_Saedalen_Saedalen_3.RDS"))
  
  ##
  ## Bergen_Svartediket
  ## Svartediket
  
  d <- CleanWP1SpecificWaterWorkWide(
    fileIn=file.path(
      RPROJ$PROJRAW,
      "WP1_clean_water",
      "Bergen_Svartediket",
      "Rådata",
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
  saveRDS(d, file.path(RPROJ$PROJCLEAN,"WP1_waterworks_clean_water","Bergen_Svartediket_Svartediket_1.RDS"))
  
  d <- CleanWP1SpecificWaterWorkWide(
    fileIn=file.path(
      RPROJ$PROJRAW,
      "WP1_clean_water",
      "Bergen_Svartediket",
      "Rådata",
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
  saveRDS(d, file.path(RPROJ$PROJCLEAN,"WP1_waterworks_clean_water","Bergen_Svartediket_Svartediket_2.RDS"))
  
  d <- CleanWP1SpecificWaterWorkWide(
    fileIn=file.path(
      RPROJ$PROJRAW,
      "WP1_clean_water",
      "Bergen_Svartediket",
      "Rådata",
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
  saveRDS(d, file.path(RPROJ$PROJCLEAN,"WP1_waterworks_clean_water","Bergen_Svartediket_Svartediket_3.RDS"))
  
  ##
  ## Bergen_Kismul
  ## Kismul
  
  d <- CleanWP1SpecificWaterWorkWide(
    fileIn=file.path(
      RPROJ$PROJRAW,
      "WP1_clean_water",
      "Bergen_Kismul",
      "Rådata",
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
  saveRDS(d, file.path(RPROJ$PROJCLEAN,"WP1_waterworks_clean_water","Bergen_Kismul_Kismul_1.RDS"))
  
  d <- CleanWP1SpecificWaterWorkWide(
    fileIn=file.path(
      RPROJ$PROJRAW,
      "WP1_clean_water",
      "Bergen_Kismul",
      "Rådata",
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
  saveRDS(d, file.path(RPROJ$PROJCLEAN,"WP1_waterworks_clean_water","Bergen_Kismul_Kismul_2.RDS"))
  
  d <- CleanWP1SpecificWaterWorkWide(
    fileIn=file.path(
      RPROJ$PROJRAW,
      "WP1_clean_water",
      "Bergen_Kismul",
      "Rådata",
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
  saveRDS(d, file.path(RPROJ$PROJCLEAN,"WP1_waterworks_clean_water","Bergen_Kismul_Kismul_3.RDS"))
  
  ##
  ## Bergen_Jordalsvatnet
  ## Jordalsvatnet
  
  d <- CleanWP1SpecificWaterWorkWide(
    fileIn=file.path(
      RPROJ$PROJRAW,
      "WP1_clean_water",
      "Bergen_Jordalsvatnet",
      "Rådata",
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
  saveRDS(d, file.path(RPROJ$PROJCLEAN,"WP1_waterworks_clean_water","Bergen_Jordalsvatnet_Jordalsvatnet_1.RDS"))
  
  d <- CleanWP1SpecificWaterWorkWide(
    fileIn=file.path(
      RPROJ$PROJRAW,
      "WP1_clean_water",
      "Bergen_Jordalsvatnet",
      "Rådata",
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
  saveRDS(d, file.path(RPROJ$PROJCLEAN,"WP1_waterworks_clean_water","Bergen_Jordalsvatnet_Jordalsvatnet_2.RDS"))
  
  d <- CleanWP1SpecificWaterWorkWide(
    fileIn=file.path(
      RPROJ$PROJRAW,
      "WP1_clean_water",
      "Bergen_Jordalsvatnet",
      "Rådata",
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
  saveRDS(d, file.path(RPROJ$PROJCLEAN,"WP1_waterworks_clean_water","Bergen_Jordalsvatnet_Jordalsvatnet_3.RDS"))
  
  
}


  