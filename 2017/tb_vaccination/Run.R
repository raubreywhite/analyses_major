RAWmisc::AllowFileManipulationFromInitialiseProject()

RAWmisc::InitialiseProject(
  HOME = "/git/code_major/2017/tb_vaccination/",
  RAW = "/analyses/data_raw/code_major/2017/tb_vaccination/",
  CLEAN = "/analyses/data_clean/code_major/2017/tb_vaccination",
  BAKED = "/analyses/results_baked/code_major/2017/tb_vaccination/",
  FINAL = "/analyses/results_final/code_major/2017/tb_vaccination/",
  SHARED = "/dropbox/results_shared/code_major/2017/tb_vaccination/")

library(data.table)
library(pomp)
library(ggplot2)

countries <- 
c("000",
"101",
"102",
"103",
"104",
"105",
"106",
"111",
"114",
"153",
"120",
"112",
"155",
"113",
"122",
"158",
"142",
"115",
"117",
"151",
"144",
"118",
"119",
"162",
"152",
"121",
"164",
"123",
"163",
"161",
"124",
"128",
"136",
"129",
"156",
"126",
"138",
"130",
"160",
"127",
"131",
"132",
"133",
"140",
"134",
"159",
"125",
"157",
"146",
"135",
"137",
"141",
"143",
"148",
"139",
"154",
"196",
"199",
"203",
"204",
"229",
"205",
"213",
"393",
"216",
"270",
"273",
"337",
"373",
"220",
"279",
"278",
"239",
"250",
"249",
"235",
"241",
"246",
"254",
"256",
"260",
"264",
"266",
"276",
"281",
"283",
"286",
"289",
"296",
"299",
"306",
"307",
"322",
"303",
"319",
"308",
"309",
"313",
"323",
"329",
"209",
"333",
"336",
"338",
"339",
"346",
"359",
"355",
"349",
"356",
"357",
"369",
"376",
"379",
"386",
"304",
"389",
"326",
"395",
"397",
"398",
"399",
"404",
"406",
"407",
"409",
"410",
"412",
"416",
"478",
"484",
"500",
"430",
"436",
"444",
"448",
"456",
"452",
"460",
"464",
"476",
"480",
"496",
"502",
"504",
"508",
"510",
"512",
"513",
"516",
"420",
"528",
"488",
"572",
"520",
"534",
"524",
"428",
"540",
"544",
"546",
"548",
"492",
"576",
"556",
"424",
"564",
"432",
"550",
"568",
"537",
"552",
"426",
"554",
"575",
"578",
"597",
"598",
"599",
"660",
"603",
"657",
"605",
"602",
"604",
"606",
"659",
"608",
"612",
"613",
"616",
"620",
"661",
"622",
"624",
"672",
"628",
"629",
"631",
"632",
"636",
"644",
"648",
"650",
"652",
"654",
"656",
"664",
"668",
"669",
"685",
"687",
"677",
"678",
"686",
"676",
"679",
"658",
"680",
"681",
"684",
"601",
"697",
"698",
"699",
"705",
"710",
"715",
"725",
"730",
"735",
"740",
"745",
"720",
"755",
"760",
"765",
"770",
"775",
"797",
"798",
"799",
"802",
"805",
"807",
"808",
"809",
"811",
"814",
"817",
"815",
"835",
"826",
"818",
"833",
"820",
"821",
"822",
"840",
"839",
"827",
"828",
"830",
"806",
"823",
"829",
"813",
"816",
"819",
"812",
"831",
"832",
"895",
"899",
"980",
"990")

d <- readRDS(file.path(RAWmisc::PROJ$RAW,"data.RDS"))
d <- d[isActive==1]
d <- d[cyear>=1986,.(isActive=sum(isActive)),by=.(
  cyear,cAltNorwegianStatusNB,cAlgr,cFlandNB,cPernorNB
)]
d[,cFlandNB:=gsub("å",RAWmisc::NORCHAR$aa,cFlandNB)]
d[,cFlandNB:=gsub("Å",RAWmisc::NORCHAR$AA,cFlandNB)]
d[,cFlandNB:=gsub("ø",RAWmisc::NORCHAR$oe,cFlandNB)]
d[,cFlandNB:=gsub("Ø",RAWmisc::NORCHAR$OE,cFlandNB)]
d[,cFlandNB:=gsub("æ",RAWmisc::NORCHAR$ae,cFlandNB)]
d[,cFlandNB:=gsub("Æ",RAWmisc::NORCHAR$AE,cFlandNB)]
RAWmisc::RecodeDT(d,RAWmisc::CountriesNBtoEN(),"cFlandNB")
d[cFlandNB=="Sør-Afrika",cFlandNB:="South Africa"]
d[cFlandNB=="Fǣrøyene",cFlandNB:="Faroe Islands"]
d[cFlandNB=="Østerrike",cFlandNB:="Austria"]
d[cFlandNB=="Sør-Korea",cFlandNB:="South Korea"]

RAWmisc::RecodeDT(d,c(
  "Repub. Congo"="Congo-Brazzaville",
  "DPRK"="North Korea",
  "Hungry"="Hungary",
  "Yugoslavia"="Serbia",
  "Bosnia and Herzegovina"="Bosnia-Herzegovina",
  "DRC"="Congo",
  "USA"="United States",
  "UK"="United Kingdom",
  "Mauretania"="Mauritania",
  "Nanibia"="Namibia",
  "Laso"="Laos",
  "UAE"="United Arab Emirates",
  "Cote d'Ivoire"="Côte d'Ivore"
),"cFlandNB")
unique(d$cFlandNB)[!unique(d$cFlandNB) %in% sort(as.character(unique(immi$country)))]



      

bake(file.path(RAWmisc::PROJ$RAW,"pop.RDS"),{
  pop <- data.table(read.csv(url("https://data.ssb.no/api/v0/dataset/1082.csv?lang=en"),stringsAsFactors = FALSE))
  pop[,sex:=NULL]
  pop[,contents:=NULL]
  pop[,x:=as.numeric(stringr::str_extract(age,"^[0-9][0-9][0-9]"))]
  pop[,age:=NULL]
  pop[,region:=NULL]
  setnames(pop,c("year","pop","age"))
  pop <- pop[,.(pop=sum(pop)),by=.(year,age)]
  pop[,total:=sum(pop),by=year]
  pop[,prop:=pop/total]
  setnames(pop,"pop","popNorBornTo1PNor")
  
  b <- paste0('{
    "query": [
              {
              "code": "Land",
              "selection": {
              "filter": "item",
              "values": [
              ',paste0('"',countries,'"',collapse=","),'
              ]
              }
              },
              {
              "code": "ContentsCode",
              "selection": {
              "filter": "item",
              "values": [
              "Innvandring"
              ]
              }
              },
              {
              "code": "Tid",
              "selection": {
              "filter": "item",
              "values": [
              ',paste0('"',1986:2017,'"',collapse=","),'
              ]
              }
              }
              ],
              "response": {
              "format": "json-stat"
              }
}')
  # 07822: Immigration, emigration and net migration, by country of emigration/immigration 1967 - 2016
  x <- httr::POST("http://data.ssb.no/api/v0/en/table/07822",
                  body=b,encode="json")
  
  y <- jsonlite::fromJSON(httr::content(x,"text"))
  d2 <- unlist(y$dataset$dimension$Land$category$label)
  d3 <- unlist(y$dataset$dimension$Tid$category$label)
  res <- expand.grid(d3,d2)
  res$enteringImmigrants <- as.numeric(as.character(y$dataset$value))
  res$enteringImmigrants[res$enteringImmigrants<=3] <- 0
  enteringCountry <- data.table(res)
  
  b <- paste0('
{
  "query": [
  {
  "code": "Kjonn",
  "selection": {
  "filter": "item",
  "values": [
  "1",
  "2"
  ]
  }
  },
  {
  "code": "Landbakgrunn",
  "selection": {
  "filter": "item",
  "values": [
',paste0('"',countries,'"',collapse=","),'
  ]
  }
  },
  {
  "code": "ContentsCode",
  "selection": {
  "filter": "item",
  "values": [
  "Personer"
  ]
  }
  },
  {
  "code": "Tid",
  "selection": {
  "filter": "item",
  "values": [
',paste0('"',1986:2017,'"',collapse=","),'
  ]
  }
  }
  ],
  "response": {
  "format": "json-stat"
  }
}
')              

  # 05184: Immigrants, by sex and country background 1970 - 2017
  x <- httr::POST("http://data.ssb.no/api/v0/en/table/05184",
                  body=b,encode="json")
  
  y <- jsonlite::fromJSON(httr::content(x,"text"))
  d2 <- unlist(y$dataset$dimension$Kjonn$category$label)
  d3 <- unlist(y$dataset$dimension$Landbakgrunn$category$label)
  d4 <- unlist(y$dataset$dimension$Tid$category$label)
  res <- expand.grid(d4,d3,d2)
  res$immigrants <- as.numeric(as.character(y$dataset$value))
  res$immigrants[res$immigrants<=3] <- 0
  immi1 <- data.table(res)
  
  # 05183: Immigrants and Norwegian-born to immigrant parents, by sex and country background 1970 - 2017
  # norwegian born to immigrant parent=Persons born in Norway of two foreign-born parents and four foreign- born grandparents.
  x <- httr::POST("http://data.ssb.no/api/v0/en/table/05183",
                  body=b,encode="json")
  
  y <- jsonlite::fromJSON(httr::content(x,"text"))
  d2 <- unlist(y$dataset$dimension$Kjonn$category$label)
  d3 <- unlist(y$dataset$dimension$Landbakgrunn$category$label)
  d4 <- unlist(y$dataset$dimension$Tid$category$label)
  res <- expand.grid(d4,d3,d2)
  res$immigrantsAndNorwegianBornToImmigrants <- as.numeric(as.character(y$dataset$value))
  res$immigrantsAndNorwegianBornToImmigrants[res$immigrantsAndNorwegianBornToImmigrants<=3] <- 0
  immi2 <- data.table(res)
  
  immi <- merge(immi1,immi2,by=c("Var1","Var2","Var3"))
  immi <- immi[,.(immigrants=sum(immigrants),
                  immigrantsAndNorwegianBornToImmigrants=sum(immigrantsAndNorwegianBornToImmigrants)),
               by=.(Var1,Var2)]
  immi <- merge(immi,enteringCountry,by=c("Var1","Var2"),all.x=T)
  immi[is.na(enteringImmigrants),enteringImmigrants:=0]
  setnames(immi,c("year","country","popImmigrants","immigrantsAndNorwegianBornToImmigrants","enteringImmigrants"))
  immi[,popNorBornToImmigrants:=immigrantsAndNorwegianBornToImmigrants-popImmigrants]
  immi[,immigrantsAndNorwegianBornToImmigrants:=NULL]
  
  immi[,country:=as.character(country)]
  immi[!country %in% unique(d$cFlandNB),country:="Other"]
  
  immi <- immi[,.(popImmigrants=sum(popImmigrants),
                  popNorBornToImmigrants=sum(popNorBornToImmigrants),
                  enteringImmigrants=sum(enteringImmigrants)),
               by=.(country,year)]
  
  immi[,x:=0]
  immi[,x:=sum(popNorBornToImmigrants),by=year]
  immi[country=="Norway",popNorBornToImmigrants:=x]
  immi[country!="Norway",popNorBornToImmigrants:=0]
  immi[,x:=NULL]
  immi[,year:=as.numeric(as.character(year))]
  immi[,popImmigrantsForSubtractingNorway:=sum(popImmigrants),by=year]
  
  
  
  skeleton <- data.table(expand.grid(
    year=unique(immi$year),
    country=unique(immi$country),
    age=unique(pop$age)
  ))
  skeleton <- merge(skeleton,immi,by=c("country","year"),all.x=T)
  skeleton <- merge(skeleton,pop,by=c("year","age"),all.x=T)
  
  skeleton[country!="Norway",popImmigrants:=popImmigrants*prop]
  skeleton[country!="Norway",enteringImmigrants:=enteringImmigrants*prop]
  skeleton[country=="Norway",popNorBornToImmigrants:=popNorBornToImmigrants*prop]
  skeleton[country=="Norway",popImmigrantsForSubtractingNorway:=popImmigrantsForSubtractingNorway*prop]
  skeleton[country!="Norway",popNorBornTo1PNor:=0]
  
  skeleton[country=="Norway",popNorBornTo1PNor:=popNorBornTo1PNor-popImmigrantsForSubtractingNorway-popNorBornToImmigrants]
  skeleton[,popImmigrantsForSubtractingNorway:=NULL]

  skeleton[country!="Norway",popNorBornTo1PNor:=0]
  skeleton[,total:=NULL]
  skeleton[,prop:=NULL]
  skeleton
}) -> pop

skeleton <- pop[year==2000]

skeleton <- expand.grid(
  age=0:105,
  
)