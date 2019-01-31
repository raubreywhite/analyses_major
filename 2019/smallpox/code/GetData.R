Shorten <- function(v,len=15){
  pattern <- paste0('(.{1,',len,'})(\\s)')
  gsub(pattern, '\\1\n', v)
}

Convert <- function(d, oldName, newName){
  d[,(newName):=as.character(get(oldName))]  
  x <- as.character(attributes(dInfo[[oldName]])$labels)
  names(x) <- gsub('(.{1,15})(\\s)', '\\1\n', names(attributes(dInfo[[oldName]])$labels))
  RAWmisc::RecodeDT(d,x,newName, oldOnLeft=F)
}

CleanData <- function(dInfo,detailed=F){
  
  d <- haven::zap_label(dInfo)
  d <- data.table(d)
  
  d[,idnum:=1:.N]
  
  names(d)
  
  d[,c("idnum","op_57","op_58")]
  
  openxlsx::write.xlsx(d,file.path(org::PROJ$SHARED_TODAY,"text_comments.xlsx"))
  
  
  
  Convert(d,"op_1","c_omrade")
  Convert(d,"op_2","c_arbeidsoppgaver")
  Convert(d,"op_3","c_lenge")
  
  xtabs(~d$c_omrade,addNA=T)
  xtabs(~d$c_arbeidsoppgaver,addNA=T)
  xtabs(~d$c_lenge,addNA=T)
  
  if(detailed==FALSE){
    RAWmisc::RecodeDT(d,
                      c(
                        "Forskning\n(forsker /\nkunnskapsoppsummeringer\n/\nfagekspert)"="Medarbeidere",
                        "Annen faglig\nproduksjon"="Medarbeidere",
                        "Administrative\neller tekniske\nstøttefunksjonerenester"="Medarbeidere", 
                        "Leder (leder\nmed\npersonalansvar\neller\nfagdirektør\nuten\npersonalansvar)"="Ledere"
                      ),
                      "c_arbeidsoppgaver")
  } else {
    RAWmisc::RecodeDT(d,
                      c(
                        "Forskning\n(forsker /\nkunnskapsoppsummeringer\n/\nfagekspert)"="Forskning",
                        "Annen faglig\nproduksjon"="Faglig produksjon",
                        "Administrative\neller tekniske\nstøttefunksjonerenester"="Admin/Teknisk", 
                        "Leder (leder\nmed\npersonalansvar\neller\nfagdirektør\nuten\npersonalansvar)"="Ledere"
                      ),
                      "c_arbeidsoppgaver")
  }
  xtabs(~d$c_arbeidsoppgaver,addNA=T)
  
  
  res <- vector("list", length=length(vars))
  for(i in seq_along(res)){
    var <- vars[i]
    at <- attributes(dInfo[[var]])
    labs <- data.frame(get=at$labels)
    labs$value <- row.names(labs)
    setDT(labs)
    if(nrow(labs)==0) next
    
    d[get(var) %in% labs[value %in% c("Vet ikke","Vet ikke, Ikke relevant")]$get, (var):=10]
    d[is.na(get(var)),(var):=10]
    resx <- d[,.(
      N=.N
    ),keyby=.(get(var),c_omrade,c_arbeidsoppgaver,c_lenge)]
    
    labs[value %in% c("Vet ikke","Vet ikke, Ikke relevant"),get:=10]
    labs[value %in% c("Vet ikke","Vet ikke, Ikke relevant"),value:="Vet ikke/ikke svart"]
    
    skeleton <- expand.grid(
      c_omrade=unique(d$c_omrade),
      c_arbeidsoppgaver=unique(d$c_arbeidsoppgaver),
      c_lenge=unique(d$c_lenge),
      get=labs$get)
    skeleton <- merge(skeleton, labs, all.x=T, by="get")
    setDT(skeleton)
    
    resx <- merge(skeleton, haven::zap_labels(resx), all=T, by=c("get","c_omrade","c_arbeidsoppgaver","c_lenge"))
    setnames(resx,"get","x")
    resx[,var:=var]
    resx[,varName:=at$label]
    resx[is.na(N), N:=0]
    resx[is.na(x),x:=10]
    resx[is.na(value),value:="Ikke valgt"]
    
    res[[i]] <- resx
  }
  
  res <- rbindlist(res)
  
  resA <- res[,.(
    N=sum(N)
  ),by=.(
    x,
    c_omrade,
    c_arbeidsoppgaver,
    value,
    var,
    varName
  )]
  resA[,c_lenge:="Alle"]
  
  resB <- res[,.(
    N=sum(N)
  ),by=.(
    x,
    c_omrade,
    c_lenge,
    value,
    var,
    varName
  )]
  resB[,c_arbeidsoppgaver:="Alle"]
  
  resC <- res[,.(
    N=sum(N)
  ),by=.(
    x,
    c_arbeidsoppgaver,
    c_lenge,
    value,
    var,
    varName
  )]
  resC[,c_omrade:="Alle"]
  
  resD <- res[,.(
    N=sum(N)
  ),by=.(
    x,
    c_omrade,
    value,
    var,
    varName
  )]
  resD[,c_lenge:="Alle"]
  resD[,c_arbeidsoppgaver:="Alle"]
  
  resE <- res[,.(
    N=sum(N)
  ),by=.(
    x,
    c_arbeidsoppgaver,
    value,
    var,
    varName
  )]
  resE[,c_lenge:="Alle"]
  resE[,c_omrade:="Alle"]
  
  resF <- res[,.(
    N=sum(N)
  ),by=.(
    x,
    c_lenge,
    value,
    var,
    varName
  )]
  resF[,c_arbeidsoppgaver:="Alle"]
  resF[,c_omrade:="Alle"]
  
  resG <- res[,.(
    N=sum(N)
  ),by=.(
    x,
    value,
    var,
    varName
  )]
  resG[,c_lenge:="Alle"]
  resG[,c_arbeidsoppgaver:="Alle"]
  resG[,c_omrade:="Alle"]
  
  res <- rbind(
    res,
    resA,
    resB,
    resC,
    resD,
    resE,
    resF,
    resG
  )
  
  unique(res$c_arbeidsoppgaver)
  unique(res$c_omrade)
  
  if(detailed==FALSE){
    res[,c_arbeidsoppgaver:=factor(c_arbeidsoppgaver,levels=c(
      "Alle","Ledere","Medarbeidere"
    ))]
  } else {
    res[,c_arbeidsoppgaver:=factor(c_arbeidsoppgaver,levels=c(
      "Alle","Ledere","Forskning","Faglig produksjon","Admin/Teknisk"
    ))]
  }
    
  res[,c_omrade:=factor(c_omrade,levels=c(
    "Alle",
    "Instituttstab /\ninstituttledelse",
    "Smittevern,\nmiljø og\nhelse",
    "Psykisk og\nfysisk\nhelse",
    "Helsedata og\ndigitalisering",
    "Helsetjenester"
  ))]
  res[,c_omrade_silent:=c_omrade]
  levels(res$c_omrade_silent) <- c(" ","  ","   ","    ","     ","      ")
  res[,c_omrade_short:=c_omrade]
  levels(res$c_omrade_short) <- c("Alle","SI+IL","SM","PF","HD","HT")
  
  res[,denom:=sum(N),by=.(c_omrade,c_arbeidsoppgaver,c_lenge,var)]
  res[,prop := N/denom]
  
  res[,c_lenge:=gsub("\\n"," ",c_lenge)]
  res[,c_lenge:=factor(c_lenge,levels=c(
    "Alle",
    "0–3 år",
    "4 år eller lengre"
  ))]
  
  res[,x:=as.numeric(x)]
  
  return(res)
}