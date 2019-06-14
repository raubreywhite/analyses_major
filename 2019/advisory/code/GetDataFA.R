GetDataFA <- function(dInfo){
  # one factor: kultur og felles identitet: 4-8
  # one factor: organisering: 9-11
  # one factor: 12?
  # one factor: formaliserte samarbeidsstrukturer: 15-21
  # one factor: samarbeid: 23-26
  # one factor: medbestemmelse: 27-30
  # one factor: støtte fra nærmeste leder: 33-36
  # one factor: områdeledelse: 39-46
  # one factor: toppledergruppen: 47-51
  # one factor: lederrolle: 52-56
  
  #dFA <- list()
  
  #for(i in seq_along(varsFA)){
  dFA <- data.table(dInfo[,varsFA])
  for(j in varsFA){
    dFA[get(j)>=6,(j):=NA]
    m <- mean(dFA[[j]],na.rm=T)+0.0001
    dFA[is.na(get(j)),(j):=m]
  }
  
  return(dFA)
}

GetDataFAL <- function(dInfo){
  # one factor: kultur og felles identitet: 4-8
  # one factor: organisering: 9-11
  # one factor: 12?
  # one factor: formaliserte samarbeidsstrukturer: 15-21
  # one factor: samarbeid: 23-26
  # one factor: medbestemmelse: 27-30
  # one factor: støtte fra nærmeste leder: 33-36
  # one factor: områdeledelse: 39-46
  # one factor: toppledergruppen: 47-51
  # one factor: lederrolle: 52-56
  
  #dFA <- list()
  
  #for(i in seq_along(varsFA)){
  dFAL <- data.table(dInfo[,varsFAL])
  for(j in varsFAL){
    dFAL[get(j)>=6,(j):=NA]
    m <- mean(dFAL[[j]],na.rm=T)+0.0001
    dFAL[is.na(get(j)),(j):=m]
  }
  
  return(dFAL)
}