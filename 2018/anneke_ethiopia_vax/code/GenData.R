GenData <- function(ndistricts=5,nhealthposts=2,nhouseholds=5,val_after_intervention=0.00,val_received_intervention=0.19,val_intercept = 0.38, sdVal=0.11){
  
  data <- data.table(expand.grid(
    districtid=1:ndistricts,
    healthcenterid=1:10,
    healthpostid=1:nhealthposts,
    householdid=1:nhouseholds,
    after_intervention=0:1
  ))
  
  data[,is_poor:=0]
  data[districtid %in% 1:3,is_poor:=1]
  
  data[,fpc_district:=15]
  data[,fpc_healthcenter:=10]
  data[is_poor==1,fpc_healthcenter:=10]
  data[,fpc_healthpost:=10]
  data[,fpc_households:=10]
  
  data[,randomized_intervention:=0]
  data[healthcenterid %in% 1:5, randomized_intervention:=1]
  
  data[, received_intervention:=0]
  data[after_intervention==1 & healthcenterid %in% 1:5, received_intervention:=1]
  
  data[,healthpostid:=sprintf("%s_%s_%s",districtid,healthcenterid,healthpostid)]
  data[,healthcenterid:=sprintf("%s_%s",districtid,healthcenterid)]
  data[,districtid:=sprintf("%s",districtid)]
  
  multiplier <- c("district"=1,"healthcenter"=1.2,"healthpost"=1.5)
  for(i in c("district","healthcenter","healthpost")){
    id <- sprintf("%sid",i)
    intercept <- sprintf("intercept_%s",i)
    x <- unique(data[,id,with=F])
    x[,(intercept):=rnorm(.N,sd=sdVal*multiplier[i])]
    data <- merge(data,x,by=id)
  }
  
  data[,intercept:=val_intercept]
  
  data[,intercept_after_intervention:=0]
  data[after_intervention==1,intercept_after_intervention:=val_after_intervention]
  
  data[,intercept_received_intervention:=0]
  data[received_intervention==1,intercept_received_intervention:=val_received_intervention]
  
  data[,p:=intercept + intercept_district + intercept_healthcenter + intercept_healthpost + intercept_after_intervention + intercept_received_intervention]
  data[,r:=runif(n=.N)]
  data[,outcome:=0]
  data[r<p,outcome:=1]
  
  return(data) 
}
