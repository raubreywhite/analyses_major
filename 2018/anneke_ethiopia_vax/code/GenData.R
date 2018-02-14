GenData <- function(nhouseholds=5,val_after_intervention=0.00,val_received_intervention=0.19,val_intercept = 0.38, sdVal=0.11, m=c(1,1.2,1.5)){
  
  data <- data.table(expand.grid(
    districtid=1:10,
    healthcenterid=1:6,
    healthpostid=1:5,
    householdid=1:nhouseholds,
    after_intervention=0:1
  ))
   
  data[,fpc_district:=10]
  data[,fpc_healthcenter:=6]
  data[,fpc_healthpost:=5]
  data[,fpc_households:=500]
  
  data[,fpc_healthpost_above:=fpc_healthcenter*fpc_district*fpc_healthpost]
  
  data[,randomized_intervention:=0]
  data[healthcenterid %in% 1:3, randomized_intervention:=1]
  
  data[, received_intervention:=0]
  data[after_intervention==1 & healthcenterid %in% 1:3, received_intervention:=1]
  
  data[,person_randomized_intervention:=0]
  data[householdid %in% 1:(nhouseholds/2), person_randomized_intervention:=1]
  
  data[, person_received_intervention:=0]
  data[after_intervention==1 & person_randomized_intervention==1, person_received_intervention:=1]
  
  data[,healthpostid:=sprintf("%s_%s_%s",districtid,healthcenterid,healthpostid)]
  data[,healthcenterid:=sprintf("%s_%s",districtid,healthcenterid)]
  data[,districtid:=sprintf("%s",districtid)]
  
  multiplier <- c("district"=m[1],"healthcenter"=m[2],"healthpost"=m[3])
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
  
  data[,intercept_person_received_intervention:=0]
  data[person_received_intervention==1,intercept_person_received_intervention:=val_received_intervention]
  
  data[,p:=intercept + intercept_district + intercept_healthcenter + intercept_healthpost + intercept_after_intervention + intercept_received_intervention]
  data[,r:=runif(n=.N)]
  data[,outcome:=0]
  data[r<p,outcome:=1]
  
  data[,p:=intercept + intercept_district + intercept_healthcenter + intercept_healthpost + intercept_after_intervention + intercept_person_received_intervention]
  data[,r:=runif(n=.N)]
  data[,person_outcome:=0]
  data[r<p,person_outcome:=1]
  
  return(data) 
}
