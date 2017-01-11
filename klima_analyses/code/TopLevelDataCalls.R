WP1Data <- function(){
  if(RUN_ALL) unlink("data_clean/WP1.RDS")
  bake("data_clean/WP1.RDS",{
    CleanDataWP1NVE()
    CleanDataWP1MET()
    CleanDataWaterworks()
    readRDS("data_clean/WP1.RDS")}) -> d
  return(d)
}

WP2Data <- function(initialDataCall=FALSE){
  if(initialDataCall & RUN_ALL) system(paste0("rm -f ",file.path(getwd(),"data_clean/WP2.RDS")))
  bake("data_clean/WP2.RDS",{
    CleanData()
  }) -> d
  return(d)
}