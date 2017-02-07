WP1Data <- function(){
  if(RUN_ALL) unlink(file.path(RPROJ$PROJCLEAN,"WP1.RDS"))
  bake(file.path(RPROJ$PROJCLEAN,"WP1.RDS"),{
    dir.create(file.path(RPROJ$PROJCLEAN,"WP1_waterworks"))
    dir.create(file.path(RPROJ$PROJFINAL,"WP1"))
    
    CleanDataWP1NVE()
    CleanDataWP1MET()
    CleanDataWaterworks()
    d <- readRDS(file.path(RPROJ$PROJCLEAN,"WP1.RDS"))
    d <- d[value>=0]
    }) -> d
  return(d)
}

WP2Data <- function(initialDataCall=FALSE){
  if(initialDataCall & RUN_ALL) system(paste0("rm -f ",file.path(RPROJ$PROJCLEAN,"WP2.RDS")))
  bake(file.path(RPROJ$PROJCLEAN,"WP2.RDS"),{
    CleanData()
  }) -> d
  return(d)
}