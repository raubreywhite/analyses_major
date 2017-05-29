

WP1DataRaw <- function(){
  if(RUN_ALL) unlink(file.path(RPROJ$PROJCLEAN,"WP1_raw.RDS"))
  bake(file.path(RPROJ$PROJCLEAN,"WP1_raw.RDS"),{
    dir.create(file.path(RPROJ$PROJCLEAN,"WP1_waterworks"))
    dir.create(file.path(RPROJ$PROJFINAL,"WP1"))
    
    CleanDataWP1NVE()
    CleanDataWP1MET()
    CleanDataWaterworksRawWater()
    d <- readRDS(file.path(RPROJ$PROJCLEAN,"WP1_raw.RDS"))
    d <- d[value>=0]
    }) -> d
  return(d)
}

WP1DataClean <- function(){
  if(RUN_ALL) unlink(file.path(RPROJ$PROJCLEAN,"WP1_clean.RDS"))
  bake(file.path(RPROJ$PROJCLEAN,"WP1_clean.RDS"),{
    dir.create(file.path(RPROJ$PROJCLEAN,"WP1_waterworks_clean_water"))
    dir.create(file.path(RPROJ$PROJFINAL,"WP1"))
    
    CleanDataWP1NVE()
    CleanDataWP1MET()
    CleanDataWaterworksCleanWater()
    d <- readRDS(file.path(RPROJ$PROJCLEAN,"WP1_clean.RDS"))
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