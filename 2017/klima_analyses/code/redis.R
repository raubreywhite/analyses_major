GetRedisIP <- function(){
  res <- system("getent hosts redis", intern=TRUE)
  res <- stringr::str_split(res," ")[[1]][1]
  return(res)
}

UploadWP2DataToRedis <- function(d){
  try(rredis::redisClose(),TRUE)
  tryCatch({
    rredis::redisConnect(host=GetRedisIP())
  }, error=function(err){
    rredis::redisConnect(host=GetRedisIP())
  })
  
  rredis::redisSet("klima-wp2-data",d)

  try(rredis::redisClose(),TRUE)
}