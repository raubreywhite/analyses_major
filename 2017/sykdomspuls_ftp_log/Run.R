if(.Platform$OS.type=="unix"){
  RAWmisc::UseRClone()
  RAWmisc::AllowFileManipulationFromInitialiseProject()
  
  if(dir.exists("/dropbox")){
    SHARED <- "/dropbox/analyses/results_shared/code_major/2017/sykdomspuls_ftp_log"
    RCLONE_SHARED <- NULL
  } else {
    SHARED <- "/tmp/results_shared/code_major/2017/sykdomspuls_ftp_log/"
    RCLONE_SHARED <- "data:/analyses/results_shared/code_major/2017/sykdomspuls_ftp_log/"
  }
  
  RAWmisc::InitialiseProject(
    HOME = "/git/code_major/2017/sykdomspuls_ftp_log/",
    RAW = "/tmp/data_raw/code_major/2017/sykdomspuls_ftp_log/",
    CLEAN = "/tmp/data_clean/code_major/2017/sykdomspuls_ftp_log",
    BAKED = "/tmp/results_baked/code_major/2017/sykdomspuls_ftp_log/",
    FINAL = "/tmp/results_final/code_major/2017/sykdomspuls_ftp_log/",
    SHARED = SHARED,
    RCLONE_RAW = "crypt:/data_raw/code_major/2017/sykdomspuls_ftp_log/",
    RCLONE_SHARED = RCLONE_SHARED
  )
}




library(data.table)

d <- list()
for(i in list.files(RAWmisc::PROJ$RAW)){
  try({
    d[[i]] <- fread(file.path(RAWmisc::PROJ$RAW,i),skip=4L,sep=" ")
    if(ncol(d[[i]])==14) d[[i]][,V4:=NULL]
    setnames(d[[i]],paste0("V",1:13))
    d[[i]][,V10:=NULL]
  },TRUE)
}

d <- rbindlist(d)
d <- d[V7=="USER"]
logins <- d[,.(N=.N),by=.(
  V1,
  V8
)]
setorder(logins,V8,V1)
openxlsx::write.xlsx(logins,file=file.path(RAWmisc::PROJ$SHARED_TODAY,"logins.xlsx"))
logins[V8=="sykdomspulsen.fhi.no|data"]
logins[V8=="sykdomspulsen.fhi.no|riwh"]

RAWmisc::SaveProject()

