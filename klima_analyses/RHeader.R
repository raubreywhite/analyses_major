# new
isLinux <- length(grep("linux",sessionInfo()$platform))>0
isRStudio <- Sys.getenv("RSTUDIO") == "1"
msg <- function(p,s,m){
  if(!s %in% c("RUN","ERROR","WARN","FINISHED")) stop("Error in message function")
  if(length(grep("linux",sessionInfo()$platform))>0) system(paste0('/log/log.sh "',lubridate::now(tzone="CET"),'@',p,'@',s,'@',m,'"'))
}
# Change if you want local setup to be pulled from github
upgradeRLocalSetup <- FALSE

if(isLinux){
  msg("klima","RUN","Beginning program")
  if(!exists("RPROJ")) RPROJ <- list(PROJHOME = normalizePath("/src/"))
  
  setwd(RPROJ$PROJHOME)
  source("RLocalSetup.R")
  
} else {
  Sys.setenv(R_TOOLS="C:\\Apps\\Rtools")
  Sys.setenv(R_TOOLS_PATH="C:\\Apps\\Rtools\\bin;C:\\Apps\\Rtools\\gcc-4.6.3\\bin")
  Sys.setenv(PATH=paste0(c(Sys.getenv("R_TOOLS_PATH"),Sys.getenv("PATH"),Sys.getenv("R_PATH")),collapse=";"))
  Sys.setenv(RSTUDIO_PANDOC="C:/Apps/RStudio/bin/pandoc")
  
  setwd(RPROJ$PROJHOME)
  source("RLocalSetup.R")
  
  # Packrat
  setwd("KlimaAnalyses")
  suppressWarnings(packrat::on(auto.snapshot=FALSE))
  setwd("..")
  #packrat::status()
  #packrat::snapshot()
  
  # Unload package
  try(devtools::unload("KlimaAnalyses"),TRUE)
  
  # Load package
  devtools::load_all("KlimaAnalyses")
}
# new end

# Commit to Git
try(CommitToGit(paste0("Committing while loading at ",Sys.time())),TRUE)
