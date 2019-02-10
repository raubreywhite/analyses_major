# 1.0
#
# Richard White
# r.aubrey.white@gmail.com
# github.com/raubreywhite/RLocalSetup
#
# Local setup of skeleton R analysis work

rm(list=ls())

Sys.setenv(R_TOOLS="C:\\Apps\\Rtools")
Sys.setenv(RSTUDIO_PANDOC="C:\\Apps\\RStudio\\bin\\pandoc")
Sys.setenv(R_TOOLS_PATH="C:\\Apps\\Rtools\\bin;C:\\Apps\\Rtools\\gcc-4.6.3\\bin")
Sys.setenv(PATH=paste0(c(Sys.getenv("R_TOOLS_PATH"),Sys.getenv("PATH"),Sys.getenv("R_PATH")),collapse=";"))

setwd("H:/MSF_SSudan")
source("RLocalSetup.R")

CreatePackage("MSFSSudan")
