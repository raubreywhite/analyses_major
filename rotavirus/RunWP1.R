RAWmisc::InitialiseProject(
  PROJHOME = "/analyses/code_major/rotavirus/",
  PROJRAW = "/dropbox/data_raw/rotavirus/",
  PROJCLEAN = "/analyses/data_clean/rotavirus",
  PROJBAKED = "/analyses/results_baked/rotavirus/",
  PROJFINAL = "/analyses/results_final/rotavirus/",
  PROJSHARED = "/dropbox/results_shared/rotavirus/")

suppressWarnings(suppressMessages(library(data.table)))
suppressWarnings(suppressMessages(library(ggplot2)))
suppressWarnings(suppressMessages(library(foreach)))
suppressWarnings(suppressMessages(library(pomp)))
suppressMessages(library(doParallel))
registerDoParallel()
assign("RUN_ALL", TRUE, envir=globalenv())
