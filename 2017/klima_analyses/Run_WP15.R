suppressMessages(library(ggplot2))
RAWmisc::AllowFileManipulationFromInitialiseProject()
RAWmisc::InitialiseProject(
  HOME = "/git/code_major/2017/klima_analyses/",
  RAW = "/analyses/data_raw/code_major/2017/klima_analyses/",
  CLEAN = "/analyses/data_clean/code_major/2017/klima_analyses",
  BAKED = "/analyses/results_baked/code_major/2017/klima_analyses/",
  FINAL = "/analyses/results_final/code_major/2017/klima_analyses/",
  SHARED = "/dropbox/results_shared/code_major/2017/klima_analyses/")

suppressWarnings(suppressMessages(library(data.table)))
suppressWarnings(suppressMessages(library(ggplot2)))
suppressWarnings(suppressMessages(library(foreach)))
suppressWarnings(suppressMessages(library(pomp)))

WP2Data()
d <- WP2WaterworkRawData()