if(.Platform$OS.type=="unix"){
  RAWmisc::AllowFileManipulationFromInitialiseProject()
  RAWmisc::InitialiseProject(
    HOME = "/git/code_major/2018/ier_curves/",
    RAW = "/analyses/data_raw/code_major/2018/ier_curves/",
    CLEAN = "/analyses/data_clean/code_major/2018/ier_curves",
    BAKED = "/analyses/results_baked/code_major/2018/ier_curves/",
    FINAL = "/analyses/results_final/code_major/2018/ier_curves/",
    SHARED = "/dropbox/results_shared/code_major/2018/ier_curves/")
}

#install.packages("RAWmisc", repos="https://raubreywhite.github.com/drat")

library(data.table)
library(rstan)

masterData <- fread(file.path(RAWmisc::PROJ$RAW,"input_dataset.csv"))
masterData[,S:=as.numeric(as.factor(study))]

outputData <- fread(file.path(RAWmisc::PROJ$RAW,"input_dataset.csv"))

data <- list(
  N=nrow(masterData),
  log_rr=masterData$log_rr,
  log_rr_sd=masterData$log_se,
  S=max(masterData$S),
  source=masterData$S,
  exposure=masterData$conc,
  cf_exposure=masterData$conc+1,
  T=5,
  test_exposure=c(0,5,10,15,20),
  tmrel=0.0
)


fit1 <- stan(
  file = "model_power2_simsd_source.stan",  # Stan program
  data = data,    # named list of data
  chains = 1,             # number of Markov chains
  warmup = 100,          # number of warmup iterations per chain
  iter = 200,            # total number of iterations per chain
  cores = 1,              # number of cores (using 2 just for the vignette)
  refresh = 1000          # show progress every 'refresh' iterations
)
