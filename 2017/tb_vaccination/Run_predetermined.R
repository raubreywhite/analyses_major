RAWmisc::AllowFileManipulationFromInitialiseProject()

RAWmisc::InitialiseProject(
  HOME = "/git/code_major/2017/tb_vaccination/",
  RAW = "/analyses/data_raw/code_major/2017/tb_vaccination/",
  CLEAN = "/analyses/data_clean/code_major/2017/tb_vaccination",
  BAKED = "/analyses/results_baked/code_major/2017/tb_vaccination/",
  FINAL = "/analyses/results_final/code_major/2017/tb_vaccination/",
  SHARED = "/dropbox/results_shared/code_major/2017/tb_vaccination/")

library(data.table)
library(pomp)
library(ggplot2)
library(rstan)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
#everyone enters on jan 1
#results are for dec 31
newPeople <- data.table(id=1:100,year=2000,age=1,timeNotVax=0,timeVax=0,vaccinated=0,ltbi=0,atbi=0)
res <- vector("list",length=17)
res[[1]] <- newPeople[1,]
res[[1]] <- res[[1]][-1]
for(i in 1:17){
  if(i!=1) res[[i]] <- copy(res[[i-1]])
  res[[i]][,age:=age+1]
  res[[i]] <- rbind(res[[i]],newPeople)
  res[[i]][,year:=2000+i]
  N <- nrow(res[[i]])
  if(i<=9){
    res[[i]][sample(1:N,size=round(N*0.050)),ltbi:=1]
    res[[i]][,timeNotVax:=timeNotVax+1]
  } else {
    res[[i]][sample(1:N,size=round(N*0.050)),ltbi:=1]
    res[[i]][,timeVax:=timeVax+1]
  }
  
  
  Nltbi <- sum(res[[i]]$ltbi)
  res[[i]][sample(which(res[[i]]$ltbi==1),size=round(Nltbi*0.05)),atbi:=1]
}


stanData=res[[17]]
data = list(N=nrow(stanData),
            y=stanData$ltbi,
            timeNotVax=stanData$timeNotVax,
            timeVax=stanData$timeVax)

fit <- stan('model.stan', # 'hmm-fit-semisup.stan',
            data=data,
            iter=2000, chains=1, init=0)

summary(fit)$summary

stanData=res[[17]]
data = list(N=nrow(stanData),
            y=stanData$atbi,
            time=stanData$timeNotVax+stanData$timeVax)

fit <- stan('model_tb.stan', # 'hmm-fit-semisup.stan',
            data=data,
            iter=2000, chains=1, init=0)

summary(fit)$summary



mean(res[[i]]$ltbi)

