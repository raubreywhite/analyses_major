org::AllowFileManipulationFromInitialiseProject()
org::InitialiseProject(
  HOME = "/git/code_major/2017/water_longitudinal/",
  RAW = "/Volumes/crypt_data/org/data_raw/code_major/2017/water_longitudinal/",
  SHARED = "/dropbox/analyses/results_shared/code_major/2017/water_longitudinal/"
)

library(data.table)
library(pbmcapply)

# water exposures
data <- data.table(x = SampleWaterExposure(10000))
meanWater <- mean(data$x)
data <- data[, .(y = .N), by = .(x)]
data[, p := y / sum(y)]

## number people sick
d1025 <- copy(data)
setorder(d1025,x)
d1025[, truth := 0.0645 * (1.05 ^ x)]
d1025[, truthRisk :=round(truth*100,1)]
d1025[, waterMean := 0.0645 * (1.05 ^ meanWater)]
d1025[x < meanWater, waterMean := truth]
d1025[, waterZero := 0.0645 * (1.05 ^ 0)]

d1025[, truth := round(173000*12*truth*p)]
d1025[, waterMean := round(173000 * 12 * waterMean * p)]
d1025[, waterZero := round(173000 * 12 * waterZero * p)]
d1025[, casesCaused := truth - waterZero]
d1025[, num := round(173000 * p)]
d1025 <- d1025[, c("x", "truthRisk", "num", "truth", "casesCaused"), with = F]
d1025
apply(d1025,2,sum)

d1025 <- copy(data)
setorder(d1025, x)
d1025[, truth := 0.066 * (1.0 ^ x)]
d1025[, truthRisk := round(truth * 100, 1)]
d1025[, waterMean := 0.066 * (1.0 ^ meanWater)]
d1025[x < meanWater, waterMean := truth]
d1025[, waterZero := 0.066 * (1.0 ^ 0)]

d1025[, truth := round((5000000-173000) * 12 * truth * p)]
d1025[, waterMean := round((5000000 - 173000) * 12 * waterMean * p)]
d1025[, waterZero := round((5000000 - 173000) * 12 * waterZero * p)]
d1025[, casesCaused := truth - waterZero]
d1025[, num := round((5000000 - 173000) * p)]
d1025 <- d1025[, c("x", "truthRisk", "num", "truth", "casesCaused"), with = F]
d1025
apply(d1025, 2, sum)

#
res <- pbmclapply(1:400,
                  function(x) RunMultipleRisk(npeople=5600),
                  mc.cores = parallel::detectCores()
)
res <- rbindlist(res)
res[,.(
  powerInteraction=mean(anovaPval<0.05),
  powerCombined=mean(pvalCombined<0.05)
)]

sampleSizes <- c(8600,1950)
saveRDS(sampleSizes[1], "results_final/n_large_sample.RDS")
saveRDS(sampleSizes[2], "results_final/n_small_sample.RDS")


res <- vector("list",100)
pb <- txtProgressBar(min=1,max=length(res),style=3)
for(i in 1:length(res)){
  res[[i]] <- RunMultipleRisk(sampleSizes[1])
  setTxtProgressBar(pb,i)
}
close(pb)

saveRDS(res, "results_final/res_large_sample.RDS")

res <- vector("list",100)
res <- pbmclapply(1:10,
                  function(x) RunMultipleRisk(sampleSizes[1]),
                  mc.cores = parallel::detectCores()
)
pb <- txtProgressBar(min=1,max=length(res),style=3)
for(i in 1:length(res)){
  res[[i]] <- RunMultipleRisk(sampleSizes[2])
  setTxtProgressBar(pb,i)
}
close(pb)

saveRDS(res, "results_final/res_small_sample.RDS")

resLarge <- rbindlist(readRDS("results_final/res_large_sample.RDS"))
resSmall <- rbindlist(readRDS("results_final/res_small_sample.RDS"))

saveRDS(mean(resLarge$anovaPval<0.05),"results_final/power_interaction_large.RDS")
saveRDS(mean(resSmall$anovaPval<0.05),"results_final/power_interaction_small.RDS")

# Self contained HTML file
# - Copying base64 images to word/docx won't work
# - But you can email this to people and it will still work
RAWmisc::RmdToHTMLDOCX("reports_skeleton/report.Rmd",paste0("reports_formatted/HTMLReport_",format(Sys.time(), "%Y_%m_%d"),".html"), copyFrom="reports_skeleton")



