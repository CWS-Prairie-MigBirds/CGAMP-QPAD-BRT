#Investigate relative influence of variables in boosted regression tree models across bootstrap samples
library(gbm)

#load bootstrap models and obtain summaries
models <- list.files("Output/Models_bootstrap", full.names = T)
models <- lapply(list.files("Output/Models_bootstrap", full.names = T), readRDS)
rel.inf <- lapply(models, summary)

#split by species
rel.inf.merge <- Reduce(function(...) merge(..., by = "var"), rel.inf)
ri.sum <- rowMeans(rel.inf.merge[,2:101])
ri.sum <-cbind(rel.inf.merge$var,ri.sum)
ri.sum <- ri.sum[order(as.numeric(ri.sum[,2]), decreasing = T),]
ri.sum[,2] <- as.numeric(ri.sum[,2])
