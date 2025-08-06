---
title: "Extrapolate BRT bootstrap models to a specific year"
author: "Barry Robinson"
date: "November 27, 2019"
output: html_document
---

#load list of bootstrap models and raster files to extrapolate to

library(terra)
library(dismo)
library(gbm)
#Enter the year(s) you are extrapolating to
year <- as.character(c(2020))

#create list of all bootstrap models
models <- list.files("Output/Models_bootstrap")

#run one of these line if you want to add or remove models based on a list of species***
#models <- models[!grepl(pattern = "CASP|CONI|EAME|NOBO|STGR", models)]
species <- "BAIS|BOBO|CASP|CCLO|CONI|EAME|GRSP|LARB|LOSH|LBCU|MAGO|NOBO|SPPI|TBLO|UPSA|WEME"
models <- models[grep(pattern = species, models)]

#create list of models to extrapolate
ext.list <- vector(mode="list", length=length(models)*length(year))
count <- 1
for(i in 1:length(models)) {
  for(j in year) {
    ext.list[[count]] <- list(model=models[[i]], year=j)
    count = count+1
  }
}

#assign names that will be given to extrapolated rasters to each element of ext.list
names(ext.list) <- sapply(ext.list, 
                          FUN = function(x) {
                            end = gregexpr("\\.RD", x$model)[[1]][1]-1
                            name = paste0(substr(x$model, 1, end),"_", x$year,".tif")
                            return(name)
                          })
rm(count, i, j, models)

#check if any models have already been extrapolated and remove from the list
if(dir.exists("Output/BootstrapDensityRasters")) {
  done <- list.files("Output/BootstrapDensityRasters", full.names = F, recursive = T, include.dirs = F)
  ext.list <- ext.list[names(ext.list) %in% setdiff(names(ext.list),done)]
  rm(done)
} else {
  dir.create("Output/BootstrapDensityRasters")
}


#create function to load model and covariates needed for extrapolation in parellel

extrapolate <- function(x) {
  require(dismo)
  require(terra)
  require(gbm)
  #list all files in BRT Covariates directory associated with the correct year
  file.list = list.files("Data/SpatialCovariates", pattern = paste0(x$year, "|CHILI|TPI|TRI"), recursive = T)
  
  #remove binomial or continuous versions of landcover data, depending on what version of model you're using
  file.list = file.list[!grepl(pattern = "Binomial", x = file.list)]

  #remove sample weight raster
  file.list = file.list[!(file.list %in% paste0("SampleWeight/wt", x$year,".tif"))]
  
  #remove RAP rasters if not working on southern great plains only
  file.list = file.list[!grepl(pattern = "RAP", x = file.list)]
  
  #import file.list into raster.list
  raster.list = lapply(paste0("Data/SpatialCovariates/", file.list),rast)
  
  #extract name of each raster from file.list, remove the year portion, and apply it to raster.list
  name.list = vector(length=length(raster.list))
  for (i in 1:length(raster.list)) {
    start = gregexpr("[/]", file.list[i])[[1]]
    if(length(start)==2) {start <- start[2]+1} else {start <- start[1]+1}
    end <- gregexpr("[.]", file.list[i])[[1]][1]-1
    name.list[[i]] = substr(file.list[[i]],start,end)
    name.list[i] = gsub(paste0("_", x$year),"",name.list[i])
    name.list[i] = gsub(x$year,"",name.list[i])

    # test = as.vector(gregexpr("[_]", name.list[i])[[1]])
    # if(nchar(name.list[[i]]) %in% test) {
    #   name.list[[i]] = substr(name.list[[i]], 1, nchar(name.list[[i]])-1)
    # }
  }
  names(raster.list) = name.list
  
  #turn raster.list into a raster stack
  covars = rast(raster.list)
  
  #load model and extrapolate using covariates from required year
  model = readRDS(paste0("Output/Models_bootstrap/",x$model))
  den = suppressWarnings(predict(covars, model, type="response", n.trees=model$n.trees, na.rm = T))
  den = den*640000 #convert to males/pixel (males/64ha)
  
  #change open water pixel to have a values of 0
  #load open water raster
  water <- rast("Data/SpatialCovariates/Landcover/wat2021.tif")
  #crop water raster to match species density raster in case they have different extents
  water <- crop(x = water, y = den)
  #change open water pixels to 0
  den[water == 1] = 0
  
  #truncate rasters to 99th percentile of densities every observed in the dataset (previously calcuated and saved in maxDen)
  #isolate maxDen threshold for appropriate species
  maxDen = read.csv("Data/CountData/Processed/maxDen_pixel.csv")
  sp = substr(x$model, 1, 4)
  thres = maxDen[maxDen$species == sp, "max"]
  #truncate to maxDen threshold
  den[den > thres] = thres
  
  #truncate all pixels with a value < 0.5 individuals/pixel (0.0078125 individuals/ha) to 0
  den[den < 0.5] = 0
  
  #define filename for raster
  end = gregexpr("\\.RD", x$model)[[1]][1]-1
  file = paste0(substr(x$model, 1, end),"_", x$year,".tif")
  #save to Temp directory because final raster will be based on median of bootstrap rasters
  writeRaster(den,filename = paste0("Output/BootstrapDensityRasters/", file))
  #den = raster(paste(dir,file, sep="/"))
  #return(den)
}



#implement above function in parallel
library(parallel)
cl <- makeCluster(5)
clusterEvalQ(cl,{
  library(terra)
  library(dismo)
  library(gbm)
  }) #load packages in the cluster
#clusterExport(cl,c("ext.list","extrapolate")) #move required objects to the cluster
ext.model <- parLapply(cl=cl, X=ext.list, fun=extrapolate)
stopCluster(cl)

rm(list = ls())
gc()
