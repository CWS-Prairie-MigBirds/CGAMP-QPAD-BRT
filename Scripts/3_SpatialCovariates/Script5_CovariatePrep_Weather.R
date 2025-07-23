---
title: "Script 5 - Weather covariate preparation"
author: "Barry Robinson"
date: "September 19, 2019"
output: html_document
---

####Download require Daymet data (monthly precipitation and daily tmin tmax) in raster format####

library(daymetr)
library(raster)
library(ncdf4)
setwd("C:/Users/robinsonba/Documents/Projects/Priority Areas Analysis/Data/Temp")

#Use the extent of ACI layer to query daymet data
template <- raster("C:/Users/robinsonba/Documents/Projects/Priority Areas Analysis/Data/GIS/Rasters/BRT_covariates/ACI/grass2019.tif")
extent <- projectRaster(from = template, crs = "+proj=longlat") #need lat/long coordinates
extent <- c(ymax(extent),xmin(extent),ymin(extent),xmax(extent)) #order required for package to work
rm(template)

#define the years for which data are downloaded
years <- as.character(2009:2018)

#download Daymet data
dir.create("PrecipMonthly")
download_daymet_ncss(location = extent, start = years[1], end = years[length(years)], frequency = "monthly", param = "prcp", path = "PrecipMonthly")
dir.create("TminDaily")
download_daymet_ncss(location = extent, start = years[1], end = years[length(years)], frequency = "daily", param = "tmin", path = "TminDaily")
dir.create("TmaxDaily")
download_daymet_ncss(location = extent, start = years[1], end = years[length(years)], frequency = "daily", param = "tmax", path = "TmaxDaily")
dir.create("TminMonthly")
download_daymet_ncss(location = extent, start = years[1], end = years[length(years)], frequency = "monthly", param = "tmin", path = "TminMonthly")
dir.create("TmaxMonthly")
download_daymet_ncss(location = extent, start = years[1], end = years[length(years)], frequency = "monthly", param = "tmax", path = "TmaxMonthly")
dir.create("PAS")
download_daymet_ncss(location = extent, start = years[1], end = years[length(years)], frequency = "daily", param = "swe", path = "PAS")

# #the above downloads a file for each year, each of which has 12 bands (1 for each month); import dataset into a list of raster stacks
# prcp <- lapply(list.files(pattern="ncss[.]nc"),stack)
# #set the appropriate projection
# for (i in 1:length(prcp)) {
#   projection(prcp[[i]]) <- "+proj=lcc +lat_1=25 +lat_2=60 +lat_0=42.5 +lon_0=-100 +x_0=0 +y_0=0 +ellps=WGS84 +units=km +no_defs"
# }
# 
# plot(prcp[[1]])



####calculate total summer precipitation for each year (MSP)####

library(raster)
library(ncdf4)
#define the years for which data are downloaded
years <- as.character(2019)

#import monthly temperature values for each year
setwd("C:/Users/robinsonba/Documents/Projects/Priority Areas Analysis/Data/Temp/PrecipMonthly")
prcp <- lapply(list.files(),stack)
names(prcp) <- years

#correct the projection for all stacks
for (i in 1:length(prcp)) {
  projection(prcp[[i]]) <- "+proj=lcc +lat_1=25 +lat_2=60 +lat_0=42.5 +lon_0=-100 +x_0=0 +y_0=0 +ellps=WGS84 +units=km +no_defs"
}

#create list of lists for input into the SUMP function defined below. Input includes the monthly precip raster stacks and file names
prcp.list <- vector(mode="list", length = length(prcp))
names(prcp.list) <- years
for (i in years) {
  prcp.list[[i]] <- list(raster=prcp[[i]], filename=(paste0("MSP_",i,".nc")))
}

#Define function for calculating summer precip in parrallel
SUMP <- function(x) {
  calc(x=x$raster[[5:9]], fun=sum, filename=x$filename)
  return(raster(x$filename))
}

#create directory for MSP
dir.create("C:/Users/robinsonba/Documents/Projects/Priority Areas Analysis/Data/Temp/MSP")
setwd("C:/Users/robinsonba/Documents/Projects/Priority Areas Analysis/Data/Temp/MSP")

#implement GDD function in parallel
library(parallel)
cl <- makeCluster(length(prcp.list)) #assign a core to each year
clusterEvalQ(cl,library(raster)) #load package in the cluster
clusterExport(cl,"prcp.list"); clusterExport(cl,"SUMP") #move required objects to the cluster
MSP.list <- parLapply(cl=cl, X=prcp.list, fun=SUMP)
names(MSP.list) <- years
stopCluster(cl)



###Calculate total annual precipitation (MAP)####

#change filename in prcp.list
for (i in years) {
  prcp.list[[i]]$filename <- (paste0("MAP_",i,".nc"))
}

#Define function for calculating annual precip in parrallel
AP <- function(x) {
  calc(x=x$raster, fun=sum, filename=x$filename)
  return(raster(x$filename))
}

#create directory for MSP
dir.create("C:/Users/robinsonba/Documents/Projects/Priority Areas Analysis/Data/Temp/MAP")
setwd("C:/Users/robinsonba/Documents/Projects/Priority Areas Analysis/Data/Temp/MAP")

#implement AP function in parallel
library(parallel)
cl <- makeCluster(length(prcp.list)) #assign a core to each year
clusterEvalQ(cl,library(raster)) #load package in the cluster
clusterExport(cl,"prcp.list"); clusterExport(cl,"AP") #move required objects to the cluster
MAP.list <- parLapply(cl=cl, X=prcp.list, fun=AP)
names(MSP.list) <- years
stopCluster(cl)



####Calculate mean temp of warmest month (MWMT)####

library(daymetr)
library(raster)
library(ncdf4)
#define the years for which data are to be downloaded
years <- as.character(2009:2019)

#import monthly min and max temperatures into a list of raster stacks
setwd("C:/Users/robinsonba/Documents/Projects/Priority Areas Analysis/Data/Temp/TminMonthly")
tmin <- lapply(list.files(),stack)

setwd("C:/Users/robinsonba/Documents/Projects/Priority Areas Analysis/Data/Temp/TmaxMonthly")
tmax <- lapply(list.files(),stack)

names(tmin) <- years
names(tmax) <- years

#correct the projection for all stacks
for (i in 1:length(tmin)) {
  projection(tmin[[i]]) <- "+proj=lcc +lat_1=25 +lat_2=60 +lat_0=42.5 +lon_0=-100 +x_0=0 +y_0=0 +ellps=WGS84 +units=km +no_defs"
  projection(tmax[[i]]) <- "+proj=lcc +lat_1=25 +lat_2=60 +lat_0=42.5 +lon_0=-100 +x_0=0 +y_0=0 +ellps=WGS84 +units=km +no_defs"
}

temp.list <- vector(mode="list", length = length(tmin))
names(temp.list) <- years
for (i in years) {
  temp.list[[i]] <- list(tmin=tmin[[i]], tmax=tmax[[i]], filename=(paste0("MWMT_",i,".nc")))
}

#remove tmin and tmax because they are now stored in temp.list
rm(tmin,tmax)

#Define function to calculate MWMT in parallel
MWMT <- function(x) {
  overlay(x=x$tmin, y=x$tmax, fun=function(z,w) {return((z+w)/2)}, filename=paste0("temp1_",x$filename))
  temp = stack(paste0("temp1_",x$filename))
  calc(x=temp, fun=max, filename=x$filename)
  unlink(paste0("temp1_",x$filename))
  return(raster(x$filename))
}

#create directory for MWMT
dir.create("C:/Users/robinsonba/Documents/Projects/Priority Areas Analysis/Data/Temp/MWMT", showWarnings = F)
setwd("C:/Users/robinsonba/Documents/Projects/Priority Areas Analysis/Data/Temp/MWMT")

#implement GDD function in parallel
memory.limit(size=10000000) #set no limit to memory allocation
library(parallel)
cl <- makeCluster(min(detectCores()-1, length(temp.list))) #assign 3 cores only to limit RAM bottleneck
clusterEvalQ(cl,library(raster)) #load package in the cluster
clusterExport(cl,"temp.list"); clusterExport(cl,"MWMT") #move required objects to the cluster
MWMT.list <- parLapply(cl=cl, X=temp.list, fun=MWMT)
names(MWMT.list) <- years
stopCluster(cl)



####Calculate summer heat moisture index (SHM = MWMT/(MSP/1000))####

library(raster)
library(ncdf4)

#define the years for which data are to be downloaded
years <- as.character(2009:2019)

#import MWMT and MSP layers for each year
setwd("C:/Users/robinsonba/Documents/Projects/Priority Areas Analysis/Data/Temp/MWMT")
MWMT.list <- lapply(list.files(), raster)
names(MWMT.list) <- years
setwd("C:/Users/robinsonba/Documents/Projects/Priority Areas Analysis/Data/Temp/MSP")
MSP.list <- lapply(list.files(), raster)
names(MSP.list) <- years

#create empty list for SHM
temp.list <- vector(mode="list", length=length(MWMT.list))
names(temp.list) <- years
#calculate SHM for each year
for (i in years) {
  temp.list[[i]] <- list(MWMT=MWMT.list[[i]], MSP=MSP.list[[i]], filename=paste0("SHM_",i,".nc"))
}

rm(MSP.list,MWMT.list)

#Define function for calculating SHM
SHM <- function(x) {
  overlay(x=x$MWMT, y=x$MSP, fun=function(w,z) {return(w/(z/1000))}, filename=x$filename)
  return(raster(x$filename))
}

#create directory for SHM
dir.create("C:/Users/robinsonba/Documents/Projects/Priority Areas Analysis/Data/Temp/SHM")
setwd("C:/Users/robinsonba/Documents/Projects/Priority Areas Analysis/Data/Temp/SHM")

#implement GDD function in parallel
memory.limit(size=10000000) #set no limit to memory allocation
library(parallel)
cl <- makeCluster(min(detectCores()-1, length(temp.list))) #assign 3 cores only to limit RAM bottleneck
clusterEvalQ(cl,library(raster)) #load package in the cluster
clusterExport(cl,"temp.list"); clusterExport(cl,"SHM") #move required objects to the cluster
SHM.list <- parLapply(cl=cl, X=temp.list, fun=SHM)
names(SHM.list) <- years
stopCluster(cl)



####calculate growing degree days above 5C####

library(daymetr)
library(raster)
library(ncdf4)
#define the years for which data are to be downloaded
years <- as.character(2019)

#import daily min and max temperatures into a list of raster stacks
setwd("C:/Users/robinsonba/Documents/Projects/Priority Areas Analysis/Data/Temp/TminDaily")
tmin <- lapply(list.files(),stack)

setwd("C:/Users/robinsonba/Documents/Projects/Priority Areas Analysis/Data/Temp/TmaxDaily")
tmax <- lapply(list.files(),stack)

names(tmin) <- years
names(tmax) <- years

#correct the projection for all stacks
for (i in 1:length(tmin)) {
  projection(tmin[[i]]) <- "+proj=lcc +lat_1=25 +lat_2=60 +lat_0=42.5 +lon_0=-100 +x_0=0 +y_0=0 +ellps=WGS84 +units=km +no_defs"
  projection(tmax[[i]]) <- "+proj=lcc +lat_1=25 +lat_2=60 +lat_0=42.5 +lon_0=-100 +x_0=0 +y_0=0 +ellps=WGS84 +units=km +no_defs"
}

#create list of lists for input into the DD5 function defined below. Input includes the tmin and tmax rasters and file name
temp.list <- vector(mode="list", length = length(tmin))
names(temp.list) <- years
for (i in years) {
  temp.list[[i]] <- list(tmin=tmin[[i]], tmax=tmax[[i]], filename=(paste0("DD5_",i,".nc")))
}

#remove tmin and tmax because they are now stored in temp.list
rm(tmin,tmax)

#Define function to calculate DD5 in parallel
DD5 <- function(x) {
  overlay(x=x$tmin, y=x$tmax, fun=function(z,w) {return(((z+w)/2)-5)}, filename=paste0("temp1_",x$filename))
  temp = stack(paste0("temp1_",x$filename))
  calc(x=temp, fun=function(y) {y[y < 0] <- 0; return(y)}, filename=paste0("temp2_",x$filename))
  unlink(paste0("temp1_",x$filename))
  temp = stack(paste0("temp2_",x$filename))
  calc(x=temp, fun=sum, filename=x$filename)
  unlink(paste0("temp2_",x$filename))
  return(raster(x$filename))
}

#create directory for GDD
dir.create("C:/Users/robinsonba/Documents/Projects/Priority Areas Analysis/Data/Temp/DD5", showWarnings = F)
setwd("C:/Users/robinsonba/Documents/Projects/Priority Areas Analysis/Data/Temp/DD5")

#implement GDD function in parallel
memory.limit(size=10000000) #set no limit to memory allocation
library(parallel)
cl <- makeCluster(min(detectCores()-1, length(temp.list))) #assign 3 cores only to limit RAM bottleneck
clusterEvalQ(cl,library(raster)) #load package in the cluster
clusterExport(cl,"temp.list"); clusterExport(cl,"DD5") #move required objects to the cluster
DD5.list <- parLapply(cl=cl, X=temp.list, fun=DD5)
names(DD5.list) <- years
stopCluster(cl)

# work flow has changed so that all climate variables are reprojected together at the end and transfered out of the temp directory
# #create list of lists to input into projectRaster function for parrellel computing
# template <- raster("C:/Users/robinsonba/Documents/Projects/Priority Areas Analysis/Data/GIS/Rasters/BRT_covariates/ACI/grass2018.tif")
# 
# for (i in years) {
#   DD5.list[[i]] <- list(raster=DD5.list[[i]], template = template, filename=time.list[[i]]$filename)
# }
# 
# #define projectRaster function for multi-core processing 
# pR.par <- function(x) {
#   require(raster)
#   projectRaster(from=x$from, to=x$to, method="bilinear", filename=x$filename, overwrite=T)
# }
# 
# #set up clusters for multi-core processing
# cl <- makeCluster(length(DD5.list)/2) #only proccess half at once to minimize RAM bottleneck
# clusterEvalQ(cl,library(raster)) #load package in the cluster
# clusterExport(cl,"DD5.list"); clusterExport(cl,"pR.par") #move required objects to the cluster
# 
# #run projectRaster in parallel
# DD5.repro <- parLapply(cl=cl, X=DD5.list, fun=pR.par)
# stopCluster(cl)



####calculate degree days below 0C####

###Uncomment and run code below if min and max temperature data needs to be imported############################################
# library(daymetr)
# library(raster)
# library(ncdf4)
# #define the years for which data are to be downloaded
# years <- as.character(2009:2018)
# 
# #import daily min and max temperatures into a list of raster stacks
# setwd("C:/Users/robinsonba/Documents/Projects/Priority Areas Analysis/Data/Temp/TminDaily")
# tmin <- lapply(list.files(),stack)
# 
# setwd("C:/Users/robinsonba/Documents/Projects/Priority Areas Analysis/Data/Temp/TmaxDaily")
# tmax <- lapply(list.files(),stack)
# 
# names(tmin) <- years
# names(tmax) <- years
# 
# #correct the projection for all stacks
# for (i in 1:length(tmin)) {
#   projection(tmin[[i]]) <- "+proj=lcc +lat_1=25 +lat_2=60 +lat_0=42.5 +lon_0=-100 +x_0=0 +y_0=0 +ellps=WGS84 +units=km +no_defs"
#   projection(tmax[[i]]) <- "+proj=lcc +lat_1=25 +lat_2=60 +lat_0=42.5 +lon_0=-100 +x_0=0 +y_0=0 +ellps=WGS84 +units=km +no_defs"
# }
# 
# #create list of lists for input into the GDD function defined below. Input includes the tmin and tmax rasters and file name
# temp.list <- vector(mode="list", length = length(tmin))
# names(temp.list) <- years
################################################################################################################################

#update file name for DD_0
for (i in years) {
  temp.list[[i]]$filename <- (paste0("DD_0_",i,".nc"))
}

#remove tmin and tmax because they are now stored in temp.list
rm(tmin,tmax)

#Define function to calculate DD_0
DD_0 <- function(x) {
  overlay(x=x$tmin, y=x$tmax, fun=function(z,w) {return(0-((z+w)/2))}, filename=paste0("temp1_",x$filename))
  temp = stack(paste0("temp1_",x$filename))
  calc(x=temp, fun=function(y) {y[y < 0] <- 0; return(y)}, filename=paste0("temp2_",x$filename))
  unlink(paste0("temp1_",x$filename))
  temp = stack(paste0("temp2_",x$filename))
  calc(x=temp, fun=sum, filename=x$filename)
  unlink(paste0("temp2_",x$filename))
  return(raster(x$filename))
}

#create directory for GDD
dir.create("C:/Users/robinsonba/Documents/Projects/Priority Areas Analysis/Data/Temp/DD_0", showWarnings = F)
setwd("C:/Users/robinsonba/Documents/Projects/Priority Areas Analysis/Data/Temp/DD_0")

#implement GDD function in parallel
memory.limit(size=10000000) #set no limit to memory allocation
library(parallel)
cl <- makeCluster(min(detectCores()-1, length(temp.list))) #assign a core for each year
clusterEvalQ(cl,library(raster)) #load package in the cluster
clusterExport(cl,"temp.list"); clusterExport(cl,"DD_0") #move required objects to the cluster
DD_0.list <- parLapply(cl=cl, X=temp.list, fun=DD_0)
names(DD_0.list) <- years
stopCluster(cl)

#work flow has changed so that all climate variables are reprojected together at the end and transfered out of the temp directory
# #create list of lists to input into projectRaster function for parrellel computing
# template <- raster("C:/Users/robinsonba/Documents/Projects/Priority Areas Analysis/Data/GIS/Rasters/BRT_covariates/ACI/grass2018.tif")
# 
# for (i in years) {
#   DD_0.list[[i]] <- list(raster=DD_0.list[[i]], template = template, filename=time.list[[i]]$filename)
# }
# 
# #define projectRaster function for multi-core processing 
# pR.par <- function(x) {
#   require(raster)
#   projectRaster(from=x$from, to=x$to, method="bilinear", filename=x$filename, overwrite=T)
# }
# 
# #set up clusters for multi-core processing
# cl <- makeCluster(length(DD_0.list)/2) #only proccess half at once to minimize RAM bottleneck
# clusterEvalQ(cl,library(raster)) #load package in the cluster
# clusterExport(cl,"DD_0.list"); clusterExport(cl,"pR.par") #move required objects to the cluster
# 
# #run projectRaster in parallel
# DD_0.repro <- parLapply(cl=cl, X=DD_0.list, fun=pR.par)
# stopCluster(cl)



####Calculate number of frost-free days####

##Uncomment and run code below if min temperature data needs to be imported############################################
library(daymetr)
library(raster)
library(ncdf4)
#define the years for which data are to be downloaded
years <- as.character(2019)

#import daily min and max temperatures into a list of raster stacks
setwd("C:/Users/robinsonba/Documents/Projects/Priority Areas Analysis/Data/Temp/TminDaily")
tmin <- lapply(list.files(),stack)


names(tmin) <- years

#correct the projection for all stacks
for (i in 1:length(tmin)) {
  projection(tmin[[i]]) <- "+proj=lcc +lat_1=25 +lat_2=60 +lat_0=42.5 +lon_0=-100 +x_0=0 +y_0=0 +ellps=WGS84 +units=km +no_defs"
}

#create list of lists for input into the GDD function defined below. Input includes the tmin and tmax rasters and file name
temp.list <- vector(mode="list", length = length(tmin))
names(temp.list) <- years
###############################################################################################################################

#update file name for NFFD
for (i in years) {
  temp.list[[i]] <- list(tmin = tmin[[i]], filename = paste0("NFFD_",i,".nc"))
}

rm(tmin)

#define function to calculate number of frost free days
NFFD <- function(x) {
  require(raster)
  calc(x=x$tmin, fun=function(y) {y[y >= 0] <- 1; y[y < 0] <- 0;  return(y)}, filename=paste0("temp1_",x$filename))
  temp=stack(paste0("temp1_",x$filename))
  calc(x=temp, fun=sum, filename=x$filename)
  unlink(paste0("temp1_",x$filename))
  return(raster(x$filename))
}

#create directory for NFFD
dir.create("C:/Users/robinsonba/Documents/Projects/Priority Areas Analysis/Data/Temp/NFFD", showWarnings = F)
setwd("C:/Users/robinsonba/Documents/Projects/Priority Areas Analysis/Data/Temp/NFFD")

#implement NFFD function in parallel
memory.limit(size=10000000) #set no limit to memory allocation
require(parallel)
cl <- makeCluster(min(detectCores()-1, length(temp.list))) #assign a core for each year
clusterEvalQ(cl,library(raster)) #load package in the cluster
clusterExport(cl,"temp.list"); clusterExport(cl,"NFFD") #move required objects to the cluster
NFFD.list <- parLapply(cl=cl, X=temp.list, fun=NFFD)
names(NFFD.list) <- years
stopCluster(cl)



####Reproject all final weather variables to match projection of other spatial covariates####

#import all weather variables
library(raster)
library(ncdf4)
setwd("C:/Users/robinsonba/Documents/Projects/Priority Areas Analysis/Data/Temp/")
raster.list <- lapply(list.files(pattern = "DD_0|DD5|MAP|MSP|MWMT|NFFD|SHM", recursive=T), raster)

#create list of lists to input into projectRaster function for parrellel computing
template <- raster("C:/Users/robinsonba/Documents/Projects/Priority Areas Analysis/Data/GIS/Rasters/BRT_covariates/ACI/grass2019.tif")

for (i in 1:length(raster.list)) {
  raster.list[[i]] <- list(from=raster.list[[i]], to=template, filename=list.files(pattern = "DD_0|DD5|MAP|MSP|MWMT|NFFD|SHM", recursive=T)[i])
}

#define projectRaster function for multi-core processing 
pR.par <- function(x) {
  require(raster)
  projectRaster(from=x$from, to=x$to, method="bilinear", filename=x$filename)
}

#change working directory so that reprojected files are saved outside of temp folder
dir.create("C:/Users/robinsonba/Documents/Projects/Priority Areas Analysis/Data/GIS/Rasters/BRT_covariates/Climate", showWarnings = FALSE)
setwd("C:/Users/robinsonba/Documents/Projects/Priority Areas Analysis/Data/GIS/Rasters/BRT_covariates/Climate")
dirs <- list.dirs("C:/Users/robinsonba/Documents/Projects/Priority Areas Analysis/Data/Temp", full.names=F, recursive=F)
dirs <- dirs[grep(pattern = "DD_0|DD5|MAP|MSP|MWMT|NFFD|SHM", dirs)]
sapply(X = dirs, FUN=dir.create, showWarnings = FALSE)

#set up clusters for multi-core processing
library(parallel)
cl <- makeCluster(min(detectCores()-1, length(raster.list))) #only proccess half at once to minimize RAM bottleneck
clusterEvalQ(cl,library(raster)) #load package in the cluster
clusterExport(cl,"raster.list"); clusterExport(cl,"pR.par") #move required objects to the cluster

#run projectRaster in parallel
reproj.list <- parLapply(cl=cl, X=raster.list, fun=pR.par)
stopCluster(cl)



#####Calculate annual precipication as snow (PAS). THIS VARIABLE IS NOT USED IN THE MODELS SO DOESN'T NEED TO BE ESTIMATED####

library(raster)
library(ncdf4)
#define the years for which data are downloaded
years <- as.character(2009:2018)

#import monthly temperature values for each year
setwd("C:/Users/robinsonba/Documents/Projects/Priority Areas Analysis/Data/Temp/PAS")
swe <- lapply(list.files(),stack)
names(swe) <- years

#correct the projection for all stacks
for (i in years) {
  projection(swe[[i]]) <- "+proj=lcc +lat_1=25 +lat_2=60 +lat_0=42.5 +lon_0=-100 +x_0=0 +y_0=0 +ellps=WGS84 +units=km +no_defs"
}

#create list of lists for input into the PAS function defined below. Input includes the daily snow-water equivilant raster stacks and file names
swe.list <- vector(mode="list", length = length(swe))
names(swe.list) <- years
for (i in years) {
  swe.list[[i]] <- list(raster=swe[[i]], filename=(paste0("PAS_",i,".nc")))
}

#Define function for calculating total annual precipication as snow
PAS <- function(x) {
  calc(x=x$raster, fun=sum, filename=x$filename)
  return(raster(x$filename))
}

#create directory for MSP
dir.create("C:/Users/robinsonba/Documents/Projects/Priority Areas Analysis/Data/Temp/PAS")
setwd("C:/Users/robinsonba/Documents/Projects/Priority Areas Analysis/Data/Temp/PAS")

#implement GDD function in parallel
library(parallel)
cl <- makeCluster(length(swe.list)) #assign a core to each year
clusterEvalQ(cl,library(raster)) #load package in the cluster
clusterExport(cl,"swe.list"); clusterExport(cl,"PAS") #move required objects to the cluster
PAS.list <- parLapply(cl=cl, X=swe.list, fun=PAS)
names(PAS.list) <- years
stopCluster(cl)

#delete daily swe files
unlink(list.files(pattern="swe"))

