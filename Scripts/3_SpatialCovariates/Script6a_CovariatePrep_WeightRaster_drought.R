#temporary script to create weight raster from data for the drought analysis. This script will have to be modified once the workflow is cleaned up
#Create weight raster based on inverse of the total number of point count locations in 24.8 x 24.8 km grid cells (31x31 pixels)
library(terra)
library(tidyverse)
library(dplyr)
#load any existing species RData files to get PKEYs, YYYY, and Lon/Lat for each survey
XY <- readRDS("Data/BRT_Input/wDrought/CCLO.rda") %>% select(PKEY, YYYY, lon, lat)

#create list of XY locations with an element for each year
years <- as.character(2012:2021)
xy.list <- vector(mode="list", length=length(years))
names(xy.list) <- years

#create list of sampling weight rasters with an element for each year
wtRaster.list <- vector(mode="list", length=length(years))
names(wtRaster.list) <- years

template <- rast("Data/RawSpatialCovariates/cgamp_ras.tif")
#populate xy.list and wtRaster list and export weight rasters
for (i in years) {
  #extract point count loations for each year
  xy.list[[i]] <- XY %>% filter(YYYY == i) %>% select(lon, lat)
  #turn into SpatVector object
  xy.list[[i]] <- vect(xy.list[[i]], geom = c("lon","lat"), crs = "EPSG:4326")
  #reproject to match template raster
  xy.list[[i]] <- project(xy.list[[i]], y = crs(template))
  #create a raster where cell values represent the number of point count locations wihtin each cell in a given year
  wtRaster.list[[i]] <- terra::rasterize(x = xy.list[[i]], y = template, fun = 'length')
  #count number of point counts in each 31x31 pixel roving window for each year
  wtRaster.list[[i]] <- focal(wtRaster.list[[i]], w=31, fun = sum, na.rm=T)
  #calculate weight as inverse (higher weight for point counts in more sparsely sampled pixels)
  wtRaster.list[[i]] <- 1/wtRaster.list[[i]]
  #export raster
  writeRaster(wtRaster.list[[i]], filename=paste0("Data/SpatialCovariates/SampleWeight/wt",i,".tif"))
}