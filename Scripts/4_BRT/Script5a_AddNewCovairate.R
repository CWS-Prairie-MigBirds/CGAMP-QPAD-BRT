#This script adds new spatial covariates to exisitng species RDATA files that have already been created for intput into Boosted Regression Trees
#Code is written to add in drought indices for each PKEY based on location (SS) and year (YYYY) point count was conducted
library(sf)
library(terra)
library(dplyr)

#load in count data linked with existing covariates. RDATA files should be identical across species, except for the count column, so load any species to begin with
load("C:/RProjects/CGAMP-QPAD-BRT/Output/BRT_Input/CCLO.RData")

#Load XY_all to get lat/longs for each PKEY and create sf
xy <- read.csv("Data/CountData/XY_all.csv")
xy.loc <- merge(data[,c("PKEY", "SS", "YYYY")], xy[,c("SS", "X", "Y")], by = "SS")
#check for NAs
any(is.na(xy.loc[,"X"]))
any(is.na(xy.loc[,"Y"]))
xy.loc <- st_as_sf(xy.loc, coords = c("X", "Y"), crs = 4326)

#load drought polygon file
drought <- st_read("Data/SpatialCovariates/drought_polygons.gpkg")

#rasterize the drought polygons, creating separate rasters for each year.
template <- rast("Data/SpatialCovariates/cgamp_ras.tif") #load CGAMP covariate raster template
drought <- st_transform(drought, crs = crs(template)) #match projections
xy.loc <- st_transform(xy.loc, crs = crs(template))
drought <- drought[drought$year %in% unique(xy.loc$YYYY),] #remove rasters for years that have no count data
years <- unique(drought$year)
drought.ras <- lapply(years, FUN = function(x) {
  tmp = drought[drought$year == x,]
  tmp = rasterize(x=tmp, y=template, field = "severity")
  names(tmp) = x
  return(tmp)
})
names(drought.ras) <- years

#extract drought covariate for each PKEY, linking to correct year
xy.drought <- lapply(drought.ras, FUN = function(x) {
  year.tmp = names(x)
  xy.tmp = xy.loc[xy.loc$YYYY == year.tmp,]
  drought.tmp = extract(x = x, y = xy.tmp, method = 'simple', ID = F)
  xy.tmp = cbind(xy.tmp, drought.tmp)
  xy.tmp = rename(xy.tmp, drought = paste0("X",year.tmp)) #rename drought column
  xy.tmp = st_drop_geometry(xy.tmp)
  return(xy.tmp)
})

#combine across years into a single table
drought.final <- do.call(rbind, xy.drought)

#Add new drought covariate to existing species RDATA files
#List species of interest
species <- c("CCLO", "LARB", "SPPI")
lapply(species, function(x) {
  load(paste0("Output/BRT_Input/", x, ".RData"))
  data = merge(data,drought.final[,c("PKEY","drought")], by = "PKEY")
  save(data, file = paste0("Output/BRT_Input/",x,"_drought.RData"))
})

any(is.na(xy.drought[[1]]$drought))






