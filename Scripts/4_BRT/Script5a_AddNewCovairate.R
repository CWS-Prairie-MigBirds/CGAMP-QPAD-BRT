#This script adds new spatial covariates to exisitng species RDATA files that have already been created for intput into Boosted Regression Trees
#Code is written to add in drought indices for each PKEY based on location (SS) and year (YYYY) point count was conducted
library(sf)
library(terra)
library(dplyr)

#load in count data linked with existing covariates. RDATA files should be identical across species, except for the count column, so load any species to begin with
load("Data/BRT_Input/CCLO.RData")

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
# drought <- drought[drought$year %in% unique(xy.loc$YYYY),] #remove rasters for years that have no count data
years <- unique(xy.loc$YYYY)
drought.ras <- lapply(years, FUN = function(x) {
  tmp = drought[drought$year == x,]
  tmp.L = drought[drought$year == x-1,]
  tmp = rasterize(x=tmp, y=template, field = "severity")
  tmp.L = rasterize(x=tmp.L, y=template, field = "severity")
  names(tmp) = paste0("drought_",x)
  names(tmp.L) = paste0("drought_L",x)
  writeRaster(tmp, paste0("Data/SpatialCovariates/Drought/drought_",x,".tif"))
  writeRaster(tmp.L, paste0("Data/SpatialCovariates/DroughtLag/drought_L",x,".tif"))
  return(c(tmp,tmp.L))
})
names(drought.ras) <- years

#extract drought covariate for each PKEY, linking to correct year
xy.drought <- lapply(drought.ras, FUN = function(x) {
  year.tmp = unique(gsub("[^0-9]", "", names(x)))
  xy.tmp = xy.loc[xy.loc$YYYY == year.tmp,]
  drought.tmp = extract(x = x, y = xy.tmp, method = 'simple', ID = F)
  #replace NAs with "ND" for no drought
  drought.tmp = replace(drought.tmp, is.na(drought.tmp), "ND")
  xy.tmp = cbind(xy.tmp, drought.tmp)
  xy.tmp = rename(xy.tmp, c(drought = paste0("drought_",year.tmp), drought_L = paste0("drought_L",year.tmp))) #rename drought column
  xy.tmp = st_drop_geometry(xy.tmp)
  return(xy.tmp)
})

#combine across years into a single table
drought.final <- do.call(rbind, xy.drought)
drought.order <- c("ND", 0, 1, 2, 3, 4)
#convert drought columns to ordinal factors
drought.final$drought <- ordered(drought.final$drought, levels = drought.order)
drought.final$drought_L <- ordered(drought.final$drought_L, levels = drought.order)

#Add new drought covariate to existing species RDATA files
#List species of interest
species <- c("CCLO", "LARB", "SPPI")
final <- lapply(species, function(x) {
  load(paste0("Data/BRT_Input/", x, ".RData"))
  data = merge(data,drought.final[,c("PKEY","drought","drought_L")], by = "PKEY")
  save(data, file = paste0("Data/BRT_Input/wDrought/",x,".RData"))
  return(data)
})

any(is.na(xy.drought[[1]]$drought))






