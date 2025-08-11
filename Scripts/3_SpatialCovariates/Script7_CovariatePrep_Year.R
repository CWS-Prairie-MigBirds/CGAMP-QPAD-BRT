#Create a covariate raster for year (YYYY) to be used for model extrapolation

library(terra)

#load template raster
template <- rast("Data/SpatialCovariates/cgamp_ras.tif")

#populate with years of interest and save
#define years of interest
years <- c(2012:2019)
YYYY.list <- lapply(years, FUN = function(x) {
  yyyy.tmp = template
  yyyy.tmp[!is.na(yyyy.tmp)] = x
  names(yyyy.tmp) = paste0("YYYY",x)
  return(yyyy.tmp)
})

lapply(YYYY.list, FUN = function(x) {
  writeRaster(x, paste0("Data/SpatialCovariates/YYYY/",names(x), ".tif"))
})

