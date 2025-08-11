#Create drought rasters from the Canada drought monitor product and SPEI for year of survey and year-1
library(terra)
library(sf)
#isolate years of interest
years <- 2012:2021

###Canada Drought Monitor
#load drought polygon file
drought <- st_read("Data/SpatialCovariates/drought_polygons.gpkg")

#rasterize the drought polygons, creating separate rasters for each year.
template <- rast("Data/SpatialCovariates/cgamp_ras.tif") #load CGAMP covariate raster template
drought <- st_transform(drought, crs = crs(template)) #match projections

# drought <- drought[drought$year %in% unique(xy.loc$YYYY),] #remove rasters for years that have no count data
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
rm(drought)

###SPEI
#isolate years of interest
years <- 2012:2021
#annual rasters for month of May
may <- rast("Data/SpatialCovariates/spei_raw/spei_1_may_na.nc")
names(may) <- rep("may", nlyr(may))
#annual rasters for mean may-august
summer <- rast("Data/SpatialCovariates/spei_raw/spei_4_aug_na.nc")
names(summer) <- rep("summer", nlyr(summer))
#annual rasters for 48 month mean
m48 <- rast("Data/SpatialCovariates/spei_raw/spei_48_may_na.nc")
names(m48) <- rep("m48", nlyr(m48))

#create function to extract years of interes
extract.year <- function(x, y) { #x = year, y = raster stack
  year = format(time(y), "%Y")
  raster = y[[which(year==x)]]
  raster.L = y[[which(year==x-1)]]
  raster = project(raster, template)
  raster.L = project(raster.L, template)
  raster[is.na(template)] <- NA
  raster.L[is.na(template)] <- NA
  writeRaster(raster, paste0("Data/SpatialCovariates/SPEI/",names(raster),"_", x,".tif"))
  writeRaster(raster.L, paste0("Data/SpatialCovariates/SPEILag/",names(raster.L), "_L", x, ".tif"))
}

may.final <- lapply(years, FUN = extract.year, y = may)
summer.final <- lapply(years, FUN = extract.year, y = summer)
m48.final <- lapply(years, FUN = extract.year, y = m48)

deparse(substitute(may))
