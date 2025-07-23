library(rgdal)
library(raster)
prov <- readOGR(dsn="Y:/Documents/GIS/Priority Areas Analysis/PHJV_TargetLand", layer="PHJV_Target_Landscapes_v2")
ext <- extent(prov)
r <- raster(ext, res=0.01)
provRaster <- rasterize(prov, r)
plot(provRaster)
template <- raster()

provRaster <- projectRaster(from = provRaster, to = template, method="ngb",
                            file=paste0("W:/Priority_Areas_Analysis/Analysis/Zonation/InputRasters/Province.tif"), overwrite=TRUE)

IBA <- readOGR(dsn="W:/Priority_Areas_Analysis/Temp/Zonation", layer="CanadaIBA")
ext <- extent(template)
r <- raster(template, res=400)
IBARaster <- rasterize(IBA, r, field=1)
plot(IBARaster)
plot(IBA)

IBARaster <- projectRaster(from = IBARaster, to = template, method="ngb",
                          file="W:/Priority_Areas_Analysis/Analysis/Zonation/InputRasters/IBA.tif")
