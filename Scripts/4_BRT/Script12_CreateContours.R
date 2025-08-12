#create contour lines/isopleths to represent population core areas
library(terra)
require(smoothr)
library(sf)
library(tidyverse)
#load raster layer of interest
larb <- rast("Output/FinalDensityRasters/LARB_2021_50_trunc.tif")
plot(larb)
#calculate quntiles and create contours
quants <- quantile(larb[larb>=0.5], probs = c(0.2, 0.5, 0.8), na.rm = T)
cont <- as.contour(larb, levels = quants)
plot(cont)
#smooth contours to look nicer
cont_drop <- drop_crumbs(cont, threshold = 500)
plot(cont_drop)
cont_smooth = smooth(cont, method = "ksmooth", smoothness = 8) 
cont_drop = 
plot(cont)

