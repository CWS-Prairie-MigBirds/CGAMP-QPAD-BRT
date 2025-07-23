#This script modifies the extrapolated density rasters in two ways: 1) change all open water pixels to a value of 0,
#and 2) truncate predicted densities to the 99th percentile count ever observed for each species in the data
library(raster)
library(parallel)
setwd("C:/Users/robinsonba/OneDrive - EC-EC/Documents/Projects/CGAMP/Data/")

#####This code only needs to be run once to create the maxDen table. Skip to line 54 if its already created#############
# ####calculate 99th percentile densities by species####
# #import raw point count data
# PC <- read.csv("Data files/CountData/CGAMP_database/PC_all.csv")
# 
# #In order to transform counts into density, need to estimate area surveyed based on point count radii
# #extract max point count radius for each project; for projects with max dist of Inf, count data from the last distance bin (x-inf) will be removed before calculating the max. 
# proj <- read.csv("Data files/CountData/CGAMP_database/Project_all.csv")
# dist <- read.csv("Data files/CountData/CGAMP_database/DISTANCE.csv")
# distLU <- merge(proj, dist[,c("DISTMETH", "DistanceID", "DistStart", "DistEnd","MaxDist")], by.x = "Distance_Method", by.y = "DISTMETH")
# 
# #make list of projects that have inf max distance and DistanceID associated with the x-inf dist bin.
# remove <- distLU[which(distLU$MaxDist==Inf&distLU$DistEnd==Inf),]
# remove <- paste(remove$Project, remove$DistanceID, sep = "_")
# 
# #remove counts from projects-distance bin combinations listed in remove
# PC$remove <- paste(PC$Project, PC$Distance_Level, sep = "_")
# pcTrunc <- PC[!(PC$remove %in% remove),]
# 
# #sum counts across distance bins (excluding those with Inf as the endpoint)
# maxY <- aggregate(pcTrunc$Abundance, by = list(Project = pcTrunc$Project, species = pcTrunc$Species, PKEY = pcTrunc$PKEY), FUN = "sum")
# 
# #identify point count radii for each survey
# radii <- distLU[which(distLU$DistEnd==distLU$MaxDist),c("Project", "DistStart", "DistEnd", "MaxDist")]
# radii$radii <- radii$MaxDist
# radii[which(radii$MaxDist==Inf),"radii"] <- radii[which(radii$MaxDist==Inf),"DistStart"]
# maxY <- merge(maxY, radii[,c("Project", "radii")])
# 
# #transform max counts to density, ensuring you choose the line below with the desired units
# #INDIVIDUALS/HA
# #maxY$density <- maxY$x/((pi*maxY$radii^2)/10000)
# 
# #INDIVIDUALS/QUARTER SECTION
# #maxY$density <- maxY$x/((pi*maxY$radii^2)/647497)
# 
# #INDIVIDUALS/PIXEL (800x800m = 640000m^2)
# maxY$density <- maxY$x/((pi*maxY$radii^2)/640000)
# 
# #calculate 99th percentile to remove outliers
# maxDen <- aggregate(maxY$density, by = list(species = maxY$species), FUN = "quantile", probs = 0.99)
# colnames(maxDen) <- c("species", "max")
# 
# #save maxDen so it can be used later
# #write.csv(maxDen, "Data Files/CountData/maxDen_qs.csv")
# #write.csv(maxDen, "Data Files/CountData/maxDen_pixel.csv")
# #max.LARB <- maxDen[which(maxDen$species=="LARB"),"max"]
#######################################################################################################################################
maxDen <- read.csv("C:/Users/robinsonba/OneDrive - EC-EC/Documents/Projects/CGAMP/Data/Data Files/CountData/maxDen_pixel.csv")

#####Truncate density estimates to 99th percentile observed (upper), change densities <0.5/pixel to 0, emove water pixels in parallel####
#Load density rasters
#list all rasters with _50.tif in file name (i.e. the median density raster)
#****MAKE SURE TO CHOOSE THE LINE BELOW WITH THE APPROPRIATE FOLDER, DEPENDING ON THE PROJECT BEING WORKED ON
# files <- list.files("GIS files/Rasters/PrelimDensityRasters", pattern = "_50.tif", recursive = T, full.names = T)
# files <- list.files("GIS files/Rasters/ReverseAuction/PrelimDensityRasters", pattern = "_50.tif", recursive = T, full.names = T)
# files <- list.files("GIS files/Rasters/PrelimDensityRasters_Qing", pattern = "median", full.names = T)
files <- list.files("GIS files/Rasters/MeanDensityRastersAcrossYears", full.names = T)

#apply list of desired species if applicable
# species <- "LARB"
# species <- "CCLO|LARB"
# files <- files[grepl(pattern = species, files)]
den <- lapply(files, raster)

#ONLY RUN THIS LINE IF THE UNITS OF THE DENSITY RASTERS NEEDS TO BE ADJUSTED
#Change units to individuals per pixel (800x800m = 640,000). Qing's extrapolated models are in n/m^2
den <- lapply(den, FUN = function(x) {return(x*640000)})

#create directory for truncated density rasters
#****MAKE SURE TO CHOOSE THE LINE BELOW WITH THE APPROPRIATE FOLDER, DEPENDING ON THE PROJECT BEING WORKED ON
# dir.create("GIS files/Rasters/FinalDensityRasters", showWarnings = F)
# dir.create("GIS files/Rasters/ReverseAuction/FinalDensityRasters", showWarnings = F)
dir.create("GIS files/Rasters/PrelimDensityRasters_Qing")
           
#define function to work in parallel; note maxDen needs to be loaded into the cluster for this function to work
#****MAKE SURE TO CHOOSE THE LINE BELOW WITH THE APPROPRIATE FOLDER, DEPENDING ON THE PROJECT BEING WORKED ON
trunc.par <- function(x) {
  #load open water raster
  water <- raster("C:/Users/robinsonba/OneDrive - EC-EC/Documents/Projects/CGAMP/Data/GIS files/Rasters/CGAMP_covariates/Final_final/Landcover/Continuous/wat2021.tif")
  
  #crop water raster to match species density raster in case they have different extents
  water <- crop(x = water, y = x)
  
  #change open water pixels to 0
  x[water == 1] = 0
  
  #isolate maxDen threshold for appropriate species
  #RUN APPROPRIATE LINE BASED ON FILE NAMES. FIRST LINE IS FOR MY FILES, SECOND IS FOR QING'S
  sp = substr(names(x), 1, 4)
  # sp = substr(names(x), 8, 11)
  thres = maxDen[maxDen$species == sp, "max"]
  
  #truncate to maxDen threshold
  x[x > thres] = thres
  
  #truncate all pixels with a value <0.5 individuals/pixel to 0
  x[x < 0.5] = 0
  
  #save updated raster
  # dir = paste0("GIS files/Rasters/FinalDensityRasters/", sp, "/")
  dir = paste0("GIS files/Rasters/FinalDensityRasters_Qing/", sp, "/")
  # dir = paste0("GIS files/Rasters/ReverseAuction/FinalDensityRasters/", sp, "/")
  dir.create(dir, showWarnings = F)
  writeRaster(x, filename = paste0(dir, names(x), "_trunc.tif"))
  return(x)
}

#remove objects no longer needed to save RAM
rm(dist, distLU, maxY, PC, pcTrunc, proj, radii, remove, files, species)

#implement function in parallel
cl <- makeCluster(min(detectCores()-1, length(den)))
#load packages into cluster
clusterEvalQ(cl, {library(raster)})
#move required objects to cluster
clusterExport(cl, c("maxDen"))
#run trunc.par
trunc.rasters <- parLapply(cl = cl, X = den, fun = trunc.par)
stopCluster(cl)

####Calculate mean (and sd) of truncated rasters across years if desired####
larb.stack <- stack(trunc.rasters)
setwd("C:/Users/robinsonba/OneDrive - EC-EC/Documents/Projects/CGAMP/Data/GIS files/Rasters/MeanDensityRastersAcrossYears")
mean.larb <- calc(larb.stack, mean, filename = "LARB_mean_2012-2021.tif")
sd.larb <- calc(larb.stack, sd, filename = "LARB_sd_2012-2021.tif")
