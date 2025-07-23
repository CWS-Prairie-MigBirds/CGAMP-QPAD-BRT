# ---
# title: Script 9 - Calculate pixel-based point estimate and 
#         confidence intervals from bootstrap models
# author: "Barry Robinson"
# date: "November 29, 2019"
# output: html_document
# ---

#Load and stack extrapolated bootstrap models

library(terra)
library(stringr)
library(parallel)
#setwd("C:/Users/robinsonba/Documents/Projects/Priority Areas Analysis/Data/Results/BRT_rasters_bootstrap")
#setwd("C:/Users/robinsonba/OneDrive - EC-EC/Documents/Projects/CGAMP/Data/Temp/bootstrapModels")

#create list of species based list of extrapolated bootstrap models or create your own custom list
# species <- list.files("Output/BootstrapDensityRasters", pattern = ".tif", recursive = F,full.names = FALSE)
# species <- unique(substr(species, 1,4))
species <- c("BAIS","BOBO","CASP","CCLO","CONI","EAME","GRSP","LARB","LOSH","LBCU","MAGO","NOBO","SPPI","TBLO","UPSA","WEME")
# species <- species[!(species %in% "TBLO")]
# species <- c("TBLO")

#create list of years over which bootstrap models for given species list have been extrapolated
years <- vector(mode = "list", length = length(species))
names(years) <- species
for (i in species) {
  years[[i]] <- list.files("Output/BootstrapDensityRasters", pattern = i, recursive = F,full.names = FALSE)
  loc = unlist(gregexpr('\\.', years[[i]]))
  years[[i]] <- unique(substr(years[[i]], loc-4, loc-1))
}

#create list of raster stacks, 1 stack per species-year, along with filenames for final rasters
stack.list <- vector(mode = "list", length = length(species)*length(years[[i]]))
count = 1
for(i in species) {
  for (j in 1:length(years[[i]])) {
    files = list.files("Output/BootstrapDensityRasters", pattern = i, full.names = T)
    files = files[grepl(pattern = years[[i]][[j]], x = files)]
    tmp = rast(files)
    filename = paste0(i,"_",years[[i]][[j]],c("_025", "_500", "_975") ,".tif")
    stack.list[[count]] <- list(stack = tmp, folder = i, filename = filename)
    count = count + 1
  }
}

#Check if any species-year combinations have already been completed and remove from list
# complete <- list.files("C:/Users/robinsonba/OneDrive - EC-EC/Documents/Projects/CGAMP/Data/GIS files/Rasters/PrelimDensityRasters",
#                        recursive = T, full.names = F)
complete <- list.files("Output/FinalDensityRasters",recursive = T, full.names = F)
complete <- substr(complete, start = 6, stop = nchar(complete))
remove <- c()
for (i in 1:length(stack.list)) {
  if(all(stack.list[[i]]$filename %in% complete)) {remove <- append(remove,i,length(remove))}
}
stack.list <- stack.list[-remove]

#load rasters into the stack portion of stack.list
# for(i in 1:length(stack.list)) {
#   stack.list[[i]]$stack <- stack(stack.list[[i]]$stack)
# }

#create function to calculate 0.025, 0.5, and 0.975 quantiles for each raster stack in parrelel
quantRaster <- function(x) {
  #setwd("C:/Users/robinsonba/OneDrive - EC-EC/Documents/Projects/CGAMP/Data/GIS files/Rasters/PrelimDensityRasters")
  #setwd("C:/Users/robinsonba/OneDrive - EC-EC/Documents/Projects/CGAMP/Data/GIS files/Rasters/ReverseAuction/PrelimDensityRasters")
  dir = "Output/FinalDensityRasters/"
  dir.create(paste0(dir, x$folder), showWarnings = F)
  temp = app(x$stack, fun = function (y) {quantile(y, probs = c(0.025, 0.5, 0.975), na.rm = T)}, cores = detectCores()-1)
  temp = as.list(temp)
  temp = mapply(FUN = function(z,y) {writeRaster(z, filename = paste0(dir, x$folder, "/", y))}, z = temp, y = x$filename)
  return(temp)
}

FinalRasters <- lapply(stack.list, FUN = "quantRaster")

#implement above function in parrellel
library(parallel)
cl <- makeCluster(min(length(stack.list),detectCores()-1))
clusterEvalQ(cl,{library(terra)}) #load packages in the cluster
#clusterExport(cl,c("quantRaster")) #move required objects to the cluster
quantiles <- parLapply(cl=cl, X=stack.list, fun=quantRaster)
stopCluster(cl)

