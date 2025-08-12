#This is a temporary script that extract covarites from individual species RDATa files that have already been created
#Once I clean the workflow, covariate extraction will occur prior to splitting count data into separate files for each species.

library(terra)
library(tidyverse)
library(dplyr)
#list all files in working directory (should only contain rasters needed for BRT models)
file.list <- list.files("Data/SpatialCovariates", pattern = "tif|nc|TIF", recursive = T, full.names = T)

#import file.list into raster.list
raster.list <- lapply(file.list, FUN = rast)

#extract name of each raster from file.list and apply it to raster.list
name.list <- lapply(file.list, FUN = function(x) {
  tmp = sub(".*/", "", x)
  tmp = sub("\\..*", "", tmp)
  return(tmp)
})
names(raster.list) <- unlist(name.list)
rm(file.list)

#create list with each element containing a stack of rasters for each year **change year range if needed**
years <- as.character(2012:2021)
stack.list <- vector(mode="list", length=length(years))
names(stack.list) <- years
for (i in years) {
  #extract names containing the appropriate year (i) plus topography layers
  temp = c(grep(i,name.list,value=T),grep("CHILI|TPI|TRI",name.list,value=T))  
  stack.list[[i]] <- rast(raster.list[which(names(raster.list) %in% temp)]) #stack all rasters from appropriate year (i)
  rm(temp,i)
}
rm(raster.list)


#load count data for 1 species (all species should have the exact same set of locations, so only need to load 1) and isolate PKEY, YYYY, lon, and lat
pkey <- readRDS("Data/BRT_Input/wDrought/CCLO.rda") %>% select(PKEY, YYYY, lon, lat)

#create a list of point count coordinates where each element contains coordinates for a given year. Turn coords into a SpatVect and reproject to any covariate raster
xy.list <- lapply(years, FUN = function(x) {
  points = filter(pkey, YYYY == x)
  points = vect(points, geom = c("lon","lat"), crs = "EPSG:4326")
  points = project(x = points, y = crs(stack.list[[1]]))
  return(points)
})

#create list of lists containing XY locations of point counts, raster stack objects, and PKEY values for each year
input.list <- vector(mode = "list", length = length(stack.list))
for(i in 1:length(stack.list)) {
  input.list[[i]] <- list(stack=stack.list[[i]], pts=xy.list[[i]], pkey=xy.list[[i]]$PKEY)
}

#create function to extract raster values for each year in parallel and create data.frame
extract.par <- function(x) {
  require(terra)
  covars = terra::extract(x=x$stack, y=x$pts, ID=F, method='simple')
  covars$PKEY = x$pkey
  return(covars)
}
covars <- lapply(input.list, FUN = extract.par)

#combine covars across all years into a single dataframe and merge with species count data
#remove year portion of colnames, so that they match across years and add year column
names(covars) <- years
for (i in years) {
  colnames(covars[[i]]) <- gsub(i,"",colnames(covars[[i]]))
  #covars[[i]]$YYYY <- as.numeric(i)
}

#rbind into one dataframe and remove ID column
covars <- do.call(rbind,covars)
summary(covars)

#export covar names as an object to use for BRT modelling
covar.names <- colnames(covars)
covar.names <- covar.names[which(covar.names!="PKEY")]
covar.names <- covar.names[which(covar.names!="wt")]
saveRDS(covar.names, "Data/BRT_Input/covarnames.rda")

#change drought NA values to "ND" and make them factors
covars$drought_[is.na(covars$drought_)] <- "ND"
covars$drought_L[is.na(covars$drought_L)] <- "ND"

#convert drought columns to ordinal factors
drought.order <- c("ND", 0, 1, 2, 3, 4)
covars$drought_ <- ordered(covars$drought_, levels = drought.order)
covars$drought_L <- ordered(covars$drought_L, levels = drought.order)

#check for NAs
apply(covars,2,function(x)any(is.na(x)))
#4 rows with NA values in YYYY, and all SPEI covariates
covars[is.na(covars$YYYY),]
#4 points from twedt2020 are giving NAs
plot(xy.list[[3]][which(xy.list[[3]]$PKEY %in% c("twedt2020:twedt2020_3986:2014_5_26_10_34", 
                                                 "twedt2020:twedt2020_4006:2014_5_26_10_34",
                                                 "twedt2020:twedt2020_3986:2015_5_25_10_34",
                                                 "twedt2020:twedt2020_4006:2015_5_25_10_34"))],)

#I think those 4 points counts are in the periphery, so just remove
covars <- covars[complete.cases(covars),]

#load species-specific counts and offsets, merge with covars, and export
count.list <- list.files(pattern = ".rda","Data/BRT_Input/wDrought", full.names = T)
#load count data and remove unnecessary columns
count.list <- lapply(count.list, function(x) {
  tmp = readRDS(x) %>% select(PKEY, Y, Offset, lat)
  return(tmp)
  })

#merge count data with covars and export
species <- list.files(pattern = ".rda", "Data/BRT_Input/wDrought")
species <- gsub("\\..*", "", species)
names(count.list) <- species
for (i in species) {
  count.list[[i]] <- left_join(x = covars, y = count.list[[i]], by = "PKEY")
  #remove all data south of 44th latitude to restrict analysis to Northern Great Plains
  data <- count.list[[i]] %>% filter(lat >=44)
  data$Offset = log(data$Offset)
  saveRDS(data, file=paste0("Data/BRT_Input/wDrought/final/", i, ".rda"))
  rm(data)
}

#check to make sure data looks ok
data.list <- list.files(pattern = ".rda","Data/BRT_Input/wDrought/final", full.names = T)
#load count data and remove unnecessary columns
data.list <- lapply(data.list, readRDS)

data.list_ <- list.files(pattern = ".rda","Data/BRT_Input/wDrought/", full.names = T)
#load count data and remove unnecessary columns
data.list_ <- lapply(data.list_, readRDS)
larb <- readRDS("Data/BRT_Input/wDrought/LARB.rda")
