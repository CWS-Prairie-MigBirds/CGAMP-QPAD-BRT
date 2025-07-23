# ---
# title: "Estimate population size with CI from BRT bootstrap models"
# author: "Barry Robinson"
# date: "September 18, 2019"
# output: html_document
# ---

####Setup workspace and import extrapolated model###
library(terra)

#setwd("C:/Users/robinsonba/Documents/Projects/Priority Areas Analysis/Data/Results/BRT_rasters_bootstrap")
#setwd("C:/Users/robinsonba/OneDrive - EC-EC/Documents/Projects/CGAMP/Data/GIS files/Rasters/BootstrapDensityRasters")

# create list of species based on folder names within Temp directory
# species <- list.dirs(recursive = F,full.names = FALSE)

#create list of species based on first 4 letters of bootstrap density available in the folder
species <- unique(substr(list.files("Output/BootstrapDensityRasters", pattern = ".tif"), 1, 4))

#If desirable, query species of interest
species <- species[species %in% c("BAIS", "CASP", "CCLO", "LARB", "SPPI", "TBLO")]

#create list of species included in the analysis
# species <- unique(substr(files, 1,4))

#create list of raster stacks, 1 stack per species, each with 100 rasters from bootstrap resamples
stacks <- lapply(species, FUN = function(x) {require(terra)
                                             return(rast(list.files("Output/BootstrapDensityRasters", pattern = x, full.names = T)))
                                             })

#give species name to each list
names(stacks) <- species

#If using PIF pair corrections, import PIF population estimate table and extract species-specific pair adjustments
pif <- read.csv("Data/LookupTables/PopEstsGlobal2020.04.29.csv")
IBP <- read.csv("Data/LookupTables/IBPSpeciesCodes.csv", stringsAsFactors = F)
#change LeConte's Sparrow to match
IBP[IBP$SPEC=="LCSP","COMMONNAME"] <- "LeConte's Sparrow"
pif <- merge(pif,IBP[,1:2], by.x = "English.Name", by.y = "COMMONNAME")
#change mclo to tblo
pif$SPEC[which(pif$SPEC=="MCLO")] <- "TBLO"
pif <- pif[which(pif$SPEC %in% species),which(colnames(pif)=="SPEC"|colnames(pif)=="Pair.Adjust.Category")]
pif <- pif[which(!(duplicated(pif$SPEC))),]
#add pair correction for LBCU
#pif <- rbind(pif, c(1.20, "LBCU"))

rm(IBP)

#load or create stratification polygons being used. If no strata are being used, set strata to null
strata <- NULL
#Canadian Priarie Provinces
# strata <- getData('GADM', country="CAN", level=0) #download province polygons
# strata <- st_as_sf(strata)

# prov = c("Alberta", "Saskatchewan", "Manitoba")
# can = can[can$NAME_1 %in% prov,]

################################################

#BCRxState/Province interactions
# st_layers(dsn = "C:/Users/robinsonba/OneDrive - EC-EC/Documents/Projects/CGAMP/Data/GIS files/Shapefiles/BCR_Master_v10.gdb")
# strata <- st_read(dsn = "C:/Users/robinsonba/OneDrive - EC-EC/Documents/Projects/CGAMP/Data/GIS files/Shapefiles/BCR_Master_v10.gdb", layer = "BCR_Terrestrial_master")
# strata$strata <- paste(strata$BCR, strata$PROVINCE_S, sep = "_")
# strata <- st_transform(x = strata, crs = crs(stacks[[1]][[1]]))
# #Isolate only those BCRs wanted
# strata <- strata[strata$BCR %in% c(11, 17, 18, 19, 20, 21),]
# #remove polygons with open water
# strata <- strata[strata$WATER == 3,]

################################################

#Proposed IBS/KBAs from Birds Canada
# strata <- st_read(dsn = "C:/Users/robinsonba/OneDrive - EC-EC/Documents/Projects/CGAMP/Data/Temp/PrairieKBAsforBarry", layer = "PrairieKBAsforBarry2")
# strata <- st_transform(x = strata, crs = crs(stacks[[1]]))

# strata <- vect("C:/Users/robinsonba/OneDrive - EC-EC/Documents/Projects/CGAMP/Data/Temp/PrairieKBAsforBarry/PrairieKBAsforBarry2.shp")
# strata <- project(x = strata, y = stacks[[1]])

#Run below if there are multiple shapefiles that need to be combined
# files <- list.files("C:/Users/robinsonba/OneDrive - EC-EC/Documents/Projects/CGAMP/Data/Temp/PrairieKBAsforBarry", pattern = ".shp", full.names = T)
# strata <- lapply(files, st_read)
# #check that crs matches across all layers
# unique(sapply(strata, st_crs))
# #combine into 1 shapefile
# strata <- do.call(what = sf:::rbind.sf, args=strata)

#the sf polygon for IBAs spans all of Canada, so need to crop the east, north, and west boundaries using
#the species density rasters, but maintain southern extent.
# extent <- c(st_bbox(stacks[[1]])["xmin"], st_bbox(strata)["ymin"],
#             st_bbox(stacks[[1]])["xmax"], st_bbox(stacks[[1]])["ymax"])
# strata <- st_crop(strata, extent)

#Create a list of lists with all inputs needed for function in parallel.
list <- vector(mode = "list", length = length(species))
names(list) <- species
for(i in species) {
  list[[i]] <- list(stacks = stacks[[i]], pcorrect = pif$Pair.Adjust.Category[pif$SPEC==i], quants = c(0.025, 0.50, 0.975), strata = strata, species = i)
}
rm(strata, stacks, pif, files, i)
gc()
names(list) <- species

#####estiamte population for entire study area for each species and bootstrap sample, then calculate 0.05, 0.50, and 0.95 quantiles####
#define function the estaimte population size and CI in parellel. In the function below, x is a stack of 100 boostrap models for a specie. pcorrect is either a table with pair corrections by species or a single, user
#defined number for the pair correction. quant are the quntiles the user wants reported for pop estimates. stata is a polygon that defines strata in which to estimate population size. 
popEst <- function(x) {
  require(terra)
  stacks = x$stacks
  sp = x$species
  pcorrect = x$pcorrect
  quants = x$quants
  strata = x$strata
  #bootstrap models are now in individuals/pixel and they have already been rounded down from <0.5 to 0, so below steps are no longer needed.
  # #calculate area of each pixel in ha 
  # pixArea = round(prod(res(stacks))) #area in m^2
  # pixArea = pixArea*0.0001 #convert to ha
  # #multiply value of each pixel (individuals/ha) by pixel area (ha) to get individuals/pixel
  # temp = stacks*pixArea
  # #Round any pixels with <0.5 individuals down to 0
  # temp[temp < 0.5] = 0
  #import stratification polygon and reproject
  if(!is.null(strata)) {
    strata <- project(x = strata, y = stacks)
    #strata = st_transform(x = strata, crs = crs(stacks))
    #sum all pixels in each raster separately for each province to get initial population estimate for each bootstrap sample (males only for some species)
    PopEst_boot = exact_extract(x=stacks, y=strata, fun='sum')
    rownames(PopEst_boot) = strata$nationalna
    pa = as.numeric(pcorrect[which(pcorrect$SPEC == sp),1])
    PopEst_boot = PopEst_boot*pa
    saveRDS(PopEst_boot, file = paste0("C:/Users/robinsonba/OneDrive - EC-EC/Documents/Projects/CGAMP/Data/Results/PopEsts/","PopEst_boot_kba",sp,".RData"))
    PopEst = apply(PopEst_boot, 1, FUN = function(y) {return(quantile(y, probs = quants))})
  } else {
    PopEst_boot = colSums(values(stacks), na.rm = T)
    PopEst = quantile(PopEst_boot, probs = quants)
  }
  
  # #assign pair correction based on user input for pcorrect
  # if(is.data.frame(pcorrect)) {
  #   #extract species-specific pair adjustment **pif table must be loaded into the cluster**
  #   sp = substr(names(stacks)[[1]], start=1, stop=4)
  #   if(sp %in% pcorrect$SPEC) {
  #     pa = pcorrect[which(pcorrect$SPEC == sp),1]
  #   } else {pa = 2}
  # } else {pa = pcorrect}
  
  # #apply species-specific pair adjustment
  # PopEst_boot = PopEst_boot*pa
  #estimate median and 95% CIs
  
  # test = apply(temp, 1, FUN = "mean")
  # colnames(PopEst)
  return(PopEst)
}
PopEst <- lapply(list, FUN = popEst)
write.csv(t(as.data.frame(PopEst)), "Output/PopulationEsts/PopEsts_continental_2021.csv")

#"BAIS" "BOBO" "CCLO" "LARB" "LBCU" "SPPI" "TBLO"

# BAIS <- popEst(list[[1]])
# BOBO <- popEst(list[[2]])
# CCLO <- popEst(list[[3]])
# LARB <- popEst(list[[4]])
# LBCU <- popEst(list[[5]])
# SPPI <- popEst(list[[6]])
# TBLO <- popEst(list[[7]])
# 
# write.csv(BAIS, "C:/Users/robinsonba/OneDrive - EC-EC/Documents/Projects/CGAMP/Data/Results/PopEsts/BAIS_Canada_2021.csv")
# write.csv(CCLO, "C:/Users/robinsonba/OneDrive - EC-EC/Documents/Projects/CGAMP/Data/Results/PopEsts/CCLO_Canada_2021.csv")
# write.csv(BOBO, "C:/Users/robinsonba/OneDrive - EC-EC/Documents/Projects/CGAMP/Data/Results/PopEsts/BOBO_Canada_2021.csv")
# write.csv(LARB, "C:/Users/robinsonba/OneDrive - EC-EC/Documents/Projects/CGAMP/Data/Results/PopEsts/LARB_Canada_2021.csv")
# write.csv(LBCU, "C:/Users/robinsonba/OneDrive - EC-EC/Documents/Projects/CGAMP/Data/Results/PopEsts/LBCU_Canada_2021.csv")
# write.csv(SPPI, "C:/Users/robinsonba/OneDrive - EC-EC/Documents/Projects/CGAMP/Data/Results/PopEsts/SPPI_Canada_2021.csv")
# write.csv(TBLO, "C:/Users/robinsonba/OneDrive - EC-EC/Documents/Projects/CGAMP/Data/Results/PopEsts/TBLO_Canada_2021.csv")
#If running for one species, just used the above function
# lbcu <- popEst(stacks[[1]], pcorrect = 1.2, quants = c(0.025, 0.50, 0.975))
# write.csv(lbcu, "C:/Users/robinsonba/OneDrive - EC-EC/Documents/Projects/CGAMP/Data/Results/PopEsts/lbcu_2021.csv")

#implement above function in parallel (if running for multiple species)
library(parallel)
cl <- makeCluster(min(length(list),detectCores()-1))
clusterEvalQ(cl,{library(terra)}) #load packages in the cluster
#clusterExport(cl,c("popEst")) #move required objects to the cluster
PopQuants <- parLapply(cl=cl, X=list, fun=popEst)
stopCluster(cl)
#export PopQUants
write.csv(PopQuants[["BAIS"]], "C:/Users/robinsonba/OneDrive - EC-EC/Documents/Projects/CGAMP/Data/Results/PopEsts/BAIS_PrairieKBAs_2021.csv")
write.csv(PopQuants[["CCLO"]], "C:/Users/robinsonba/OneDrive - EC-EC/Documents/Projects/CGAMP/Data/Results/PopEsts/CCLO_PrairieKBAs_2021.csv")
write.csv(PopQuants[["BOBO"]], "C:/Users/robinsonba/OneDrive - EC-EC/Documents/Projects/CGAMP/Data/Results/PopEsts/BOBO_PrairieKBAs_2021.csv")
write.csv(PopQuants[["LARB"]], "C:/Users/robinsonba/OneDrive - EC-EC/Documents/Projects/CGAMP/Data/Results/PopEsts/LARB_PrairieKBAs_2021.csv")
write.csv(PopQuants[["LBCU"]], "C:/Users/robinsonba/OneDrive - EC-EC/Documents/Projects/CGAMP/Data/Results/PopEsts/LBCU_PrairieKBAs_2021.csv")
write.csv(PopQuants[["SPPI"]], "C:/Users/robinsonba/OneDrive - EC-EC/Documents/Projects/CGAMP/Data/Results/PopEsts/SPPI_PrairieKBAs_2021.csv")
write.csv(PopQuants[["TBLO"]], "C:/Users/robinsonba/OneDrive - EC-EC/Documents/Projects/CGAMP/Data/Results/PopEsts/TBLO_PrairieKBAs_2021.csv")


####summarize population estimates (median) and CIs into a table####
Pops <- do.call(rbind,PopQuants)
Pops <- as.data.frame(Pops)
Pops$species <- species
# setwd("C:/Users/robinsonba/Documents/Projects/Priority Areas Analysis/Data/Results/")
# dir.create("PopEstimates",showWarnings = F)
# write.csv(Pops, file="PopEstimates/PopEstimates_2018.csv", row.names = F)

#for some reason, 8 bootstrap sample for GRSP lead to a HUGE population estimates (1 billion +), while the rest of the bootstrap samples have more similar estimates (<3.8 million). Remove the outliers and recalculate
x <- stacks[[which(names(stacks)=="GRSP")]]
pixArea = round(prod(res(x[[1]]))) #area in m^2
pixArea = pixArea*0.0001 #convert to ha
#multipy value of each pixel (males/ha) by pixel area to get males/pixel
temp = x*pixArea
#sum pixels in each raster and multiply by 2 to get population estimate for each bootstrap sample (males and females)
temp = cellStats(temp, stat = 'sum')
temp = temp*pif[which(pif$SPEC == "GRSP"),1]

#look at range of values
temp[order(temp)]

#remove all estimates >=3.9 million
temp = temp[which(temp<3.9e+06)]

#recalculate quantiles
GRSP <- quantile(temp,probs=c(0.05,0.50,0.95))

#replace values in Pops table
Pops[which(Pops$species=="GRSP"), c(1:3)] <- GRSP

####export final population estimates####
setwd("C:/Users/robinsonba/Documents/Projects/Priority Areas Analysis/Data/Results/")
dir.create("PopEstimates",showWarnings = F)
write.csv(Pops, file="PopEstimates/PopEstimates_2018.csv", row.names = F)






#Old Code used to explore different methods of estimating population size. Shouldn't be needed now that I'm using bootstrap samples to estimate population size


#calculate area of each pixel in hectars (will be identical for all rasters)
pixArea <- round(prod(res(stacks[[1]][[1]]))) #area in m^2
pixArea <- pixArea*0.0001 #convert to ha

#multiple value of each pixel (males/ha) by pixel size to get males/pixel
stacks <- lapply(stacks,FUN = function(x) {return(x*pixArea)})

#sum value of all pixels in the raster
cclo.pop <- sum(cclo.pix[],na.rm=T)
cclo.pop*2 #multiply by 2 to get males and females.

#look at a histogram of pixel values to determien why estimates are so large
breaks <- hist(cclo.pix)$breaks
counts <- hist(cclo.pix)$counts

#the vast majority of pixels have values <500, but pixels with larger values are likely contrtibuting toward the huge estimate
1-sum(counts[2:16])/sum(counts) #99.995% of pixel have values <500

#only sum up pixels with values <500 and see what population estimate is
cclo.500 <- cclo.pix[which(cclo.pix[]<=500)]
sum(cclo.500, na.rm=T)

#isolate only those pixels within the 95% quantile (remove largest 5% of pixels)
q95 <- quantile(cclo.pix[],probs=c(0.95), na.rm=T)
cclo.q95 <- cclo.pix[which(cclo.pix[]<=q95)]
sum(cclo.q95,na.rm=T)

q75 <- quantile(cclo.pix[],probs=c(0.75), na.rm=T)
cclo.q75 <- cclo.pix[which(cclo.pix[]<=q75)]
sum(cclo.q75,na.rm=T)

grass <- raster("C:/Users/robinsonba/Documents/Projects/Priority Areas Analysis/Data/GIS/Rasters/ACI/grass2018.tif")
crop <- raster("C:/Users/robinsonba/Documents/Projects/Priority Areas Analysis/Data/GIS/Rasters/ACI/crop2018.tif")
cclo.pix <- projectRaster(from=cclo.pix, to=grass)
cclo.grass <- cclo.pix*grass
hist(cclo.grass[]
     median(cclo.grass[],na.rm=T)
     sorted <- sort(cclo.grass[])
     head(sorted)
     
     #count number of pixels containing grassland
     grass.pix <- sum(grass[],na.rm=T)
     #multiply by pixel area
     grass.m2 <- grass.pix*round(prod(res(grass)))
     grass.km2 <- grass.m2*1e-6
     grass.km2*80
     
     
     
     
     