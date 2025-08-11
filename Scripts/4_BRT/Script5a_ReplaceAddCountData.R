#Replace IMBCR and GBMP data and add BBS data to the existing species RDATA files (created by Qing) that get fed into the BRT models
#This is a temporary script that will be replaced once we have a clean data wrangle workflow from raw data
#download to offset calculations
#This script is only written for species being considered for the drought response analysis, but could be replicated for other species
library(tidyverse)
library(dplyr)
#load updated count data with offset for species from drought response analysis
load("Data/CountData/Processed/offsets_DroughtResponse.rda")
load("Data/CountData/Processed/species_DroughtResponse.rda")
names(offsets) <- species

#load existing species RDATA file (from Qing)
data.list <- lapply(species, FUN = function(x) {
  get(load(paste0("Data/BRT_Input/", x, ".RData")))
})
names(data.list) <- species
#load xy coordinates from original CGAMP data
xy <- read.csv("Data/CountData/Processed/XY_all.csv") %>% rename(lon = X, lat = Y)

#remove GBMP and IMBCR rows; remove covariate columns; add lat longs
data.list <- lapply(data.list, FUN = function(x) {
  tmp = x
  tmp$pcode = sub(":.*", "", tmp$PKEY) #extract pcode from PKEY
  tmp = tmp %>% filter(!(pcode %in% c("GBM", "IMBCR"))) %>% select(pcode, PKEY, SS, YYYY, Y, Offset) %>% left_join(y = xy, by = "SS") %>% select(-PCODE)
})

#Add updated IMBCR, GBM, and BBS data back in
#create function to merge (x = offsets, y = data.list)
combine.data <- function (x, y) {
  #first manipulate new offsets data
  new = x %>% select(pcode, location, year, month, day, time, lon, lat, abundance, offset) %>% 
    rename(SS = location, YYYY = year, Y = abundance, Offset = offset)
  new$HH = format(new$time, "%H")
  new$Min = format(new$time, "%M")
  new$PKEY = paste(new$pcode, new$SS, paste(new$YYYY, new$month, new$day, new$HH, new$Min, sep = "_"), sep = ":")
  new = select(new, pcode, PKEY, SS, YYYY, Y, Offset, lon, lat)
  #add existing data
  data = rbind(new, y)
  return(data)
} 

final.data <- mapply(combine.data, x = offsets, y = data.list, SIMPLIFY = F)
names(final.data) <- species

#save data
for(i in species) {
  saveRDS(as.data.frame(final.data[[i]]), paste0("Data/BRT_Input/wDrought/", i, ".RData"))
}
mapply(function(x,y) {saveRDS(as.data.frame(x), paste0("Data/BRT_Input/wDrought/", y, ".rda"))}, x = final.data, y = species)
