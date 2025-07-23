###This code should be used in place of the first code chunk in Geospatial Scritp2_CovariatePrep_NDVI.Rmd to access files that are zipped on the local drive instead of being loaded onto InGeo (2017-19). Only need to run lines 5-32 below to upzip appropriate files into the temp data folder
setwd("C:/Users/robinsonba/Downloads")
#The suffix of each NDVI file name indicates the start and end date of the week the data represent in the form YYDDD.YYDDD, where DDD is Ordinal Day. Find the files that contain the following ordinal days to represent early, mid, and late season NDVI, respectively: 138, 162, 186.

#loop through each year and find the appropriate list of files to copy to temp drive
years <- as.character(2017:2018) #list years to extract data from
file.list <- vector(mode="list", length=length(years))
names(file.list) <- years
#list ordinal days (target day are 138, 162, and 186, but also downloading previous and subsequent weeks to fill cloud gaps)
OD <- c("131","138","145","155","162","169","179","186","193") 
OD.list <- vector(mode="list", length=length(OD))
names(OD.list) <- OD

for (i in years) {
  file.list[[i]] <- OD.list
  dir.create(paste("C:/Users/robinsonba/Documents/Projects/Priority Areas Analysis/Data/Temp",i,sep="/"),showWarnings = F)
  for (j in OD) {
    file.list[[i]][[j]] <- list.files(i)
    file.list[[i]][[j]] <- substr(file.list[[i]][[j]],nchar(file.list[[i]][[j]])-18, nchar(file.list[[i]][[j]])-4)
    file.list[[i]][[j]] <- file.list[[i]][[j]][which(as.numeric(substr(file.list[[i]][[j]],5,7)) <= as.numeric(j) & 
                                                       as.numeric(substr(file.list[[i]][[j]],13,15)) >= as.numeric(j))]
    file.list[[i]][[j]] <- list.files(i)[grep(list.files(i), pattern = file.list[[i]][[j]])]
    unzip(paste(i,file.list[[i]][[j]],sep="/"), exdir = paste(getwd(),i, sep="/"))
    file.list[[i]][[j]] <- sub(".zip",".tif",paste0(i,"/",file.list[[i]][[j]]))
    dir.tmp <- paste("C:/Users/robinsonba/Documents/Projects/Priority Areas Analysis/Data/Temp",i,j,sep="/")
    dir.create(dir.tmp)
    file = list.files(path = i, pattern=".tif", full.names=T)
    file = file[which(nchar(file)==min(nchar(file)))]
    file.copy(from = file, to = dir.tmp)
    unlink(file)
  }
}


###############################
#This is old code used prior to implementing IMA method for filling in clouds
###############################
file.copy(from = list.files(pattern=".tif")[which(nchar(list.files(pattern=".tif"))<=52)], to = "C:/Users/robinsonba/Documents/Projects/Priority Areas Analysis/Data/Temp")
unlink(list.files(pattern="AgExtent"))

#load rasters into workspace
library(raster)
setwd("C:/Users/robinsonba/Documents/Projects/Priority Areas Analysis/Data/Temp")
raster.list <- lapply(list.files(), raster)

raster.list <- vector(mode="list", length=length(years))
names(raster.list) <- years
for (i in years) {
  raster.list[[i]] <- OD.list
  for (j in OD) {
    raster.list[[i]][[j]] <- raster(paste(i,j,list.files(paste(i,j,sep="/")), sep="/"))
  }
}


#download replacement images for each item in replace. Replace with image from the previous week.
setwd("C:/Users/robinsonba/Downloads")
file.list <- vector(mode="list", length=length(replace))
for (i in 1:length(replace)) {
  temp.OD <- ifelse(substr(replace[[i]],nchar(replace[[i]]),nchar(replace[[i]]))=="e",138,
                    ifelse(substr(replace[[i]],nchar(replace[[i]]),nchar(replace[[i]]))=="m",162,
                           186)) #determine appropriate Ordinal Day
  temp.yr <- substr(replace[[i]],5,8) #determine year folder to look in
  file.list[[i]] <- list.files(temp.yr)
  temp.str <- substr(file.list[[i]],nchar(file.list[[i]])-18, nchar(file.list[[i]])-4) #isolatre start and end dates from file name
  file.list[[i]] <- temp.str[which(as.numeric(substr(temp.str,5,7)) <= temp.OD & #find file which spans temp.OD and subtract 1 to choose previous file
                                     as.numeric(substr(temp.str,13,15)) >= temp.OD)+1] 
  file.list[[i]] <- list.files(temp.yr)[grep(list.files(temp.yr), pattern = file.list[[i]])] #save appropirate file name
  unzip(paste(temp.yr,file.list[[i]],sep="/"))
}

file.copy(from = list.files(pattern=".tif")[which(nchar(list.files(pattern=".tif"))<=52)], to = "C:/Users/robinsonba/Documents/Projects/Priority Areas Analysis/Data/Temp/replace")
unlink(list.files(pattern="AgExtent"))

#download second batch of replacement images for each item in replace2. This time replace with image from subsequent week.
setwd("C:/Users/robinsonba/Downloads")
file.list <- vector(mode="list", length=length(replace2))
for (i in 1:length(replace2)) {
  temp.OD <- ifelse(substr(replace2[[i]],nchar(replace2[[i]]),nchar(replace2[[i]]))=="e",138,
                    ifelse(substr(replace2[[i]],nchar(replace2[[i]]),nchar(replace2[[i]]))=="m",162,
                           186)) #determine appropriate Ordinal Day
  temp.yr <- substr(replace2[[i]],5,8) #determine year folder to look in
  file.list[[i]] <- list.files(temp.yr)
  temp.str <- substr(file.list[[i]],nchar(file.list[[i]])-18, nchar(file.list[[i]])-4) #isolatre start and end dates from file name
  file.list[[i]] <- temp.str[which(as.numeric(substr(temp.str,5,7)) <= temp.OD & #find file which spans temp.OD and add 1 to choose subsequent file
                                     as.numeric(substr(temp.str,13,15)) >= temp.OD)+1] 
  file.list[[i]] <- list.files(temp.yr)[grep(list.files(temp.yr), pattern = file.list[[i]])] #save appropirate file name
  unzip(paste(temp.yr,file.list[[i]],sep="/"))
}

file.copy(from = list.files(pattern=".tif")[which(nchar(list.files(pattern=".tif"))<=52)], to = "C:/Users/robinsonba/Documents/Projects/Priority Areas Analysis/Data/Temp/replace")
unlink(list.files(pattern="AgExtent"))

#download final batch of replacement images for each item in replace3. This time replace with image from subsequent week.
setwd("C:/Users/robinsonba/Downloads")
file.list <- vector(mode="list", length=length(replace3))
for (i in 1:length(replace3)) {
  temp.OD <- ifelse(substr(replace3[[i]],nchar(replace3[[i]]),nchar(replace3[[i]]))=="e",138,
                    ifelse(substr(replace3[[i]],nchar(replace3[[i]]),nchar(replace3[[i]]))=="m",162,
                           186)) #determine appropriate Ordinal Day
  temp.yr <- substr(replace3[[i]],5,8) #determine year folder to look in
  file.list[[i]] <- list.files(temp.yr)
  temp.str <- substr(file.list[[i]],nchar(file.list[[i]])-18, nchar(file.list[[i]])-4) #isolatre start and end dates from file name
  file.list[[i]] <- temp.str[which(as.numeric(substr(temp.str,5,7)) <= temp.OD & #find file which spans temp.OD and add 1 to choose subsequent file
                                     as.numeric(substr(temp.str,13,15)) >= temp.OD)+index[[i]]] 
  file.list[[i]] <- list.files(temp.yr)[grep(list.files(temp.yr), pattern = file.list[[i]])] #save appropirate file name
  unzip(paste(temp.yr,file.list[[i]],sep="/"))
}

file.copy(from = list.files(pattern=".tif")[which(nchar(list.files(pattern=".tif"))<=52)], to = "C:/Users/robinsonba/Documents/Projects/Priority Areas Analysis/Data/Temp/replace")
unlink(list.files(pattern="AgExtent"))

