#####This code only needs to be run once to create the maxDen table. Skip to line 55 if its already created#############
####calculate 99th percentile densities by species####
#import raw point count data
PC <- read.csv("Data files/CountData/CGAMP_database/PC_all.csv")

#In order to transform counts into density, need to estimate area surveyed based on point count radii
#extract max point count radius for each project; for projects with max dist of Inf, count data from the last distance bin (x-inf) will be removed before calculating the max.
proj <- read.csv("Data files/CountData/CGAMP_database/Project_all.csv")
dist <- read.csv("Data files/CountData/CGAMP_database/DISTANCE.csv")
distLU <- merge(proj, dist[,c("DISTMETH", "DistanceID", "DistStart", "DistEnd","MaxDist")], by.x = "Distance_Method", by.y = "DISTMETH")

#make list of projects that have inf max distance and DistanceID associated with the x-inf dist bin.
remove <- distLU[which(distLU$MaxDist==Inf&distLU$DistEnd==Inf),]
remove <- paste(remove$Project, remove$DistanceID, sep = "_")

#remove counts from projects-distance bin combinations listed in remove
PC$remove <- paste(PC$Project, PC$Distance_Level, sep = "_")
pcTrunc <- PC[!(PC$remove %in% remove),]

#sum counts across distance bins (excluding those with Inf as the endpoint)
maxY <- aggregate(pcTrunc$Abundance, by = list(Project = pcTrunc$Project, species = pcTrunc$Species, PKEY = pcTrunc$PKEY), FUN = "sum")

#identify point count radii for each survey
radii <- distLU[which(distLU$DistEnd==distLU$MaxDist),c("Project", "DistStart", "DistEnd", "MaxDist")]
radii$radii <- radii$MaxDist
radii[which(radii$MaxDist==Inf),"radii"] <- radii[which(radii$MaxDist==Inf),"DistStart"]
maxY <- merge(maxY, radii[,c("Project", "radii")])

#transform max counts to density, ensuring you choose the line below with the desired units
#INDIVIDUALS/HA
#maxY$density <- maxY$x/((pi*maxY$radii^2)/10000)

#INDIVIDUALS/QUARTER SECTION
#maxY$density <- maxY$x/((pi*maxY$radii^2)/647497)

#INDIVIDUALS/PIXEL (800x800m = 640000m^2)
maxY$density <- maxY$x/((pi*maxY$radii^2)/640000)

#calculate 99th percentile to remove outliers
maxDen <- aggregate(maxY$density, by = list(species = maxY$species), FUN = "quantile", probs = 0.99)
colnames(maxDen) <- c("species", "max")

#save maxDen so it can be used later
#write.csv(maxDen, "Data Files/CountData/maxDen_qs.csv")
#write.csv(maxDen, "Data Files/CountData/maxDen_pixel.csv")
#max.LARB <- maxDen[which(maxDen$species=="LARB"),"max"]
#######################################################################################################################################