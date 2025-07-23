# ---
# title: "Calculate Time since local sunrise (TSSR) and Ordinal day (JDAY) or each point count survey"
# author: "Barry Robinson"
# created: "October 22, 2018"
# updated: "December 2, 2023"
# ---

#library(maptools) this is depricated because it relies on sp
#use library(suntools) instead

#load data
PKEY_XY <- read.csv("Processed/PKEY_XY.csv") #Survey events in space and time (Access query)
any(is.na(PKEY_XY))#all good


#Create & Format DateTime variable(s)

PKEY_XY$Date <- paste(PKEY_XY$YYYY,"-",PKEY_XY$MM, "-", PKEY_XY$DD,sep="")
PKEY_XY$Date <- as.POSIXct(paste(PKEY_XY$Date), format = "%Y-%m-%d")
PKEY_XY$Time <- paste(PKEY_XY$HR, PKEY_XY$Min, "00", sep=":")
PKEY_XY$DateTime <- as.POSIXct(paste(PKEY_XY$Date, PKEY_XY$Time), format = "%Y-%m-%d %H:%M:%S")

#Calculate time since local sunrise (TSSR)
##Use maptools function (Lewin-Koh & Bivand 2012) to calculate sunrise time (Solymos et al. 2013)

coords_PKEY_XY <- as.matrix(PKEY_XY[,c("X", "Y")])
sunriset_PKEY_XY <- maptools::sunriset(coords_PKEY_XY, PKEY_XY$Date, direction="sunrise", POSIXct.out=TRUE)
PKEY_XY$sunriset <- sunriset_PKEY_XY$time



#Now take the difference between survey time and sunrise time
#Negative values indicate survey was done before dawn, positive is after sunrise

PKEY_XY$TSSR <- as.numeric(difftime(PKEY_XY$DateTime, PKEY_XY$sunriset, units="hours"))

any(is.na(PKEY_XY$TSSR)) #no NAs
summary(PKEY_XY$TSSR)

#center the data
PKEY_XY$TSSR <- PKEY_XY$TSSR/24

#Create new variable TSSR2 = TSSR^2; something Solymos suggested might fit better if peak singing is related quadratically to sunrise time rather than linearly
PKEY_XY$TSSR2   <- PKEY_XY$TSSR^2



#Get Julian date

PKEY_XY$JDAY <- as.numeric(strftime(PKEY_XY$Date, format="%j"))
any(is.na(PKEY_XY$JDAY)) #check for NAs
summary(PKEY_XY$JDAY)
PKEY_XY$JDAY <- PKEY_XY$JDAY/365 #center the data

#spp_data <- spp_data[!spp_data$DURMETH=="J",] #remove unknown (J) #these data are removed in Script2
#PKEY_XY   <- PKEY_XY[!PKEY_XY$DURMETH=="J",]
#table(PKEY_XY$DURMETH)


#Export data including only the necessary columns

write.csv(subset(PKEY_XY, select=c(PKEY,SS,DateTime,YYYY,DURMETH,DISTMETH,TSSR,TSSR2,JDAY)), "Data/PKEY_TSSR.csv",row.names = FALSE)
