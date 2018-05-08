rm(list=ls())

# Load necessary libraries to process data in this script/
library(proj4)
library(dismo)
library(rgdal)
library(adehabitatLT)
library(date)
library(zoom)
library(raster)
library(sendmailR)
library(maptools)

setwd("C:/Users/akell/Desktop/Spring_2018/Research/GIS/Chapter1/FW477/GIS Lab3/GIS Lab3")

#read in all the files
ele.all <- list.files(path="C:/Users/akell/Desktop/Spring_2018/Research/GIS/Chapter1/FW477/GIS Lab3/GIS Lab3", pattern='.csv', all.files=FALSE, full.names=FALSE)

# Create Null data frame to append all the data
ele <- NULL

# Loop through the ele.all file
for (i in 1:length(ele.all)){
  #Read in data
  temp <- read.csv(ele.all[i],header=TRUE,sep=",")
  # Bind together (if data from same sensor/company, structure 'should' be the same
  ele <- rbind(ele,temp)
}

#remove fields we don't need
ele <- ele[,c(4:5,7:9,11:12,14)]

# Simplify names for future processing ease
names <- c("ID","DateTime","Lat","Long","DOP","Heading","Speed","Alt")
names(ele) <- names

#Make sure time stamps in local time
ele$Date <- as.POSIXct(x=ele$DateTime,
                       format = "%m/%d/%y %H:%M", tz="Africa/Nairobi")

# Check to see if converted properly.  The date should not change in this instance (GMT to GMT)	
attributes(ele$Date)
#The newly added Date column should be populated. 
#Problems can emerge if the format of th eDateTime column in the CSV file differ
str(ele)

#*************************************************************************************************************
# Data Cleaning:
# *************************************************************************************************************

ID.all <- unique(ele$ID)

# Show data in a plot
par(mar = rep(2, 4))  #Adjusts margins so large enough to contain info
plot(ele$Long,ele$Lat,xlab="Longitude",ylab="Latitude",type="n",pch=".",cex=4,frame=FALSE,asp = 1)
lines(ele$Long,ele$Lat,lwd=0.5,col="light gray")
points(ele$Long,ele$Lat,pch=".",cex=3)

for(i in 1:length(ID.all)){
  temp <- subset(ele, ID == ID.all[i])
  temp <- temp[complete.cases(temp[,3:4]),] # Only omits if these records are blank...shouldn't be any since I already removed
  xy <- temp[c("Long","Lat")]
  proj.info <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
  temp2 <- SpatialPointsDataFrame(xy,temp,proj4string = CRS(proj.info))
  points(temp2,pch=".",cex=4,col=i)
}

