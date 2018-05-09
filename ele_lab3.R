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

# Show data on top of imagery to help understand context
e <- extent(min(ele$Long),max(ele$Long), min(ele$Lat), max(ele$Lat))
r = gmap(e,type='satellite',zoom=7,lonlat=TRUE)
plot(r)

xy <- ele[c("Long","Lat")]
proj.info <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
points(xy,pch=".",cex=4,col=i)

####################################################################
##### TRAJECTORY ANALYSES #########################################
###################################################################

# Create a matrix to use to project data
# *************************************
temp <-as.matrix(cbind(ele$Long,ele$Lat))

# Set the projection and project data writing to new dataframe xy
proj.info <- paste("+proj=utm +zone=",36," +units=m +datum=WGS84",sep="")
xy <-project(temp,proj.info)

X <-xy[,1]
Y <-xy[,2]

# Bind these data to the dataset
temp <-cbind(ele,X,Y)
xy <-temp[,c("X","Y")]

# Calculate animal trajectories
# Convert point data to trajectory
temp.traj <-as.ltraj(xy,temp$Date,id = temp$ID, typeII = TRUE, slsp = c("remove"))
summary(temp.traj)

# The problem with above, is that it doesn't account for positions (fixes) that weren't collected (NA values not included).  Would make it seem like all the data were collected
# To fix, use the timing of collection to obtain a regular trajectory, based on a reference date.
start.ele <- paste("2016-05-16","10:00:00",sep=" ")
refda <- strptime(start.ele, "%Y-%m-%d %H:%M:%S", tz="Africa/Nairobi")
temp.traj <- setNA(temp.traj, refda, 30, units = "min")

# Now, create trajectory, but keep the info
traj <- as.ltraj(xy,date = temp$Date, id = temp$ID, infolocs = temp[,2:8], typeII = TRUE, slsp = c("remove"))

# Set NAs in trajectory
traj2 <- setNA(traj, refda, 30, units = "min")

# Convert to a dataframe using the ld command
traj.ele <-ld(traj2)

#the dist column provides a metric of ground speed that can be used to remove erroneous high speed movements
summary(traj.ele$dist)
# Graph Speed before cleaning data
hist(traj.ele$dist,xlab="Speed",main="Speed distribution - Elephants")
plot(traj.ele$dist,type="p",xlab="Index",ylab="Speed",main="Overall Speed distribution - Elephants",cex=.5)

#If we have really high speeds, need to remove and then reprocess.
traj.ele <- traj.ele[which(traj.ele$dist < 5000),] # This will be specific to animal.  This is seemingly a reaonsable cut-off for 30 min data.
nrow(traj.ele)

# Graph Speed after cleaning data
hist(traj.ele$dist,xlab="Speed",main="Speed distribution")
plot(traj.ele$dist,type="p",xlab="Index",ylab="Speed",main="Overall Speed distribution",cex=.5)
nrow(traj.ele)

# In removing high speed values, also removed NAs, so we need to add them back in
# Convert point data to trajectory
xy <-traj.ele[,c("x","y")]
temp.traj <-as.ltraj(xy,traj.ele$date,id = traj.ele$id, typeII = TRUE, slsp = c("remove"))
summary(temp.traj)

# Use the timing of collection to obtain a regular trajectory, based on a reference date.
temp.traj <- setNA(temp.traj, refda, 30, units = "min")

# Summarize
Summary.traj2 <- summary(temp.traj)



# Convert to a dataframe using the ld command
traj.ele <-ld(temp.traj)

# Considering# Plot the trajectories and look at them
par(mfrow=c(1,1)) #Adjusts margins so large enough to contain info
plot(traj2) # View all
plot(traj2[2]) # Or just 1 of the elephants.  Some very interesting patterns there.

# Plotting function used with the adehabitat trajectory information
par(mar = rep(2, 4))  #Adjusts margins so large enough to contain info
plotltr(traj2, "DOP") # Graphic of DOP over time......these should all be < 5, since we've cleaned them above.  Anything noticeable in the pattern?
plotltr(traj2,"dt/60") # Show when data were collected...should be every 30 minutes.