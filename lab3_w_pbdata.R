rm(list = ls())

# Install necessary libraries (libraries provide the code that we will use to analyze data)
install.packages("proj4")
install.packages("dismo")
install.packages("rgdal")
install.packages("adehabitatLT")
install.packages("date")
install.packages("zoom")
install.packages("raster")
install.packages("sendmailR")
install.packages("maptools")

# Load necessary libraries to process data in this script
library(proj4)
library(dismo)
library(rgdal)
library(adehabitatLT)
library(date)
library(zoom)
library(raster)
library(sendmailR)
library(maptools)
library(lubridate)

setwd("C:/Users/akell/Desktop/Spring_2018/Research/GIS/Chapter1")
land<- read.csv("Land.csv")

#set tz to US/Alaska


land$datetime <- ymd_hms(paste(land$year, land$month, land$day, land$hour, land$minute, land$second, sep = "-"), tz="US/Alaska") #use lubridate fxn to format datetime, add column to land dataframe
which(is.na(land$datetime)) #which datetime=NA (#=rows)

attributes(land$datetime)

## Plot
ID.all.pb <- unique(land$animal)

# Show data in a plot

par(mar = rep(0, 4))  #Adjusts margins so large enough to contain info
plot(land$gps_lon,land$gps_lat,xlab="Longitude",ylab="Latitude",type="n",pch=".",cex=4,frame=FALSE,asp = 1)
lines(land$gps_lon,land$gps_lat,lwd=0.5,col="light gray")
points(land$gps_lon,land$gps_lat,pch=".",cex=3)

for(i in 1:length(ID.all.pb)){
  temp <- subset(land, animal == ID.all.pb[i])
  temp <- temp[complete.cases(temp[,3:4]),] # Only omits if these records are blank...shouldn't be any since I already removed
  xy <- temp[c("gps_lon","gps_lat")]
  proj.info <- "+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"
  temp2 <- SpatialPointsDataFrame(xy,temp,proj4string = CRS(proj.info))
  points(temp2,pch=".",cex=4,col=i)
}