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
  xy <- temp[c("gps_lon","gps_lat")]
  proj.info <- "+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"
  temp2 <- SpatialPointsDataFrame(xy,temp,proj4string = CRS(proj.info))
  points(temp2,pch=".",cex=4,col=i)
}

# Show data on top of imagery to help understand context
e <- extent(min(land$gps_lon),max(land$gps_lon), min(land$gps_lat), max(land$gps_lat)) #not working well for AK region
r = gmap(e,exp=2, type='satellite',zoom=3,lonlat=TRUE)
plot(r)

xy <- land[c("gps_lon","gps_lat")]
points(xy,pch=".",cex=4,col=i)

############################################################################################
#### TRAJECTORY ANALYSES #####################################################################
#################################################################################

library(adehabitatLT)
ows <- subset(land, land$month > 6 & land$month < 11)
ows$datetime <- ymd_hms(paste(ows$year, ows$month, ows$day, ows$hour, ows$minute, ows$second, sep = "-"), tz="US/Alaska") #use lubridate fxn to format datetime, add column to land dataframe


traj.pb<-as.ltraj(xy=ows[,c("X","Y")], date=ows$datetime, id=as.character(ows$animal))

summary(traj.pb)

# STEP 6: Remove data with unbiologically plausible speeds
#the dist column provides a metric of ground speed that can be used to remove erroneous high speed movements
summary(traj.pb$dist)

# Graph Speed before cleaning data
hist(traj.pb$dist,xlab="Speed",main="Speed distribution - PB")
plot(traj.pb$dist,type="p",xlab="Index",ylab="Speed",main="Overall Speed distribution - PB",cex=.5)

traj.pb.outliers <- traj.pb[which(traj.pb$dist > 250000),]

Summary.traj.pb <- summary(traj.pb)

write.csv(Summary.traj.pb, file="traj_summary.csv") #write to .csv for landing analysis


# Summarize the completeness of the dataset that will allow you to pull these results out in a table
Summary.traj.pb$DaysTrack <-round(difftime(Summary.traj.pb$date.end,Summary.traj.pb$date.begin, units="days"),digits=1)
Summary.traj.pb$Records <- Summary.traj.pb$nb.reloc-Summary.traj.pb$NAs
Summary.traj.pb$PctComplete <- round((Summary.traj.pb$nb.reloc-Summary.traj.pb$NAs)/Summary.traj.pb$nb.reloc*100,digits=1)

traj.pb2<-ld(traj.pb) #convert to df

par(mfrow=c(1,1)) #Adjusts margins so large enough to contain info
plot(traj.pb) # View all
plot(traj.pb[2]) # Or just 1 

# Plotting function used with the adehabitat trajectory information
par(mar = rep(2, 4))  #Adjusts margins so large enough to contain info
#plotltr(traj.pb, "DOP") # Graphic of DOP over time
plotltr(traj.pb,"dt/60") # Show when data were collected 


