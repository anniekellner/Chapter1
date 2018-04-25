rm(list = ls())

#Projecting lat/long data (.csv) to a shapefile
### Todd's lat/long data --> projected to NAD83 Alaska Albers


library(proj4)
loc <- read.csv("C:/Users/akell/Desktop/Spring_2018/Research/GIS/usgs_pbear_gps_ccde16_v20170131.csv")
M <- as.matrix(cbind(loc$gps_lon, loc$gps_lat)) #create a matrix to project
xy <- project(M, "+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs") #project to NAD 1983 Alaska Albers
X <- xy[,1]
Y <- xy[,2]

#bind the datasets
loc2 <- cbind(loc,X,Y)

library(sp)
library(rgdal)
projection <- CRS("+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs") #find this in spatialreference.org
coords <- cbind(loc2$X, loc2$Y)
spdf.use <- SpatialPointsDataFrame(coords = coords, data = loc2, proj4string = projection) 

#create shapefile (Spaital Point DataFrame for the use locations for extraction)

writeOGR(spdf.use,dsn = "./Output", layer = "all_locations", driver = "ESRI Shapefile")

