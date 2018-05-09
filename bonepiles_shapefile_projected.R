library(proj4)
bp <- read.csv("C:/Users/akell/Desktop/Spring_2018/Research/GIS/bonepiles_latlong.csv")
M <- as.matrix(cbind(bp$gps_lon, bp$gps_lat)) #create a matrix to project
xy <- project(M, "+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs") #project to NAD 1983 Alaska Albers
X <- xy[,1]
Y <- xy[,2]

#bind the datasets
bp2 <- cbind(bp,X,Y)

library(sp)
library(rgdal)
projection <- CRS("+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs") #find this in spatialreference.org
coords <- cbind(bp2$X, bp2$Y)
spdf.use <- SpatialPointsDataFrame(coords = coords, data = bp2, proj4string = projection) 

#create shapefile (Spaital Point DataFrame for the use locations for extraction)

writeOGR(spdf.use,dsn = "./Output", layer = "bonepiles", driver = "ESRI Shapefile")