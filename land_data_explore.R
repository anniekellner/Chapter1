library(foreign)
library(dplyr)
land <- read.dbf("C:/Users/akell/Desktop/Spring_2018/Research/GIS/land_locations.dbf")
unique(land$animal) 
table(land$year)

#2004
year2004 <- subset(land, year=="2004")
unique(year2004$animal)

#2005
year2005 <- subset(land, year=="2005")
unique(year2005$animal)

#2006
year2006 <- subset(land, year=="2006")
unique(year2006$animal)
