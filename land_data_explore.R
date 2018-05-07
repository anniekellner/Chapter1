rm(list = ls())

library(foreign)
library(dplyr)
library(tidyr)
library(adehabitatLT)
library(lubridate)

land <- read.dbf("C:/Users/akell/Desktop/Spring_2018/Research/GIS/land_locations.dbf") #Import attribute data from shapefile


### Concatenate and format time columns #####

paste(land$year, land$month, land$day, sep = "-")
form.dates <- ymd(paste(land$year, land$month, land$day, sep = "-"))
form.dates

str(form.dates)

land$ymd <- form.dates
str(land)

land$datetime <- ymd_hms(paste(land$year, land$month, land$day, land$hour, land$minute, land$second, sep = "-")) #use lubridate fxn to format datetime, add column to land dataframe
str(land)

write.csv(land, file = "Land.csv")

# Test to see whether error in adehabitat is beacuse of negative numbers
land_trial <- mutate(land, x_new=X+300000, y_new=Y+300000)


### Individuals per year ###

table(land$animal, land$year)

#Test for duplicated data
tt<-paste(land$animal, land$datetime)
table(duplicated(tt, fromLast=F)) #Test if you have duplicated data

traj<-as.ltraj(xy=land_trial[,"X":"Y"], date=land_trial$datetime, id=as.character(land_trial$animal))
traj2<-ld(traj)
