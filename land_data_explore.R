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


### Individuals per year ###

table(land$animal, land$year)

#Test for duplicated data
tt<-paste(land$animal, land$datetime)
table(duplicated(tt, fromLast=F)) #Test if you have duplicated data

traj<-as.ltraj(xy=land[,c("X","Y")], date=land$datetime, id=as.character(land$animal))
traj2<-ld(traj)

mean(traj2$dt, na.rm=T) #dt=time interval between successive relocations
traj2_hrs <- mutate(traj2, dt_hrs=dt/3600)

mean(traj2_hrs$dt_hrs, na.rm = T)
median(traj2_hrs$dt_hrs,  na.rm = T)

tapply(traj2_hrs$dt_hrs, traj2$id, mean, na.rm=T)
