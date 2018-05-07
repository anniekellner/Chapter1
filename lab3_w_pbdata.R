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

land<- read.csv("Land.csv")

#format datetime
paste(land$year, land$month, land$day, sep = "-")
form.dates <- ymd(paste(land$year, land$month, land$day, sep = "-"))
form.dates

str(form.dates)

land$ymd <- form.dates
str(land)

land$datetime <- ymd_hms(paste(land$year, land$month, land$day, land$hour, land$minute, land$second, sep = "-"), tz="US/Alaska") #use lubridate fxn to format datetime, add column to land dataframe
which(is.na(land$datetime)) #which datetime=NA (#=rows)


#convert to trajectory
traj<-as.ltraj(xy=land[,c("X","Y")], date=land$datetime, id=as.character(land$animal))
traj.pb<-ld(traj) #convert to dataframe

summary(traj.pb$dist) #dist = length of each move

#graph speed
hist(traj.pb$dist,xlab="Speed",main="Speed distribution")
plot(traj.pb$dist,type="p",xlab="Index",ylab="Speed",main="Overall Speed distribution",cex=.5)

Summary.traj2 <- summary(traj.pb)

# Summarize the completeness of the dataset that will allow you to pull these results out in a table
Summary.traj2$DaysTrack <-round(difftime(Summary.traj2$date.end,Summary.traj2$date.begin, units="days"),digits=1)
Summary.traj2$Records <- Summary.traj2$nb.reloc-Summary.traj2$NAs
Summary.traj2$PctComplete <- round((Summary.traj2$nb.reloc-Summary.traj2$NAs)/Summary.traj2$nb.reloc*100,digits=1)
