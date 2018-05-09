######################################
##### DATA CLEANING #################
#####################################
rm(list = ls())

library(dplyr)
library(lubridate)

land <- read.csv("Land.csv")
land <- as.data.frame(land) #convert to df
ows <- subset(land, land$month > 6 & land$month < 11)
 

#format as.POSIXct
ows$datetime <- ymd_hms(paste(ows$year, ows$month, ows$day, ows$hour, ows$minute, ows$second, sep = "-")) #use lubridate fxn to format datetime, add column to land dataframe

ows <- subset(ows, datetime >=as.POSIXct("2004-07-01 00:00:00", tz="US/Alaska")) #records before April 2004 removed

nrow(ows)
ows <- ows[complete.cases(ows[,9:10]),] # N/A lat/long removed
nrow(ows)
