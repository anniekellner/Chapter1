library(adehabitatLT)

library(foreign)


# Date and Time formatting for adehabitatLT
data<-read.dbf("Yourshapefile.dbf")
data$date<-as.POSIXct(strptime(as.character(data$Fixtime),"%Y-%m-%d %H:%M:%S", tz="Africa/Nairobi")) #Change "fixtime for your date column and tz for your time zone –
data$Year<-as.numeric(format(data$date, "%Y"))



table(data$MovDataID, data$Year) #Number of locations per individual and year


#Convert GPS to UTM
sp<-SpatialPoints(data[,c("X","Y")], proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84")) #Create a spatialpoints object. Change X and y if your columns have different names
sp<-spTransform(sp, CRS("+init=epsg:32737")) #Find the epsg code (use google) corresponding to the UTM zone in alaska?
data$xutm<-coordinates(sp)[,1] #Add utm to dataframe
data$yutm<-coordinates(sp)[,2]

#Test for duplicated data
tt<-paste(data$MovDataID, data$date)
table(duplicated(tt, fromLast=F)) #Test if you have duplicated data.

traj<-as.ltraj(xy=data[,10:11], date=data$date, id=as.character(data$MovDataID)) #Change 10:11 for the UTM columsn. change movdataID for the column with your ID

traj2<-ld(traj) #Convert trajectory to dataframe




#Example of time interval extraction

mean(traj2$dt, na.rm=T)

tapply(traj2$dt, traj2$id, mean, na.rm=T)



#You can look at the vignette for adehabitatLT, some plot functions are nice too.