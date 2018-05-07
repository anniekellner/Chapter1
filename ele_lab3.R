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

#setwd("C:/Users/akell/Desktop/Spring_2018/Research/FW 477/GISLab3/GIS Lab3")
#ele.all <- list.files(path="C:/Users/akell/Desktop/Spring_2018/Research/FW 477/GISLab3/GIS Lab3", pattern='.csv', all.files=FALSE, full.names=FALSE)

# Create Null data frame to append all the data
#ele <- NULL

# Loop through the ele.all file
#for (i in 1:length(ele.all)){
  #Read in data
  #temp <- read.csv(ele.all[i],header=TRUE,sep=",")
  # Bind together (if data from same sensor/company, structure 'should' be the same
  #ele <- rbind(ele,temp)
#}

##### DID NOT HAVE SUCCESS WITH ABOVE APPROACH ######

ele.locs<-read.csv(file.choose())
ele <- ele[,c(4:5,7:9,11:12,14)]
names <- c("ID","DateTime","Lat","Long","DOP","Heading","Speed","Alt")
names(ele) <- names




# Create a matrix to use to project data
# *************************************
temp <-as.matrix(cbind(ele$Long,ele$Lat))

# Set the projection and project data writing to new dataframe xy
proj.info <- paste("+proj=utm +zone=",36," +units=m +datum=WGS84",sep="")
xy <-project(temp,proj.info)

X <-xy[,1]
Y <-xy[,2]

# Bind these data to the dataset
temp <-cbind(ele,X,Y)
xy <-temp[,c("X","Y")]

# Calculate animal trajectories
# Convert point data to trajectory
temp.traj <-as.ltraj(xy,temp$Date,id = temp$ID, typeII = TRUE, slsp = c("remove"))
summary(temp.traj)

# The problem with above, is that it doesn't account for positions (fixes) that weren't collected (NA values not included).  Would make it seem like all the data were collected
# To fix, use the timing of collection to obtain a regular trajectory, based on a reference date.
start.ele <- paste("2016-05-16","10:00:00",sep=" ")
refda <- strptime(start.ele, "%Y-%m-%d %H:%M:%S", tz="Africa/Nairobi")
temp.traj <- setNA(temp.traj, refda, 30, units = "min")

# Now, create trajectory, but keep the info
traj <- as.ltraj(xy,date = temp$Date, id = temp$ID, infolocs = temp[,2:8], typeII = TRUE, slsp = c("remove"))

# Set NAs in trajectory
traj2 <- setNA(traj, refda, 30, units = "min")

# Convert to a dataframe using the ld command
traj.ele <-ld(traj2)

# STEP 6: Remove data with unbiologically plausible speeds

#the dist column provides a metric of ground speed that can be used to remove erroneous high speed movements
summary(traj.ele$dist)
# Graph Speed before cleaning data
hist(traj.ele$dist,xlab="Speed",main="Speed distribution")
plot(traj.ele$dist,type="p",xlab="Index",ylab="Speed",main="Overall Speed distribution",cex=.5)

#If we have really high speeds, need to remove and then reprocess.
traj.ele <- traj.ele[which(traj.ele$dist < 5000),] # This will be specific to animal.  This is seemingly a reaonsable cut-off for 30 min data.
nrow(traj.ele)

# Graph Speed after cleaning data
hist(traj.ele$dist,xlab="Speed",main="Speed distribution")
plot(traj.ele$dist,type="p",xlab="Index",ylab="Speed",main="Overall Speed distribution",cex=.5)
nrow(traj.ele)

# In removing high speed values, also removed NAs, so we need to add them back in
# Convert point data to trajectory
xy <-traj.ele[,c("x","y")]
temp.traj <-as.ltraj(xy,traj.ele$date,id = traj.ele$id, typeII = TRUE, slsp = c("remove"))
summary(temp.traj)

# Use the timing of collection to obtain a regular trajectory, based on a reference date.
temp.traj <- setNA(temp.traj, refda, 30, units = "min")

# Summarize
Summary.traj2 <- summary(temp.traj)

# Summarize the completeness of the dataset that will allow you to pull these results out in a table
Summary.traj2$DaysTrack <-round(difftime(Summary.traj2$date.end,Summary.traj2$date.begin, units="days"),digits=1)
Summary.traj2$Records <- Summary.traj2$nb.reloc-Summary.traj2$NAs
Summary.traj2$PctComplete <- round((Summary.traj2$nb.reloc-Summary.traj2$NAs)/Summary.traj2$nb.reloc*100,digits=1)

# Convert to a dataframe using the ld command
traj.ele <-ld(temp.traj)

# Considering# Plot the trajectories and look at them
par(mfrow=c(1,1)) #Adjusts margins so large enough to contain info
plot(traj2) # View all
plot(traj2[2]) # Or just 1 of the elephants.  Some very interesting patterns there.  

#Question 6: Anything interesting that you could formulate into a research question?

# Plotting function used with the adehabitat trajectory information
par(mar = rep(2, 4))  #Adjusts margins so large enough to contain info
plotltr(traj2, "DOP") # Graphic of DOP over time......these should all be < 5, since we've cleaned them above.  Anything noticeable in the pattern?
plotltr(traj2,"dt/60") # Show when data were collected...should be every 30 minutes.  

#Question 7: How much do the fix collections deviate from when expected?

# Considering NA values and especially the distribution of when they appear is important before one analyzes GPS data.  
# Is there randomness in the missing data or are the missing data a result of the habitat structure where animals are located at a particular time of day?
# Could it be that, for example, habitat structure is obscuring the signal?  
# This could bias further results.

TrajNA <- subset(traj.ele, is.na(x))
# How many NAs?
nrow(TrajNA)
# How many per animal?
TrajNA$Number <- 1
aggregate(Number ~ id, sum, data = TrajNA)

# Test if the missing values occur at random
runsNAltraj(traj2)
summaryNAltraj(traj2)

# Plot the NAs
plotNAltraj(traj2)

# Look at the summary file
# What percentage of the data are we receiving?
Summary.traj2 

# Let's look at the time that we received missing data and plot the data
# Create an hour field (Let's group the NA data into hours to see if their are any times of day we have failures)
TrajNA$Hour <- as.numeric(formatC(format(TrajNA$date,"%H"),width=2,format="d",flag="0"))
hist(TrajNA$Hour,xlab="Hour",breaks=length(unique(TrajNA$Hour)))

#Question 8: When do most of the NAs occur?

NA.Summary <- aggregate(Number ~ id + Hour, sum, data = TrajNA)
Loc.or <-order(NA.Summary$id,NA.Summary$Hour,decreasing=FALSE)
NA.Summary <-NA.Summary[Loc.or,]

#Now lets look at the NA failures by hour for each individual
# Plot each individual
ID <- unique(TrajNA$id)

# Set plotting window
par(mfrow=c(length(ID),2))

for (i in 1:length(ID)){	
  T1 <- subset(TrajNA, id == ID[i])
  hist(T1$Hour,xlab="Hour",breaks=length(unique(T1$Hour)),main=unique(T1$id),ylim=c(0,max(NA.Summary$Number)))
}

#Question 8: What sort of patterns do you see in where GPS failures have occurred? Do the patterns differ by individual?
#            Does the pattern give any insight to what could be causing these failures?

# Files to save in a list
list(Data.Traj=traj.ele,Dataset=temp,Summary=Summary.traj2,traj=traj)

# Output files from the function to descriptive names
Ele.Traj.Dataset <- traj.ele # This is the dataframe with the trajectory information
Ele.Dataset <- temp  # This is the original data, with the updated UTM X and Y appended
Ele.Summary <- Summary.traj2 # Summary of records received
Ele.Traj <- traj # The trajectory (adehabitat format)

# Graph the data to look at the histogram of calculated movements	
par(mfrow=c(1,1))
hist(Ele.Traj.Dataset$dist, breaks=50, xlim=c(0,4000),xlab="Steplength (m)",main="Half Hourly Movements",freq=FALSE,col="gray")
text(4000,0.005,paste0("Avg. Movement (m): ",round(mean(Ele.Traj.Dataset$dist,na.rm=TRUE),digits=2)),pos=4)

# Look at the data points.
plot(Ele.Traj.Dataset$x,Ele.Traj.Dataset$y,xlab="Longitude",ylab="Latitude",type="n",pch=".",cex=4,frame=FALSE,asp = 1)
lines(Ele.Traj.Dataset$x,Ele.Traj.Dataset$y,lwd=0.5,col="light gray")
points(Ele.Traj.Dataset$x,Ele.Traj.Dataset$y,pch=".",cex=3)

# *******************************************
# *******************************************
# Summarize movements
# *******************************************
# *******************************************

# Let's start with looking at hourly movements
# Look at the distance traveled.  
# We need to remove some of the data, since the data are collected every hour.
traj.ele$Minutes <- as.numeric(formatC(format(traj.ele$date,"%M"),width=2,format="d",flag="0"))

# Remote all the data, except for those collected at 0 minutes
traj.ele <- subset(traj.ele, Minutes == 0)
# Remove the trajectory and re-calculate the statistics
traj.ele <- traj.ele[,c(1:3,11)]
traj.ele <- traj.ele[complete.cases(traj.ele[,1:2]),]

# Rename the fiels and calculate the trajectory as before
names <- c("X","Y","Date","ID")
names(traj.ele) <- names

# Same as above
xy <-traj.ele[,c("X","Y")]
temp.traj <-as.ltraj(xy,traj.ele$Date,id = traj.ele$ID, typeII = TRUE, slsp = c("remove"))
temp.traj <- setNA(temp.traj, refda, 1, units = "hour")
traj <- as.ltraj(xy,date = traj.ele$Date, id = traj.ele$ID, typeII = TRUE, slsp = c("remove"))
# Set NAs in trajectory
traj2 <- setNA(traj, refda, 1, units = "hour")
# Convert to a dataframe using the ld command
traj.ele <-ld(traj2)

# Get the Hour
traj.ele$Hour <- formatC(format(traj.ele$date,"%H"),width=2,format="d",flag="0")

# Sort in order
Loc.or <-order(traj.ele$Hour,decreasing=FALSE)
traj.ele <-traj.ele[Loc.or,]

# Unique hours
Hour.unique <- unique(traj.ele$Hour)

# Summarize the Hourly Movements
moves <- aggregate(dist ~ Hour, data = traj.ele, summary)
hour.move <- aggregate(dist ~ Hour, data = traj.ele, mean)

par(mfrow=c(1,1))
plot(hour.move$Hour,hour.move$dist,type="b", xlab="Hour",ylab="Average Hourly Movement (m)", ylim=c(0,1000), frame=FALSE, axes=FALSE)
axis(1, at=-0.5:22.5,lab=0:23)
axis(2)

# Let's look at the box plot also	
boxplot(dist ~ Hour, data=traj.ele, boxwex=0.25, at=seq(0.5,23.5,1), main="Elephant - Hourly Movements", frame = FALSE, axes = FALSE, xlab = "Hour", ylab = "Hourly movement (m)")
axis(1, at=seq(0,23,1), lab=seq(0,23,1))
axis(2)

#Question 9: What patterns do you see in the diurnal movement behavior?  
#            When are these elephants most active? When do they rest?

# Julian date into the data.frame
Year <- as.integer(format(Ele.Traj.Dataset$date,"%Y"))
Month <- as.integer(format(Ele.Traj.Dataset$date,"%m"))
Day <- as.integer(format(Ele.Traj.Dataset$date,"%d"))
Ele.Traj.Dataset$Julian <- mdy.date(Month,Day,Year,nineteen=TRUE,fillday=FALSE,fillmonth=FALSE)

# Create a blank frame to hold everything
Id.sum <- as.data.frame(ID.all)

Day.Sums.All <- NULL

for (i in 1:length(ID.all)){
  temp <- subset(Ele.Traj.Dataset, id == ID.all[i])
  Id.sum$Belt[i] <- as.character(unique(temp$Belt))
  # First, summarize how many days monitored
  Id.sum$Start[i] <- as.character(temp[1,4])
  Id.sum$End[i] <- as.character(temp[nrow(temp),4])
  Id.sum$Days[i] <- round(temp[nrow(temp),4]-temp[1,4],digits=1)
  # Or do
  #Id.sum$SumDays[i] <- round(sum(temp$dt,na.rm=TRUE)/86400,digits=0) #86400 is converting seconds to days (60 secs * 60 mins * 24 hour)
  
  # Add in the temporal resolution, calculating the mean
  Id.sum$Int[i] <- paste0(round(mean(temp$dt/3600,na.rm=TRUE))," Hour")
  
  # Sum of Movement within period monitored
  Id.sum$AvgMove[i] <- round(mean(temp$dist,na.rm=TRUE),digits=2) # This is meters	
  Id.sum$SumMove[i] <- round(sum(temp$dist,na.rm=TRUE)/1000,digits=2) # This is kilometers
  
  # Calculate the average daily movement.  Use Julian day
  Days <- unique(temp$Julian)
  
  # Create blank dataset to hold everything.
  Sum.Day2 <- NULL
  
  for (j in 1:length(Days)){
    # Subset by day
    temp2 <- subset(temp, Julian == Days[j])
    Sum.Day1 <- round(sum(temp2$dist,na.rm=TRUE)/1000,digits=2)
    #Sum.Day2 <- rbind(Sum.Day2,Sum.Day1)
    Sum.Day2 <- c(Sum.Day2,Sum.Day1)
  }
  # Average how much animals move each day
  Id.sum$AvgDailyMve[i] <- round(mean(Sum.Day2),digits=2)
  Day.Sums.All <- c(Day.Sums.All,Sum.Day2)
}

Id.sum

# Look at the movement histogram
hist(Ele.Traj.Dataset$dist,freq=FALSE,breaks=200,main="Movements",xlim=c(0,3000),xlab="Distance (m)")
hist(Day.Sums.All,freq=TRUE,breaks=100,main="Daily Movements",xlim=c(0,35),xlab="Distance (km)")

#Question 10: What is the average daily distance traveled by these elephants? 
#            How does distance traveled relate to overall range size (compare to map)?