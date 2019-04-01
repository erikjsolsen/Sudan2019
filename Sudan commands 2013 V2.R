# Sudan surveys (2012 - 2013) 
# R -script for analyzing output - plotting figures
#
# By: Erik Olsen
# Created: 12.11.2012
# Updated: 16.04.2014

# This code is used to load all survey data from the Nov'12, May/June'13 and Nov/Dec'13 surveys in the Red Sea, Sudan. 
#-------------------------------------------


#--------------------------------------------
# IMPORTING DATA TO R
#--------------------------------------------

library(lattice) #load lattice library
library(RColorBrewer)
library(plyr)
library(ggplot2)

source("/Users/eriko/ownCloud/R/R functions/suncalc.r") #function to calculate  sunrise/sunset
#Set working directory
#Must be changed to according to your own directories
setwd("/Users/eriko/ownCloud/Research/Sudan/SurveyProj2012_14/All_data")



# Import species data & calculate sunrise & sunset
catch<-read.table("catch.csv", header=TRUE, dec=",",sep=";")
catch<-cbind(catch, suncalc(catch$Day1,catch$lat,catch$lon)) #add sunrise and sunset data to data frame for day 1
catch<-rename(catch, c("sunrise"="sunrise1", "sunset"="sunset1"))
catch<-cbind(catch, suncalc(catch$Day2,catch$lat,catch$lon)) #add sunrise and sunset data to data frame for day 2
catch<-rename(catch, c("sunrise"="sunrise2", "sunset"="sunset2"))
catch<-subset(catch, select=c(survey:hrs2, sunset1, sunrise2) ) #select only relevant columns
catch<-cbind(subset(catch, select=c(survey:hrs2)), catch$sunset1+1, catch$sunrise2+1) #adjust for incorrect time-zone
catch<-rename(catch, c("catch$sunset1 + 1"="sunset1", "catch$sunrise2 + 1"="sunrise2"))

#Calculate hours of daylight after setting
attach(catch)
daylight<-sunset1-hrs1 #hours of sunlight after setting and before night
catch<-cbind(catch, daylight)
detach(catch)


# Import Length Data
#length<-read.table("length.csv", header=TRUE, dec=",",sep=";")


#---------------------------------------------------------------
# EXPLORATORY ANALYSIS OF STATION DATA
#---------------------------------------------------------------
# Create station data from catch data - summing up over stations
sv<-c("weight", "number")
summary(catch)

apply(catch[5:6], 2, sum, na.rm=TRUE) # applying SUM function over Weight and Numbers

tapply(catch[,5], catch[,3], sum) # applying sum for one variable by each Station

catch1<-aggregate(cbind(weight, number) ~ ss, data = catch, FUN = sum) # sums catch weight and numbers by station

catch2<-aggregate(cbind(survey, station, bdep, lon, lat, gear, daylight) ~ ss, data = catch, FUN=median) # trying to make make an aggregate tabel for the remaining variables

catch2$gear <- factor(levels(catch$gear)[catch2$gear], levels=levels(catch$gear)) # from StackOverflow - solves the problem!!!

station<-cbind(catch2, catch1[,2:3]) # comnbine the two catch tables


# Stations Exploratory statistical analysis
summary(station)
station13<-(subset(station, survey==2013002))
by(station[9], station[7], summary) #summary catchweight by gear type
by(station[9], station[2], summary) #summary of weight by survey
by(station[10], station[2], summary) #summary of weight by survey

by(catch[14],catch[4], summary)

no_nocatch<-subset(station, number!=0)
# used to generate table 1 in MS:
by(no_nocatch$gear, no_nocatch$survey, summary)
by(station$gear, station$survey, summary)

summary(station $ gear)

#subset by year
summary(subset(station, survey==2012901)) #for 2012
summary(subset(station, survey==2013002))  #for May/June 2013
summary(subset(station, survey==2013005))  #for Nov 2013


#---------------------
# IMPORT STATION TABLE DIRECTLY
#---------------------
station2<-read.table("stations.csv", header=TRUE, dec=",",sep=";")
traps<-subset(station2, gear=="TB")
traps<-subset(traps, geardepthstart>0)
summary(traps)

#-------------------------------------
# COMPARISON OF CATCHES PR STATION
#----------------------------------
# COMPARISON OF GEARS AND YEARS
#Plot catch weigh by different gears and years
box.col<-brewer.pal(3,"Blues")
pdf(file="catch by gear type.pdf")
boxplot(weight ~ gear, data=station, subset=survey==2012901, boxwex=0.2,  xlab="Gear Type", ylab="Catch weight (kg)", col=box.col[1], show.names=FALSE)
boxplot(weight ~ gear, data=station, add=TRUE, boxwex=0.2,  at=1:7+0.2,  subset=survey==2013002, col=box.col[2])
boxplot(weight ~ gear, data=station, add=TRUE, boxwex=0.2,  at=1:7+0.4,  subset=survey==2013005, col=box.col[3], show.names=FALSE)
legend(0.3,60, c("November 2012", "May 2013", "November 2013"), fill = box.col)
dev.off()


#Plot CPUE (catch/hour/gear or fisherman) for the different gears and years
#boxplot(Catch_hr_gear ~ gear, data=station, subset=survey==2012901, boxwex=0.25,  xlab="Gear Type", ylab="Catch (kg) pr hour pr gear / fisherman", col="cornsilk2")
#boxplot(Catch_hr_gear ~ gear, data=station, add=TRUE, boxwex=0.25,  at=1:7+0.4,  subset=survey==2013002, col="darkolivegreen4")
#legend(1,3, c("November 2012", "May 2013"), fill = c("cornsilk2", "darkolivegreen4"))

#Comparing trap catches in North and South Area  - by year
#Only look at Big Traps as we know from Nov'12 that smaller traps catch less
traps<-subset(station, gear=="TB")
pdf(file="traps catch N vs S.pdf")
boxplot(weight ~ survey, data=traps, subset=lat<20, boxwex=0.25, names=c("Nov 2012", "May 2013", "Nov 2013"), xlab="Survey (Year)", ylab="Catch (kg) pr trap", col="cornsilk2")
boxplot(weight ~ survey, data=traps, add=TRUE, boxwex=0.25,  at=1:3+0.3,  subset=lat>20, col="darkolivegreen4", show.names=FALSE)
legend(0.5,18, c("South (of PS)", "North (of PS)"), fill = c("cornsilk2", "darkolivegreen4"))
dev.off()

## TRAPS CATC RATE BY HRS DAYLIGHT
#using ggplot

daylight <- ggplot(traps, aes(daylight, weight))
daylight + geom_point(aes(size=2, colour = factor(survey))) #simples scatterplot for all data
ggsave("catch by daylight.pdf", scale = 1, dpi = 400)



## TRAPS CATCH RATE BY DEPTH
#plot catch by depth
traps<-subset(station, gear=="TB")
attach(traps)
plot(weight~bdep, subset=weight>0)
dcat<-c(ceiling(bdep/10)*10)
dcat<-factor(c(dcat))
traps<-cbind(traps,dcat)
dcatlab<-factor(c(dcat))
catchatdepth<-by(traps$weight, traps$dcat,  mean)
#barplot(catchatdepth, col="maroon4", main="Mean catch weight at different depth categories", xlab="10m depth categories", ylab="Mean catch weight (kg)")
#histogram(~weight|dcatlab ) #need to load Lattice
pdf(file="catch by depth.pdf")
bwplot(dcat~weight, data=traps, scales="free", ylab="Depth category (10m interval)", xlab="mean catch weight")
dev.off()
detach(traps)


#--------------------------------------------------
# ANALYSIS OF SPECIES SPECIFIC CATCH DATA
#--------------------------------------------------

## SPECIES CATCH DATA ANALYSIS
# Species exploratory analysis
by(species[4:5], species["Sci_name"], summary)
plot(species[4:5])
par(mar=c(5,10,4.1,2.1))
boxplot(weight ~ Sci_name, data=species, col="olivedrab", las=2, xlab="Catch weight (kg)", cex.axis=0.5, horizontal=TRUE)
boxplot(number ~ Sci_name, data=species, col="peachpuff1", las=2, xlab="Number of fish in catch", cex.axis=0.5, horizontal=TRUE)

#Making list of number of occurences pr species (number of stations where species was observed)
#par(mar=c(6,10,6,3) #setting plotting margins
ssp<-subset(species, weight>0) #remove stations with no catch
attach(ssp)
sorted_sci<-c(sort(summary(Sci_name), decreasing=TRUE))
ssci<-as.data.frame(sorted_sci)
scnames<-rownames(ssci)
barplot(ssci[[1]], names.arg=scnames, horiz=TRUE, col="orange", xlab="Number of stations where species was caught", main="All gear types", cex.names=0.4, las=1)
detach(ssp)
# Only for species with 3 or more occurrence
M1ssci<-subset(ssci, sorted_sci>2) 
scnames<-rownames(M1ssci)
barplot(M1ssci[[1]], names.arg=scnames, horiz=TRUE, col="red3", xlab="Number of stations where species was caught", main="All gear types", cex.names=0.6, las=1)



# Same analysis, but by TRAPS
ss1<-subset(species, weight>0) #remove stations with no catch
ssp<-subset(ss1, gear=="TB")
attach(ssp)
sorted_sci<-c(sort(summary(Sci_name), decreasing=TRUE))
ssci<-as.data.frame(sorted_sci)
scnames<-rownames(ssci)
barplot(ssci[[1]], names.arg=scnames, horiz=TRUE, col="green3", xlab="Number of stations where species was caught", main="Traps", cex.names=0.4, las=1)
# Only for species with 3 or more occurrence
M1ssci<-subset(ssci, sorted_sci>1) 
scnames<-rownames(M1ssci)
barplot(M1ssci[[1]], names.arg=scnames, horiz=TRUE, col="green4", xlab="Number of stations where species was caught", main="Traps", cex.names=0.7, las=1)
detach(ssp)

# Same analysis, but by HAND LINE
ss1<-subset(species, weight>0) #remove stations with no catch
ssp<-subset(ss1, gear=="HL")
attach(ssp)
sorted_sci<-c(sort(summary(Sci_name), decreasing=TRUE))
ssci<-as.data.frame(sorted_sci)
scnames<-rownames(ssci)
barplot(ssci[[1]], names.arg=scnames, horiz=TRUE, col="blue3", xlab="Number of stations where species was caught", main="Hand Line", cex.names=0.4, las=1)
# Only for species with 1 or more occurrence
M1ssci<-subset(ssci, sorted_sci>0) 
scnames<-rownames(M1ssci)
barplot(M1ssci[[1]], names.arg=scnames, horiz=TRUE, col="blue4", xlab="Number of stations where species was caught", main="Hand Line", cex.names=0.7, las=1)
detach(ssp)

# Same analysis, but by GILL NETS
ss1<-subset(species, weight>0) #remove stations with no catch
ssp<-subset(ss1, gear=="GN")
attach(ssp)
sorted_sci<-c(sort(summary(Sci_name), decreasing=TRUE))
ssci<-as.data.frame(sorted_sci)
scnames<-rownames(ssci)
barplot(ssci[[1]], names.arg=scnames, horiz=TRUE, col="brown", xlab="Number of stations where species was caught", main="Gill Nets", cex.names=0.4, las=1)
# Only for species with 1 or more occurrence
M1ssci<-subset(ssci, sorted_sci>0) 
scnames<-rownames(M1ssci)
barplot(M1ssci[[1]], names.arg=scnames, horiz=TRUE, col="brown2", xlab="Number of stations where species was caught", main="Gill Nets", cex.axis=0.9,  cex.names=0.6, las=1)
detach(ssp)



