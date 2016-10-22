# Sudan 2012 and 2013 Survey
#-------------------
# analysis commands
#
# By: Erik Olsen
# Created: 12.11.2012
# Updated: 11.09.2013

# This code is used to load all survey data from the Nov'12 and May/June'13 surveys in the Red Sea, Sudan. 
#-------------------------------------------


#--------------------------------------------
# IMPORTING DATA TO R
#--------------------------------------------

#Set working directory
#Must be changed to according to your own directories
setwd("/Users/eriko/Documents/G-copy/research/Sudan/SurveyMay13/Analyses")

# Import station data
station<-read.table("stations.csv",header=TRUE, dec=",", sep=";")
#traps<-subset(catches, Trap==1)

# Import species data
species<-read.table("catch.csv", header=TRUE, dec=",",sep=";")
#remove empty catches
#species2<-subset(species1, Sci_name!="")
#species<-subset(species2, Sci_name!="N O   S A M P L E")

# Import Length Data
length<-read.table("length.csv", header=TRUE, dec=",",sep=";")


#---------------------------------------------------------------
# EXPLORATORY ANALYSIS OF STATION DATA
#---------------------------------------------------------------
# Stations Exploratory statistical analysis
summary(station)
station13<-(subset(station, survey==2013002))
by(station13[14], station13[7], summary)

summary(station $ gear)
#subset by year
summary(subset(station, survey==2012901)) #for 2012
summary(subset(station, survey==2013002))  #for 2013



#-------------------------------------
# COMPARISON OF CATCHES PR STATION
#----------------------------------
# COMPARISON OF GEARS AND YEARS
#Plot catch weigh by different gears and years
boxplot(weight ~ gear, data=station, subset=survey==2012901, boxwex=0.25,  xlab="Gear Type", ylab="Catch weight (kg)", col="snow3")
boxplot(weight ~ gear, data=station, add=TRUE, boxwex=0.25,  at=1:7+0.4,  subset=survey==2013002, col="darkslateblue")
legend(1,30, c("November 2012", "May 2013"), fill = c("snow3", "darkslateblue"))

#Plot CPUE (catch/hour/gear or fisherman) for the different gears and years
boxplot(Catch_hr_gear ~ gear, data=station, subset=survey==2012901, boxwex=0.25,  xlab="Gear Type", ylab="Catch (kg) pr hour pr gear / fisherman", col="cornsilk2")
boxplot(Catch_hr_gear ~ gear, data=station, add=TRUE, boxwex=0.25,  at=1:7+0.4,  subset=survey==2013002, col="darkolivegreen4")
legend(1,3, c("November 2012", "May 2013"), fill = c("cornsilk2", "darkolivegreen4"))

#Comparing trap catches in North and South Area  - by year
#Only look at Big Traps as we know from Nov'12 that smaller traps catch less
traps<-subset(station, gear=="TB")
boxplot(Catch_hr_gear ~ survey, data=traps, subset=LAT<20, boxwex=0.25,  xlab="Survey (Year)", ylab="Catch (kg) pr hour pr trap", col="cornsilk2")
boxplot(Catch_hr_gear ~ survey, data=traps, add=TRUE, boxwex=0.25,  at=1:2+0.3,  subset=LAT>20, col="darkolivegreen4")
legend(0.5,1, c("South (of PS)", "North (of PS)"), fill = c("cornsilk2", "darkolivegreen4"))



#Plot catch weight vs catch no
## NB Nansis does not export catch numbers :-( 
plot(station[13:14])

## TRAPS CATCH RATE BY DEPTH
#plot catch by depth
traps<-subset(station, gear=="TB")
attach(traps)
plot(weight~geardepthstart, subset=weight>0)
dcat<-c(ceiling(geardepthstart/10)*10)
dcat<-factor(c(dcat))
traps<-cbind(traps,dcat)
dcatlab<-factor(c(dcat))
catchatdepth<-by(traps$weight, traps$dcat,  mean)
barplot(catchatdepth, col="maroon4", main="Mean catch weight at different depth categories", xlab="10m depth categories", ylab="Mean catch weight (kg)")
histogram(~weight|dcatlab ) #need to load Lattice
bwplot(dcat~weight, data=traps, scales="free", ylab="Depth category (10m interval)", xlab="mean catch weight")
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


#-------------------------------------------------
# LENGTH FREQUENCY ANALYSIS
# Run 6. Sept. 2013
#-------------------------------------------------
## LENGTH FREQUENCIES
# Length-frequenciesb for all species
rn_l<-length$L_cm
row.names(length)<-rn_l
df_length<-data.matrix(length[2:93])
#barplot(df_length, beside=TRUE)
#plot(length[1], length[2:4])
#NB!! Load lattice in "packages&Data" in menu bar
barchart(length[2:7])
barplot(as.matrix(length[48]), main="Length", ylab="Total", xlab=rn_l, beside=TRUE, col=terrain.colors(5))
#bruk Lattice 
attach(length)
histogram( length[,2]~length[,4] | interaction(length[,1]) )
histogram( length[,2]~length[,4] )

for (i in 2:93) {
	pdf(paste(colnames(length[i]), ".pdf", sep = ""))
	my.plot<-histogram(rep_l<-rep(length[,1],  length[,i]), type=c("count"), xlab=colnames(length[i]))
	print(my.plot)
	dev.off()
	
	}

 
