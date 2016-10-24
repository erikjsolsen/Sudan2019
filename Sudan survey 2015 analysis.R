#' @title Analysis of Oct 2015 Sudan Survey
#' @description mapping, analysis of data to pick out errors etc
#' @description updated with data files for entire survey
#' 

#' LIBRARIES AND SOURCES
#' ------------------------
library("maps")
library("mapdata")
library("ggplot2")
library("RColorBrewer")


#' IMPORTING DATA
#' ------------------------
setwd("~/ownCloud/Research/Sudan/2015/SurveyNov15/Survey data")
bruv<-read.csv2("BRUV UVC/BRUV UVC stations FINAL.csv", dec=",") 
stations<-read.csv2("trap and HL data/survey 2015004 STATION LIST FINAL.csv")
#biology<-read.csv2("trap and HL data/survey 2015004 BIOLOGY FINAL v010316.csv")
biology<-read.csv2("trap and HL data/survey 2015004 BIOLOGY 19102016.csv")
ctd<-read.csv2("CTD/CTD2015.csv")

#' combining data
bc<-c(1,2,5,14,16)
sc<-c(1,2,18,13,16)
bc<-bruv[,bc]
sc<-stations[,sc]
colnames(bc)<-colnames(sc)
AllGear<-rbind(bc, sc)

ps<-data.frame(x=37.21709967,y=19.600512, group=1, survey=2012901 )


#' CORRECT NAMES and POSITIONS according to STAIONS
#' 

biology$lat<-c(1)
biology$lon<-c(1)
c_factors<-c(levels(biology$Area),levels(stations$AreaName))
levels(biology$Area)<-c_factors


stations$DTG_start<-c(as.POSIXct(paste(as.character(stations$Date.Start), as.character(stations$Time.Start)), format = "%d.%m.%y  %H:%M"))
stations$DTG_stop<-c(as.POSIXct(paste(as.character(stations$Date.Stop), as.character(stations$Time.Stop)), format = "%d.%m.%y  %H:%M"))
stations$Soak_time<-stations$DTG_stop-stations$DTG_start

for (i in 1:length(biology$lat)) 
{
  biology$lat[i]<- subset(stations, Station.No==biology$StationNo[i])[,13]
  biology$lon[i]<- subset(stations, Station.No==biology$StationNo[i])[,16]
  biology$Effort[i] <- subset(stations, Station.No==biology$StationNo[i])[,18]
  biology$Minutes.soak_time.[i] <- subset(stations, Station.No==biology$StationNo[i])[,26]
}

write.csv2(biology, "survey_2015004_biology_corr_name_pos_effort_soak.csv")

#' MAPPING ALL DATA
#' --------------------
world<- map_data("worldHires", c("Sudan", "Egypt", "Ethiopia")) 

bruvmap<-ggplot(world, aes(x=long, y=lat, group=group)) + geom_polygon(colour="gray35", fill="gray85") + geom_point(data=AllGear, shape="+", size=6, aes(x=Longitude, y=Latitude, group=Survey, colour= Gear))  +  coord_cartesian(xlim = c(36.8, 38.9), ylim=c(17.7, 22.5)) + theme_classic() 
bruvmap 

#' PANELLED MAP BY GEAR
#' --------------------------
survey15map<-ggplot(AllGear, aes(x=Longitude, y=Latitude, group=Gear)) + geom_point(colour = "blue", shape="+", size=6)  + facet_wrap(~Gear, as.table=FALSE)   + geom_polygon(data=world, colour="gray35", fill="gray85", aes(x=long, y=lat, group=group))  + geom_text(data=ps, label="PZU", size=4,  hjust=1.15,  aes(x=x, y=y, group=group)) +  coord_cartesian(xlim = c(36.8, 38.9), ylim=c(17.7, 22.5)) + theme_classic() 
survey15map
#' 

#' MAP OF ALL CATCH POSITIONS & PLACE NAMES
#' --------------------------

setwd("~/ownCloud/Research/Sudan/2015/SurveyNov15/cruise plan")
stationnames<-read.csv2("stations place names.csv")
surveymap<-ggplot(world, aes(x=long, y=lat, group=group)) + geom_polygon(colour="gray35", fill="gray70") + geom_point(data=AllGear, shape="+", size=4, colour="steelblue1", aes(x=Longitude, y=Latitude, group=Gear)) + geom_point(data=stationnames, shape="*", size=8, colour="red2", aes(x=LON, y=LAT, group=Area.No))+  coord_cartesian(xlim = c(36, 39), ylim=c(17, 22.5)) + theme_classic() + geom_text(data=stationnames, label=stationnames$AreaName, size=4,  hjust=1.15,  aes(x=LON, y=LAT, group=Area.No)) 
surveymap 
