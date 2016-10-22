#' @title Analysis of Oct 2015 Sudan Survey
#' @description mapping, analysis of data to pick out errors etc
#' 

#' LIBRARIES AND SOURCES
#' ------------------------
library("maps", lib.loc="/Users/eriko/Library/R/3.0/library")
library("mapdata", lib.loc="/Users/eriko/Library/R/3.0/library")
library("ggplot2", lib.loc="/Users/eriko/Library/R/3.0/library")
library("RColorBrewer", lib.loc="/Users/eriko/Library/R/3.0/library")


#' IMPORTING DATA
#' ------------------------
setwd("~/ownCloud/Research/Sudan/2015/SurveyNov15/Survey data")
bruv<-read.csv2("BRUV UVC/BRUV UVC stations 0211.csv", dec=".") 
stations<-read.csv2("trap and HL data/station list 03112015.csv")

#' combining data
bc<-c(1,2,5,13,15)
sc<-c(1,2,18,13,16)
bc<-bruv[,bc]
sc<-stations[,sc]
colnames(bc)<-colnames(sc)
AllGear<-rbind(bc, sc)

#' MAPPING DATA
#' --------------------
world<- map_data("worldHires", c("Sudan", "Egypt", "Ethiopia")) 

bruvmap<-ggplot(world, aes(x=long, y=lat, group=group)) + geom_polygon(colour="gray35", fill="gray85") + geom_point(data=AllGear, shape="+", size=6, aes(x=Longitude, y=Latitude, group=Survey, colour= Gear))  +  coord_cartesian(xlim = c(36.5, 38), ylim=c(19.5, 22.5)) + theme_classic() 
bruvmap 

#' MAP FOR SURVEY PART 2 and 3
#' --------------------------
setwd("/Users/eriko/ownCloud/Research/Sudan/SurveyProj2012_14/All_data")
catch<-read.csv2("catch.csv")
setwd("~/ownCloud/Research/Sudan/2015/SurveyNov15/cruise plan")
part2_3<-read.csv2("stations part 2 and 3.csv")
surveymap<-ggplot(world, aes(x=long, y=lat, group=group)) + geom_polygon(colour="gray35", fill="gray70") + geom_point(data=catch, shape="+", size=4, colour="steelblue1", aes(x=lon, y=lat, group=survey)) + geom_point(data=part2_3, shape="*", size=8, colour="red2", aes(x=LON, y=LAT, group=Area.No))+  coord_cartesian(xlim = c(36.5, 39), ylim=c(17, 22.5)) + theme_classic() + geom_text(data=part2_3, label=part2_3$AreaName, size=4,  hjust=1.15,  aes(x=LON, y=LAT, group=Area.No)) 
surveymap + geom_point(data=AllGear, shape="x", size=5, colour="darkblue", aes(x=Longitude, y=Latitude, group=Survey))

# add plot of positions from pilot survey

