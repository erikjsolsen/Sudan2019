#' @title Analysis of Oct - Dec 2016 Sudan Survey
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
setwd("~/ownCloud/Research/Sudan/2016/Survey OctDec2016/survey data")
#bruv<-read.csv2("BRUV UVC/BRUV UVC stations FINAL.csv", dec=",") 
stations<-read.delim("Hl and TRAPS/red sea 2016002 station export.txt", )
biology<-read.delim("Hl and TRAPS/red sea 2016002 data export.txt")
#ctd<-read.csv2("CTD/CTD2015.csv")

#' selecting columns for stations and biology & adding positions to biology
bc<-c(2,3,4,5,6,8,10,11,12,13,14,15,17,19,20,21)
sc<-c(2,3,5,7,10,11,12,13,14,15,16, 17)
bc<-biology[,bc]
sc<-stations[,sc]
bc$Station<-bc$SurvStat
colnames(bc)[9]<-c("weight")
colnames(bc)[6]<-c("duration")
colnames(bc)[8]<-c("length")
colnames(bc)[11]<-c("gonads")

sc$Station.Id<-sc$Stat_Unique..Key
colnames(sc)[1]<-c("Station")
colnames(sc)[6]<-c("depth")
colnames(sc)[7]<-c("date_start")
colnames(sc)[9]<-c("time_start")
colnames(sc)[10]<-c("time_stop")
colnames(sc)[12]<-c("date_stop")


#' combining stations file and BRUV + UVC for a complete station list. 
#select same columns from BRUV and stations
#give same names
#colnames(bruv)<-colnames(stations)
#AllGear<-rbind(bc, sc)

ps<-data.frame(x=37.21709967,y=19.600512, group=1, survey=2012901 )


#' CORRECT NAMES and POSITIONS according to STAIONS
#' 

bc$lat<-c(1)
bc$lon<-c(1)
bc$name<-c("name")

for (i in 1:length(bc$lat)) 
{
  bc$lat[i]<- subset(sc, Station==bc$Station[i])[,3]
  bc$lon[i]<- subset(sc, Station==bc$Station[i])[,4]
  bc$name[i]<- as.character(subset(sc, Station==bc$Station[i])[,2])
}

write.csv2(bc, "survey_2016002_biology_corr_name_pos_effort_soak.csv")

#' MAPPING ALL DATA
#' --------------------
world<- map_data("worldHires", c("Sudan", "Egypt", "Ethiopia")) 

stationmap<-ggplot(world, aes(x=long, y=lat, group=group)) + geom_polygon(colour="gray35", fill="gray85") + geom_point(data=sc, shape="+", size=6, aes(x=Longitude, y=Latitude, group=Survey, colour= Gear))  +  coord_cartesian(xlim = c(36.6, 38.9), ylim=c(17.7, 22.5)) + theme_classic() 
stationmap 



#'adding place names
stationnames<-read.csv2("stations place names.csv")
stationnames <- subset(stationnames, AreaName != "Port Sudan")
stationmap <- stationmap + geom_point(data=stationnames, shape="#", size=6, colour="red2", aes(x=LON, y=LAT, group=Area.No)) + geom_text(data=stationnames, label=stationnames$AreaName, size=4,  hjust=1.15,  aes(x=LON, y=LAT, group=Area.No)) 
stationmap

#' reefmap
reefmap <- ggplot(world, aes(x=long, y=lat, group=group)) + geom_polygon(colour="gray35", fill="gray85") + geom_point(data=stationnames, size=3, aes(x=LON, y=LAT, group=Area.No, colour=as.factor(Part))) + geom_text(data=stationnames, label=stationnames$AreaName, size=2,  hjust=1.15,  aes(x=LON, y=LAT, group=Area.No)) + theme_classic() + scale_color_brewer(palette = "Set1", name="Survey parts") + scale_shape_discrete() + geom_point(data=ps, size=3, colour="gray35", aes(x=x, y=y)) + geom_text(data=ps, label="Port Sudan", size=3,  hjust=1.15,  aes(x=x, y=y, group=group)) +  coord_cartesian(xlim = c(36, 38.9), ylim=c(17.7, 22.5)) 

#' reefmap whole sudan
reefmap +  coord_cartesian(xlim = c(36, 38.9), ylim=c(17.7, 22)) 
ggsave("reefmap_all.png", width=4.3, height=6)

#' southern area only
reefmap +  coord_cartesian(xlim = c(36.6, 38.9), ylim=c(18.0, 19.7))
ggsave("reefmap_south.png", width=5, height=5)


#' PANELLED MAP BY GEAR
#' --------------------------
survey16map<-ggplot(sc, aes(x=Longitude, y=Latitude, group=Gear)) + geom_point(colour = "blue", shape="+", size=6)  + facet_wrap(~Gear)   + geom_polygon(data=world, colour="gray35", fill="gray85", aes(x=long, y=lat, group=group))  + geom_text(data=ps, label="PZU", size=4,  hjust=1.15,  aes(x=x, y=y, group=group)) +  coord_cartesian(xlim = c(36.8, 38.9), ylim=c(17.7, 22.5)) + theme_classic() 
survey16map
#' 

#' L-W plots
lwplot <- ggplot(bc, aes(x=length, y=weight, group=Species)) + geom_point(colour="steelblue") + facet_wrap(~Species, scale ="free")
lwplot

# some errors need to be corrected 29.10:
# HOLMY02, HOLSA05, LUTLU06, LUTLU18, SERVA01, MURGY13

#' CHECK OF DATA
#' 
#' Duration
hist(bc$duration/60)
summary(bc$duration/60)

#' duration histogram by gear
durationplot<-ggplot(bc, aes(x=duration/60, group=Gear)) + geom_histogram(binwidth = 1, aes(fill=Gear)) + theme_bw() +facet_wrap(~Gear, scales = "free_y") + scale_fill_brewer(palette = "Set1") + xlab("Duration (hours)")
durationplot

#' depth histogram by gear
depthplot<-ggplot(sc, aes(x=depth, group=Gear)) + geom_histogram(binwidth = 5, aes(fill=Gear)) + theme_bw() +facet_wrap(~Gear, scales = "free_y") + scale_fill_brewer(palette = "Set1") + xlab("Depth (m)")
depthplot
