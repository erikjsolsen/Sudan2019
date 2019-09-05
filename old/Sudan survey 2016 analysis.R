#' @title Analysis of Oct - Dec 2016 Sudan Survey
#' @description mapping, analysis of data to pick out errors etc
#' @description used to genereate survey maps and check for errors in STATION and BIOLOGY data
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
stations<-read.delim("Hl and TRAPS/2016002stations.txt", dec=",")
biology<-read.delim("Hl and TRAPS/2016002data.txt", dec=",")
species<-read.delim("Hl and TRAPS/2016002species.txt", dec=",")
BRUV_UVC<-read.csv2("BRUV and UVC/BRUV_UVC.csv")
#ctd<-read.csv2("CTD/CTD2015.csv")

#' giving correct column names
bc<-biology
sc<-stations
colnames(bc)[11]<-c("weight")
colnames(bc)[10]<-c("length")
colnames(bc)[13]<-c("gonads")
colnames(bc)[20]<-c("duration")

sc$Station.Id<-sc$Stat_Unique..Key
colnames(sc)[2]<-c("Station")
colnames(sc)[11]<-c("depth")
colnames(sc)[12]<-c("date_start")
colnames(sc)[14]<-c("time_start")
colnames(sc)[15]<-c("time_stop")
colnames(sc)[17]<-c("date_stop")
colnames(sc)[23]<-c("Duration2")

colnames(species)[2]<-c("species_code")
colnames(species)[3]<-c("name_latin")
colnames(species)[4]<-c("name_local")
colnames(species)[9]<-c("name_english")
colnames(species)[10]<-c("family_code")

colnames(BRUV_UVC)[5]<-c("Gear")

#' combining stations file with biology  for a complete station list. 

ps<-data.frame(x=37.21709967,y=19.600512, group=1, survey=2012901 )

#' combine fishing stations with BRUV & UVC stations
BRUV_UVC<-na.omit(BRUV_UVC)
stations_all<-rbind(sc[,c(2,5,7,13)], BRUV_UVC[,c(2,14,16,5)])
stations_all$Survey<-1


#' CORRECT NAMES and POSITIONS according to STAIONS
#' 

bc$lat<-c(1)
bc$lon<-c(1)
bc$name<-c("name")

for (i in 1:length(bc$lat)) 
{
  bc$lat[i]<- subset(sc, Station==bc$Station[i])[,5]
  bc$lon[i]<- subset(sc, Station==bc$Station[i])[,7]
  bc$name[i]<- as.character(subset(sc, Station==bc$Station[i])[,3])
}

write.csv2(bc, "survey_2016002_biology_corr_name_pos_effort_soak.csv")

#' MAPPING ALL DATA
#' --------------------
world<- map_data("worldHires", c("Sudan", "Egypt", "Ethiopia")) 

stationmap<-ggplot(world, aes(x=long, y=lat, group=group)) + geom_polygon(colour="gray35", fill="gray85") + geom_point(data=stations_all, shape="+", size=6, aes(x=Longitude, y=Latitude, group=Survey, colour= Gear))  +  coord_cartesian(xlim = c(36.6, 38.9), ylim=c(17.7, 22.5)) + theme_classic() 
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
survey16map<-ggplot(stations_all, aes(x=Longitude, y=Latitude, group=Gear)) + geom_point(colour = "blue", shape="+", size=6)  + facet_wrap(~Gear, nrow=1, ncol=5)   + geom_polygon(data=world, colour="gray35", fill="gray85", aes(x=long, y=lat, group=group))  + geom_text(data=ps, label="PZU", size=4,  hjust=1.15,  aes(x=x, y=y, group=group)) +  coord_cartesian(xlim = c(36.8, 38.9), ylim=c(17.7, 22.5)) + theme_classic() 
survey16map
ggsave("surveymap_by_gear_2016.png")
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
durationplot<-ggplot(sc, aes(x=Duration2/60, group=Gear)) + geom_histogram(binwidth = 1, aes(fill=Gear)) + theme_bw() +facet_wrap(~Gear, scales = "free_y") + scale_fill_brewer(palette = "Set1") + xlab("Duration (hours)")
durationplot

#' depth histogram by gear
depthplot<-ggplot(sc, aes(x=depth, group=Gear)) + geom_histogram(binwidth = 5, aes(fill=Gear)) + theme_bw() +facet_wrap(~Gear, scales = "free_y") + scale_fill_brewer(palette = "Set1") + xlab("Depth (m)")
depthplot

#' DNA samples by species

bc_dna<-bc[grep("S", bc$DNAvials),]
bc_dna<-bc_dna[grep("E", bc_dna$DNAvials, invert=TRUE),]

spdna<-as.data.frame(cbind(as.character(unique(bc_dna$Species)), count=1, name="name"))
colnames(spdna)<-c("species_code", "nosamples", "latin_name")
spdna$nosamples<-c(1)
spdna$latin_name<-c("test")

for (i in 1:length(spdna$species)) {
  spdna$nosamples[i]<-as.integer(nrow(subset(bc, Species == as.character(spdna$species_code[i]))))
  spdna$latin_name[i]<-as.character(subset(species, species_code == as.character(spdna$species_code[i]))[,3])
}

dnasampleplot<-ggplot(spdna, aes(latin_name, nosamples)) +geom_bar(stat="identity", aes(fill=latin_name))+ scale_fill_brewer(palette = "Set1", guide= "none") + ggtitle(" DNA samples pr species during 2016 survey") + coord_flip() + theme_bw() +xlab("") +ylab("Number of DNA samples taken")
dnasampleplot
