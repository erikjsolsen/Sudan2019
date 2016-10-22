#' @title Sudan Survey maps
#' @description simple maps plotting all sampling stations
#' @author Erik Olsen
#' 

#' LIBRARIES AND SOURCES
#' -----------------------------
library("maps", lib.loc="/Users/eriko/Library/R/3.0/library")
library("mapdata", lib.loc="/Users/eriko/Library/R/3.0/library")
library("ggplot2", lib.loc="/Users/eriko/Library/R/3.0/library")
library("RColorBrewer", lib.loc="/Users/eriko/Library/R/3.0/library")


#' IMPORT DATA
#' -----------------------------
setwd("/Users/eriko/ownCloud/Research/Sudan/SurveyProj2012_14/All_data")
catch<-read.csv2("catch.csv")
catch$survey<-as.factor(catch$survey)
ps<-data.frame(x=37.21709967,y=19.600512, group=1, survey=2012901 )

#' MAKE MAP
#' ----------------
world<- map_data("worldHires", c("Sudan", "Egypt", "Ethiopia")) 
sudan.map <- ggplot(world, aes(x=long, y=lat, group=group)) + geom_point(data=catch, shape="+", size=6, aes(x=lon, y=lat, group=survey, colour= survey)) + geom_polygon(colour="gray35", fill="gray85") +  coord_cartesian(xlim = c(36.5, 39), ylim=c(17, 22.5)) + theme_classic() + geom_point(data=ps, size=6, colour="gray35", aes(x=x, y=y)) + geom_text(data=ps, label="Port Sudan", size=6,  hjust=1.15,  aes(x=x, y=y, group=group)) +scale_colour_brewer(type = "seq", palette = "Dark2", name="Survey month & year", labels=c("Nov. 2012", "May. 2013", "Nov. 2013")) + theme(legend.title = element_text(size=16, face="bold")) + theme(legend.text = element_text(size=14))
sudan.map 

ggsave("sudan_stations_12_13.png")


#' MAP faceted by year
world<- map_data("worldHires", c("Sudan", "Ethiopia")) 
world$survey<-2012901
world2<-world
world2$survey<-2013002
world3<-world
world3$survey<-2013005
world<-rbind(world, world2, world3)

ps<-rbind(data.frame(x=37.21709967,y=19.600512, group=1, survey=2012901 ),data.frame(x=37.21709967,y=19.600512, group=1, survey=2013002 ),data.frame(x=37.21709967,y=19.600512, group=1, survey=2013005 ))

sudan.map2<-ggplot(catch, aes(x=lon, y=lat, group=survey)) + geom_point(shape="+", size=6, aes(group=survey, colour= survey)) + facet_wrap(~survey, ncol=3) + geom_polygon(data=world, colour="gray35", fill="gray85", aes(x=long, y=lat, group=survey))+  coord_cartesian(xlim = c(36.5, 39), ylim=c(17.7, 22.5)) + theme_classic() + geom_point(data=ps, size=5, colour="gray35", aes(x=x, y=y)) + geom_text(data=ps, label="PZU", size=6,  hjust=1.3,  aes(x=x, y=y, group=survey)) +scale_colour_brewer(type = "seq", palette = "Dark2", name="Survey month & year", labels=c("Nov. 2012", "May. 2013", "Nov. 2013")) + theme(legend.title = element_text(size=16, face="bold")) + theme(legend.text = element_text(size=14)) 

sudan.map2

ggsave("sudan_stations_12_13_facet.png")

