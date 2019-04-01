#' @title Sudan Survey maps
#' @description simple maps plotting all sampling stations
#' @author Erik Olsen
#' 

#' LIBRARIES AND SOURCES
#' -----------------------------
library(maps)
library(mapdata)
library(ggplot2)
library(RColorBrewer)
library(rgdal)
library(rgeos)


#' IMPORT DATA
#' -----------------------------
setwd("/Users/eriko/ownCloud/Research/Sudan/SurveyProj2012_14/All_data")
catch<-read.csv2("catch.csv")
catch$survey<-as.factor(catch$survey)
ps<-data.frame(x=37.21709967,y=19.600512, group=1, survey=2012901, survey_m=c("Nov. 2012") )

#' MAKE MAP
#' ----------------
#' 
world<- map_data("worldHires", c("Sudan", "Egypt", "Ethiopia")) 
sudan.map <- ggplot(world, aes(x=long, y=lat, group=group)) + geom_point(data=catch, shape="+", size=6, aes(x=lon, y=lat, group=survey, colour= survey)) + geom_polygon(colour="gray35", fill="gray85") +  coord_cartesian(xlim = c(36.5, 39), ylim=c(17, 22.5)) + theme_classic() + geom_point(data=ps, size=6, colour="gray35", aes(x=x, y=y)) + geom_text(data=ps, label="Port Sudan", size=6,  hjust=1.15,  aes(x=x, y=y, group=group)) +scale_colour_brewer(type = "seq", palette = "Dark2", name="Survey month & year", labels=c("Nov. 2012", "May. 2013", "Nov. 2013")) + theme(legend.title = element_text(size=16, face="bold")) + theme(legend.text = element_text(size=14))
sudan.map 

ggsave("sudan_stations_12_13.png")


#' MAP faceted by year
world<- map_data("worldHires", c("Sudan", "Ethiopia")) 
#world$survey<-2012901
world$survey_m<-c("Survey 1: Nov. 2012")
world2<-world
#world2$survey<-2013002
world2$survey_m<-c("Survey 2: May 2013")
world3<-world
#world3$survey<-2013005
world3$survey_m<-c("Survey 3: Nov. 2013")
world<-rbind(world, world2, world3)

#' Add Month. Year survey column
catch$survey_m<-as.character(catch$survey)
catch$survey_m<-replace(catch$survey_m, grep("2012901", catch$survey_m), "Survey 1: Nov. 2012")
catch$survey_m<-replace(catch$survey_m, grep("2013002", catch$survey_m), "Survey 2: May 2013")
catch$survey_m<-replace(catch$survey_m, grep("2013005", catch$survey_m), "Survey 3: Nov. 2013")

ps<-rbind(data.frame(x=37.21709967,y=19.600512, group=1, survey_m=c("Survey 1: Nov. 2012")),data.frame(x=37.21709967,y=19.600512, group=1, survey_m=c("Survey 2: May 2013") ),data.frame(x=37.21709967,y=19.600512, group=1, survey_m=c("Survey 3: Nov. 2013") ))

sudan.map2<-ggplot(catch, aes(x=lon, y=lat, group=survey_m)) + geom_point(shape="+", size=6, aes(group=survey_m, colour= survey_m)) + facet_wrap(~survey_m, ncol=3) + geom_polygon(data=world, colour="gray35", fill="gray85", aes(x=long, y=lat, group=survey_m)) +  coord_cartesian(xlim = c(36.5, 39), ylim=c(17.7, 22.5)) + theme_classic() + geom_point(data=ps, size=5, colour="gray35", aes(x=x, y=y)) + geom_text(data=ps, label="PZU", size=5,  hjust=1, vjust=-1.2,  aes(x=x, y=y, group=survey_m)) +scale_colour_brewer(type = "seq", palette = "Dark2", name="Survey month & year", labels=c("Nov. 2012", "May. 2013", "Nov. 2013")) + theme(legend.position="none")

sudan.map2

ggsave("sudan_stations_12_13_facet.tiff")

# Add MANAGEMENT AREAS from Shape file
setwd("/Users/eriko/ownCloud/Research/Sudan/maps and gis/Sudan management areas/")
ManageAreas<-readOGR(dsn = ".", "sudan_regions")
setwd("/Users/eriko/ownCloud/Research/Sudan/SurveyProj2012_14/All_data")

#setting correct projection LAT LON
ManageAreas <- spTransform(ManageAreas, CRS("+proj=longlat +ellps=GRS80"))

### Create  area map with numbers for each region
ManageAreas.f<-fortify(ManageAreas, region="id") #creates X - Y points of the polygons

cnames <- aggregate(cbind(long, lat) ~ id, data=ManageAreas.f, FUN=function(x)mean(range(x)))
cnames$id<-c(1:7)

sudan.map + geom_path( data=ManageAreas.f, aes(x=long, y=lat, group=group) )

# add management areas to faceted sudan map 
#figure 3 in MS
sudan.map2 + geom_path( data=ManageAreas.f, aes(x=long, y=lat, group=group) )
ggsave("sudan_stations_12_13.png")

#### Add bathymetri
library("marmap", lib.loc="/Users/eriko/Library/R/3.0/library")
redsea<-readGEBCO.bathy("/Users/eriko/ownCloud/Research/Sudan/maps and gis/GEBCO_red_sea/red_sea.nc")
sudan<-readGEBCO.bathy("/Users/eriko/ownCloud/Research/Sudan/maps and gis/GEBCO_sudan/sudan.nc")
blues <- colorRampPalette(c("midnightblue", "deepskyblue3", "deepskyblue1", "cadetblue3", "cadetblue1", "darkseagreen1", "white"))
#blues <- colorRampPalette(c("red","purple","blue","cadetblue1"))
#plot(redsea, n = 1, image = TRUE, bpal = blues(100), main = "Red Sea GEBCO data")
plot(sudan, n=1, image = TRUE, bpal=blues(100), main ="Red Sea Sudan")

plot(sudan, image = TRUE, bpal = blues(1000), deep = c(-3000, -2000, 0), shallow = c(-2000, -1, 0), step = c(1000, 50, 0), lwd = c(0.8, 0.8, 1), lty = c(1, 1, 1), col = c("lightgrey", "darkgrey", "black"), drawlabel = c(FALSE, FALSE, FALSE))


# Bathymetric map highlighting reef areas (<20m depth)
#figure 1 for MS
# save in dim 625 X 1000 as PNG
#png(file="sudan_bathy.png", width=625, height=1000, res=400)
#png(file="sudan_bathy2.png")
#png(file="fig 1 sudan_bathy4.png", width=1900, height=3000, res=400, pointsize=10)

tiff(file="fig 1 sudan_bathy.tiff", width=1900, height=3000, res=400, pointsize=10, compression=c("none"))


plot(sudan, land = TRUE, n = 10, image = TRUE,
     bpal = list(c(min(sudan), -20, "midnightblue", "blue", "lightblue3"),
                 c(-20, 0, "lightblue1", "aquamarine1"),
                 c(0, max(sudan), "gray90", "gray50")), 
                  deep = c(-3000, -500, 0),
                  shallow = c(-500, -10, 0),
                  step = c(500, 200, 0),
                  lwd = c(0.8, 0.8, 1), lty = c(1, 1, 1),
                  col = c("lightgrey", "gray15", "black"),
                 drawlabel = c(TRUE, TRUE, TRUE))
                

scaleBathy(sudan, deg=0.5 ,x="bottomleft", inset=5)

points(37.21709967, 19.600512, pch=19)

text(37.21709967, 19.600512,"Port \nSudan", adj=c(1.2,0), font=2)
text(37.2, 21.7, "1. Marsas north \nof Dongonab", cex=0.9)
text(37.4, 20.92, "2. Dongonab", cex=0.9)
text(37.38, 20.3, "3. Arakiai", cex=0.9)
text(37.4, 19.9, "4. Port \nSudan", cex=0.9)
text(37.6, 19, "5. Suakin", cex=0.9)
text(37.9, 19.3, "6. Suakin \narchipelago", cex=0.9)
text(38.3, 18.5, "7. Agig", cex=0.9)


  
ms<-c(1,3,4,5,6,7,8)
for (i in 1:7){
  ma<-subset(ManageAreas.f, id==ms[i])
  polygon(ma$long, ma$lat, lwd=2)  
}
dev.off()



########
#alternative attempt to use ggplot (not for MS)
autoplot(sudan, geom=c("r")) + scale_fill_etopo()
autoplot(sudan, geom=c("r")) +  scale_fill_gradient(limits = c(-2847, 0), breaks=depths, low="midnightblue", high="darkseagreen1")+ geom_path( data=ManageAreas.f, aes(x=long, y=lat, group=group) ) +  coord_cartesian(xlim = c(36.5, 39), ylim=c(17.7, 22.5))

+ geom_point(data = catch , shape="+", size=6, aes(group=survey, colour= survey)) 

+   geom_contour(aes(z=z), breaks=c(-20, -50, -100, -200, -500, -1000, -2000, -4000), colour="white", size=0.1) 

autoplot(redsea, geom=c("r")) +     scale_fill_gradient(limits = c(-2847, 0))




