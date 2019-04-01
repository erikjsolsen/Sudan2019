############################################
#### Gridding SUDAN Red Sea catch data by FAMILY into group
# http://stackoverflow.com/questions/21889529/producing-a-1-km-grid-of-mean-pond-areas-using-coordinates-of-ponds-and-their-po

# By: Erik Olsen

###################

--------------------------------------------
# IMPORTING DATA TO R
#--------------------------------------------

library(dplyr) # for aggregation
library(ggplot2) # for plotting
library(maps)
library(mapdata)

library(lattice) #load lattice library
library(RColorBrewer)
library(plyr)
library(rgdal)
library(rgeos)



source("/Users/eriko/ownCloud/R/R functions/suncalc.r") #function to calculate  sunrise/sunset
#Set working directory
#Must be changed to according to your own directories
setwd("/Users/eriko/ownCloud/Research/Sudan/SurveyProj2012_14/All_data")

# Import species data & calculate sunrise & sunset
#catch<-read.table("catch.csv", header=TRUE, dec=".",sep=",")
catch<-read.csv2("catch.csv")
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


# set grid X and Y limits
xvals <- seq(36.5, 38.8, by=0.1)
yvals <- seq(18.3, 22.1, by=0.1)

#kategorizing the geom-data
#catch$x<-ceiling(catch$lon*10)/10
#catch$y<-ceiling(catch$lat*10)/10
catch$x<-round(catch$lon, 1)
catch$y<-round(catch$lat, 1)


###  Must split the analysis by TRAPS and GILLNETS only
## Calculate CPUE (kg/hr and no/hr)
W_CPUE<-catch$weight/catch$Fhrs
N_CPUE<-catch$number/catch$Fhrs
catch<-cbind(catch, W_CPUE, N_CPUE)



# don't want number weigth categories after all
#no. categories
#n_brks<-classIntervals(catch$N_CPUE, n=7, style="fixed", fixedBreaks=c(0, 0.01, 0.05, 0.1, 0.15, 0.5, 1, 12.5)) #define categories
#n_brks <- round(n_brks$brks,digits=2) #round
#n_catVar<-findInterval(catch$N_CPUE, n_brks, all.inside=TRUE) #assign categories
#catch<-cbind(catch,n_catVar)


## TRAPS, GILLNET and HAND-LINE CPUE data set
#remove NO CATCH data
catch<-subset(catch, Fam_name!="NO CATCH")
TBcpue<-subset(catch, gear=="TB")
GNcpue<-subset(catch, gear=="GN")
HLcpue<-subset(catch, gear=="HL")

#select top5 fish 
fish5<-c("Lutjanus bohar", "Lutjanus gibbus", "Lethrinus lentjan", "Lethrinus mahsena", "Sargocentron spiniferum")
topfish<-subset(catch, Sci_name==fish5)


#calculate Mean and Total
#Must first detach plyr to use the 'dplyr' summarise function
detach("package:plyr", unload=TRUE) 

catch_pr_cell<-as.data.frame(group_by(catch,x,y) %>% 
  summarise(mean=mean(W_CPUE),total=sum(W_CPUE)))

#top 5 fish
top_catch_pr_cell<-as.data.frame(group_by(topfish,Sci_name, survey,x,y) %>% 
  summarise(mean=mean(W_CPUE),total=sum(W_CPUE)))


#by species families
species_catch_pr_cell<-as.data.frame(group_by(catch,survey,Fam_name,x,y) %>% 
  summarise(mean=mean(W_CPUE),total=sum(W_CPUE), first(survey_m)))

#by CPUE, species families & gear type
TBspecies_catch_pr_cell<-as.data.frame(group_by(TBcpue,survey,Fam_name,x,y) %>% 
  summarise(mean=mean(W_CPUE), total=sum(W_CPUE)))

HLspecies_catch_pr_cell<-as.data.frame(group_by(HLcpue,survey,Fam_name,x,y) %>% 
  summarise(mean=mean(W_CPUE), total=sum(W_CPUE)))

GNspecies_catch_pr_cell<-as.data.frame(group_by(GNcpue,survey,Fam_name,x,y) %>% 
  summarise(mean=mean(W_CPUE), total=sum(W_CPUE)))

#create categories
library("classInt", lib.loc="/Users/eriko/Library/R/3.0/library")
library("RColorBrewer", lib.loc="/Users/eriko/Library/R/3.0/library")

#weight categories
#ALL
ALL_brks<-classIntervals(catch_pr_cell$mean, n=7, style="fixed", fixedBreaks=c(0, 0.02, 0.05, 0.1, 0.5, 1, 5, 18.2)) #define categories
ALL_brks<-round(ALL_brks$brks,digits=3) #round
ALL_catVar<-findInterval(catch_pr_cell$mean, ALL_brks, all.inside=TRUE) #assign categories
catch_pr_cell$All_catVar<-ALL_catVar

#TOP
top_brks<-classIntervals(top_catch_pr_cell$mean, n=7, style="fixed", fixedBreaks=c(0, 0.02, 0.05, 0.1, 0.5, 1, 5, 18.2)) #define categories
top_brks<-round(top_brks$brks,digits=3) #round
top_catVar<-findInterval(top_catch_pr_cell$mean, top_brks, all.inside=TRUE) #assign categories
top_catch_pr_cell$top_catVar<- top_catVar

#TB
TB_brks<-classIntervals(TBspecies_catch_pr_cell$mean, n=7, style="fixed", fixedBreaks=c(0, 0.02, 0.05, 0.1, 0.5, 1, 5, 18.2)) #define categories
TB_brks <- round(TB_brks$brks,digits=3) #round
TB_catVar<-findInterval(TBspecies_catch_pr_cell$mean, TB_brks, all.inside=TRUE) #assign categories
TBspecies_catch_pr_cell$TB_catVar<-TB_catVar

#HL
HL_brks<-classIntervals(HLspecies_catch_pr_cell$mean, n=7, style="fixed", fixedBreaks=c(0, 0.02, 0.05, 0.1, 0.5, 1, 5, 18.2)) #define categories
HL_brks <- round(HL_brks$brks,digits=3) #round
HL_catVar<-findInterval(HLspecies_catch_pr_cell$mean, HL_brks, all.inside=TRUE) #assign categories
HLspecies_catch_pr_cell$HL_catVar<- HL_catVar

#GN
GN_brks<-classIntervals(GNspecies_catch_pr_cell$mean, n=7, style="fixed", fixedBreaks=c(0, 0.02, 0.05, 0.1, 0.5, 1, 5, 18.2)) #define categories
GN_brks <- round(GN_brks$brks,digits=3) #round
GN_catVar<-findInterval(GNspecies_catch_pr_cell$mean, GN_brks, all.inside=TRUE) #assign categories
GNspecies_catch_pr_cell$GN_catVar<- GN_catVar

#create subset of catches >0 
catch_pr_cell1<-subset(catch_pr_cell, mean>=0)

#creata a plot-data set
plotdata<-merge(catch_pr_cell,expand.grid(x=xvals,y=yvals),all.y=T)
#select non NA lines
plotdata1<-subset(plotdata, mean>=0)
#create plot-data for species family data set
plotdata_s<-merge(species_catch_pr_cell,expand.grid(x=xvals,y=yvals),all.y=T)
plotdata_s<-subset(plotdata_s, mean>=0)
plotdata_s<-subset(plotdata_s, Fam_name!="NO CATCH")

## Create PLOTDATA for GEAR and CPUE
TBplotdata<-merge(TBspecies_catch_pr_cell,expand.grid(x=xvals,y=yvals),all.y=T)
HLplotdata<-merge(HLspecies_catch_pr_cell,expand.grid(x=xvals,y=yvals),all.y=T)
GNplotdata<-merge(GNspecies_catch_pr_cell,expand.grid(x=xvals,y=yvals),all.y=T)
topplotdata<-merge(top_catch_pr_cell,expand.grid(x=xvals,y=yvals),all.y=T)

#select non NA lines
TBplotdata<-subset(TBplotdata, mean>=0)
HLplotdata<-subset(HLplotdata, mean>=0)
GNplotdata<-subset(GNplotdata, mean>=0)
topplotdata<-subset(topplotdata, mean>=0)

#' Add Month. Year survey column
TBplotdata$survey_m<-as.character(TBplotdata$survey)
TBplotdata$survey_m<-replace(TBplotdata$survey_m, grep("2012901", TBplotdata$survey_m), "Nov. 2012")
TBplotdata$survey_m<-replace(TBplotdata$survey_m, grep("2013002", TBplotdata$survey_m), "May 2013")
TBplotdata$survey_m<-replace(TBplotdata$survey_m, grep("2013005", TBplotdata$survey_m), "Nov. 2013")
TBplotdata$survey_f<-factor(TBplotdata$survey_m, levels=c('Nov. 2012','May 2013','Nov. 2013'))
  

HLplotdata$survey_m<-as.character(HLplotdata$survey)
HLplotdata$survey_m<-replace(HLplotdata$survey_m, grep("2012901", HLplotdata$survey_m), "Nov. 2012")
HLplotdata$survey_m<-replace(HLplotdata$survey_m, grep("2013002", HLplotdata$survey_m), "May 2013")
HLplotdata$survey_m<-replace(HLplotdata$survey_m, grep("2013005", HLplotdata$survey_m), "Nov. 2013")
HLplotdata$survey_f<-factor(HLplotdata$survey_m, levels=c('Nov. 2012','May 2013','Nov. 2013'))


GNplotdata$survey_m<-as.character(GNplotdata$survey)
GNplotdata$survey_m<-replace(GNplotdata$survey_m, grep("2012901", GNplotdata$survey_m), "Nov. 2012")
GNplotdata$survey_m<-replace(GNplotdata$survey_m, grep("2013002", GNplotdata$survey_m), "May 2013")
GNplotdata$survey_m<-replace(GNplotdata$survey_m, grep("2013005", GNplotdata$survey_m), "Nov. 2013")
GNplotdata$survey_f<-factor(GNplotdata$survey_m, levels=c('Nov. 2012','May 2013','Nov. 2013'))


topplotdata$survey_m<-as.character(topplotdata$survey)
topplotdata$survey_m<-replace(topplotdata$survey_m, grep("2012901", topplotdata$survey_m), "Nov. 2012")
topplotdata$survey_m<-replace(topplotdata$survey_m, grep("2013002", topplotdata$survey_m), "May 2013")
topplotdata$survey_m<-replace(topplotdata$survey_m, grep("2013005", topplotdata$survey_m), "Nov. 2013")
topplotdata$survey_f<-factor(topplotdata$survey_m, levels=c('Nov. 2012','May 2013','Nov. 2013'))


il<-c("<0.02", "0.02-0.05", "0.05-0.1","0.1-0.5", "0.5-1", ">1")
#il<-c("<0.02", "0.02-0.05", "0.05-0.1","0.1-0.5", "0.5-1", "1-5", ">5")

#plot TOTAL data using ggplot
gridmap<-ggplot(plotdata1) + geom_tile(aes(x,y,fill=factor(ALL_catVar))) + scale_fill_brewer(palette="OrRd", labels=il) + labs(x = "LON", y = "LAT", fill = "kg/hr fishing") 
  gridmap
  ggtitle("CPUE all GEAR and all SURVEYS combined")

ggplot(TBplotdata) + theme_bw()    

# trying to plot sample areas
ggplot(plotdata1) + geom_tile(data=plotdata1,aes(x=x,y=y),colour="black",fill="white",alpha=0,lwd=0.4)

#adding Sudan world map
sudanmap<-map_data("worldHires", "Sudan")
#select only E part of country
sudanmap2<-subset(sudanmap, long>36)

#Selecting only coastline to plot as polygon
sudanmap2<-subset(sudanmap, long>=36.5)
sudanmap2<-subset(sudanmap2, long<38.7)
sudanmap2<-subset(sudanmap2, lat>=18)
sudanmap2<-subset(sudanmap2, lat<=22)
dim(sudanmap2)
sudansmall<-sudanmap2[1:4]
top<-c(36.5, 22, 1, 8621)
bottom<-c(36.5, 18, 1, 8622)
sudanmap3<-rbind(sudansmall, top, bottom)



#Plotting grid & coastline
RedSeaMap<-gridmap + geom_path( data=sudanmap, aes(x=long, y=lat, group=group) )+ xlim(36.5, 39) +ylim(18, 22)
RedSeaMap

### Map of total catches pr cell - 
# figure 6 in MS
RedSeaMap2<-gridmap + theme_bw() + geom_polygon(data=sudanmap3, aes(x=long, y=lat, group=group, fill=group), fill="gray90") + xlim(36.5, 39) +ylim(18, 22)  
RedSeaMap2
#RedSeaMap2 + expand_limits(x = c(37,39), y = c(18,22))

ggsave("total biomass all gear.pdf", scale = 1, dpi = 400) # save plot to file

######
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

# add manageareas to RedSeaMap2
# new figure 6 in MS
mlon<-c(37, 37.4, 37.2, 37.3, 37.5, 37.9, 38.3)
mlat<-c(21.7, 20.92, 20.3, 19.9, 19, 19.3, 18.5)
mname<-c(1, 2, 3, 4, 5, 6, 7)
group<-c(1,1,1,1,1,1,1)
areanum<-as.data.frame(cbind(mlon, mlat, mname, group))
colnames(areanum)<-c("lon", "lat", "mname", "group")

#RedSeaMap2 + geom_path( data=ManageAreas.f, aes(x=long, y=lat, group=group) ) + geom_text(data=areanum,  aes(x=lon, y=lat, label=mname)) + geom_point(data=catch, shape="+", size=6, aes(x=lon, y=lat, group=survey, colour= survey))
RedSeaMap2 + geom_path( data=ManageAreas.f, aes(x=long, y=lat, group=group) ) + geom_text(data=areanum,  aes(x=lon, y=lat, label=mname)) 

# figure 5 in MS
ggsave("total biomass all gear areas.tiff", scale = 1.5, dpi = 400) # save plot to file

#####################

## Faceted CPUE plotes by GEAR type
#Plotting gridded catches by species family
fam_map<-ggplot(plotdata_s) + geom_tile(aes(x,y,fill=mean)) + scale_fill_continuous(low="khaki2",high="red4") + facet_wrap(~Fam_name + survey)
fam_map + geom_polygon(data=sudanmap3, aes(x=long, y=lat, group=group, fill=group), fill="gray52")
fam_map

##### by gear-type and CPUE wrapped
il<-c("<0.02", "0.02-0.05", "0.05-0.1","0.1-0.5", "0.5-1", ">1")
#il<-c("<0.02", "0.02-0.05", "0.05-0.1","0.1-0.5", "0.5-1", "1-5", ">5")

#top 5 species maps
top5_map<-ggplot(topplotdata) + geom_tile(aes(x,y,fill=mean)) + scale_fill_continuous(low="khaki2",high="red4") + facet_wrap(~Sci_name)
top5_map + theme_bw() + geom_polygon(data=sudanmap3, aes(x=long, y=lat, group=group, fill=group), fill="gray52") + geom_path( data=ManageAreas.f, aes(x=long, y=lat, group=group) )

ggsave("top 5 species gridded maps.pdf", scale = 1, dpi = 400) # save plot to file

### TB faceted maps

#plotting all variables
ggplot(TBplotdata) + theme_bw() + geom_tile(aes(x,y,fill=factor(TB_catVar))) + scale_fill_brewer(palette="OrRd", labels=il)  + labs(x = "LON", y = "LAT", fill = "kg/hr fishing") + ggtitle("CPUE Traps - all surveys combined") + facet_wrap(~Fam_name)

#plotting only select variables - facetting
facets<-c("ACANTHURIDAE", "CARANGIDAE", "CARCHARHINIDAE", "EPHIPPIDAE", "HAEMULIDAE", "HOLOCENTRIDAE", "LETHRINIDAE", "LUTJANIDAE", "MURAENIDAE", "SCOMBRIDAE", "SERRANIDAE", "SPARIDAE")
TB_map<-ggplot(TBplotdata[TBplotdata$Fam_name %in% facets,]) + theme_bw() + geom_tile(aes(x,y,fill=factor(TB_catVar))) + scale_fill_brewer(palette="OrRd", labels=il)  + labs(x = "LON", y = "LAT", fill = "kg/hr fishing") + ggtitle("CPUE Traps - all surveys combined") + facet_wrap(~Fam_name)

#combining grid maps w coastline
TB2<-TBplotdata[1:2]
TB_map + geom_tile(data=TB2,aes(x=x,y=y),colour="black",fill="white",alpha=0,lwd=0.1) + geom_polygon(data=sudanmap3, aes(x=long, y=lat, group=group, fill=group), fill="gray90")
ggsave("TB_CPUE_all_facets.pdf", scale = 1, width=7.78, height=10, dpi = 400)

####
#plotting only select variables - facetting - by SURVEY
facets<-c("LETHRINIDAE", "LUTJANIDAE", "SERRANIDAE")
TB_map<-ggplot(TBplotdata[TBplotdata$Fam_name %in% facets,]) + theme_bw() + geom_tile(aes(x,y,fill=factor(TB_catVar))) + scale_fill_brewer(palette="OrRd", labels=il)  + labs(x = "LON", y = "LAT", fill = "kg/hr fishing") + ggtitle("CPUE Traps - by survey") + facet_wrap(~Fam_name + survey_f)
TB_map

#combining grid maps w coastline
#TB2<-TBplotdata[1:2]
#figure 7 in MS
TB_map + geom_polygon(data=sudanmap3, aes(x=long, y=lat, group=group, fill=group), fill="gray90") + geom_tile(data=plotdata1,aes(x=x,y=y),colour="black",fill="white",alpha=0,lwd=0.4)
ggsave("TB_CPUE_by_survey_facets.tiff", scale = 1, width=7.78, height=10, dpi = 400)
ggsave("TB_CPUE_by_survey_facets.png", scale = 1, width=7.78, height=10, dpi = 400)

#############

### HL faceted maps
il<-c("<0.02", "0.02-0.05", "0.05-0.1","0.1-0.5", "0.5-1", "1-5", ">5")

#plotting all variables
ggplot(HLplotdata) + theme_bw() + geom_tile(aes(x,y,fill=factor(HL_catVar))) + scale_fill_brewer(palette="OrRd", labels=il)  + labs(x = "LON", y = "LAT", fill = "kg/hr fishing") + ggtitle("CPUE Hand-line - all surveys combined") + facet_wrap(~Fam_name)

#plotting only select variables - facetting
facets<-c("CARANGIDAE", "HOLOCENTRIDAE", "LETHRINIDAE", "LUTJANIDAE", "SERRANIDAE", "SPARIDAE")
HL_map<-ggplot(HLplotdata[HLplotdata$Fam_name %in% facets,]) + theme_bw() + geom_tile(aes(x,y,fill=factor(HL_catVar))) + scale_fill_brewer(palette="OrRd", labels=il)  + labs(x = "LON", y = "LAT", fill = "kg/hr fishing") + ggtitle("CPUE Hand-Line - all surveys combined") + facet_wrap(~Fam_name)

#combining grid maps w coastline
HL2<-HLplotdata[1:2]
HL_map + geom_tile(data=HL2,aes(x=x,y=y),colour="black",fill="white",alpha=0,lwd=0.1) + geom_polygon(data=sudanmap3, aes(x=long, y=lat, group=group, fill=group), fill="gray90")
ggsave("HL_CPUE_all_facets.pdf", scale = 1, width=7.78, height=10, dpi = 400)


###### 
#plotting only select variables - facetting - by SURVEY
facets<-c("LETHRINIDAE", "LUTJANIDAE", "SERRANIDAE")
HL_map<-ggplot(HLplotdata[HLplotdata$Fam_name %in% facets,]) + theme_bw() + geom_tile(aes(x,y,fill=factor(HL_catVar))) + scale_fill_brewer(palette="OrRd", labels=il)  + labs(x = "LON", y = "LAT", fill = "kg/hr fishing") + ggtitle("CPUE Hand-Line - by survey") + facet_wrap(~Fam_name + survey_f)

#combining grid maps w coastline
#figure 8 in MS
HL_map  + geom_polygon(data=sudanmap3, aes(x=long, y=lat, group=group, fill=group), fill="gray90") + geom_tile(data=plotdata1,aes(x=x,y=y),colour="black",fill="white",alpha=0,lwd=0.4)
ggsave("HL_CPUE_by_survey_facets.tiff", scale = 1, width=7.78, height=10, dpi = 400)
ggsave("HL_CPUE_by_survey_facets.png", scale = 1, width=7.78, height=10, dpi = 400)


#############

### GN faceted maps
il<-c("<0.02", "0.02-0.05", "0.05-0.1","0.1-0.5", "0.5-1", "1-5", ">5")

#plotting all variables
ggplot(GNplotdata) + theme_bw() + geom_tile(aes(x,y,fill=factor(GN_catVar))) + scale_fill_brewer(palette="OrRd", labels=il)  + labs(x = "LON", y = "LAT", fill = "kg/hr fishing") + ggtitle("CPUE Gillnet - all surveys combined") + facet_wrap(~Fam_name)

#plotting only select variables - facetting
facets<-c("ACANTHURIDAE", "BELONIDAE", "CARANGIDAE", "CHIROCENTRIDAE", "LETHRINIDAE", "LUTJANIDAE", "SCARIDAE", "SCOMBRIDAE", "SERRANIDAE", "SPHYRAENIDAE")
GN_map<-ggplot(GNplotdata[GNplotdata$Fam_name %in% facets,]) + theme_bw() + geom_tile(aes(x,y,fill=factor(GN_catVar))) + scale_fill_brewer(palette="OrRd", labels=il)  + labs(x = "LON", y = "LAT", fill = "kg/hr fishing") + ggtitle("CPUE Gillnet - all surveys combined") + facet_wrap(~Fam_name)

#combining grid maps w coastline
GN2<-GNplotdata[1:2]
GN_map + geom_tile(data=GN2,aes(x=x,y=y),colour="black",fill="white",alpha=0,lwd=0.1)  + geom_polygon(data=sudanmap3, aes(x=long, y=lat, group=group, fill=group), fill="gray90")
ggsave("GN_CPUE_all_facets.pdf", scale = 1, width=7.78, height=10, dpi = 400)


#####
#plotting only select variables - facetting - by SURVEY
facets<-c("CARANGIDAE",  "LUTJANIDAE", "SCOMBRIDAE")
GN_map<-ggplot(GNplotdata[GNplotdata$Fam_name %in% facets,]) + theme_bw() + geom_tile(aes(x,y,fill=factor(GN_catVar))) + scale_fill_brewer(palette="OrRd", labels=il)  + labs(x = "LON", y = "LAT", fill = "kg/hr fishing") + ggtitle("CPUE Gillnet - by survey") + facet_wrap(~Fam_name + survey_f)

#combining grid maps w coastline
#figure 9 in MS
GN_map  + geom_polygon(data=sudanmap3, aes(x=long, y=lat, group=group, fill=group), fill="gray90") + geom_tile(data=plotdata1,aes(x=x,y=y),colour="black",fill="white",alpha=0,lwd=0.4)
ggsave("GN_CPUE_by_survey_facets.tiff", scale = 1, width=7.78, height=10, dpi = 400)
ggsave("GN_CPUE_by_survey_facets.png", scale = 1, width=7.78, height=10, dpi = 400)

