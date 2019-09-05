#' @title Analysis of catch data from coastal surveys of Sudan 2012-2013
#' @description Figures 1 - 9 of manuscript 
#' @author Erik Olsen
#' @note Revised Oct 2017
#' 


#' 
#' LIBRARIES AND SOURCES
#' --------------------------
#' 

library(reshape2)
library(maps)
library(mapdata)
library(ggplot2)
library(RColorBrewer)
library(sp)
library(rgdal)
library(rgeos)
# plyr mus be loaded befor dplyr for both to work
library(plyr) 
library(dplyr)
library(tidyr)
library(maptools)
library(marmap)
library(classInt)
library(colorspace)

#setwd("~/github/sudan")
setwd("~/Documents/GitHub/Sudan2019/Sudan-master")
source('multiplot function.R', encoding='UTF-8')

#' FUNCTIONS
#' ---------------------------
capwords <- function(s, strict = FALSE) {
  cap <- function(s) paste(toupper(substring(s, 1, 1)),
                           {s <- substring(s, 2); if(strict) tolower(s) else s},
                           sep = "", collapse = " " )
  sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
}

#' CATCH DATA
#' --------------------------

catch<-read.csv2("catch.csv")
catch$survey<-as.factor(catch$survey)
catch$CPUEw<-catch$weight/catch$Fhrs
catch$CPUEn<-catch$number/catch$Fhrs


#' import station
station <- read.csv2("stations.csv")

#' add depth to catch from station

catch$d1<- station$geardepthstart[match(catch$ss, station$ss)]  
catch$depth <- apply(X=cbind(catch$bdep, catch$d1), MARGIN=1, FUN=max, na.rm=TRUE)

#' Add Month. Year survey column
catch$survey_m<-as.character(catch$survey)
catch$survey_m<-replace(catch$survey_m, grep("2012901", catch$survey_m), "Survey 1: Nov. 2012")
catch$survey_m<-replace(catch$survey_m, grep("2013002", catch$survey_m), "Survey 2: May 2013")
catch$survey_m<-replace(catch$survey_m, grep("2013005", catch$survey_m), "Survey 3: Nov. 2013")

#' Species, L-W data and Traits
species.list<-read.csv2("Species_table.csv")
length.weight<-read.csv2("LW_pilot_sudan12_15.csv")
traits <- read.csv2("traits_noNa.csv")
#traits$MaxLength <-  as.numeric(levels(traits$MaxLength))[traits$MaxLength]
#traits$Trophic.Level <-  as.numeric(levels(traits$Trophic.Level))[traits$Trophic.Level]

#' SUDAN MAP DATA
#' ---------------------------
world<- map_data("worldHires", c("Sudan", "Ethiopia")) 
world$survey_m<-c("Survey 1: Nov. 2012")
world2<-world
world2$survey_m<-c("Survey 2: May 2013")
world3<-world
world3$survey_m<-c("Survey 3: Nov. 2013")
world<-rbind(world, world2, world3)

#'  SPATIAL POLYGONS FOR MANAGEMENT AREAS
#' -------------------------------------------
#setwd("~/github/sudan/sudan_management_areas/")
setwd("~/Documents/GitHub/Sudan2019/Sudan-master/sudan_management_areas")
ManageAreas<-readOGR(dsn = ".", "sudan_regions")
#setwd("~/github/sudan")
setwd("~/Documents/GitHub/Sudan2019/Sudan-master")

#' setting correct projection LAT LON
ManageAreas <- spTransform(ManageAreas, CRS("+proj=longlat +ellps=GRS80"))

#' Create  area map with numbers for each region
ManageAreas.f<-fortify(ManageAreas, region="id") #creates X - Y points of the polygons

cnames <- aggregate(cbind(long, lat) ~ id, data=ManageAreas.f, FUN=function(x)mean(range(x)))
cnames$id<-c(1:7)


#' DEFINE VARIABLES
#' ---------------------------
#' position of Port Sudan
ps<-rbind(data.frame(x=37.21709967,y=19.600512, group=1, survey_m=c("Survey 1: Nov. 2012")),data.frame(x=37.21709967,y=19.600512, group=1, survey_m=c("Survey 2: May 2013") ),data.frame(x=37.21709967,y=19.600512, group=1, survey_m=c("Survey 3: Nov. 2013") ))



#' ALLOCATING CATCH POSITIONS TO MANAGEMENT AREAS
#' ---------------------------
#' use 'over' method from SP package

#' Create spatial.points data base from catch

catch.points<- SpatialPoints(catch[8:9])
proj4string(catch.points) <- proj4string(ManageAreas)
catch.areas<-over(catch.points, ManageAreas)

#' fix area number to area name
alist<-data.frame(name=levels(catch.areas$name), id=c(7,3,2,1,4,5,6))

catch <- cbind(catch, catch.areas[2])
catch$id <- alist$id[match(catch$name, alist$name)]  


#' allocate management areas to catches outside of polygons (choose nearest polygon)
naarea <- catch[is.na(catch$id),]
naarea$id <- naarea$Area
naarea$name[1] <- c("Arakiai")
naarea$name[2:8] <- c("Suakin Archipelago")
naarea$name[9:16] <- c("Donganab")

catch<- rbind(subset(catch, id>=1), naarea) 


#' SELECTING AND MANIPULATING CATCH DATA
#' -----------------------------------
#' Keep a copy of original 'catch'
catch.unmod <- catch

#' remove Hand Line stations with too short time <1 hr, and too long time >8 hrs
hist((subset(catch, gear=="HL" & Fhrs>0))$Fhrs, breaks =20) # get an idea of the spread in fishing time
catch <- rbind(subset(catch, gear!="HL"), subset(catch, gear=="HL" & Fhrs>1 & Fhrs<8))

#' selecting only stations with catch & removes stations shorter than 1 hour - two handline stations with fishing time recorded as 5min, that we know is wrong, but we don't have the correct time. 
catch1<-subset(catch, Sci_name!="No catch" & Sci_name!="NO CATCH") 


#' reordering the species groups
fam.names<-c("ACANTHURIDAE", "CARANGIDAE", "CHIROCENTRIDAE", "LETHRINIDAE", "LUTJANIDAE", "SCOMBRIDAE",     "SERRANIDAE", "OTHER SPP" )  
levels(catch1$FamGroup)<-fam.names


#' ADDING TRAITS DATA TO CATCH TABLE
#' ---------------------------------

catch.traits <- catch1
catch.traits$MaxLength<- traits$MaxLength[match(catch1$Sci_name, traits$x1)]  
catch.traits$TrophicLevel<- traits$Trophic.Level[match(catch1$Sci_name, traits$x1)]  
catch.traits$TrophicGroup<- traits$Trophic.group[match(catch1$Sci_name, traits$x1)] 
catch.traits$WaterCol<- traits$Water.column[match(catch1$Sci_name, traits$x1)] 
catch.traits$DielActivity<- traits$Diel.Activity[match(catch1$Sci_name, traits$x1)] 
catch.traits$Habitat<- traits$Habitat[match(catch1$Sci_name, traits$x1)] 
catch.traits$Gregariousness<- traits$Gregariousness[match(catch1$Sci_name, traits$x1)] 
catch.traits$Sci_name_New<- traits$x2[match(catch1$Sci_name, traits$x1)] 

catch.traits$TrophicLevel[catch.traits$Sci_name=="Plectorhinchus gaterinus"] <- c(4)

#' collate Trophic groups and give shorter names
tn<-data.frame(TCat=levels(catch.traits$TrophicGroup), short=c("Invert.", "Herb.", "Coral.", "Carni.", "Plankt.", "Herb."))

catch.traits$TGShort <- tn$short[match(catch.traits$TrophicGroup, tn$TCat)]

#' ANALYSIS OF CATCHES BY TRAIT AND DEPTH 
#' (NEW FOR FEB.2019 REVISION)
#' -------------------------------------
#' 
#' remove all catches without traits (these are generally cathces not iD.ed to species, and a few species for which traits were not found.)

#' select only Traps, Gillnet and Handline
catch.traits <- subset(catch.traits, gear=="HL" | gear=="TB" | gear=="GN")
catch.traits$ss1 <- as.factor(catch.traits$ss)

#' add number of stations in that area to each line
c3 <- catch.traits %>% group_by(ss1) %>% summarise( first(ss1), first(depth), first(gear), first(id), first(survey), first(survey_m))

catch.traits %>% group_by(TGShort) %>% summarise( mean(depth))

colnames(c3) <- c("ss", "depth", "gear", "id", "survey", "survey_m")

n.area <- c3 %>% group_by(id) %>% summarise(n_distinct(ss))
colnames(n.area) <- c("id", "nos")

catch.traits$NArea <- n.area$nos[match(catch.traits$id, n.area$id)]


#' PLOT CPUE by trophic group by area & by survey (year) -FIG 5
#' ----------------------------------
trophic.plot <- ggplot(subset(catch.traits, TGShort!="NA" & gear=="TB"), aes(TGShort, CPUEw/NArea))

trophic.plot <- trophic.plot + geom_col(aes(fill=TGShort)) + facet_grid(vars(id), vars(survey_m)) + scale_fill_manual(name="Trophic group",values=qualitative_hcl(n = 5, h = c(0, 295), c = 80, l = 60, register = ) ) + labs(x=" ", y="CPUE (kg/hrs fishing) / number of traps pr area") + theme_bw() 


tiff(file="~/ownCloud/Research/Sudan/Distribution diversity LW paper/PlosOne/new figures/fig 5 trophic_plot2.tiff", width=3000, height=3000, res=400, pointsize=10, compression=c("none"))
trophic.plot
dev.off()


#' PLOT CPUE by depth plot - traps,  by area and trophic group - FIG 6
#' ---------------------------------
depth.plot <- ggplot(subset(catch.traits, gear=="TB" & TGShort!="NA"), aes(depth, CPUEw)) 

depth.plot <- depth.plot + geom_point(aes(colour=survey_m))  + facet_grid(vars(id),vars(TGShort)) +  scale_colour_manual(name="Survey",values=qualitative_hcl(n = 3, h = c(0, 295), c = 80, l = 60, register = ) ) + labs(x="Depth (m)", y="CPUE (kg(hrs)")  +theme_bw()  

#+ scale_colour_manual(name="Management area",values=sequential_hcl(n = 7, h = c(0, -100), c = c(80, NA, 40), l = c(53, 75), power = c(1, 1), rev = TRUE, register = ), guide=FALSE )

depth.plot

tiff(file="~/ownCloud/Research/Sudan/Distribution diversity LW paper/PlosOne/new figures/fig 6 CPUE depth_plot.tiff", width=3000, height=3000, res=400, pointsize=10, compression=c("none"))
depth.plot
dev.off()



#' PLOT number of set traps per depth - FIG 3
#' Need unique lines from 'catch' as 'station' does not contain station info for Nov.2013 survey
c2 <- catch %>% group_by(ss) %>% summarise(first(depth), first(gear), first(id), first(survey), first(survey_m))
colnames(c2) <- c("ss", "depth", "gear", "id", "survey", "survey_m")

dp <- ggplot(subset(c2, gear=="TB" & depth!=0), aes(as.factor(id), depth))

dp <- dp +  geom_violin(scale="width", aes(fill=as.factor(id)), draw_quantiles = c(0.5)) + scale_y_reverse() + labs(x="Management areas", y="Depth (m)") + scale_fill_manual(name="Management area",values=sequential_hcl(n = 7, h = c(0, -100), c = c(80, NA, 40), l = c(53, 75), power = c(1, 1), rev = TRUE, register = ), guide=FALSE ) +theme_bw()  +facet_wrap(~as.factor(survey_m))

dp
tiff(file="fig 3 depth_plot_station.tiff", width=3000, height=1500, res=400, pointsize=10, compression=c("none"))
dp
dev.off()

#´ Explore: Trap Catch rates (CPUE) by depth and area

d3 <- ggplot(subset(catch, gear=="TB"), aes(depth, CPUEw))

d3 + geom_point() + geom_smooth(method="auto")  +facet_wrap(~as.factor(survey_m))




#' BATHYMETRIC MAP OF SUDAN WITH MANAGEMENT AREAS (FIGURE 1)
#' ----------------------------------------------
sudan<-readGEBCO.bathy("./sudan_bathymetry/sudan.nc")
blues <- colorRampPalette(c("midnightblue", "deepskyblue3", "deepskyblue1", "cadetblue3", "cadetblue1", "darkseagreen1", "white"))

tiff(file="~/ownCloud/Research/Sudan/Distribution diversity LW paper/PlosOne/new figures/fig 1 sudan_bathy_v3.tiff", width=1900, height=3000, res=400, pointsize=10, compression=c("none"))

plot(sudan, land = TRUE, n = 10, image = TRUE,
     bpal = list(c(min(sudan), -20, "midnightblue", "blue", "lightblue3"),
                 c(-20, 0, "lightblue1", "aquamarine1"),
                 c(0, max(sudan), "gray90", "gray50")), 
     deep = c(-3000, -500, 0),
     shallow = c(-500, -10, 0),
     step = c(500, 200, 0),
     lwd = c(0, 0, 1), lty = c(1, 1, 1),
     col = c("lightgrey", "gray15", "black"),
     drawlabel = c(TRUE, TRUE, TRUE))


scaleBathy(sudan, deg=0.47 ,x="bottomleft", inset=5)

points(37.21709967, 19.600512, pch=19)
points(37.33333, 19.1, pch=19)

text(37.21709967, 19.600512,"Port \nSudan", adj=c(1.2,0), font=2)
text(37.333333, 19.1,"Suakin", adj=c(1.2,0), font=1)
text(37.4, 21.7, "1. Marsas \nnorth of \nDungonab", cex=0.9, font=2, col="white")
text(37.653, 20.92, "2. Dungonab", cex=0.9, font=2, col="white")
text(37.6, 20.3, "3. Arakiai", cex=0.9, font=2, col="white")
text(37.615, 19.95, "4. Port \nSudan", cex=0.9, font=2, col="white")
text(37.6, 18.9, "5. Suakin", cex=0.9, font=2)
text(38, 19.2, "6. Suakin \narchipelago", cex=0.9, font=2)
text(38.3, 18.5, "7. Agig", cex=0.9, font=2)

MApoints<-data.frame(lon=c(37.2, 37.4, 37.38, 37.4, 37.6, 37.9, 38.3), lat=c(21.7, 20.92, 20.3, 19.9, 19, 19.3, 18.5), area=c(1,2,3,4,5,6,7))

ms<-c(1,3,4,5,6,7,8)
for (i in 1:7){
  ma<-subset(ManageAreas.f, id==ms[i])
  polygon(ma$long, ma$lat, lwd=2)  
}
dev.off()





#' PLOTTING CATCH POSITIONS BY SURVEY (FIGURE 2)
#' ----------------------------------

MApoints<-data.frame(lon=c(37.2, 37.53, 37.5, 37.4, 37.3, 37.9, 38.2), lat=c(21.7, 20.92, 20.3, 19.9, 19, 19.3, 18.5), area=c(1,2,3,4,5,6,7))

sudan.map<-ggplot(catch, aes(x=lon, y=lat, group=survey_m)) + geom_point(shape="+", size=6, aes(group=survey_m, colour= survey_m)) + facet_wrap(~survey_m, ncol=3) + geom_path(data=ManageAreas.f,colour="dodgerblue3", aes(x=long, y=lat, group=id)) + geom_polygon(data=world, colour="gray35", fill="gray85", aes(x=long, y=lat, group=survey_m)) +  coord_cartesian(xlim = c(36.5, 39), ylim=c(17.7, 22.5)) + theme_classic() + geom_point(data=ps, size=2, colour="gray35", aes(x=x, y=y)) + geom_text(data=ps, label="PZU", size=3,  hjust=1, vjust=-1.2,  aes(x=x, y=y, group=survey_m)) +geom_text(data=MApoints, aes(x=lon, y=lat, group=area, label=area)) +scale_colour_brewer(type = "seq", palette = "Dark2", name="Survey month & year", labels=c("Nov. 2012", "May. 2013", "Nov. 2013")) + theme(legend.position="none")
sudan.map

sudan.map

ggsave("Fig 2 sudan_stations_12_13_facet.tiff")


####TESTING#####
#' check that all area-ids are correct
#' sudan.map<-ggplot(catch, aes(x=lon, y=lat, group=survey_m)) +geom_text(aes(label=id)) + facet_wrap(~survey_m, ncol=3) + geom_polygon(data=world, colour="gray35", fill="gray85", aes(x=long, y=lat, group=survey_m)) +  coord_cartesian(xlim = c(36.5, 39), ylim=c(17.7, 22.5)) + theme_classic() + geom_point(data=ps, size=5, colour="gray35", aes(x=x, y=y)) + geom_text(data=ps, label="PZU", size=5,  hjust=1, vjust=-1.2,  aes(x=x, y=y, group=survey_m)) +scale_colour_brewer(type = "seq", palette = "Dark2", name="Survey month & year", labels=c("Nov. 2012", "May. 2013", "Nov. 2013")) + theme(legend.position="none")
#' sudan.map



#' ANOVA OF CATCHES BY GEAR AND MANAGEMENT AREAS (CPUE by Weight) 
#' only station with catch - Why? Shouldn't we be comparing all data?
#' ---------------------------
catch.df<-subset(catch1, number>0 & weight>0)

#' test for normality
shapiro.test(catch.df$CPUEw)
shapiro.test(catch.df$CPUEn)

#' Q-Q plots
library("qqplotr")
gg <- ggplot(data = catch.df, mapping = aes(sample = CPUEw,  fill = factor(id))) + stat_qq_band() + stat_qq_line() + stat_qq_point(size=0.2) + facet_wrap(~ id, scales="free") +  labs(x = "CPUEw predicted", y = "CPUEw Observed") + ggtitle("Q-Q plots CPUEw")
gg

#' Conclusion: Data is non-normal, and almost 'zero-inflated'
#' Use non-parametric Kruskal - Wallis test
#' 1 - test for difference between areas
kruskal.test(catch.df$CPUEw, catch.df$id)
kruskal.test(catch.df$CPUEn, catch.df$id)
#' both CPUEw and CPUEn are significantly different between the areas. 
#' Post-Hoc tests of Kruskal - Wallis results
library("PMCMR", lib.loc="/Library/Frameworks/R.framework/Versions/3.5/Resources/library")
posthoc.kruskal.nemenyi.test(catch.df$CPUEw, catch.df$id, "Chisq")
posthoc.kruskal.nemenyi.test(catch.df$CPUEn, catch.df$id, "Chisq")
#' significant difference in CPUEn between area 6 and area 4

#' 2 test for differences between surveys
kruskal.test(catch.df$CPUEw, catch.df$survey)
kruskal.test(catch.df$CPUEn, catch.df$survey)

posthoc.kruskal.nemenyi.test(catch.df$CPUEw, catch.df$survey, "Chisq")
posthoc.kruskal.nemenyi.test(catch.df$CPUEn, catch.df$survey, "Chisq")
#' significant difference for CPUEW between May 2013 survey and the Nov 2012 and Nov 2013 surveys, but not between the Nov12 and Nov13 surveys


#´ ZERO-INFLATED MODEL
-----------------------
#' Fitting GAM Zero Inflated model to catch data
library("gamlss", lib.loc="/Library/Frameworks/R.framework/Versions/3.5/Resources/library")

# select only variables used in model
c2 <- catch[,c(1,14,22,25,28)]
c2 <- subset(c2, gear=="TB" | gear=="GN" |gear=="HL")


# inclued area (id) and survey as factors
mod<-gamlss(CPUEw~depth+factor(id)+factor(survey),family=ZAGA, data=subset(c2, gear=="TB")) 
summary(mod)

# exploratory - including gear. 
mod<-gamlss(CPUEw~depth+factor(id)+factor(survey)+factor(gear),family=ZAGA, data=c2) 
summary(mod)



#' Plot trap catch rates as Violin plots per area - FIG 7
#' 
ca.plot<-ggplot(subset(catch, gear=="TB" & depth!=0), aes(as.factor(id), CPUEw))

ca.plot <- ca.plot + geom_violin(scale="width", aes(fill=as.factor(id)), draw_quantiles = c(0.5))  + labs(x="Management areas", y="CPUE kg/hrs fishing") + scale_fill_manual(name="Management area",values=sequential_hcl(n = 7, h = c(0, -100), c = c(80, NA, 40), l = c(53, 75), power = c(1, 1), rev = TRUE, register = ), guide=FALSE ) +theme_bw() +facet_wrap(~as.factor(survey_m))

tiff(file="fig 7 catch rates traps pr area survey.tiff", width=3000, height=2000, res=400, pointsize=10, compression=c("none"))
ca.plot
dev.off()




#' CPUE (weight) PLOT BY GEAR TYPE (FIGURE 3)
#' ------------------------------
catch.3gear<-subset(catch.unmod, gear=="GN" | gear=="HL" | gear =="TB")

gear.plot <- ggplot(catch.3gear, aes(capwords(tolower(as.character(catch.3gear$FamGroup))), fill=survey_m)) + geom_bar(binwidt=0.2, aes( weight=weight)) +theme_bw() + facet_grid(vars(gear), vars(survey_m)) + coord_flip() +xlab("") + ylab("kg")  + guides(fill=guide_legend(title="Survey")) + theme(legend.position = "none")
gear.plot
ggsave("Fig 4 gearplot.tiff")









#' Plot by family groups  - linked to functional groups. Similar to facet trait group figure. 


#' --- REMOVE --- Not interesting as it just a function of the number of traps / hrs fishing per area (table 1) 
#' CPUE (weight) PLOT BY MANAGEMENT AREAS (FIGURE 4)
#' ---------------------------------
#' lat.plot<-ggplot(catch1, aes(id, fill=FamGroup)) + geom_bar(binwidt=0.2, aes( weight=CPUEw)) +theme_bw() + scale_x_reverse(breaks=c(1,2,3,4,5,6,7)) 
#' lat.plot + scale_fill_brewer(palette="RdYlBu", name="Family groups") + coord_flip() + theme(legend.title = element_text(size=14, face="bold")) + theme(legend.text = element_text(size=12)) + ylab("CPUE weight (kg) of fish/hr fishing") + xlab("Management areas from N (1) to S (7)") 
#+ theme(axis.text.x = element_blank()) 
#lat.plot
#figure 
#' ggsave("Fig 4 management area CPUE weight.tiff")



#' GRIDDING CATCH DATA (CPUE by weight) to 0.1 x 0.1 DEGREE GRID OF SUDAN
#' FIGURES 5, 6, 7, and 8
#' ---------------------------------

# set grid X and Y limits
xvals <- seq(36.5, 38.8, by=0.1)
yvals <- seq(18.3, 22.1, by=0.1)

#kategorizing the geom-data
catch$x<-round(catch$lon, 1)
catch$y<-round(catch$lat, 1)

# TRAPS, GILLNET and HAND-LINE CPUE data set
#remove NO CATCH data
TBcpue<-subset(catch, gear=="TB" & Fam_name!="NO CATCH")
GNcpue<-subset(catch, gear=="GN"& Fam_name!="NO CATCH")
HLcpue<-subset(catch, gear=="HL"& Fam_name!="NO CATCH")

#select top5 fish 
fish5<-c("Lutjanus bohar", "Lutjanus gibbus", "Lethrinus lentjan", "Lethrinus mahsena", "Sargocentron spiniferum")
topfish<-subset(catch, Sci_name==fish5, & Fam_name!="NO CATCH")

#calculate Mean and Total
#Must first detach plyr to use the 'dplyr' summarise function
detach("package:plyr", unload=TRUE) 

# total catch
catch_pr_cell<-as.data.frame(group_by(catch,x,y) %>% summarise(mean=mean(CPUEw),total=sum(CPUEw), first(survey_m)))
colnames(catch_pr_cell)[5] <- c("survey_m")

#catch of top 5 fish
top_catch_pr_cell<-as.data.frame(group_by(topfish,Sci_name, survey,x,y) %>%  summarise(mean=mean(CPUEw),total=sum(CPUEw), first(survey_m)))
colnames(top_catch_pr_cell)[7] <- c("survey_m")

#catch by gear type
TBspecies_catch_pr_cell<-as.data.frame(group_by(TBcpue,survey,Fam_name,x,y) %>% summarise(mean=mean(CPUEw), total=sum(CPUEw), first(survey_m)))
colnames(TBspecies_catch_pr_cell)[7] <- c("survey_m")

HLspecies_catch_pr_cell<-as.data.frame(group_by(HLcpue,survey,Fam_name,x,y) %>%  summarise(mean=mean(CPUEw), total=sum(CPUEw), first(survey_m)))
colnames(HLspecies_catch_pr_cell)[7] <- c("survey_m")

GNspecies_catch_pr_cell<-as.data.frame(group_by(GNcpue,survey,Fam_name,x,y) %>%  summarise(mean=mean(CPUEw), total=sum(CPUEw), first(survey_m)))
colnames(GNspecies_catch_pr_cell)[7] <- c("survey_m")

#weight categories
#ALL
ALL_brks<-classIntervals(catch_pr_cell$mean, n=7, style="fixed", fixedBreaks=c(0, 0.02, 0.05, 0.1, 0.5, 1, 5, 18.2)) #define categories
ALL_brks<-round(ALL_brks$brks,digits=3) #round
ALL_catVar<-findInterval(catch_pr_cell$mean, ALL_brks, all.inside=TRUE) #assign categories
catch_pr_cell$All_catVar<-ALL_catVar

#TOP 5 species
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
#plotdata_s<-merge(species_catch_pr_cell,expand.grid(x=xvals,y=yvals),all.y=T)
#plotdata_s<-subset(plotdata_s, mean>=0)
#plotdata_s<-subset(plotdata_s, Fam_name!="NO CATCH")

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

#define intervals for legend in plot
il<-c("<0.02", "0.02-0.05", "0.05-0.1","0.1-0.5", "0.5-1", ">1")

#Selecting Sudan from world map
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

# legends for management areas
mlon<-c(37, 37.4, 37.2, 37.3, 37.5, 37.9, 38.3)
mlat<-c(21.7, 20.92, 20.3, 19.9, 19, 19.3, 18.5)
mname<-c(1, 2, 3, 4, 5, 6, 7)
group<-c(1,1,1,1,1,1,1)
areanum<-as.data.frame(cbind(mlon, mlat, mname, group))
colnames(areanum)<-c("lon", "lat", "mname", "group")

#' MAP TOTAL CATCHES PR CELL (FIGURE 8)
RedSeaMap2<-ggplot(plotdata1) + geom_tile(aes(x,y,fill=factor(ALL_catVar))) + scale_fill_brewer(palette="OrRd", labels=il) + labs(x = "LON", y = "LAT", fill = "kg/hr fishing") + theme_bw() + geom_polygon(data=sudanmap3, aes(x=long, y=lat, group=group, fill=group), fill="gray90") + xlim(36.5, 39) +ylim(18, 22)  + geom_path( data=ManageAreas.f, aes(x=long, y=lat, group=group) ) + geom_text(data=areanum,  aes(x=lon, y=lat, label=mname)) 
RedSeaMap2

ggsave("Fig 8 total biomass all gear areas.tiff", width=20, height=24, units=c("cm"), dpi = 400) # save plot to file

#' MAP TOTAL CATCHES PR CELL - TRAPS ONLY
RedSeaMap3<-ggplot(TBplotdata) + geom_tile(aes(x,y,fill=factor(TB_catVar))) + scale_fill_brewer(palette="OrRd", labels=il) + labs(x = "LON", y = "LAT", fill = "kg/hr fishing") + theme_bw() + geom_polygon(data=sudanmap3, aes(x=long, y=lat, group=group, fill=group), fill="gray90") + xlim(36.5, 39) +ylim(18, 22)  + geom_path( data=ManageAreas.f, aes(x=long, y=lat, group=group) ) + geom_text(data=areanum,  aes(x=lon, y=lat, label=mname)) 
RedSeaMap3

ggsave("~/ownCloud/Research/Sudan/Distribution diversity LW paper/PlosOne/new figures/TB_CPUE_ALL_AREAS.tiff", width=20, height=24, units=c("cm"), dpi = 400) # save plot to file


#' MAP TOTAL CATCHES PR CELL - GILLNETS ONLY
RedSeaMap4<-ggplot(GNplotdata) + geom_tile(aes(x,y,fill=factor(GN_catVar))) + scale_fill_brewer(palette="OrRd", labels=il) + labs(x = "LON", y = "LAT", fill = "kg/hr fishing") + theme_bw() + geom_polygon(data=sudanmap3, aes(x=long, y=lat, group=group, fill=group), fill="gray90") + xlim(36.5, 39) +ylim(18, 22)  + geom_path( data=ManageAreas.f, aes(x=long, y=lat, group=group) ) + geom_text(data=areanum,  aes(x=lon, y=lat, label=mname)) 
RedSeaMap4

ggsave("~/ownCloud/Research/Sudan/Distribution diversity LW paper/PlosOne/new figures/GN_CPUE_ALL_AREAS.tiff", width=20, height=24, units=c("cm"), dpi = 400) # save plot to file


#' FACETED MAPS, BY GEAR AND 3 MOST PROMINENT FAMILIES
facets<-c("LETHRINIDAE", "LUTJANIDAE", "SERRANIDAE")

#' Add Month. Year survey column as factor
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



#' TRAPS FACETED MAP (FIGURE S1 - 1)
TB_map<-ggplot(TBplotdata[TBplotdata$Fam_name %in% facets,]) + theme_bw() + geom_tile(aes(x,y,fill=factor(TB_catVar))) + scale_fill_brewer(palette="OrRd", labels=il)  + labs(x = "LON", y = "LAT", fill = "kg/hr fishing") + ggtitle("CPUE Traps - by survey") + facet_wrap(~Fam_name + survey_f)  + geom_polygon(data=sudanmap3, aes(x=long, y=lat, group=group, fill=group), fill="gray90") + geom_tile(data=plotdata1,aes(x=x,y=y),colour="black",fill="white",alpha=0,lwd=0.4)
TB_map
ggsave("Fig S1 TB_CPUE_by_survey_facets.tiff", scale = 1, width=7.78, height=10, dpi = 400)


#' DON'T PLOT - TOO FEW DATA POINTS DUE TO MISSING TIME DATA
#' HANDLINES FACETED MAP (FIGURE 7)
# HL_map<-ggplot(HLplotdata[HLplotdata$Fam_name %in% facets,]) + theme_bw() + geom_tile(aes(x,y,fill=factor(HL_catVar))) + scale_fill_brewer(palette="OrRd", labels=il)  + labs(x = "LON", y = "LAT", fill = "kg/hr fishing") + ggtitle("CPUE Handlines - by survey") + facet_wrap(~Fam_name + survey_f)  + geom_polygon(data=sudanmap3, aes(x=long, y=lat, group=group, fill=group), fill="gray90") + geom_tile(data=plotdata1,aes(x=x,y=y),colour="black",fill="white",alpha=0,lwd=0.4)
# HL_map
# ggsave("Fig 7 HL_CPUE_by_survey_facets.tiff", scale = 1, width=7.78, height=10, dpi = 400)


#' GILLNET FACETED MAP (FIGURE S1 - 2)
 GN_map<-ggplot(GNplotdata[GNplotdata$Fam_name %in% facets,]) + theme_bw() + geom_tile(aes(x,y,fill=factor(GN_catVar))) + scale_fill_brewer(palette="OrRd", labels=il)  + labs(x = "LON", y = "LAT", fill = "kg/hr fishing") + ggtitle("CPUE Gillnet - by survey") + facet_wrap(~Fam_name + survey_f)  + geom_polygon(data=sudanmap3, aes(x=long, y=lat, group=group, fill=group), fill="gray90") + geom_tile(data=plotdata1,aes(x=x,y=y),colour="black",fill="white",alpha=0,lwd=0.4)
GN_map
ggsave("Fig S2 GN_CPUE_by_survey_facets.tiff", scale = 1, width=7.78, height=10, dpi = 400)




#' LENGTH WEIGHT ANALYSIS - NOT INCLUDED
#' ---------------------------------

study.species<-c("LUTLU06", "LETLE02","LUTLU04","CARSC01","CARSC04","ACAAC28")


lw.select<-data.frame()

for (i in 1:length(study.species)){
  lw.select<-rbind(lw.select, subset(length.weight, species==study.species[i]))
}

#' add latin names
for (i in 1:nrow(lw.select)){
  lw.select[i,16]<-species.list[grep(lw.select[i,3], species.list[,3]),4]
}

cn<-colnames(lw.select)
cn[16]<-c("LatinName")
colnames(lw.select)<-cn

#' add FishBase LW growth columns
for (i in 1:nrow(lw.select)){
  lw.select[i,17]<-species.list[grep(lw.select[i,3], species.list[,3]),5]
  lw.select[i,18]<-species.list[grep(lw.select[i,3], species.list[,3]),6]
  lw.select[i,19]<-species.list[grep(lw.select[i,3], species.list[,3]),7]
  lw.select[i,20]<-species.list[grep(lw.select[i,3], species.list[,3]),8]
  lw.select[i,21]<-species.list[grep(lw.select[i,3], species.list[,3]),9]
  lw.select[i,22]<-species.list[grep(lw.select[i,3], species.list[,3]),10]
}

cn<-colnames(lw.select)
cn[17]<-c("a_FB")
cn[18]<-c("b_FB")
cn[19]<-c("a_N")
cn[20]<-c("b_N")
cn[21]<-c("a_S")
cn[22]<-c("b_S")
colnames(lw.select)<-cn

#' CALCULATE ERRORS BETWEEN OBSERVED WEiGHT AND PREDICTED WEIGHT
#' 

species.lw.table<-data.frame(Species=character(1), N=numeric(1), L_min=numeric(1), L_max=numeric(1), a=numeric(1),a_2_5=numeric(1), a_97=numeric(1), b=numeric(1), b_2_5=numeric(1), b_97=numeric(1), error=numeric(1),  t_test=numeric(1))

for (i in 1:length(study.species)){ 
  weight.table<-cbind(subset(lw.select, species==study.species[i])[,5], subset(lw.select, species==study.species[i])[,6], (species.list[grep(study.species[i], species.list[,3]),5] * subset(lw.select, species==study.species[i])[,5]^species.list[grep(study.species[i], species.list[,3]),6]), (species.list[grep(study.species[i], species.list[,3]),7] * subset(lw.select, species==study.species[i])[,5]^species.list[grep(study.species[i], species.list[,3]),8]), (species.list[grep(study.species[i], species.list[,3]),9] * subset(lw.select, species==study.species[i])[,5]^species.list[grep(study.species[i], species.list[,3]),10]))
  
  weight.table<-as.data.frame(weight.table)  
  
  weight.table[,6]<-(weight.table[,2]-weight.table[,3])/weight.table[,2]  
  colnames(weight.table)<-c("Length", "Obs", "FB", "N", "S", "Err")
  t.fb<-t.test(weight.table[,2], weight.table[,3], paired=TRUE) #test vs FishBase data
  
  test.lw<-subset(lw.select, species==study.species[i])
  nls.lw<-nls(weight~a*length^b, data=test.lw, start = list(a=0.003, b=3), control= list(warnOnly=TRUE))
  #coef(nls.lw)
  
  #' enter species data into species-LW-table
  sp.line<-as.data.frame(list(test.lw[1,3], nrow(test.lw), min(test.lw$length), max(test.lw$length), coef(nls.lw)[1], confint(nls.lw)[1,1], confint(nls.lw)[1,2], coef(nls.lw)[2], confint(nls.lw)[2,1], confint(nls.lw)[2,2], mean(weight.table[,6]), t.fb$p.value))
  colnames(sp.line)<-colnames(species.lw.table)
  species.lw.table<-rbind(species.lw.table, sp.line[1,])
}

species.lw.table<-species.lw.table[2:26,]

#' add latin names
for (i in 1:nrow(species.lw.table)){
  species.lw.table[i,13]<-species.list[grep(species.lw.table[i,1], species.list[,3]),4]
}

cn<-colnames(species.lw.table)
cn[13]<-c("LatinName")
colnames(species.lw.table)<-cn

#' export table
write.csv2(species.lw.table, file= "species_LW_table.csv")

#' L-W PLOTS
lw.colours<- c("gray10", brewer.pal(length(study.species), "Set1"))
for (i in 1:length(study.species)){ 
  
  ss<-subset(lw.select, species==study.species[i])
  lw.plot.1<-ggplot(ss, aes(length, weight)) + geom_point() + ggtitle(species.list[grep(study.species[i], species.list[,3]),4]) + theme_classic()
  #' +stat_smooth(colour=lw.colours[1])
  #lw.plot.1
  
  #' adding L-W curve calculated from data
  l.fb<-c(min(subset(lw.select, species==study.species[i])[5]):max(subset(lw.select, species==study.species[i])[5]))
  w.fb<-species.lw.table[grep(study.species[i], species.lw.table[,1]),5] * l.fb^species.lw.table[grep(study.species[i], species.lw.table[,1]),8] 
  w.lo<-species.lw.table[grep(study.species[i], species.lw.table[,1]),6] * l.fb^species.lw.table[grep(study.species[i], species.lw.table[,1]),9]
  w.up<-species.lw.table[grep(study.species[i], species.lw.table[,1]),7] * l.fb^species.lw.table[grep(study.species[i], species.lw.table[,1]),10]
  
  lo.points<-as.data.frame(cbind(l.fb, w.fb, w.lo, w.up))
  
  lw.plot.1 <- lw.plot.1 + geom_line(data=lo.points,  colour=lw.colours[1], aes(l.fb, w.fb))
  
  #' adding local growth curves
  #' Northern Area growth curve (BLUE line)
  l.ln<-c(min(subset(lw.select, species==study.species[i])[5]):max(subset(lw.select, species==study.species[i])[5]))
  w.ln<-species.list[grep(study.species[i], species.list[,3]),7] * l.fb^species.list[grep(study.species[i], species.list[,3]),8] 
  ln.points<-as.data.frame(cbind(l.ln, w.ln))
  
  lw.plot.1 <- lw.plot.1 + geom_line(data=ln.points, colour=lw.colours[3], aes(l.ln, w.ln) )
  
  #' Southern Area growth curve (GREEN line)
  
  l.ls<-c(min(subset(lw.select, species==study.species[i])[5]):max(subset(lw.select, species==study.species[i])[5]))
  w.ls<-species.list[grep(study.species[i], species.list[,3]),9] * l.fb^species.list[grep(study.species[i], species.list[,3]),10] 
  ln.points<-as.data.frame(cbind(l.ls, w.ls))
  
  lw.plot.1<-lw.plot.1 + geom_line(data=ln.points, colour=lw.colours[4], aes(l.ls, w.ls))
  ggsave(paste(study.species[i],".png", sep=""))
  assign(paste(study.species[i],".plot", sep=""), lw.plot.1)
}

#' Combine 6 species plot into one multiplot
lw.layout<-matrix(1:6, nrow=3, byrow=TRUE)
lw.plotlist<-list(LUTLU06.plot, LETLE02.plot, LUTLU04.plot, CARSC01.plot, CARSC04.plot, ACAAC28.plot)

tiff(file="fig 9 LWplot.tiff", width=1900, height=1900, res=400, pointsize=8, compression=c("none"))

multiplot(LUTLU06.plot, LETLE02.plot, LUTLU04.plot, CARSC01.plot, CARSC04.plot, ACAAC28.plot, layout=lw.layout)
dev.off()








###################################
###################################
#' ADDITIONAL ANALYSES
#' --------------------------------
#' SPECIES NUMBERS PR AREA
#' ------------------------------
#' not presented as figures in the MS submitted to Frontiers in Nov'17
ss.list<-unique(subset(catch1, gear=="GN" | gear=="TB" | gear=="HL")$ss)

spue.table<-data.frame(0,0,0,0,0)
colnames(spue.table)<-c("ss","area", "spue", "nspec", "survey")

for (i in 1:length(ss.list)) { 
  spue.table[i,1] <- subset(catch1, catch1$ss == ss.list[i])$ss[1]
  spue.table[i,2] <- subset(catch1, catch1$ss == ss.list[i])$id[1]
  spue.table[i,3] <- nrow(subset(catch1, catch1$ss == ss.list[i]))/subset(catch1, catch1$ss == ss.list[i])$Fhrs[1] 
  spue.table[i,4] <- nrow(subset(catch1, catch1$ss == ss.list[i])) 
  spue.table[i,5] <- as.integer(substr(ss.list[i], 1,7))
}

spue.agg<-aggregate(spue.table, list(Area = spue.table$area), mean)

sp_n_area <- catch %>% group_by(name) %>% summarise(Tot_N_species=n_distinct(Sci_name))

#ANOVA Species Numbers (not corrected for effort)
spue.df<-spue.table
spue.df$area<-factor(spue.df$area, labels = c(1:7))
#' test for normality
shapiro.test(spue.df$nspec)
#' not normal distributed

#' kruskal test (non-parametric ANOVA)
kruskal.test(spue.df$spue, spue.df$area)
posthoc.kruskal.nemenyi.test(spue.df$spue, spue.df$area, "Chisq")
#' significant differences between areas 2:3

kruskal.test(spue.df$nspec, spue.df$area)
posthoc.kruskal.nemenyi.test(spue.df$nspec, spue.df$area, "Chisq")
#' no significant differences 

#' summary statistics
by(spue.df, spue.df$area, summary)

#' SPECIES DENSITIES -TRAPS
#' species pr area pr hour of fishing - TRAPS only
#' -----------------------
sp.pr.area <- subset(catch, gear=="TB") %>% group_by(id) %>% summarise(n_distinct(species), n_distinct(ss), mean(Fhrs))
colnames(sp.pr.area) <- c("id", "Nsp","NTraps", "Fhrs")
summary(sp.pr.area)

sp.pr.area$Species_by_NoStations <- sp.pr.area$Nsp / sp.pr.area$NTraps
sp.pr.area$Species_Hrs <- sp.pr.area$Nsp / sp.pr.area$Fhrs
# Species per hours! - Area 2 has highest value, followed by area 3 and area 6!!! Lowest number in area 4 (Port Sudan)
write.csv2(sp.pr.area, "~/ownCloud/Research/Sudan/Distribution diversity LW paper/ICES ASC 2019/species_density_area.csv")

#' SPECIES NUMBERS PLOTS
species.plot<-ggplot(sp.pr.area, aes(x=id, y=Species_Hrs)) + geom_bar(stat="identity", fill="steelblue") +theme_bw() + scale_x_reverse(breaks=c(1,2,3,4,5,6,7)) 
species.plot  + coord_flip() + theme(legend.title = element_text(size=14, face="bold")) + theme(legend.text = element_text(size=12)) + ylab("Number of species caught in traps pr hour fishing") + xlab("Management areas from N (1) to S (7)") 
ggsave("~/ownCloud/Research/Sudan/Distribution diversity LW paper/ICES ASC 2019/No species pr area.png")






#' ADDITIONAL PLOTS FOR ICES ASC POSTER 2019

#' Trophic level, by survey, area, trophic group w. mean TL
#' ----------------------------------
#' 
catch.traits$TrophicLevel[catch.traits$Sci_name=="Plectorhinchus gaterinus"] <- c(4)

#' group means for plotting

TMean <- catch.traits %>% group_by(id, survey_m) %>% summarise(TM=mean(na.omit(TrophicLevel)))
CMean <- catch.traits %>% group_by(id, survey_m) %>% summarise(CM=mean(CPUEw))
TMean$CM <- CMean$CM

t0.plot <- ggplot(subset(catch.traits, gear=="TB" & TrophicLevel!="NA"), aes(TrophicLevel, CPUEw)) 

t0.plot <- t0.plot + geom_point(aes(colour=TGShort)) + geom_vline(data = TMean, aes(xintercept = TM)) + facet_grid(vars(id),vars(survey_m)) +  scale_colour_manual(name="Trophic group",values=qualitative_hcl(n = 5, h = c(0, 295), c = 80, l = 60, register = ) ) + labs(x="Trophic level", y="CPUE (kg(hrs)")  +theme_bw()  
t0.plot

#+ scale_colour_manual(name="Management area",values=sequential_hcl(n = 7, h = c(0, -100), c = c(80, NA, 40), l = c(53, 75), power = c(1, 1), rev = TRUE, register = ), guide=FALSE )


tiff(file="~/ownCloud/Research/Sudan/Distribution diversity LW paper/ICES ASC 2019/trophic_level.tiff", width=3000, height=3000, res=400, pointsize=10, compression=c("none"))
t0.plot
dev.off()


#' Place in water column, trap catches
catch.traits$WaterCol[catch.traits$WaterCol=="Demersal"] <- "demersal"
tl.plot <- ggplot(subset(catch.traits, WaterCol!="NA" & gear=="TB"), aes(WaterCol, CPUEw/NArea))

tl.plot <- tl.plot + geom_col(aes(fill=WaterCol)) + facet_grid(vars(id), vars(survey_m)) + scale_fill_manual(name="Place in water column",values=qualitative_hcl(n = 6, h = c(0, 295), c = 80, l = 60, register = ) ) + labs(x=" ", y="CPUE (kg/hrs fishing) / number of traps pr area") + theme_bw() + 
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank())
tl.plot


tiff(file="~/ownCloud/Research/Sudan/Distribution diversity LW paper/ICES ASC 2019/water_col.tiff", width=3000, height=3000, res=400, pointsize=10, compression=c("none"))
tl.plot
dev.off()


#' Diel activity, trap catches

t2.plot <- ggplot(subset(catch.traits, DielActivity!="#N/A" & gear=="TB"), aes(DielActivity, CPUEw/NArea))

t2.plot <- t2.plot + geom_col(aes(fill=DielActivity)) + facet_grid(vars(id), vars(survey_m)) + scale_fill_manual(name="Diel activity",values=qualitative_hcl(n = 6, h = c(0, 295), c = 80, l = 60, register = ) ) + labs(x=" ", y="CPUE (kg/hrs fishing) / number of traps pr area") + theme_bw() + 
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank())
t2.plot


tiff(file="~/ownCloud/Research/Sudan/Distribution diversity LW paper/ICES ASC 2019/diel_act.tiff", width=3000, height=3000, res=400, pointsize=10, compression=c("none"))
t2.plot
dev.off()


#' Gregariousness, trap catches

catch.traits$Gregariousness[catch.traits$Sci_name=="Lethrinus mahsena"] <- c(2)
catch.traits$Gregariousness[catch.traits$Sci_name=="Sphyraena jello"] <- c(3)
catch.traits$Gregariousness[catch.traits$Sci_name=="Thunnus albacares"] <- c(3)
catch.traits$Gregariousness[catch.traits$Gregariousness=="#N/A"] <- NA

catch.traits$Gregariousness <- droplevels(catch.traits$Gregariousness)

t3.plot <- ggplot(subset(catch.traits, Gregariousness!="#N/A" & gear=="TB"), aes(Gregariousness, CPUEw/NArea))

t3.plot <- t3.plot + geom_col(aes(fill=Gregariousness)) + facet_grid(vars(id), vars(survey_m)) + scale_fill_manual(name="Gregariousness",values=qualitative_hcl(n = 6, h = c(0, 295), c = 80, l = 60, register = ) ) + labs(x=" ", y="CPUE (kg/hrs fishing) / number of traps pr area") + theme_bw() + 
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank())
t3.plot


tiff(file="~/ownCloud/Research/Sudan/Distribution diversity LW paper/ICES ASC 2019/gregariousness.tiff", width=3000, height=3000, res=400, pointsize=10, compression=c("none"))
t3.plot
dev.off()


#' TABLE OF RESULTS (MEAN CPUE BY GEAR PR AREA, MEAN TROPHIC LEVEL, GREGARIOUSNESS, CARNIVORE CPUE, INVERTIVORE CPUE) - ALL BY AREA, AVERAGE ACROSS THE THREE SURVEYS
#' 
#' 
#' Need to recalculate CPUE by station, to get correct estimate of mean(CPUE)
st2 <- catch %>% group_by(ss) %>% summarise(first(id), first(Fhrs), first(gear), sum(weight))
colnames(st2) <- c("ss", "id", "Fhrs","gear", "weight")
st2$CPUEw <- st2$weight/st2$Fhrs

#' table w results
results.table <- data.frame(area=(1:7))

results.table <- cbind(results.table,(subset(st2, gear=="TB") %>% group_by(id) %>% summarise( mean(CPUEw)))[,2])
results.table <- cbind(results.table,(subset(st2, gear=="GN") %>% group_by(id) %>% summarise( mean(CPUEw)))[,2])
results.table <- cbind(results.table,(subset(catch.traits, gear=="TB") %>% group_by(id) %>% summarise( mean(TrophicLevel, na.rm = TRUE)))[,2])
results.table <- cbind(results.table,(subset(catch.traits, gear=="GN") %>% group_by(id) %>% summarise( mean(TrophicLevel, na.rm = TRUE)))[,2])
results.table<- cbind(results.table,(subset(catch.traits, gear=="TB") %>% group_by(id) %>% summarise( mean(as.integer(Gregariousness), na.rm = TRUE)))[,2])
results.table<- cbind(results.table,(subset(catch.traits, gear=="GN") %>% group_by(id) %>% summarise( mean(as.integer(Gregariousness), na.rm = TRUE)))[,2])
colnames(results.table) <- c("area", "TrapCPUE", "GillnetCPUE", "TrophicL_Traps", "TrophicL_Gillnets", "Greg_Traps", "Greg_Gillnets" )

write.csv2(results.table, "~/ownCloud/Research/Sudan/Distribution diversity LW paper/ICES ASC 2019/results_table.csv")
#' to do: Carnivore and Invertivore CPUE 



#' Not so relevant, since species richness = number of species
#' SPECIES PR UNIT EFFORT
#' anova analysis
#' ss.list<-unique(catch1$ss)

#' spue.table<-data.frame(0,0,0,0,0)
#' colnames(spue.table)<-c("ss","area", "spue", "nspec", "survey")

#' for (i in 1:length(ss.list)) { 
#'  spue.table[i,1] <- subset(catch1, catch1$ss == ss.list[i])$ss[1]
#'  spue.table[i,2] <- subset(catch1, catch1$ss == ss.list[i])$Area[1]
#'  spue.table[i,3] <- nrow(subset(catch1, catch1$ss == ss.list[i]))/subset(catch1, catch1$ss == ss.list[i])$Fhrs[1] 
 #' spue.table[i,4] <- nrow(subset(catch1, catch1$ss == ss.list[i])) 
#'  spue.table[i,5] <- as.integer(substr(ss.list[i], 1,7))
#'}

#' spue.agg<-aggregate(spue.table, list(Area = spue.table$area), mean)

#' spue.plot<-ggplot(spue.agg, aes(x=area, y=spue)) + geom_bar(stat="identity", fill="steelblue") +theme_bw() + scale_x_reverse(breaks=c(1,2,3,4,5,6,7)) 
#' spue.plot  + coord_flip() + theme(legend.title = element_text(size=14, face="bold")) + theme(legend.text = element_text(size=12)) + ylab("Species caught pr hour of fishing") + xlab("Management areas from N (upper) to S (lower)") 
#' ggsave("SPUE species pr area.png")



