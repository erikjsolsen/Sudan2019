#' @title Sudan bar-graph of catches by latitude & management areas
#' @description make stacked bar-graph of catches
#' @author Erik Olsen
#' @note Revised Oct 2016
#' 


#' 
#' LIBRARIES AND SOURCES
#' --------------------------
#' 
library("ggplot2")
library("RColorBrewer")
library("reshape2")

#' LOADING DATA
#' --------------------------
setwd("/Users/eriko/ownCloud/Research/Sudan/SurveyProj2012_14/All_data")
catch<-read.csv2("catch.csv")
catch$survey<-as.factor(catch$survey)
ps<-data.frame(x=37.21709967,y=19.600512, group=1 )
catch$CPUEw<-catch$weight/catch$Fhrs
catch$CPUEn<-catch$number/catch$Fhrs


#' ALGORITHM FOR ALLOCATING management areas
# A1 lat > 21.3
# A2 lat > 20.6 & lat <= 21.3
# A3 lat > 20.0 & lat <= 20.6
# A4 lat > 19.0 & lat <= 20.0, lon < 37.5
# A5 lat > 18.6 & lat <= 19.0, lon < 37.8
# A6  - the rest
# A7 lat <= 18.9    lon >= 37.8  & lon < 38.5
A1<-subset(catch, lat > 21.3)
A1$Area<-1

A2<-subset(catch, lat<=21.3)
A2<-subset(A2, lat>20.6)
A2$Area<-2

A3<-subset(catch, lat<=20.6)
A3<-subset(A3, lat>20)
A3$Area<-3

A4<-subset(catch, lat<=20)
A4<-subset(A4, lat>19)
A4<-subset(A4, lon<37.5)
A4$Area<-4

A5<-subset(catch, lat<=19)
A5<-subset(A5, lat>18.6)
A5<-subset(A5, lon<37.8)
A5$Area<-5

A6<-subset(catch, lon>=37.122)
A6a<-subset(A6, lat>18.97)
A6a$Area<-6
A6b<-subset(A6, lon>=38.5)
A6b<-6

A7<-subset(catch, lat<=18.9)
A7<-subset(A7, lon<38.5)
A7<-subset(A7, lon>=37.8)
A7$Area<-7

catch<-rbind(A1, A2, A3, A4, A5, A6a, A6b, A7)


#' selecting only stations with catch
catch1<-subset(catch, Sci_name!="NO CATCH")
#' removes stations shorter than 1 hour
catch1<-subset(catch1, Fhrs>1)
catch.traps<-subset(catch1, gear!="GN")

#' reordering the species groups
fam.names<-c("ACANTHURIDAE", "CARANGIDAE", "CHIROCENTRIDAE", "LETHRINIDAE", "LUTJANIDAE", "SCOMBRIDAE",     "SERRANIDAE", "OTHER SPP" )  
levels(catch1$FamGroup)<-fam.names


#' CREATING CHARTS
#' ---------------------------
#' just for stations with catch
#lat.plot<-ggplot(catch1, aes(lat, fill=FamGroup)) + geom_bar(binwidt=0.2, aes( weight=weight)) +theme_bw()
# CPUE Numbers plot
lat.plot<-ggplot(catch1, aes(Area, fill=FamGroup)) + geom_bar(binwidt=0.2, aes( weight=CPUEn)) +theme_bw() + scale_x_reverse(breaks=c(1,2,3,4,5,6,7)) 
lat.plot + scale_fill_brewer(palette="RdYlBu", name="Family groups") + coord_flip() + theme(legend.title = element_text(size=14, face="bold")) + theme(legend.text = element_text(size=12)) + ylab("CPUE number of fish/hr fishing") + xlab("Management areas from N (1) to S (7)") + theme(axis.text.x = element_blank()) 
#ggsave("latitude_catch.png")
ggsave("management area CPUE numbers.png")

#CPUE kg plot
lat.plot<-ggplot(catch1, aes(Area, fill=FamGroup)) + geom_bar(binwidt=0.2, aes( weight=CPUEw)) +theme_bw() + scale_x_reverse(breaks=c(1,2,3,4,5,6,7)) 
lat.plot + scale_fill_brewer(palette="RdYlBu", name="Family groups") + coord_flip() + theme(legend.title = element_text(size=14, face="bold")) + theme(legend.text = element_text(size=12)) + ylab("CPUE weight (kg) of fish/hr fishing") + xlab("Management areas from N (1) to S (7)") + theme(axis.text.x = element_blank()) 
ggsave("management area CPUE weight.png")

# demersal only
ggplot(catch.traps, aes(lat, fill=FamGroup)) + geom_bar(binwidt=0.2, aes(weight=weight)) +theme_bw()+ scale_fill_brewer(palette="RdYlBu") + coord_flip() + ylab("Cumulative Catch Weight") + xlab("Latitude (Deg. N)") + theme(axis.text.x = element_blank())
ggsave("latitude_catch_noGN.png")


#' PLOT BY GEAR TYPE
#' ------------------------------
catch.3gear<-subset(catch1, gear=="GN" | gear=="HL" | gear =="TB")

gear.plot <- ggplot(catch.3gear, aes(FamGroup, fill=survey)) + geom_bar(binwidt=0.2, aes( weight=CPUEw)) +theme_bw() + facet_wrap(~gear) + coord_flip()  + theme(axis.text.x = element_blank()) + ylab("") + xlab("")
gear.plot
ggsave("gearplot.png")


#' ------------------------------
#' SPECIES NUMBERS PR AREA

sp.indicies<-by(catch$species, catch$Area, summary)

sp.numbers<-c(0)

species.area.list<-list(1,2,3,4,5,6,7)

for (j in 1:length(species.area.list)) {
  for (i in 1:length(sp.indicies[[j]])){
    if (sp.indicies[[j]][i] != 0) { 
      species.area.list[[j]] <- species.area.list[[j]] +sp.indicies[[j]][i]/sp.indicies[[j]][i] }
    else  { species.area.list[[j]]<-species.area.list[[j]] }
  }
}

m.species<-melt(species.area.list)

#' Species No. Plots
species.plot<-ggplot(m.species, aes(x=L1, y=value)) + geom_bar(stat="identity", fill="steelblue") +theme_bw() + scale_x_reverse(breaks=c(1,2,3,4,5,6,7)) 
species.plot  + coord_flip() + theme(legend.title = element_text(size=14, face="bold")) + theme(legend.text = element_text(size=12)) + ylab("Number of species caught") + xlab("Management areas from N (1) to S (7)") 
ggsave("No species pr area.png")



#' SPECIES PR UNIT EFFORT
#' --------------------------

ss.list<-unique(catch1$ss)

spue.table<-data.frame(0,0,0)
colnames(spue.table)<-c("ss","area", "spue")

for (i in 1:length(ss.list)) { 
  spue.table[i,1] <- subset(catch1, catch1$ss == ss.list[i])$ss[1]
  spue.table[i,2] <- subset(catch1, catch1$ss == ss.list[i])$Area[1]
  spue.table[i,3] <- nrow(subset(catch1, catch1$ss == ss.list[i]))/subset(catch1, catch1$ss == ss.list[i])$Fhrs[1] 
}

spue.agg<-aggregate(spue.table, list(Area = spue.table$area), mean)

spue.plot<-ggplot(spue.agg, aes(x=area, y=spue)) + geom_bar(stat="identity", fill="steelblue") +theme_bw() + scale_x_reverse(breaks=c(1,2,3,4,5,6,7)) 
spue.plot  + coord_flip() + theme(legend.title = element_text(size=14, face="bold")) + theme(legend.text = element_text(size=12)) + ylab("Species caught pr hour of fishing") + xlab("Management areas from N (upper) to S (lower)") 
ggsave("SPUE species pr area.png")
