#' @title Sudan bar-graph of catches by latitude
#' @description make stacked bar-graph of catches
#' @author Erik Olsen
#' 


#' 
#' LIBRARIES AND SOURCES
#' --------------------------
#' 
library("ggplot2", lib.loc="/Users/eriko/Library/R/3.0/library")
library("RColorBrewer", lib.loc="/Users/eriko/Library/R/3.0/library")

#' LOADING DATA
#' --------------------------
setwd("/Users/eriko/ownCloud/Research/Sudan/SurveyProj2012_14/All_data")
catch<-read.csv2("catch.csv")
catch$survey<-as.factor(catch$survey)
ps<-data.frame(x=37.21709967,y=19.600512, group=1 )

catch1<-subset(catch, Sci_name!="NO CATCH")
catch.traps<-subset(catch1, gear!="GN")

#' reordering the species groups
fam.names<-c("ACANTHURIDAE", "CARANGIDAE", "CHIROCENTRIDAE", "LETHRINIDAE", "LUTJANIDAE", "SCOMBRIDAE",     "SERRANIDAE", "OTHER SPP" )  
levels(catch1$FamGroup)<-fam.names


#' CREATING CHARTS
#' ---------------------------
#' just for stations with catch
lat.plot<-ggplot(catch1, aes(lat, fill=FamGroup)) + geom_bar(binwidt=0.2, aes( weight=weight)) +theme_bw()
lat.plot + scale_fill_brewer(palette="RdYlBu", name="Family groups") + coord_flip() + theme(legend.title = element_text(size=14, face="bold")) + theme(legend.text = element_text(size=12)) + ylab("c Weight") + xlab("Latitude (Deg. N)") + theme(axis.text.x = element_blank())
ggsave("latitude_catch.png")

ggplot(catch.traps, aes(lat, fill=FamGroup)) + geom_bar(binwidt=0.2, aes(weight=weight)) +theme_bw()+ scale_fill_brewer(palette="RdYlBu") + coord_flip() + ylab("Cumulative Catch Weight") + xlab("Latitude (Deg. N)")+ theme(axis.text.x = element_blank())
ggsave("latitude_catch_noGN.png")
