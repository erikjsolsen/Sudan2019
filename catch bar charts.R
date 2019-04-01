#' @title Sudan bar-graph of catches by latitude & management areas
#' @description make stacked bar-graph of catches
#' @author Erik Olsen
#' @note Revised Oct 2017
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

#' Add Month. Year survey column
catch$survey_m<-as.character(catch$survey)
catch$survey_m<-replace(catch$survey_m, grep("2012901", catch$survey_m), "Nov. 2012")
catch$survey_m<-replace(catch$survey_m, grep("2013002", catch$survey_m), "May 2013")
catch$survey_m<-replace(catch$survey_m, grep("2013005", catch$survey_m), "Nov. 2013")


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
lat.plot
ggsave("management area CPUE numbers.png")

#ANOVA CPUEn
catch.df<-subset(catch1, number>0)
catch.df$Area<-factor(catch.df$Area, labels = c(1:7))

ggplot(catch.df, aes(x = Area, y = CPUEn)) +geom_boxplot(fill = "grey80", colour = "blue") +scale_x_discrete() + xlab("Areas") +ylab("CPUEn")

catch.anova<-lm(CPUEn ~ Area, data = catch.df)
summary(catch.anova)
anova(catch.anova)
confint(catch.anova)
# not significant

# Residual plot ANOVA CPUEn
catch.mod <- data.frame(Fitted = fitted(catch.anova), Residuals = resid(catch.anova), Treatment = catch.df$Area)

ggplot(catch.mod, aes(Fitted, Residuals, colour = Treatment)) + geom_point()
ggsave("CPUEn anova residuals.png")

#Tukey post.hoc test
catch.aov<-aov(CPUEn ~ Area, data = catch.df)
posthoc<-TukeyHSD(x=catch.aov, 'Area', conf.level=0.95)
plot(catch.aov)

#CPUE kg plot
lat.plot<-ggplot(catch1, aes(Area, fill=FamGroup)) + geom_bar(binwidt=0.2, aes( weight=CPUEw)) +theme_bw() + scale_x_reverse(breaks=c(1,2,3,4,5,6,7)) 
lat.plot + scale_fill_brewer(palette="RdYlBu", name="Family groups") + coord_flip() + theme(legend.title = element_text(size=14, face="bold")) + theme(legend.text = element_text(size=12)) + ylab("CPUE weight (kg) of fish/hr fishing") + xlab("Management areas from N (1) to S (7)") 
+ theme(axis.text.x = element_blank()) 
lat.plot
ggsave("management area CPUE weight.tiff")

#ANOVA analysis of catch pr management area

catch.anova<-lm(CPUEw ~ Area, data = catch.df)
summary(catch.anova)
anova(catch.anova)
confint(catch.anova)
# no significant differences between management areas for CPUE-weight

#ANOVA by year and area
catch.anova2<-lm(CPUEw ~  survey + Area, data = catch.df)
summary(catch.anova2)
anova(catch.anova2)
#significant for intercept & survey 2013002
a1 <- aov(CPUEw ~  survey + Area, data = catch.df)
posthoc <- TukeyHSD(x=a1, 'survey', conf.level=0.95)
posthoc <- TukeyHSD(x=a1, 'Area', conf.level=0.95)


# demersal only
ggplot(catch.traps, aes(lat, fill=FamGroup)) + geom_bar(binwidt=0.2, aes(weight=weight)) +theme_bw()+ scale_fill_brewer(palette="RdYlBu") + coord_flip() + ylab("Cumulative Catch Weight") + xlab("Latitude (Deg. N)") + theme(axis.text.x = element_blank())
ggsave("latitude_catch_noGN.png")


#' PLOT BY GEAR TYPE
#' ------------------------------
catch.3gear<-subset(catch1, gear=="GN" | gear=="HL" | gear =="TB")

gear.plot <- ggplot(catch.3gear, aes(FamGroup, fill=survey_m)) + geom_bar(binwidt=0.2, aes( weight=CPUEw)) +theme_bw() + facet_wrap(~gear) + coord_flip() +xlab("") + ylab("kg fish/hour fishing")  + guides(fill=guide_legend(title="Survey"))
gear.plot
ggsave("gearplot.tiff")

gear.plot <- ggplot(catch.3gear, aes(FamGroup, fill=survey)) + geom_bar(binwidt=0.2, aes( weight=number)) +theme_bw() + facet_wrap(~gear) + coord_flip()  + theme(axis.text.x = element_blank()) + ylab("") + xlab("")
gear.plot

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

spue.table<-data.frame(0,0,0,0,0)
colnames(spue.table)<-c("ss","area", "spue", "nspec", "survey")

for (i in 1:length(ss.list)) { 
  spue.table[i,1] <- subset(catch1, catch1$ss == ss.list[i])$ss[1]
  spue.table[i,2] <- subset(catch1, catch1$ss == ss.list[i])$Area[1]
  spue.table[i,3] <- nrow(subset(catch1, catch1$ss == ss.list[i]))/subset(catch1, catch1$ss == ss.list[i])$Fhrs[1] 
  spue.table[i,4] <- nrow(subset(catch1, catch1$ss == ss.list[i])) 
  spue.table[i,5] <- as.integer(substr(ss.list[i], 1,7))
}

#summary statistics
by(spue.table, spue.table$survey, summary)


spue.agg<-aggregate(spue.table, list(Area = spue.table$area), mean)

spue.plot<-ggplot(spue.agg, aes(x=area, y=spue)) + geom_bar(stat="identity", fill="steelblue") +theme_bw() + scale_x_reverse(breaks=c(1,2,3,4,5,6,7)) 
spue.plot  + coord_flip() + theme(legend.title = element_text(size=14, face="bold")) + theme(legend.text = element_text(size=12)) + ylab("Species caught pr hour of fishing") + xlab("Management areas from N (upper) to S (lower)") 
ggsave("SPUE species pr area.png")



#ANOVA Species Numbers
spue.df<-spue.table
spue.df$area<-factor(spue.df$area, labels = c(1:7))

ggplot(spue.df, aes(x = area, y = nspec)) +geom_boxplot(fill = "grey80", colour = "blue") +scale_x_discrete() + xlab("Areas") +ylab("number of species")

spue.anova<-lm(nspec ~ area, data = spue.df)
summary(spue.anova)
#significant for 1, 2 and 3
anova(spue.anova)
confint(spue.anova)
#  significant

# Residual plot species number
spue.mod <- data.frame(Fitted = fitted(spue.anova), Residuals = resid(spue.anova), Treatment = spue.df$area)

ggplot(spue.mod, aes(Fitted, Residuals, colour = Treatment)) + geom_point()
ggsave("species numbers anova residuals.png")

#Tukey post.hoc test
spue.aov<-aov(nspec ~ area, data = spue.df)
posthoc<-TukeyHSD(x=spue.aov, 'area', conf.level=0.95)
plot(spue.aov)
# many significan effects



#ANOVA Species CPUE
spue.df<-spue.table
spue.df$area<-factor(spue.df$area, labels = c(1:7))

ggplot(spue.df, aes(x = area, y = spue)) +geom_boxplot(fill = "grey80", colour = "blue") +scale_x_discrete() + xlab("Areas") +ylab("CPUEn")

spue.anova<-lm(spue ~ area, data = spue.df)
summary(spue.anova)
#not significant
anova(spue.anova)
confint(spue.anova)
#  significant

# Residual plot species number
spue.mod <- data.frame(Fitted = fitted(spue.anova), Residuals = resid(spue.anova), Treatment = spue.df$area)

ggplot(spue.mod, aes(Fitted, Residuals, colour = Treatment)) + geom_point()
ggsave("species numbers anova residuals.png")

#Tukey post.hoc test
spue.aov<-aov(spue ~ area, data = spue.df)
posthoc<-TukeyHSD(x=spue.aov, 'area', conf.level=0.95)
plot(spue.aov)
# many significan effects
