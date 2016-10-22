#' @title Analysis of Length - Weight data from Sudan pilot survey 2012-2013
#' @details Three surveys: Nov 2012, May 2013 and Nov 2013
#' @details Bilogical data extracted from Nansis 27.10.2015 from survey computer, and corrected for outliers in excel
#' @author Erik Olsen
#' @note Revised Oct 2016



#' LIBRARIES AND SOURCES
#' -----------------------------
library("ggplot2")
library("RColorBrewer")
source('~/AtlantisNEUS_R/multiplot function.R', encoding='UTF-8')


#' IMPORT DATA
#' -----------------------------
setwd("~/ownCloud/Research/Sudan/Length_Weight paper")

species.list<-read.csv2("Species_table.csv")
length.weight<-read.csv2("LW_pilot_sudan12_15.csv")

#' Select 6 species for which specific LW data exists
#' LW data also exists for "SERPL03" and 'SEREP73', but no specimens of this species was caught during the pilot survey
study.species<-c("LUTLU06", "SEREP17", "SERPL07", "ACAAC28", "CARCA04", "CARCS04", "CARCS13", "CARSC01", "CARSC04", "CHRCH01", "GERGE02", "HOLSA03", "LETLE02", "LETLE05", "LETLE13", "LUTLU04", "LUTLU18", "LUTLU57", "SCMGY01", "SCMRA01", "SCMSM02", "SEREP07", "SEREP12", "SIGSI24","SPAAR01")

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


#' 
#' CALCULATE ERRORS BETWEEN OBSERVED WEiGHT AND PREDICTED WEIGHT
#' 

species.lw.table<-data.frame(Species=character(1), N=numeric(1), L_min=numeric(1), L_max=numeric(1), a=numeric(1),a_2_5=numeric(1), a_97=numeric(1), b=numeric(1), b_2_5=numeric(1), b_97=numeric(1), error=numeric(1),  t_test=numeric(1))

for (i in 1:length(study.species)){ 
    weight.table<-cbind(subset(lw.select, species==study.species[i])[,5], subset(lw.select, species==study.species[i])[,6], (species.list[grep(study.species[i], species.list[,3]),5] * subset(lw.select, species==study.species[i])[,5]^species.list[grep(study.species[i], species.list[,3]),6]), (species.list[grep(study.species[i], species.list[,3]),7] * subset(lw.select, species==study.species[i])[,5]^species.list[grep(study.species[i], species.list[,3]),8]), (species.list[grep(study.species[i], species.list[,3]),9] * subset(lw.select, species==study.species[i])[,5]^species.list[grep(study.species[i], species.list[,3]),10]))
    
  weight.table<-as.data.frame(weight.table)  
  
  weight.table[,6]<-(weight.table[,2]-weight.table[,3])/weight.table[,2]  
  colnames(weight.table)<-c("Length", "Obs", "FB", "N", "S", "Err")
  t.fb<-t.test(weight.table[,2], weight.table[,3], paired=TRUE) #test vs FishBase data
  #t.n<-t.test(weight.table[,2], lw.error.table[,4], paired=TRUE)
  #t.s<-t.test(weight.table[,2], lw.error.table[,5], paired=TRUE)
  
  #' to do:
  #' - export T-test results
  #' - make FB / Observed plots for all species w more than 10 observations - update 'study.species' list
  #' - calculate skill metrics - eg. RMSE 
  #' - calculate L-W equations based on own data using exponential regression
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
#' Plots for all species

lw.colours<- c("gray10", brewer.pal(length(study.species), "Set1"))

#lw.function<-function(x) {lw.select$a_FB*x^lw.select$b_FB}

lw.plot<-ggplot(lw.select, aes(length, weight)) + geom_point() + facet_wrap(~LatinName, scales = "free" )
lw.plot

#lw.plot.all<-ggplot(length.weight, aes(length, weight)) + geom_point() + facet_wrap(~species, scales = "free")
#lw.plot.all

#' plotting each study species seperately
#' 
#' Species data, with fitted curve (loess) and 95% CI (BLACK line)
#' 

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
  
  lw.plot.1 <- lw.plot + geom_line(data=lo.points,  colour=lw.colours[1], aes(l.fb, w.fb))
  #lw.plot.1

  #' testing out plot with geom_ribbon
  # lw.plot.2<-ggplot(lo.points, aes(l.fb, w.fb)) 
  # lw.plot.2<- lw.plot.2 + geom_ribbon(data=lo.points, fill="gray80", aes(ymin=w.lo, ymax=w.up))+ geom_line(data=lo.points,  colour=lw.colours[1], aes(l.fb, w.fb)) + ggtitle(species.list[grep(study.species[i], species.list[,3]),4]) + theme_classic() 
  # lw.plot.2 + geom_point(data=ss, aes(length, weight)) + coord_cartesian(ylim=c(0,max(ss$weight)))
  # lw.plot.2 + coord_cartesian(ylim=c(0,1000 )) + geom_point(ss, aes(length, weight))

  
  #' adding L-W line from FishBase (RED line)
  l.fb<-c(min(subset(lw.select, species==study.species[i])[5]):max(subset(lw.select, species==study.species[i])[5]))
  w.fb<-species.list[grep(study.species[i], species.list[,3]),5] * l.fb^species.list[grep(study.species[i], species.list[,3]),6] 
  fb.points<-as.data.frame(cbind(l.fb, w.fb))
  
  lw.plot.1 <- lw.plot.1 + geom_line(data=fb.points,  colour=lw.colours[2], aes(l.fb, w.fb))
  
  #' adding loca growth curves
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
  
#' ADD ALL PLOTS INTO 1 MULTIPLOT
#' 
lw.layout<-matrix(1:25, nrow=5, byrow=TRUE)
lw.plotlist<-list(ACAAC28.plot, CARCA04.plot, CARCS04.plot, CARCS13.plot, CARSC01.plot, CARSC04.plot, CHRCH01.plot, GERGE02.plot, HOLSA03.plot, LETLE02.plot, LETLE05.plot, LETLE13.plot, LUTLU04.plot, LUTLU06.plot, LUTLU18.plot, LUTLU57.plot, SCMGY01.plot, SCMRA01.plot, SCMSM02.plot, SEREP07.plot, SEREP12.plot, SEREP17.plot, SERPL07.plot, SIGSI24.plot, SPAAR01.plot)

multiplot(ACAAC28.plot, CARCA04.plot, CARCS04.plot, CARCS13.plot, CARSC01.plot, CARSC04.plot, CHRCH01.plot, GERGE02.plot, HOLSA03.plot, LETLE02.plot, LETLE05.plot, LETLE13.plot, LUTLU04.plot, LUTLU06.plot, LUTLU18.plot, LUTLU57.plot, SCMGY01.plot, SCMRA01.plot, SCMSM02.plot, SEREP07.plot, SEREP12.plot, SEREP17.plot, SERPL07.plot, SIGSI24.plot, SPAAR01.plot, layout=lw.layout)


