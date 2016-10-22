# Sudan 2012 Survey
#-------------------
# analysis commands
# Created: 12.11.2012
# Updated: 12.11.2012

# Import station data
traps<-read.table("stations.csv",header=TRUE, dec=",", sep=";")
#traps<-subset(catches, Trap==1)

# Import species data
species<-read.table("Catch.csv", header=TRUE, dec=",",sep=";")
#remove empty catches
#species2<-subset(species1, Sci_name!="")
#species<-subset(species2, Sci_name!="N O   S A M P L E")

# Import Length Data
length<-read.table("Length data.csv", header=TRUE, dec=",",sep=";")


# Stations Exploratory statistical analysis
summary(traps)
summary(traps $ Gear)
#by(traps[12:14], traps[,"BaitType"], summary)
by(traps[13:14], traps[,"Gear"], summary)
boxplot(traps[12:15], col="pink")
#boxplot(TotWeight ~ BaitType, data=traps, subset = Trap ==1, boxwex = 0.5, col="lightblue", main="Total Catch Weight(kg) by bait type", xlab="bait type", ylab="Total Catch Weight (kg)")
#boxplot(SampNo ~ BaitType, data=traps, col="lightgreen")
boxplot(C_num ~ Gear, data=traps, xlab="Gear Type", ylab="Number of fish in catch", col="pink")
boxplot(C_weight ~ Gear, data=traps, xlab="Gear Type", ylab="Catch weight (kg)", col="seagreen3")
plot(traps[12:15])
#plot catch by depth
attach(traps)
plot(G_depth,C_weight)
plot(G_depth, C_num)


# Species exploratory analysis
by(species[5:6], species["Sci_name"], summary)
plot(species[5:6])
par(mar=c(5,8,4.1,2.1))
boxplot(C_Weight ~ Sci_name, data=species, col="olivedrab", las=2, xlab="Catch weight (kg)", cex.axis=0.5, horizontal=TRUE)
boxplot(C_num ~ Sci_name, data=species, col="peachpuff1", las=2, xlab="Number of fish in catch", cex.axis=0.5, horizontal=TRUE)

# Length-frequencies
rn_l<-length$L_cm
row.names(length)<-rn_l
df_length<-data.matrix(length[2:48])
#barplot(df_length, beside=TRUE)
#plot(length[1], length[2:4])
barchart(length[2:7])
barplot(as.matrix(length[48]), main="Length", ylab="Total", xlab=rn_l, beside=TRUE, col=terrain.colors(5))
#bruk Lattice 
attach(length)
histogram( length[,2]~length[,4] | interaction(length[,1]) )
histogram( length[,2]~length[,4] )

for (i in 15:48) {
	pdf(paste(colnames(length[i]), ".pdf", sep = ""))
	my.plot<-histogram(rep_l<-rep(length[,1],  length[,i]), type=c("count"), xlab=colnames(length[i]))
	print(my.plot)
	dev.off()
	
	}

 
