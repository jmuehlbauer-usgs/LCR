##### LCR sticky trap analysis #####


##### Preliminary prep #####

## Set working directory
#setwd('P:/BIOLOGICAL/Flyco/LCR/Data & Analysis/R')
setwd('C:/Users/jmuehlbauer/Documents/Projects/LCR/LCR Food Web/Data & Analysis/R')

## Load libraries and functions
library(lme4)
library(lubridate)
library(vegan)
source('PlotTypes.r')
source('NMS.r')
source('OrdFunctions.r')

## Read in data
trap0<-read.csv('StickyTrap.csv')
spec0<-read.csv('StickySpecimen.csv')

## Filter to only LCR data
trap1<-trap0[substr(trap0$TripID,1,2)=='LC',]
spec1<-spec0[spec0$BarcodeID%in%trap1$BarcodeID,]


##### Combine dataframes, clean up data #####

## Merge trap and spec
lcr0<-merge(trap1,spec1,'BarcodeID','BarcodeID')

## Get Months and years from Deployment Dates
lcr0$Month<-as.factor(month(mdy(lcr0$DateDeploy)))
lcr0$Year<-as.factor(year(mdy(lcr0$DateDeploy)))

## Filter out unneeded columns
lcr1<-lcr0[,c('BarcodeID','TripID','DateDeploy','Month','Year','TrapOrient','RiverMile','Bank','TotalHrs','DurationDeploy','CHIA','SIMA','TERR_chi.sized','TERR_sim.sized','TERR_lg.sized','HYOS','HYDA','TRIA','EPHA')]
	colnames(lcr1)<-c('Barcode','TripID','DateDeploy','Month','Year','Orientation','RM','Bank','Hours','Duration','CHIA','SIMA','TERRchi','TERRsim','TERRlg','HYOS','HYDA','TRIA','EPHA')

## Convert NAs in counts to 0s
lcr1[is.na(lcr1)]<-0

## Round duration times to nearest .1 hour
lcr1$Hours<-round(lcr1$Hours,1)

## Create total counts and EPT
lcr1$TotalAq<-lcr1$CHIA+lcr1$SIMA+lcr1$HYOS+lcr1$HYDA+lcr1$TRIA+lcr1$EPHA
lcr1$TotalTerr<-lcr1$TERRchi+lcr1$TERRsim+lcr1$TERRlg
lcr1$Total<-lcr1$TotalAq+lcr1$TotalTerr
lcr1$TRIAall<-lcr1$HYOS+lcr1$HYDA+lcr1$TRIA
lcr1$EPT<-lcr1$TRIAall+lcr1$EPHA
lcr1$percEPT<-ifelse(lcr1$TotalAq>0,lcr1$EPT/lcr1$TotalAq,0)

## Filter out emergence boats
lcr2<-lcr1[substr(lcr1$Orientation,1,4)!='Boat',]

## Filter out traps left out for longer than a few days
lcr2.1<-lcr2[lcr2$Hours<=100,]

## Filter to only 2014 and later data
lcr3<-lcr2.1[year(mdy(lcr2.1$DateDeploy))>=2014,]

## Drop factor levels
lcr3<-droplevels(lcr3)

## Combine 2.9 & 3.2 samples and 16 & 16.2 samples
	## Discontinued RM 3.2 and 16 samples after first or second sampling event, but removing them biases data because of loss of high density spring traps.
lcr4<-lcr3
	lcr4$RM[lcr4$RM==3.2]<-2.9
	lcr4$RM[lcr4$RM==16]<-16.2

## Convert durations to numeric
	## Round to 24, 48, or 72 hours if possible, or else use actual hours for any real oddballs.
lcr5.0<-lcr4
lcr5.0$Duration<-suppressWarnings(as.numeric(substr(lcr4$Duration,1,2),silent=T))
	lcr5.0$Duration<-ifelse(lcr4$Duration!='Other',lcr5.0$Duration,ifelse(lcr4$Hour>64&lcr4$Hour<80,72,lcr4$Hour))

## Convert counts to rates per day
	## Use duration unless "Other", then use total hours
strt<-match('CHIA',colnames(lcr5.0))
stp<-match('EPT',colnames(lcr5.0))
lcr5.tmp<-lcr5.0[,strt:stp]
	lcr5.tmp2<-lcr5.tmp/(lcr5.0$Duration/24)
lcr5<-cbind(lcr5.0[,1:(strt-1)],lcr5.tmp2,lcr5.0[,(stp+1):dim(lcr5.0)[2]])
colnames(lcr5)<-colnames(lcr5.0)


##### Make a dataframe for Maria #####

## Subset only Rkm 2-7
lcr6<-lcr5[lcr5$RM>=2&lcr5$RM<=7,]

## Create month/rm variable
lcr6$morm<-paste(lcr6$Month,lcr6$RM)

## Get means of taxa of interest by month and RM
chia<-round(tapply(lcr6$CHIA,lcr6$morm,mean),1)
sima<-round(tapply(lcr6$SIMA,lcr6$morm,mean),1)
tota<-round(tapply(lcr6$TotalAq,lcr6$morm,mean),1)
Month<-as.numeric(substr(names(chia),1,2))
RM<-as.numeric(substr(names(chia),3,6))
maria<-as.data.frame(cbind(Month,RM,chia,sima,tota))
	rownames(maria)<-c(1:dim(maria)[1])
maria2<-maria[order(maria$RM),]
maria3<-maria2[order(maria2$Month),]

## Export data for Maria
#write.csv(maria3,'Bug Data for Maria.csv')


##### Quick plots #####

## Plot all data by Rkm
plot(lcr5$Total~lcr5$RM)
	## Terrible, try averages

## Make function to plot averages by Rkm
spaceplot<-function(pts1,pts2,dataframe){
pts1avgs<-tapply(dataframe[,pts1],dataframe$RM,mean)
pts2avgs<-tapply(dataframe[,pts2],dataframe$RM,mean)
spots<-c(0,1.95,9,10.4,13.6,15.2,16.2,20.8)
plot(as.numeric(names(pts1avgs)),pts1avgs,xlab='River km',ylab='Mean catch per day',bty='l',col=2)
	abline(v=spots,lty=3,col=8)
	points(as.numeric(names(pts2avgs)),pts2avgs)
	ltot<-loess(dataframe[,pts1]~dataframe$RM)
	laq<-loess(dataframe[,pts2]~dataframe$RM)	
		ord<-order(dataframe$RM)
		lines(dataframe$RM[ord],ltot$fitted[ord],col=2,lty=2)
		lines(dataframe$RM[ord],laq$fitted[ord],lty=2)
	text(spots,max(c(pts1avgs,pts2avgs)),c('Confluence','Boulders camp','Coyote camp','Salt Camp','Lower Atomizer Falls','Chute Falls','Chute camp','Blue Spring'),srt=90,adj=c(1,-.25))
	legend(3,max(c(pts1avgs,pts2avgs)),legend=c(pts1,pts2),pch=1,col=c(2,1),lty=2,bty='n')
}

## Plot averages by Rkm
spaceplot('Total','TotalAq',lcr5)

## Make function to plot data by time
timeplot<-function(colname){
avgs<-tapply(lcr5[,colname],lcr5$Month,mean)
par(bty='l')
boxplot(lcr5[,colname]~as.numeric(as.character(lcr5$Month)),xaxt='n',yaxt='n',ylab=paste0('Catch per day (',colname,')'),xlab='Month')
	axis(1,at=1:length(avgs),labels=month.name[as.numeric(as.character(names(avgs)))])
	axis(2,las=2)
	points(1:length(avgs),avgs,pch=15)
	ltot2<-loess(lcr5[,colname]~as.numeric(as.character(lcr5$Month)))
		ord2<-order(as.numeric(as.character(lcr5$Month)))
		ordmon<-lcr5$Month[ord2]
			ordmon2<-as.numeric(ordmon)
		lines(ordmon2,ltot2$fitted[ord2],lty=2)
}

## Plot all data, and only aquatic data by time
timeplot('Total')
timeplot('TotalAq')	

## Plot longitudinal averages April only
april<-lcr5[lcr5$Month==4,]
spaceplot('Total','TotalAq',april)

## Plot longitudinal averages May only
may<-lcr5[lcr5$Month==5,]
spaceplot('Total','TotalAq',may)

## Plot longitudinal and temporal averages EPT and TRIAall only
spaceplot('EPT','TRIAall',lcr5)
timeplot('EPT')
timeplot('TRIAall')

## Plot longitudinal and temporal averages in TRIAreg
spaceplot('TRIAall','TRIAreg',lcr5)
timeplot('TRIAreg')
	## Appears that database is still un-differentiated for TRIA (all put in TRIAreg). Need to fix that.

## Plot longitudinal and temporal averages in %EPT
spaceplot('percEPT','percEPT',lcr5)
timeplot('percEPT')


##### Make some nice figures for the poster #####

## Catch by year (by month)
dat14<-lcr5[lcr5$Year==2014,]
dat15<-lcr5[lcr5$Year==2015,]
means14<-tapply(dat14$TotalAq,dat14$Month,mean)
means15<-tapply(dat15$TotalAq,dat15$Month,mean)
meds14<-tapply(dat14$TotalAq,dat14$Month,median)
meds15<-tapply(dat15$TotalAq,dat15$Month,median)
baryrs<-function(){
barplot(rbind(means14,means15),beside=TRUE,names.arg=month.name[as.numeric(as.character((sort(unique(lcr5$Month)))))],axis.lty=1,col=c('orange','purple'),ylab='Average sticky trap catch per day',axes=F)
	legend('topright',legend=c('2014','2015','Median'),fill=c('orange','purple',NA),border=c(1,1,NA),pch=c(NA,NA,19),bty='n')
	axis(2,las=2)
	box(bty='l')
	points(seq(1.5,16.5,3),meds14,pch=19)
	points(seq(2.5,17.5,3),meds15,pch=19)

}
#plotTypes(baryrs,directory='Figures/',filename='TimePlotByYr',width=8,height=6.5,filetype='png')


## Catch over time
nicetime<-function(){
avgs<-tapply(lcr5[,'TotalAq'],lcr5$Month,mean)
par(bty='l')
boxplot(lcr5[,'TotalAq']~as.numeric(as.character(lcr5$Month)),ylim=c(0,150),xaxt='n',yaxt='n',ylab='Average sticky trap catch per day',xlab='Month',col=c('darkgreen','lightgreen','yellow','orange','red'))
	axis(1,at=1:length(avgs),labels=month.name[as.numeric(as.character(names(avgs)))])
	axis(2,las=2)
	points(1:length(avgs),avgs,pch=15)
	ltot2<-loess(lcr5[,'TotalAq']~as.numeric(as.character(lcr5$Month)),span=.3)
		ord2<-order(as.numeric(as.character(lcr5$Month)))
		ordmon<-lcr5$Month[ord2]
			ordmon2<-as.numeric(ordmon)
		lines(ordmon2,ltot2$fitted[ord2],lty=2)
}
#plotTypes(nicetime,directory='Figures/',filename='TimePlot2Yr',width=8,height=6.5,filetype='png')

## Catch longitudinally
nicespace<-function(){
pts1avgs<-tapply(lcr5[,'Total'],lcr5$RM,mean)
pts2avgs<-tapply(lcr5[,'TotalAq'],lcr5$RM,mean)
spots<-c(0,1.95,9,10.4,13.6,15.2,16.2,20.8)
plot(as.numeric(names(pts1avgs)),pts1avgs,xlab='River km',yaxt='n',ylab='Average sticky trap catch per day',bty='l',col=2)
	axis(2,las=2)
	abline(v=spots,lty=3,col=8)
	points(as.numeric(names(pts2avgs)),pts2avgs,col=4,pch=2)
	ltot<-loess(lcr5[,'Total']~lcr5$RM)
	laq<-loess(lcr5[,'TotalAq']~lcr5$RM)	
		ord<-order(lcr5$RM)
		lines(lcr5$RM[ord],ltot$fitted[ord],col=2,lty=2)
		lines(lcr5$RM[ord],laq$fitted[ord],col=4,lty=2)
	text(spots,max(c(pts1avgs,pts2avgs)),c('Confluence','Boulders camp','Coyote camp','Salt Camp','Lower Atomizer Falls','Chute Falls','Chute camp','Blue Spring'),srt=90,adj=c(1,-.25))
	legend(2.8,58,legend=c('All insects','All aquatic insects'),pch=c(1,2),col=c(2,4),lty=2)
}
#plotTypes(nicespace,directory='Figures/',filename='SpacePlot2Yr',width=6.5,height=6.5,filetype='png')

## EPT Catch longitudinally
nicespace<-function(){
pts1avgs<-tapply(lcr5[,'EPT'],lcr5$RM,mean)
pts2avgs<-tapply(lcr5[,'TRIAall'],lcr5$RM,mean)
spots<-c(0,1.95,9,10.4,13.6,15.2,16.2,20.8)
plot(as.numeric(names(pts1avgs)),pts1avgs,xlab='River km',yaxt='n',ylab='Average sticky trap catch per day',bty='l',col='cyan4')
	axis(2,las=2)
	abline(v=spots,lty=3,col=8)
	points(as.numeric(names(pts2avgs)),pts2avgs,col='darkviolet',pch=2)
	ltot<-loess(lcr5[,'EPT']~lcr5$RM)
	laq<-loess(lcr5[,'TRIAall']~lcr5$RM)	
		ord<-order(lcr5$RM)
		lines(lcr5$RM[ord],ltot$fitted[ord],col='cyan4',lty=2)
		lines(lcr5$RM[ord],laq$fitted[ord],col='darkviolet',lty=2)
	text(spots,max(c(pts1avgs,pts2avgs)),c('Confluence','Boulders camp','Coyote camp','Salt Camp','Lower Atomizer Falls','Chute Falls','Chute camp','Blue Spring'),srt=90,adj=c(1,-.25))
	legend(2.8,1.5,legend=c('All EPT','Caddisflies only'),pch=c(1,2),col=c('cyan4','darkviolet'),lty=2)
}
plotTypes(nicespace,directory='Figures/',filename='EPTSpacePlot2Yr',width=6.5,height=6.5,filetype='png')


##### Model patterns using mixed effects models #####

## Time consuming and kind of bogus at this point, so blocked out for now.

## Look at count data to predict family
	## Use raw counts data (not rates), offset duration in the model
#hist(lcr4$Total)
	## Yep, pretty typical 0-inflated ecological count data. Try NB, along with others.

## Fit some lme4 models on Total counts
	## Use Deployment Duration as an offset, Orientation as a random effect, and catch and Rkm and month as fixed, numeric effects.
#Gaus1<-lmer(Total~RM+as.numeric(as.character(Month))+(1|Orientation)+offset(Hours),data=lcr4,REML=F)
#Pois1<-glmer(Total~scale(RM)+as.numeric(as.character(Month))+(1|Orientation)+offset(log(Hours)),data=lcr4,family='poisson')
#NB1<-glmer.nb(Total~scale(RM)+as.numeric(as.character(Month))+(1|Orientation)+offset(log(Hours)),data=lcr4)
#sapply(c(Gaus1,Pois1,NB1),AIC)
	## The Poisson model is clearly terrible. Stick with NB.

## Try fitting model using month as a factor
#NB2<-glmer.nb(Total~RM+Month+(1|Orientation)+offset(log(Hours)),data=lcr4)
#sapply(c(NB1,NB2),AIC)
	## Better to use month as a factor than a continuous numeric variable.

## Try fitting model using month as a random intercept
#NB3<-glmer.nb(Total~scale(RM)+(1|Month)+(1|Orientation)+offset(log(Hours)),data=lcr4)
#sapply(c(NB2,NB3),AIC)
	## Model markedly worse this way. Slightly easier to think of Month as a fixed effect anyway, so stick with that (NB2).

#NB4<-glmer.nb(Total~as.factor(RM)+(1|Month)+(1|Orientation)+offset(log(Hours)),data=lcr4)
#parms<-fixef(NB4)[1]
#for(i in 2:length(fixef(NB4))){
#	parms[i]<-fixef(NB4)[i]+fixef(NB4)[1]
#}
#plot(sort(unique(lcr4$RM)),parms)


##### Run preliminary NMS #####

## Create species dataframe
	## Use only taxa with some counts (will need to change in the future)
lcr6<-lcr5[,c('CHIA','SIMA','TERRchi','TRIAreg','TRIAsm','EPHA')]

## Create environmental dataframe
	## Should add in more variables (like canyon orientation) in the future
env<-lcr5[,c("RM","Month","Orientation")]
env$reach<-c("Boulders","Coyote","Salt","Blue")
for(i in 1:dim(env)[1]){
	if(env$RM[i]<=5) {env$reach[i]<-"Boulders"}
	else{if(env$RM[i]<=9.6) {env$reach[i]<-"Coyote"}
		else{if(env$RM[i]<=13.57) {env$reach[i]<-"Salt"}
			else{env$reach[i]<-"Blue"}
		}
	}
}

## Run NMS
#NMS(lcr6)
	## If firewall blocks, use: 
	#NMS(lcr6,cores=1)
	## Preliminary ordination looks 3D?
	
## Read in NMS points and species 2D and 3D output
pts2D<-read.csv("NMS Output/NMSPoints2D.csv",row.names=1)
pts3D<-read.csv("NMS Output/NMSPoints3D.csv",row.names=1)
sps2D<-read.csv("NMS Output/NMSSpecies2D.csv",row.names=1)
sps3D<-read.csv("NMS Output/NMSSpecies3D.csv",row.names=1)

## Plot 2D ordination for simplicity
plot(pts2D)

## Overlay species
points(sps2D,col=2,pch=17)
text(sps2D$MDS1,sps2D$MDS2,rownames(sps2D))
 
## Add environmental data to NMS
ef<-envfit(pts2D,env)
plot(ef,p.max=0.2)
	## Interesting "bow and arrow"-type pattern: RM is the big gradient, which goes from all TERR and CHI near the confluence to heavy EPT presence farther upstream

## Rotate ordination around RM gradient, re-plot
pts2D.rot<-ord.rotate(pts2D,ef,'RM')
sps2D.rot<-ord.rotate(sps2D,ef,'RM')
plot(pts2D.rot)
	points(sps2D.rot,col=2,pch=17)
	text(sps2D.rot$MDS1,sps2D.rot$MDS2,rownames(sps2D.rot))
ef.rot<-envfit(pts2D.rot,env)
	plot(ef.rot,p.max=0.2)

## Get Axis R2s
AxisR2(lcr6,pts2D.rot)
	## ~40% overall. Not too great.