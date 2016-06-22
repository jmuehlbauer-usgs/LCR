##### Set Working Directory, read in data #####

## Working directory
setwd('P:/Biological/Flyco/LCR/Data & Analysis')

## Get data file names
 ## April and May 2015
may15<-list.files('Data Loggers/HOBO/LIGHT/May_2015',pattern='.csv')
apr15<-list.files('Data Loggers/HOBO/LIGHT/April_2015',pattern='.csv')
jun15<-list.files('Data Loggers/HOBO/LIGHT/June_2015',pattern='.csv')

## Read in data files, cut out useless rows and columns of data, including bad dates
##add useful columns of data for depth(sensor number), time, site, Date##
#find actual depths for light sensors then create the following code: Z<-cbind(c(1:20))
		Z<-cbind(c(0,37.5,67,86,0,39.5,69,88,0,38.8,68.3,87.3,0,32,61.5,80.5,0,35,64.5,83.5))
       rownames(Z)<-paste0(rep(c('Blue','Chute','Salt','Coyote','Boulders'),rep(4,5)),rep(1:4,5))
aprdat<-list()
maydat<-list()
for(i in 1:length(apr15)){
	t1<-read.csv(paste0('Data Loggers/HOBO/LIGHT/April_2015/',apr15[i]),skip=1,row.names=1)
	t2<-t1[,1:3]
		colnames(t2)<-c('DateTime','Temp','Lux')
		t2$Date<-as.Date(t2$DateTime,'%m/%d/%y')
	aprdat[[i]]<-t2[t2$Date %in% as.Date(c('2015-04-14','2015-04-15','2015-04-16')),]
	}
	names(aprdat)<-substr(apr15,1,regexpr('0',apr15)-1)
for(i in 1:length(may15)){
	t1<-read.csv(paste0('Data Loggers/HOBO/LIGHT/May_2015/',may15[i]),skip=1,row.names=1)
	t2<-t1[,1:3]
		colnames(t2)<-c('DateTime','Temp','Lux')
		t2$DateTime<-strptime(t2$DateTime,'%m/%d/%y %I:%M:%S %p') 
		t2$Date<-as.Date(t2$DateTime)	
		t2$Sensor<-as.numeric(substr(may15[i],regexpr('-',may15[i])+1,regexpr('-',may15[i])+1))
		t2$Site<-substr(may15[i],1,regexpr('-',may15[i])-1)
		t2$Depth<-Z[paste0(t2$Site,t2$Sensor),]
			maydat[[i]]<-t2[t2$Date %in% as.Date(c('2015-05-19','2015-05-20','2015-05-21')),]
	}
	names(maydat)<-substr(may15,1,regexpr('_',may15)-1)
	

##### Get some averages for data to check data quality #####

## Write a function for computing means, medians, maxima by date
fxMean<-function(x) round(tapply(x$Lux,x$Date,mean))

## Look at the means, medians, and maxima by date
aprMean<-as.data.frame(t(sapply(aprdat,fxMean)))
mayMean<-(sapply(maydat,fxMean))


#### Plot April mean lux data by river mile (actually km) ####

## Make a vector of river miles and of surface vs. underwater for April data
aprRM<-c(21,2.1,16.2,9.0,10.4)
aprDepth<-as.factor(rep(c('Underwater','OnLand'),5))

## Plot the data
plot(c(min(aprRM),max(aprRM)),c(min(aprMean),max(aprMean)),type='n',xlab='River km',ylab='Mean lux')
	for(i in 1:3){
		UW<-aprMean[aprDepth=='Underwater',i]
		OL<-aprMean[aprDepth=='OnLand',i]
		points(aprRM,UW,col='blue')
			lines(lowess(aprRM,UW),lty=i,col='blue')
		points(aprRM,OL,col='red')
			lines(lowess(aprRM,OL),lty=i,col='red')
	}
	legend('topright',legend=c('On land','Underwater','2015-04-14','2015-04-15','2015-04-16'),lty=c(NA,NA,1:3),pch=c(1,1,rep(NA,3)),col=c('red','blue',rep(1,3)))
	

##### Plot April mean lux data by date #####

## Get data in a more useful format for such a plot
aprMean.t<-as.data.frame(cbind(rep(rownames(aprMean),3),rep(colnames(aprMean),each=10),rep(aprRM,each=2),c('Underwater','OnLand'),unlist(aprMean)))
	rownames(aprMean.t)<-c(1:dim(aprMean.t)[1])
	colnames(aprMean.t)<-c('Sensor','Date','RM','Depth','Lux')
	aprMean.t$Date<-as.Date(aprMean.t$Date)
	aprMean.t$Lux<-as.numeric(as.character(aprMean.t$Lux))
	
## Use lattice library to plot
library(lattice)

xyplot(Lux~Date|Sensor,data=aprMean.t,type='o')

### Eric: This gives you some basic examples to work with. You may want to try and do the same for April medians, maxima, etc. YOu may also want to play around with the May data in some sensible fashion. Note that the last line of code relies on the lattice library (called near the top of the code), which is a common one you'll probably want. I happen to prefer using the base functions because I don't like being constrained by lattice, but that's just me. Also note that I ended up using the function sapply instead of lapply to get the means and such, as this ended up being just a bit cleaner. I also broke out the function we're running within apply into its own line of code to hopefully make what I did a little easier for you to understand. Finally, I modified the for() loop a bit to cut out the useless dates and so on. Enjoy!


#### Erics attempt to work with May Data below ####
## transpose the mayMean##
mayMean<-as.data.frame(t(sapply(maydat,fxMean)))

## add columns to mayMean for RM and Depth##
##create vectors for rKm and sensor depth specifically for may data##

mayMean$RKm<-rep(c(21,1.95,16.2,9.0,10.4),c(4,4,4,4,4))
mayMean$Depth<-as.factor(rep(c('OnLand','Underwater1','Underwater2','Underwater3'),5))
##Plot may data##
	#created specific points and lines for different depths,EK#
plot(c(min(mayMean$RKm),max(mayMean$RKm)),c(min(mayMean[,1:3]),max(mayMean[,1:3])),type='n',xlab='River km',ylab='Mean lux')
	for(i in 1:3){
		UW1<-mayMean[mayMean$Depth=='Underwater1',c(i,4,5)]
		UW2<-mayMean[mayMean$Depth=='Underwater2',c(i,4,5)]
		UW3<-mayMean[mayMean$Depth=='Underwater3',c(i,4,5)]
		OL<-mayMean[mayMean$Depth=='OnLand',c(i,4,5)]
		points(UW1$RKm,UW1[,1],col='blue',pch=1)
			lines(lowess(UW1$RKm,UW1[,1]),lty=i,col='blue')
		points(UW1$RKm,UW2[,1],col='purple',pch=17)
			lines(lowess(UW2$RKm,UW2[,1]),lty=i,col='purple')
		points(UW1$RKm,UW3[,1],col='green',pch=19)
			lines(lowess(UW3$RKm,UW3[,1]),lty=i,col='green')	
		points(OL$RKm,OL[,1],col='red')
			lines(lowess(OL$RKm,OL[,1]),lty=i,col='red')
	}
	legend('topright',legend=c('On land','Underwater1','Underwater2','Underwater3','2015-05-19','2015-05-20','2015-05-21'),lty=c(NA,NA,NA,NA,1:3),pch=c(1,1,17,19,rep(NA,3)),col=c('red','blue','purple','green',rep(1,3)))
	
	
#####Natural log transformations of camps data, spatially looking at light attenuation/extinction. Take snapshot of data, do not look at entire time, e.g.- 1-2 hours of data from day 1 of deployments to avoid travertine blockage, blue spring and chute camp were not deployed until day two so physical blockages like clouds and the like will not be in line with downstream sites#####

		
		

		####make matrix of deployment sites, create a dataframe? or matrix? for each site that light was measured####
		allmay<-do.call(rbind,maydat)
		
		chutemay<-allmay[allmay$Site=='Chute',]
		bluemay<-allmay[allmay$Site=='Blue',]
		saltmay<-allmay[allmay$Site=='Salt',]
		coyotemay<-allmay[allmay$Site=='Coyote',]
		bouldermay<-allmay[allmay$Site=='Boulders',]
		
		
		####select the correct times to measure light attenuation (e.g. 1100am-0100pm) for each site(df)####
		cuttimes<-list()
		cuttimes$Cb<-strptime(c('2015-05-20 12:05:00','2015-05-20 14:05:00'),format='%Y-%m-%d %H:%M:%S')
		cuttimes$Csb<-strptime(c('2015-05-19 12:05:00','2015-05-19 14:05:00'),format='%Y-%m-%d %H:%M:%S')
		
		
		att<-list()
		att$chute<-chutemay[chutemay$DateTime>=cuttimes$Cb[1]&chutemay$DateTime<=cuttimes$Cb[2],]
		att$blue<-bluemay[bluemay$DateTime>=cuttimes$Cb[1]&bluemay$DateTime<=cuttimes$Cb[2],]
		att$salt<-saltmay[saltmay$DateTime>=cuttimes$Csb[1]&saltmay$DateTime<=cuttimes$Csb[2],]
		att$coyot<-coyotemay[coyotemay$DateTime>=cuttimes$Csb[1]&coyotemay$DateTime<=cuttimes$Csb[2],]
		att$bould<-bouldermay[bouldermay$DateTime>=cuttimes$Cb[1]&bouldermay$DateTime<=cuttimes$Cb[2],]
		
		
		
		## take att list and turn into dataframe, easier to work with##
		mayatt<-do.call(rbind,att)
		
		##natural log transformation##
		mayatt$lnLux<-log(mayatt$Lux)
		
		
		##linear regression of adjusted light readings, essentially this is our curve of light extinction##
		Q<-c('Blue','Chute','Salt','Coyote','Boulders')
		outmay<-list()
		for(i in 1:5) {
			t1<-mayatt[mayatt$Site==Q[i]&mayatt$Sensor!=4,]
			
			outmay[[i]]<-lm(t1$lnLux~t1$Depth)
			}
			names(outmay)<-Q
		#plot the data#	
			
		xyplot(lnLux~Depth|Site,data=mayatt)

		#plot the data in parameters set as 2x2 frame, then add abline(regression line), create a plot with our own parameters then add points in to fill the plot,make plots pretty for all the little scientists, big ones too#	
		
		par(mfrow=c(2,3),oma=c(0,0,3,0))
		
		plot(c(0,100),c(7,13),type='n',xlab='depth(cm)',ylab='lnLux',bty='l',main='Blue Spring Rkm=20.75',yaxt='n')
		points(lnLux~Depth,data=mayatt[mayatt$Site=='Blue'&mayatt$Sensor!=4,])
		abline(outmay$Blue)
		text(60,13,paste0('lnLux = ',round(outmay$Blue$coefficients[2],4),'*Depth + ',round(outmay$Blue$coefficients[1],4)))
		text(60,12.5,bquote(r^2*'='*.(round(summary(outmay$Blue)$r.squared,4))))
		axis(2,las=2)
		#a plot window must open before you can mtext it#
		mtext('Little Colorado River Light Attenuation',side=3,outer=T,font=2)
		
		
		plot(c(0,100),c(7,13),type='n',xlab='depth(cm)',ylab='lnLux',bty='l',main='Chute Camp Rkm=16.2',yaxt='n')
		points(lnLux~Depth,data=mayatt[mayatt$Site=='Chute'&mayatt$Sensor!=4,])
		abline(outmay$Chute)
		text(60,13,paste0('lnLux = ',round(outmay$Chute$coefficients[2],4),'*Depth + ',round(outmay$Chute$coefficients[1],4)))
		text(60,12.5,bquote(r^2*'='*.(round(summary(outmay$Chute)$r.squared,4))))
		axis(2,las=2)
		
		plot(c(0,100),c(7,13),type='n',xlab='depth(cm)',ylab='lnLux',bty='l',main='Salt Camp Rkm=10.4',yaxt='n')
		points(lnLux~Depth,data=mayatt[mayatt$Site=='Salt'&mayatt$Sensor!=4,])
		abline(outmay$Salt)
		text(60,13,paste0('lnLux = ',round(outmay$Salt$coefficients[2],4),'*Depth + ',round(outmay$Salt$coefficients[1],4)))
		text(60,12.5,bquote(r^2*'='*.(round(summary(outmay$Salt)$r.squared,4))))
		axis(2,las=2)
		
		plot(c(0,100),c(7,13),type='n',xlab='depth(cm)',ylab='lnLux',bty='l',main='Coyote Camp Rkm=9.0',yaxt='n')
		points(lnLux~Depth,data=mayatt[mayatt$Site=='Coyote'&mayatt$Sensor!=4,])
		abline(outmay$Coyote)
		text(60,13,paste0('lnLux = ',round(outmay$Coyote$coefficients[2],4),'*Depth + ',round(outmay$Coyote$coefficients[1],4)))
		text(60,12.5,bquote(r^2*'='*.(round(summary(outmay$Coyote)$r.squared,4))))
		axis(2,las=2)
		
		plot(c(0,100),c(7,13),type='n',xlab='depth(cm)',ylab='lnLux',bty='l',main='Boulders Camp Rkm=1.95',yaxt='n')
		points(lnLux~Depth,data=mayatt[mayatt$Site=='Boulders'&mayatt$Sensor!=4,])
		abline(outmay$Boulders)
		text(60,13,paste0('lnLux = ',round(outmay$Boulders$coefficients[2],4),'*Depth + ',round(outmay$Boulders$coefficients[1],4)))
		text(60,12.5,bquote(r^2*'='*.(round(summary(outmay$Boulders)$r.squared,4))))
		axis(2,las=2)
		
		plot(c(0,100),c(7,13),type='n',xlab='depth(cm)',ylab='lnLux',bty='l',main='All Camp Slopes',yaxt='n')
		abline(outmay$Blue,col='blue')
		abline(outmay$Chute,col='green')
		abline(outmay$Salt,col='yellow')
		abline(outmay$Coyote,col='orange')
		abline(outmay$Boulders,col='red')
		legend('topright',legend=c('Blue','Chute','Salt','Coyote','Boulders'),col=c('blue','green','yellow','orange','red'),lty=1)
		
		##make a model of the light attenuation gradient from Blue to Salt##
		
		gradmo<-as.data.frame(Q)
		
		#name the only column in gradmo#
		colnames(gradmo)<-'Site'
		
		#add column for river kilometer#
		gradmo$Rkm<-c(20.75,16.2,10.4,9.0,1.95)
		
		#add slopes to create gradient#
		gradmo$slopes<-NA
		
		for(i in 1:dim(gradmo)[1])	{
			gradmo$slopes[i]<-round(outmay[[i]]$coefficients[2],4)
			}
			
		outgrad<-lm(gradmo$slopes~gradmo$Rkm)
	