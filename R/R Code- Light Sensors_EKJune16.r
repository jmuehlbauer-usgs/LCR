##### Set Working Directory, read in data #####

## Working directory
setwd('P:/Biological/Flyco/LCR/Data/')

## Get data file names
 ## April and May 2015
may15<-list.files('Data Loggers/HOBO/LIGHT/2015-05',pattern='.csv')
apr15<-list.files('Data Loggers/HOBO/LIGHT/2015-04',pattern='.csv')
jun15<-list.files('Data Loggers/HOBO/LIGHT/2015-06',pattern='.csv')
apr16<-list.files('Data Loggers/HOBO/LIGHT/2016-04',pattern='.csv')
may16<-list.files('Data Loggers/HOBO/LIGHT/2016-05',pattern='.csv')
#jun16<-list.files('Data Loggers/HOBO/LIGHT/April_2016',pattern='.csv')

## Read in data files, cut out useless rows and columns of data, including bad dates
aprdat15<-list()
maydat15<-list()
jundat15<-list()
aprdat16<-list()
maydat16<-list()
for(i in 1:length(apr15)){
	t1<-read.csv(paste0('Data Loggers/HOBO/LIGHT/2015-04/',apr15[i]),skip=1,row.names=1)
	t2<-t1[,1:3]
		colnames(t2)<-c('DateTime','Temp','Lux')
		t2$Date<-as.Date(t2$DateTime,'%m/%d/%y')
	aprdat15[[i]]<-t2[t2$Date %in% as.Date(c('2015-04-14','2015-04-15','2015-04-16')),]
	}
	names(aprdat15)<-substr(apr15,1,regexpr('0',apr15)-1)
for(i in 1:length(may15)){
	t1<-read.csv(paste0('Data Loggers/HOBO/LIGHT/2015-05/',may15[i]),skip=1,row.names=1)
	t2<-t1[,1:3]
		colnames(t2)<-c('DateTime','Temp','Lux')
		t2$Date<-as.Date(t2$DateTime,'%m/%d/%y')		
	maydat15[[i]]<-t2[t2$Date %in% as.Date(c('2015-05-19','2015-05-20','2015-05-21')),]
	}
	names(maydat15)<-substr(may15,1,regexpr('_',may15)-1)
for(i in 1:length(apr16)){
	t1<-read.csv(paste0('Data Loggers/HOBO/LIGHT/2016-04/',apr16[i]),skip=1,row.names=1)
	t2<-t1[,1:3]
		colnames(t2)<-c('DateTime','Temp','Lux')
		t2$Date<-as.Date(t2$DateTime,'%m/%d/%y')
	aprdat16[[i]]<-t2[t2$Date %in% as.Date(c('2016-04-19','2016-04-20','2016-04-21')),]
	}
	names(aprdat16)<-substr(apr16,1,regexpr('0',apr16)-1)	
for(i in 1:length(may16)){
	t1<-read.csv(paste0('Data Loggers/HOBO/LIGHT/2016-05/',may16[i]),skip=1,row.names=1)
	t2<-t1[,1:3]
		colnames(t2)<-c('DateTime','Temp','Lux')
		t2$Date<-as.Date(t2$DateTime,'%m/%d/%y')
	maydat16[[i]]<-t2[t2$Date %in% as.Date(c('2016-05-17','2016-05-18','2016-05-19')),]
	}
	names(maydat16)<-substr(may16,1,regexpr('0',may16)-1)	

	
##### Get some averages for data to check data quality #####

## Write a function for computing means, medians, maxima by date
fxMean<-function(x) round(tapply(x$Lux,x$Date,mean))

## Look at the means, medians, and maxima by date
aprMean<-as.data.frame(t(sapply(aprdat15,fxMean)))
mayMean<-(sapply(maydat15,fxMean))
apr16Mean<-as.data.frame(t(sapply(aprdat16,fxMean)))
may16Mean<-as.data.frame(t(sapply(maydat16,fxMean)))




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
xyplot(Lux~Date|Sensor,data=aprMean.t,type='o')

### Eric: This gives you some basic examples to work with. You may want to try and do the same for April medians, maxima, etc. YOu may also want to play around with the May data in some sensible fashion. Note that the last line of code relies on the lattice library (called near the top of the code), which is a common one you'll probably want. I happen to prefer using the base functions because I don't like being constrained by lattice, but that's just me. Also note that I ended up using the function sapply instead of lapply to get the means and such, as this ended up being just a bit cleaner. I also broke out the function we're running within apply into its own line of code to hopefully make what I did a little easier for you to understand. Finally, I modified the for() loop a bit to cut out the useless dates and so on. Enjoy!


#### Erics attempt to work with May Data below ####
## transpose the mayMean##
mayMean<-as.data.frame(t(sapply(maydat,fxMean)))

## add columns to mayMean for RM and Depth##
##create vectors for rKm and sensor depth specifically for may data##

mayMean$RKm<-rep(c(21,16.2,9.0,10.4),c(4,4,3,4))
mayMean$Depth<-as.factor(c('OnLand','Underwater1','Underwater2','Underwater3','OnLand','Underwater1','Underwater2',
	'Underwater3','Underwater1','Underwater2','Underwater3','OnLand','Underwater1','Underwater2','Underwater3'))
##Plot##
plot(c(min(mayMean$RKm),max(mayMean$RKm)),c(min(mayMean[,1:3]),max(mayMean[,1:3])),type='n',xlab='River km',ylab='Mean lux')
	for(i in 1:3){
		UW1<-mayMean[mayMean$Depth=='Underwater1',c(i,4,5)]
		UW2<-mayMean[mayMean$Depth=='Underwater2',c(i,4,5)]
		UW3<-mayMean[mayMean$Depth=='Underwater3',c(i,4,5)]
		OL<-mayMean[mayMean$Depth=='OnLand',c(i,4,5)]
		points(UW$RKm,UW[,1],col='blue')
			lines(lowess(UW$RKm,UW[,1]),lty=i,col='blue')
		points(OL$RKm,OL[,1],col='red')
			lines(lowess(OL$RKm,OL[,1]),lty=i,col='red')
	}
	legend('topright',legend=c('On land','Underwater','2015-05-19','2015-05-20','2015-05-21'),lty=c(NA,NA,1:3),pch=c(1,1,rep(NA,3)),col=c('red','blue',rep(1,3)))

## 2016 April add columns to mayMean for RM and Depth##
##create vectors for rKm and sensor depth specifically for may data##

apr16Mean$RKm<-rep(c(21,2.0,16.2,9.0,10.4),c(4,4,4,4,4))
apr16Mean$Depth<-rep(as.factor(c('OnLand','Underwater1','Underwater2','Underwater3')),5)
##Plot##
plot(c(min(apr16Mean$RKm),max(apr16Mean$RKm)),c(min(apr16Mean[,1:3]),max(apr16Mean[,1:3])),type='n',xlab='River km',ylab='Mean lux')
	for(i in 1:3){
		UW1<-apr16Mean[apr16Mean$Depth=='Underwater1',c(i,4,5)]
		UW2<-apr16Mean[apr16Mean$Depth=='Underwater2',c(i,4,5)]
		UW3<-apr16Mean[apr16Mean$Depth=='Underwater3',c(i,4,5)]
		OL<-apr16Mean[apr16Mean$Depth=='OnLand',c(i,4,5)]
		points(UW1$RKm,UW1[,1],col='blue')
			lines(lowess(UW1$RKm,UW1[,1]),lty=i,col='blue')
		points(UW2$RKm,UW2[,1],col='blue')
			lines(lowess(UW2$RKm,UW2[,1]),lty=i,col='blue')	
		points(UW3$RKm,UW3[,1],col='blue')
			lines(lowess(UW3$RKm,UW3[,1]),lty=i,col='blue')	
		points(OL$RKm,OL[,1],col='red')
			lines(lowess(OL$RKm,OL[,1]),lty=i,col='red')
	}
	legend('topright',legend=c('On land','Underwater','2016-04-19','2016-04-20','2016-04-21'),lty=c(NA,NA,1:3),pch=c(1,1,rep(NA,3)),col=c('red','blue',rep(1,3)))	
###Plot May mean lux data by date, adjust points to show the depths, adjust lowess to depth pts###

## Get MAY data in a more useful format for such a plot ##
mayMean.t<-as.data.frame(cbind(rep(rownames(mayMean),3),rep(colnames(mayMean),each=15),rep(mayRM,each=2),c('Underwater','OnLand'),unlist(,mayMean)))
	rownames(mayMean.t)<-c(1:dim(aprMean.t)[1])
	colnames(mayMean.t)<-c('Sensor','Date','RM','Depth','Lux')
	mayMean.t$Date<-as.Date(mayMean.t$Date)
	mayMean.t$Lux<-as.numeric(as.character(mayMean.t$Lux))
	
	##