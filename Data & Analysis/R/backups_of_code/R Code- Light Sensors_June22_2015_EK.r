##### Set Working Directory, read in data #####

## Working directory
setwd('P:/Biological/Flyco/LCR/Data & Analysis')

## Get data file names
 ## April and May 2015
may15<-list.files('Data Loggers/HOBO/LIGHT/May_2015',pattern='.csv')
apr15<-list.files('Data Loggers/HOBO/LIGHT/April_2015',pattern='.csv')

## Read in data files, cut out useless rows and columns of data, including bad dates
#find actual depths for light sensors then create the following code: Z<-c(0,x,x,x)#
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
		t2$Date<-as.Date(t2$DateTime,'%m/%d/%y')	
		t2$Time<-strftime(strptime(t2$DateTime,'%m/%d/%y %T'),'%H:%M:%S')
		t2$Sensor<-as.numeric(substr(may15[i],regexpr('-',may15[i])+1,regexpr('-'may15[i])+1))
		#make Z t2$Depth<-Z[t2$Sensor]
		t2$Site<-substr(may15[i],1,regexpr('-',may15[i]-1)
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
	
###Plot May mean lux data by date, adjust points to show the depths, adjust lowess to depth pts.see above code that did this###

## Get MAY data in a more useful format for such a plot ##
#didn't chase the below code too much as I think the above plot tells us the info we want#
mayMean.t<-as.data.frame(cbind(rep(rownames(mayMean),3),rep(colnames(mayMean),each=15),rep(mayRM,each=2),c('Underwater','OnLand'),unlist(,mayMean)))
	rownames(mayMean.t)<-c(1:dim(aprMean.t)[1])
	colnames(mayMean.t)<-c('Sensor','Date','RM','Depth','Lux')
	mayMean.t$Date<-as.Date(mayMean.t$Date)
	mayMean.t$Lux<-as.numeric(as.character(mayMean.t$Lux))
	
#####Natural log transformations of camps data, spatially looking at light attenuation/extinction. Take snapshot of data, do not look at entire time, e.g.- 1-2 hours of data from day 1 of deployments to avoid travertine blockage, blue spring and chute camp were not deployed until day two so physical blockages like clouds and the such will not be in line with downstream sites#####

		###create a column that relates the sensor depths###
		maydat1<- maydat$Depth<-c(maydat[['Blue-1']],colname='Depth',1:i)

		####make matrix of deployment sites, create a dataframe? or matrix? for each site that light was measured####
		
		saltlist<-cbind(maydat[['Salt-1']],maydat[['Salt-2']],maydat[['Salt-3']],maydat[['Salt-4']])
		
		####select the correct times to measure light attenuation (e.g. 1100am-0100pm) for each site(df)####
		
		####create attenuation curve####
		##natural log transformation##
		##linear regression of adjusted light readings, essentially this is our curve of light extinction##
		
		
	
	