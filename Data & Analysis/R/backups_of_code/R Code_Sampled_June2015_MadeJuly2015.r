#####June 2015 surface light measurements#####

####Set working directory, read in data####

##Working Directory##

setwd('P:/Biological/Flyco/LCR/Data & Analysis')

## Get data file names
 ## April and May 2015
 
 jun15<-list.files('Data Loggers/HOBO/LIGHT/June_2015',pattern='.csv')
 
 ## Read in data files, cut out useless rows and columns of data, including bad dates
##add useful columns of data for time, site, Date##

jundat<-list()

for(i in 1:length(jun15))  {

	t1<-read.csv(paste0('Data Loggers/HOBO/LIGHT/June_2015/',jun15[i]),skip=1,row.names=1)
	t2<-t1[,1:3]
		colnames(t2)<-c('DateTime','Temp','Lux')
		t2$Date<-as.Date(t2$DateTime,'%m/%d/%y')
	jundat[[i]]<-t2[t2$Date %in% as.Date(c('2015-06-25','2015-06-26','2015-06-27')),]
	}
	names(jundat)<-substr(jun15,5,regexpr('_2015',jun15)-1)
	
	
	##### Get some averages for data to check data quality #####

## Write a function for computing means, medians, maxima by date
fxMean<-function(x) round(tapply(x$Lux,x$Date,mean))

## Look at the means, medians, and maxima by date
junMean<-as.data.frame(t(sapply(jundat,fxMean)))

##add column relating RKm##

junMean$RKm<-as.numeric(substr(rownames(junMean),5,10))

#### Plot April mean lux data by river mile (actually km) ####


## Plot the data##
plot(c(min(junMean$RKm),max(junMean$RKm)),c(min(junMean[,1:3]),max(junMean[,1:3])),type='n',xlab='River km',ylab='Mean lux')
	for(i in 2)  {
	
		
		points(junMean$RKm,junMean[,i],col=i)
			lines(lowess(junMean$RKm,junMean[,i]),lty=i,col=i)
	}
	legend('topright',legend=c('2015-06-25','2015-06-26','2015-06-27'),lty=c(1:3),pch=1,col=1:3)





	
	


