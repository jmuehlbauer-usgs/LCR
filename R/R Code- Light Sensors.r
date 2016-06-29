##### Code for HOBO light sensors #####


##### Load libraries #####

## Load parallel and lubridate libraries, for running apply operations in parallel and dealing with datetimes
library(parallel)
library(lubridate)


##### Read in light sensor data #####

## Set preliminary working directory for getting data (network or local)
setwd('P:/Biological/Flyco/LCR/Data/HOBO/Light/Processed')

## Get light data file names
	## NOTE: Add new lines for new months here, and also at Lines 25 and 93.
Apr15files <- list.files('2015-04', pattern = '.csv')
May15files <- list.files('2015-05', pattern = '.csv')
Jun15files <- list.files('2015-06', pattern = '.csv')
Apr16files <- list.files('2016-04', pattern = '.csv')
May16files <- list.files('2016-05', pattern = '.csv')
Jun16files <- list.files('2016-06', pattern = '.csv')

## Create dataframes of start-stop sensor deployment times
	## Trim an additional hour off start and stop deployment times just to be safe.
deep <- as.data.frame(c('Blue', 'Chute', 'Salt', 'Coyote', 'Boulders'))
	colnames(deep) <- 'Site'
	deep$Apr15Start <- as.POSIXlt(paste('2015-04-14', c('15:35', '12:10', '09:30', '09:50', '15:30'))) + 3600
	deep$Apr15Stop <- as.POSIXlt(paste('2015-04-16', c('12:31', '10:08', '08:10', '08:15', '11:30'))) - 3600
	deep$May15Start <- as.POSIXlt(paste('2015-05-19', c('09:48', '08:00', '11:40', '09:30', '14:00'))) + 3600
	deep$May15Stop <- as.POSIXlt(paste('2015-05-22', c('12:44', '14:00', '19:00', '13:10', '13:10'))) - 3600
		## Adjust removal dates for some May15 traps (left in the field for a while)
		deep[deep$Site == 'Coyote', 'May15Stop'] <- as.POSIXlt('2015-05-24 23:55') - 0
		deep[deep$Site == 'Boulders', 'May15Stop'] <- as.POSIXlt('2015-06-25 13:10') - 3600
	deep$Apr16Start <- as.POSIXlt(paste('2016-04-19', c('17:10', '14:20', '10:20', '11:00', '15:15'))) + 3600
	deep$Apr16Stop <- as.POSIXlt(paste('2016-04-23', c('10:55', '12:30', '14:00', '11:50', '16:00'))) - 3600
		## Adjust removal dates for some traps (LCR intensive trip, some variation in trip length)
		deep[c(1,5), 'Apr16Stop'] <- as.POSIXlt(deep[c(1,5), 'Apr16Stop']) - 24 * 3600		
	deep$May16Start <- as.POSIXlt(paste('2016-05-17', c('18:00', '14:30', '10:30', '10:37', '15:34'))) + 3600
	deep$May16Stop <- as.POSIXlt(paste('2016-05-19', c('13:45', '10:00', '07:00', '08:12', '11:55'))) - 3600
	#deep$Jun16Start <- as.POSIXlt(paste('2016-06-24', c('16:30', '13:45', '09:00', ))) + 3600
	#deep$Jun16Stop <- as.POSIXlt(paste('2016-06-26', c('10:35', '08:27', '06:00', ))) - 3600
long <- as.data.frame(substr(Jun15files, 1, regexpr('_', Jun15files) - 1))
	colnames(long) <- 'Site'
	long$Jun15Start <- as.POSIXlt(paste('2015-06-25', c('15:15', '14:15', '13:10', '12:20', '11:30', '10:55', '09:30', '09:27', '11:05', '11:40', '12:35', '13:15', '13:40', '16:09', '17:05', '17:30', '18:05'))) + 3600
	long$Jun15Stop <- as.POSIXlt(paste('2015-06-27', c('10:30', '10:00', '09:20', '09:00', '08:30', '08:05', '06:00', '06:43', '07:35', '08:05', '08:43', '09:06', '09:26', '10:33', '11:56', '12:19', '12:50'))) - 3600
		## Adjust removal dates for RKM19 trap (flipped over)
		long[long$Site == 'Rkm19.0', 'Jun15Stop'] <- as.POSIXlt('2015-06-26 23:55') - 0

## Create dataframe of sensor depths
sensor <- as.data.frame(cbind(1:4, c(0, .35, .75, 1.15)))
	colnames(sensor) <- c('Sensor', 'Depth')

## Create function to read in data
filefx <- function(X,files) {
	filename <- deparse(substitute(files))
	yr <- paste0('20', substr(filename, 4, 5))
	montmp <- grep(substr(filename, 1, 3), month.abb)
	mon <- ifelse(substr(montmp, 1, 1) != '1', paste0('0', montmp), montmp)
	yrmon <- paste0(yr, '-', mon)
	t1 <- read.csv(paste0(yrmon, '/', files[X]), skip = 1, row.names = 1)
	t2 <- t1[,1:3]
		colnames(t2) <- c('POSIX', 'Temp', 'Lux')
		t2$POSIX <- as.POSIXlt(t1[,1],format='%m/%d/%y %I:%M:%S %p')
		t2$Temp <- round(t2$Temp, 2)
		t2$Lux <- round(t2$Lux)
	samp <- substr(filename, 1, 5)
	mysite <- substr(files[X], 1, regexpr('_', files[X]) - 1)
	if(substr(files[X],1,3) == 'Rkm'){
		t2$Depth <- 0
		date1 <- long[long$Site == mysite, paste0(samp, 'Start')]
		date2 <- long[long$Site == mysite, paste0(samp, 'Stop')]		
	} else{
		mysite2 <- substr(mysite, 1, 4)
		date1 <- deep[substr(deep$Site,1, 4) == mysite2, paste0(samp, 'Start')]
		date2 <- deep[substr(deep$Site,1, 4) == mysite2, paste0(samp, 'Stop')]
		if(regexpr('-', mysite) > 0) {
			sensnum <- as.numeric(substr(files[X], (regexpr('-', mysite) + 1)[1], (regexpr('-', mysite) + 1)[1]))
			t2$Depth <- sensor[sensor$Sensor == sensnum, 'Depth']
		} else{
			t2$Depth <- ifelse(nchar(mysite) > 8, 1, 0)
		}
	}
	### Stopped here. Need to unlist sites within each sample month (BlueSurface with Blue, Blue-1, 2, 3, 4 together, etc.)
	int <- new_interval(date1, date2)
	t3 <- t2[t2$POSIX %within% int,]
}

## Make list element names more descriptive
listname <- function(x) {
	deparse(substitute(x))
	filename2 <- deparse(substitute(x))
	fileupper <- paste0(toupper(substr(filename2, 1, 1)), substr(filename2, 2, 5), 'files')
	filename3 <- get(fileupper)
	substr(filename3, 1, regexpr('_', filename3) - 1)
}

## Make socket clusters for running lapply operations in parallel
	## NOTE: Add new lines for new months here.
cl<-makeCluster(getOption("cl.cores",detectCores()-1))
clusterExport(cl = cl, varlist = objects())
cllibs <- clusterEvalQ(cl, library(lubridate))

## Read in data, by month
	## NOTE: Add new lines for new months here.
apr15 <- parLapply(cl, 1:length(Apr15files), function(x) filefx(X = x, files = Apr15files))
	names(apr15) <- listname(apr15)
may15 <- parLapply(cl, 1:length(May15files), function(x) filefx(X = x, files = May15files))
	names(may15) <- listname(may15)
jun15 <- parLapply(cl, 1:length(Jun15files), function(x) filefx(X = x, files = Jun15files))
	names(jun15) <- listname(jun15)
apr16 <- parLapply(cl, 1:length(Apr16files), function(x) filefx(X = x, files = Apr16files))
	names(apr16) <- listname(apr16)
may16 <- parLapply(cl, 1:length(May16files), function(x) filefx(X = x, files = May16files))
	names(may16) <- listname(may16)
#jun16 <- parLapply(cl, 1:length(Jun16files), function(x) filefx(X = x, files = Jun16files))
#	names(jun16) <- listname(jun16)

## Close the clusters
stopCluster(cl)

## Set the working directory to a higher level
setwd('P:/Biological/Flyco/LCR')


##### Get light attenuation coefficients #####

## Condense light sensor arrays by site
a15 <- 








##### OLD CODE BELOW #####

##### Get some averages for data to check data quality #####

## Write a function for computing means, medians, maxima by date
fxMean<-function(x) round(tapply(x$Lux,x$Date,mean))

## Look at the means, medians, and maxima by date
aprMean<-as.data.frame(t(sapply(aprdat,fxMean)))
mayMean<-(sapply(maydat,fxMean))
apr6Mean<-as.data.frame(t(sapply(apr6dat,fxMean)))


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

apr6Mean$RKm<-rep(c(21,2.0,16.2,9.0,10.4),c(4,4,4,4,4))
apr6Mean$Depth<-rep(as.factor(c('OnLand','Underwater1','Underwater2','Underwater3')),5)
##Plot##
plot(c(min(apr6Mean$RKm),max(apr6Mean$RKm)),c(min(apr6Mean[,1:3]),max(apr6Mean[,1:3])),type='n',xlab='River km',ylab='Mean lux')
	for(i in 1:3){
		UW1<-apr6Mean[apr6Mean$Depth=='Underwater1',c(i,4,5)]
		UW2<-apr6Mean[apr6Mean$Depth=='Underwater2',c(i,4,5)]
		UW3<-apr6Mean[apr6Mean$Depth=='Underwater3',c(i,4,5)]
		OL<-apr6Mean[apr6Mean$Depth=='OnLand',c(i,4,5)]
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