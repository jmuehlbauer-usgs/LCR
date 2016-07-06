##### Code for HOBO light sensors #####


##### Load libraries #####

## Create function to check if package is installed. Install it if not and load it regardless.
pkgload <- function(pkg){
	newpkg <- pkg[!(pkg %in% installed.packages()[, 'Package'])]
	if(length(newpkg) > 0){
		install.packages(newpkg, repos = 'http://cran.cnr.berkeley.edu/')
	}
	loaded <- lapply(pkg, require, character.only = TRUE, quietly = TRUE)
}

## Run the function for each package desired
pkgload(c('parallel', 'foreach', 'lubridate'))


##### Read in light sensor data #####

## Set preliminary working directory for getting data (network or local)
setwd('P:/Biological/Flyco/LCR/Data/HOBO/Light/Processed')

## Get light data file names
	## NOTE: Add new lines for new months here, and also at Lines 35 and 126.
samptype <- c('deep', 'deep', 'long', 'deep', 'deep', 'deep')
	names(samptype) <- c('apr15', 'may15', 'jun15', 'apr16', 'may16', 'jun16')
Apr15files <- list.files('2015-04', pattern = '.csv')
May15files <- list.files('2015-05', pattern = '.csv')
Jun15files <- list.files('2015-06', pattern = '.csv')
Apr16files <- list.files('2016-04', pattern = '.csv')
May16files <- list.files('2016-05', pattern = '.csv')
Jun16files <- list.files('2016-06', pattern = '.csv')

## Create dataframes of start-stop sensor deployment times
	## NOTE: Add new lines for new months here.
	## Trim an additional hour off start and stop deployment times just to be safe.
deep <- as.data.frame(c('Blue', 'Chute', 'Salt', 'Coyote', 'Boulders'))
	colnames(deep) <- 'Site'
	deep$Apr15Start <- as.POSIXlt(paste('2015-04-14', c('15:35', '12:10', '09:30', '09:50', '15:30'))) + 3600
	deep$Apr15Stop <- as.POSIXlt(paste('2015-04-16', c('12:31', '10:08', '08:10', '08:15', '11:30'))) - 3600
	deep$May15Start <- as.POSIXlt(paste('2015-05-19', c('09:48', '08:00', '11:40', '09:30', '14:00'))) + 3600
	deep$May15Stop <- as.POSIXlt(paste('2015-05-22', c('12:44', '14:00', '19:00', '13:10', '13:10'))) - 3600
		## Adjust removal dates for some May15 traps (left in the field for a while)
		deep[deep$Site %in% c('Coyote', 'Boulders'), 'May15Stop'] <- c(as.POSIXlt('2015-05-24 23:55') - 0, as.POSIXlt('2015-06-25 13:10') - 3600)
	deep$Apr16Start <- as.POSIXlt(paste('2016-04-19', c('17:10', '14:20', '10:20', '11:00', '15:15'))) + 3600
	deep$Apr16Stop <- as.POSIXlt(paste('2016-04-23', c('10:55', '09:00', '14:00', '11:50', '16:00'))) - 3600
		## Adjust removal dates for some traps (LCR intensive trip, some variation in trip length)
		deep[deep$Site %in% c('Chute', 'Boulders'), 'Apr16Stop'] <- deep[deep$Site %in% c('Chute', 'Boulders'), 'Apr16Stop'] - 24 * 3600
	deep$May16Start <- as.POSIXlt(paste('2016-05-17', c('18:00', '14:30', '10:30', '10:37', '15:34'))) + 3600
	deep$May16Stop <- as.POSIXlt(paste('2016-05-19', c('13:45', '10:00', '07:00', '08:12', '11:55'))) - 3600
	deep$Jun16Start <- as.POSIXlt(paste('2016-06-24', c('16:30', '13:45', '09:00', '09:50', '17:00'))) + 3600
	deep$Jun16Stop <- as.POSIXlt(paste('2016-06-26', c('10:35', '08:27', '06:00', '17:00', '12:20'))) - 3600
long <- as.data.frame(substr(Jun15files, 1, regexpr('_', Jun15files) - 1))
	colnames(long) <- 'Site'
	long$Jun15Start <- as.POSIXlt(paste('2015-06-25', c('15:15', '14:15', '13:10', '12:20', '11:30', '10:55', '09:30', '09:27', '11:05', '11:40', '12:35', '13:15', '13:40', '16:09', '17:05', '17:30', '18:05'))) + 3600
	long$Jun15Stop <- as.POSIXlt(paste('2015-06-27', c('10:30', '10:00', '09:20', '09:00', '08:30', '08:05', '06:00', '06:43', '07:35', '08:05', '08:43', '09:06', '09:26', '10:33', '11:56', '12:19', '12:50'))) - 3600
		## Adjust removal dates for RKM19 trap (flipped over)
		long[long$Site == 'Rkm19.0', 'Jun15Stop'] <- as.POSIXlt('2015-06-26 23:55') - 0

## Create dataframe of sensor depths and site Rkms
sensor <- as.data.frame(cbind(1:4, c(0, .35, .65, .84)))
	colnames(sensor) <- c('Sensor', 'Depth')
siterkm <- as.data.frame(deep$Site)
	colnames(siterkm) <- 'Site'
	siterkm$Rkm <- c(20.75, 16.2, 10.4, 9.0, 1.95)
	
## Create function to read in data
filefx <- function(X,files) {
	filename <- deparse(substitute(files))
	yr <- paste0('20', substr(filename, 4, 5))
	montmp <- grep(substr(filename, 1, 3), month.abb)
	mon <- ifelse(substr(montmp, 1, 1) != '1', paste0('0', montmp), montmp)
	yrmon <- paste0(yr, '-', mon)
	samp <- substr(filename, 1, 5)
	mysite <- substr(files[X], 1, regexpr('_', files[X]) - 1)
	t1 <- read.csv(paste0(yrmon, '/', files[X]), skip = 1, row.names = 1)
	t2 <- t1[,1:3]
		colnames(t2) <- c('POSIX', 'Temp', 'Lux')
		t2$POSIX <- as.POSIXlt(t1[,1],format='%m/%d/%y %I:%M:%S %p', tz = 'America/Phoenix')
		t2$Temp <- round(t2$Temp, 2)
		t2$Lux <- round(t2$Lux)
	if(substr(files[X],1,3) == 'Rkm'){
		t2$Site <- mysite
		t2$Rkm <- as.numeric(substr(mysite, 4, 8))
		t2$Depth <- 0
		date1 <- long[long$Site == mysite, paste0(samp, 'Start')]
		date2 <- long[long$Site == mysite, paste0(samp, 'Stop')]		
	} else{
		mysite2 <- substr(mysite, 1, 4)
		t2$Site <- siterkm[match(mysite2, substr(siterkm$Site, 1, 4)),'Site']
		t2$Rkm <- siterkm[substr(siterkm$Site, 1, 4) == mysite2, 'Rkm']
		date1 <- deep[substr(deep$Site,1, 4) == mysite2, paste0(samp, 'Start')]
		date2 <- deep[substr(deep$Site,1, 4) == mysite2, paste0(samp, 'Stop')]
		if(regexpr('-', mysite) > 0) {
			sensnum <- as.numeric(substr(files[X], regexpr('-', mysite)[1] + 1, regexpr('-', mysite)[1] + 1))
			t2$Depth <- sensor[sensor$Sensor == sensnum, 'Depth']
		} else{
			t2$Depth <- ifelse(nchar(mysite) > 8, 0, .3)
		}
	}
	int <- new_interval(date1, date2)
	t3 <- t2[t2$POSIX %within% int,]
}

## Make element names of monthly lists more descriptive
indivname <- function(x) {
	deparse(substitute(x))
	filename2 <- deparse(substitute(x))
	fileupper <- paste0(toupper(substr(filename2, 1, 1)), substr(filename2, 2, 5), 'files')
	filename3 <- get(fileupper)
	substr(filename3, 1, regexpr('_', filename3) - 1)
}

## Make element names of consolidated list more descriptive
listnames <- function(X) {
	first <- X$POSIX[1]
	paste0(tolower(month.abb[month(first)]), substr(year(first), 3, 4))
}

## Make socket clusters for running lapply operations in parallel
cl<-makeCluster(getOption("cl.cores",detectCores()-1))
	clusterExport(cl = cl, varlist = ls())
	cllibs <- clusterEvalQ(cl, c(library(lubridate), library(foreach)))

## Read in data, by month
	## NOTE: Add new lines for new months here.
apr15 <- parLapply(cl, 1:length(Apr15files), function(x) filefx(X = x, files = Apr15files))
	names(apr15) <- indivname(apr15)
may15 <- parLapply(cl, 1:length(May15files), function(x) filefx(X = x, files = May15files))
	names(may15) <- indivname(may15)
jun15 <- parLapply(cl, 1:length(Jun15files), function(x) filefx(X = x, files = Jun15files))
	names(jun15) <- indivname(jun15)
apr16 <- parLapply(cl, 1:length(Apr16files), function(x) filefx(X = x, files = Apr16files))
	names(apr16) <- indivname(apr16)
may16 <- parLapply(cl, 1:length(May16files), function(x) filefx(X = x, files = May16files))
	names(may16) <- indivname(may16)
jun16 <- parLapply(cl, 1:length(Jun16files), function(x) filefx(X = x, files = Jun16files))
	names(jun16) <- indivname(jun16)

## Condense each sample month into a single dataframe
alls <- parLapply(cl, list(apr15, may15, jun15, apr16, may16), function(x) do.call('rbind', x))
	names(alls) <- parSapply(cl, alls, function(x) listnames(x))

## Set the working directory to a higher level
setwd('P:/Biological/Flyco/LCR')


##### Get light attenuation coefficients #####

## Cut out any rows of data with 0 lux (nighttime, or bad readings), take log
no0 <- function(mylist){
	mylist2 <- mylist[mylist$Lux > 0, ]
	mylist2$logLux <- round(log(mylist2$Lux), 2)
	print(mylist2)
}
clusterExport(cl, varlist = 'no0')
alls1 <- parLapply(cl, alls, function(x) no0(x))

## Limit dataset to only samples with multiple depth measurements
	## Restrict apr15 dataset to only times and places with both a surface and subsurface measurement
	## Restrict light sensor array datasets to only times and places with at least 4 measurements
	## For longitudinal datasets, reduce to only times and places with at least a single measurement
restrict <- function(mylist){
	mysums <- as.data.frame(table(as.character(mylist$POSIX),mylist$Site))
		colnames(mysums) <- c('POSIX', 'Site', 'Freq')
	yrmon <- paste(year(mylist$POSIX[1]), month(mylist$POSIX[1]), sep = '-')
	if(yrmon == '2015-4'){
		mysums2 <- mysums[mysums$Freq == 2, ]
	} else{
		site <- substr(mysums$Site[1], 1, 3)
		if(site == 'Rkm'){
			mysums2 <- mysums[mysums$Freq == 1, ]
 		} else{
			mysums2 <- mysums[mysums$Freq == 4, ]
		}
	}
	timesiteall <- paste(mylist$POSIX, mylist$Site)
	timesitesub <- paste(mysums2$POSIX, mysums2$Site)
	mylist2 <- mylist[timesiteall %in% timesitesub, ]
}
clusterExport(cl, varlist = 'restrict')
alls2 <- parLapply(cl, alls1, function(x) restrict(x))

## Subset only light sensor array (underwater apparatus) data, ignoring surface longitudinal sets
deep <- alls2[names(alls2) %in% names(which(samptype=='deep'))]

## Run a linear regression for every site-time combination
lregall<- function(mylist){
	sitetimeall <- paste(mylist$Site, mylist$POSIX)
	sitetimeunq <- unique(sitetimeall)
	lreg <- function(sitetimes){
		dat <- mylist[sitetimeall %in% sitetimeunq[i],]
		out1 <- lm(dat$logLux ~ dat$Depth)
			tab <- dat[1, c('POSIX', 'Site', 'Rkm')]
				coeff <- round(t(out1$coefficients), 4)
					colnames(coeff) <- c('Intercept', 'Slope')
				tab2 <- cbind(tab, coeff)
				tab2$R2 <- round(summary(out1)$adj.r.squared, 4)
		print(tab2)
	}
	mydat <- foreach(i = 1:length(sitetimeunq), .combine = rbind) %dopar% lreg(i)
}
clusterExport(cl, varlist = 'lregall')
regs <- parLapply(cl, deep, function(x) lregall(x))

## Funciton to plot R2s by sample event, by site, over time
plotR2 <- function(mylist){
	plot(mylist$R2 ~ as.POSIXct(mylist$POSIX), type = 'n', xlab = 'Time', ylab = 'R2')
		Bluepts <- mylist[mylist$Site == 'Blue',]
			points(Bluepts$R2 ~ as.POSIXct(Bluepts$POSIX), col = 4)
		Chutepts <- mylist[mylist$Site == 'Chute',]
			points(Chutepts$R2 ~ as.POSIXct(Chutepts$POSIX), col = 5)
		Saltpts <- mylist[mylist$Site == 'Salt',]
			points(Saltpts$R2 ~ as.POSIXct(Saltpts$POSIX), col = 3)
		Coyotepts <- mylist[mylist$Site == 'Coyote',]
			points(Coyotepts$R2 ~ as.POSIXct(Coyotepts$POSIX), col = 6)
		Boulderspts <- mylist[mylist$Site == 'Boulders',]
			points(Boulderspts$R2 ~ as.POSIXct(Boulderspts$POSIX), col = 2)
		legend('bottomleft', legend = c('Blue', 'Chute', 'Salt', 'Coyote', 'Boulders'), col = c(4, 5, 3, 6, 2), pch = 1)
}

## Function to plot k by sample event, by site, over time
plotk <- function(mylist){
	plot(mylist$Slope ~ as.POSIXct(mylist$POSIX), type = 'n', xlab = 'Time', ylab = 'Slope')
		Bluepts <- mylist[mylist$Site == 'Blue',]
			points(Bluepts$Slope ~ as.POSIXct(Bluepts$POSIX), col = 4)
		Chutepts <- mylist[mylist$Site == 'Chute',]
			points(Chutepts$Slope ~ as.POSIXct(Chutepts$POSIX), col = 5)
		Saltpts <- mylist[mylist$Site == 'Salt',]
			points(Saltpts$Slope ~ as.POSIXct(Saltpts$POSIX), col = 3)
		Coyotepts <- mylist[mylist$Site == 'Coyote',]
			points(Coyotepts$Slope ~ as.POSIXct(Coyotepts$POSIX), col = 6)
		Boulderspts <- mylist[mylist$Site == 'Boulders',]
			points(Boulderspts$Slope ~ as.POSIXct(Boulderspts$POSIX), col = 2)
		legend('bottomleft', legend = c('Blue', 'Chute', 'Salt', 'Coyote', 'Boulders'), col = c(4, 5, 3, 6, 2), pch = 1)
}

## Actually plot all these
plotk(regs$apr15)
plotR2(regs$may15); windows()
plotk(regs$may15)
plotR2(regs$apr16); windows()
plotk(regs$apr16)
plotR2(regs$may16); windows()
plotk(regs$may16)
### STOPPED HERE. Need to investigate these plots more. Blue and Chute in May16 seemed to suck, for instance. Sensors getting bent, maybe? Possibly remove offending sensors from dataset?

## Close the clusters
stopCluster(cl)





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