### Rather than averaging 2 hours of data points taken every 5 minutes draw regressions for 5 minute points so that OL, UW1, UW2, UW3 are all drawn (e.g.- 1305, 1305, 1305, 1305) which will give you a r^2 value very  very close to 1.0 which is ok for the moment. Then write a code to average the regressions, this will give a better attenuation line. This form of analysis assumes that the angle of the light is a nominal factor since the sensors are set up at a 90degree angle relative to gravity of Earth###

	##May 2015 data using the Light Attenuation Doohicky Thingy's that are still down in the little C##
	
	####select the correct times to measure light attenuation (e.g. 1100am-0100pm) for each site(df)####
		cuttimes<-list()
		cuttimes$Cb1<-strptime('2015-05-20 12:05:00',format='%Y-%m-%d %H:%M:%S')
		cuttimes$Csb1<-strptime('2015-05-19 12:05:00',format='%Y-%m-%d %H:%M:%S')
		cuttimes$Cb2<-strptime('2015-05-20 13:05:00',format='%Y-%m-%d %H:%M:%S')
		cuttimes$Csb2<-strptime('2015-05-19 13:05:00',format='%Y-%m-%d %H:%M:%S')
		cuttimes$Cb3<-strptime('2015-05-20 15:05:00',format='%Y-%m-%d %H:%M:%S')
		cuttimes$Csb3<-strptime('2015-05-19 15:05:00',format='%Y-%m-%d %H:%M:%S')
		
		
		att<-list()
		att$chute1<-chutemay[chutemay$DateTime>=cuttimes$Cb1[1],]
		att$chute2<-chutemay[chutemay$DateTime>=cuttimes$Cb2[1],]
		att$chute3<-chutemay[chutemay$DateTime>=cuttimes$Cb3[1],]
		att$blue1<-bluemay[bluemay$DateTime>=cuttimes$Cb1[1],]
		att$blue2<-bluemay[bluemay$DateTime>=cuttimes$Cb2[1],]
		att$blue3<-bluemay[bluemay$DateTime>=cuttimes$Cb3[1],]
		att$salt1<-saltmay[saltmay$DateTime>=cuttimes$Csb1[1],]
		att$salt2<-saltmay[saltmay$DateTime>=cuttimes$Csb2[1],]
		att$salt3<-saltmay[saltmay$DateTime>=cuttimes$Csb3[1],]
		att$coyot1<-coyotemay[coyotemay$DateTime>=cuttimes$Csb1[1],]
		att$coyot2<-coyotemay[coyotemay$DateTime>=cuttimes$Csb2[1],]
		att$coyot3<-coyotemay[coyotemay$DateTime>=cuttimes$Csb3[1],]
		att$bould1<-bouldermay[bouldermay$DateTime>=cuttimes$Cb1[1],]
		att$bould2<-bouldermay[bouldermay$DateTime>=cuttimes$Cb2[1],]
		att$bould3<-bouldermay[bouldermay$DateTime>=cuttimes$Cb3[1],]
		
		
		
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

	