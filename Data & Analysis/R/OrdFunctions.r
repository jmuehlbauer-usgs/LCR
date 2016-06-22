## Various functions for preparing a matrix for ordination and running statistics post-ordination
	## remove rare species, relativize by species max or sample sum, calculate row and column cv, calculate ordination Axis R^2s, rotate ordination axes.
## J. D. Muehlbauer, 23 January 2015

##### Function calls #####
## del.rare<-function(x) 
## rel<-function(x,byrow=F,bycol=T)
## cv<-function(x)
## AxisR2<-function(x,ord,dist.method='bray')
## ord.rotate<-function(ord,env.fit,env.vector,x.axis=TRUE,positive=TRUE,flip=FALSE) 

##### Input Variables #####	
## x: A matrix (i.e, the primary matrix, in PC-Ord language).  All rows and columns must be numeric.
## ord: The ordination output (i.e., the points dataframe).
## byrow: Relativize by row (samples)?  Default is FALSE.
## bycol: Relativize by columns (species)?  Default is TRUE
## ord: A dataframe containing ordination points
## env.fit: An envfit result object
## env.vector: The environmental vector to rotate the ordination about (must be one of the vector names from env.fit)
## x.axis: Should the ordination be rotated along the x (TRUE) or y (FALSE) axes? Default is TRUE.
## positive: Should the environmental variable be rotated so it is positive (TRUE) or negative (FALSE) along the axis?  Default is TRUE.
## flip: Should the ordination also be flipped 180 degrees around the other axis?  Default is FALSE.


##### Delete rare species (columns occurring in <5% of rows--samples) #####
## Also delete rows that become empty as a result of rare species deletion

del.rare<-function(x) 
{
	spp.presence<-vector()
	for(i in 1:dim(x)[2]){
		spp.presence[[i]]<-length(grep(TRUE,x[,i]>0,value=FALSE))
		}
	cols<-grep(TRUE,spp.presence>dim(x)[1]*.05)
	new.x0<-x[,c(cols)]
	new.x<-new.x0[grep(TRUE,rowSums(new.x0)>0),]
new.x
}


##### Relativize x by row sum, column maximum or both. #####
## SCG 01/01/2001.  From Dean Urban's class.  See also scale()

rel<-function(x,byrow=F,bycol=T)
{
	if(byrow){
		rsum<-apply(x,1,sum)
		rsum[rsum==0]<- 1
		new.x<-sweep(x,1,rsum,"/")
	}
	if(bycol){
		cmax<-apply(x,2,max)
		cmax[cmax==0]<-1
		new.x<-sweep(x,2,cmax,"/")
	}
new.x		
}


##### Calculate coefficient of variation #####

cv<-function(x)
{
	cvs<-as.data.frame(matrix(nrow=2,ncol=1))
		cvs[1,]<-round(100*sd(rowSums(x))/mean(rowSums(x)),2)
		cvs[2,]<-round(100*sd(colSums(x))/mean(colSums(x)),2)
	rownames(cvs)<-c('Row','Column')
	colnames(cvs)<-'CV (%)'
cvs	
}

##### Compute percent of variation in ordination data explained by each ordination axis #####
## For 2D or 3D ordinations

AxisR2<-function(x,ord,dist.method='bray')
{
library(vegan)
dist.x<-vegdist(x,method=dist.method)
	dist.ord1<-dist(ord[,1])
	dist.ord2<-dist(ord[,1:2])
		axis1<-mantel(dist.ord1,dist.x)$statistic
		axis2<-mantel(dist.ord2,dist.x)$statistic
			axis.r2.single<-rbind(axis1,(axis2-axis1))
				rownames(axis.r2.single)<-c('Axis1','Axis2')
				colnames(axis.r2.single)<-c('R^2')
					total.axis.r2<-axis.r2.single[[1]]+axis.r2.single[[2]]
						axis.r2<-round(rbind(total.axis.r2,axis.r2.single),5)
							rownames(axis.r2)<-c('Total','Axis1','Axis2')
	
if(dim(ord)[2]==3){
	dist.ord3<-dist(ord[,1:3])
		axis3<-mantel(dist.ord3,dist.x)$statistic
			axis.r2.single<-rbind(axis1,(axis2-axis1),(axis3-axis2))
				rownames(axis.r2.single)<-c('Axis1','Axis2','Axis3')
				colnames(axis.r2.single)<-c('R^2')
					total.axis.r2<-axis.r2.single[[1]]+axis.r2.single[[2]]+axis.r2.single[[3]]
						axis.r2<-round(rbind(total.axis.r2,axis.r2.single),5)
							rownames(axis.r2)<-c('Total','Axis1','Axis2','Axis3')
	}
	round(axis.r2,6)
}

##### Rotate a 2D ordination to line up an environmental variable from vegan's envfit with an axis. #####

ord.rotate<-function(ord,env.fit,env.vector,x.axis=TRUE,positive=TRUE,flip=FALSE) 
{
additive<-ifelse(x.axis==TRUE,0,pi/2)
ax<-ifelse(x.axis==TRUE,2,1)
pos<-ifelse(positive==TRUE,0,pi)
flp<-ifelse(flip==TRUE,-1,1)
vect.num<-match(env.vector,rownames(env.fit$vectors$arrows))
theta<-(additive-atan(env.fit$vectors$arrows[vect.num,2]/env.fit$vectors$arrows[vect.num,1]))+pos
rotate<-cbind(ord[,1]*cos(theta)-ord[,2]*sin(theta),ord[,2]*cos(theta)+ord[,1]*sin(theta))
	rownames(rotate)<-rownames(ord)
	colnames(rotate)<-colnames(ord)
rotate[,ax]<-rotate[,ax]*flp
as.data.frame(rotate)
}