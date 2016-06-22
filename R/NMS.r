## Run NMS, including step-down, and save relevant ordination output to an "NMS Output" folder in the working directory.
	## J. D. Muehlbauer, 14 May 2012.

##### Warnings #####
## Requires R libaries vegan and parallel to be installed (but not necessarily loaded) prior to running NMS.
## Windows firewall may pop up as a result of running parallel on multiple cores.  Select "Allow."
## Running this function multiple times from the same working directory will cause old csv points and species files will be overwritten.  
	## To avoid this, change the working directory or the previously written filenames before running this function, or use a different file.append value.
	
##### Input Variables #####
## Data: A matrix containing only numeric data (i.e., no rows or columns of factors).
## maxruns: The maximum number of random starts allowed to find a convergent ordination solution.  Default is 1000.  Corresponds to trymax in vegan's metaMDS function.
## step.maxruns: The number of maxruns used when creating the ordinations for the stepdown plot.  Default is 100.
## file.append: Appends a character(s) to the end of the code-generated filenames in the output ordination files (in front of '.csv', etc.)  Default is blank ('').
## stepdown: Should a stepdown series of ordinations be run, from 6 to 1 axis, and plot related scree plots (TRUE)?  Default is TRUE.
## only23: Should only 2D and 3D ordinations be run (TRUE), or should R ask if any other dimensions should be tried (FALSE)?  Default is FALSE.  
	## Note that only23=TRUE should probably only be used if NMS is being run as part of a batch of code (in which case only23=FALSE may give errors).  
## cores: Number of computer cores to parse the function onto (min = 1, max = computer's max, and more = faster).  Defaults to the computer's max.


NMS<-function(Data,maxruns=1000,step.maxruns=100,file.append='',stepdown=TRUE,only23=FALSE,cores=as.numeric(Sys.getenv('NUMBER_OF_PROCESSORS')))
{
## Create the output folder
dir.create('NMS Output',showWarnings=FALSE)

## Save datasheet so it can be run in parallel
write.csv(Data,paste('NMS Output/R_DataForNMS',file.append,'.csv',sep=''))
library(parallel,quietly=TRUE)

most<-{
if(stepdown==TRUE){
## Start with stepdown from 6 axes for preliminary analysis
run.NMS.stress<-function(X){
	library(vegan)
	Data<-read.csv(paste('NMS Output/R_DataForNMS',file.append,'.csv',sep=''),header=T,row.names=1)
	vegan::metaMDS(Data,k=X,trymax=step.maxruns)
	}
cl.stress<-makeCluster(getOption("cl.cores",cores))
	MDS.stress<-parLapply(cl.stress,c(1:6),run.NMS.stress)
		stopCluster(cl.stress)

## Combine stress values from step down, make scree plot figure
step.stress<-sapply(c(1:6),function(x) MDS.stress[[x]]$stress)
	png(paste('NMS Output/ScreePlot',file.append,'.png',sep=''))
		plot(c(1:6),step.stress,xlab='# Axes',ylab='Stress')
		lines(c(1:6),step.stress)
		dev.off()
		
## Open the scree plot in the plotting window	
plot(c(1:6),step.stress,xlab='# Axes',ylab='Stress')
	lines(c(1:6),step.stress)
}
	
## Save 2D and 3D data, work from those rather than re-running MDS every time
run.NMS<-function(X){
	library(vegan)
	Data<-read.csv(paste('NMS Output/R_DataForNMS',file.append,'.csv',sep=''),header=T,row.names=1)
	vegan::metaMDS(Data,k=X,trymax=maxruns,trace=0)
	}
cl<-makeCluster(getOption("cl.cores",cores))
	MDS<-parLapply(cl,c(2:3),run.NMS)
		stopCluster(cl)

for(i in 1:length(MDS)){
MDS.points <-MDS[[i]]$points	
	MDS.points[,1]<-MDS.points[,1] *(-1)
		write.csv(MDS.points,paste('NMS Output/NMSPoints',MDS[[i]]$ndim,'D',file.append,'.csv',sep=''),row.names=T)
MDS.species<-MDS[[i]]$species
	MDS.species[,1]<-MDS.species[,1] *(-1)
		write.csv(MDS.species,paste('NMS Output/NMSSpecies',MDS[[i]]$ndim,'D',file.append,'.csv',sep=''),row.names=T)
	}

## Save the 2D and 3D stress values to a datasheet
	thestress<-cbind(c(2:3),c(MDS[[1]]$stress,MDS[[2]]$stress))
		colnames(thestress)<-c('Number of Axes','Stress')
		write.csv(thestress,paste('NMS Output/Stress',file.append,'.csv',sep=''),row.names=FALSE)
		

		
## Run ordinations in additional dimensions upon request
print(cat("\n","2D and 3D solutions have been saved in the 'NMS Output' folder,",
	"\n","along with a csv of the input matrix, stress data, and the scree plot.			"))
}

if(only23==FALSE){
most
print(cat("\n","Based on the scree plot (either the saved one or the one that has popped up)...							"))
repeat{
print(cat("\n","Would you like to run the ordination in any additional dimensions?",
	"\n","Enter the desired dimensions separated by commas, or type '0' (no quotes) for 'No.'		"))
	
d<-scan(nlines=1,sep=',')
if(d[1]==0){break}
else{
	if(length(d)>1){
		cl.more<-makeCluster(getOption("cl.cores",cores))
		MDS.more<-parLapply(cl.more,c(d),run.NMS)
		stopCluster(cl)} 
	else{MDS.more<-lapply(d,run.NMS)}
for(i in 1:length(MDS.more)){
MDS.more.points <-MDS.more[[i]]$points	
	MDS.more.points[,1]<-MDS.more.points[,1] *(-1)
		write.csv(MDS.more.points,paste('NMS Output/NMSPoints',MDS.more[[i]]$ndim,'D',file.append,'.csv',sep=''),row.names=T)
MDS.more.species<-MDS.more[[i]]$species
	MDS.more.species[,1]<-MDS.more.species[,1] *(-1)
		write.csv(MDS.more.species,paste('NMS Output/NMSSpecies',MDS.more[[i]]$ndim,'D',file.append,'.csv',sep=''),row.names=T)
		}
	}
}
	
print(cat("\n","OK, All done!											"))	
}
else{
most
print(cat("\n","All done!											"))	
}
}