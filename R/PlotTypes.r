## To automatically plot and save a figure in multiple filetypes
	## 4 November 2014, J. D. Muehlbauer
	
##### Input Variables #####
## plotfx: All the plot commands, wrapped as a function.
## directory: The Directory where the figure is to be saved. Defaults to working directory. If set, must end with a slash (/).
## filetype: Filetypes to save. Possibilities include pdf (the default), png, jpeg, tiff, bmp, and eps.
## width: Width of the figure. Defaults to 6.5.
## heigth: Height of the figure. Defaults to 9.
## units: Units on the width and height measurements. Possibilities include px, cm, mm, or in (the default).
## res: Resolution of the figure (if relevant). Defaults to 600 dpi.

##### Example #####
#t1<-function(){plot(0,0,main='Hiya!')}
#plotTypes(t1,'HelloWorld',filetype=c('pdf','png'))

plotTypes<-function(plotfx,filename,directory='',filetype=c('pdf'),width=6.5,height=9,units='in',res=600){
	if('pdf'%in%filetype){
		pdf(paste0(directory,filename,'.pdf'),width=width,height=height)
		plotfx()
		dev.off()
	}
	if('png'%in%filetype){
		png(paste0(directory,filename,'.png'),width=width,height=height,units=units,res=res)
		plotfx()
		dev.off()
	}
	if('jpeg'%in%filetype){
		jpeg(paste0(directory,filename,'.jpg'),width=width,height=height,units=units,res=res)
		plotfx()
		dev.off()
	}
	if('tiff'%in%filetype){
		tiff(paste0(directory,filename,'.tif'),width=width,height=height,units=units,res=res)
		plotfx()
		dev.off()
	}
	if('bmp'%in%filetype){
		bmp(paste0(directory,filename,'.bmp'),width=width,height=height,units=units,res=res)
		plotfx()
		dev.off()
	}
	if('eps'%in%filetype){
		setEPS()
		postscript(paste0(directory,filename,'.eps'),width=width,height=height)
		plotfx()
		dev.off()	
	}
}
