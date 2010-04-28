## COLOR MAP (IMAGE)
################################################################################
#load necessary libraries

#list the libraries needed
necessary=c("adehabitat","maptools","SDMTools")
#check if library is installed
installed = necessary %in% installed.packages()
#if library is not installed, install it
if (length(necessary[!installed]) >=1) install.packages(necessary[!installed], dep = T)

#load the libraries
for (lib in necessary) library(lib,character.only=T)


###########################FUNCTIONS########################################


# CREATE A FUNCTION FOR MAP COLORS

quick.map= function(sdm.asc, threshold, bkgd.col = 'grey',cols=heat.colors(100), limits=c("Low","High"), axes=TRUE, ...){
    #get range of data
    trange=range(sdm.asc,na.rm=T)
    #make sure threshold is within range
    if (!(trange[1]< threshold & trange[2]>threshold)) stop('Cannot be 0')
    #rework sdm.asc
    sdm.asc[which(!is.na(sdm.asc) & sdm.asc<=threshold)] = 0
    tvals = seq(threshold,trange[2],(trange[2]-threshold)/(length(cols)+2))
    for (i in 1:length(cols)) sdm.asc[which(!is.na(sdm.asc) & sdm.asc<tvals[i] & sdm.asc>=tvals[i+1])] = 0
    #create the image
    image(sdm.asc, col=c(bkgd.col,cols),axes=T, xlab="", ylab="",...)
    if (!any(ls() == 'zlim')) { lim = range(sdm.asc,na.rm=T) } else if (!is.null(zlim)) { lim = range(sdm.asc,na.rm=T) } else { lim = zlim }
    #add the scale legend
    legend.gradient(pnts,cols=c(bkgd.col,cols),limits=lim)
    
}
 
#######################EXAMPLE COLORMAP#########################

#create a matrix
tmat = { matrix(c(	0,0,0,1,0,0,1,1,0,1,
                    0,0,1,0,1,0,0,0,0,0,
                    0,1,NA,1,0,1,0,0,0,1,
					          1,0,1,1,1,0,1,0,0,1,
				            0,1,0,1,0,1,0,0,0,1,
					          0,0,1,0,1,0,0,1,1,0,
					          1,0,0,1,0,0,1,0,0,0,
				            0,1,0,0,0,1,0,0,0,1,
					          0,0,1,1,1,0,0,1,1,1,
				            1,1,1,0,0,0,0,1,1,1),nr=10,byrow=T) }

#do the connected component labelling
tasc = ConnCompLabel(tmat)

#put in the gradient scale
pnts = cbind(x =c(1.1,1.2,1.2,1.1), y =c(0.9,0.9,0.7,0.7))

# Set the map and gradient leyend colors 
tasc.col=colorRampPalette(c("yellow","orange", "red"))(5)

#Create an image with the gradient legend
quick.map(tasc,0.09,bkgd.col = 'darkgrey', cols=tasc.col,axes=F, xlim=c(0.0,1.35))
