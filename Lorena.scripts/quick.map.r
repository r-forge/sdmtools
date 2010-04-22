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

quick.map= function(sdm.asc, threshold, bkgd.col = 'grey',cols=heat.colors(100)){
    #get range of data
    trange=range(sdm.asc,na.rm=T)
    #make sure threshold is within range
    if (!(trange[1]< threshold & trange[2]>threshold)) stop('Cannot be 0')
    #rework sdm.asc
    sdm.asc[which(!is.na(sdm.asc) & sdm.asc<=threshold)] = 0
    tvals = seq(threshold,trange[2],(trange[2]-threshold)/(length(cols)+2))
    for (i in 1:length(cols)) sdm.asc[which(!is.na(sdm.asc) & sdm.asc<tvals[i] & sdm.asc>=tvals[i+1])] = 0
    #create the image
    image(sdm.asc, col=c(bkgd.col,cols))
    #add the scale bar
    legend.gradient(pnts,cols=c(bkgd.col,cols))
    Scalebar(x= 145.101, y=-19535, distance=20)
}

#######################EXAMPLE COLORMAP#########################
  # READING AN ASC II FILE
#define the input data directory
SDM.dir = "D:/Lorena/R Package (fragmentation)"

#define the output folder
outfolder = "D:/Lorena/R Package (fragmentation)"

tasc = read.asc.gz("D:/Lorena/R Package (fragmentation)/rf.6kybp.asc.gz")

#put in the gradient scale
pnts = cbind(x =c(146.458, 146.688, 146.688, 146.458), y =c(-16.333, -16.333, -16.752,-16.752))

quick.map(tasc,0.08,bkgd.col = 'darkgrey', Scalebar=(x= 145.101, y=-19535, distance=20))

Scalebar(x= 145.101, y=-19535, distance=20)