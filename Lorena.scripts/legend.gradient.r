## LEGEND GRADIENT
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

# CREATE A FUNCTION FOR THE LEGEND GRADIENT
legend.gradient = function(pnts,cols=heat.colors(100),limits=c(0,1), title='Legend', cex=2){
  pnts = try(as.matrix(pnts),silent=T)
  if(!is.matrix(pnts)) stop("you must have a 4x2 matrix")
  if(dim(pnts)[1]!=4 || dim (pnts)[2]!=2) stop ("Matrix must have dimensions of 4 rows and 2 columms")
  if(length(cols)<2) stop("You must have 2 or more colors")
  #break up the min and max into a number of values == length(cols)
  yvals = seq(min(pnts[,2]),max(pnts[,2]),(max(pnts[,2])-min(pnts[,2]))/(length(cols)+1))
  #cycle through each of the yvals and create polygons
  for (i in 1:length(cols)){  #create the polygon for that color
  polygon(x=pnts[,1],y=c(yvals[i],yvals[i],yvals[i+1],yvals[i+1]),col=cols[i],border=F)
  }
  #add the text
  text(max(pnts[,1]),min(pnts[,2]),labels=limits[1],pos=4)
  text(max(pnts[,1]),max(pnts[,2]),labels=limits[2],pos=4)
  text(min(pnts[,1]),max(pnts[,2]),labels=title,adj=c(0,-1))
}


######################EXAMPLE SCALE LEGEND######################################

#define the input data directory
SDM.dir = "D:/Lorena/R Package (fragmentation)"

#define the output folder
outfolder = "D:/Lorena/R Package (fragmentation)"

# READING AN ASC II FILE
tasc = read.asc.gz("D:/Lorena/R Package (fragmentation)/rf.6kybp.asc.gz")

# Create a color ramp
colormap=c("grey","yellow","yellow2","yellowgreen","goldenrod3","firebrick")

#create an image
image(tasc,col=colormap,zlim=c(0,1), axes=F, xlab="", ylab="", ann=FALSE)

#put in the gradient scale
pnts = cbind(x =c(146.458, 146.688, 146.688, 146.458), y =c(-16.333, -16.333, -16.752,-16.752))

#create the scale legend
legend.gradient(pnts,colormap,c("Low","High"))

#create the Scalebar
Scalebar(x= 145.101, y=-19.535, distance=1)

