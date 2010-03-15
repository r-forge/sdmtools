#This is the base function for common components labelling based on the one pass contour tracing technique
#Chang, F., C.-J. Chen, and C.-J. Lu. 2004. A linear-time component-labeling algorithm using contour tracing technique. Comput. Vis. Image Underst. 93:206-220.

#it simply requres a binary matrix of values which are 0 & 1. NA's are permitted.

####################################################################################
#required to build code
cd /homes/31/jc165798/working/R.packages/pieces/fragments/PatchStats/

R CMD SHLIB PS.c


####################################################################################
#other functions
#connected components
dyn.load("/homes/31/jc165798/working/R.packages/pieces/fragments/CommonComponentsLabelling/ccl.so")

ConnCompLabel = function(mat)	{
	#get the input attributes
	tattrib = attributes(mat)
	#check to ensure matrix
	mat = as.matrix(mat)
	if (length(dim(mat))!=2) stop('objects must be a matrix')
	if (!is.matrix(mat)) stop('objects must be a matrix')
	#run the connected component labelling
	out = .Call('ccl',mat)
	#reset the attributes of the input
	attributes(out) = tattrib
	return(out)
}

dyn.load("/homes/31/jc165798/working/R.packages/pieces/fragments/PatchStats/PS.so")

#the function
#mat is a matrix of data representing a raster of uniquely identified patches

PatchStat = function(mat,cellsize=1,scaling=1)	{
	#check to ensure matrix
	mat = try(as.matrix(mat))
	if (!is.matrix(mat)) stop('objects must be a matrix')
	if (length(dim(mat))!=2) stop('objects must be a matrix')
	#get the unique patch ID's
	ID.vals = as.numeric(na.omit(unique(as.vector(mat))))
	#extract the base patch info
	out = as.data.frame(.Call('PS',mat,ID.vals))
	names(out) = c('patchID','n.cell','n.core.cell','n.edges.perimeter','n.edges.internal')
	
	return(out)
}

####################################################################################
#Examples

tmat = matrix(c(	0,0,0,1,0,0,1,1,0,1,
					0,0,1,0,1,0,0,0,0,0,
					0,1,NA,1,0,1,0,0,0,1,
					1,0,1,1,1,0,1,0,0,1,
					0,1,0,1,0,1,0,0,0,1,
					0,0,1,0,1,0,0,1,1,0,
					1,0,0,1,0,0,1,0,0,1,
					0,1,0,0,0,1,0,0,0,1,
					0,0,1,1,1,0,0,0,0,1,
					1,1,1,0,0,0,0,0,0,1),nr=10,byrow=T)
tmat
ccl.mat = ConnCompLabel(tmat)

PatchStat(ccl.mat)



#real example
#load the library
library(SDMTools,lib.loc='/homes/31/jc165798/R_libraries')
#set the working directory
setwd('/homes/31/jc165798/working/R.fragments')
#readin in a ascii grid file
tasc = read.asc('testdata/rf_00000.asc')
#convert it to binary based on a threshold
tasc[which(tasc>=0.2 & is.finite(tasc))] = 1
tasc[which(tasc<0.2 & is.finite(tasc))] = 0
#run the analysis
tout = ConnCompLabel(tasc)

#post analysis
tt = as.vector(tout)
tdata = aggregate(tt,by=list(tt),length) #get the number of cells of each patch
names(tdata) = c('label','count')
plot(tdata$label[-1],tdata$count[-1]);dev.off() #plot this

tdata$colors = c('grey',terrain.colors(nrow(tdata)-1)) #recolor the patches
#uniquely color the 5 bigest patches
tdata[order(tdata$count,decreasing=T)[1:10],]
tdata$colors[order(tdata$count,decreasing=T)[2:6]]=c('red','blue','green','yellow','orange')

#create a figure
png(filename = "tt.png",width=3000, height=3000)
	par(mfrow=c(1,2))
	image(tasc,col=c('grey','red'))
	image(tout,col=tdata$colors)
dev.off()

