#This is the base function for common components labelling based on the one pass contour tracing technique
#Chang, F., C.-J. Chen, and C.-J. Lu. 2004. A linear-time component-labeling algorithm using contour tracing technique. Comput. Vis. Image Underst. 93:206-220.

#it simply requres a binary matrix of values which are 0 & 1. NA's are permitted.

####################################################################################
#required to build code
cd /homes/31/jc165798/working/R.packages/pieces/fragments/src/
R CMD SHLIB ccl.c #connected components labelling
R CMD SHLIB PS.c #patch statistics

####################################################################################
#other functions

### connected components labelling
#mat is a binary matrix of data with NA, 0 for background and 1 for 'patch' to be classified
dyn.load("/homes/31/jc165798/working/R.packages/pieces/fragments/src/ccl.so")
ConnCompLabel = function(mat)	{
	#get the input attributes
	tattrib = attributes(mat)
	#check to ensure matrix
	mat = try(as.matrix(mat))
	if (!is.matrix(mat)) stop('objects must be a matrix')
	#run the connected component labelling
	out = .Call('ccl',mat)
	#reset the attributes of the input
	attributes(out) = tattrib
	return(out)
}
#
##method to calculate shape index or aggregation indexes
#a = area of the patch in number of cells
#p is the perimeter in number of edges
#g is the number of 'internal' edges (single count)
shape.index = function(a,p) {
	n = trunc(sqrt(a))
	m = a - n^2
	minp=rep(0,length(m))
	for (ii in 1:length(m)){
		if (m[ii]==0) minp[ii] = 4*n[ii]
		if (n[ii]^2<a[ii] & a[ii]<=n[ii]*(1+n[ii])) minp[ii] = 4 * n[ii] + 2
		if (a[ii] > n[ii]*(1+n[ii])) minp[ii] = 4 * n[ii] + 4
	}
	return(p/minp)
}
aggregation.index = function(a,g) {
	n = trunc(sqrt(a))
	m = a - n^2
	if (m==0) maxg = 2*n*(n-1)
	if (m<=n) maxg = 2*n*(n-1)+2*m-1
	if (m>n) maxg = 2*n*(n-1)+2*m-2
	minp=rep(0,length(m))
	for (ii in 1:length(m)){
		if (m[ii]==0) minp[ii] = 4*n[ii]
		if (n[ii]^2<a[ii] & a[ii]<=n[ii]*(1+n[ii])) minp[ii] = 4 * n[ii] + 2
		if (a[ii] > n[ii]*(1+n[ii])) minp[ii] = 4 * n[ii] + 4
	}
	return((g/maxg)*100)
}

### patch statistics
#mat is a matrix of data representing a raster of uniquely identified patches (from ConnCompLabel)
#cell size is a single value representing the width/height of cell edges (assuming square cells and distance is in m)
dyn.load("/homes/31/jc165798/working/R.packages/pieces/fragments/src/PS.so")
PatchStat = function(mat,cellsize=1)	{
	#check to ensure matrix
	mat = try(as.matrix(mat))
	if (!is.matrix(mat)) stop('objects must be a matrix')
	#get the unique patch ID's
	ID.vals = as.numeric(na.omit(unique(as.vector(mat))));ID.vals = ID.vals[order(ID.vals)]
	#extract the base patch info
	out = as.data.frame(.Call('PS',mat,ID.vals))
	names(out) = c('patchID','n.cell','n.core.cell','n.edges.perimeter','n.edges.internal')
	#calculate other stats
	out$area = out$n.cell * cellsize^2
	out$core.area = out$n.core.cell * cellsize^2
	out$perimeter = out$n.edges.perimeter * cellsize
	out$perim.area.ratio = out$perimeter / out$area 
	out$shape.index = shape.index(out$n.cell,out$n.edges.perimeter)
	out$frac.dim.index = (2 * log(0.25 * out$perimeter)) / log(out$area)
	out$core.area.index = out$core.area / out$area
	return(out)
}

#calculate class statistics
#mat is a matrix of data representing a raster of uniquely identified patches (from ConnCompLabel)
#cell size is a single value representing the width/height of cell edges (assuming square cells and distance is in m)
#bkgd is the background value for which statistics will not be calculated
ClassStat = function(mat,cellsize=1,bkgd=NA) {
	#check to ensure matrix
	mat = try(as.matrix(mat))
	if (!is.matrix(mat)) stop('objects must be a matrix')
	#get the uniqu classes of data
	classes = as.numeric(na.omit(unique(as.vector(mat))));classes = classes[order(classes)]
	#omit the background value
	if (!is.na(bkgd)) classes = classes[-which(classes==bkgd)]
	#out is the final object to be returned
	out = NULL
	#cycle through each of the classes
	for (cl in classes){
		#create a reclassed matrix
		mat2 = mat; mat2 = mat * 0; mat2[which(mat==cl)] = 1
		#get the patch info for the class
		out.patch = PatchStat(.Call('ccl',mat2),cellsize=cellsize);rm(mat2)
		#define a couple constants
		L.cell = sum(out.patch$n.cell) #n cells in landscape
		L.area = L.cell * cellsize^2 #full area of landscape
		#remove the background patch (id = 0)
		out.patch = out.patch[-1,]		
		#create a temporary variable to store output & calculate patch stats
		tout = list(class=cl)
		tout$n.patches = nrow(out.patch)
		tout$total.area = sum(out.patch$n.cell * cellsize^2)
		tout$prop.landscape = sum(out.patch$n.cell) / L.cell
		tout$patch.density = tout$n.patches / L.area
		tout$total.edge = sum(out.patch$perimeter)
		tout$edge.density = tout$total.edge / L.area
		tout$landscape.shape.index = shape.index(sum(out.patch$n.cell),sum(out.patch$n.edges.perimeter))
		tout$largest.patch.index = max(out.patch$n.cell) / L.cell
		tout$mean.patch.area = mean(out.patch$area)
		tout$sd.patch.area = sd(out.patch$area)
		tout$min.patch.area = min(out.patch$area)
		tout$max.patch.area = max(out.patch$area)
		tout$perimeter.area.frac.dim = 2 / (((tout$n.patches*sum(log(out.patch$perimeter)+log(out.patch$area)))-(tout$total.edge*tout$total.area))/(tout$n.patches*sum(log(out.patch$perimeter^2))-tout$total.edge^2))
		tout$mean.perim.area.ratio = mean(out.patch$perim.area.ratio)
		tout$sd.perim.area.ratio = sd(out.patch$perim.area.ratio)
		tout$min.perim.area.ratio = min(out.patch$perim.area.ratio)
		tout$max.perim.area.ratio = max(out.patch$perim.area.ratio)
		tout$mean.shape.index = mean(out.patch$shape.index,na.rm=T)
		tout$sd.shape.index = sd(out.patch$shape.index,na.rm=T)
		tout$min.shape.index = min(out.patch$shape.index,na.rm=T)
		tout$max.shape.index = max(out.patch$shape.index,na.rm=T)
		tout$mean.frac.dim.index = mean(out.patch$frac.dim.index,na.rm=T)
		tout$sd.frac.dim.index = sd(out.patch$frac.dim.index,na.rm=T)
		tout$min.frac.dim.index = min(out.patch$frac.dim.index,na.rm=T)
		tout$max.frac.dim.index = max(out.patch$frac.dim.index,na.rm=T)	
		tout$total.core.area = sum(out.patch$n.core.cell * cellsize^2)
		tout$prop.landscape.core = sum(out.patch$n.core.cell) / L.cell
		tout$mean.patch.core.area = mean(out.patch$n.core.cell * cellsize^2)
		tout$sd.patch.core.area = sd(out.patch$n.core.cell * cellsize^2)
		tout$min.patch.core.area = min(out.patch$n.core.cell * cellsize^2)
		tout$max.patch.core.area = max(out.patch$n.core.cell * cellsize^2)
		tout$prop.like.adjacencies = sum(out.patch$n.edges.internal) / sum(out.patch$n.edges.internal+out.patch$n.edges.perimeter*2)
		tout$aggregation.index = aggregation.index(sum(out.patch$n.cell),sum(out.patch$n.edges.internal)/2)
		tout$lanscape.division.index = 1-sum((out.patch$n.cell / L.cell)^2)
		tout$splitting.index = L.area / sum(out.patch$area^2)
		tout$effective.mesh.size = sum(out.patch$area^2) / L.area 
		tout$patch.cohesion.index = ((1-(sum(out.patch$n.edges.internal)/sum(out.patch$n.edges.internal*sqrt(out.patch$n.cell))) )*((1-1/sqrt(L.cell))/10))*100
		
		#store in out 
		out = rbind(out,as.data.frame(tout))
	}
	return(out)
}

####################################################################################
#Examples
#define a simple binary matrix
tmat = { matrix(c(	0,0,0,1,0,0,1,1,0,1,
					0,0,1,0,1,0,0,0,0,0,
					0,1,NA,1,0,1,0,0,0,1,
					1,0,1,1,1,0,1,0,0,1,
					0,1,0,1,0,1,0,0,0,1,
					0,0,1,0,1,0,0,1,1,0,
					1,0,0,1,0,0,1,0,0,1,
					0,1,0,0,0,1,0,0,0,1,
					0,0,1,1,1,0,0,0,0,1,
					1,1,1,0,0,0,0,0,0,1),nr=10,byrow=T) }

#do the connected component labelling
ccl.mat = ConnCompLabel(tmat)
ccl.mat
image(t(ccl.mat[10:1,]),col=c('grey',rainbow(length(unique(ccl.mat))-1)));dev.off()
#calculate the patch statistics
ps.data = PatchStat(ccl.mat)
ps.data
#calculate the class statistics
cl.data = ClassStat(tmat)
cl.data
#identify background data is 0
cl.data = ClassStat(tmat,bkgd=0)
cl.data

#real example
#load the library
library(SDMTools,lib.loc='/homes/31/jc165798/R_libraries')
#readin in a ascii grid file
tasc = read.asc.gz('/homes/31/jc165798/working/R.packages/trial.data/rf.current.asc.gz')
#convert it to binary based on a threshold
tasc[which(tasc>=0.2 & is.finite(tasc))] = 1
tasc[which(tasc<0.2 & is.finite(tasc))] = 0
#do the connected component labelling
ccl.mat = ConnCompLabel(tasc)
image(ccl.mat,col=c('grey',rainbow(length(unique(ccl.mat))-1)));dev.off()
#calculate the patch statistics
ps.data = PatchStat(ccl.mat,250)
head(ps.data)
#calculate the class statistics
cl.data = ClassStat(tasc,250,bkgd=0)
cl.data


