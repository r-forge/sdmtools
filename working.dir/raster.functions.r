
.libPaths('/homes/31/jc165798/R_libraries')
install.packages("SDMTools", repos="http://R-Forge.R-project.org")
install.packages("raster", repos="http://R-Forge.R-project.org")
library(raster)
library(SDMTools)
library(rgdal)

setwd('/homes/31/jc165798/trial/')
tfile = 'rf.current.asc'

tasc = read.asc(tfile)
object.size(tasc)/1024/1024

traster = raster(tfile,values=T)
object.size(traster)/1024/1024

tgdal = readGDAL(tfile)
object.size(tgdal)/1024/1024

asc.from.raster = function(x) {
	if (!any(class(traster) %in% 'RasterLayer')) stop('x must be of class raster or RasterLayer')
	cellsize = (x@extent@ymax-x@extent@ymin)/x@nrows
	yll = x@extent@ymin + 0.5 * cellsize
	xll = x@extent@xmin + 0.5 * cellsize
	tmat = t(matrix(x@data@values,nr=x@nrows,ncol=x@ncols,byrow=T)[x@nrows:1,])
	tmat[which(tmat==x@file@nodatavalue)] = NA
	return(as.asc(tmat,yll=yll,xll=xll,cellsize=cellsize))
}
raster.from.asc = function(x,projs=NA) {
	if (class(x) != 'asc') stop('x must be of class asc')
	cellsize = attr(x, "cellsize")
	nrows = dim(x)[2]; ncols= dim(x)[1]
	xmin = attr(x, "xll") - 0.5 * cellsize
	ymin = attr(x, "yll") - 0.5 * cellsize
	xmax = xmin + ncols*cellsize
	ymax = ymin + nrows*cellsize
	r <- raster(ncols=ncols, nrows=nrows, projs=projs, xmn=xmin, xmx=xmax, ymn=ymin, ymx=ymax)
	r <- setValues(r, as.vector(t(t(unclass(x))[nrows:1,])))
	return(r)
}
asc.from.sp = function(x) {
	#assumes single band data
	if (!any(class(x) == 'SpatialGridDataFrame')) stop('x must be of class SpatialGridDataFrame')
	cellsize = mean(x@grid@cellsize)
	yll = as.numeric(x@grid@cellcentre.offset[2])
	xll = as.numeric(x@grid@cellcentre.offset[1])
	names(x@data)[1] = 'z'
	tmat = t(matrix(x@data$z,nr=x@grid@cells.dim[2],ncol=x@grid@cells.dim[1],byrow=T)[x@grid@cells.dim[2]:1,])
	return(as.asc(tmat,yll=yll,xll=xll,cellsize=cellsize))
}
sp.from.asc = function(x,projs=CRS(as.character(NA))) {
	if (class(x) != 'asc') stop('x must be of class asc')
	tgrid = GridTopology(c(attr(x, "xll"),attr(x, "yll")),rep(attr(x, "cellsize"),2),dim(x))
	return(SpatialGridDataFrame(tgrid,data.frame(z=as.vector(unclass(x)[,dim(x)[2]:1])),proj4string=projs))
}

tgdal2=sp.from.asc(tasc)
writeGDAL(tgdal, 'tgdal.tif', drivername = "GTiff", options="INTERLEAVE=PIXEL")
writeGDAL(tgdal, 'tgdal.asc', drivername = "AAIGrid")

traster2=raster.from.asc(tasc)
writeRaster(traster,'traster.tif',format='GTiff')
writeRaster(traster2,'traster2.tif',format='GTiff')
writeRaster(traster,'traster.asc',format='ascii')
writeRaster(traster,'traster.bil',format="BIL")
writeRaster(traster,'traster.png',format='PNG')


