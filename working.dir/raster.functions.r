library(raster)
library(SDMTools)

setwd('c:/tmp/')
tfile = 'rf.current.asc'

tasc = read.asc(tfile)
object.size(tasc)/1024/1024

traster = raster(tfile,values=T)
object.size(traster)/1024/1024

asc.from.raster = function(x) {
	if (class(x) != 'asc') stop('x must be of class asc')
	cellsize = (x@extent@ymax-x@extent@ymin)/x@nrows
	yll = x@extent@ymin + 0.5 * cellsize
	xll = x@extent@xmin + 0.5 * cellsize
	tmat = t(matrix(x@data@values,nr=x@nrows,ncol=x@ncols,byrow=T)[x@nrows:1,])
	tmat[which(tmat==x@file@nodatavalue)] = NA
	return(as.asc(tmat,yll=yll,xll=xll,cellsize=cellsize))
}
raster.from.asc = function(x,projs=NA) {
	if (!any(class(traster) %in% c('raster','RasterLayer'))) stop('x must be of class raster or RasterLayer')
	cellsize = attr(x, "cellsize")
	nrows = dim(x)[2]; ncols= dim(x)[1]
	xmin = attr(x, "xll") - 0.5 * cellsize
	ymin = attr(x, "yll") - 0.5 * cellsize
	xmax = xmin + ncols*cellsize
	ymax = ymin + nrows*cellsize
	r <- raster(ncols=ncols, nrows=nrows, projs=projs, xmn=xmin, xmx=xmax, ymn=ymin, ymx=ymax)
	tvals = as.vector(t(t(unclass(x))[nrows:1,]))
	r <- setValues(r, as.vector(t(t(unclass(x))[nrows:1,])))
	return(r)
}


if (any(class(traster) %in% c('raster','RasterLayer'))) print('yes')
if (any(class(tasc) %in% c('raster','RasterLayer'))) print('yes')


