library(raster)
library(SDMTools)

setwd('c:/tmp/')

tasc = read.asc('MTHORN_p00.asc')
object.size(tasc)/1024/1024

traster = raster('MTHORN_p00.asc',values=T)
object.size(traster)/1024/1024

asc.from.raster = function(x) {
	cellsize = (x@extent@ymax-x@extent@ymin)/x@nrows
	yll = x@extent@ymin + 0.5 * cellsize
	xll = x@extent@xmin + 0.5 * cellsize
	return(as.asc(t(matrix(x@data@values,nr=x@nrows,ncol=x@ncols,byrow=T)[x@nrows:1,]),yll=yll,xll=xll,cellsize=cellsize))
}
raster.from.asc = function(x) {


}
spatialgrid.from.asc


tasc2 = as.asc.from.raster(traster)
image(tasc2)

if (any(class(traster) %in% c('raster','RasterLayer'))) print('yes')
if (any(class(tasc) %in% c('raster','RasterLayer'))) print('yes')

