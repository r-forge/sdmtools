
cd /homes/31/jc165798/SCRIPTS/sdmtools/Rdev/downscale/Interpolate/

R CMD SHLIB interpolate.c

#.call inputs are 
# a matrix, the x and then y coords of the matrix, the x & y coordinates of the output matrix and the type of interpolation
#################################################################################
setwd('/homes/31/jc165798/SCRIPTS/sdmtools/Rdev/downscale/Interpolate/')
dyn.load("/homes/31/jc165798/SCRIPTS/sdmtools/Rdev/downscale/Interpolate/interpolate.so")


interpolate = function(tmat,pnts,type=2) {
	#values to interpolate to
	tx = seq(0,3,0.1)
	ty = seq(0,3,0.1)
	txy=expand.grid(x=tx,y=ty)
	#tmat = matrix(round(runif(16,1,16)),nr=4)
	#tmat[1,1] = NA
	tmat = matrix(1:16,nr=4)
	txy$bilinear = .Call('interpolate',tmat,0:3,0:3,tx,ty,as.integer(1))
	txy$bicubic1 = .Call('interpolate',tmat,0:3,0:3,tx,ty,as.integer(2))
	txy$bicubic2 = .Call('interpolate',tmat,0:3,0:3,tx,ty,as.integer(3))

	return(txy)

}


#values to interpolate to
tx = seq(0,3,0.1)
ty = seq(0,3,0.1)
txy=expand.grid(x=tx,y=ty)
#tmat = matrix(round(runif(16,1,16)),nr=4)
#tmat[1,1] = NA
tmat = matrix(1:16,nr=4)
txy$bilinear = .Call('interpolate',tmat,0:3,0:3,tx,ty,as.integer(1))
txy$bicubic1 = .Call('interpolate',tmat,0:3,0:3,tx,ty,as.integer(2))
txy$bicubic2 = .Call('interpolate',tmat,0:3,0:3,tx,ty,as.integer(3))

png(filename = "trials.png",width=480*4,height=480*4,pointsize=20)
	par(mfrow=c(2,2),cex=1)
	image(tmat,main='base',zlim=c(0,16),col=heat.colors(100))	 
	image(matrix(txy$bilinear,nr=length(ty),byrow=T),main='bilinear',zlim=c(0,16),col=heat.colors(100))	 
	image(matrix(txy$bicubic1,nr=length(ty),byrow=T),main='bicubic1',zlim=c(0,16),col=heat.colors(100))	 
	image(matrix(txy$bicubic2,nr=length(ty),byrow=T),main='bicubic2',zlim=c(0,16),col=heat.colors(100))	 
dev.off()




#################################################################################
#try actual data
library(adehabitat)
tasc = import.asc('testdata/rain.asc')

tasc.x = getXYcoords(tasc)$x
tasc.y = getXYcoords(tasc)$y

tx = seq(min(tasc.x),max(tasc.x),0.01)
ty = seq(min(tasc.y),max(tasc.y),0.01)
txy = expand.grid(x=tx,y=ty)

txy$bilinear = .Call('interpolate',t(tasc),tasc.x,tasc.y,tx,ty,as.integer(1))
txy$bicubic1 = .Call('interpolate',t(tasc),tasc.x,tasc.y,tx,ty,as.integer(2))
txy$bicubic2 = .Call('interpolate',t(tasc),tasc.x,tasc.y,tx,ty,as.integer(3))

png(filename = "trials.png",width=480*4,height=480*4,pointsize=20)
	par(mfrow=c(2,2),cex=1)
	image(tasc,main='base',col=heat.colors(100))	 
	image(x=tx,y=ty,z=matrix(txy$bilinear,nr=length(tx),byrow=T),main='bilinear',col=heat.colors(100))	 
	image(x=tx,y=ty,z=matrix(txy$bicubic1,nr=length(tx),byrow=T),main='bicubic1',col=heat.colors(100))	 
	image(x=tx,y=ty,z=matrix(txy$bicubic2,nr=length(tx),byrow=T),main='bicubic2',col=heat.colors(100))	 
dev.off()
