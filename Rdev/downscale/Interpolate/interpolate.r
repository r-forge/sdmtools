
#this code was includes bicubic interpolation and bilinear interpolation adapted from 
#Numerical Recipes in C: The are of scientific computing http://www.nrbook.com/nr3/ 
#(chapter 3... bicubic interpolation) and a bicubic interpolation from 
#http://www.paulinternet.nl/?page=bicubic in java code

####################################################################################
#required to build code
cd /homes/31/jc165798/working/R.downscale/Interpolate

R CMD SHLIB interpolate.c


####################################################################################
#the function

dyn.load("/homes/31/jc165798/SCRIPTS/sdmtools/Rdev/downscale/Interpolate/interpolate.so")

#mat is a matrix of values... NAs are not permitted
#xin and yin are coordinates of the x & y axis of the matrix
#xout and yout are the x& y coordinates of the output values
#type is the type of interpolation ... 1 for bilinear, 2 for bicubic (numeric recipes) and 3 for bicubic (java code)

#NOTE this assumes locations are midpoints of cells...

interp = function(mat,xin,yin,xout,yout,type)	{
	#check to ensure matrix and has no NA's
	mat = as.matrix(mat)
	if (length(dim(mat))!=2) stop('objects must be a matrix')
	if (!is.matrix(mat)) stop('objects must be a matrix')
	if (length(which(!is.finite(mat)))>0) stop('All matrix values must be numeric... NA, inf, etc. are not permitted')
	#ensure xin & yin are the same length as the dimensions of the matrix
	if (length(xin)!=dim(mat)[2] & length(yin)!=dim(mat)[1]) stop('input coords must match dimensions of matrix')
	#check to ensure tx & ty fall within the boundaries of xin & yin
	if (min(xout)>=min(xin) & min(xout)<=min(xin) & max(xout)>=max(xin) & max(xout)<=max(xin)){
		if (min(yout)>=min(yin) & min(yout)<=min(yin) & max(yout)>=max(yin) & max(yout)<=max(yin)){
			#run the interpolation
			out = .Call('interpolate',mat,xin,yin,xout,yout,as.integer(type))
			return(out)
		} else {
			stop('interpolation data falls outside the input data boundaries')
		}
	} else {
		stop('interpolation data falls outside the input data boundaries')
	}
}



####################################################################################
#Examples

setwd('/homes/31/jc165798/working/R.downscale/Interpolate/')

#values to interpolate to
tx = seq(0,3,0.1)
ty = seq(0,3,0.1)
txy=expand.grid(x=tx,y=ty)
tmat = matrix(round(runif(16,1,16)),nr=4)
#tmat[1,1] = NA
tmat = matrix(runif(16,1,16),nr=4)
txy$bilinear = interp(tmat,0:3,0:3,tx,ty,as.integer(1))
txy$bicubic1 = interp(tmat,0:3,0:3,tx,ty,as.integer(2))
txy$bicubic2 = interp(tmat,0:3,0:3,tx,ty,as.integer(3))

png(filename = "trials1.png",width=480*4,height=480*4,pointsize=20)
	par(mfrow=c(2,2),cex=1)
	image(tmat,main='base',zlim=c(0,16),col=heat.colors(100))	 
	image(matrix(txy$bilinear,nr=length(ty),byrow=F),main='bilinear',zlim=c(0,16),col=heat.colors(100))	 
	image(matrix(txy$bicubic1,nr=length(ty),byrow=F),main='bicubic1',zlim=c(0,16),col=heat.colors(100))	 
	image(matrix(txy$bicubic2,nr=length(ty),byrow=F),main='bicubic2',zlim=c(0,16),col=heat.colors(100))	 
dev.off()

#try actual data
library(SDMTools)
tasc = read.asc.gz('/homes/31/jc165798/SCRIPTS/sdmtools/Rdev/downscale/testdata/rain.asc.gz')

tasc.x = getXYcoords(tasc)$x
tasc.y = getXYcoords(tasc)$y

tx = seq(min(tasc.x),max(tasc.x),0.01)
ty = seq(min(tasc.y),max(tasc.y),0.01)

bilinear1 = interp2grid(tasc,tx,ty,type=1)
bicubic2 = interp2grid(tasc,tx,ty,type=2)
bicubic3 = interp2grid(tasc,tx,ty,type=3)

png(filename = "trials2.png",width=480*4,height=480*4,pointsize=20)
	par(mfrow=c(2,2),cex=1)
	image(tasc,main='base',col=heat.colors(100))	 
	image(bilinear1,main='bilinear',col=heat.colors(100))	 
	image(bicubic2,main='bicubic2',col=heat.colors(100))	 
	image(bicubic3,main='bicubic3',col=heat.colors(100))	 
dev.off()

