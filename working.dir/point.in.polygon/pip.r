#this is code for point in polygon algorithm.

####################################################################################
#required to build code
cd /homes/31/jc165798/SCRIPTS/sdmtools/working.dir/point.in.polygon

R CMD SHLIB pip.c


####################################################################################
#the function

dyn.load("/homes/31/jc165798/SCRIPTS/sdmtools/working.dir/point.in.polygon/pip.so")

point.in.polygon = function(pnts,poly)	{
	#check if pnts & poly is 2 column matrix or dataframe
	pnts = as.matrix(pnts); poly = as.matrix(poly)
	if (!(is.matrix(pnts) & is.matrix(poly))) stop('pnts & poly must be a 2 column dataframe or matrix')
	if (!(dim(pnts)[2] == 2 & dim(poly)[2] == 2)) stop('pnts & poly must be a 2 column dataframe or matrix')
	
	#ensure first and last polygon points are NOT the same
	if (poly[1,1] == poly[nrow(poly),1] & poly[1,2] == poly[nrow(poly),2]) poly = poly[-1,]
	
	#run the point in polygon code
	out = .Call('pip',pnts[,1],pnts[,2],nrow(pnts),poly[,1],poly[,2],nrow(poly))
	out = data.frame(pnts,pip=out)
	
	#return the value
	return(out)
}

####################################################################################
#Examples

pnts = cbind(x=c(1.5,3,1.5,2,2.5,3,4,3.25),y=c(1.5,1.5,2.5,3,3.5,3.5,4.5,2.25))
poly = cbind(x=c(2,3,3.5,3.5,3,4,5,4,5,5,4,3,3,3,2,2,1,1,1,1,2),y=c(1,2,2.5,2,2,1,2,3,4,5,4,5,4,3,3,4,5,4,3,2,2))

out = point.in.polygon(pnts,poly)

