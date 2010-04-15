#this is code for point in polygon algorithm.

####################################################################################
#required to build code
cd /homes/31/jc165798/SCRIPTS/sdmtools/working.dir/point.in.polygon

R CMD SHLIB pip.c


####################################################################################
#the function

dyn.load("/homes/31/jc165798/SCRIPTS/sdmtools/working.dir/point.in.polygon/pip.so")

point.in.polygon = function(pnts,poly.pnts)	{
	#check if pnts & poly is 2 column matrix or dataframe
	pnts = as.matrix(pnts); poly.pnts = as.matrix(poly.pnts)
	if (!(is.matrix(pnts) & is.matrix(poly.pnts))) stop('pnts & poly.pnts must be a 2 column dataframe or matrix')
	if (!(dim(pnts)[2] == 2 & dim(poly.pnts)[2] == 2)) stop('pnts & poly.pnts must be a 2 column dataframe or matrix')
	
	#ensure first and last polygon points are NOT the same
	if (poly.pnts[1,1] == poly.pnts[nrow(poly.pnts),1] & poly.pnts[1,2] == poly.pnts[nrow(poly.pnts),2]) poly.pnts = poly.pnts[-1,]
	
	#run the point in polygon code
	out = .Call('pip',pnts[,1],pnts[,2],nrow(pnts),poly.pnts[,1],poly.pnts[,2],nrow(poly.pnts))
	out = data.frame(pnts,pip=out)
	
	#return the value
	return(out)
}


####################################################################################
#Examples

#define the points and polygon
pnts = expand.grid(x=seq(1,6,0.1),y=seq(1,6,0.1))
polypnts = cbind(x=c(2,3,3.5,3.5,3,4,5,4,5,5,4,3,3,3,2,2,1,1,1,1,2),y=c(1,2,2.5,2,2,1,2,3,4,5,4,5,4,3,3,4,5,4,3,2,2))

#plot the polygon and all points to be checked
plot(rbind(polypnts, pnts))
polygon(polypnts,col='#99999990')

#create check which points fall within the polygon
out = point.in.polygon(pnts,polypnts)
head(out)

#identify points not in the polygon with an X
points(out[which(out$pip==0),1:2],pch='X')
dev.off()
