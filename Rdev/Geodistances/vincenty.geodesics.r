#this is code for simulated annealing optimization

####################################################################################
#required to build code
cd /homes/31/jc165798/SCRIPTS/R.development/Geodistances/

R CMD SHLIB vincenty.geodesics.c


####################################################################################
#the function
dyn.load("/homes/31/jc165798/SCRIPTS/R.development/Geodistances/vincenty.geodesics.so")

#given a location east / north or lat lon, a bearing & distance, what is the destination
#y is latitude / northing in decimal degrees
#x is longitude / easting in decimal degrees
#bearing is the bearing of interest in decimal degrees
#distance is the distance in m
destination = function(lat, lon, bearing, distance) {
	#check the data
	if (length(lat)!=length(lon)) stop('lat & lon must be of the same length')
	if (any(lon < -180) | any(lon > 180)) stop('lon must be decimal degrees between 0 & 360')
	if (any(lat < -90) | any(lat > 90)) stop('lat must be decimal degrees between -90 & 90')
	if (length(lat)==1) {
		out = expand.grid(bearing=bearing,distance=distance)
		out = data.frame(lon1=lon,lat1=lat,out,lon2=NA,lat2=NA)
	} else {
		if (length(bearing)>1) { if (length(bearing)!=length(lat)) stop('number of bearing values is not the same length as lon & lat') } else { bearing = rep(bearing,length(lat)) }
		if (length(distance)>1) { if (length(distance)!=length(lat)) stop('number of distance values is not the same length as lon & lat') } else { distance = rep(distance,length(lat)) }
		out = data.frame(lon1=lon,lat1=lat,bearing=bearing,distance=distance,lon2=NA,lat2=NA)
	}
	#clatcle through and output the new data
	for (ii in 1:nrow(out)) {
		tt = .Call('dest',out$lat1[ii],out$lon1[ii],out$bearing[ii],out$distance[ii])
		out$lon2[ii] = tt[2]; out$lat2[ii] = tt[1]
	}
	#return the output
	return(out)
}


####################################################################################
#Examples

###single lat lons
lats = -85; lons = 165
#single bearing & single distance
destination(lats,lons,bearing=180,distance=500000)

#multiple bearings
destination(lats,lons,bearing=seq(0,360,length.out=9),distance=500000)

#multiple bearings
destination(lats,lons,bearing=45,distance=seq(0,5000000,length.out=11))

#multiple bearings, multiple distances
destination(lats,lons,bearing=seq(0,360,length.out=9),distance=seq(0,5000000,length.out=11))

###multiple lat lons
lats = seq(-90,90,length.out=9); lons = seq(-180,180,length.out=9)

#multiple lat lons but single bearings / distances
destination(lats,lons,bearing=45,distance=500000)

#different bearings for each lat lon
destination(lats,lons,bearing=seq(0,360,length.out=9),distance=500000)

#different distances for each lat lon
destination(lats,lons,bearing=45,distance=seq(0,5000000,length.out=9))

#different bearings & distances for each lat lon
destination(lats,lons,bearing=seq(0,360,length.out=9),distance=seq(0,5000000,length.out=9))
