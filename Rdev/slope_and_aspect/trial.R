#drafted by Jeremy VanDerWal ( jjvanderwal@gmail.com ... www.jjvanderwal.com )
#GNU General Public License .. feel free to use / distribute ... no warranties

################################################################################
####################################################################################
#required to build code OUTSIDE R
cd /home1/31/jc165798/SCRIPTS/sdmtools/Rdev/slope_and_aspect/

R CMD SHLIB slope.aspect.c


####################################################################################
#START R
#the function
dyn.load("/home1/31/jc165798/SCRIPTS/sdmtools/Rdev/slope_and_aspect/slope.aspect.so")
library(SDMTools)
tasc = as.asc(matrix(1:50,nr=10,nc=5),yll=75); print(tasc)
slope = function(mat,latlon=FALSE) {
	#check input for class for returning info
	if (any(class(mat) == 'asc')) { attrib = attributes(mat)
	} else if (any(class(mat) %in% 'RasterLayer')) { attrib = mat; mat = asc.from.raster(mat)
	} else if (any(class(mat) == 'SpatialGridDataFrame')) { attrib = mat; mat = asc.from.sp(mat)
	} else { attrib = attributes(mat) }
	if (!any(class(mat) == 'asc')) { stop('objects must be of class "asc"') } #check to ensure asc 
	
	# get the cell size information
	if (latlon) {
		tt = grid.info(getXYcoords(mat)$y,attr(mat,'cellsize')) #if latlon = true get the length & width of cells
		width = rowMeans(cbind(tt$top,tt$bottom))/1000; height = tt$side/1000 #get the width and height of the cells in km (NOT m)
	} else { width = height = rep(attr(mat,'cellsize'),length(getXYcoords(mat)$y)) } #get the cell width & height 
	
	slop = t(mat[,dim(mat)[2]:1])
	slop = .Call('slope',slop,width,height) #get the slope information
	mat[,] = t(slop[dim(slop)[1]:1,]) #move all slope info to mat
	
	#reset the attributes of the input
	if (any(class(attrib) %in% 'RasterLayer')) { attrib = setValues(attrib, as.vector(t(t(unclass(mat))[dim(mat)[2]:1,]))); return(attrib)
	} else if (any(class(attrib) == 'SpatialGridDataFrame')) { attrib@data[1] = as.vector(unclass(mat)[,dim(mat)[2]:1]); return(attrib)
	} else { attributes(mat) = attrib; return(mat) }
}
tasc[,]
system.time({ tt = slope(tasc,latlon=TRUE) })
tt[,]

aspect = function(mat,latlon=FALSE) {
	#check input for class for returning info
	if (any(class(mat) == 'asc')) { attrib = attributes(mat)
	} else if (any(class(mat) %in% 'RasterLayer')) { attrib = mat; mat = asc.from.raster(mat)
	} else if (any(class(mat) == 'SpatialGridDataFrame')) { attrib = mat; mat = asc.from.sp(mat)
	} else { attrib = attributes(mat) }
	if (!any(class(mat) == 'asc')) { stop('objects must be of class "asc"') } #check to ensure asc 
	
	# get the cell size information
	if (latlon) {
		tt = grid.info(getXYcoords(mat)$y,attr(mat,'cellsize')) #if latlon = true get the length & width of cells
		width = rowMeans(cbind(tt$top,tt$bottom))/1000; height = tt$side/1000 #get the width and height of the cells in km (NOT m)
	} else { width = height = rep(attr(mat,'cellsize'),length(getXYcoords(mat)$y)) } #get the cell width & height 
	
	asp = t(mat[,dim(mat)[2]:1]) #reset the grid so that [1,1] is the North West corner (not the default of lower-left with reversed lat & lon)
	asp = .Call('aspect',asp,width,height) #get the aspect information
	mat[,] = t(asp[dim(asp)[1]:1,]) #move all aspect info to mat

	#reset the attributes of the input
	if (any(class(attrib) %in% 'RasterLayer')) { attrib = setValues(attrib, as.vector(t(t(unclass(mat))[dim(mat)[2]:1,]))); return(attrib)
	} else if (any(class(attrib) == 'SpatialGridDataFrame')) { attrib@data[1] = as.vector(unclass(mat)[,dim(mat)[2]:1]); return(attrib)
	} else { attributes(mat) = attrib; return(mat) }
}
tasc[,]
system.time({ tt = aspect(tasc,latlon=TRUE) })
tt[,]


tasc = as.asc(matrix(rep(1:5,10),nr=10,nc=5,byrow=TRUE),yll=75); print(tasc)

