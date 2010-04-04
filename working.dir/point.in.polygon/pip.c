/* File: pip.c */ 
/*
this is code for extracting information about point in polygon..


*/
#include <R.h> 
#include <Rinternals.h>

double TWOPI = 2 * PI;
double epsilon = 0.000000001; // threshold value

/*
   Return the angle between two vectors on a plane
   The angle is from vector 1 to vector 2, positive anticlockwise
   The result is between -pi -> pi
*/
double Angle2D(double x1, double y1, double x2, double y2)
{
	double dtheta,theta1,theta2;

	theta1 = atan2(y1,x1);
	theta2 = atan2(y2,x2);
	dtheta = theta2 - theta1;
	while (dtheta > PI) dtheta -= TWOPI;
	while (dtheta < -PI) dtheta += TWOPI;
	printf("n points ... %f \n", dtheta);
	return(dtheta);
}


SEXP pip(SEXP pntx, SEXP pnty, SEXP pntn, SEXP polyx, SEXP polyy, SEXP polyn)
{
	//define the pointers to the variables
	PROTECT(pntx = coerceVector(pntx, REALSXP)); double *ptx = REAL(pntx); // pnts x values
	PROTECT(pnty = coerceVector(pnty, REALSXP)); double *pty = REAL(pnty); // pnts y values
	PROTECT(pntn = coerceVector(pntn, INTSXP)); int npt = INTEGER(pntn)[0]; // number of points
	PROTECT(polyx = coerceVector(polyx, REALSXP)); double *plx = REAL(polyx); // polygon x values
	PROTECT(polyy = coerceVector(polyy, REALSXP)); double *ply = REAL(polyy); // polygon y values
	PROTECT(polyn = coerceVector(polyn, INTSXP)); int npl = INTEGER(polyn)[0]; // number of polygon points
	
	printf("n points ... %d \n", npt);
	printf("n poly points ... %d \n", npl);
	
	//define the output variables
	SEXP ans; int *out; 
	PROTECT(ans = allocVector(INTSXP, npt)); out = INTEGER(ans); //pointer to output dataset
	
	//define some other variables
	int ii, jj;
	double p1x, p2x, p1y, p2y;
	
	
	//cycle through the points
	for (ii=0;ii<npt;ii++) {
		printf("point... %d \n", ii);
		//cycle through the polygon vertices and sum the angles
		double angle = 0.0;
		for (jj=0;jj<npl;jj++) {
			if (ply[jj] == pty[ii] && plx[jj] == ptx[ii]) { angle = PI+1; break; }//if point is a vertex... set it as within the polygon
			if (ply[jj] - pty[ii] < epsilon && pty[ii] - ply[(jj+1) % npl] < epsilon) { // if points in horizontal line... point is in polygon
				angle = PI+1; break;
			} else if (plx[jj] - ptx[ii] < epsilon && ptx[ii] - plx[(jj+1) % npl] < epsilon) { //if points in verticle line... point is in polygon
				angle = PI+1; break;
			} else if ((ply[jj] - pty[ii])/(plx[jj] - ptx[ii]) - (ply[jj] - ply[(jj+1) % npl])/(plx[jj] - plx[(jj+1) % npl]) < epsilon ) { //if the slopes are the same... check if within bounding box
				if ((pty[ii] >= ply[jj] && pty[ii] <= ply[(jj+1) % npl]) ||(pty[ii] <= ply[jj] && pty[ii] >= ply[(jj+1) % npl]))			{
					if ((ptx[ii] >= plx[jj] && ptx[ii] <= plx[(jj+1) % npl]) || (ptx[ii] <= plx[jj] && ptx[ii] >= plx[(jj+1) % npl])) { //if point is within the bounding box of the points
						angle = PI+1; break; 
					}
				}			
			} else { // if not a vertex or part of the polygon, sum the angles
				p1y = ply[jj] - pty[ii];
				p2y = ply[(jj+1) % npl] - pty[ii];
				p1x = plx[jj] - ptx[ii];
				p2x = plx[(jj+1) % npl] - ptx[ii];
				
				double theta1 = atan2(ply[jj] - pty[ii],plx[jj] - ptx[ii]);
				double theta2 = atan2(ply[(jj+1) % npl] - pty[ii],plx[(jj+1) % npl] - ptx[ii]);
				double dtheta = theta2 - theta1;

				while (dtheta > PI) dtheta -= TWOPI;
				while (dtheta < -PI) dtheta += TWOPI;
				angle += dtheta;
			}
		}
		//write out if point is in polygon
		if (abs(angle) < PI) { out[ii] = 0; } else { out[ii] = 1; }
	}
	
	//return the output data
	UNPROTECT(7);
    return(ans); 

}
