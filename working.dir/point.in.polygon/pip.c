/* File: pip.c */ 
/*
this is code for extracting information about point in polygon..


*/
#include <R.h> 
#include <Rinternals.h>

double TWOPI = 2 * PI;
double epsilon = 0.000000001; // threshold value

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
	double x, x1, x2, y, y1, y2;
	
	
	//cycle through the points
	for (ii=0;ii<npt;ii++) {
		printf("point... %d \n", ii);
		//cycle through the polygon vertices and sum the angles
		double angle = 0.0;
		for (jj=0;jj<npl;jj++) {
			//define the points
			x1 = plx[jj]; x2 = plx[(jj+1) % npl]; x = ptx[ii];
			y1 = ply[jj]; y2 = ply[(jj+1) % npl]; y = pty[ii];
			//check if point are vertix
			if (x == x1 && y == y1) { angle = PI+1; break; }
			//check if point is on border between 2 points
			if (x == x1 && x == x2) { if ((y1 <= y && y <= y2) || (y1 >= y && y >= y2)) { angle = PI+1; break; } } // check point between two horizontal points
			if (y == y1 && y == y2) { if ((x1 <= x && x <= x2) || (x1 >= x && x >= x2)) { angle = PI+1; break; } } // check point between two verticle points
			if ((y1-y2)/(x1-x2) == (y1-y)/(x1-x)) { //check if points have same slope and then within bounding box of polygon points
				if (((x1 <= x && x <= x2) || (x1 >= x && x >= x2)) && ((y1 <= y && y <= y2) || (y1 >= y && y >= y2))) { angle = PI+1; break; }
			}
			
			double theta1 = atan2(y1 - y, x1 - x);
			double theta2 = atan2(y2 - y, x2 - x);
			double dtheta = theta2 - theta1;

			while (dtheta > PI) dtheta -= TWOPI;
			while (dtheta < -PI) dtheta += TWOPI;
			angle += dtheta;

		}
		//write out if point is in polygon
		if (abs(angle) < PI) { out[ii] = 0; } else { out[ii] = 1; }
	}
	
	//return the output data
	UNPROTECT(7);
    return(ans); 

}
