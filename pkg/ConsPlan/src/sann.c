/* File: sann.c */ 
/*
this is is modifying the sann optimization code from the optim package to 
permit selection of new points with an n that differs from the starting set.

*/

#include <R.h> 
#include <Rinternals.h>

#define big             1.0e+35   /*a very large number*/
#define E1 1.7182818  /* exp(1.0)-1.0 */


//method to create a vector of data
static double * vect(int n) {
    return (double *)R_alloc(n, sizeof(double));
}

//define some global variables
SEXP R_fcall, R_gcall, R_env; //function to be minimized, function to select new points, R environment pointer

//function to evalute the function to be minimized
static double fminfn(int n, double *p) {
    SEXP s, x;
    int i;
    double val;
    PROTECT_INDEX ipx;

    PROTECT(x = allocVector(REALSXP, n));
    for (i = 0; i < n; i++) REAL(x)[i] = p[i];
    SETCADR(R_fcall, x);
    PROTECT_WITH_INDEX(s = eval(R_fcall, R_env), &ipx);
    REPROTECT(s = coerceVector(s, REALSXP), ipx);
    val = REAL(s)[0];
    UNPROTECT(2);
    return val;
}

//function to select new points to try
static SEXP genptry(int n, double *p) {
    SEXP s, x;
    int i;
    //PROTECT_INDEX ipx;

	/* user defined generation of candidate point */
	PROTECT(x = allocVector(REALSXP, n));
	for (i = 0; i < n; i++) REAL(x)[i] = p[i];
	SETCADR(R_gcall, x);
	//PROTECT_WITH_INDEX(s = eval(R_gcall, R_env), &ipx);
	s = eval(R_gcall, R_env);
	//REPROTECT(s = coerceVector(s, REALSXP), ipx);
	PROTECT(s = coerceVector(s, REALSXP));
	//n = LENGTH(s);
	//double *ptry = vect (n);
	//for (i = 0; i < n; i++) ptry[i] = REAL(s)[i];
	UNPROTECT(2);
	return(s);
 }

 
/* par fn gr method options */
SEXP sann(SEXP startseq, SEXP fn, SEXP gr, SEXP MAXIT, SEXP REPORT, SEXP TMAX, SEXP TEMP, SEXP rho)
{
    //protect R_fcall, R_gcall & R_env
	PROTECT(R_fcall = lang2(fn, R_NilValue));
	PROTECT(R_gcall = lang2(gr, R_NilValue));
	R_env = rho;
	//extract the other information
	int maxit = INTEGER(coerceVector(MAXIT,INTSXP))[0];
	int trace = INTEGER(coerceVector(REPORT,INTSXP))[0];
	int tmax = INTEGER(coerceVector(TMAX,INTSXP))[0];
	int temp = REAL(coerceVector(TEMP,REALSXP))[0];
	
	//define some variables
	double *pcur, *ptry, *pbest; //points of current, try and best sequences
	int ncur, ntry, nbest; //n of 'subsets' of current, try and best
	double rescur, restry, resbest; //results of current, try and best
	int ii, k, its, itdoc;
	double t, dres;
	
	//get the starting sequence and initial results
	ncur = LENGTH(startseq); 
	pcur = vect(ncur); for (ii = 0; ii < ncur; ii++) pcur[ii] = REAL(startseq)[ii];
	rescur = fminfn(ncur, pcur); if (!R_FINITE(rescur)) rescur = big; //ensure the result has some value that is finite
	
	//set the initial data as best
	nbest = ncur; 
	pbest = vect(nbest); for (ii = 0; ii < nbest; ii++) pbest[ii] = pcur[ii];
	resbest = rescur;
	Rprintf ("initial       value %f\n", resbest);
	
	//start the simulated annealling
	GetRNGstate();
	its = itdoc = 1;
    while (its < maxit) {  //* cool down system 
		t = temp/log((double)its + E1);  // temperature annealing schedule 
		k = 1;
		while ((k <= tmax) && (its < maxit)) { // iterate at constant temperature
			SEXP tmp_pnt = genptry(ncur, pcur);  // generate new candidate point
			tmp_pnt = coerceVector(tmp_pnt, REALSXP); // the temporary new points
			ntry = LENGTH(tmp_pnt); // get the new length in number of points
			ptry = vect(ntry); for (ii = 0; ii < ntry; ii++) ptry[ii] = REAL(tmp_pnt)[ii];
			restry = fminfn (ntry, ptry); if (!R_FINITE(restry)) restry = big; //get the results of the trial points
			dres = restry - rescur;
			if ((dres <= 0.0) || (unif_rand() < exp(-dres/t))) {  // accept new point?
				ncur = ntry; // update system state
				pcur = vect(ncur); for (ii = 0; ii < ncur; ii++) pcur[ii] = ptry[ii];
				rescur = restry;  
				if (rescur <= resbest) { // if system state is best, then update best system state
					resbest = rescur;
					nbest = ncur;
					pbest = vect(nbest); for (ii = 0; ii < nbest; ii++) pbest[ii] = pcur[ii];
				}
			}
			its++; k++;
		}
		if (((itdoc % trace) == 0)) Rprintf("iter %8d value %f\n", its - 1, resbest);
		itdoc++;
		//Rprintf ("iteration %i\n", its);
    }
    Rprintf ("final         value %f\n", resbest); Rprintf ("sann stopped after %d iterations\n", its - 1);
	PutRNGstate();
	
	//prepare the outputs
	SEXP ans, res, value;
	PROTECT(ans = allocVector(REALSXP, nbest));
	for (ii = 0; ii < nbest; ii++) REAL(ans)[ii] = pbest[ii];

	PROTECT(res = allocVector(VECSXP, 2));
    PROTECT(value = allocVector(REALSXP, 1));
	
    REAL(value)[0] = resbest;
    SET_VECTOR_ELT(res, 0, ans); SET_VECTOR_ELT(res, 1, value);
    UNPROTECT(5);
    return res;
}

#undef E1

/*
SEXP trial(SEXP p, SEXP fn, SEXP rho) {
	//define the function and output
	SEXP R_fcall;
    int i;
	int n = LENGTH(p); //get the length of p
	SEXP s, x;
	PROTECT_INDEX ipx; //not sure what this does
	PROTECT(p = coerceVector(p,INTSXP));
	int *tp = INTEGER(p);
    PROTECT(x = allocVector(REALSXP, n)); //create a vector the same length as p
	PROTECT(R_fcall = lang2(fn, R_NilValue));
	

    for (i = 0; i < n; i++) { //copy data ensuring it is finite
		REAL(x)[i] = tp[i];
    }

    SETCADR(R_fcall, x);
    PROTECT_WITH_INDEX(s = eval(R_fcall, rho), &ipx);
    REPROTECT(s = coerceVector(s, REALSXP), ipx);
    UNPROTECT(4);
    return s;

}
*/


// void min_sann(int n, double *pb, double *yb, int maxit, int tmax, double ti, int trace)
// {
    // long j;
    // int k, its, itdoc;
    // double t, y, dy, ytry, scale;
    // double *p, *dp, *ptry;

    // if(n == 0) { /* don't even attempt to optimize */
		// *yb = fminfn(n, pb);
		// return;
    // }
    // p = vect (n); dp = vect (n); ptry = vect (n);
    // GetRNGstate();
    // *yb = fminfn (n, pb);  /* init best system state pb, *yb */
    // if (!R_FINITE(*yb)) *yb = big;
    // for (j = 0; j < n; j++) p[j] = pb[j];
    // y = *yb;  /* init system state p, y */
    // if (trace) {
	// Rprintf ("sann objective function values\n");
	// Rprintf ("initial       value %f\n", *yb);
    // }
    // scale = 1.0/ti;
    // its = itdoc = 1;
    // while (its < maxit) {  /* cool down system */
	// t = ti/log((double)its + E1);  /* temperature annealing schedule */
	// k = 1;
	// while ((k <= tmax) && (its < maxit))  /* iterate at constant temperature */
	// {
	    // ptry = genptry(n, p);  /* generate new candidate point */
	    // ytry = fminfn (n, ptry);
	    // if (!R_FINITE(ytry)) ytry = big;
	    // dy = ytry - y;
	    // if ((dy <= 0.0) || (unif_rand() < exp(-dy/t))) {  /* accept new point? */
		// for (j = 0; j < n; j++) p[j] = ptry[j];
		// y = ytry;  /* update system state p, y */
		// if (y <= *yb)  /* if system state is best, then update best system state pb, *yb */
		// {
		    // for (j = 0; j < n; j++) pb[j] = p[j];
		    // *yb = y;
		// }
	    // }
	    // its++; k++;
	// }
	// if (trace && ((itdoc % trace) == 0))
	    // Rprintf("iter %8d value %f\n", its - 1, *yb);
	// itdoc++;
    // }
    // if (trace) {
	// Rprintf ("final         value %f\n", *yb);
	// Rprintf ("sann stopped after %d iterations\n", its - 1);
    // }
    // PutRNGstate();
// }

