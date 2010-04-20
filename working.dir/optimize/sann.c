/* File: sann.c */ 
/*
this is is modifying the sann optimization code from the optim package to 
permit selection of new points with an n that differs from the starting set.

*/

#include <R.h> 
#include <Rinternals.h>
#include <R_ext/Applic.h>
#include <R_ext/Random.h>	/* for the random number generation in samin() */
#include <R_ext/Applic.h>	/* setulb() */

static SEXP getListElement(SEXP list, char *str)
{
    SEXP elmt = R_NilValue, names = getAttrib(list, R_NamesSymbol);
    int i;

    for (i = 0; i < length(list); i++)
	if (strcmp(CHAR(STRING_ELT(names, i)), str) == 0) {
	    elmt = VECTOR_ELT(list, i);
	    break;
	}
    return elmt;
}

static double * vect(int n)
{
    return (double *)R_alloc(n, sizeof(double));
}

//define some global variables
SEXP R_fcall, R_gcall, R_env; //function to be minimized, function to select new points, R environment pointer

static double fminfn(int n, double *p)
{
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

static SEXP genptry(int n, double *p)
{
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

 
#define big             1.0e+35   /*a very large number*/
#define E1 1.7182818  /* exp(1.0)-1.0 */


/* Given a starting point pb[0..n-1], simulated annealing minimization
   is performed on the function fminfn. The starting temperature
   is input as ti. To make sann work silently set trace to zero.
   sann makes in total maxit function evaluations, tmax
   evaluations at each temperature. Returned quantities are pb
   (the location of the minimum), and yb (the minimum value of
   the function func).  Author: Adrian Trapletti
*/

/* par fn gr method options */
SEXP sann(SEXP par, SEXP fn, SEXP gr, SEXP con, SEXP rho)
{
    //protect R_fcall, R_gcall & R_env
	PROTECT(R_fcall = lang2(fn, R_NilValue));
	PROTECT(R_gcall = lang2(gr, R_NilValue));
	R_env = rho;
	
	//define the variables
	SEXP res, value, counts, conv;
	int i, trace, maxit, fncount, grcount, tmax;
	double *dpar, val, temp;
	int ifail = 0;	
	
	//get some information
	int npar = LENGTH(par);
	dpar = vect(npar); //starting points
	for (i = 0; i < npar; i++) dpar[i] = REAL(par)[i]; //populate the starting points
	
	//setup some parameters of the sann and reporting
	trace = asInteger(getListElement(con, "trace"));
    maxit = asInteger(getListElement(con, "maxit"));
	tmax = asInteger(getListElement(con, "tmax"));
	temp = asReal(getListElement(con, "temp"));
	if (trace) trace = asInteger(getListElement(con, "REPORT"));
	
	//do the simulated annealing analysis
	long j;
    int k, its, itdoc;
    double t, dy, ytry;
    double *ptry;

    GetRNGstate();
    val = fminfn(npar, dpar);  /* init best system state */
	double bestval = val; double *bestpnt; bestpnt = vect(npar); for (i = 0; i < npar; i++) bestpnt[i] = REAL(par)[i];
    if (!R_FINITE(val)) val = big;
    if (trace) { Rprintf ("sann objective function values\n"); Rprintf ("initial       value %f\n", val); }
    its = itdoc = 1;
    while (its < maxit) {  /* cool down system */
		t = temp/log((double)its + E1);  /* temperature annealing schedule */
		k = 1;
		while ((k <= tmax) && (its < maxit)) { /* iterate at constant temperature */
			SEXP tmp_pnt = genptry(npar, dpar);  /* generate new candidate point */
			tmp_pnt = coerceVector(tmp_pnt, REALSXP); // the temporary new points
			int tmp_n = LENGTH(tmp_pnt); // get the new length in number of points
			ptry = vect(tmp_n); for (i = 0; i < tmp_n; i++) ptry[i] = REAL(tmp_pnt)[i];
			ytry = fminfn (tmp_n, ptry);
			if (!R_FINITE(ytry)) ytry = big;
			dy = ytry - val;
			if ((dy <= 0.0) || (unif_rand() < exp(-dy/t))) {  /* accept new point? */
				dpar = vect(tmp_n); for (j = 0; j < tmp_n; j++) dpar[j] = ptry[j];
				npar = tmp_n;
				val = ytry;  /* update system state p, y */
				if (val <= bestval) { /* if system state is best, then update best system state pb, *yb */
					bestval = val;
					bestpnt = vect(npar); for (i = 0; i < npar; i++) bestpnt[i] = dpar[i];
				}
			}
			its++; k++;
		}
		if (trace && ((itdoc % trace) == 0)) Rprintf("iter %8d value %f\n", its - 1, val);
		itdoc++;
		Rprintf ("iteration %i\n", its);
    }
    if (trace) { Rprintf ("final         value %f\n", val); Rprintf ("sann stopped after %d iterations\n", its - 1); }
    PutRNGstate();
	
	////////////////////////
	//min_sann(npar, dpar, &val, maxit, tmax, temp, trace);
	SEXP ans;
	PROTECT(ans = allocVector(REALSXP, npar));
	for (i = 0; i < npar; i++) REAL(ans)[i] = dpar[i];
	Rprintf ("DONE %i\n", val);
	fncount = npar > 0 ? maxit : 1;
	grcount = NA_INTEGER;

	PROTECT(res = allocVector(VECSXP, 5));
    PROTECT(value = allocVector(REALSXP, 1));
    PROTECT(counts = allocVector(INTSXP, 2));
    PROTECT(conv = allocVector(INTSXP, 1));
	
	
    REAL(value)[0] = val;
    SET_VECTOR_ELT(res, 0, ans); SET_VECTOR_ELT(res, 1, value);
    INTEGER(counts)[0] = fncount; INTEGER(counts)[1] = grcount;
    SET_VECTOR_ELT(res, 2, counts);
    INTEGER(conv)[0] = ifail;
    SET_VECTOR_ELT(res, 3, conv);
    UNPROTECT(7);
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

