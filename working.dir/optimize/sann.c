/* File: sann.c */ 
/*
this is is modifying the sann optimization code from the optim package to 
permit selection of new points with an n that differs from the starting set.

*/

#include <R.h> 
#include <Rinternals.h>
#include <R_ext/Applic.h>

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

