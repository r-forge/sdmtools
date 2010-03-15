/* File: CS.c */ 
/*
this is code to calculate class -based landscape statistics
*/
#include <R.h> 
#include <Rinternals.h>

//global variables
int nrow, ncol;
int *data; 
double *out;
SEXP ans;

/* 
tdata is a matrix of binary data 0 for background and 1 for foreground
*/

SEXP CS(SEXP tdata) 
	{
	//define the pointers for the data
	PROTECT(tdata = coerceVector(tdata, INTSXP));
	data = INTEGER(tdata); //this is a binary matrix of data
	int *dims = INTEGER(coerceVector(getAttrib(tdata, R_DimSymbol), INTSXP)); //get the dimension of the input matrix
    nrow = dims[0]; ncol = dims[1]; //assign the number of rows and columns in the matrix
	
	//setup the output matrix
	PROTECT(ans = allocVector(REALSXP, 2));
	out = REAL(ans); //pointer to output dataset
	
	//cycle through the input matrix and count perimter values excluding boundaries for the AI
	int gii = 0, aii = 0; //aii is the number of cells in the class, gii is the number of shared edges within a class type
	int row, col;
	//first cycle through all but the last column and last row
	for (row=0; row<nrow-1; row++)	{
		for (col=0; col<ncol-1; col++)	{	
			if (data[row+nrow*col]==1)	{
				aii ++;
				if (data[row+1+nrow*col]==1) gii++;
				if (data[row+nrow*(col+1)]==1) gii++;
			}
		}	
	}
	//cycle through the last row
	row = nrow-1;
	for (col=0; col<ncol-1; col++)	{
		if (data[row+nrow*col]==1)	{
			aii ++; 
			if (data[row+nrow*(col+1)]==1) gii++;
		}	
	}
	//cycle through the last column
	col = ncol-1;
	for (row=0; row<nrow-1; row++)	{
		if (data[row+nrow*col]==1)	{
			aii ++;
			if (data[row+1+nrow*col]==1) gii++;
		}	
	}
	//check the last cell
	col = ncol-1;row = nrow-1;
	if (data[row+nrow*col]==1) aii++;
	
	//calculate AI aggregation index
	double n = trunc(sqrt(aii));
	double m = aii - n*n;
	double maxii = 0.0; //maximum number of within class connections for AI
	if (m==0) maxii = 2 * n * (n-1);
	if (m==n) maxii = 2 * n * (n-1) + 2 * m - 1;
	if (m>n) maxii = 2 * n * (n-1) + 2 * m - 2;
	double AI = gii / maxii;
	
	//printf("aii: %d gii: %d maxii: %4.2f\n",aii,gii,maxii);
	
	//define the output
	out[0] = AI;
	
	//return the output data
	UNPROTECT(2);
    return(ans); 

}
