#this is a function to work out a simulated annealing optimizaiton

#start.seq is the initial starting sequence of values
#fn is the function to be optimized
#gr is the function for selection of new points in the sequence
#maxit is the maximum number of iteration
#REPORT is how frequently (number of iterations) status information is reported to the screen
#tmax is the maximum number of trials per temperature
#temp is the starting temperature

sann <- function (start.seq, fn, gr, maxit=10000, REPORT=10, tmax=10, temp=10) {
	#do some checks
	if (!(is.function(fn) & is.function(gr))) stop('fn & gr must be functions')
	
	#do the work
    fn1 <- function(start.seq) fn(start.seq) #the function to be optimized and the prameters for it
    gr1 <- if (!is.null(gr)) { function(start.seq) gr(start.seq) } #define the fuction to replace pnts
    res <- .Call('sann',start.seq, fn1, gr1, maxit, REPORT, tmax, temp, new.env())
    names(res) <- c("sequence", "value")
    return(res)
}
