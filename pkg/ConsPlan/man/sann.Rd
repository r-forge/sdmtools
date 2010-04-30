\name{sann}
\Rdversion{1.1}
\alias{lcmw}
\title{ Simulated Anneal Optimization }

\description{
\code{sann} performs an optimization using a simulated annealing algorithm.
}

\usage{
sann(start.seq, fn, gr, maxit=10000, REPORT=10, tmax=10, temp=10)
}

\arguments{
  \item{start.seq}{ is a vector of values representing the initial starting sequence }
  \item{fn}{ is the function to be optimized }
  \item{gr}{ is the function for selection of new points in the sequence }
  \item{maxit}{ is the maximum number of iteration }
  \item{REPORT}{ is how frequently (number of iterations) status information is 
  reported to the screen }
  \item{tmax}{ is the maximum number of trials per temperature }
  \item{temp}{ is the starting temperature }
}

\details{
need to fill in!!!
}

\value{
a list containing the optimized sequence ($sequence) of values and the solution 
value ($value).
}

\author{Jeremy VanDerWal \email{jjvanderwal@gmail.com}}

\examples{

## Traveling salesman problem (modified from \code{optim})
library(stats) # normally loaded

eurodistmat <- as.matrix(eurodist)

distance <- function(sq) {  # Target function
	sq2 <- embed(sq, 2)
	return(as.numeric(sum(eurodistmat[cbind(sq2[,2],sq2[,1])])))
}

genseq <- function(sq) {  # Generate new candidate sequence
	idx <- seq(2, NROW(eurodistmat)-1, by=1)
	changepoints <- sample(idx, size=2, replace=FALSE)
	tmp <- sq[changepoints[1]]
	sq[changepoints[1]] <- sq[changepoints[2]]
	sq[changepoints[2]] <- tmp
	return(as.numeric(sq))
}

sq <- c(1,2:NROW(eurodistmat),1)  # Initial sequence
distance(sq)

set.seed(123) # chosen to get a good soln relatively quickly
res <- sann(sq, distance, genseq, maxit=30000, REPORT=500, temp=2000)
res  # Near optimum distance around 12842

loc <- cmdscale(eurodist)
rx <- range(x <- loc[,1])
ry <- range(y <- -loc[,2])
tspinit <- loc[sq,]
tspres <- loc[res$sequence,]
s <- seq(NROW(tspres)-1)

plot(x, y, type="n", asp=1, xlab="", ylab="", main="initial solution of traveling salesman problem")
arrows(tspinit[s,1], -tspinit[s,2], tspinit[s+1,1], -tspinit[s+1,2], angle=10, col="green")
text(x, y, labels(eurodist), cex=0.8)

plot(x, y, type="n", asp=1, xlab="", ylab="", main="optim() 'solving' traveling salesman problem")
arrows(tspres[s,1], -tspres[s,2], tspres[s+1,1], -tspres[s+1,2], angle=10, col="red")
text(x, y, labels(eurodist), cex=0.8)


}


