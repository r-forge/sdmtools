#this is code for simulated annealing optimization

####################################################################################
#required to build code
cd /home1/31/jc165798/SCRIPTS/R.development/simulated.annealing/

R CMD SHLIB sann.c


####################################################################################
#the function
dyn.load("/home1/31/jc165798/SCRIPTS/R.development/simulated.annealing/sann.so")

sann <- function (start.seq, fn, gr = NULL, maxit=10000, REPORT=10, tmax=10, temp=10) {
    fn1 <- function(start.seq) fn(start.seq) #the function to be optimized and the prameters for it
    gr1 <- if (!is.null(gr)) { function(start.seq) gr(start.seq) } #define the fuction to replace pnts
    res <- .Call('sann',start.seq, fn1, gr1, maxit, REPORT, tmax, temp, new.env())
    names(res) <- c("sequence", "value")
    return(res)
}

####################################################################################
#Examples

## Combinatorial optimization: Traveling salesman problem
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

#multiple runs
loc <- cmdscale(eurodist)
rx <- range(x <- loc[,1])
ry <- range(y <- -loc[,2])
tspinit <- loc[sq,]
plot(x, y, type="n", asp=1, xlab="", ylab="", main="initial solution of traveling salesman problem")
arrows(tspinit[s,1], -tspinit[s,2], tspinit[s+1,1], -tspinit[s+1,2], angle=10, col="green")
text(x, y, labels(eurodist), cex=0.8)
legend('topleft',legend=paste("distance=",round(distance(sq)),'km',sep=''),bty='n')

for (ii in 1:10) {

res <- sann(sq, distance, genseq, maxit=30000, REPORT=500, temp=2000)

tspres <- loc[res$sequence,]
s <- seq(NROW(tspres)-1)

plot(x, y, type="n", asp=1, xlab="", ylab="", main=paste("simulated annealing solution ",ii,sep=''))
arrows(tspres[s,1], -tspres[s,2], tspres[s+1,1], -tspres[s+1,2], angle=10, col="red")
text(x, y, labels(eurodist), cex=0.8)
legend('topleft',legend=paste("distance=",round(res$value),'km',sep=''),bty='n')

}
dev.off()
