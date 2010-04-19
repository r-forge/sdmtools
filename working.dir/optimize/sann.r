#this is code for simulated annealing optimization

####################################################################################
#required to build code
cd /homes/31/jc165798/Rforge/working.dir/optimize/

R CMD SHLIB sann.c


####################################################################################
#the function

dyn.load("/homes/31/jc165798/Rforge/working.dir/optimize/sann.so")

fn = function(x) {return(length(x))}

.Call('trial',1:10,fn,new.env())


dyn.load("/homes/31/jc165798/Rforge/working.dir/optimize/sann.so")

sann <- function (par, fn, gr = NULL, ..., control = list()) {
    fn1 <- function(par) fn(par, ...) #the function to be optimized and the prameters for it
    gr1 <- if (!is.null(gr)) { function(par) gr(par, ...) } #define the fuction to replace pnts
    con <- list(trace=0, maxit=10000, REPORT=10, tmax=10, temp=10)  #default conditions
    nmsC <- names(con)
    con[(namc <- names(control))] <- control #alter the conditions by what was entered in the function call
    if (length(noNms <- namc[!namc %in% nmsC])) warning("unknown names in control: ", paste(noNms, collapse = ", ")) #check control names
    if (con$trace < 0) warning("read the documentation for 'trace' more carefully")
    if (con$trace && as.integer(con$REPORT) == 0) stop("'trace != 0' needs 'REPORT >= 1'")
    res <- .Call('sann',par, fn1, gr1, con)
    names(res) <- c("par", "value", "counts", "convergence","message")
    nm <- names(par)
    if (!is.null(nm)) names(res$par) <- nm
    names(res$counts) <- c("function", "gradient")
    res
}

####################################################################################
#Examples

#define the points and polygon
pnts = expand.grid(x=seq(1,6,0.1),y=seq(1,6,0.1))
polypnts = cbind(x=c(2,3,3.5,3.5,3,4,5,4,5,5,4,3,3,3,2,2,1,1,1,1,2),y=c(1,2,2.5,2,2,1,2,3,4,5,4,5,4,3,3,4,5,4,3,2,2))

#plot the polygon and all points to be checked
plot(rbind(polypnts, pnts))
polygon(polypnts,col='#99999990')

#create check which points fall within the polygon
out = point.in.polygon(pnts,polypnts)
head(out)

#identify points not in the polygon with an X
points(out[which(out$pip==0),1:2],pch='X')
dev.off()
