
cd /homes/31/jc165798/working/R.fragments

R CMD SHLIB ccl4.c


#################################################################################

setwd('/homes/31/jc165798/working/R.fragments')
dyn.load("/homes/31/jc165798/working/R.fragments/ccl4.so")

tmat = matrix(c(	0,0,0,1,0,0,1,1,0,1,
					0,0,1,0,1,0,0,0,0,0,
					0,1,NA,1,0,1,0,0,0,1,
					1,0,1,1,1,0,1,0,0,1,
					0,1,0,1,0,1,0,0,0,1,
					0,0,1,0,1,0,0,1,1,0,
					1,0,0,1,0,0,1,0,0,1,
					0,1,0,0,0,1,0,0,0,1,
					0,0,1,1,1,0,0,0,0,1,
					1,1,1,0,0,0,0,0,0,1),nr=10,byrow=T)
tmat
.Call('ccl',tmat)

tmat = matrix(c(	0,1,0,0,0,1,0,0,0,1,
					0,0,1,0,0,1,0,0,1,0,
					0,0,0,1,0,1,0,1,0,0,
					0,0,0,1,0,1,0,1,0,0,
					0,0,0,1,1,0,1,1,0,0,
					0,0,0,0,0,1,0,0,0,0,
					1,1,1,1,1,0,1,1,0,0,
					0,0,0,0,0,0,0,1,0,1,
					1,1,1,1,1,1,1,1,0,1,
					0,0,0,0,0,0,0,1,1,1),nr=10,byrow=T)

.Call('ccl',tmat)

library(SDMTools,lib.loc='/homes/31/jc165798/R_libraries')
#tasc = read.asc('testdata/rf_00000.asc')
tasc = read.asc('testdata/ABT.asc')
tasc[which(tasc>=0.2 & is.finite(tasc))] = 1
tasc[which(tasc<0.2 & is.finite(tasc))] = 0
tout = .Call('ccl',tasc)

unique(as.vector(tout))
tt = as.vector(tout)
tdata = aggregate(tt,by=list(tt),length)
names(tdata) = c('label','count')
plot(tdata$label[-1],tdata$count[-1]);dev.off()

tdata$colors = c('grey',terrain.colors(nrow(tdata)-1))
tdata[order(tdata$count,decreasing=T)[1:10],]
tdata$colors[635]='red'
tdata$colors[8200]='blue'
tdata$colors[55]='green'  
tdata$colors[948]='yellow' 
tdata$colors[4364]='brown'
tdata$colors[377]='orange' 

t3=tasc
t3[,] = tout[,]
tout=t3;rm(t3)

png(filename = "tt.png",width=3000, height=3000)
	par(mfrow=c(1,2))
	image(tasc,col=c('grey','red'))
	image(tout,col=tdata$colors)
dev.off()



png(filename = "tt.png",width=2000, height=1000)
par(mfrow=c(1,2))
image(tasc[1000:2000,1000:2000])
image(tout[1000:2000,1000:2000])
dev.off()

 png(filename = "tout.png")

dev.off()
