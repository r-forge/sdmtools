#drafted by Jeremy VanDerWal ( jjvanderwal@gmail.com ... www.jjvanderwal.com )
#GNU General Public License .. feel free to use / distribute ... no warranties

################################################################################
####################################################################################
#required to build code OUTSIDE R
cd /home1/31/jc165798/SCRIPTS/sdmcode/misc/ben.phillips/individual.genetic.models/

R CMD SHLIB Functions.c


####################################################################################
#START R
#the function
dyn.load("/home1/31/jc165798/SCRIPTS/sdmcode/misc/ben.phillips/individual.genetic.models/Functions.so")
system.time( .Call('mother',R_n=10000,R_spX=10,R_spY=10,R_lambda=5.0,R_K=1000,R_disp_cost=0,R_reports_rate=10,R_num_gens=100,rho=environment()) )
