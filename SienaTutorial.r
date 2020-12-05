              # SIENA tutorial using R for the Social Networks and Health 
#   workshop at Duke University on May 20, 2016
# Adapted from tutorial by Christian Steglich
#   http://www.gmw.rug.nl/~steglich/workshops/Groningen2015.htm

renv::init()
renv::snapshot()
##############################################
# Contents:
#     (1) preparatory steps
#     (2) inspect data
#     (3) preparing objects for RSiena
#     (4) model specification / selection
#         of effects
#     (5) model estimation
##############################################

# ==========================
# (1) preparatory steps in R
# ==========================

# if RSiena is not yet installed, un-hash & run the following:
install.packages("RSiena", repos="http://R-Forge.R-project.org")
# and for the RSienaTest package (optional)
install.packages("RSienaTest", repos="http://R-Forge.R-project.org")
# if difficulty loading RSiena from R-Forge, use the older CRAN version
install.packages("RSiena")

# load RSiena commands and sna to the R namespace:
library(RSiena)
library(sna)
library(rgl)  # for interactive 3d plot (optional)

# check that your version is relatvely recent
packageVersion("RSiena")

# set working directory to where you want to store output files:
setwd('/Users/davidschaefer/AeroFS/ASU/Teaching/Duke/lab/')


# ======================
# (2) inspection of data
# ======================

# The s50 data are part of the RSiena package. For information visit
#   http://www.stats.ox.ac.uk/~snijders/siena/s50_data.htm

# alcohol use:
dim(s50a)  # 50 students (rows), 3 time points (columns)
head(s50a, 10)  # first 10 rows of data
apply(s50a,FUN=table,MARGIN=2,useNA='always')  # freq. distribution by wave
# alcohol use is already coded as ordinal - GOOD! (required for dependent behaviors)

# alcohol change between subsequent observations:
table(s50a[,1],s50a[,2],useNA='always')
table(s50a[,2],s50a[,3],useNA='always')
# & total:
( totalChange <- table(s50a[,1],s50a[,3],useNA='always') ) 
sum(diag(totalChange)) / sum(totalChange)  # 50% at same level t1 & t3

# smoking: 
dim(s50s)  # 50 students (rows), 3 time points (columns)
head(s50s, 10)  # first 10 rows of data
apply(s50s,FUN=table,MARGIN=2,useNA='always')  # freq. distribution by wave

# create a fake attribute as an example later
( fakeAttr <- rep(c(0, 1), each=25) )

# networks:
dim(s501)  # adjacency matrix with 50 actors
table(s501,useNA='always')  # 113 ties, none missing
# converting the matrices to an array makes some things more efficient
s50all <- array(c(s501,s502,s503),dim=c(dim(s501),3))
dim(s50all)  # 3 50x50 adjacency matrices layered atop one another
apply(s50all, 3, function(x) table(x,useNA='always')) # column for each layer

# network change between subsequent observations:
table(s501,s502,useNA='always') 
# 2328 0's and 57 1's remained stable; 
# 59 0's became 1's; 56 1's became 0's
table(s502,s503,useNA='always')
# & total:
table(s501,s503,useNA='always')

# create colors to shade nodes by substance use
color5 <- c('gray95','gray70','gray50','gray30','gray5')  # light to dark for drinking 
color3 <- c('gray95','gray50','gray5')  # light to dark for smoking

# create and save a layout based on union of 3 networks.
# we will use the same layout to graph the network at each wave.
# rerun the g.layout until it is pleasing.
g.layout <- gplot(apply(s50all, c(1,2), max), usearrows=F)  

# plot each network with nodes shaded by accompanying alcohol use
par(mfrow=c(2,3))
gplot(s501, vertex.col=color5[s50a[,1]], arrowhead.cex=.75, edge.col='gray70', coord=g.layout, main='Drinking t1')
gplot(s502, vertex.col=color5[s50a[,2]], arrowhead.cex=.75, edge.col='gray70', coord=g.layout, main='Drinking t2')
gplot(s503, vertex.col=color5[s50a[,3]], arrowhead.cex=.75, edge.col='gray70', coord=g.layout, main='Drinking t3')
# smoking level
gplot(s501, vertex.col=color3[s50s[,1]], arrowhead.cex=.75, edge.col='gray70', coord=g.layout, main='Smoking t1')
gplot(s502, vertex.col=color3[s50s[,2]], arrowhead.cex=.75, edge.col='gray70', coord=g.layout, main='Smoking t2')
gplot(s503, vertex.col=color3[s50s[,3]], arrowhead.cex=.75, edge.col='gray70', coord=g.layout, main='Smoking t3')


# ================================
# (3) preparing objects for RSiena
# ================================

# networks must be stored as a matrix;
#   each matrix is the same size;
#   matrices are ultimately layered to form an array
# convert network to "dependent network variable" (RSiena object):
friendship <- sienaDependent(array(c(s501,s502,s503),
	dim=c(dim(s501),3)))

# behaviors to be modeled as an outcome must be a matrix
#   where rows are actors and columns are time points
class(s50a)  # alcohol use is already a matrix
# convert behavior to "dependent behavior variable" (RSiena object):
drinking <- sienaDependent(s50a,type="behavior")
smoking <- sienaDependent(s50s,type="behavior")

# attributes are identified independently
# our fake attribute is a constant covariate
fakeAttrCC <- coCovar(fakeAttr)

# bind data together for Siena analysis:
myDataDrink <- sienaDataCreate(friendship,drinking,fakeAttrCC)
myDataDrink

# we can have multiple behaviors
myDataDrinkSmoke <- sienaDataCreate(friendship,drinking,smoking,fakeAttrCC)
myDataDrinkSmoke

# we could have no behaviors
myData <- sienaDataCreate(friendship)
myData

# Any dependent objects specified during the binding step will be
#   included in the model by default. It is easiest to only list
#   networks or behaviors you intend to model.

# create a model specification object for the data:
myModelDrinkSmoke <- getEffects(myDataDrinkSmoke)
myModelDrinkSmoke

# optionally, write descriptive summary to protocol file (a good check):
print01Report(myDataDrinkSmoke,modelname='drinkSmoke160520')
# older software versions require CoEvolutionModel as 2nd argument


# ================================================
# (4) model specification / selection of effects
# ================================================

# why do we want to model? hypothesized effects? controls?

### TIME OUT TO DEVELOP HYPOTHESES ###

# inspect possible effects:
effectsDocumentation(myModelDrinkSmoke)
# Note how column "name" has three names (scroll way down),
#   one for each function in the model

# specify the model 

# Hyp: drinking is 'contagious' (i.e., peer influence)
myModelDrinkSmoke <- includeEffects(myModelDrinkSmoke,avSim,
	interaction1="friendship",name="drinking")
# instead of 'avSim' could use 'totSim', 'avAlt', or 'totAlt'

# Hyp: being lonely leads to drinking
myModelDrinkSmoke <- includeEffects(myModelDrinkSmoke,isolate,
	interaction1="friendship",name="drinking")
# instead of 'isolate' could use 'indegree' 

# Hyp: drinking helps one make friends (e.g., liquid courage)
myModelDrinkSmoke <- includeEffects(myModelDrinkSmoke,egoX,
	interaction1="drinking",name="friendship")

# Hyp: drinkers prefer friends who drink; non-drinkers prefer non-drinkers
#   (i.e., selection homophily)
myModelDrinkSmoke <- includeEffects(myModelDrinkSmoke,simX,
	interaction1="drinking",name="friendship")
# instead of 'simX' could use 'egoXaltX' 

# Hyp: drinking enhances (or reduces) popularity
myModelDrinkSmoke <- includeEffects(myModelDrinkSmoke,altX,
	interaction1="drinking",name="friendship")

# add structural effects as control variables in network function:
myModelDrinkSmoke <- includeEffects(myModelDrinkSmoke, recip, transTrip,
	transRecTrip, cycle3, name="friendship")

# control for selection on the fake (categorical) attrbute
myModelDrinkSmoke <- includeEffects(myModelDrinkSmoke, egoX, altX, sameX,
    interaction1='fakeAttrCC', name="friendship")

# control for effect of the fake attrbute on drinking
myModelDrinkSmoke <- includeEffects(myModelDrinkSmoke, effFrom,
    interaction1='fakeAttrCC', name="drinking")

# we could add effects for smoking too...

# full model specification looks like this now:
myModelDrinkSmoke
# note that smoking is in the model, even though we didn't specify effects

# ====================
# (5) model estimation
# ====================

# create an object with options for algorithm tuning:
modelOptions <- sienaAlgorithmCreate(
	projname='lab060520', MaxDegree=c(friendship=6),
	doubleAveraging=0, diagonalize=.2, seed=786840)  # the seed is optional

# estimate the model, you can increase estimation time by 
#   using more processors with the nbrNodes option
myResults <- siena07(modelOptions, data=myDataDrinkSmoke,
	effects= myModelDrinkSmoke, batch=FALSE, verbose=TRUE,
	useCluster=TRUE, nbrNodes=3)
myResults  # examine results

# Check that 'Overall maximum convergence ratio' is less than .25
# If not, try rerunning the model, using previous estimates as starting values
# To identify troublesome effects, look for high 'Convergence t-ratios' (>.1) 

myResults2 <- siena07(modelOptions, data=myDataDrinkSmoke,
	effects= myModelDrinkSmoke, batch=FALSE, verbose=TRUE,
	useCluster=TRUE, nbrNodes=3, 
	prevAns=myResults, returnDeps=T) # specify starting values, return simulated nets (for GOF)
myResults2  # examine results

myRes <- myResults2   # specify which results to use in next steps
# This allows us to use the same script without changing the results object name each time

# to give approximate p-values: 
parameter <- myRes$effects$effectName
estimate <- myRes $theta
st.error <- sqrt(diag(myRes $covtheta))
normal.variate <- estimate/st.error
p.value.2sided <- 2*pnorm(abs(normal.variate),lower.tail=FALSE)
data.frame(parameter,
	estimate=round(estimate,3),
	st.error=round(st.error,3),
	normal.variate=round(normal.variate,2),
	p.value=round(p.value.2sided,4)
)


# ====================
# (6) goodness of fit
# ====================

# Calculate the fit with respect to the indegree distribution.
# By specifying "verbose=TRUE" we get information on the screen telling us
#   how far calculations have progressed.
# (You may see a note about "method with signature CsparseMatrix# (etc.)
#   which you can ignore.)

# joint indegree
( gof.id <- sienaGOF(myRes, verbose=TRUE, varName="friendship", IndegreeDistribution,
     join=T, cumulative=F) )
plot(gof.id)

# joint indegree
( gof.od <- sienaGOF(myRes, verbose=TRUE, varName="friendship", OutdegreeDistribution,
     join=T, cumulative=F) )
plot(gof.od)

# More GOF functions
#   To run, FIRST run the functions at the end of this file
#   WARNING: some of these take a long time (may want to skip during lab)

# geodesic distances
( gof.gd <- sienaGOF(myRes, verbose=TRUE, varName="friendship", GeodesicDistribution,
     join=T, cumulative=F) )
plot(gof.gd)
# On the low side. There are too many short distances, not enough separation into components.

# triad census
(gof.tc <- sienaGOF(myRes, verbose=TRUE, varName="friendship", TriadCensus, join=T) )
plot(gof.tc, scale=TRUE, center=TRUE)

# eigenvector centralities
( gof.ev <- sienaGOF(myRes, verbose=TRUE, varName="friendship", EigenvalueDistribution,
     join=T, cumulative=F) )
plot(gof.ev)

# goodness of fit overall behaviour distribution
( gof.behaviour <- sienaGOF(myRes,BehaviorDistribution,
	verbose=TRUE,join=TRUE,varName="drinking") )
plot(gof.behaviour) # looks very good!

# two spatial autocorrelation coefficients
( gof.MoranGeary <- sienaGOF(myRes,MoranGeary,
	verbose=TRUE,join=FALSE,varName=c("friendship","drinking")) )
plot(gof.MoranGeary,period=1) # acceptable
plot(gof.MoranGeary,period=2) # acceptable

# Moran's I autocorrelation at a distance: 
( gof.Moran <- sienaGOF(myRes,Moran123,
	verbose=TRUE,join=TRUE,varName=c("friendship","drinking")) )
plot(gof.Moran) # acceptable
# note that coefficients are summed up over periods!

# same for Geary's c
( gof.Geary <- sienaGOF(sim.results,Geary123,
	verbose=TRUE,join=TRUE,varName=c("friendship","drinking")) )
plot(gof.Geary)	# acceptable
# note that coefficients are summed up over periods!

# ego-by-alter table for alcohol scores
( gof.EgoAlterTable <- sienaGOF(myRes,EgoAlterTable,
	verbose=TRUE,join=TRUE,varName=c("friendship","drinking")) )
plot(gof.EgoAlterTable,center=TRUE,scale=TRUE)	


# ======================
# (7) time heterogeneity
# ======================

tt <- sienaTimeTest(myRes)
summary(tt)
# see "Joint significance test", null hypothesis is time homogeneity 


# ======================
# (7) Interpretation
# ======================

# Ego-Alter Selection Table
# Interpret the effects of drinking on friend selection
# First define a function that incorporates the relevant part
#   of the evaluation function, dependent on the parameters b1, b2, b3,
#   the overall average v_av, the similarity average sim_av,
#   and the range ran_v
obj_n <- function(vi, vj){
  b1*(vi-v_av) + b2*(vj-v_av) + b3*(1 - abs(vi-vj)/ran_v - sim_av)
  }
# Now fill in the values of the parameter estimates and the averages (from the descriptive output)
v_av <- 3.113  # average drink 
sim_av <- 0.6744  # average similarity
ran_v <- 4   # range of values
b1 <- .0557  # drink ego
b2 <- -.0814  # drink alter
b3 <- 1.3071  # drink similarity

vv <- c(1, 2, 3, 4, 5)  # Define the values of v for which the table is to be given
sel_tab <- outer(vv, vv, obj_n)  # calculate the table
sel_tab  # display the table: rows are egos, columns are alters
round(sel_tab,3)  # round the values to increase readability

# plot the predicted contribution to the network function
nx <- 5
ny <- nx
hgt = 0.25 * (sel_tab[-nx,-ny] + sel_tab[-1,-ny] + sel_tab[-nx,-1] + sel_tab[-1,-1])
hgt = (hgt - min(hgt)) / (max(hgt) - min(hgt))
persp(vv, vv, sel_tab, col=heat.colors(32)[floor(31*hgt+1)], theta=20, phi=00, zlim=c(-1.5,1),
 xlab='Ego Drinking', ylab='Alter Drinking', zlab='Contribution to Network Function', ticktype = "detailed")

# manipulating the plot can help find a better perspective
open3d(windowRect=c(50,50,800,800))
persp3d(vv, vv, sel_tab, col=heat.colors(32)[floor(31*hgt+1)], theta=20, phi=00, zlim=c(-1.5,1),
 xlab='Ego Drinking', ylab='Alter Drinking', zlab='Contribution to Network Function', ticktype = "detailed")

# Ego-Alter Influence Table
# Define part of evaluation function
obj_b <- function(vi, vj){
  b1*(vi-v_av) + b2*(vi-v_av)^2 + b3*(1 - abs(vi-vj)/ran_v - sim_av)
  }

# Fill in the values of the parameter estimates and the averages
v_av <- 3.113
sim_av <- 0.6983
b1 <- 0.42  # linear
b2 <- -0.0721  # quad
b3 <- 3.9603  # behavior: average similarity
vv <- c(1, 2, 3, 4, 5)

# Note that ego behavior is in the columns!
# Ego chooses which behavior to adopt based on average level of alters
t(outer(vv, vv, obj_b))
round(t(outer(vv, vv, obj_b)),2)
tab_sel2 <- xtable(round(t(outer(vv, vv, obj_b)),2))



##### Functions for GOF - run once ######
igraphNetworkExtraction <- function(i, data, sims, period, groupName, varName){
     require(igraph)
     dimsOfDepVar<- attr(data[[groupName]]$depvars[[varName]], "netdims")
     missings <- is.na(data[[groupName]]$depvars[[varName]][,,period]) |
                 is.na(data[[groupName]]$depvars[[varName]][,,period+1])
     if (is.null(i)) {
   # sienaGOF wants the observation:
       original <- data[[groupName]]$depvars[[varName]][,,period+1]
       original[missings] <- 0
       returnValue <- graph.adjacency(original)
     }
     else
     {
       missings <- graph.adjacency(missings)
   #sienaGOF wants the i-th simulation:
       returnValue <- graph.difference(
       graph.empty(dimsOfDepVar) +
           edges(t(sims[[i]][[groupName]][[varName]][[period]][,1:2])),
                missings)
     }
     returnValue
   }
   
GeodesicDistribution <- function (i, data, sims, period, groupName,
                           varName, levls=c(1:5,Inf), cumulative=TRUE, ...) {
     x <- networkExtraction(i, data, sims, period, groupName, varName)
     require(sna)
     a <- sna::geodist(symmetrize(x))$gdist
     if (cumulative)
     {
       gdi <- sapply(levls, function(i){ sum(a<=i) })
     }
	 else
     {
       gdi <- sapply(levls, function(i){ sum(a==i) })
     }
     names(gdi) <- as.character(levls)
     gdi
   }

   # Holland and Leinhardt Triad Census; see ?sna::triad.census.
   TriadCensus <- function(i, data, sims, wave, groupName, varName, levls=1:16){
       unloadNamespace("igraph") # to avoid package clashes
       require(sna)
       require(network)
       x <- networkExtraction(i, data, sims, wave, groupName, varName)
	   if (network.edgecount(x) <= 0){x <- symmetrize(x)}
       # because else triad.census(x) will lead to an error
       tc <- sna::triad.census(x)[1,levls]
       # names are transferred automatically
       tc
   }

  # Distribution of Bonacich eigenvalue centrality; see ?igraph::evcent.
  EigenvalueDistribution <- function (i, data, sims, period, groupName, varName,
                           levls=c(seq(0,1,by=0.125)), cumulative=TRUE){
     require(igraph)
     x <- igraphNetworkExtraction(i, data, sims, period, groupName, varName)
     a <- igraph::evcent(x)$vector
     a[is.na(a)] <- Inf
     lel <- length(levls)
     if (cumulative)
     {
       cdi <- sapply(2:lel, function(i){sum(a<=levls[i])})
     }
     else
     {
       cdi <- sapply(2:lel, function(i){
                     sum(a<=levls[i]) - sum(a <= levls[i-1])})
     }
     names(cdi) <- as.character(levls[2:lel])
     cdi
    }
    
MoranGeary <- function(i, data, sims, wave, groupName, varName, levls=1:2){
	#unloadNamespace("igraph") # to avoid package clashes
	require(sna)
	require(network)
	x <- as.sociomatrix(networkExtraction(i, data, sims, wave, groupName, varName[1]))
	z <- behaviorExtraction(i,data,sims,wave,groupName,varName[2])
	n <- length(z)
	z.ave <- mean(z,na.rm=TRUE)
	numerator <- n*sum(x*outer(z-z.ave,z-z.ave),na.rm=TRUE)
	denominator <- sum(x,na.rm=TRUE)*sum((z-z.ave)^2,na.rm=TRUE)
	res <- numerator/denominator
	numerator <- (n-1)*sum(x*(outer(z,z,FUN='-')^2),na.rm=TRUE)
	denominator <- 2*sum(x,na.rm=TRUE)*sum((z-z.ave)^2,na.rm=TRUE)
	res[2] <- numerator/denominator
	names(res) <- c("Moran","Geary")
	return(res)
}

Moran123 <- function(i, data, sims, wave, groupName, varName, levls=1){
	#unloadNamespace("igraph") # to avoid package clashes
	require(sna)
	require(network)
	x <- as.sociomatrix(networkExtraction(i, data, sims, wave, groupName, varName[1]))
	z <- behaviorExtraction(i,data,sims,wave,groupName,varName[2])
	# handle missing data [not checked if this makes sense]:
	x[is.na(x)] <- 0
	z[is.na(z)] <- mean(z,na.rm=TRUE)
	res <- nacf(x,z,lag.max=3,typ="moran")[2:4]
	names(res) <- c("d=1","d=2","d=3")
	return(res)
}

Geary123 <- function(i, data, sims, wave, groupName, varName, levls=1){
	#unloadNamespace("igraph") # to avoid package clashes
	require(sna)
	require(network)
	x <- as.sociomatrix(networkExtraction(i, data, sims, wave, groupName, varName[1]))
	z <- behaviorExtraction(i,data,sims,wave,groupName,varName[2])
	# handle missing data [not checked if this makes sense]:
	x[is.na(x)] <- 0
	z[is.na(z)] <- mean(z,na.rm=TRUE)
	res <- nacf(x,z,lag.max=5,typ="geary")[2:4]
	names(res) <- c("d=1","d=2","d=3")
	return(res)
}

EgoAlterTable <- function(i, data, sims, wave, groupName, varName, levls=1){
	#unloadNamespace("igraph") # to avoid package clashes
	#require(sna)
	#require(network)
	x <- as.sociomatrix(networkExtraction(i, data, sims, wave, groupName, varName[1]))
	z <- behaviorExtraction(i,data,sims,wave,groupName,varName[2])
	res <- matrix(0,nr=5,nc=5)
	for (ego in 1:5) {
	for (alt in 1:5) {
		thesum <- sum(x[z==ego,z==alt],na.rm=TRUE)
		if (thesum>0) {
			res[ego,alt] <- thesum
		}
	}}
	thenames <- paste('e',col(res),'a',row(res),sep='')
	res <- c(t(res))
	names(res) <- thenames
	return(res)
}


            