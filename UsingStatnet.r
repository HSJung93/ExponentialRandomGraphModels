library(statnet)

set.seed(0)

####### 1. Statistical network modeling with ERGMs

#The general form of the model specifies the probability of the entire network (the left hand side), as a function of terms that represent network features we hypothesize may occur more or less likely than expected by chance (the right hand side).

#The statistics g(y) can be thought of as the “covariates” in the model. In the network modeling context, these represent network features like density, homophily, triads, etc. In one sense, they are like covariates you might use in other statistical models.

#terms are either dyad independent or dyad dependent. Dyad independent terms (like nodal homophily terms) imply no dependence between dyads—the presence or absence of a tie may depend on nodal attributes, but not on the state of other ties. Dyad dependent terms (like degree terms, or triad terms), by contrast, imply dependence between dyads.
library(ergm)
data(package = "ergm")
data(florentine)
flomarriage
par(mfrow=c(1,2))
plot(flomarriage,
    main="Florentine Marriage",
    cex.main=0.8,
    label= network.vertex.names(flomarriage))
wealth <- flomarriage %v% 'wealth' 
wealth
plot(flomarriage,
    vertex.cex=wealth/25,
    main="Florentine marriage by wealth",
    cex.main=0.8)

### A simple Bernoulli (“Erdos/Renyi”) model
summary(flomarriage ~ edges)
flomodel.01 <- ergm(flomarriage ~ edges)
summary(flomodel.01)

#This simple model specifies a single homogeneous probability for all ties, which is captured by the coefficient of the edges term. How should we interpret this coefficient? The easiest way is to return to the logit form of the ERGM. The log-odds that a tie is present is

# logit(p(y))=θ×δ(g(y))
# = −1.61×change in the number of ties
# = −1.61×1

# for every tie, since the addition of any tie to the network always increases the total number of ties by 1.

# any model containing the ergm-term triangle has the property that dyads are not probabilistically independent of one another. 

### Triad formation
summary(flomarriage~edges+triangle) # Look at the g(y) stats for this model

flomodel.02 <- ergm(flomarriage~edges+triangle) 
summary(flomodel.02)

# Now, how should we interpret coefficients? The conditional log-odds of two actors having a tie, keeping the rest of the network fixed, is −1.67×change in the number of ties+0.17×change in number of triangles.

# For a tie that will create no triangles, the conditional log-odds is: −1.67.
# if one triangle: −1.67+0.17=−1.51
# if two triangles: −1.67+2×0.17=−1.34
# the corresponding probabilities are 0.16, 0.18, and 0.20.
class(flomodel.02) # this has the class ergm
names(flomodel.02) # the ERGM object contains lots of components.
flomodel.02$coef # you can extract/inspect individual components

# We saw earlier that wealth appeared to be associated with higher degree in this network. We can use ergm to test this. Wealth is a nodal covariate, so we use the ergm-term nodecov.
summary(wealth) # summarize the distribution of wealth
summary(flomarriage~edges+nodecov('wealth')) # observed statistics for the model

flomodel.03 <- ergm(flomarriage~edges+nodecov('wealth'))
summary(flomodel.03)

# How do we interpret the coefficients here? Note that the wealth effect operates on both nodes in a dyad. The conditional log-odds of a tie between two actors is:

# −2.59×change in the number of ties+0.01×the sum of the wealth of the two nodes

# This model specification does not include a term for homophily by wealth, i.e., a term accounting for similarity in wealth of the two end nodes of a potential tie. It just specifies a relation between wealth and mean degree. To specify homophily on wealth, you could use the ergm-term absdiff

### Nodal covariates: Homophily
data(faux.mesa.high) 
mesa <- faux.mesa.high
mesa

par(mfrow=c(1,1)) # Back to 1-panel plots
plot(mesa, vertex.col='Grade')
legend('bottomleft',fill=7:12,
       legend=paste('Grade',7:12),cex=0.75)

fauxmodel.01 <- ergm(mesa ~edges + 
                       nodefactor('Grade') + nodematch('Grade',diff=T) +
                       nodefactor('Race') + nodematch('Race',diff=T))
                    
summary(fauxmodel.01)
table(mesa %v% 'Race') # Frequencies of race
mixingmatrix(mesa, "Race")
# The problem is that there are very few students in the Black and Other race categories, and these few students form no within-group ties. The empty cells are what produce the -Inf estimates.
summary(mesa ~edges  + 
          nodefactor('Grade') + nodematch('Grade',diff=T) +
          nodefactor('Race') + nodematch('Race',diff=T))

### Directed ties
data(samplk) 
ls() # directed data: Sampson's Monks
samplk3
plot(samplk3)
summary(samplk3~edges+mutual)

sampmodel.01 <- ergm(samplk3~edges+mutual)
summary(sampmodel.01)
# There is a strong and significant mutuality effect. The coefficients for the edges and mutual terms roughly cancel for a mutual tie, so the conditional log-odds of a mutual tie are about zero, which means the probability is about 50%. (Do you see why a log-odds of zero corresponds to a probability of 50%?) By contrast, a non-mutual tie has a conditional log-odds of -2.16, or 10% probability.

###### 2. Missing Data
# It is important to distinguish between the absence of a tie and the absence of data on whether a tie exists. The former is an observed zero, whereas the latter is unobserved. You should not code both of these as “0”. 

missnet <- network.initialize(10,directed=F) # initialize an empty net with 10 nodes
missnet[1,2] <- missnet[2,7] <- missnet[3,6] <- 1 # add a few ties
missnet[4,6] <- missnet[4,9] <- missnet[5,6] <- NA # mark a few dyads missing
summary(missnet)

# plot missnet with missing dyads colored red. 
tempnet <- missnet
tempnet[4,6] <- tempnet[4,9] <- tempnet[5,6] <- 1
missnetmat <- as.matrix(missnet)
missnetmat[is.na(missnetmat)] <- 2
plot(tempnet,label = network.vertex.names(tempnet),
     edge.col = missnetmat)

# fit an ergm to the network with missing data identified
summary(missnet~edges)
summary(ergm(missnet~edges))
# The coefficient equals -2.56, which corresponds to a probability of 7.14%. Our network has 3 ties, out of the 42 non-missing nodal pairs (10 choose 2 minus 3): 3/42 = 7.14%. So our estimate represents the probability of a tie in the observed sample.
# Now let’s assign those missing ties the (observed) value “0” and check how the value of the coefficient will change.
missnet_bad <- missnet # create network with missing dyads set to 0
missnet_bad[4,6] <- missnet_bad[4,9] <- missnet_bad[5,6] <- 0

# fit an ergm to the network with missing dyads set to 0
summary(missnet_bad)

summary(ergm(missnet_bad~edges))
# The coefficient is smaller now because the missing ties are counted as “0”, and this translates to a conditional tie probability of 6.67%.

###### 3. Model terms available for ergm estimation and simulation
# Many ERGM terms are simple counts of configurations (e.g., edges, nodal degrees, stars, triangles), but others are more complex functions of these configurations (e.g., geometrically weighted degrees and shared partners). In theory, any configuration (or function of configurations) can be a term in an ERGM. In practice, however, these terms have to be constructed before they can be used—that is, one has to explicitly write an algorithm that defines and calculates the network statistic of interest.

help('ergm-terms')

###### 4. Assessing convergence for dyad dependent models: MCMC Diagnostics
# When dyad dependent terms are in the model, the computational algorithms in ergm use MCMC (with a Metropolis-Hastings sampler) to estimate the parameters.
# start with an initial vector of parameter values; the default is to use the maximum psuedo-likelihood estimates (MPLEs, using the logistic regression estimation algorithm)
# choose a dyad at random, and flip a coin, weighted by the model, to decide whether there will be a tie. repeat this for 1024 steps (the default ``MCMC interval") take the network at the last step and calculate the network statistics (the ones in the model) repeat this 1024 times (the default ``MCMC sample size") calculate the average for each statistic in the sample, and compare this to the observed statistic
# update the parameter estimates as needed. repeat until the process converges: the difference between the MCMC sample average and the observed statistic is sufficiently small

# Below we show a simple example of a model that converges, and how to use the MCMC diagnostics to identify this.

### What it looks like when a model converges properly
# We will first consider a simple dyadic dependent model where the algorithm works using the program defaults, with a degree(1) term that captures whether there are more (or less) degree 1 nodes than we would expect, given the density.

summary(flobusiness~edges+degree(1))

fit <- ergm(flobusiness~edges+degree(1))
summary(fit)
mcmc.diagnostics(fit)
# This is what you want to see in the MCMC diagnostics: the MCMC sample statistics are varying randomly around the observed values at each step (so the chain is “mixing” well), the sampled values show little serial correlation (so they are independent draws from the graph space) and the difference between the observed and simulated values of the sample statistics have a roughly bell-shaped distribution, centered at 0.
fit <- ergm(flobusiness~edges+degree(1),
            control=control.ergm(MCMC.interval=1))

##### 5. Network simulation: the simulate command and network.list objects
### we’ll look at some models that don’t converge properly, and how to use MCMC diagnostics to identify and address this.
flomodel.03.sim <- simulate(flomodel.03,nsim=10)
class(flomodel.03.sim) # what does this produce?
summary(flomodel.03.sim) # quick summary
attributes(flomodel.03.sim) # what's in this object?

# are the simulated stats centered on the observed stats?
rbind("obs"=summary(flomarriage~edges+nodecov("wealth")),
      "sim mean"=colMeans(attr(flomodel.03.sim, "stats"))) 
# we can also plot individual simulations
flomodel.03.sim[[1]]
plot(flomodel.03.sim[[1]], 
     label= flomodel.03.sim[[1]] %v% "vertex.names",
     vertex.cex = (flomodel.03.sim[[1]] %v% "wealth")/25)
#  Simulation plays a large role in analyizing egocentrically sampled data, and if you take the tergm workshop, you will see how we can use simulation to examine the temporal implications of a model based on a single cross-sectional egocentrically sampled dataset.

###### 6. Examining the quality of model fit – GOF
# One test of whether a local model “fits the data” is therefore how well it reproduces the observed global network properties that are not in the model. We do this by choosing a network statistic that is not in the model, and comparing the value of this statistic observed in the original network to the distribution of values we get in simulated networks from our model, using the gof function.
# The gof function is a bit different than the summary, ergm, and simulate functions, in that it currently only takes 3 ergm-terms as arguments: degree, esp (edgwise share partners), and distance (geodesic distances). Each of these terms captures an aggregate network distribution, at either the node level (degree), the edge level (esp), or the dyad level (distance).

flomodel.03.gof <- gof(flomodel.03)
flomodel.03.gof
plot(flomodel.03.gof)

mesamodel.02 <- ergm(mesa~edges)
mesamodel.02.gof <- gof(mesamodel.02~degree + esp + distance, 
                        control.gof.formula(nsim=10))
plot(mesamodel.02.gof)

###### 7. Diagnostics: troubleshooting and checking for model degeneracy

# What it looks like when a model fails
data('faux.magnolia.high')
magnolia <- faux.magnolia.high
magnolia
plot(magnolia, vertex.cex=.5)
# Consider a simple model
summary(magnolia~edges+triangle)
fit <- ergm(magnolia~edges+triangle)
# This is such a clear indicator of a degenerate model specification that the algorithm stops after 3 iterations, to avoid heading off into areas that would cause memory issues.
fit <- ergm(magnolia~edges+triangle, 
            control=control.ergm(MCMLE.maxit=2))
mcmc.diagnostics(fit)

# We also now have more robust version of modeling triangles: the geometrically-weighed edgewise shared partner term (GWESP).
fit <- ergm(magnolia~edges+gwesp(0.25,fixed=T),
            verbose=T)
mcmc.diagnostics(fit)

fit <- ergm(magnolia~edges+gwesp(0.25,fixed=T)+nodematch('Grade')+
              nodematch('Race')+nodematch('Sex'),
            control = control.ergm(MCMC.interval=10000),
            verbose=T)

mcmc.diagnostics(fit)

###### 8. Working with egocentrically sampled network data