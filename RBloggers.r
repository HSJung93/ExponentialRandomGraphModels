library(statnet)
data('sampson')
n = samplike
n
list.vertex.attributes(n)
get.vertex.attribute(n, 'cloisterville')   # Whether they attended cloisterville before coming to the monestary
set.vertex.attribute(n, 'integers', 1:network.size(n))
get.vertex.attribute(n, 'integers')
delete.vertex.attribute(n, 'integers')
plot(n
     , displaylabels = TRUE
     , vertex.cex = degree(n, cmode = 'indegree') / 2
     , vertex.col = 'group'
     , vertex.sides = ifelse(n %v% 'cloisterville', 4, 50)
     , pad = 1
)
m1 = ergm(n ~ edges)
summary(m1)
# Because that is a dyadic-independent model (the likelihood of a tie doesn’t depend on any other), ergm solves the logistic regression instead of resorting to MCMC.

# Note that the edges term represents exactly the density of the network (in log-odds). That is, the probability of any tie (aka the density of the network) is the inverse-logit of the coefficient on edges:

all.equal(network.density(n), plogis(coef(m1)[[1]]))
m2 = ergm(n ~ edges + mutual)
summary(m2)

library(latticeExtra)

plogis(coef(m2)[['edges']])
plogis(coef(m2)[['edges']] + coef(m2)[['mutual']])
mcmc.diagnostics(m2)

# silly example
mbad = ergm(n ~ edges + mutual,
            control = control.ergm(MCMC.interval = 2))
mcmc.diagnostics(mbad)

### Examining model fit
m2_gof = gof(m2, GOF = ~model)
m2_gof
# Now that we can trust our model estimates, let’s see if they make a good fit to the data.

m2_gof2 = gof(m2)
par(mfrow = c(3, 2))
plot(m2_gof2)

### Simulating networks
sim_nets = simulate(m2, nsim = 4)
# Define a plotting function:
plot_nets = function(n)
    plot(n
     , displaylabels = FALSE
     , vertex.cex = degree(n, cmode = 'indegree') / 2 + 1
     , vertex.col = 'group'
     , vertex.sides = ifelse(n %v% 'cloisterville', 4, 50)
     )
par(mfrow = c(2, 2))
invisible(lapply(sim_nets, plot_nets))  # I wrap that in `invsible()` because `plot.network` returns the coordinates of nodes in the plot, which can be useful for reproducability or programmatic manipulation, but is distracting here.

# To add a term for homophily within Sampson’s groups we use the term nodematch, which takes at least one argument (the nodal attribute), and provides the change in the likelihood of a tie if the two nodes match on that attribute. Note that you can estimate a differential homophily effect; that is, the change in tie likelihood for two nodes being in the same group can vary by group, by specifying the diff = TRUE argument to nodematch.

summary(n ~ edges + mutual + nodematch('group'))
m3 = ergm(n ~ edges + mutual + nodematch('group'))
# So of the 88 ties in the network, 28 of them are reciprocal, and 63 of them are between monks within a group. So we should expect a strong positive coefficient for the group-homophily term.
summary(m3)
# Indeed. The log-odds of a within-group tie are 2x greater than an across-group tie. We can exponentiate to find the change in the odds, exp(coef(m3)[3]) = 7.63.
# The probability of a tie, however, is non-linear: it depends on the value of other statistics, so to calculate a change in probability you must choose a value for every other statistic in the model, then you can use the inverse-logit to find the difference in probability across your effect of interest. E.g. Let’s look at the probability of non-reciprocal ties within- and across-groups:
plogis(coef(m3)[1])
plogis(sum(coef(m3)[c(1, 3)]))  
par(mfrow = c(2, 2))
invisible(plot(gof(m3)))
# Two-stars can be used to represent popularity (because the more edges on a node, the more two stars an additional edge will create):
m4 = ergm(n ~ edges + mutual + nodematch('group') + istar(2))
summary(m4)
# GOF plots:
par(mfrow = c(3, 2))
invisible(plot(gof(m4)))

sim_nets = simulate(m4, nsim = 4)
par(mfrow = c(2, 2))
invisible(lapply(sim_nets, plot_nets)) 

round(sapply(list(m1, m2, m3, m4), AIC), 0)

### Model degeneracy and geometerically weighted terms
# Degeneracy refers to a case where the MLE for the specified sufficient statistics produce graphs that are either complete, or empty, or have all edges concentrated in a small region of the graph, or otherwise produce networks that are not of interest.
m5 = ergm(n ~ edges + mutual + nodematch('group') + istar(2) + triangles)
summary(m5)
# It is a sign of an ill-specified model, but unfortunately we often want estimates for theoretically justified reasons that we cannot get due to degeneracy issues. The quintessential such term is for triangles: How does the likelihood of a friendship change if two people already have a friend in common? For this small of a network we can estimate that directly:
# It is also rare that non-degenerate estimates are possible for models with triangles, particularly for graphs larger than a few dozen nodes.

# The implication of a triangles term is that the likelihood of tie changes proportionately to the number of shared friends two people have. That is, if having one shared friend makes a tie 25% more likely, having six shared friends makes a tie 150% more likely. Perhaps we should discount each additional tie. We can do that with the geometrically-weighted edgewise shared partners (gwesp) term. It takes a parameter, \(\alpha\) that controls how much to discount 2nd, 3rd, etc. shared partners.
# The closer \(\alpha\) is to zero, the more dramatic the discounting applied to subsequent shared partners.
m6 = ergm(n ~ edges + mutual + nodematch('group') + gwesp(decay = .5, fixed = TRUE)) # alpha -> decay
summary(m6)
#  Like triangles, two-stars often produces degeneracy for all but the smallest graphs; gwdegree can circumvent this problem. It takes a parameter related to gwesp’s \(\alpha\), decay, which should also generally be fixed. The closer decay is to zero, the more gwdegree considers low degree nodes relative to high degree nodes.
# These terms can be useful for modeling a popularity effect, but they are often, perhaps unfortunately, used simply to aid model convergence.
m7 = ergm(n ~ edges + mutual + nodematch('group') + 
              gwesp(decay = .5, fixed = TRUE) + gwidegree(decay = 1, fixed = TRUE))
summary(m7)
# The way the gwdegree terms are constructed, negative estimates reflect an increased likelihood on ties to higher-degree nodes.