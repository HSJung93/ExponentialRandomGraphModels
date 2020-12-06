renv::init()

### Getting Started
install.packages("devtools")
library(devtools)
install_github("zalmquist/networkdata")
library(networkdata)
library(statnet)
load("bott.RData")
summary(bott)
bott
plot(bott[[1]])
summary(bott[[4]]~edges) # how many edges?
bottmodel.01 <- ergm(bott[[4]]~edges) # Estimate minimal model
summary(bottmodel.01) # The fitted model object

### The Allure of Triangles
summary(bott[[4]]~edges+triangle)
bottmodel.02 <- ergm(bott[[4]]~edges+triangle)
summary(bottmodel.02)

### Nodal Covariates
age <- bott[[4]] %v% "age.month"
summary(age)
plot(bott[[4]], vertex.cex=age/20)
summary(bott[[4]]~edges+nodecov('age.month'))

bottmodel.03 <- ergm(bott[[4]]~edges+nodecov('age.month'))
summary(bottmodel.03)
bottmodel.03b <- ergm(bott[[4]]~edges+nodeicov('age.month'))
summary(bottmodel.03b)
bottmodel.03c <- ergm(bott[[4]]~edges+absdiff('age.month'))
summary(bottmodel.03c)

### Reciprocity
bottmodel.04 <- ergm(bott[[4]]~edges+mutual)
summary(bottmodel.04)

### Edge Covariates
# Test the imitation network also using edges from the talking network
bottmodel.05 <- ergm(bott[[4]]~edges+edgecov(bott[[1]]))
summary(bottmodel.05)

## note that this creates a vector of all the kids' ages
bott[[4]]%v%"age.month"

## create matrix of pairwise absolute age differences
agediff <- abs(outer(bott[[4]]%v%"age.month",bott[[4]]%v%"age.month","-"))
bottmodel.06 <- ergm(bott[[4]]~edges+edgecov(bott[[1]])+edgecov(agediff))
summary(bottmodel.06)

### Determining Model Goodness-of-Fit
bottmodel.06.gof <- gof(bottmodel.06 ~ model + esp + distance)
bottmodel.06.gof
plot(bottmodel.06.gof)

### Assessing MCMC diagnostics
mcmc.diagnostics(bottmodel.04)
## MCMC diagnostics shown here are from the last round of simulation, prior to computation of final parameter estimates. Because the final estimates are refinements of those used for this simulation run, these diagnostics may understate model performance. To directly assess the performance of the final model on in-model statistics, please use the GOF command: gof(ergmFitObject, GOF=~model).

### Edgewise shared partnerships, or triangles revisited
bottmodel.07 <- ergm(bott[[4]]~edges+nodeicov('age.month')+edgecov(bott[[1]])+edgecov(agediff)+gwesp(1,fixed=FALSE))
summary(bottmodel.07)
