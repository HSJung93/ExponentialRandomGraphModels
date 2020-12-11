renv::init()
renv::restore()
install.packages("igraph")
library(igraph)
library(devtools)
install_github("DougLuke/UserNetR")
library(UserNetR)
data(Coevolve)
fr_w1 <- Coevolve$fr_w1 
fr_w2 <- Coevolve$fr_w2 
fr_w3 <- Coevolve$fr_w3 
fr_w4 <- Coevolve$fr_w4 

colors <- c("darkgreen","SkyBlue2")
shapes <- c("circle","square")
coord <- layout.kamada.kawai(fr_w1)
op <- par(mfrow=c(2,2),mar=c(1,1,2,1))

plot(fr_w1,vertex.color=colors[V(fr_w1)$smoke+1],
vertex.shape=shapes[V(fr_w1)$gender], vertex.size=10,main="Wave 1",vertex.label=NA, edge.arrow.size=0.5,layout=coord)

plot(fr_w2,vertex.color=colors[V(fr_w2)$smoke+1], vertex.shape=shapes[V(fr_w2)$gender], vertex.size=10,main="Wave 2",vertex.label=NA, edge.arrow.size=0.5,layout=coord)

plot(fr_w3,vertex.color=colors[V(fr_w3)$smoke+1], vertex.shape=shapes[V(fr_w3)$gender], vertex.size=10,main="Wave 3",vertex.label=NA, edge.arrow.size=0.5,layout=coord)

plot(fr_w4,vertex.color=colors[V(fr_w4)$smoke+1], vertex.shape=shapes[V(fr_w4)$gender], vertex.size=10,main="Wave 4",vertex.label=NA, edge.arrow.size=0.5,layout=coord)

par(op)

# the size and density of the networks remain constant, but the number of smokers increases over time, as does the modularity based on smoking status.

# A network variable is the basic dependent variable in an RSiena model, and can be a one-mode or two-mode network. A behavior variable is another type of dependent variable. It is a node characteristic that changes over time, the evolution of which may considered in a co-evolutionary model. In our example dataset, smoking status will be handled as a behavior variable. Then there are four types of variables that are all handled as covariates. A coCovar is a constant node attribute that does not change over time (e.g., gender). A varCovar, on the other hand, is an attribute that does change over time. (Note that a behavior variable is a type of varying covariate, but one that is being treated as a dependent variable.) Similar to ergm, RSiena can also handle dyadic covariates. A coDyadCovar is a constant dyadic covariate (for example, a kinship relationship), while varDyadCovar is a dyadic covariate that changes over time.

install.packages("RUnit")
install.packages("Matrix")
install.packages("MASS")
install.packages("lattice")
install.packages("codetools")
install.packages("network")
install.packages("xtable")
install.packages("tcltk")
install.packages("sna")
install.packages("RSienaTest", repos="http://R-Forge.R-project.org",
type = "source", dependencies = TRUE)
library(RSienaTest)
renv::snapshot()
matw1 <- as.matrix(get.adjacency(fr_w1)) 
matw2 <- as.matrix(get.adjacency(fr_w2)) 
matw3 <- as.matrix(get.adjacency(fr_w3)) 
matw4 <- as.matrix(get.adjacency(fr_w4)) 
matw1[1:8,1:8]

fr4wav<-sienaDependent(array(c(matw1,matw2,matw3,matw4), dim=c(37,37,4)),sparse=FALSE)
class(fr4wav)
# Then, a dependent variable object is created with sienaDependent. This ex- pects a stacked array with each array corresponding to one of the network waves. By default, sienaDependent expects data in the form of a sparse matrix (from the Matrix package). Here, the data are simple full matrices, so the sparse option must be set to false.

fr4wav
#  RSiena cannot natively handle edgelists, so they must be transformed into sparse matrices as implemented by the Matrix package. The following code produces the same friendship RSiena dependent variable, but via edgelists instead of sociomatrices.
library(Matrix)
w1 <- cbind(get.edgelist(fr_w1), 1)
w2 <- cbind(get.edgelist(fr_w2), 1)
w3 <- cbind(get.edgelist(fr_w3), 1)
w4 <- cbind(get.edgelist(fr_w4), 1)
w1s <- spMatrix(37, 37, w1[,1], w1[,2], w1[,3]) 
w2s <- spMatrix(37, 37, w2[,1], w2[,2], w2[,3]) 
w3s <- spMatrix(37, 37, w3[,1], w3[,2], w3[,3]) 
w4s <- spMatrix(37, 37, w4[,1], w4[,2], w4[,3]) 
fr4wav2 <- sienaDependent(list(w1s,w2s,w3s,w4s)) 
fr4wav2

# Once the RSienda dependent variable is constructed, then other data objects such as covariates can be created. Gender is stored in the igraph networks as a vertex characteristic, so it is easy to extract that to create the coCoVar object. Gender is coded 1 for female and 2 for males. 

gender_vect <- V(fr_w1)$gender 
table(gender_vect)

gender <- coCovar(gender_vect,centered=FALSE) 
gender

# Smoking status is our behavior variable for the co-evolution model. RSiena expects an N×W matrix, with N (actor) rows and W (wave) columns. Again, we extract this information from the igraph objects. Because a behavior variable is a type of dependent variable, the sienaDependent function is used, but we specify that this is a behavior variable.
smoke <- array(c(V(fr_w1)$smoke,V(fr_w2)$smoke, V(fr_w3)$smoke,V(fr_w4)$smoke),dim=c(37,4))
smokebeh <- sienaDependent(smoke,type = "behavior")
smokebeh

friend <- sienaDataCreate(fr4wav,smokebeh,gender)
friend

print01Report(friend,modelname = 'Coevolve Example' )

# he report contains a variety of information about the RSiena data, inc- luding information about any missing data, the degree distribution pattern observed across the waves of network data, and summary information about each dependent variable and covariate. A critical piece of information is found near the bottom of the report under the heading ‘Change in Networks.’ The Jaccard index, which is a measure of similarity, is calculated on the tie variables for each consecutive pair of waves. Although there needs to be enough change between the observation periods to allow for modeling, too much change would imply that the assumption of gradual change is not tenable. The authors of RSiena suggest that Jaccard values should be higher than 0.3 (Snijders et al. 2010).

##### 12.3 Model Specification and Estimation
frndeff <- getEffects(friend)
frndeff
# This is a basic model that only includes a small number of default effects, notably the outdegree and reciprocity effects.
# All the effects that are available given the structure of the friend data set can be seen using the effectsDocumentation function.
effectsDocumentation(frndeff)

# Because we are exploring a co-evolutionary model, we will examine effects on the likelihood of tie formations (the fr4wav dependent variable), and the effects on changes in behavior (the smokebeh dependent variable). So, there are two broad types of effects, and they are distinguished by the ‘Name’ column in the effects doc- umentation report. (‘ED Name’ in Table 12.2.) The actual specific effect that will be included is specified by the term in the ‘shortName’ column. Many of these effects will need to refer to a specific covariate or dependent variable, this is typically spec- ified by the term in the ‘interaction1’ column.


frndeff <- getEffects( friend )
frndeff <- includeEffects(frndeff,sameX,
                  interaction1="gender",name="fr4wav")
frndeff <- includeEffects(frndeff,egoX, interaction1="smokebeh",name="fr4wav")
frndeff <- includeEffects(frndeff,altX, interaction1="smokebeh",name="fr4wav")
frndeff <- includeEffects(frndeff,sameX, interaction1="smokebeh",name="fr4wav")
frndeff <- includeEffects(frndeff,avSim, interaction1="fr4wav",name="smokebeh")
frndeff <- includeEffects(frndeff,totSim, interaction1="fr4wav",name="smokebeh")
frndeff <- includeEffects(frndeff,recip,transTrip, name="fr4wav")
frndeff

myalgorithm <- sienaAlgorithmCreate(projname='coevolve')
set.seed(999)
RSmod1 <- siena07( myalgorithm, data = friend,
                 effects = frndeff,batch=TRUE,
                 verbose=FALSE,useCluster=TRUE,
                 initC=TRUE,nbrNodes=3)
summary(RSmod1)
