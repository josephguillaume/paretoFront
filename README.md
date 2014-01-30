paretoFront
===========

R version of "Two efficient algorithms to find Pareto Front" from Matlab Central
http://www.mathworks.com/matlabcentral/fileexchange/17251-pareto-front

install.packages("Rcpp")
source("paretoFront.R")

## Identify pareto front of random points
## Returns logical vector of same number of rows as input dataset
M=matrix(runif(1e5),ncol=2)
pp <- paretofront(M)
plot(M[pp,])

## Same, but splits the given objective set into several smaller groups 
##  to be examined by the first algorithm, for potential further efficiency gains
pp<- paretoGroup(M)