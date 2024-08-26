rm(list=ls())

# Make some fake data

library(rrandvec)
library(abind)
library(ggplot2)

catch_values <- seq(0,100,1) # fake weekly catch values
n_weeks<- 12 # number of stat weeks in each fishing year
n_years <- 40 # number of years
years <- seq(1980, length.out=n_years) # years
populations <- c("Kitsumkalum", "Lower Skeena", "Middle Skeena", "Zymoetz-Fiddler", "Large Lakes", "Upper Skeena") # populations
n_populations <- length(populations)

# make array of weekly catches
G <- array(sample(catch_values, n_weeks*n_years, replace=TRUE), dim=c(n_weeks, n_years), 
           dimnames = list(w= 1:n_weeks, y= years))
G
# # make list of matrices of genetic mixture proportions (add to 1)
# P_list <- lapply(1:n_years, FUN = function(x) {
#   array( rrandvec( n = n_weeks, d = length(populations)), dim = c(n_weeks,length(populations)))
# }
# )
# # bind list into array
# P <- abind(P_list, along=3)

# Make array of genetic mixture proportions (add to 1)
P <- sapply(1:n_years, FUN = function(x) {
  rrandvec( n = n_weeks, d = n_populations)
}, simplify = "array"
)
# name array dimensions
dimnames(P) <- list(w = 1:n_weeks, i = populations, y = years)
P

# Function to do weekly expansions of catch by weekly genetic proportions, and 
# then sum the weekly expansions by population and year
get_P_tilde <- function(P, G) {
  n_years <- length(dimnames(P)$y)
  P_tilde_list <- list() # make empty list to store results for each year
  for(i in 1: n_years) { # cycle through years
    # multiply arrays to get weekly expansions. Works because P and G both have 
    # week as their first dimension and when G is indexed it becomes a numeric vector. 
    # when multiplying array by vector, R automatically uses the dimension of the vector
    # with same length as vector it is multiplying by. Caution. Need to make 
    # that explicit which vector to use. If there are the same number of weeks w and 
    # years y than there could be issues.
    P_tilde_list[[i]] <- P[,,i] * G[,i] 
    P_tilde_list1 <- lapply(P_tilde_list, FUN = function(x) {apply(x, 2, sum)}) # go through the list and sum weekly expansions by population
    # Make into proportion of yearly catch by dividing expansions for each population 
    # by the total yearly catch. Note that the function(x) gets applied to each 
    # element of each vector automatically (not sure why). 
    P_tilde_list2 <- lapply(P_tilde_list1, FUN = function(x) { x / sum(x)} ) 
    }
  P_tilde <- abind( P_tilde_list2, along=2) # bind list into array
  dimnames(P_tilde) <- list(i = populations, y = years) # fix names of dimensions
  P_tilde
}

# get estimated annual proportions from weekly catch and weekly genetic mixture analysis
P_tilde <- get_P_tilde(P = P, G= G) 
dimnames(P_tilde)
P_tilde

dimnames(P)
dimnames(G)
P_tilde[,"2010"]

sum(P[,"Kitsumkalum","2010"] * G[,"2010"])/ sum(G[,"2010"])

P_tilde["Kitsumkalum", "1980"]


