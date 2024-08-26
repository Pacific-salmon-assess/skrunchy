
#' Do weekly expansions of catch by weekly genetic proportions, and then sum the weekly expansions by population and year
#'
#' @param P Numeric, array of genetic proportions with 3 dimensions: w (statistical week), i (population), and y (year)
#' @param G Numeric, array of how many Chinook were caught in the gillnet Tyee Test Fishery by week, with 2 dimensions: w (week) and y (year)
#'
#' @return Numeric, array of pooled genetic proportions with 2 dimensions: i (population) and y (year)
#' @export
#'
#' @examples
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