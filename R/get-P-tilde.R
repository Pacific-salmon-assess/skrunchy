#' Get population proportions
#'
#' Do weekly expansions of catch by weekly genetic proportions, and then sum the weekly expansions by population and year
#'
#' @param P Numeric, array of genetic proportions with 3 dimensions: w (statistical week), i (population), and y (year)
#' @param G Numeric, array of how many Chinook were caught in the gillnet Tyee Test Fishery by week, with 2 dimensions: w (week) and y (year)
#'
#' @return Numeric, list of 2 arrays. First is an array of pooled genetic proportions with 2 dimensions: i (population) and y (year).
#' Second is an array of SD of pooled genetic proportions with 2 dimensions: i (population) and y (year).

#' @examples
#' library(rrandvec)
#' library(abind)
#' test <- make_P_G()
#' P <- test$P
#' sigma_P <- test$sigma_P
#' G <- test$G
#'
#' P_tilde <- get_P_tilde(P = P, sigma_P = sigma_P, G= G)
#'
#' @export
get_P_tilde <- function(P, sigma_P, G) {
  populations <- dimnames(P)$i
  stopifnot( "Years are not equal for genetic and catch data" = all.equal(unique(dimnames(P)$y), unique(dimnames(G)$y)))
  years <- dimnames(P)$y
  n_years <- length(dimnames(P)$y)
  P_tilde_list <- list() # make empty list to store results for each year
  for(i in 1: n_years) { # cycle through years
    # multiply arrays to get weekly expansions. Works because P and G both have
    # week as their first dimension and when G is indexed it becomes a numeric vector.
    # when multiplying array by vector, R automatically uses the dimension of the vector
    # with same length as vector it is multiplying by. Caution. Need to make
    # that explicit which vector to use. If there are the same number of weeks w and
    # years y than there could be issues. P is divided by 100 because it comes
    # from the Molecular Genetics Lab in proportions of 100.
    P_tilde_list[[i]] <- P[,,i] / 100 * G[,i]
    P_tilde_list1 <- lapply(P_tilde_list, FUN = function(x) {apply(x, 2, sum)}) # go through the list and sum weekly expansions by population
    # Make into proportion of yearly catch by dividing expansions for each population
    # by the total yearly catch. Note that the function(x) gets applied to each
    # element of each vector automatically (not sure why).
    P_tilde_list2 <- lapply(P_tilde_list1, FUN = function(x) { x / sum(x)} )
  }
  P_tilde <- abind( P_tilde_list2, along=2) # bind list into array
  dimnames(P_tilde) <- list(i = populations, y = years) # fix names of dimensions

  # sigma for P_tilde
  sigma_P_tilde_list <- list() # make empty list to store results for each year
  for(i in 1: n_years) { # cycle through years
    # multiply arrays to get weekly expansions. Works because P and G both have
    # week as their first dimension and when G is indexed it becomes a numeric vector.
    # when multiplying array by vector, R automatically uses the dimension of the vector
    # with same length as vector it is multiplying by. Caution. Need to make
    # that explicit which vector to use. If there are the same number of weeks w and
    # years y than there could be issues. P is divided by 100 because it comes
    # from the Molecular Genetics Lab in proportions of 100.
    sigma_P_tilde_list[[i]] <- sigma_P[,,i] / 100 * G[,i]
    sigma_P_tilde_list1 <- lapply(sigma_P_tilde_list, FUN = function(x) {apply(x, 2, function(y) { sqrt(sum(y^2))} )}) # go through the list
    # Make into proportion of yearly catch by dividing expansions for each population
    # by the total yearly catch. Note that the function(x) gets applied to each
    # element of each vector automatically (not sure why).
    sigma_P_tilde_list2 <- lapply(sigma_P_tilde_list1, FUN = function(x) { x / sum(x)} ) # FLAG: this is wrong. need to divide by G_y
  }
  sigma_P_tilde <- abind( sigma_P_tilde_list2, along=2) # bind list into array
  dimnames(sigma_P_tilde) <- list(i = populations, y = years) # fix names of dimensions

  res <- list(P_tilde = P_tilde, sigma_P_tilde = sigma_P_tilde)
  res
}

