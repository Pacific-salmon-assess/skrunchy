#' Get wild terminal mortalities
#'
#' Calculate the wild total terminal mortalities by population, year, and age.
#'
#' @param tau Numeric, array of total terminal mortalities, with three dimensions: population (i), year (y), and age (a).
#' @param p Numeric, an array of wild spawner proportions with values between 0 and 1, with three dimensions: population (i), year (y), and age (a).
#'
#' @return List with two elements. First element: numeric, array of wild terminal mortalities with three dimensions:  population (i), year (y), and age (a).
#'    Second element: data frame version of first element, for plotting and tables.
#' @examples
#' populations <- c("Kitsumkalum", "Lower Skeena", "Zymoetz-Fiddler", "Upper Skeena", "Middle Skeena", "Large Lakes", "Skeena")
#' n_populations <- length(populations)
#' years <- 2000:2001
#' n_years <- length(years)
#' ages <- c(3,4,5,6,7)
#' n_ages <- length(ages)
#' n_obs <- n_populations * n_years * n_ages
#' com_dims <- c(n_populations, n_years, n_ages)
#' com_dimnames <- list( i = populations, y= years, a = ages)
#' tau_U <- array( sample(0:1000, size = n_obs, replace=TRUE), dim = com_dims, dimnames = com_dimnames )
#' tau_L <- array( sample(0:500, size = n_obs, replace=TRUE), dim = com_dims, dimnames = com_dimnames)
#' tau_M <- array( sample(0:2000, size = n_obs, replace=TRUE), dim = com_dims, dimnames = com_dimnames)
#' tau <- get_tau( tau_U = tau_U, tau_L =tau_L, tau_M = tau_M)
#' p <- array( runif(length(tau$tau), 0.8, 1), dim= dim(tau$tau), dimnames = dimnames(tau$tau))
#' tau_W <- get_tau_W(tau = tau$tau, p = p)
#'
#'
#' @export
get_tau_W <- function(tau, p) {
  # check dimensions are the same
  if(!dim(tau)[1] == dim(p)[1] )  {
    stop("Length of population (i) dimensions not equal.") }
  if(! dim(tau)[2] == dim(p)[2])  {
    stop("Length of year (y) dimensions not equal.") }
  if(!dim(tau)[3] == dim(p)[3])  {
    stop("Length of age (a) dimensions not equal.") }
  if(!all(dimnames(tau)$i %in% dimnames(p)$i) ) {
    stop("Population (i) values are not equal.")    }
  if(!all(dimnames(tau)$y %in% dimnames(p)$y)) {
    stop("Year (y) values are not equal.")    }
  if(!all(dimnames(tau)$a %in% dimnames(p)$a)) {
    stop("Age (a) values are not equal.")    }

  populations <- dimnames(tau)$i
  years <- dimnames(tau)$y
  ages <- dimnames(tau)$a
  tau_W <- array(NA, dim= dim(tau), dimnames = dimnames(tau))
  for(i in populations) {
    for(y in years) {
      for(a in ages) {
        tau_W[i,y,a] <- tau[i,y,a] * p[i,y,a]
      }
    }
  }
  d <- as.data.frame.table(tau_W, responseName = "tau_W", stringsAsFactors = FALSE)
  d$y <- as.integer(d$y)
  res <- list(tau_W = tau_W, df = d)
  res
}
