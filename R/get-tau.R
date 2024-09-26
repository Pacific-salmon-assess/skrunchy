#' Get total terminal mortalities
#'
#' Add terminal mortalities in the upper Skeena, lower Skeena, and terminal marine areas.
#'
#' @param tau_U Numeric, array of total terminal mortalities in the upper Skeena with three dimensions: population (i), year (y), and age (a).
#' @param tau_L Numeric, array of total terminal mortalities in the lower Skeena with three dimensions: population (i), year (y), and age (a).
#' @param tau_M Numeric, array of total terminal mortalities in marine fisheries, with three dimensions: population (i), year (y), and age (a).
#'
#' @return List with two elements. First element: numeric, array with three dimensions: population (i), year (y), and age (a).
#' Second element: data frame version of first element, for plotting and tables.
#'
#'
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
#'
#' @export
get_tau <- function( tau_U, tau_L, tau_M) {
  # check dimensions are the same
    if(!all( dim(tau_U)[1] == dim(tau_L)[1] , dim(tau_U)[1] == dim(tau_M)[1]))  {
      stop("Length of population (i) dimensions not equal.") }
    if(!all( dim(tau_U)[2] == dim(tau_L)[2] , dim(tau_U)[2] == dim(tau_M)[2]))  {
      stop("Length of year (y) dimensions not equal.") }
    if(!all( dim(tau_U)[3] == dim(tau_L)[3] , dim(tau_U)[3] == dim(tau_M)[3]))  {
      stop("Length of age (a) dimensions not equal.") }
    if(!all(dimnames(tau_U)$i %in% dimnames(tau_L)$i , dimnames(tau_U)$i %in% dimnames(tau_M)$i )) {
      stop("Population (i) values are not equal.")    }
    if(!all(dimnames(tau_U)$y %in% dimnames(tau_L)$y , dimnames(tau_U)$y %in% dimnames(tau_M)$y )) {
      stop("Year (y) values are not equal.")    }
    if(!all(dimnames(tau_U)$a %in% dimnames(tau_L)$a , dimnames(tau_U)$a %in% dimnames(tau_M)$a )) {
      stop("Age (a) values are not equal.")    }

    populations <- dimnames(tau_U)$i
    years <- dimnames(tau_U)$y
    ages <- dimnames(tau_U)$a
    tau <- array(NA, dim = dim(tau_U), dimnames = dimnames(tau_U))
    for(i in populations) {
      for(y in years) {
        for(a in ages) {
        tau[i,y,a] <- tau_U[i,y,a] + tau_L[i,y,a] + tau_M[i,y,a]
        }
      }
    }
    d <- as.data.frame.table(tau, responseName = "tau", stringsAsFactors = FALSE)
    d$y <- as.integer(d$y)
    res <- list(tau = tau, df = d)
    res
}
