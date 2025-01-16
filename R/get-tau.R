#' Get total terminal mortalities
#'
#' Add terminal mortalities in the upper Skeena, lower Skeena, and terminal marine areas.
#'
#' @param tau_U Numeric, array of total terminal mortalities in the upper Skeena with three dimensions: population (i), year (y), and age (a).
#' @param tau_L Numeric, array of total terminal mortalities in the lower Skeena with three dimensions: population (i), year (y), and age (a).
#' @param tau_M Numeric, array of total terminal mortalities in marine fisheries, with three dimensions: population (i), year (y), and age (a).
#'
#' @return List with two elements. First element: numeric, array of total terminal mortalities, with three dimensions: population (i), year (y), and age (a).
#' Second element: data frame version of first element, for plotting and tables.
#'
#'
#' @examples
#' tau <- get_tau( tau_U = ex_tau_U, tau_L = ex_tau_L, tau_M = ex_tau_M)
#'
#' @export
get_tau <- function( tau_U, tau_L, tau_M) {
  # check dimensions are the same
  # Populations
    if(!all( dim(tau_U)[1] == dim(tau_L)[1] , dim(tau_U)[1] == dim(tau_M)[1]))  {
      stop("Length of population (i) dimensions not equal.") }
    if(!all(dimnames(tau_U)$i %in% dimnames(tau_L)$i , dimnames(tau_U)$i %in% dimnames(tau_M)$i )) {
      stop("Population (i) values are not equal.")    }
  # Years
    if(!all( dim(tau_U)[2] == dim(tau_L)[2] , dim(tau_U)[2] == dim(tau_M)[2]))  {
      stop("Length of year (y) dimensions not equal.") }
    if(!all(dimnames(tau_U)$y %in% dimnames(tau_L)$y , dimnames(tau_U)$y %in% dimnames(tau_M)$y )) {
      stop("Year (y) values are not equal.")    }
  # ages
    if(!all( dim(tau_U)[3] == dim(tau_L)[3] , dim(tau_U)[3] == dim(tau_M)[3]))  {
      stop("Length of age (a) dimensions not equal.") }
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
    d$a <- as.integer(d$a)
    res <- list(tau = tau, df = d)
    res
}
