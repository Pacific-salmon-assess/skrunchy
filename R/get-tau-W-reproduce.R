#' Get wild terminal mortalities (different formula to reproduce old excel analysis)
#'
#' Calculate the wild total terminal mortalities by population, year, and age.
#'
#' Winther et al. 2024 applied p to τU and τL, whereas in the updated run reconstruction, we
#' apply p to τL only. This is because τU is assumed to be all wild spawnwers,
#' since all hatchery origin spawners in this method are Kitsumkalum origin and
#' we assume exit the Skeena mainstem just downstream of Terrace, and thus
#' are not encountered in freshwater fisheries upstream of Terrace. Therefore,
#' we assume that all fish caught in freshwater fisheries upstream of Terrace
#' (τU) are wild origin.

#'
#' @param tau_U Numeric, array of total terminal mortalities in the upper Skeena with three dimensions: population (i), year (y), and age (a).
#' @param tau_L Numeric, array of total terminal mortalities in the lower Skeena with three dimensions: population (i), year (y), and age (a).
#' @param tau_M Numeric, array of total terminal mortalities in marine fisheries, with three dimensions: population (i), year (y), and age (a).
#' @param p Numeric, an array of wild spawner proportions with values between 0 and 1, with three dimensions: population (i), year (y), and age (a).
#'
#' @return List with two elements. First element: numeric, array of wild terminal mortalities with three dimensions:  population (i), year (y), and age (a).
#'    Second element: data frame version of first element, for plotting and tables.
#' @examples
#' tau_W <- get_tau_W_reproduce(tau_L = ex_tau_L, tau_U = ex_tau_U, tau_M = ex_tau_M, p = ex_p_wild)
#'
#'
#' @export
get_tau_W_reproduce <- function( tau_U, tau_L, tau_M, p) {
  # check dimensions are the same
  # Check one of the tau variables against p
  # Populations (check one of the tau variables against p)
  if(!dim(tau_U)[1] == dim(p)[1] )  {
    stop("Length of population (i) dimensions not equal.") }
  if(!all(dimnames(tau_U)$i %in% dimnames(p)$i) ) {
    stop("Population (i) values are not equal.")    }
  # Years
  if(! dim(tau_U)[2] == dim(p)[2])  {
    stop("Length of year (y) dimensions not equal.") }
  if(!all(dimnames(tau_U)$y %in% dimnames(p)$y)) {
    stop("Year (y) values are not equal.")    }
  # Ages
  if(!dim(tau_U)[3] == dim(p)[3])  {
    stop("Length of age (a) dimensions not equal.") }
  if(!all(dimnames(tau_U)$a %in% dimnames(p)$a)) {
    stop("Age (a) values are not equal.")    }
  # Check the three tau variables
  # Populations
  if(!isTRUE( identical(dim(tau_U)[1], dim(tau_L)[1]) && identical(dim(tau_U)[1], dim(tau_M)[1])))  {
    stop("Length of population (i) dimensions not equal.") }
  if(!all(dimnames(tau_U)$i %in% dimnames(tau_L)$i , dimnames(tau_U)$i %in% dimnames(tau_M)$i )) {
    stop("Population (i) values are not equal.")    }
  # Years
  if(!isTRUE( identical(dim(tau_U)[2], dim(tau_L)[2]) && identical(dim(tau_U)[2], dim(tau_M)[2])))  {
    stop("Length of year (y) dimensions not equal.") }
  if(!all(dimnames(tau_U)$y %in% dimnames(tau_L)$y , dimnames(tau_U)$y %in% dimnames(tau_M)$y )) {
    stop("Year (y) values are not equal.")    }
  # ages
  if(!isTRUE( identical(dim(tau_U)[3], dim(tau_L)[3]) && identical(dim(tau_U)[3], dim(tau_M)[3]))) {
    stop("Length of age (a) dimensions not equal.") }
  if(!all(dimnames(tau_U)$a %in% dimnames(tau_L)$a , dimnames(tau_U)$a %in% dimnames(tau_M)$a )) {
    stop("Age (a) values are not equal.")    }


  populations <- dimnames(tau_U)$i
  years <- dimnames(tau_U)$y
  ages <- dimnames(tau_U)$a
  tau_W <- array(NA, dim= dim(tau_U), dimnames = dimnames(tau_U))
  for(i in populations) {
    for(y in years) {
      for(a in ages) {
        tau_W[i,y,a] <- tau_M[i,y,a] + p[i,y,a] * tau_L[i,y,a] +  p[i,y,a] * tau_U[i,y,a]
      }
    }
  }
  d <- as.data.frame.table(tau_W, responseName = "tau_W", stringsAsFactors = FALSE)
  d$y <- as.integer(d$y)
  d$a <- as.integer(d$a)
  res <- list(tau_W = tau_W, df = d)
  res
}
