#' Get TotalRun by return year
#'
#' Calculate the total run by return year by adding mature run and preterminal fishing mortality
#' in adult equivalents.
#' This is all the fish in the cohort: harvest of all types plus escapement. Used to estimate recruitment.
#' Produces an array by return year and a data frame with both return year and brood year.
#'
#' @param MatureRun Numeric, array of mature run size by population (i), year (y), and age (a).
#' @param phi_Q Numeric, array of preterminal fishing mortality in
#' adult equivalents, with three dimensions: population (i), year (y), and age (a).
#'
#' @return List with three elements. First element: numeric, array of recruits by return year with three dimensions: population (i), return year (y), and
#' age (a). Third element: data frame version of first and second elements, for plotting and tables.
#'
#'
#' @examples
#'   TotalRun <- get_TotalRun( MatureRun = ex_MatureRun, phi_Q = ex_phi_Q)
#'
#' @export
get_TotalRun <- function(MatureRun, phi_Q) {
  # Data checks
  # populations
  if(!dim(MatureRun)[1] == dim(phi_Q)[1] )  {
    stop("Length of population (i) dimensions not equal.") }
  if(!all(dimnames(MatureRun)$i %in% dimnames(phi_Q)$i)) {
    stop("Population (i) values are not equal.")    }
  # Years
  if(!dim(MatureRun)[2] == dim(phi_Q)[2] ) {
    stop("Length of year (y) dimensions not equal.") }
  if(!all(dimnames(MatureRun)$y %in% dimnames(phi_Q)$y )) {
    stop("Year (y) values are not equal.")    }
  # ages
  if(!dim(MatureRun)[3] == dim(phi_Q)[3] )  {
    stop("Length of age (a) dimensions not equal.") }
  if(!all(dimnames(MatureRun)$a %in% dimnames(phi_Q)$a )) {
    stop("Age (a) values are not equal.")    }
  populations <- dimnames(MatureRun)$i
  years <- dimnames(MatureRun)$y
  ages <- dimnames(MatureRun)$a
  n_populations <- length(populations)
  n_ages <- length(ages)
  TotalRun <- array(NA, dim= dim(MatureRun), dimnames = dimnames(MatureRun))
  for(i in populations) {
    for(y in years) {
      for(a in ages) {
        TotalRun[i,y,a] <- MatureRun[i,y,a] + phi_Q[i,y,a]
      }
    }
  }

  d <- as.data.frame.table( TotalRun, responseName = "R_star", stringsAsFactors = FALSE)
  d$y <- as.integer(d$y)
  d$a <- as.integer(d$a)
  res <- list( TotalRun = TotalRun, df = d)
  res
}
