#' Get recruits by return year and brood year
#'
#' Calculate recruits by return year and brood year by adding mature run and preterminal fishing mortality
#' in adult equivalents. Produces an array by return year, an array by brood year, and a data frame with both return year and brood year.
#'
#' @param MatureRun Numeric, array of mature run size by population (i), year (y), and age (a).
#' @param phi_Q Numeric, array of preterminal fishing mortality in
#' adult equivalents, with three dimensions: population (i), year (y), and age (a).
#'
#' @return List with three elements. First element: numeric, array of recruits by return year with three dimensions: population (i), return year (y), and
#' age (a). Second element: numeric, array of recruits by brood year with three dimensions: population (i), brood year (b), and
#' age (a). Third element: data frame version of first and second elements, for plotting and tables.
#'
#'
#' @examples
#'   populations <- c("Kitsumkalum", "Lower Skeena", "Zymoetz-Fiddler", "Upper Skeena", "Middle Skeena", "Large Lakes", "Skeena")
#'   n_populations <- length(populations)
#'   years <- 2000:2010
#'   n_years <- length(years)
#'   ages <- c(4,5,6,7)
#'   p_ages <- c(30,40,40,1)
#'   n_ages <- length(ages)
#'   MatureRun <- array(runif(min = 5000, max = 10000, n= n_populations*n_years*n_ages),
#'     dim = c(n_populations, n_years, n_ages), dimnames = list(i = populations, y = years, a = ages))
#'   MatureRun["Skeena",, ] <- apply(MatureRun[ !dimnames(MatureRun)$i =="Skeena",, ], c(2,3), sum)
#'   phi_Q <- array(runif( min = 0, max = 4000, n = length(MatureRun)), dim = dim(MatureRun), dimnames = dimnames(MatureRun))
#'   R_star <- get_R_star( MatureRun = MatureRun, phi_Q = phi_Q)
#'
#' @export
get_R_star <- function(MatureRun, phi_Q) {
  if(!dim(MatureRun)[1] == dim(phi_Q)[1] )  {
    stop("Length of population (i) dimensions not equal.") }
  if(!dim(MatureRun)[2] == dim(phi_Q)[2] ) {
    stop("Length of year (y) dimensions not equal.") }
  if(!dim(MatureRun)[3] == dim(phi_Q)[3] )  {
    stop("Length of age (a) dimensions not equal.") }
  if(!all(dimnames(MatureRun)$i %in% dimnames(phi_Q)$i)) {
    stop("Population (i) values are not equal.")    }
  if(!all(dimnames(MatureRun)$y %in% dimnames(phi_Q)$y )) {
    stop("Year (y) values are not equal.")    }
  if(!all(dimnames(MatureRun)$a %in% dimnames(phi_Q)$a )) {
    stop("Age (a) values are not equal.")    }
  populations <- dimnames(MatureRun)$i
  years <- dimnames(MatureRun)$y
  ages <- dimnames(MatureRun)$a
  R_star <- array(NA, dim= dim(MatureRun), dimnames = dimnames(MatureRun))
  for(i in populations) {
    for(y in years) {
      for(a in ages) {
        R_star[i,y,a] <- MatureRun[i,y,a] + phi_Q[i,y,a]
      }
    }
  }
  # Now get array of recruits with brood year dimension instead of return year
  brood_years <- seq(min(as.integer(years))- max(as.integer(ages)), max(as.integer(years)) - min(as.integer(ages)))
  n_brood_years <- length(brood_years)
  R_star_b <- array(NA, dim = c(n_populations, n_brood_years, n_ages), dimnames = list( i = populations, b= brood_years, a = ages))
  for(i in populations) {
    for(y in years) {
      for(a in ages) {
        b <- as.character( as.integer(y) - as.integer(a)) # brood year to fill in, character to work with indexing array
        R_star_b[i,b,a] <- R_star[i, y, a]
      }
    }
  }
  d <- as.data.frame.table( R_star, responseName = "R_star", stringsAsFactors = FALSE)
  d$y <- as.integer(d$y)
  d$a <- as.integer(d$a)
  d$b <- d$y - d$a # make brood year variable for data frame
  first_complete_brood <- min(as.integer(years)) - min(as.integer(ages))
  last_complete_brood <- max(as.integer(years)) - max(as.integer(ages))
  if(first_complete_brood <= last_complete_brood)
  complete_brood_years <- seq(first_complete_brood, last_complete_brood, by=1)
  # if first complete brood year is greater than last complete brood year, then there are no complete brood years.
  if(first_complete_brood > last_complete_brood )
  complete_brood_years <- NA
  d$complete_brood <- ifelse(d$b %in% complete_brood_years, TRUE, FALSE) # make complete brood cohort variable
  res <- list(R_star = R_star, R_star_b = R_star_b, df = d)
  res
}
