#' Get preterminal fishing mortality in nominal fish
#'
#' The number of fish caught in pre-terminal non-net fisheries (e.g., troll).
#'
#' @param A_P Numeric, array of ocean pre-fishery
#' abundance with three dimensions: population (i), year (y), and age (a).
#' @param A_phi Numeric, array of preterminal post-fishery abundance with
#' three dimensions: population (i), year (y), and age (a).
#'
#' @return List with two elements. First element: numeric, array of preterminal fishing mortality in nominal fish, with three dimensions:
#' population (i), year (y), and age (a). Second element: numeric, dataframe version of first element
#'
#'
#' @examples
#'   populations <- c("Kitsumkalum", "Lower Skeena", "Zymoetz-Fiddler", "Upper Skeena", "Middle Skeena", "Large Lakes", "Skeena")
#'   n_populations <- length(populations)
#'   years <- 2000:2001
#'   n_years <- length(years)
#'   ages <- c(4,5,6,7)
#'   n_ages <- length(ages)
#'   MatureRun <- array(sample(5000:10000, size=n_populations*n_years*n_ages),
#'     dim = c(n_populations, n_years, n_ages), dimnames = list(i = populations, y = years, a = ages))
#'   MatureRun["Skeena",, ] <- apply(MatureRun[ !dimnames(MatureRun)$i =="Skeena",, ], c(2,3), sum)
#'   r <- array( sample( seq(0.8,0.9, 0.01), size = n_years * n_ages, replace=TRUE), dim = c(n_years, n_ages),
#'     dimnames = list( y = years, a = ages))
#'   r[, dimnames(r)$a %in% c(6,7) ] <- 1
#'   A_phi <- get_A_phi(MatureRun = MatureRun, r = r)
#'   phi_dot_E <- array( sample( seq(0.01, 0.2, 0.01), size = n_years * n_ages, replace=TRUE), dim = c(n_years, n_ages),
#'     dimnames = list( y = years, a = ages))
#'   A_P <- get_A_P( A_phi = A_phi$A_phi, phi_dot_E = phi_dot_E)
#'   phi_N <- get_phi_N(A_P = A_P$A_P, A_phi = A_phi$A_phi)
#'
#'
#'
#'
#' @export
get_phi_N <- function(A_P, A_phi) {
  if(!dim(A_P)[1] == dim(A_phi)[1] )  {
    stop("Length of population (i) dimensions not equal.") }
  if(!dim(A_P)[2] == dim(A_phi)[2] ) {
    stop("Length of year (y) dimensions not equal.") }
  if(!dim(A_P)[3] == dim(A_phi)[3] )  {
    stop("Length of age (a) dimensions not equal.") }
  if(!all(dimnames(A_P)$i %in% dimnames(A_phi)$i)) {
    stop("Population (i) values are not equal.")    }
  if(!all(dimnames(A_P)$y %in% dimnames(A_phi)$y )) {
    stop("Year (y) values are not equal.")    }
  if(!all(dimnames(A_P)$a %in% dimnames(A_phi)$a )) {
    stop("Age (a) values are not equal.")    }
  populations <- dimnames(A_P)$i
  years <- dimnames(A_P)$y
  ages <- dimnames(A_P)$a
  phi_N <- array(NA, dim= dim(A_P), dimnames = dimnames(A_P))
  for(i in populations) {
    for(y in years) {
      for(a in ages) {
        phi_N[i,y,a] <- A_P[i,y,a] - A_phi[i,y,a]
      }
    }
  }
  d <- as.data.frame.table( phi_N, responseName = "phi_N", stringsAsFactors = FALSE)
  d$y <- as.integer(d$y)
  res <- list(phi_N = phi_N, df = d)
  res
}
