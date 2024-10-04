#' Get preterminal fishing mortality in adult equivalents
#'
#' Calculate the preteriminal fishing mortality in adult equivalents (AEQ).
#'
#' @param phi_N Numeric, array of preterminal fishing mortality in nominal fish, with three dimensions:
#' population (i), year (y), and age (a).
#' @param Q Numeric, array of adult equivalency (AEQ) rates with two dimensions: year (y) and age (a).
#' Output from CTC Chinook model based on Kitsumkalum CWT releases (KLM).
#'
#' @return List with two elements. First element: numeric, array of preterminal fishing mortality in
#' adult equivalents, with three dimensions: population (i), year (y), and age (a).
#' Second element: dataframe version of first element, for plotting and tables.
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
#'   Q <- array( sample( seq(0.5,0.9, 0.01), size = n_years * n_ages, replace=TRUE), dim = c(n_years, n_ages),
#'     dimnames = list( y = years, a = ages))
#'   Q[, dimnames(Q)$a %in% c(6,7) ] <- 1
#'   phi_Q <- get_phi_Q(phi_N = phi_N$phi_N, Q = Q)
#'
#'
#' @export
get_phi_Q <- function(phi_N, Q) {
  # check dimensions are the same
  if(! dim(phi_N)[2] == dim(Q)[1])  { # Note year is second dimension of phi_N and first dimension of Q
    stop("Length of year (y) dimensions not equal.") }
  if(!dim(phi_N)[3] == dim(Q )[2])  {  # Note age is third dimension of phi_N and second dimension of Q
    stop("Length of age (a) dimensions not equal.") }
  if(!all(dimnames(phi_N)$y %in% dimnames(Q )$y)) {
    stop("Year (y) values are not equal.")    }
  if(!all(dimnames(phi_N)$a %in% dimnames(Q )$a)) {
    stop("Age (a) values are not equal.")    }
  populations <- dimnames(phi_N)$i
  years <- dimnames(phi_N)$y
  ages <- dimnames(phi_N)$a
  phi_Q <- array(NA, dim= dim(phi_N), dimnames = dimnames(phi_N))
  for(i in populations) {
    for(y in years) {
      for(a in ages) {
        phi_Q[i,y,a] <- phi_N[i,y,a] * Q[y,a]
      }
    }
  }
  d <- as.data.frame.table(phi_Q, responseName = "phi_Q", stringsAsFactors = FALSE)
  d$y <- as.integer(d$y)
  res <- list(phi_Q = phi_Q, df = d)
  res
}
