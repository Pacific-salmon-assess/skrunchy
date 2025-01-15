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
#'   phi_Q <- get_phi_Q(phi_N = ex_phi_N, Q = ex_Q)
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
