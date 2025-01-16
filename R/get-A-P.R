#' Get ocean pre-fishery abundance
#'
#' Estimate ocean pre-fishery abundance, which is the preterminal post-fishery
#' abundance plus the fish caught in preterminal non-net fisheries (e.g., troll).
#'
#'
#' @param A_phi Numeric, array of preterminal post-fishery abundance with
#' three dimensions: population (i), year (y), and age (a).
#' @param phi_dot_E Numeric, array of preterminal total mortality exploitation rates
#' for non-net fisheries (e.g., troll), with two dimensions: year (y) and age (a).
#' From the CTC Chinook model output for Kitsumkalum CWT releases (KLM).
#' Equal to the number of fish caught plus incidental mortalities for non-net fisheries (e.g., troll), in
#' areas seaward of the terminal area, divided by total stock for the cohort, in nominal fish (not adjusted for AEQs).
#'
#' @return List with two elements. First element: numeric, array of ocean pre-fishery
#' abundance with three dimensions: population (i), year (y), and age (a).
#' Second element: data frame version of first element for plotting and tables.
#'
#'
#' @examples
#'   A_P <- get_A_P( A_phi = ex_A_phi, phi_dot_E = ex_phi_dot_E)
#'
#'
#' @export
get_A_P <- function(A_phi, phi_dot_E) {
  # check dimensions are the same
  # Years
  if(! dim(A_phi)[2] == dim(phi_dot_E)[1])  { # Note year is second dimension of A_phi and first dimension of phi_dot_E
    stop("Length of year (y) dimensions not equal.") }
  if(!all(dimnames(A_phi)$y %in% dimnames(phi_dot_E )$y)) {
    stop("Year (y) values are not equal.")    }
  # Ages
  if(!dim(A_phi)[3] == dim(phi_dot_E )[2])  {  # Note age is third dimension of A_phi and second dimension of phi_dot_E
    stop("Length of age (a) dimensions not equal.") }
  if(!all(dimnames(A_phi)$a %in% dimnames(phi_dot_E )$a)) {
    stop("Age (a) values are not equal.")    }

  populations <- dimnames(A_phi)$i
  years <- dimnames(A_phi)$y
  ages <- dimnames(A_phi)$a
  A_P <- array(NA, dim= dim(A_phi), dimnames = dimnames(A_phi))
  for(i in populations) {
    for(y in years) {
      for(a in ages) {
        A_P[i,y,a] <- A_phi[i,y,a] / ( 1 - phi_dot_E[y,a])
      }
    }
  }
  d <- as.data.frame.table(A_P, responseName = "A_P", stringsAsFactors = FALSE)
  d$y <- as.integer(d$y)
  d$a <- as.integer(d$a)
  res <- list(A_P = A_P, df = d)
  res
}
