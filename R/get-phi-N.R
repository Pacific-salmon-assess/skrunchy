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
#'   phi_N <- get_phi_N(A_P = ex_A_P, A_phi = ex_A_phi)
#'
#'
#'
#'
#' @export
get_phi_N <- function(A_P, A_phi) {
  # Check dimensions
  # Populations
  if(!dim(A_P)[1] == dim(A_phi)[1] )  {
    stop("Length of population (i) dimensions not equal.") }
  if(!all(dimnames(A_P)$i %in% dimnames(A_phi)$i)) {
    stop("Population (i) values are not equal.")    }
  # Years
  if(!dim(A_P)[2] == dim(A_phi)[2] ) {
    stop("Length of year (y) dimensions not equal.") }
  if(!all(dimnames(A_P)$y %in% dimnames(A_phi)$y )) {
    stop("Year (y) values are not equal.")    }
  # Ages
  if(!dim(A_P)[3] == dim(A_phi)[3] )  {
    stop("Length of age (a) dimensions not equal.") }
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
  d$a <- as.integer(d$a)
  res <- list(phi_N = phi_N, df = d)
  res
}
