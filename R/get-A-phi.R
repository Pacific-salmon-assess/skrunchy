#' Get preterminal post-fishery abundance
#'
#' Calculate the preterminal post-fishery abundance, which is the abundance of the
#' cohort after non-net fisheries (e.g., troll), after accounting for maturation rate but
#' before preterminal net (gillnet, seine) fishery harvest of mature fish. Of the fish that survived
#' non-net ocean fisheries, the number of fish that matured into the mature run.
#'
#' @param MatureRun Numeric, array of mature run size by population (i), year (y), and age (a).
#' @param r Numeric, array of maturation rate by year and age. CTC Chinook model output for Kitsumkalum (KLM).
#'
#' @return List with two elements. First element: numeric, array of preterminal post-fishery abundance with
#' three dimensions: population (i), year (y), and age (a). Second element: data frame version
#' of first element for plotting and tables.
#'
#'
#' @examples
#'   A_phi <- get_A_phi(MatureRun = ex_MatureRun, r = ex_r)
#'
#'
#' @export
get_A_phi <- function(MatureRun, r) {
  # check dimensions are the same
  # Years
  if(! dim(MatureRun)[2] == dim(r)[1])  { # Note year is second dimension of MatureRun and first dimension of r
    stop("Length of year (y) dimensions not equal.") }
  if(!all(dimnames(MatureRun)$y %in% dimnames(r)$y)) {
    stop("Year (y) values are not equal.")    }
  # Ages
  if(!dim(MatureRun)[3] == dim(r)[2])  {  # Note age is third dimension of MatureRun and second dimension of r
    stop("Length of age (a) dimensions not equal.") }
  if(!all(dimnames(MatureRun)$a %in% dimnames(r)$a)) {
    stop("Age (a) values are not equal.")    }

  populations <- dimnames(MatureRun)$i
  years <- dimnames(MatureRun)$y
  ages <- dimnames(MatureRun)$a
  A_phi <- array(NA, dim= dim(MatureRun), dimnames = dimnames(MatureRun))
  for(i in populations) {
    for(y in years) {
      for(a in ages) {
        A_phi[i,y,a] <- MatureRun[i,y,a] / (r[y,a])
      }
    }
  }
  d <- as.data.frame.table(A_phi, responseName = "A_phi", stringsAsFactors = FALSE)
  d$y <- as.integer(d$y)
  d$a <- as.integer(d$a)
  res <- list(A_phi = A_phi, df = d)
  res
}
