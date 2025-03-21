#' Get mature run size abundance
#'
#' Calculates the mature run size abundance, by population, year, and age. The mature run is equal to the wild terminal run plus
#' the mature fish that were caught in marine net (gillnet and seine) fisheries plus incidental mortalities (preterminal). Net fisheries are considered to encounter mature
#' fish so adjustments for natural mortality and adult equivalent calculations are not necessary.
#'
#' @param TermRun Numeric, array of wild terminal run abundance, with dimensions population (i), year (y), and age (a).
#' @param phi_dot_M Numeric, array of preterminal net total mortality harvest rates (greater than or equal to 0, less than 1) with two dimensions: year (y) and age (a).
#' Preterminal net total mortality harvest rate on mature fish: seine and gillnet catch and incidental mortalities of mature fish. From CTC Chinook model output.
#'
#' @return List with two elements. First element: numeric, array of mature run size by population (i), year (y), and age (a).
#' Second element: data frame version of first element, for plotting and tables.
#'
#' @examples
#' MatureRun <- get_MatureRun(TermRun = ex_TermRun, phi_dot_M = ex_phi_dot_M)
#'
#'
#'
#'
#' @export
get_MatureRun <- function(TermRun, phi_dot_M) {
  # check dimensions are the same
  # Years
  if(! dim(TermRun)[2] == dim(phi_dot_M)[1])  {
    stop("Length of year (y) dimensions not equal.") }
  if(!all(dimnames(TermRun)$y %in% dimnames(phi_dot_M)$y)) {
    stop("Year (y) values are not equal.")    }
  # Ages
  if(!dim(TermRun)[3] == dim(phi_dot_M)[2])  {
    stop("Length of age (a) dimensions not equal.") }
  if(!all(dimnames(TermRun)$a %in% dimnames(phi_dot_M)$a)) {
    stop("Age (a) values are not equal.")    }

  populations <- dimnames(TermRun)$i
  years <- dimnames(TermRun)$y
  ages <- dimnames(TermRun)$a
  MatureRun <- array(NA, dim= dim(TermRun), dimnames = dimnames(TermRun))
  for(i in populations) {
    for(y in years) {
      for(a in ages) {
      MatureRun[i,y,a] <- TermRun[i,y,a] / (1 - phi_dot_M[y,a])
      }
    }
  }
  d <- as.data.frame.table(MatureRun, responseName = "MatureRun", stringsAsFactors = FALSE)
  d$y <- as.integer(d$y)
  d$a <- as.integer(d$a)
  res <- list(MatureRun = MatureRun, df = d)
  res

}
