#' Get terminal run size (wild)
#'
#' Calculate the the terminal run size of wild Chinook by population, year, and age.
#' The terminal run is the number of wild Chinook that made it back to the terminal marine area, and thus
#' includes the wild total terminal mortalities (fish harvested in terminal areas including marine and river),
#' the wild spawners, and any wild fish removed for brood stock. Calculated by population (i), year (y) and age (a).
#'
#' @param tau_W Numeric, array of wild terminal mortalities with three dimensions:  population (i), year (y), and age (a).
#' @param W_star Numeric, array of wild spawner values with three dimensions:  population (i), year (y), and age (a).
#' @param B_star Numeric, array of number of brood stock adult Chinook taken from the Kitsumkalum river, by year (y) and age (a).
#' @param brood_population Character, name of population i with where brood removals occurred. Defaults to Kitsumkalum.
#' @param aggregate_population Character, name of population i that is the sum of the other populations. Defaults to Skeena. Brood also need to removed from this since brood population is part of the aggregate.
#'
#' @return List with two elements. First element: numeric, array of wild terminal run abundance, with dimensions population (i), year (y), and age (a).
#' Second element: data frame version of first element for plotting and tables.
#'
#'
#' @examples
#' TermRun <- get_TermRun(tau_W = ex_tau_W, W_star = ex_W_star, B_star=ex_B_star)
#'
#'
#' @export
get_TermRun <- function(tau_W, W_star, B_star,
                        brood_population = "Kitsumkalum",
                        aggregate_population = "Skeena") {
  # check dimensions and dimension names
  # Populations
  if(! dim(tau_W)[1] == dim(W_star)[1])  {
    stop("Length of population (i) dimensions not equal.") }
  if(!all(dimnames(tau_W)$i %in% dimnames(W_star)$i )) {
    stop("Population (i) values are not equal.")    }
  # Years
  if(!isTRUE( identical( dim(tau_W)[2] , dim(W_star)[2]) && identical( dim(tau_W)[2], dim(B_star)[1])))  {
    stop("Length of year (y) dimensions not equal.") }
  if(!all(dimnames(tau_W)$y %in% dimnames(W_star)$y , dimnames(tau_W)$y %in% dimnames(B_star)$y )) {
    stop("Year (y) values are not equal.")    }
  # Ages
  if(!isTRUE( identical(dim(tau_W)[3] , dim(W_star)[3]) && identical( dim(tau_W)[3] , dim(B_star)[2])))  {
    stop("Length of age (a) dimensions not equal.") }
  if(!all(dimnames(tau_W)$a %in% dimnames(W_star)$a , dimnames(tau_W)$a %in% dimnames(B_star)$a )) {
    stop("Age (a) values are not equal.")    }

  no_brood_populations <- dimnames(W_star)$i[ !dimnames(W_star)$i %in% c(brood_population, aggregate_population)]
  years <- dimnames(W_star)$y
  ages <- dimnames(W_star)$a
  TermRun <- array(NA, dim= dim(W_star), dimnames = dimnames(W_star))
    for(y in years) {
      for(a in ages) {
        TermRun[brood_population,y,a] <- tau_W[brood_population,y,a] + W_star[brood_population,y,a] + B_star[y,a]
        TermRun[aggregate_population,y,a] <- tau_W[aggregate_population,y,a] + W_star[aggregate_population,y,a] + B_star[y,a]
        TermRun[no_brood_populations,y,a] <- tau_W[no_brood_populations,y,a] + W_star[no_brood_populations,y,a]
      }
    }
  d <- as.data.frame.table(TermRun, responseName = "TermRun", stringsAsFactors = FALSE)
  d$y <- as.integer(d$y)
  d$a <- as.integer(d$a)
  res <- list(TermRun = TermRun, df = d)
  res

}
