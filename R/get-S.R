#' Get spawners by population
#'
#' Calculates spawners (S,  not age-specific) by population by either summing S_star by ages, or subtracting brood stock removals (B) from escapement (E) of Kitsumkalum and Skeena aggregate populations.
#' All other populations, spawners (S) = escapement (E).
#'
#'
#' @param method How to calcute spawners (S)? If "sum_ages", take the sum across ages of S_star (age-specific spawner array).
#'                If "subtract_brood", uses non-age-specific escapement (E) and subtracts non-age-specific brood stock removals (B). Defaults to sum_ages.
#' @param S_star Numeric, array of spawner values with three dimensions:  population (i), year (y), and age (a).
#' @param E numeric, matrix of escapement values for Chinook with two dimensions: population (i), and year (y). Chinook that 'escaped' fisheries en route to spawning river.
#' @param B Numeric, vector of number of brood stock adult Chinook taken from the Kitsumkalum river, by year (y).
#' @param brood_population Character, name of population i with where brood removals occurred. Defaults to Kitsumkalum.
#' @param aggregate_population Character, name of population i that is the sum of the other populations. Defaults to Skeena. Brood also need to removed from this since brood population is part of the aggregate.
#'
#' @return List with two elements. First element: numeric, matrix of spawner values for Chinook with two dimensions: population (i), and year (y).
#'         Second element: data frame version of matrix, for plotting and tables.
#'
#' @examples
#' # Sum ages method
#' S <- get_S(method="sum_ages", S_star = ex_S_star)
#'
#' # Subtract brood method
#'  S <- get_S( method= "subtract_brood", E = ex_E, B = ex_B)
#'
#' @export
get_S <- function( method = "sum_ages",
                   S_star,
                    E, B,
                   brood_population = "Kitsumkalum",
                   aggregate_population = "Skeena") {
  if(method == "sum_ages") {
  S <- apply(S_star, c(1,2), sum, simplify = TRUE)
  }

  if(method == "subtract_brood") {
    # Year check
    if(!dim(E)[2] == length(B) )  {
      stop("Length of year (y) dimensions not equal.") }
  populations <- dimnames(E)$i
  n_populations <- length(dimnames(E)$i)
  years <- dimnames(E)$y
  n_years <- length(dimnames(E)$y)
  no_brood_populations <- dimnames(E)$i[ !dimnames(E)$i %in% c(brood_population, aggregate_population)]
  start_array <- array(data = NA, dim = c(n_populations, n_years),
                       dimnames = list( i =  populations, y = years))
  S <- start_array
  for(y in 1:n_years) {
    # Get Spawners for brood population and aggregate population. Escapement minus brood stock.
    S[brood_population, y ] <- E[ brood_population, y] - B[y]
    S[ aggregate_population, y] <- E[aggregate_population, y] - B[y]
    # All other populations, spawners = escapement (no brood)
    S[ no_brood_populations, y ] <- E[ no_brood_populations, y]
  }
  }
  ds <- as.data.frame.table(S, responseName = "S")
  ds$y <- as.integer(as.character(ds$y))
  res <- list(S = S, df = ds)
  res
}
