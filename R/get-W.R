#' Get wild spawners (natural origin) by population
#'
#' Calculates wild spawners (W, not age specific) by population by either summing age-specific wild spawners (W_star) across ages, or subtracting hatchery-origin spawners (H) from spawners (S) for Kitsumkalum and Skeena aggregate populations.
#' All other populations, wild spawners (W) = spawners (S).
#' Not age-specific.
#'
#' @param method How to calcute spawners (W)? If "sum_ages", take the sum across ages of W_star (age-specific wild spawner array).
#'                If "subtract_hatchery", uses non-age-specific spawners (S) and subtracts non-age-specific hatchery spawners (H). Defaults to sum_ages.
#' @param W_star Numeric, array of wild spawner values with three dimensions:  population (i), year (y), and age (a).
#' @param S  numeric, matrix of spawner values for Chinook with two dimensions: population (i), and year (y). Chinook that spawned (doesn't include brood stock removals).
#' @param H Numeric, vector of number of hatchery origin spawners returning to the population with a hatchery (e.g., Kitsumkalum River), by year (y).
#' @param hatchery_population Character, name of population i with where brood removals occurred. Defaults to Kitsumkalum.
#' @param aggregate_population Character, name of population i that is the sum of the other populations. Defaults to Skeena. Brood also need to removed from this since brood population is part of the aggregate.
#'
#' @return List with two elements. First element: numeric, array of wild spawner values for Chinook with two dimensions: population (i), and year (y).
#'         Second element: data frame version of matrix, for plotting and tables.
#'
#' @examples
#' # Sum ages method
#'   W <- get_W(method = "sum_ages", W_star = ex_W_star)
#'
#' # Subtract hatchery method
#'  W <- get_W( method = "subtract_hatchery", S = ex_S, H = ex_H)
#'
#' @export
get_W <- function( method = "sum_ages",
                   W_star,
                   S, H,
                   hatchery_population = "Kitsumkalum",
                   aggregate_population = "Skeena") {
  if(method == "sum_ages") {
    W <- apply(W_star, c(1,2), sum, simplify = "array")
  }
  if(method == "subtract_hatchery") {
  # year check
  if(!dim(S)[2] == length(H) )  {
    stop("Length of year (y) dimensions not equal.") }
  populations <- dimnames(S)$i
  n_populations <- length(dimnames(S)$i)
  years <- dimnames(S)$y
  n_years <- length(dimnames(S)$y)
  no_hatchery_populations <- dimnames(S)$i[ !dimnames(S)$i %in% c(hatchery_population, aggregate_population)]
  start_array <- array(data = NA, dim = c(n_populations, n_years),
                       dimnames = list( i =  populations, y = years))
  W <- start_array
  for(y in 1:n_years) {
    # Get Spawners for brood population and aggregate population. Escapement minus brood stock.
    W[hatchery_population, y ] <- S[ hatchery_population, y] - H[y]
    W[ aggregate_population, y] <- S[aggregate_population, y] - H[y]
    # All other populations, spawners = escapement (no brood)
    W[ no_hatchery_populations, y ] <- S[ no_hatchery_populations, y]
  }
  }
  d <- as.data.frame.table(W, responseName = "W")
  d$y <- as.integer(as.character(d$y))
  res <- list(W = W, df = d)
  res
}
